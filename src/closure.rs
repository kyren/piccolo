use std::{
    hash::{Hash, Hasher},
    io::Read,
};

use allocator_api2::{boxed, vec, SliceExt};
use gc_arena::{allocator_api::MetricsAlloc, lock::Lock, Collect, Gc, Mutation};
use thiserror::Error;

use crate::{
    compiler::{self, CompiledPrototype},
    opcode::OpCode,
    types::UpValueDescriptor,
    Constant, Context, String, Table, Thread, Value,
};

#[derive(Debug, Error)]
pub enum ProtoCompileError {
    #[error(transparent)]
    Parser(#[from] compiler::ParserError),
    #[error(transparent)]
    Compiler(#[from] compiler::CompilerError),
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct FunctionProto<'gc> {
    pub fixed_params: u8,
    pub stack_size: u16,
    pub constants: boxed::Box<[Constant<String<'gc>>], MetricsAlloc<'gc>>,
    pub opcodes: boxed::Box<[OpCode], MetricsAlloc<'gc>>,
    pub upvalues: boxed::Box<[UpValueDescriptor], MetricsAlloc<'gc>>,
    pub prototypes: boxed::Box<[Gc<'gc, FunctionProto<'gc>>], MetricsAlloc<'gc>>,
}

impl<'gc> FunctionProto<'gc> {
    pub fn new(mc: &Mutation<'gc>, compiled_function: &CompiledPrototype<String<'gc>>) -> Self {
        Self::new_map_strings(mc, compiled_function, |s| *s)
    }

    pub fn new_map_strings<S>(
        mc: &Mutation<'gc>,
        compiled_function: &CompiledPrototype<S>,
        map_string: impl Fn(&S) -> String<'gc> + Copy,
    ) -> Self {
        fn new<'gc, S>(
            mc: &Mutation<'gc>,
            compiled_function: &CompiledPrototype<S>,
            map_string: impl Fn(&S) -> String<'gc> + Copy,
        ) -> FunctionProto<'gc> {
            let alloc = MetricsAlloc::new(mc);

            let mut constants = vec::Vec::new_in(alloc.clone());
            constants.extend(
                compiled_function
                    .constants
                    .iter()
                    .map(|c| c.as_string_ref().map_string(map_string)),
            );

            let opcodes = SliceExt::to_vec_in(compiled_function.opcodes.as_slice(), alloc.clone());
            let upvalues =
                SliceExt::to_vec_in(compiled_function.upvalues.as_slice(), alloc.clone());

            let mut prototypes = vec::Vec::new_in(alloc);
            prototypes.extend(
                compiled_function
                    .prototypes
                    .iter()
                    .map(|cf| Gc::new(mc, new(mc, cf, map_string))),
            );

            FunctionProto {
                fixed_params: compiled_function.fixed_params,
                stack_size: compiled_function.stack_size,
                constants: constants.into_boxed_slice(),
                opcodes: opcodes.into_boxed_slice(),
                upvalues: upvalues.into_boxed_slice(),
                prototypes: prototypes.into_boxed_slice(),
            }
        }

        new(mc, compiled_function, map_string)
    }

    pub fn compile(
        ctx: Context<'gc>,
        source: impl Read,
    ) -> Result<FunctionProto<'gc>, ProtoCompileError> {
        #[derive(Copy, Clone)]
        struct Interner<'gc>(Context<'gc>);

        impl<'gc> compiler::StringInterner for Interner<'gc> {
            type String = String<'gc>;

            fn intern(&mut self, s: &[u8]) -> Self::String {
                self.0.state.strings.intern(&self.0, s)
            }
        }

        let interner = Interner(ctx);

        let chunk = compiler::parse_chunk(source, interner)?;
        let compiled_function = compiler::compile_chunk(&chunk, interner)?;

        Ok(FunctionProto::new(&ctx, &compiled_function))
    }
}

#[derive(Debug, Collect, Copy, Clone)]
#[collect(no_drop)]
pub enum UpValueState<'gc> {
    Open(Thread<'gc>, usize),
    Closed(Value<'gc>),
}

#[derive(Debug, Collect, Copy, Clone)]
#[collect(no_drop)]
pub struct UpValue<'gc>(pub Gc<'gc, Lock<UpValueState<'gc>>>);

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct ClosureState<'gc> {
    pub proto: Gc<'gc, FunctionProto<'gc>>,
    pub upvalues: vec::Vec<UpValue<'gc>, MetricsAlloc<'gc>>,
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Closure<'gc>(pub Gc<'gc, ClosureState<'gc>>);

impl<'gc> PartialEq for Closure<'gc> {
    fn eq(&self, other: &Closure<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Closure<'gc> {}

impl<'gc> Hash for Closure<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.0 as *const ClosureState).hash(state)
    }
}

#[derive(Debug, Copy, Clone, Error)]
pub enum ClosureError {
    #[error("cannot use prototype with upvalues other than _ENV to create top-level closure")]
    HasUpValues,
    #[error("closure requires _ENV upvalue but no environment was provided")]
    RequiresEnv,
}

impl<'gc> Closure<'gc> {
    /// Create a top-level closure, prototype must not have any upvalues besides _ENV.
    pub fn new(
        mc: &Mutation<'gc>,
        proto: FunctionProto<'gc>,
        environment: Option<Table<'gc>>,
    ) -> Result<Closure<'gc>, ClosureError> {
        let proto = Gc::new(mc, proto);
        let mut upvalues = vec::Vec::new_in(MetricsAlloc::new(mc));

        if !proto.upvalues.is_empty() {
            if proto.upvalues.len() > 1 || proto.upvalues[0] != UpValueDescriptor::Environment {
                return Err(ClosureError::HasUpValues);
            } else if let Some(environment) = environment {
                upvalues.push(UpValue(Gc::new(
                    mc,
                    Lock::new(UpValueState::Closed(Value::Table(environment))),
                )));
            } else {
                return Err(ClosureError::RequiresEnv);
            }
        }

        Ok(Closure(Gc::new(mc, ClosureState { proto, upvalues })))
    }

    /// Compile a top-level closure from source, using the globals table as the `_ENV` table.
    pub fn load(ctx: Context<'gc>, source: impl Read) -> Result<Closure<'gc>, ProtoCompileError> {
        Self::load_with_env(ctx, source, ctx.state.globals)
    }

    /// Compile a top-level closure from source, using the given table as the `_ENV` table.
    pub fn load_with_env(
        ctx: Context<'gc>,
        source: impl Read,
        env: Table<'gc>,
    ) -> Result<Closure<'gc>, ProtoCompileError> {
        let proto = FunctionProto::compile(ctx, source)?;
        Ok(Closure::new(&ctx, proto, Some(env)).unwrap())
    }
}
