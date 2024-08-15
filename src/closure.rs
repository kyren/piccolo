use std::{
    hash::{Hash, Hasher},
    io::Read,
};

use allocator_api2::{boxed, vec, SliceExt};
use gc_arena::{allocator_api::MetricsAlloc, lock::Lock, Collect, Gc, Mutation};
use thiserror::Error;

use crate::{
    compiler::{self, CompiledPrototype, FunctionRef, LineNumber},
    opcode::OpCode,
    thread::OpenUpValue,
    types::UpValueDescriptor,
    Constant, Context, String, Table, Value,
};

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("parse error")]
    Parsing(#[from] compiler::ParseError),
    #[error("compile error")]
    Compilation(#[from] compiler::CompileError),
}

/// A compiled Lua function.
///
/// In Lua jargon, a "prototype" is only executable code, it has none of its "upvalues" set and
/// cannot be called directly.
///
/// If a prototype has only an single (optional) `_ENV` upvalue, then it can be turned into an
/// executable `Closure` by binding it with its environment with [`Closure::new`].
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct FunctionPrototype<'gc> {
    pub chunk_name: String<'gc>,
    pub reference: FunctionRef<String<'gc>>,
    pub fixed_params: u8,
    pub has_varargs: bool,
    pub stack_size: u16,
    pub constants: boxed::Box<[Constant<String<'gc>>], MetricsAlloc<'gc>>,
    pub opcodes: boxed::Box<[OpCode], MetricsAlloc<'gc>>,
    pub opcode_line_numbers: boxed::Box<[(usize, LineNumber)], MetricsAlloc<'gc>>,
    pub upvalues: boxed::Box<[UpValueDescriptor], MetricsAlloc<'gc>>,
    pub prototypes: boxed::Box<[Gc<'gc, FunctionPrototype<'gc>>], MetricsAlloc<'gc>>,
}

impl<'gc> FunctionPrototype<'gc> {
    pub fn from_compiled(
        mc: &Mutation<'gc>,
        chunk_name: String<'gc>,
        compiled_function: &CompiledPrototype<String<'gc>>,
    ) -> Self {
        Self::from_compiled_map_strings(mc, chunk_name, compiled_function, |s| *s)
    }

    pub fn from_compiled_map_strings<S>(
        mc: &Mutation<'gc>,
        chunk_name: String<'gc>,
        compiled_function: &CompiledPrototype<S>,
        map_string: impl Fn(&S) -> String<'gc>,
    ) -> Self {
        fn new<'gc, S>(
            mc: &Mutation<'gc>,
            chunk_name: String<'gc>,
            compiled_function: &CompiledPrototype<S>,
            map_string: impl Fn(&S) -> String<'gc> + Copy,
        ) -> FunctionPrototype<'gc> {
            let alloc = MetricsAlloc::new(mc);

            let mut constants = vec::Vec::new_in(alloc.clone());
            constants.extend(
                compiled_function
                    .constants
                    .iter()
                    .map(|c| c.as_string_ref().map_string(map_string)),
            );

            let opcodes = SliceExt::to_vec_in(compiled_function.opcodes.as_slice(), alloc.clone());
            let opcode_line_numbers = SliceExt::to_vec_in(
                compiled_function.opcode_line_numbers.as_slice(),
                alloc.clone(),
            );
            let upvalues =
                SliceExt::to_vec_in(compiled_function.upvalues.as_slice(), alloc.clone());

            let mut prototypes = vec::Vec::new_in(alloc);
            prototypes.extend(
                compiled_function
                    .prototypes
                    .iter()
                    .map(|cf| Gc::new(mc, new(mc, chunk_name, cf, map_string))),
            );

            FunctionPrototype {
                chunk_name,
                reference: compiled_function
                    .reference
                    .as_string_ref()
                    .map_strings(map_string),
                fixed_params: compiled_function.fixed_params,
                has_varargs: compiled_function.has_varargs,
                stack_size: compiled_function.stack_size,
                constants: constants.into_boxed_slice(),
                opcodes: opcodes.into_boxed_slice(),
                opcode_line_numbers: opcode_line_numbers.into_boxed_slice(),
                upvalues: upvalues.into_boxed_slice(),
                prototypes: prototypes.into_boxed_slice(),
            }
        }

        new(mc, chunk_name, compiled_function, &map_string)
    }

    pub fn compile(
        ctx: Context<'gc>,
        source_name: &str,
        source: impl Read,
    ) -> Result<FunctionPrototype<'gc>, CompilerError> {
        // Make a concrete (non-generic) function here to prevent the Rust compiler
        // from having to also monomorphize the entire compiler for each crate that
        // uses this.
        fn compile_inner<'gc>(
            ctx: Context<'gc>,
            source_name: &str,
            interner: Interner<'gc>,
            chunk: compiler::parser::Chunk<String<'gc>>,
        ) -> Result<FunctionPrototype<'gc>, CompilerError> {
            let compiled_function = compiler::compile_chunk(&chunk, interner)?;

            Ok(FunctionPrototype::from_compiled(
                &ctx,
                ctx.intern(source_name.as_bytes()),
                &compiled_function,
            ))
        }

        let interner = Interner(ctx);
        let chunk = compiler::parse_chunk(source, interner)?;

        compile_inner(ctx, source_name, interner, chunk)
    }
}

#[derive(Copy, Clone)]
struct Interner<'gc>(Context<'gc>);

impl<'gc> compiler::StringInterner for Interner<'gc> {
    type String = String<'gc>;

    fn intern(&mut self, s: &[u8]) -> Self::String {
        self.0.intern(s)
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum UpValueState<'gc> {
    Open(OpenUpValue<'gc>),
    Closed(Value<'gc>),
}

pub type UpValueInner<'gc> = Lock<UpValueState<'gc>>;

#[derive(Debug, Collect, Copy, Clone)]
#[collect(no_drop)]
pub struct UpValue<'gc>(Gc<'gc, UpValueInner<'gc>>);

impl<'gc> UpValue<'gc> {
    pub fn new(mc: &Mutation<'gc>, state: UpValueState<'gc>) -> Self {
        Self(Gc::new(mc, Lock::new(state)))
    }

    pub fn from_inner(inner: Gc<'gc, UpValueInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, UpValueInner<'gc>> {
        self.0
    }

    pub fn get(self) -> UpValueState<'gc> {
        self.0.get()
    }

    pub fn set(self, mc: &Mutation<'gc>, state: UpValueState<'gc>) {
        self.0.set(mc, state)
    }
}

#[derive(Debug, Copy, Clone, Error)]
pub enum ClosureError {
    #[error("cannot use prototype with upvalues other than _ENV to create top-level closure")]
    HasUpValues,
    #[error("closure requires _ENV upvalue but no environment was provided")]
    RequiresEnv,
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct ClosureInner<'gc> {
    proto: Gc<'gc, FunctionPrototype<'gc>>,
    upvalues: vec::Vec<UpValue<'gc>, MetricsAlloc<'gc>>,
}

/// A garbage collected pointer to an executable Lua function.
///
/// A `Closure` represents a [`FunctionPrototype`] bound to an environment. A closure "closes over"
/// free variables that it references, and as such, calling a `Closure` may reference (and mutate!)
/// these closed over variables. In Lua jargon, these references that closures "close over" are
/// called "upvalues".
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Closure<'gc>(Gc<'gc, ClosureInner<'gc>>);

impl<'gc> PartialEq for Closure<'gc> {
    fn eq(&self, other: &Closure<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Closure<'gc> {}

impl<'gc> Hash for Closure<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}

impl<'gc> Closure<'gc> {
    /// Create a top-level closure, prototype must not have any upvalues besides _ENV.
    pub fn new(
        mc: &Mutation<'gc>,
        proto: FunctionPrototype<'gc>,
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

        Ok(Closure(Gc::new(mc, ClosureInner { proto, upvalues })))
    }

    pub fn from_parts(
        mc: &Mutation<'gc>,
        proto: Gc<'gc, FunctionPrototype<'gc>>,
        upvalues: vec::Vec<UpValue<'gc>, MetricsAlloc<'gc>>,
    ) -> Self {
        Self(Gc::new(mc, ClosureInner { proto, upvalues }))
    }

    pub fn from_inner(inner: Gc<'gc, ClosureInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, ClosureInner<'gc>> {
        self.0
    }

    /// Compile a top-level closure from source, using the globals table as the `_ENV` table.
    pub fn load(
        ctx: Context<'gc>,
        name: Option<&str>,
        source: impl Read,
    ) -> Result<Closure<'gc>, CompilerError> {
        Self::load_with_env(ctx, name, source, ctx.globals())
    }

    /// Compile a top-level closure from source, using the given table as the `_ENV` table.
    pub fn load_with_env(
        ctx: Context<'gc>,
        name: Option<&str>,
        source: impl Read,
        env: Table<'gc>,
    ) -> Result<Closure<'gc>, CompilerError> {
        let proto = FunctionPrototype::compile(ctx, name.unwrap_or("<anonymous>"), source)?;
        Ok(Closure::new(&ctx, proto, Some(env)).unwrap())
    }

    pub fn prototype(self) -> Gc<'gc, FunctionPrototype<'gc>> {
        self.0.proto
    }

    pub fn upvalues(self) -> &'gc [UpValue<'gc>] {
        &Gc::as_ref(self.0).upvalues
    }
}
