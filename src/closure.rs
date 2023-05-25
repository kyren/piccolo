use std::hash::{Hash, Hasher};

use gc_arena::{lock::Lock, Collect, Gc, Mutation};
use thiserror::Error;

use crate::{
    CompiledPrototype, Constant, OpCode, RegisterIndex, String, Table, Thread, UpValueIndex, Value,
};

#[derive(Debug, Collect, Clone, Copy, PartialEq, Eq)]
#[collect(require_static)]
pub enum UpValueDescriptor {
    Environment,
    ParentLocal(RegisterIndex),
    Outer(UpValueIndex),
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct FunctionProto<'gc> {
    pub fixed_params: u8,
    pub stack_size: u16,
    pub constants: Vec<Constant<String<'gc>>>,
    pub opcodes: Vec<OpCode>,
    pub upvalues: Vec<UpValueDescriptor>,
    pub prototypes: Vec<Gc<'gc, FunctionProto<'gc>>>,
}

impl<'gc> FunctionProto<'gc> {
    pub fn from_compiled(
        mc: &Mutation<'gc>,
        compiled_function: CompiledPrototype<String<'gc>>,
    ) -> Self {
        Self {
            fixed_params: compiled_function.fixed_params,
            stack_size: compiled_function.stack_size,
            constants: compiled_function.constants,
            opcodes: compiled_function.opcodes,
            upvalues: compiled_function.upvalues,
            prototypes: compiled_function
                .prototypes
                .into_iter()
                .map(|cf| Gc::new(mc, FunctionProto::from_compiled(mc, *cf)))
                .collect(),
        }
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
    pub upvalues: Vec<UpValue<'gc>>,
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

#[derive(Debug, Copy, Clone, Error, Collect)]
#[collect(require_static)]
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
        let mut upvalues = Vec::new();

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
}
