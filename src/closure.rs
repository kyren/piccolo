use std::error::Error as StdError;
use std::fmt;
use std::hash::{Hash, Hasher};

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::{Constant, OpCode, RegisterIndex, Table, Thread, UpValueIndex, Value};

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
    pub has_varargs: bool,
    pub stack_size: u16,
    pub constants: Vec<Constant<'gc>>,
    pub opcodes: Vec<OpCode>,
    pub upvalues: Vec<UpValueDescriptor>,
    pub prototypes: Vec<Gc<'gc, FunctionProto<'gc>>>,
}

#[derive(Debug, Collect, Copy, Clone)]
#[collect(no_drop)]
pub enum UpValueState<'gc> {
    Open(Thread<'gc>, usize),
    Closed(Value<'gc>),
}

#[derive(Debug, Collect, Copy, Clone)]
#[collect(no_drop)]
pub struct UpValue<'gc>(pub GcCell<'gc, UpValueState<'gc>>);

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

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_static)]
pub enum ClosureError {
    HasUpValues,
    RequiresEnv,
}

impl StdError for ClosureError {}

impl fmt::Display for ClosureError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClosureError::HasUpValues => write!(
                fmt,
                "cannot use prototype with upvalues other than _ENV to create top-level closure"
            ),
            ClosureError::RequiresEnv => write!(
                fmt,
                "closure requires _ENV upvalue but no environment was provided"
            ),
        }
    }
}

impl<'gc> Closure<'gc> {
    /// Create a top-level closure, prototype must not have any upvalues besides _ENV.
    pub fn new(
        mc: MutationContext<'gc, '_>,
        proto: FunctionProto<'gc>,
        environment: Option<Table<'gc>>,
    ) -> Result<Closure<'gc>, ClosureError> {
        let proto = Gc::allocate(mc, proto);
        let mut upvalues = Vec::new();

        if !proto.upvalues.is_empty() {
            if proto.upvalues.len() > 1 || proto.upvalues[0] != UpValueDescriptor::Environment {
                return Err(ClosureError::HasUpValues);
            } else if let Some(environment) = environment {
                upvalues.push(UpValue(GcCell::allocate(
                    mc,
                    UpValueState::Closed(Value::Table(environment)),
                )));
            } else {
                return Err(ClosureError::RequiresEnv);
            }
        }

        Ok(Closure(Gc::allocate(mc, ClosureState { proto, upvalues })))
    }
}
