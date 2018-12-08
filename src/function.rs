use std::hash::{Hash, Hasher};

use failure::{err_msg, Error};

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::opcode::{OpCode, Register, UpValueIndex};
use crate::value::Value;

#[derive(Debug, Collect, Clone, Copy)]
pub enum UpValueDescriptor {
    ParentLocal(Register),
    Outer(UpValueIndex),
}

#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct FunctionProto<'gc> {
    pub fixed_params: u8,
    pub has_varargs: bool,
    pub stack_size: u16,
    pub constants: Vec<Value<'gc>>,
    pub opcodes: Vec<OpCode>,
    pub upvalues: Vec<UpValueDescriptor>,
    pub prototypes: Vec<Gc<'gc, FunctionProto<'gc>>>,
}

#[derive(Debug, Collect, Copy, Clone)]
#[collect(require_copy)]
pub enum UpValueState<'gc> {
    Open(usize),
    Closed(Value<'gc>),
}

#[derive(Debug, Collect, Copy, Clone)]
#[collect(require_copy)]
pub struct UpValue<'gc>(pub GcCell<'gc, UpValueState<'gc>>);

#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct ClosureState<'gc> {
    pub proto: Gc<'gc, FunctionProto<'gc>>,
    pub upvalues: Vec<UpValue<'gc>>,
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Closure<'gc>(pub Gc<'gc, ClosureState<'gc>>);

impl<'gc> PartialEq for Closure<'gc> {
    fn eq(&self, other: &Closure<'gc>) -> bool {
        &*self.0 as *const ClosureState == &*other.0 as *const ClosureState
    }
}

impl<'gc> Eq for Closure<'gc> {}

impl<'gc> Hash for Closure<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.0 as *const ClosureState).hash(state)
    }
}

impl<'gc> Closure<'gc> {
    // Create a top-level closure, prototype must not have any upvalues
    pub fn new(
        mc: MutationContext<'gc, '_>,
        proto: FunctionProto<'gc>,
    ) -> Result<Closure<'gc>, Error> {
        if !proto.upvalues.is_empty() {
            Err(err_msg(
                "cannot use prototype with upvalues to create top-level closure",
            ))
        } else {
            Ok(Closure(Gc::allocate(
                mc,
                ClosureState {
                    proto: Gc::allocate(mc, proto),
                    upvalues: Vec::new(),
                },
            )))
        }
    }
}
