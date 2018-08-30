use std::hash::{Hash, Hasher};

use gc_arena::{Gc, MutationContext};

use opcode::{OpCode, Register, UpValueIndex};
use value::Value;

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
    pub functions: Vec<FunctionProto<'gc>>,
}

#[derive(Debug, Collect, Copy, Clone)]
#[collect(require_copy)]
pub enum UpValue<'gc> {
    Open(usize),
    Closed(Value<'gc>),
}

#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct ClosureState<'gc> {
    pub proto: Gc<'gc, FunctionProto<'gc>>,
    pub upvalues: Vec<Gc<'gc, UpValue<'gc>>>,
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
    pub fn new(mc: MutationContext<'gc, '_>, proto: FunctionProto<'gc>) -> Closure<'gc> {
        Closure(Gc::allocate(
            mc,
            ClosureState {
                proto: Gc::allocate(mc, proto),
                upvalues: Vec::new(),
            },
        ))
    }
}
