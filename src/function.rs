use std::hash::{Hash, Hasher};

use gc_arena::{Gc, GcCell, MutationContext};

use opcode::OpCode;
use value::Value;

#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct FunctionProto<'gc> {
    pub fixed_params: u8,
    pub has_varargs: bool,
    pub constants: Vec<Value<'gc>>,
    pub opcodes: Vec<OpCode>,
}

#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct Closure<'gc> {
    pub proto: Gc<'gc, FunctionProto<'gc>>,
}

impl<'gc> Closure<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>, proto: FunctionProto<'gc>) -> Closure<'gc> {
        Closure {
            proto: Gc::allocate(mc, proto),
        }
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Function<'gc>(pub GcCell<'gc, Closure<'gc>>);

impl<'gc> PartialEq for Function<'gc> {
    fn eq(&self, other: &Function<'gc>) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl<'gc> Eq for Function<'gc> {}

impl<'gc> Hash for Function<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
    }
}

impl<'gc> Function<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>, closure: Closure<'gc>) -> Function<'gc> {
        Function(GcCell::allocate(mc, closure))
    }
}
