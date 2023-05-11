pub mod callback;
pub mod closure;

use gc_arena::Collect;

pub use self::{
    callback::{
        AnyCallback, AnyContinuation, AnySequence, Callback, CallbackMode, CallbackReturn,
        Continuation, Sequence,
    },
    closure::{
        Closure, ClosureError, ClosureState, FunctionProto, UpValue, UpValueDescriptor,
        UpValueState,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(AnyCallback<'gc>),
}

impl<'gc> From<Closure<'gc>> for Function<'gc> {
    fn from(closure: Closure<'gc>) -> Self {
        Self::Closure(closure)
    }
}

impl<'gc> From<AnyCallback<'gc>> for Function<'gc> {
    fn from(callback: AnyCallback<'gc>) -> Self {
        Self::Callback(callback)
    }
}
