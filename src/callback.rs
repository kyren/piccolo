use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

use gc_arena::{Collect, Gc, MutationContext};

use crate::error::Error;
use crate::sequence::Sequence;
use crate::value::Value;

pub type CallbackSequence<'gc> = Box<Sequence<'gc, Item = Vec<Value<'gc>>, Error = Error> + 'gc>;

#[derive(Collect)]
#[collect(require_static)]
pub struct CallbackFn(pub Box<for<'gc> Fn(&[Value<'gc>]) -> CallbackSequence<'gc> + 'static>);

impl CallbackFn {
    pub fn new<F: 'static + for<'gc> Fn(&[Value<'gc>]) -> CallbackSequence<'gc>>(
        f: F,
    ) -> CallbackFn {
        CallbackFn(Box::new(f))
    }
}

impl Debug for CallbackFn {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("CallbackFn")
            .field(&(&self.0 as *const _))
            .finish()
    }
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_copy)]
pub struct Callback<'gc>(pub Gc<'gc, CallbackFn>);

impl<'gc> Callback<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>, callback_fn: CallbackFn) -> Callback<'gc> {
        Callback(Gc::allocate(mc, callback_fn))
    }

    pub fn call(&self, args: &[Value<'gc>]) -> CallbackSequence<'gc> {
        (*(self.0).0)(args)
    }
}

impl<'gc> PartialEq for Callback<'gc> {
    fn eq(&self, other: &Callback<'gc>) -> bool {
        Gc::ptr_eq(&self.0, &other.0)
    }
}

impl<'gc> Eq for Callback<'gc> {}

impl<'gc> Hash for Callback<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.0 as *const CallbackFn).hash(state)
    }
}

#[macro_export]
macro_rules! lua_callback {
    ($mc:expr, $f:expr) => {
        $crate::callback::CallbackFn::new(|args| {
            Box::new($crate::sequence::IntoSequence::into_sequence($f(args)))
        })
    };
}
