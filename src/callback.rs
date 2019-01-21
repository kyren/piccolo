use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

use gc_arena::{Collect, Gc, MutationContext};

use crate::{Continuation, Error, Value};

#[derive(Collect)]
#[collect(require_static)]
pub struct CallbackFn(
    pub Box<for<'gc> Fn(&[Value<'gc>]) -> Continuation<'gc, Vec<Value<'gc>>, Error> + 'static>,
);

impl CallbackFn {
    pub fn new<F>(f: F) -> CallbackFn
    where
        F: 'static
            + for<'gc> Fn(&[Value<'gc>]) -> Continuation<'gc, Vec<Value<'gc>>, Error>
            + 'static,
    {
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

    pub fn call(&self, args: &[Value<'gc>]) -> Continuation<'gc, Vec<Value<'gc>>, Error> {
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

/// Constructs a `CallbackFn` object.
///
/// Expects as a parameter a function of the type:
///
/// `F: for<'gc> Fn(&[Value<'gc>]) -> impl IntoContinuation<'gc, Vec<Value<'gc>>, Error> + 'gc
///
/// This type is difficult to express in Rust, so for ergonomics reasons this fucntion must be a
/// macro.
#[macro_export]
macro_rules! lua_callback {
    ($f:expr) => {
        $crate::CallbackFn::new(|args| {
            Box::new($crate::IntoContinuation::into_continuation($f(args)))
        })
    };
}
