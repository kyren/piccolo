use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

use gc_arena::{Collect, Gc, MutationContext};

use crate::{Error, Sequence, Thread, Value};

pub enum CallbackResult<'gc> {
    Return(Vec<Value<'gc>>),
    Yield(Vec<Value<'gc>>),
    Continue(Box<Sequence<'gc, Item = CallbackResult<'gc>, Error = Error> + 'gc>),
}

#[derive(Collect)]
#[collect(require_static)]
pub struct CallbackFn(
    pub  Box<
        for<'gc> Fn(Thread<'gc>, &[Value<'gc>]) -> Result<CallbackResult<'gc>, Error> + 'static,
    >,
);

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
    pub fn new<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static
            + for<'fgc> Fn(Thread<'fgc>, &[Value<'fgc>]) -> Result<CallbackResult<'fgc>, Error>,
    {
        Callback(Gc::allocate(mc, CallbackFn(Box::new(f))))
    }

    pub fn call(
        &self,
        thread: Thread<'gc>,
        args: &[Value<'gc>],
    ) -> Result<CallbackResult<'gc>, Error> {
        (*(self.0).0)(thread, args)
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
