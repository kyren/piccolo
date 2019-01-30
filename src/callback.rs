use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

use gc_arena::{Collect, Gc, MutationContext, StaticCollect};

use crate::{Error, Sequence, Thread, Value};

// Safe, does not implement the drop trait
#[derive(Collect)]
#[collect(unsafe_drop)]
pub enum CallbackResult<'gc> {
    Return(Vec<Value<'gc>>),
    Yield(Vec<Value<'gc>>),
}

pub enum CallbackReturn<'gc> {
    Immediate(CallbackResult<'gc>),
    Sequence(Box<Sequence<'gc, Item = CallbackResult<'gc>, Error = Error<'gc>> + 'gc>),
}

pub type CallbackBox = Box<
    'static + for<'gc> Fn(Thread<'gc>, &[Value<'gc>]) -> Result<CallbackReturn<'gc>, Error<'gc>>,
>;

#[derive(Clone, Copy, Collect)]
#[collect(require_copy)]
pub struct Callback<'gc>(pub Gc<'gc, StaticCollect<CallbackBox>>);

impl<'gc> Callback<'gc> {
    pub fn new_immediate<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static
            + for<'fgc> Fn(Thread<'fgc>, &[Value<'fgc>]) -> Result<CallbackResult<'fgc>, Error<'fgc>>,
    {
        Callback(Gc::allocate(
            mc,
            StaticCollect(Box::new(move |thread, args| {
                Ok(CallbackReturn::Immediate(f(thread, args)?))
            })),
        ))
    }

    pub fn new_sequence<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static
            + for<'fgc> Fn(
                Thread<'fgc>,
                &[Value<'fgc>],
            ) -> Result<
                Box<Sequence<'fgc, Item = CallbackResult<'fgc>, Error = Error<'fgc>> + 'fgc>,
                Error<'fgc>,
            >,
    {
        Callback(Gc::allocate(
            mc,
            StaticCollect(Box::new(move |thread, args| {
                Ok(CallbackReturn::Sequence(f(thread, args)?))
            })),
        ))
    }

    pub fn call(
        &self,
        thread: Thread<'gc>,
        args: &[Value<'gc>],
    ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
        (*(self.0).0)(thread, args)
    }
}

impl<'gc> Debug for Callback<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Callback")
            .field(&Gc::as_ptr(self.0))
            .finish()
    }
}

impl<'gc> PartialEq for Callback<'gc> {
    fn eq(&self, other: &Callback<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Callback<'gc> {}

impl<'gc> Hash for Callback<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}
