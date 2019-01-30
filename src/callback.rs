use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

use gc_arena::{Collect, Gc, MutationContext, StaticCollect};

use crate::{Error, IntoSequence, Sequence, Thread, Value};

// Safe, does not implement drop
#[derive(Collect)]
#[collect(unsafe_drop)]
pub enum CallbackResult<'gc> {
    Return(Vec<Value<'gc>>),
    Yield(Vec<Value<'gc>>),
}

pub type CallbackSequenceBox<'gc> =
    Box<Sequence<'gc, Item = CallbackResult<'gc>, Error = Error<'gc>> + 'gc>;

pub type CallbackBox =
    Box<'static + for<'gc> Fn(Thread<'gc>, Vec<Value<'gc>>) -> CallbackSequenceBox<'gc>>;

#[derive(Clone, Copy, Collect)]
#[collect(require_copy)]
pub struct Callback<'gc>(pub Gc<'gc, StaticCollect<CallbackBox>>);

impl<'gc> Callback<'gc> {
    pub fn new<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static + for<'fgc> Fn(Thread<'fgc>, Vec<Value<'fgc>>) -> CallbackSequenceBox<'fgc>,
    {
        Callback(Gc::allocate(mc, StaticCollect(Box::new(f))))
    }

    pub fn new_immediate<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static
            + for<'fgc> Fn(
                Thread<'fgc>,
                Vec<Value<'fgc>>,
            ) -> Result<CallbackResult<'fgc>, Error<'fgc>>,
    {
        Callback(Gc::allocate(
            mc,
            StaticCollect(Box::new(move |thread, args| {
                Box::new(f(thread, args).into_sequence())
            })),
        ))
    }

    pub fn call(&self, thread: Thread<'gc>, args: Vec<Value<'gc>>) -> CallbackSequenceBox<'gc> {
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
