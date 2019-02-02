use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

use gc_arena::{Collect, Gc, MutationContext, StaticCollect};

use crate::{Error, Function, IntoSequence, Sequence, Value};

// TODO: Needs to be FnOnce
#[derive(Collect)]
#[collect(require_static)]
pub struct Continuation(
    Box<'static + for<'gc> Fn(Result<Vec<Value<'gc>>, Error<'gc>>) -> CallbackSequenceBox<'gc>>,
);

impl Continuation {
    pub fn new<C>(cont: C) -> Continuation
    where
        C: 'static
            + for<'fgc> Fn(Result<Vec<Value<'fgc>>, Error<'fgc>>) -> CallbackSequenceBox<'fgc>,
    {
        Continuation(Box::new(cont))
    }

    pub fn new_immediate<C>(cont: C) -> Continuation
    where
        C: 'static
            + for<'fgc> Fn(
                Result<Vec<Value<'fgc>>, Error<'fgc>>,
            ) -> Result<CallbackResult<'fgc>, Error<'fgc>>,
    {
        Continuation(Box::new(move |res| Box::new(cont(res).into_sequence())))
    }

    pub fn call<'gc>(&self, res: Result<Vec<Value<'gc>>, Error<'gc>>) -> CallbackSequenceBox<'gc> {
        (*self.0)(res)
    }
}

// Safe, does not implement drop
#[derive(Collect)]
#[collect(unsafe_drop)]
pub enum CallbackResult<'gc> {
    Return(Vec<Value<'gc>>),
    Yield(Vec<Value<'gc>>),
    TailCall {
        function: Function<'gc>,
        args: Vec<Value<'gc>>,
        continuation: Continuation,
    },
}

pub type CallbackSequenceBox<'gc> =
    Box<Sequence<'gc, Item = CallbackResult<'gc>, Error = Error<'gc>> + 'gc>;

pub type CallbackBox = Box<'static + for<'gc> Fn(Vec<Value<'gc>>) -> CallbackSequenceBox<'gc>>;

#[derive(Clone, Copy, Collect)]
#[collect(require_copy)]
pub struct Callback<'gc>(pub Gc<'gc, StaticCollect<CallbackBox>>);

impl<'gc> Callback<'gc> {
    pub fn new<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static + for<'fgc> Fn(Vec<Value<'fgc>>) -> CallbackSequenceBox<'fgc>,
    {
        Callback(Gc::allocate(mc, StaticCollect(Box::new(f))))
    }

    pub fn new_immediate<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static + for<'fgc> Fn(Vec<Value<'fgc>>) -> Result<CallbackResult<'fgc>, Error<'fgc>>,
    {
        Callback(Gc::allocate(
            mc,
            StaticCollect(Box::new(move |args| Box::new(f(args).into_sequence()))),
        ))
    }

    pub fn call(&self, args: Vec<Value<'gc>>) -> CallbackSequenceBox<'gc> {
        (*(self.0).0)(args)
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
