use std::{
    fmt::{self, Debug},
    hash::{Hash, Hasher},
};

use gc_arena::{Collect, Gc, MutationContext, StaticCollect};

use crate::{Error, Function, Sequence, SequenceExt, Value};

#[derive(Collect)]
#[collect(no_drop)]
pub enum CallbackReturn<'gc> {
    Return(Vec<Value<'gc>>),
    Yield(Vec<Value<'gc>>),
    TailCall {
        function: Function<'gc>,
        args: Vec<Value<'gc>>,
        continuation: Option<Continuation<'gc>>,
    },
}

pub enum CallbackSequence<'gc> {
    Immediate(Result<CallbackReturn<'gc>, Error<'gc>>),
    Sequence(Box<dyn Sequence<'gc, Output = Result<CallbackReturn<'gc>, Error<'gc>>> + 'gc>),
}

pub trait CallbackFn<'gc>: Collect {
    fn call(&self, mc: MutationContext<'gc, '_>, args: Vec<Value<'gc>>) -> CallbackSequence<'gc>;
}

#[derive(Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct Callback<'gc>(pub Gc<'gc, Box<dyn CallbackFn<'gc> + 'gc>>);

impl<'gc> Callback<'gc> {
    pub fn new<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static + Fn(MutationContext<'gc, '_>, Vec<Value<'gc>>) -> CallbackSequence<'gc>,
    {
        Self::new_with(mc, (), move |_, mc, args| f(mc, args))
    }

    pub fn new_with<C, F>(mc: MutationContext<'gc, '_>, c: C, f: F) -> Callback<'gc>
    where
        C: 'gc + Collect,
        F: 'static + Fn(&C, MutationContext<'gc, '_>, Vec<Value<'gc>>) -> CallbackSequence<'gc>,
    {
        #[derive(Collect)]
        #[collect(no_drop, bound = "where C: Collect, F: 'static")]
        struct ContextCallbackFn<C, F>(C, StaticCollect<F>);

        impl<'gc, C, F> CallbackFn<'gc> for ContextCallbackFn<C, F>
        where
            C: 'gc + Collect,
            F: 'static + Fn(&C, MutationContext<'gc, '_>, Vec<Value<'gc>>) -> CallbackSequence<'gc>,
        {
            fn call(
                &self,
                mc: MutationContext<'gc, '_>,
                args: Vec<Value<'gc>>,
            ) -> CallbackSequence<'gc> {
                (self.1).0(&self.0, mc, args)
            }
        }

        Callback(Gc::allocate(
            mc,
            Box::new(ContextCallbackFn(c, StaticCollect(f))),
        ))
    }

    pub fn new_immediate<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static
            + Fn(MutationContext<'gc, '_>, Vec<Value<'gc>>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::new(mc, move |mc, res| CallbackSequence::Immediate(f(mc, res)))
    }

    pub fn new_immediate_with<C, F>(mc: MutationContext<'gc, '_>, c: C, f: F) -> Callback<'gc>
    where
        C: 'gc + Collect,
        F: 'static
            + Fn(
                &C,
                MutationContext<'gc, '_>,
                Vec<Value<'gc>>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::new_with(mc, c, move |c, mc, res| {
            CallbackSequence::Immediate(f(c, mc, res))
        })
    }

    pub fn new_sequence<S, F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        S: 'gc + Sequence<'gc, Output = Result<CallbackReturn<'gc>, Error<'gc>>>,
        F: 'static + Fn(MutationContext<'gc, '_>, Vec<Value<'gc>>) -> Result<S, Error<'gc>>,
    {
        Self::new_sequence_with(mc, (), move |(), mc, res| f(mc, res))
    }

    pub fn new_sequence_with<C, S, F>(mc: MutationContext<'gc, '_>, c: C, f: F) -> Callback<'gc>
    where
        C: 'gc + Collect,
        S: 'gc + Sequence<'gc, Output = Result<CallbackReturn<'gc>, Error<'gc>>>,
        F: 'static + Fn(&C, MutationContext<'gc, '_>, Vec<Value<'gc>>) -> Result<S, Error<'gc>>,
    {
        Self::new_with(mc, c, move |c, mc, res| match f(c, mc, res) {
            Ok(seq) => CallbackSequence::Sequence(seq.boxed()),
            Err(err) => CallbackSequence::Immediate(Err(err)),
        })
    }

    pub fn call(
        &self,
        mc: MutationContext<'gc, '_>,
        args: Vec<Value<'gc>>,
    ) -> CallbackSequence<'gc> {
        self.0.call(mc, args)
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

pub trait ContinuationFn<'gc>: Collect {
    fn call(
        self: Box<Self>,
        mc: MutationContext<'gc, '_>,
        res: Result<Vec<Value<'gc>>, Error<'gc>>,
    ) -> CallbackSequence<'gc>;
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct Continuation<'gc>(Box<dyn ContinuationFn<'gc> + 'gc>);

impl<'gc> Continuation<'gc> {
    pub fn new<F>(cont: F) -> Continuation<'gc>
    where
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                Result<Vec<Value<'gc>>, Error<'gc>>,
            ) -> CallbackSequence<'gc>,
    {
        Self::new_with((), move |_, mc, res| cont(mc, res))
    }

    pub fn new_with<C, F>(context: C, continuation: F) -> Continuation<'gc>
    where
        C: 'gc + Collect,
        F: 'static
            + FnOnce(
                C,
                MutationContext<'gc, '_>,
                Result<Vec<Value<'gc>>, Error<'gc>>,
            ) -> CallbackSequence<'gc>,
    {
        #[derive(Collect)]
        #[collect(no_drop, bound = "where C: Collect, F: 'static")]
        struct ContextContinuationFn<C, F>(C, StaticCollect<F>);

        impl<'gc, C, F> ContinuationFn<'gc> for ContextContinuationFn<C, F>
        where
            C: 'gc + Collect,
            F: 'static
                + FnOnce(
                    C,
                    MutationContext<'gc, '_>,
                    Result<Vec<Value<'gc>>, Error<'gc>>,
                ) -> CallbackSequence<'gc>,
        {
            fn call(
                self: Box<Self>,
                mc: MutationContext<'gc, '_>,
                res: Result<Vec<Value<'gc>>, Error<'gc>>,
            ) -> CallbackSequence<'gc> {
                (self.1).0(self.0, mc, res)
            }
        }

        Continuation(Box::new(ContextContinuationFn(
            context,
            StaticCollect(continuation),
        )))
    }

    pub fn new_immediate<F>(cont: F) -> Continuation<'gc>
    where
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                Result<Vec<Value<'gc>>, Error<'gc>>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::new(move |mc, res| CallbackSequence::Immediate(cont(mc, res)))
    }

    pub fn new_immediate_with<C, F>(context: C, continuation: F) -> Continuation<'gc>
    where
        C: 'gc + Collect,
        F: 'static
            + FnOnce(
                C,
                MutationContext<'gc, '_>,
                Result<Vec<Value<'gc>>, Error<'gc>>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::new_with(context, move |context, mc, res| {
            CallbackSequence::Immediate(continuation(context, mc, res))
        })
    }

    pub fn new_sequence<S, F>(cont: F) -> Continuation<'gc>
    where
        S: 'gc + Sequence<'gc, Output = Result<CallbackReturn<'gc>, Error<'gc>>>,
        F: 'static
            + FnOnce(
                MutationContext<'gc, '_>,
                Result<Vec<Value<'gc>>, Error<'gc>>,
            ) -> Result<S, Error<'gc>>,
    {
        Self::new_sequence_with((), move |(), mc, res| cont(mc, res))
    }

    pub fn new_sequence_with<C, S, F>(context: C, continuation: F) -> Continuation<'gc>
    where
        C: 'gc + Collect,
        S: 'gc + Sequence<'gc, Output = Result<CallbackReturn<'gc>, Error<'gc>>>,
        F: 'static
            + FnOnce(
                C,
                MutationContext<'gc, '_>,
                Result<Vec<Value<'gc>>, Error<'gc>>,
            ) -> Result<S, Error<'gc>>,
    {
        Self::new_with(context, move |context, mc, res| {
            match continuation(context, mc, res) {
                Ok(seq) => CallbackSequence::Sequence(seq.boxed()),
                Err(err) => CallbackSequence::Immediate(Err(err)),
            }
        })
    }

    pub fn call(
        self,
        mc: MutationContext<'gc, '_>,
        res: Result<Vec<Value<'gc>>, Error<'gc>>,
    ) -> CallbackSequence<'gc> {
        self.0.call(mc, res)
    }
}
