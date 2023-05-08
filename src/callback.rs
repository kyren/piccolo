use std::{
    fmt::{self, Debug},
    hash::{Hash, Hasher},
};

use gc_arena::{unsize, Collect, Gc, MutationContext};

use crate::{Error, Function, Value};

#[derive(Collect)]
#[collect(no_drop)]
pub enum CallbackReturn<'gc> {
    Return,
    Yield,
    TailCall {
        function: Function<'gc>,
        continuation: Option<AnyContinuation<'gc>>,
    },
}

pub type CallbackResult<'gc> = Result<CallbackReturn<'gc>, Error<'gc>>;

pub trait Sequence<'gc>: Collect {
    fn step(
        &mut self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<Option<CallbackReturn<'gc>>, Error<'gc>>;
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct AnySequence<'gc>(pub Box<dyn Sequence<'gc> + 'gc>);

impl<'gc, S> From<S> for AnySequence<'gc>
where
    S: Sequence<'gc> + 'gc,
{
    fn from(v: S) -> Self {
        Self(Box::new(v))
    }
}

impl<'gc> AnySequence<'gc> {
    pub fn new(sequence: impl Sequence<'gc> + 'gc) -> Self {
        Self(Box::new(sequence))
    }

    pub fn step(
        &mut self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<Option<CallbackReturn<'gc>>, Error<'gc>> {
        self.0.step(mc, stack)
    }
}

pub enum CallbackMode<'gc> {
    Immediate(CallbackReturn<'gc>),
    Sequence(AnySequence<'gc>),
}

impl<'gc> From<CallbackReturn<'gc>> for CallbackMode<'gc> {
    fn from(v: CallbackReturn<'gc>) -> Self {
        Self::Immediate(v)
    }
}

impl<'gc> From<AnySequence<'gc>> for CallbackMode<'gc> {
    fn from(v: AnySequence<'gc>) -> Self {
        Self::Sequence(v)
    }
}

pub trait Callback<'gc>: Collect {
    fn call(
        &self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>>;
}

#[derive(Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct AnyCallback<'gc>(pub Gc<'gc, dyn Callback<'gc>>);

impl<'gc> AnyCallback<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>, callback: impl Callback<'gc> + 'gc) -> Self {
        Self(unsize!(Gc::allocate(mc, callback) => dyn Callback<'gc>))
    }

    pub fn from_fn<F>(mc: MutationContext<'gc, '_>, call: F) -> AnyCallback<'gc>
    where
        F: 'static
            + Fn(
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>,
    {
        Self::from_fn_with(mc, (), move |_, mc, stack| call(mc, stack))
    }

    pub fn from_fn_with<C, F>(mc: MutationContext<'gc, '_>, context: C, call: F) -> AnyCallback<'gc>
    where
        C: 'gc + Collect,
        F: 'static
            + Fn(
                &C,
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct ContextCallback<C, F> {
            context: C,
            #[collect(require_static)]
            call: F,
        }

        impl<'gc, C, F> Callback<'gc> for ContextCallback<C, F>
        where
            C: 'gc + Collect,
            F: 'static
                + Fn(
                    &C,
                    MutationContext<'gc, '_>,
                    &mut Vec<Value<'gc>>,
                ) -> Result<CallbackMode<'gc>, Error<'gc>>,
        {
            fn call(
                &self,
                mc: MutationContext<'gc, '_>,
                stack: &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>> {
                (self.call)(&self.context, mc, stack)
            }
        }

        AnyCallback(unsize!(Gc::allocate(
            mc,
            ContextCallback { context, call },
        ) => dyn Callback<'gc>))
    }

    pub fn call(
        &self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>> {
        self.0.call(mc, stack)
    }
}

impl<'gc> Debug for AnyCallback<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Callback")
            .field(&Gc::as_ptr(self.0))
            .finish()
    }
}

impl<'gc> PartialEq for AnyCallback<'gc> {
    fn eq(&self, other: &AnyCallback<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for AnyCallback<'gc> {}

impl<'gc> Hash for AnyCallback<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}

pub trait Continuation<'gc>: Collect {
    fn continue_ok(
        &self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>>;

    fn continue_err(
        &self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
        error: Error<'gc>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>>;
}

#[derive(Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct AnyContinuation<'gc>(pub Gc<'gc, dyn Continuation<'gc>>);

impl<'gc> AnyContinuation<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>, continuation: impl Continuation<'gc> + 'gc) -> Self {
        Self(unsize!(Gc::allocate(mc, continuation) => dyn Continuation<'gc>))
    }

    pub fn from_ok_fn<F>(mc: MutationContext<'gc, '_>, continue_ok: F) -> AnyContinuation<'gc>
    where
        F: 'static
            + Fn(
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>,
    {
        Self::from_fns_with(
            mc,
            (),
            move |_, mc, stack| continue_ok(mc, stack),
            move |_, _, _, error| Err(error),
        )
    }

    pub fn from_ok_fn_with<C, F>(
        mc: MutationContext<'gc, '_>,
        context: C,
        continue_ok: F,
    ) -> AnyContinuation<'gc>
    where
        C: Collect + 'gc,
        F: 'static
            + Fn(
                &C,
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>,
    {
        Self::from_fns_with(
            mc,
            context,
            move |context, mc, stack| continue_ok(context, mc, stack),
            move |_, _, _, error| Err(error),
        )
    }

    pub fn from_fns<FO, FE>(
        mc: MutationContext<'gc, '_>,
        continue_ok: FO,
        continue_err: FE,
    ) -> AnyContinuation<'gc>
    where
        FO: 'static
            + Fn(
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>,
        FE: 'static
            + Fn(
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
                Error<'gc>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>,
    {
        Self::from_fns_with(
            mc,
            (),
            move |_, mc, stack| continue_ok(mc, stack),
            move |_, mc, stack, error| continue_err(mc, stack, error),
        )
    }

    pub fn from_fns_with<C, FO, FE>(
        mc: MutationContext<'gc, '_>,
        context: C,
        continue_ok: FO,
        continue_err: FE,
    ) -> AnyContinuation<'gc>
    where
        C: 'gc + Collect,
        FO: 'static
            + Fn(
                &C,
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>,
        FE: 'static
            + Fn(
                &C,
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
                Error<'gc>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct ContextContinuation<C, FO, FE> {
            context: C,
            #[collect(require_static)]
            continue_ok: FO,
            #[collect(require_static)]
            continue_err: FE,
        }

        impl<'gc, C, FO, FE> Continuation<'gc> for ContextContinuation<C, FO, FE>
        where
            C: 'gc + Collect,
            FO: 'static
                + Fn(
                    &C,
                    MutationContext<'gc, '_>,
                    &mut Vec<Value<'gc>>,
                ) -> Result<CallbackMode<'gc>, Error<'gc>>,
            FE: 'static
                + Fn(
                    &C,
                    MutationContext<'gc, '_>,
                    &mut Vec<Value<'gc>>,
                    Error<'gc>,
                ) -> Result<CallbackMode<'gc>, Error<'gc>>,
        {
            fn continue_ok(
                &self,
                mc: MutationContext<'gc, '_>,
                stack: &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>> {
                (self.continue_ok)(&self.context, mc, stack)
            }

            fn continue_err(
                &self,
                mc: MutationContext<'gc, '_>,
                stack: &mut Vec<Value<'gc>>,
                error: Error<'gc>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>> {
                (self.continue_err)(&self.context, mc, stack, error)
            }
        }

        AnyContinuation(unsize!(Gc::allocate(
            mc,
            ContextContinuation {
                context,
                continue_ok,
                continue_err,
            }
        ) => dyn Continuation<'gc>))
    }

    pub fn continue_ok(
        &self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>> {
        self.0.continue_ok(mc, stack)
    }

    pub fn continue_err(
        &self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
        error: Error<'gc>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>> {
        self.0.continue_err(mc, stack, error)
    }
}
