use std::{
    fmt,
    hash::{Hash, Hasher},
};

use gc_arena::{
    static_send::{static_send, StaticSend},
    unsize, Collect, Gc, MutationContext, Root, Rootable,
};

use crate::{Error, FromMultiValue, Function, IntoMultiValue, Value};

#[derive(Collect)]
#[collect(no_drop)]
pub enum CallbackReturn<'gc> {
    Return,
    Yield(Option<AnyContinuation<'gc>>),
    TailCall(Function<'gc>, Option<AnyContinuation<'gc>>),
}

pub trait Sequence<'gc>: Collect {
    fn step(
        &mut self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<Option<CallbackReturn<'gc>>, Error<'gc>>;
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct AnySequence<'gc>(pub Box<StaticSend<dyn Sequence<'gc> + 'gc>>);

impl<'gc> AnySequence<'gc> {
    pub fn new<R: for<'a> Rootable<'a>>(sequence: Root<'gc, R>) -> Self
    where
        Root<'gc, R>: Sequence<'gc>,
        Root<'static, R>: Send,
    {
        Self(Box::new(static_send::<R>(sequence)))
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

// Represents a callback as a single pointer with an inline VTable header.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop, bound = "")]
pub struct AnyCallback<'gc>(Gc<'gc, Header<'gc>>);

struct Header<'gc> {
    call: unsafe fn(
        *const (),
        MutationContext<'gc, '_>,
        &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>>,
}

impl<'gc> AnyCallback<'gc> {
    pub fn new<C>(mc: MutationContext<'gc, '_>, callback: Root<'gc, C>) -> Self
    where
        C: for<'a> Rootable<'a>,
        Root<'static, C>: Send,
        Root<'gc, C>: Callback<'gc>,
    {
        #[repr(C)]
        struct HeaderCallback<'gc, C> {
            header: Header<'gc>,
            callback: C,
        }

        // SAFETY: We can't auto-implement `Collect` due to the function pointer lifetimes, but
        // function pointers can't hold any data. It would be nice if function pointers could have
        // higher rank `for<'gc>` lifetimes.
        unsafe impl<'gc, C: Collect> Collect for HeaderCallback<'gc, C> {
            fn needs_trace() -> bool
            where
                Self: Sized,
            {
                C::needs_trace()
            }

            fn trace(&self, cc: gc_arena::CollectionContext) {
                self.callback.trace(cc)
            }
        }

        let hc = Gc::new(
            mc,
            HeaderCallback {
                header: Header {
                    call: |ptr, mc, stack| unsafe {
                        let hc = ptr as *const HeaderCallback<Root<'gc, C>>;
                        ((*hc).callback).call(mc, stack)
                    },
                },
                callback,
            },
        );

        Self(unsafe { Gc::cast::<Header>(hc) })
    }

    pub fn as_ptr(self) -> *const () {
        Gc::as_ptr(self.0) as *const ()
    }

    pub fn call(
        self,
        mc: MutationContext<'gc, '_>,
        stack: &mut Vec<Value<'gc>>,
    ) -> Result<CallbackMode<'gc>, Error<'gc>> {
        unsafe { (self.0.call)(Gc::as_ptr(self.0) as *const (), mc, stack) }
    }

    pub fn from_fn(
        mc: MutationContext<'gc, '_>,
        call: impl Fn(
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>
            + Send
            + 'static,
    ) -> AnyCallback<'gc> {
        Self::from_fn_with::<Rootable!['a => ()]>(mc, (), move |_, mc, stack| call(mc, stack))
    }

    pub fn from_fn_with<C>(
        mc: MutationContext<'gc, '_>,
        context: Root<'gc, C>,
        call: impl Fn(
                &Root<'gc, C>,
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>
            + Send
            + 'static,
    ) -> AnyCallback<'gc>
    where
        C: for<'a> Rootable<'a>,
        Root<'gc, C>: Collect,
        Root<'static, C>: Send,
    {
        #[derive(Collect)]
        #[collect(no_drop, bound = "")]
        struct ContextCallback<'gc, C, F>
        where
            C: for<'a> Rootable<'a>,
        {
            context: Root<'gc, C>,
            #[collect(require_static)]
            call: F,
        }

        impl<'gc, C, F> Callback<'gc> for ContextCallback<'gc, C, F>
        where
            C: for<'a> Rootable<'a>,
            Root<'gc, C>: Collect,
            F: Fn(
                    &Root<'gc, C>,
                    MutationContext<'gc, '_>,
                    &mut Vec<Value<'gc>>,
                ) -> Result<CallbackMode<'gc>, Error<'gc>>
                + 'static,
        {
            fn call(
                &self,
                mc: MutationContext<'gc, '_>,
                stack: &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>> {
                (self.call)(&self.context, mc, stack)
            }
        }

        AnyCallback::new::<Rootable!['a => ContextCallback<'a, C, _>]>(
            mc,
            ContextCallback { context, call },
        )
    }

    pub fn from_immediate_fn<A, R>(
        mc: MutationContext<'gc, '_>,
        call: impl Fn(MutationContext<'gc, '_>, A) -> Result<(CallbackReturn<'gc>, R), Error<'gc>>
            + Send
            + 'static,
    ) -> AnyCallback<'gc>
    where
        A: FromMultiValue<'gc>,
        R: IntoMultiValue<'gc>,
    {
        Self::from_immediate_fn_with::<Rootable!['a => ()], _, _>(mc, (), move |_, mc, args| {
            call(mc, args)
        })
    }

    pub fn from_immediate_fn_with<C, A, R>(
        mc: MutationContext<'gc, '_>,
        context: Root<'gc, C>,
        call: impl Fn(
                &Root<'gc, C>,
                MutationContext<'gc, '_>,
                A,
            ) -> Result<(CallbackReturn<'gc>, R), Error<'gc>>
            + Send
            + 'static,
    ) -> AnyCallback<'gc>
    where
        C: for<'a> Rootable<'a>,
        Root<'static, C>: Send,
        A: FromMultiValue<'gc>,
        R: IntoMultiValue<'gc>,
    {
        Self::from_fn_with::<C>(mc, context, move |context, mc, stack| {
            let args = A::from_multi_value(mc, stack.drain(..))?;
            let (ret, vals) = call(context, mc, args)?;
            stack.extend(vals.into_multi_value(mc));
            Ok(ret.into())
        })
    }
}

impl<'gc> fmt::Debug for AnyCallback<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Callback").field(&self.as_ptr()).finish()
    }
}

impl<'gc> PartialEq for AnyCallback<'gc> {
    fn eq(&self, other: &AnyCallback<'gc>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'gc> Eq for AnyCallback<'gc> {}

impl<'gc> Hash for AnyCallback<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
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
pub struct AnyContinuation<'gc>(pub Gc<'gc, StaticSend<dyn Continuation<'gc> + 'gc>>);

impl<'gc> AnyContinuation<'gc> {
    pub fn new<C>(mc: MutationContext<'gc, '_>, continuation: Root<'gc, C>) -> Self
    where
        C: for<'a> Rootable<'a>,
        Root<'gc, C>: Continuation<'gc>,
        Root<'static, C>: Send,
    {
        Self(
            unsize!(Gc::new(mc, static_send::<C>(continuation)) => StaticSend<dyn Continuation<'gc>>),
        )
    }

    pub fn from_ok_fn(
        mc: MutationContext<'gc, '_>,
        continue_ok: impl Fn(
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>
            + Send
            + 'static,
    ) -> AnyContinuation<'gc>
where {
        Self::from_fns_with::<Rootable!['a => ()]>(
            mc,
            (),
            move |_, mc, stack| continue_ok(mc, stack),
            move |_, _, _, error| Err(error),
        )
    }

    pub fn from_ok_fn_with<C>(
        mc: MutationContext<'gc, '_>,
        context: Root<'gc, C>,
        continue_ok: impl Fn(
                &Root<'gc, C>,
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>
            + Send
            + 'static,
    ) -> AnyContinuation<'gc>
    where
        C: for<'a> Rootable<'a>,
        Root<'static, C>: Send,
    {
        Self::from_fns_with::<C>(
            mc,
            context,
            move |context, mc, stack| continue_ok(context, mc, stack),
            move |_, _, _, error| Err(error),
        )
    }

    pub fn from_fns(
        mc: MutationContext<'gc, '_>,
        continue_ok: impl Fn(
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>
            + Send
            + 'static,
        continue_err: impl Fn(
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
                Error<'gc>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>
            + Send
            + 'static,
    ) -> AnyContinuation<'gc>
where {
        Self::from_fns_with::<Rootable!['a => ()]>(
            mc,
            (),
            move |_, mc, stack| continue_ok(mc, stack),
            move |_, mc, stack, error| continue_err(mc, stack, error),
        )
    }

    pub fn from_fns_with<C>(
        mc: MutationContext<'gc, '_>,
        context: Root<'gc, C>,
        continue_ok: impl Fn(
                &Root<'gc, C>,
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>
            + Send
            + 'static,
        continue_err: impl Fn(
                &Root<'gc, C>,
                MutationContext<'gc, '_>,
                &mut Vec<Value<'gc>>,
                Error<'gc>,
            ) -> Result<CallbackMode<'gc>, Error<'gc>>
            + Send
            + 'static,
    ) -> AnyContinuation<'gc>
    where
        C: for<'a> Rootable<'a>,
        Root<'static, C>: Send,
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
            FO: Fn(
                    &C,
                    MutationContext<'gc, '_>,
                    &mut Vec<Value<'gc>>,
                ) -> Result<CallbackMode<'gc>, Error<'gc>>
                + 'static,
            FE: Fn(
                    &C,
                    MutationContext<'gc, '_>,
                    &mut Vec<Value<'gc>>,
                    Error<'gc>,
                ) -> Result<CallbackMode<'gc>, Error<'gc>>
                + 'static,
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

        AnyContinuation::new::<Rootable!['a => ContextContinuation<Root<'a, C>, _, _>]>(
            mc,
            ContextContinuation {
                context,
                continue_ok,
                continue_err,
            },
        )
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

#[cfg(test)]
mod tests {
    use crate::CallbackReturn;

    use gc_arena::rootless_arena;

    use super::*;

    #[test]
    fn test_dyn_callback() {
        rootless_arena(|mc| {
            #[derive(Collect)]
            #[collect(require_static)]
            struct CB(i64);

            impl<'gc> Callback<'gc> for CB {
                fn call(
                    &self,
                    _: MutationContext<'gc, '_>,
                    stack: &mut Vec<Value<'gc>>,
                ) -> Result<CallbackMode<'gc>, Error<'gc>> {
                    stack.push(Value::Integer(self.0));
                    Ok(CallbackReturn::Return.into())
                }
            }

            let dyn_callback = AnyCallback::new::<Rootable!['a => CB]>(mc, CB(17));

            let mut stack = Vec::new();
            assert!(dyn_callback.call(mc, &mut stack).is_ok());
            assert!(matches!(stack[0], Value::Integer(17)));
        })
    }
}
