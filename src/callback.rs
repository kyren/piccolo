use std::{
    fmt,
    hash::{Hash, Hasher},
};

use gc_arena::{unsize, Collect, Gc, Mutation};

use crate::{Context, Error, Function, Stack};

#[derive(Collect)]
#[collect(no_drop)]
pub enum CallbackReturn<'gc> {
    Return,
    Yield(Option<AnyContinuation<'gc>>),
    TailCall(Function<'gc>, Option<AnyContinuation<'gc>>),
    Sequence(AnySequence<'gc>),
}

pub trait Sequence<'gc>: Collect {
    fn step(
        &mut self,
        ctx: Context<'gc>,
        stack: &mut Stack<'gc>,
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
        ctx: Context<'gc>,
        stack: &mut Stack<'gc>,
    ) -> Result<Option<CallbackReturn<'gc>>, Error<'gc>> {
        self.0.step(ctx, stack)
    }
}

pub trait Callback<'gc>: Collect {
    fn call(
        &self,
        ctx: Context<'gc>,
        stack: &mut Stack<'gc>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>;
}

// Represents a callback as a single pointer with an inline VTable header.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop, bound = "")]
pub struct AnyCallback<'gc>(Gc<'gc, Header<'gc>>);

struct Header<'gc> {
    call: unsafe fn(
        *const (),
        Context<'gc>,
        &mut Stack<'gc>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
}

impl<'gc> AnyCallback<'gc> {
    pub fn new<C: Callback<'gc> + 'gc>(mc: &Mutation<'gc>, callback: C) -> Self {
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

            fn trace(&self, cc: &gc_arena::Collection) {
                self.callback.trace(cc)
            }
        }

        let hc = Gc::new(
            mc,
            HeaderCallback {
                header: Header {
                    call: |ptr, ctx, stack| unsafe {
                        let hc = ptr as *const HeaderCallback<C>;
                        ((*hc).callback).call(ctx, stack)
                    },
                },
                callback,
            },
        );

        Self(unsafe { Gc::cast::<Header>(hc) })
    }

    pub fn from_fn<F>(mc: &Mutation<'gc>, call: F) -> AnyCallback<'gc>
    where
        F: 'static + Fn(Context<'gc>, &mut Stack<'gc>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::from_fn_with(mc, (), move |_, ctx, stack| call(ctx, stack))
    }

    pub fn from_fn_with<R, F>(mc: &Mutation<'gc>, root: R, call: F) -> AnyCallback<'gc>
    where
        R: 'gc + Collect,
        F: 'static
            + Fn(&R, Context<'gc>, &mut Stack<'gc>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct RootCallback<R, F> {
            root: R,
            #[collect(require_static)]
            call: F,
        }

        impl<'gc, R, F> Callback<'gc> for RootCallback<R, F>
        where
            R: 'gc + Collect,
            F: 'static
                + Fn(&R, Context<'gc>, &mut Stack<'gc>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
        {
            fn call(
                &self,
                ctx: Context<'gc>,
                stack: &mut Stack<'gc>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
                (self.call)(&self.root, ctx, stack)
            }
        }

        AnyCallback::new(mc, RootCallback { root, call })
    }

    pub fn as_ptr(self) -> *const () {
        Gc::as_ptr(self.0) as *const ()
    }

    pub fn call(
        self,
        ctx: Context<'gc>,
        stack: &mut Stack<'gc>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
        unsafe { (self.0.call)(Gc::as_ptr(self.0) as *const (), ctx, stack) }
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
        ctx: Context<'gc>,
        stack: &mut Stack<'gc>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>;

    fn continue_err(
        &self,
        ctx: Context<'gc>,
        stack: &mut Stack<'gc>,
        error: Error<'gc>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>;
}

#[derive(Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct AnyContinuation<'gc>(pub Gc<'gc, dyn Continuation<'gc>>);

impl<'gc> AnyContinuation<'gc> {
    pub fn new(mc: &Mutation<'gc>, continuation: impl Continuation<'gc> + 'gc) -> Self {
        Self(unsize!(Gc::new(mc, continuation) => dyn Continuation<'gc>))
    }

    pub fn from_fns<FO, FE>(
        mc: &Mutation<'gc>,
        continue_ok: FO,
        continue_err: FE,
    ) -> AnyContinuation<'gc>
    where
        FO: 'static + Fn(Context<'gc>, &mut Stack<'gc>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
        FE: 'static
            + Fn(Context<'gc>, &mut Stack<'gc>, Error<'gc>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::from_fns_with(
            mc,
            (),
            move |_, ctx, stack| continue_ok(ctx, stack),
            move |_, ctx, stack, error| continue_err(ctx, stack, error),
        )
    }

    pub fn from_fns_with<R, FO, FE>(
        mc: &Mutation<'gc>,
        root: R,
        continue_ok: FO,
        continue_err: FE,
    ) -> AnyContinuation<'gc>
    where
        R: 'gc + Collect,
        FO: 'static
            + Fn(&R, Context<'gc>, &mut Stack<'gc>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
        FE: 'static
            + Fn(
                &R,
                Context<'gc>,
                &mut Stack<'gc>,
                Error<'gc>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct RootContinuation<R, FO, FE> {
            root: R,
            #[collect(require_static)]
            continue_ok: FO,
            #[collect(require_static)]
            continue_err: FE,
        }

        impl<'gc, R, FO, FE> Continuation<'gc> for RootContinuation<R, FO, FE>
        where
            R: 'gc + Collect,
            FO: 'static
                + Fn(&R, Context<'gc>, &mut Stack<'gc>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
            FE: 'static
                + Fn(
                    &R,
                    Context<'gc>,
                    &mut Stack<'gc>,
                    Error<'gc>,
                ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
        {
            fn continue_ok(
                &self,
                ctx: Context<'gc>,
                stack: &mut Stack<'gc>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
                (self.continue_ok)(&self.root, ctx, stack)
            }

            fn continue_err(
                &self,
                ctx: Context<'gc>,
                stack: &mut Stack<'gc>,
                error: Error<'gc>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
                (self.continue_err)(&self.root, ctx, stack, error)
            }
        }

        AnyContinuation(unsize!(Gc::new(
            mc,
            RootContinuation {
                root,
                continue_ok,
                continue_err,
            }
        ) => dyn Continuation<'gc>))
    }

    pub fn from_ok_fn<F>(mc: &Mutation<'gc>, continue_ok: F) -> AnyContinuation<'gc>
    where
        F: 'static + Fn(Context<'gc>, &mut Stack<'gc>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::from_fns_with(
            mc,
            (),
            move |_, ctx, stack| continue_ok(ctx, stack),
            move |_, _, _, error| Err(error),
        )
    }

    pub fn from_ok_fn_with<R, F>(
        mc: &Mutation<'gc>,
        root: R,
        continue_ok: F,
    ) -> AnyContinuation<'gc>
    where
        R: Collect + 'gc,
        F: 'static
            + Fn(&R, Context<'gc>, &mut Stack<'gc>) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::from_fns_with(
            mc,
            root,
            move |root, ctx, stack| continue_ok(root, ctx, stack),
            move |_, _, _, error| Err(error),
        )
    }

    pub fn continue_ok(
        &self,
        ctx: Context<'gc>,
        stack: &mut Stack<'gc>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
        self.0.continue_ok(ctx, stack)
    }

    pub fn continue_err(
        &self,
        ctx: Context<'gc>,
        stack: &mut Stack<'gc>,
        error: Error<'gc>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
        self.0.continue_err(ctx, stack, error)
    }
}

#[cfg(test)]
mod tests {
    use gc_arena::{Arena, Rootable};

    use crate::{CallbackReturn, State};

    use super::*;

    #[test]
    fn test_dyn_callback() {
        #[derive(Collect)]
        #[collect(require_static)]
        struct CB(i64);

        impl<'gc> Callback<'gc> for CB {
            fn call(
                &self,
                ctx: Context<'gc>,
                stack: &mut Stack<'gc>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
                stack.into_front(ctx, self.0);
                Ok(CallbackReturn::Return)
            }
        }

        let arena = Arena::<Rootable![State<'gc>]>::new(Default::default(), |mc| State::new(mc));
        arena.mutate(|mc, state| {
            let ctx = state.ctx(mc);
            let dyn_callback = AnyCallback::new(mc, CB(17));
            let mut stack = Stack::new();
            assert!(dyn_callback.call(ctx, &mut stack).is_ok());
            assert!(matches!(stack.from_front(ctx).unwrap(), 17));
        });
    }
}
