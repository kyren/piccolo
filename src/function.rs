use core::pin::Pin;

use gc_arena::{Collect, Gc, Mutation};

use crate::{
    BoxSequence, Callback, CallbackReturn, Closure, Context, Error, Execution, IntoMultiValue,
    Sequence, SequencePoll, Stack,
};

/// Any callable Lua value (either a [`Closure`] or a [`Callback`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> From<Closure<'gc>> for Function<'gc> {
    fn from(closure: Closure<'gc>) -> Self {
        Self::Closure(closure)
    }
}

impl<'gc> From<Callback<'gc>> for Function<'gc> {
    fn from(callback: Callback<'gc>) -> Self {
        Self::Callback(callback)
    }
}

impl<'gc> Function<'gc> {
    /// Compose functions together to form a single function.
    ///
    /// If given an array of functions `[f, g, h]`, then this will return a function equivalent to
    /// one that calls `h(g(f(...)))`. Note that functions compose "backwards" from how you might
    /// think, the first function given will be called first, and the second function called second,
    /// etc.
    pub fn compose<I>(mc: &Mutation<'gc>, functions: I) -> Self
    where
        I: AsRef<[Function<'gc>]> + Collect + 'gc,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct Compose<'gc, I: 'gc>(Gc<'gc, I>, usize);

        impl<'gc, I> Sequence<'gc> for Compose<'gc, I>
        where
            I: AsRef<[Function<'gc>]> + Collect,
        {
            fn poll(
                self: Pin<&mut Self>,
                _: Context<'gc>,
                _: Execution<'gc, '_>,
                _: Stack<'gc, '_>,
            ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                let this = self.get_mut();
                let fns = (*this.0).as_ref();
                let function = fns[this.1];
                this.1 += 1;
                if this.1 == fns.len() {
                    Ok(SequencePoll::TailCall(function))
                } else {
                    Ok(SequencePoll::Call {
                        function,
                        bottom: 0,
                    })
                }
            }
        }

        Self::Callback(Callback::from_fn_with(
            mc,
            Gc::new(mc, functions),
            |functions, ctx, _, _| {
                if (**functions).as_ref().is_empty() {
                    Ok(CallbackReturn::Return)
                } else {
                    Ok(CallbackReturn::Sequence(BoxSequence::new(
                        &ctx,
                        Compose(*functions, 0),
                    )))
                }
            },
        ))
    }

    /// Bind arguments to the given function and return a new function.
    ///
    /// If called on a function `f` with arguments `[a, b, c]`, then this will produce a function
    /// that calls `f(a, b, c, ...)`.
    pub fn bind<A>(self, mc: &Mutation<'gc>, args: A) -> Self
    where
        A: IntoMultiValue<'gc> + Collect + Clone + 'gc,
    {
        Self::Callback(Callback::from_fn_with(
            mc,
            (self, args),
            |(f, args), ctx, exec, mut stack| {
                stack.into_front(ctx, args.clone());
                match *f {
                    Function::Closure(c) => Ok(CallbackReturn::Call {
                        function: c.into(),
                        then: None,
                    }),
                    Function::Callback(c) => c.call(ctx, exec, stack),
                }
            },
        ))
    }
}
