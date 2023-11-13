use gc_arena::{Collect, Gc, Mutation};

use crate::{
    AnyCallback, AnySequence, CallbackReturn, Closure, Context, Error, Fuel, IntoMultiValue,
    Sequence, SequencePoll, Stack,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(no_drop)]
pub enum Function<'gc> {
    Closure(Closure<'gc>),
    Callback(AnyCallback<'gc>),
}

impl<'gc> From<Closure<'gc>> for Function<'gc> {
    fn from(closure: Closure<'gc>) -> Self {
        Self::Closure(closure)
    }
}

impl<'gc> From<AnyCallback<'gc>> for Function<'gc> {
    fn from(callback: AnyCallback<'gc>) -> Self {
        Self::Callback(callback)
    }
}

impl<'gc> Function<'gc> {
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
                &mut self,
                _: Context<'gc>,
                _: &mut Fuel,
                _: Stack<'gc, '_>,
            ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                let fns = (*self.0).as_ref();
                let function = fns[self.1];
                self.1 += 1;
                let is_tail = self.1 == fns.len();
                Ok(SequencePoll::Call { function, is_tail })
            }
        }

        Self::Callback(AnyCallback::from_fn_with(
            mc,
            Gc::new(mc, functions),
            |functions, ctx, _, _| {
                if (**functions).as_ref().is_empty() {
                    Ok(CallbackReturn::Return)
                } else {
                    Ok(CallbackReturn::Sequence(AnySequence::new(
                        &ctx,
                        Compose(*functions, 0),
                    )))
                }
            },
        ))
    }

    pub fn bind<A>(self, mc: &Mutation<'gc>, args: A) -> Self
    where
        A: IntoMultiValue<'gc> + Collect + Clone + 'gc,
    {
        Self::Callback(AnyCallback::from_fn_with(
            mc,
            (self, args),
            |(f, args), ctx, fuel, mut stack| {
                stack.into_front(ctx, args.clone());
                match *f {
                    Function::Closure(c) => Ok(CallbackReturn::Call {
                        function: c.into(),
                        then: None,
                    }),
                    Function::Callback(c) => c.call(ctx, fuel, stack),
                }
            },
        ))
    }
}
