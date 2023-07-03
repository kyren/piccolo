use gc_arena::{Collect, Mutation};

use crate::{
    AnyCallback, AnySequence, CallbackReturn, Closure, Context, Error, IntoMultiValue, Sequence,
    SequencePoll, Stack,
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
        I: IntoIterator<Item = Function<'gc>> + Collect + Clone + 'gc,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct Compose<'gc>(Vec<Function<'gc>>);

        impl<'gc> Sequence<'gc> for Compose<'gc> {
            fn poll(
                &mut self,
                _: Context<'gc>,
                _: &mut Stack<'gc>,
            ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                let function = self.0.pop().unwrap();
                let is_tail = self.0.is_empty();
                Ok(SequencePoll::Call { function, is_tail })
            }
        }

        Self::Callback(AnyCallback::from_fn_with(
            mc,
            functions,
            |functions, _, _| {
                let compose = Compose(Vec::from_iter(functions.clone()));
                if compose.0.is_empty() {
                    Ok(CallbackReturn::Return)
                } else {
                    Ok(CallbackReturn::Sequence(AnySequence::new(compose)))
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
            |(f, args), ctx, stack| {
                stack.into_front(ctx, args.clone());
                Ok(CallbackReturn::TailCall(*f, None))
            },
        ))
    }
}
