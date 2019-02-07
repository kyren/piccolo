use gc_arena::{Collect, MutationContext};

use crate::{LuaContext, Sequence, SequenceExt};

pub trait IntoSequence<'gc> {
    type Item;
    type Error;
    type Sequence: Sequence<'gc, Item = Self::Item, Error = Self::Error>;

    fn into_sequence(self) -> Self::Sequence;

    fn into_boxed_sequence(self) -> Box<Sequence<'gc, Item = Self::Item, Error = Self::Error> + 'gc>
    where
        Self: 'gc + Sized,
    {
        self.into_sequence().boxed()
    }
}

impl<'gc, I: Collect, E: Collect> IntoSequence<'gc> for Result<I, E> {
    type Item = I;
    type Error = E;
    type Sequence = SequenceResult<I, E>;

    fn into_sequence(self) -> SequenceResult<I, E> {
        SequenceResult(Some(self))
    }
}

impl<'gc, S: Sequence<'gc>> IntoSequence<'gc> for S {
    type Item = S::Item;
    type Error = S::Error;
    type Sequence = S;

    fn into_sequence(self) -> S {
        self
    }
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct SequenceResult<I, E>(Option<Result<I, E>>);

impl<'gc, I: Collect, E: Collect> Sequence<'gc> for SequenceResult<I, E> {
    type Item = I;
    type Error = E;

    fn step(&mut self, _: MutationContext<'gc, '_>, _: LuaContext<'gc>) -> Option<Result<I, E>> {
        Some(self.0.take().expect("cannot step a finished sequence"))
    }
}
