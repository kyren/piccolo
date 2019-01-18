use gc_arena::{Collect, MutationContext};

use crate::error::Error;
use crate::lua::LuaContext;

use super::Sequence;

pub trait IntoSequence<'gc> {
    type Item;
    type Sequence: Sequence<'gc, Item = Self::Item>;

    fn into_sequence(self) -> Self::Sequence;
}

impl<'gc, R: Collect> IntoSequence<'gc> for Result<R, Error> {
    type Item = R;
    type Sequence = SequenceResult<R>;

    fn into_sequence(self) -> SequenceResult<R> {
        SequenceResult(Some(self))
    }
}

impl<'gc, S: Sequence<'gc>> IntoSequence<'gc> for S {
    type Item = S::Item;
    type Sequence = S;

    fn into_sequence(self) -> S {
        self
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct SequenceResult<R>(Option<Result<R, Error>>);

impl<'gc, R: Collect> Sequence<'gc> for SequenceResult<R> {
    type Item = R;

    fn pump(
        &mut self,
        _: MutationContext<'gc, '_>,
        _: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        Some(self.0.take().expect("cannot pump a finished sequence"))
    }
}
