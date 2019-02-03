use gc_arena::{Collect, MutationContext};

use crate::{IntoSequence, LuaContext, Sequence};

#[must_use = "sequences do nothing unless stepped"]
#[derive(Collect)]
#[collect(empty_drop)]
pub enum Flatten<'gc, S>
where
    S: Sequence<'gc>,
    S::Item: IntoSequence<'gc, Error = S::Error>,
{
    First(S),
    Second(<S::Item as IntoSequence<'gc>>::Sequence),
}

impl<'gc, S> Flatten<'gc, S>
where
    S: Sequence<'gc>,
    S::Item: IntoSequence<'gc, Error = S::Error>,
{
    pub fn new(s: S) -> Flatten<'gc, S> {
        Flatten::First(s)
    }
}

impl<'gc, S> Sequence<'gc> for Flatten<'gc, S>
where
    S: Sequence<'gc>,
    S::Item: IntoSequence<'gc, Error = S::Error>,
{
    type Item = <S::Item as IntoSequence<'gc>>::Item;
    type Error = <S::Item as IntoSequence<'gc>>::Error;

    fn step(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Self::Error>> {
        match self {
            Flatten::First(f) => match f.step(mc, lc) {
                Some(Ok(s)) => {
                    *self = Flatten::Second(s.into_sequence());
                    None
                }
                Some(Err(err)) => Some(Err(err)),
                None => None,
            },
            Flatten::Second(s) => s.step(mc, lc),
        }
    }
}
