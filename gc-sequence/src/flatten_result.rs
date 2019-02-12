use gc_arena::{Collect, MutationContext};

use crate::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Collect)]
#[collect(empty_drop)]
pub enum FlattenResult<S, I> {
    First(S),
    Second(I),
}

impl<S, I> FlattenResult<S, I> {
    pub fn new(s: S) -> FlattenResult<S, I> {
        FlattenResult::First(s)
    }
}

impl<'gc, S, I, E, I2> Sequence<'gc> for FlattenResult<S, I>
where
    S: Sequence<'gc, Output = Result<I, E>>,
    I: Sequence<'gc, Output = Result<I2, E>>,
{
    type Output = Result<I2, E>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            FlattenResult::First(f) => match f.step(mc) {
                Some(Ok(s)) => {
                    *self = FlattenResult::Second(s);
                    None
                }
                Some(Err(err)) => Some(Err(err)),
                None => None,
            },
            FlattenResult::Second(s) => s.step(mc),
        }
    }
}
