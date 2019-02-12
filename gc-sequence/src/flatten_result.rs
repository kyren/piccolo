use gc_arena::{Collect, MutationContext};

use crate::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Collect)]
#[collect(empty_drop)]
pub enum FlattenOk<S, I> {
    First(S),
    Second(I),
}

impl<S, I> FlattenOk<S, I> {
    pub fn new(s: S) -> FlattenOk<S, I> {
        FlattenOk::First(s)
    }
}

impl<'gc, S, I, E, I2> Sequence<'gc> for FlattenOk<S, I>
where
    S: Sequence<'gc, Output = Result<I, E>>,
    I: Sequence<'gc, Output = Result<I2, E>>,
{
    type Output = Result<I2, E>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            FlattenOk::First(f) => match f.step(mc) {
                Some(Ok(s)) => {
                    *self = FlattenOk::Second(s);
                    None
                }
                Some(Err(err)) => Some(Err(err)),
                None => None,
            },
            FlattenOk::Second(s) => s.step(mc),
        }
    }
}
