use gc_arena::{Collect, MutationContext};

use super::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Collect)]
#[collect(no_drop)]
pub enum Flatten<'gc, S>
where
    S: Sequence<'gc>,
    S::Output: Sequence<'gc>,
{
    First(S),
    Second(S::Output),
}

impl<'gc, S> Flatten<'gc, S>
where
    S: Sequence<'gc>,
    S::Output: Sequence<'gc>,
{
    pub fn new(s: S) -> Flatten<'gc, S> {
        Flatten::First(s)
    }
}

impl<'gc, S> Sequence<'gc> for Flatten<'gc, S>
where
    S: Sequence<'gc>,
    S::Output: Sequence<'gc>,
{
    type Output = <S::Output as Sequence<'gc>>::Output;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            Flatten::First(f) => match f.step(mc) {
                Some(s) => {
                    *self = Flatten::Second(s);
                    None
                }
                None => None,
            },
            Flatten::Second(s) => s.step(mc),
        }
    }
}
