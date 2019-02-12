use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct Map<S, F>(S, Option<StaticCollect<F>>);

impl<S, F> Map<S, F> {
    pub fn new(s: S, f: F) -> Map<S, F> {
        Map(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for Map<S, F>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(S::Output) -> R,
{
    type Output = R;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<R> {
        match self.0.step(mc) {
            Some(res) => Some(self.1.take().expect("cannot step a finished sequence").0(
                res,
            )),
            None => None,
        }
    }
}
