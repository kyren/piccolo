use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(no_drop, bound = "where S: Collect, F: 'static")]
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

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(no_drop, bound = "where S: Collect, C: Collect, F: 'static")]
pub struct MapWith<S, C, F>(S, Option<(C, StaticCollect<F>)>);

impl<S, C, F> MapWith<S, C, F> {
    pub fn new(s: S, c: C, f: F) -> MapWith<S, C, F> {
        MapWith(s, Some((c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R> Sequence<'gc> for MapWith<S, C, F>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(C, S::Output) -> R,
{
    type Output = R;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self.0.step(mc) {
            Some(res) => {
                let (c, StaticCollect(f)) = self.1.take().expect("cannot step a finished sequence");
                Some(f(c, res))
            }
            None => None,
        }
    }
}
