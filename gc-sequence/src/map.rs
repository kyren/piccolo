use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::Sequence;

#[must_use = "sequences do nothing unless steped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct MapOk<S, F>(S, Option<StaticCollect<F>>);

impl<S, F> MapOk<S, F> {
    pub fn new(s: S, f: F) -> MapOk<S, F> {
        MapOk(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for MapOk<S, F>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(S::Item) -> R,
{
    type Item = R;
    type Error = S::Error;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Result<R, S::Error>> {
        match self.0.step(mc) {
            Some(Ok(res)) => Some(Ok(self.1.take().unwrap().0(res))),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct MapError<S, F>(S, Option<StaticCollect<F>>);

impl<S, F> MapError<S, F> {
    pub fn new(s: S, f: F) -> MapError<S, F> {
        MapError(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for MapError<S, F>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(S::Error) -> R,
{
    type Item = S::Item;
    type Error = R;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Result<S::Item, R>> {
        match self.0.step(mc) {
            Some(Ok(res)) => Some(Ok(res)),
            Some(Err(err)) => Some(Err(self.1.take().unwrap().0(err))),
            None => None,
        }
    }
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct Map<S, F>(S, Option<StaticCollect<F>>);

impl<S, F> Map<S, F> {
    pub fn new(s: S, f: F) -> Map<S, F> {
        Map(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, RI, RE> Sequence<'gc> for Map<S, F>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(Result<S::Item, S::Error>) -> Result<RI, RE>,
{
    type Item = RI;
    type Error = RE;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Result<RI, RE>> {
        match self.0.step(mc) {
            Some(res) => Some(self.1.take().unwrap().0(res)),
            None => None,
        }
    }
}
