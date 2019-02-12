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

impl<'gc, S, F, I, E, R> Sequence<'gc> for MapOk<S, F>
where
    S: Sequence<'gc, Output = Result<I, E>>,
    F: 'static + FnOnce(I) -> R,
{
    type Output = Result<R, E>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self.0.step(mc) {
            Some(Ok(res)) => Some(Ok(self
                .1
                .take()
                .expect("cannot step a finished sequence")
                .0(res))),
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

impl<'gc, S, F, I, E, R> Sequence<'gc> for MapError<S, F>
where
    S: Sequence<'gc, Output = Result<I, E>>,
    F: 'static + FnOnce(E) -> R,
{
    type Output = Result<I, R>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self.0.step(mc) {
            Some(Ok(res)) => Some(Ok(res)),
            Some(Err(err)) => Some(Err(self
                .1
                .take()
                .expect("cannot step a finished sequence")
                .0(err))),
            None => None,
        }
    }
}
