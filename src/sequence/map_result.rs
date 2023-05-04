use gc_arena::{Collect, MutationContext, StaticCollect};

use super::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(no_drop, bound = "where S: Collect, F: 'static")]
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
#[collect(no_drop, bound = "where S: Collect, C: Collect, F: 'static")]
pub struct MapOkWith<S, C, F>(S, Option<(C, StaticCollect<F>)>);

impl<S, C, F> MapOkWith<S, C, F> {
    pub fn new(s: S, c: C, f: F) -> MapOkWith<S, C, F> {
        MapOkWith(s, Some((c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, I, E, R> Sequence<'gc> for MapOkWith<S, C, F>
where
    S: Sequence<'gc, Output = Result<I, E>>,
    C: Collect,
    F: 'static + FnOnce(C, I) -> R,
{
    type Output = Result<R, E>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self.0.step(mc) {
            Some(Ok(res)) => {
                let (c, StaticCollect(f)) = self.1.take().expect("cannot step a finished sequence");
                Some(Ok(f(c, res)))
            }
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(no_drop, bound = "where S: Collect, F: 'static")]
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
