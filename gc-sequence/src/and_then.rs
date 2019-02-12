use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum AndThen<S, F, R> {
    First(S, Option<StaticCollect<F>>),
    Second(R),
}

impl<S, F, R> AndThen<S, F, R> {
    pub fn new(s: S, f: F) -> AndThen<S, F, R> {
        AndThen::First(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R, I, I2, E> Sequence<'gc> for AndThen<S, F, R>
where
    S: Sequence<'gc, Output = Result<I, E>>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, I) -> R,
    R: Sequence<'gc, Output = Result<I2, E>>,
{
    type Output = Result<I2, E>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            AndThen::First(s1, f) => match s1.step(mc) {
                Some(Ok(res)) => {
                    *self = AndThen::Second(f.take().unwrap().0(mc, res));
                    None
                }
                Some(Err(err)) => Some(Err(err)),
                None => None,
            },
            AndThen::Second(s2) => s2.step(mc),
        }
    }
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum AndThenWith<S, C, F, R> {
    First(S, Option<(C, StaticCollect<F>)>),
    Second(R),
}

impl<S, C, F, R> AndThenWith<S, C, F, R> {
    pub fn new(s: S, c: C, f: F) -> AndThenWith<S, C, F, R> {
        AndThenWith::First(s, Some((c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R, I, I2, E> Sequence<'gc> for AndThenWith<S, C, F, R>
where
    S: Sequence<'gc, Output = Result<I, E>>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, C, I) -> R,
    R: Sequence<'gc, Output = Result<I2, E>>,
{
    type Output = Result<I2, E>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            AndThenWith::First(s1, f) => match s1.step(mc) {
                Some(Ok(res)) => {
                    let (c, StaticCollect(f)) = f.take().unwrap();
                    *self = AndThenWith::Second(f(mc, c, res));
                    None
                }
                Some(Err(err)) => Some(Err(err)),
                None => None,
            },
            AndThenWith::Second(s2) => s2.step(mc),
        }
    }
}
