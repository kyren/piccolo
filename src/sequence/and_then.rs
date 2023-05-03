use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(no_drop, bound = "where S: Collect, I: Collect, F: 'static")]
pub enum AndThen<S, F, I> {
    First(S, Option<StaticCollect<F>>),
    Second(Option<(I, StaticCollect<F>)>),
}

impl<S, F, I> AndThen<S, F, I> {
    pub fn new(s: S, f: F) -> AndThen<S, F, I> {
        AndThen::First(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, I, E, R> Sequence<'gc> for AndThen<S, F, I>
where
    S: Sequence<'gc, Output = Result<I, E>>,
    I: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, I) -> Result<R, E>,
{
    type Output = Result<R, E>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            AndThen::First(seq, f) => match seq.step(mc) {
                Some(Ok(res)) => {
                    *self = AndThen::Second(Some((res, f.take().unwrap())));
                    None
                }
                Some(Err(err)) => {
                    *self = AndThen::Second(None);
                    Some(Err(err))
                }
                None => None,
            },
            AndThen::Second(sec) => {
                let (res, f) = sec.take().expect("cannot step a finished sequence");
                Some(f.0(mc, res))
            }
        }
    }
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(
    no_drop,
    bound = "where S: Collect, C: Collect, I: Collect, F: 'static"
)]
pub enum AndThenWith<S, C, F, I> {
    First(S, Option<(C, StaticCollect<F>)>),
    Second(Option<(C, I, StaticCollect<F>)>),
}

impl<S, C, F, I> AndThenWith<S, C, F, I> {
    pub fn new(s: S, c: C, f: F) -> AndThenWith<S, C, F, I> {
        AndThenWith::First(s, Some((c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, I, E, R> Sequence<'gc> for AndThenWith<S, C, F, I>
where
    S: Sequence<'gc, Output = Result<I, E>>,
    C: Collect,
    I: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, C, I) -> Result<R, E>,
{
    type Output = Result<R, E>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            AndThenWith::First(seq, cf) => match seq.step(mc) {
                Some(Ok(res)) => {
                    let (c, f) = cf.take().unwrap();
                    *self = AndThenWith::Second(Some((c, res, f)));
                    None
                }
                Some(Err(err)) => {
                    *self = AndThenWith::Second(None);
                    Some(Err(err))
                }
                None => None,
            },
            AndThenWith::Second(sec) => {
                let (c, res, f) = sec.take().expect("cannot step a finished sequence");
                Some(f.0(mc, c, res))
            }
        }
    }
}
