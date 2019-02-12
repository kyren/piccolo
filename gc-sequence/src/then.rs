use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum Then<S, F, R> {
    First(S, Option<StaticCollect<F>>),
    Second(R),
}

impl<S, F, R> Then<S, F, R> {
    pub fn new(s: S, f: F) -> Then<S, F, R> {
        Then::First(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for Then<S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, S::Output) -> R,
    R: Sequence<'gc>,
{
    type Output = <R as Sequence<'gc>>::Output;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            Then::First(s1, f) => match s1.step(mc) {
                Some(res) => {
                    *self = Then::Second(f.take().unwrap().0(mc, res));
                    None
                }
                None => None,
            },
            Then::Second(s2) => s2.step(mc),
        }
    }
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum ThenWith<S, C, F, R> {
    First(S, Option<(C, StaticCollect<F>)>),
    Second(R),
}

impl<S, C, F, R> ThenWith<S, C, F, R> {
    pub fn new(s: S, c: C, f: F) -> ThenWith<S, C, F, R> {
        ThenWith::First(s, Some((c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R> Sequence<'gc> for ThenWith<S, C, F, R>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, C, S::Output) -> R,
    R: Sequence<'gc>,
{
    type Output = <R as Sequence<'gc>>::Output;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            ThenWith::First(s1, f) => match s1.step(mc) {
                Some(res) => {
                    let (c, StaticCollect(f)) = f.take().unwrap();
                    *self = ThenWith::Second(f(mc, c, res));
                    None
                }
                None => None,
            },
            ThenWith::Second(s2) => s2.step(mc),
        }
    }
}
