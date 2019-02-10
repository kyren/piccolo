use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::{IntoSequence, Sequence};

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum Then<'gc, S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, Result<S::Item, S::Error>) -> R,
    R: IntoSequence<'gc>,
{
    First(S, Option<StaticCollect<F>>),
    Second(R::Sequence),
}

impl<'gc, S, F, R> Then<'gc, S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, Result<S::Item, S::Error>) -> R,
    R: IntoSequence<'gc>,
{
    pub fn new(s: S, f: F) -> Then<'gc, S, F, R> {
        Then::First(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for Then<'gc, S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, Result<S::Item, S::Error>) -> R,
    R: IntoSequence<'gc>,
{
    type Item = R::Item;
    type Error = R::Error;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Result<R::Item, R::Error>> {
        match self {
            Then::First(s1, f) => match s1.step(mc) {
                Some(res) => {
                    *self = Then::Second(f.take().unwrap().0(mc, res).into_sequence());
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
pub enum ThenWith<'gc, S, C, F, R>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, C, Result<S::Item, S::Error>) -> R,
    R: IntoSequence<'gc>,
{
    First(S, Option<(C, StaticCollect<F>)>),
    Second(R::Sequence),
}

impl<'gc, S, C, F, R> ThenWith<'gc, S, C, F, R>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, C, Result<S::Item, S::Error>) -> R,
    R: IntoSequence<'gc>,
{
    pub fn new(s: S, c: C, f: F) -> ThenWith<'gc, S, C, F, R> {
        ThenWith::First(s, Some((c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R> Sequence<'gc> for ThenWith<'gc, S, C, F, R>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, C, Result<S::Item, S::Error>) -> R,
    R: IntoSequence<'gc>,
{
    type Item = R::Item;
    type Error = R::Error;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Result<R::Item, R::Error>> {
        match self {
            ThenWith::First(s1, f) => match s1.step(mc) {
                Some(res) => {
                    let (c, StaticCollect(f)) = f.take().unwrap();
                    *self = ThenWith::Second(f(mc, c, res).into_sequence());
                    None
                }
                None => None,
            },
            ThenWith::Second(s2) => s2.step(mc),
        }
    }
}
