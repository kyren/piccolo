use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::error::Error;
use crate::lua::LuaContext;
use crate::sequence::Sequence;

use super::into_sequence::IntoSequence;

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum Then<'gc, S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Result<S::Item, Error>) -> R,
    R: IntoSequence<'gc>,
{
    First(S, Option<StaticCollect<F>>),
    Second(R::Sequence),
}

impl<'gc, S, F, R> Then<'gc, S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Result<S::Item, Error>) -> R,
    R: IntoSequence<'gc>,
{
    pub fn new(s: S, f: F) -> Then<'gc, S, F, R> {
        Then::First(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for Then<'gc, S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, Result<S::Item, Error>) -> R,
    R: IntoSequence<'gc>,
{
    type Item = R::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R::Item, Error>> {
        match self {
            Then::First(s1, f) => match s1.pump(mc, lc) {
                Some(res) => {
                    *self = Then::Second(f.take().unwrap().0(mc, lc, res).into_sequence());
                    None
                }
                None => None,
            },
            Then::Second(s2) => s2.pump(mc, lc),
        }
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum ThenWith<'gc, S, C, F, R>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Result<S::Item, Error>) -> R,
    R: IntoSequence<'gc>,
{
    First(S, Option<(C, StaticCollect<F>)>),
    Second(R::Sequence),
}

impl<'gc, S, C, F, R> ThenWith<'gc, S, C, F, R>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Result<S::Item, Error>) -> R,
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
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, Result<S::Item, Error>) -> R,
    R: IntoSequence<'gc>,
{
    type Item = R::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R::Item, Error>> {
        match self {
            ThenWith::First(s1, f) => match s1.pump(mc, lc) {
                Some(res) => {
                    let (c, StaticCollect(f)) = f.take().unwrap();
                    *self = ThenWith::Second(f(mc, lc, c, res).into_sequence());
                    None
                }
                None => None,
            },
            ThenWith::Second(s2) => s2.pump(mc, lc),
        }
    }
}
