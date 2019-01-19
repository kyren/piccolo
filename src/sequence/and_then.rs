use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::lua::LuaContext;
use crate::sequence::Sequence;

use super::into_sequence::IntoSequence;

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum AndThen<'gc, S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, S::Item) -> R,
    R: IntoSequence<'gc>,
{
    First(S, Option<StaticCollect<F>>),
    Second(R::Sequence),
}

impl<'gc, S, F, R> AndThen<'gc, S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, S::Item) -> R,
    R: IntoSequence<'gc>,
{
    pub fn new(s: S, f: F) -> AndThen<'gc, S, F, R> {
        AndThen::First(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for AndThen<'gc, S, F, R>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, S::Item) -> R,
    R: IntoSequence<'gc, Error = S::Error>,
{
    type Item = R::Item;
    type Error = R::Error;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R::Item, R::Error>> {
        match self {
            AndThen::First(s1, f) => match s1.pump(mc, lc) {
                Some(Ok(res)) => {
                    *self = AndThen::Second(f.take().unwrap().0(mc, lc, res).into_sequence());
                    None
                }
                Some(Err(err)) => Some(Err(err)),
                None => None,
            },
            AndThen::Second(s2) => s2.pump(mc, lc),
        }
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum AndThenWith<'gc, S, C, F, R>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, S::Item) -> R,
    R: IntoSequence<'gc>,
{
    First(S, Option<(C, StaticCollect<F>)>),
    Second(R::Sequence),
}

impl<'gc, S, C, F, R> AndThenWith<'gc, S, C, F, R>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, S::Item) -> R,
    R: IntoSequence<'gc>,
{
    pub fn new(s: S, c: C, f: F) -> AndThenWith<'gc, S, C, F, R> {
        AndThenWith::First(s, Some((c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R> Sequence<'gc> for AndThenWith<'gc, S, C, F, R>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, S::Item) -> R,
    R: IntoSequence<'gc, Error = S::Error>,
{
    type Item = R::Item;
    type Error = R::Error;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R::Item, R::Error>> {
        match self {
            AndThenWith::First(s1, f) => match s1.pump(mc, lc) {
                Some(Ok(res)) => {
                    let (c, StaticCollect(f)) = f.take().unwrap();
                    *self = AndThenWith::Second(f(mc, lc, c, res).into_sequence());
                    None
                }
                Some(Err(err)) => Some(Err(err)),
                None => None,
            },
            AndThenWith::Second(s2) => s2.pump(mc, lc),
        }
    }
}
