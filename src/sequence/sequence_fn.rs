use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::error::Error;
use crate::lua::LuaContext;
use crate::sequence::IntoSequence;
use crate::sequence::Sequence;

pub fn sequence_fn<'gc, F, R>(f: F) -> SequenceFn<'gc, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>) -> R,
    R: IntoSequence<'gc>,
{
    SequenceFn::new(f)
}

pub fn sequence_fn_with<'gc, C, F, R>(c: C, f: F) -> SequenceFnWith<'gc, C, F, R>
where
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C) -> R,
    R: IntoSequence<'gc>,
{
    SequenceFnWith::new(c, f)
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum SequenceFn<'gc, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>) -> R,
    R: IntoSequence<'gc>,
{
    First(Option<StaticCollect<F>>),
    Second(R::Sequence),
}

impl<'gc, F, R> SequenceFn<'gc, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>) -> R,
    R: IntoSequence<'gc>,
{
    fn new(f: F) -> SequenceFn<'gc, F, R> {
        SequenceFn::First(Some(StaticCollect(f)))
    }
}

impl<'gc, F, R> Sequence<'gc> for SequenceFn<'gc, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>) -> R,
    R: IntoSequence<'gc>,
{
    type Item = R::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R::Item, Error>> {
        match self {
            SequenceFn::First(f) => {
                *self = SequenceFn::Second(f.take().unwrap().0(mc, lc).into_sequence());
                None
            }
            SequenceFn::Second(s) => s.pump(mc, lc),
        }
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum SequenceFnWith<'gc, C, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C) -> R,
    C: Collect,
    R: IntoSequence<'gc>,
{
    First(Option<(C, StaticCollect<F>)>),
    Second(R::Sequence),
}

impl<'gc, C, F, R> SequenceFnWith<'gc, C, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C) -> R,
    C: Collect,
    R: IntoSequence<'gc>,
{
    fn new(c: C, f: F) -> SequenceFnWith<'gc, C, F, R> {
        SequenceFnWith::First(Some((c, StaticCollect(f))))
    }
}

impl<'gc, C, F, R> Sequence<'gc> for SequenceFnWith<'gc, C, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C) -> R,
    C: Collect,
    R: IntoSequence<'gc>,
{
    type Item = R::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R::Item, Error>> {
        match self {
            SequenceFnWith::First(f) => {
                let (c, StaticCollect(f)) = f.take().unwrap();
                *self = SequenceFnWith::Second(f(mc, lc, c).into_sequence());
                None
            }
            SequenceFnWith::Second(s) => s.pump(mc, lc),
        }
    }
}
