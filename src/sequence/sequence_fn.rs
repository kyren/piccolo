use gc_arena::{Collect, MutationContext, StaticCollect};

use failure::Error;

use crate::lua::LuaContext;
use crate::sequence::Sequence;

pub fn sequence_fn<'gc, F, R>(f: F) -> SequenceFn<F>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>) -> Result<R, Error>,
{
    SequenceFn::new(f)
}

pub fn sequence_fn_with<'gc, C, F, R>(c: C, f: F) -> SequenceFnWith<C, F>
where
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C) -> Result<R, Error>,
{
    SequenceFnWith::new(c, f)
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct SequenceFn<F>(Option<StaticCollect<F>>);

impl<F> SequenceFn<F> {
    fn new(f: F) -> SequenceFn<F> {
        SequenceFn(Some(StaticCollect(f)))
    }
}

impl<'gc, F, R> Sequence<'gc> for SequenceFn<F>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>) -> Result<R, Error>,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        Some(self.0.take().expect("cannot pump a finished sequence").0(
            mc, lc,
        ))
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct SequenceFnWith<C, F>(Option<(C, StaticCollect<F>)>);

impl<C, F> SequenceFnWith<C, F> {
    fn new(c: C, f: F) -> SequenceFnWith<C, F> {
        SequenceFnWith(Some((c, StaticCollect(f))))
    }
}

impl<'gc, C, F, R> Sequence<'gc> for SequenceFnWith<C, F>
where
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C) -> Result<R, Error>,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        let (c, f) = self.0.take().expect("cannot pump a finished sequence");
        Some(f.0(mc, lc, c))
    }
}
