use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::error::Error;
use crate::lua::LuaContext;
use crate::sequence::Sequence;

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct Map<S, F>(S, Option<StaticCollect<F>>);

impl<S, F> Map<S, F> {
    pub fn new(s: S, f: F) -> Map<S, F> {
        Map(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for Map<S, F>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, S::Item) -> R,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        match self.0.pump(mc, lc) {
            Some(Ok(res)) => Some(Ok(self.1.take().unwrap().0(mc, lc, res))),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct MapWith<S, C, F>(S, Option<(C, StaticCollect<F>)>);

impl<S, C, F> MapWith<S, C, F> {
    pub fn new(s: S, c: C, f: F) -> MapWith<S, C, F> {
        MapWith(s, Some((c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R> Sequence<'gc> for MapWith<S, C, F>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, S::Item) -> R,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        match self.0.pump(mc, lc) {
            Some(Ok(res)) => {
                let (c, StaticCollect(f)) = self.1.take().unwrap();
                Some(Ok(f(mc, lc, c, res)))
            }
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }
}
