use failure::Error;

use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::lua::LuaContext;
use crate::sequence::Sequence;

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct AndThen<S, F>(Option<(S, StaticCollect<F>)>);

impl<S, F> AndThen<S, F> {
    pub fn new(s: S, f: F) -> AndThen<S, F> {
        AndThen(Some((s, StaticCollect(f))))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for AndThen<S, F>
where
    S: Sequence<'gc>,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, S::Item) -> Result<R, Error>,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        match self.0.take() {
            Some((mut a, StaticCollect(f))) => match a.pump(mc, lc) {
                Some(Ok(r)) => Some(f(mc, lc, r)),
                Some(Err(e)) => Some(Err(e)),
                None => {
                    self.0 = Some((a, StaticCollect(f)));
                    None
                }
            },
            None => panic!("cannot pump a finished sequence"),
        }
    }
}

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct AndThenWith<S, C, F>(Option<(S, C, StaticCollect<F>)>);

impl<S, C, F> AndThenWith<S, C, F> {
    pub fn new(s: S, c: C, f: F) -> AndThenWith<S, C, F> {
        AndThenWith(Some((s, c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R> Sequence<'gc> for AndThenWith<S, C, F>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, LuaContext<'gc>, C, S::Item) -> Result<R, Error>,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        match self.0.take() {
            Some((mut a, c, StaticCollect(f))) => match a.pump(mc, lc) {
                Some(Ok(r)) => Some(f(mc, lc, c, r)),
                Some(Err(e)) => Some(Err(e)),
                None => {
                    self.0 = Some((a, c, StaticCollect(f)));
                    None
                }
            },
            None => panic!("cannot pump a finished sequence"),
        }
    }
}
