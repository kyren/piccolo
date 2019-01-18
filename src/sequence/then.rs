use failure::Error;

use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::lua::LuaContext;
use crate::sequence::Sequence;

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct Then<S, F>(Option<(S, StaticCollect<F>)>);

impl<S, F> Then<S, F> {
    pub fn new(s: S, f: F) -> Then<S, F> {
        Then(Some((s, StaticCollect(f))))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for Then<S, F>
where
    S: Sequence<'gc>,
    F: 'static
        + FnOnce(
            MutationContext<'gc, '_>,
            LuaContext<'gc>,
            Result<S::Item, Error>,
        ) -> Result<R, Error>,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        match self.0.take() {
            Some((mut a, StaticCollect(f))) => match a.pump(mc, lc) {
                Some(r) => Some(f(mc, lc, r)),
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
pub struct ThenWith<S, C, F>(Option<(S, C, StaticCollect<F>)>);

impl<S, C, F> ThenWith<S, C, F> {
    pub fn new(s: S, c: C, f: F) -> ThenWith<S, C, F> {
        ThenWith(Some((s, c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R> Sequence<'gc> for ThenWith<S, C, F>
where
    S: Sequence<'gc>,
    C: Collect,
    F: 'static
        + FnOnce(
            MutationContext<'gc, '_>,
            LuaContext<'gc>,
            C,
            Result<S::Item, Error>,
        ) -> Result<R, Error>,
{
    type Item = R;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<R, Error>> {
        match self.0.take() {
            Some((mut a, c, StaticCollect(f))) => match a.pump(mc, lc) {
                Some(r) => Some(f(mc, lc, c, r)),
                None => {
                    self.0 = Some((a, c, StaticCollect(f)));
                    None
                }
            },
            None => panic!("cannot pump a finished sequence"),
        }
    }
}
