use gc_arena::{Collect, MutationContext};

use crate::error::Error;
use crate::lua::LuaContext;
use crate::sequence::Sequence;

#[must_use = "sequences do nothing unless pumped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum Flatten<'gc, S: Sequence<'gc>> {
    First(S),
    Second(S::Item),
}

impl<'gc, S: Sequence<'gc>> Flatten<'gc, S> {
    pub fn new(s: S) -> Flatten<'gc, S> {
        Flatten::First(s)
    }
}

impl<'gc, S> Sequence<'gc> for Flatten<'gc, S>
where
    S: Sequence<'gc>,
    S::Item: Sequence<'gc>,
{
    type Item = <S::Item as Sequence<'gc>>::Item;

    fn pump(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Error>> {
        match self {
            Flatten::First(s1) => match s1.pump(mc, lc) {
                Some(res) => match res {
                    Ok(s2) => {
                        *self = Flatten::Second(s2);
                        None
                    }
                    Err(e) => Some(Err(e)),
                },
                None => None,
            },
            Flatten::Second(s2) => s2.pump(mc, lc),
        }
    }
}
