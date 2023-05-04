use gc_arena::{Collect, MutationContext, StaticCollect};

use super::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(
    no_drop,
    bound = "where <S as Sequence<'gc>>::Output: Collect, F: 'static"
)]
pub enum Then<'gc, S, F>
where
    S: Sequence<'gc>,
{
    First(S, Option<StaticCollect<F>>),
    Second(Option<(S::Output, StaticCollect<F>)>),
}

impl<'gc, S, F> Then<'gc, S, F>
where
    S: Sequence<'gc>,
{
    pub fn new(s: S, f: F) -> Then<'gc, S, F> {
        Then::First(s, Some(StaticCollect(f)))
    }
}

impl<'gc, S, F, R> Sequence<'gc> for Then<'gc, S, F>
where
    S: Sequence<'gc>,
    S::Output: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, S::Output) -> R,
{
    type Output = R;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<R> {
        match self {
            Then::First(seq, f) => match seq.step(mc) {
                Some(res) => {
                    *self = Then::Second(Some((res, f.take().unwrap())));
                    None
                }
                None => None,
            },
            Then::Second(sec) => {
                let (res, f) = sec.take().expect("cannot step a finished sequence");
                Some(f.0(mc, res))
            }
        }
    }
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(
    no_drop,
    bound = "where C: Collect, <S as Sequence<'gc>>::Output: Collect, F: 'static"
)]
pub enum ThenWith<'gc, S, C, F>
where
    S: Sequence<'gc>,
{
    First(S, Option<(C, StaticCollect<F>)>),
    Second(Option<(C, S::Output, StaticCollect<F>)>),
}

impl<'gc, S, C, F> ThenWith<'gc, S, C, F>
where
    S: Sequence<'gc>,
{
    pub fn new(s: S, c: C, f: F) -> ThenWith<'gc, S, C, F> {
        ThenWith::First(s, Some((c, StaticCollect(f))))
    }
}

impl<'gc, S, C, F, R> Sequence<'gc> for ThenWith<'gc, S, C, F>
where
    S: Sequence<'gc>,
    S::Output: Collect,
    C: Collect,
    F: 'static + FnOnce(C, MutationContext<'gc, '_>, S::Output) -> R,
{
    type Output = R;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<R> {
        match self {
            ThenWith::First(seq, cf) => match seq.step(mc) {
                Some(res) => {
                    let (c, f) = cf.take().unwrap();
                    *self = ThenWith::Second(Some((c, res, f)));
                    None
                }
                None => None,
            },
            ThenWith::Second(sec) => {
                let (c, res, f) = sec.take().expect("cannot step a finished sequence");
                Some(f.0(c, mc, res))
            }
        }
    }
}
