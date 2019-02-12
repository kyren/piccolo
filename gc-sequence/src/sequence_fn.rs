use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::Sequence;

pub fn from_fn<'gc, F, R>(f: F) -> SequenceFn<F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>) -> R,
    R: Sequence<'gc>,
{
    SequenceFn::new(f)
}

pub fn from_fn_with<'gc, C, F, R>(c: C, f: F) -> SequenceFnWith<C, F, R>
where
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, C) -> R,
    R: Sequence<'gc>,
{
    SequenceFnWith::new(c, f)
}

#[must_use = "sequences do nothing unless steped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum SequenceFn<F, R> {
    First(Option<StaticCollect<F>>),
    Second(R),
}

impl<F, R> SequenceFn<F, R> {
    fn new(f: F) -> SequenceFn<F, R> {
        SequenceFn::First(Some(StaticCollect(f)))
    }
}

impl<'gc, F, R> Sequence<'gc> for SequenceFn<F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>) -> R,
    R: Sequence<'gc>,
{
    type Output = <R as Sequence<'gc>>::Output;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            SequenceFn::First(f) => {
                *self =
                    SequenceFn::Second(f.take().expect("cannot step a finished sequence").0(mc));
                None
            }
            SequenceFn::Second(s) => s.step(mc),
        }
    }
}

#[must_use = "sequences do nothing unless steped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum SequenceFnWith<C, F, R> {
    First(Option<(C, StaticCollect<F>)>),
    Second(R),
}

impl<C, F, R> SequenceFnWith<C, F, R> {
    fn new(c: C, f: F) -> SequenceFnWith<C, F, R> {
        SequenceFnWith::First(Some((c, StaticCollect(f))))
    }
}

impl<'gc, C, F, R> Sequence<'gc> for SequenceFnWith<C, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, C) -> R,
    C: Collect,
    R: Sequence<'gc>,
{
    type Output = <R as Sequence<'gc>>::Output;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self {
            SequenceFnWith::First(f) => {
                let (c, StaticCollect(f)) = f.take().expect("cannot step a finished sequence");
                *self = SequenceFnWith::Second(f(mc, c));
                None
            }
            SequenceFnWith::Second(s) => s.step(mc),
        }
    }
}
