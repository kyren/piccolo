use gc_arena::{Collect, MutationContext, StaticCollect};

use crate::{IntoSequence, Sequence};

pub fn sequence_fn<'gc, F, R>(f: F) -> SequenceFn<'gc, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>) -> R,
    R: IntoSequence<'gc>,
{
    SequenceFn::new(f)
}

pub fn sequence_fn_with<'gc, C, F, R>(c: C, f: F) -> SequenceFnWith<'gc, C, F, R>
where
    C: Collect,
    F: 'static + FnOnce(MutationContext<'gc, '_>, C) -> R,
    R: IntoSequence<'gc>,
{
    SequenceFnWith::new(c, f)
}

#[must_use = "sequences do nothing unless steped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum SequenceFn<'gc, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>) -> R,
    R: IntoSequence<'gc>,
{
    First(Option<StaticCollect<F>>),
    Second(R::Sequence),
}

impl<'gc, F, R> SequenceFn<'gc, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>) -> R,
    R: IntoSequence<'gc>,
{
    fn new(f: F) -> SequenceFn<'gc, F, R> {
        SequenceFn::First(Some(StaticCollect(f)))
    }
}

impl<'gc, F, R> Sequence<'gc> for SequenceFn<'gc, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>) -> R,
    R: IntoSequence<'gc>,
{
    type Item = R::Item;
    type Error = R::Error;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Result<R::Item, R::Error>> {
        match self {
            SequenceFn::First(f) => {
                *self = SequenceFn::Second(f.take().unwrap().0(mc).into_sequence());
                None
            }
            SequenceFn::Second(s) => s.step(mc),
        }
    }
}

#[must_use = "sequences do nothing unless steped"]
#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub enum SequenceFnWith<'gc, C, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, C) -> R,
    C: Collect,
    R: IntoSequence<'gc>,
{
    First(Option<(C, StaticCollect<F>)>),
    Second(R::Sequence),
}

impl<'gc, C, F, R> SequenceFnWith<'gc, C, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, C) -> R,
    C: Collect,
    R: IntoSequence<'gc>,
{
    fn new(c: C, f: F) -> SequenceFnWith<'gc, C, F, R> {
        SequenceFnWith::First(Some((c, StaticCollect(f))))
    }
}

impl<'gc, C, F, R> Sequence<'gc> for SequenceFnWith<'gc, C, F, R>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>, C) -> R,
    C: Collect,
    R: IntoSequence<'gc>,
{
    type Item = R::Item;
    type Error = R::Error;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Result<R::Item, R::Error>> {
        match self {
            SequenceFnWith::First(f) => {
                let (c, StaticCollect(f)) = f.take().unwrap();
                *self = SequenceFnWith::Second(f(mc, c).into_sequence());
                None
            }
            SequenceFnWith::Second(s) => s.step(mc),
        }
    }
}
