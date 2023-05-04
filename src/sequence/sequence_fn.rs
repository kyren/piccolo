use gc_arena::{Collect, MutationContext, StaticCollect};

use super::Sequence;

pub fn from_fn<'gc, F, R>(f: F) -> SequenceFn<F>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>) -> R,
{
    SequenceFn::new(f)
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(no_drop, bound = "where F: 'static")]
pub struct SequenceFn<F>(Option<StaticCollect<F>>);

impl<F> SequenceFn<F> {
    pub fn new(f: F) -> SequenceFn<F> {
        SequenceFn(Some(StaticCollect(f)))
    }
}

impl<'gc, F, R> Sequence<'gc> for SequenceFn<F>
where
    F: 'static + FnOnce(MutationContext<'gc, '_>) -> R,
{
    type Output = R;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        Some(self.0.take().expect("cannot step a finished sequence").0(
            mc,
        ))
    }
}

pub fn from_fn_with<'gc, C, F, R>(c: C, f: F) -> SequenceFnWith<C, F>
where
    C: Collect,
    F: 'static + FnOnce(C, MutationContext<'gc, '_>) -> R,
{
    SequenceFnWith::new(c, f)
}

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(no_drop, bound = "where C: Collect, F: 'static")]
pub struct SequenceFnWith<C, F>(Option<(C, StaticCollect<F>)>);

impl<C, F> SequenceFnWith<C, F> {
    pub fn new(c: C, f: F) -> SequenceFnWith<C, F> {
        SequenceFnWith(Some((c, StaticCollect(f))))
    }
}

impl<'gc, C, F, R> Sequence<'gc> for SequenceFnWith<C, F>
where
    F: 'static + FnOnce(C, MutationContext<'gc, '_>) -> R,
    C: Collect,
{
    type Output = R;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        let (c, StaticCollect(f)) = self.0.take().expect("cannot step a finished sequence");
        Some(f(c, mc))
    }
}
