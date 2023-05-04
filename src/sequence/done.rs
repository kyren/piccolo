use gc_arena::{Collect, MutationContext};

use super::Sequence;

#[must_use = "sequences do nothing unless stepped"]
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct Done<O>(Option<O>);

impl<'gc, O: Collect> Sequence<'gc> for Done<O> {
    type Output = O;

    fn step(&mut self, _: MutationContext<'gc, '_>) -> Option<O> {
        Some(self.0.take().expect("cannot step a finished sequence"))
    }
}

pub fn done<T>(t: T) -> Done<T> {
    Done(Some(t))
}

pub fn ok<T, E>(t: T) -> Done<Result<T, E>> {
    Done(Some(Ok(t)))
}

pub fn err<T, E>(e: E) -> Done<Result<T, E>> {
    Done(Some(Err(e)))
}
