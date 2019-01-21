use gc_arena::{Collect, MutationContext};

use crate::{LuaContext, Sequence};

/// A `Sequence` that can optionally result in another `Sequence` to execute.
pub type Continuation<'gc, I, E> =
    Box<Sequence<'gc, Item = ContinuationResult<'gc, I, E>, Error = E> + 'gc>;

// Empty drop prevents pattern matching, safe as it does not implement Drop at all.
#[derive(Collect)]
#[collect(unsafe_drop)]
pub enum ContinuationResult<'gc, I, E> {
    Finish(I),
    Continue(Continuation<'gc, I, E>),
}

/// Wraps a `Continuation` so that it continues executing when resulting in a
/// `ContinuationResult::Continue`.  Further continuations are executed until one produces a
/// `ContinuationResult::Finish` value instead.
#[derive(Collect)]
#[collect(empty_drop)]
pub struct RunContinuation<'gc, I, E>(Option<Continuation<'gc, I, E>>);

impl<'gc, I, E> RunContinuation<'gc, I, E> {
    pub fn new(cont: Continuation<'gc, I, E>) -> RunContinuation<'gc, I, E> {
        RunContinuation(Some(cont))
    }

    pub fn from_sequence<S>(seq: S) -> RunContinuation<'gc, I, E>
    where
        S: Sequence<'gc, Item = ContinuationResult<'gc, I, E>, Error = E> + 'gc,
    {
        RunContinuation(Some(Box::new(seq)))
    }
}

impl<'gc, I, E> Sequence<'gc> for RunContinuation<'gc, I, E> {
    type Item = I;
    type Error = E;

    fn step(&mut self, mc: MutationContext<'gc, '_>, lc: LuaContext<'gc>) -> Option<Result<I, E>> {
        let mut cont = self.0.take().expect("cannot step a finished sequence");
        match cont.step(mc, lc) {
            Some(Ok(res)) => match res {
                ContinuationResult::Finish(res) => Some(Ok(res)),
                ContinuationResult::Continue(cont) => {
                    self.0 = Some(cont);
                    None
                }
            },
            Some(Err(err)) => Some(Err(err)),
            None => {
                self.0 = Some(cont);
                None
            }
        }
    }
}
