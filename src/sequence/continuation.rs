use gc_arena::{Collect, MutationContext};

use crate::{LuaContext, Sequence};

/// A `Sequence` that can optionally result in another `Sequence` to execute.
pub type Continuation<'gc, I, E> =
    Box<Sequence<'gc, Item = ContinuationResult<'gc, I, E>, Error = E> + 'gc>;

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

/// Similar to `IntoSequence`, but specifically produces `Continuation`.  This avoids an extra
/// required `Sequence::step` call, and removes the requirement that `ContinuationResult` implement
/// `Collect` (which is somewhat annoying to do).
pub trait IntoContinuation<'gc> {
    type Item;
    type Error;

    fn into_continuation(self) -> Continuation<'gc, Self::Item, Self::Error>;
}

impl<'gc, I: 'gc + Collect, E: 'gc + Collect> IntoContinuation<'gc>
    for Result<ContinuationResult<'gc, I, E>, E>
{
    type Item = I;
    type Error = E;

    fn into_continuation(self) -> Continuation<'gc, Self::Item, Self::Error> {
        match self {
            Ok(ContinuationResult::Continue(cont)) => cont,
            Ok(ContinuationResult::Finish(res)) => {
                Box::new(ContinuationImmediateResult(Some(Ok(res))))
            }
            Err(err) => Box::new(ContinuationImmediateResult(Some(Err(err)))),
        }
    }
}

impl<'gc, I, E, S> IntoContinuation<'gc> for S
where
    S: Sequence<'gc, Item = ContinuationResult<'gc, I, E>, Error = E> + 'gc,
{
    type Item = I;
    type Error = E;

    fn into_continuation(self) -> Continuation<'gc, I, E> {
        Box::new(self)
    }
}

#[derive(Debug, Collect)]
#[collect(empty_drop)]
pub struct ContinuationImmediateResult<I, E>(Option<Result<I, E>>);

impl<'gc, I: Collect, E: Collect> Sequence<'gc> for ContinuationImmediateResult<I, E> {
    type Item = ContinuationResult<'gc, I, E>;
    type Error = E;

    fn step(
        &mut self,
        _: MutationContext<'gc, '_>,
        _: LuaContext<'gc>,
    ) -> Option<Result<ContinuationResult<'gc, I, E>, E>> {
        Some(
            match self.0.take().expect("cannot step a finished sequence") {
                Ok(res) => Ok(ContinuationResult::Finish(res)),
                Err(err) => Err(err),
            },
        )
    }
}
