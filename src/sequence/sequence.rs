use std::{fmt, ops};

use allocator_api2::boxed;
use gc_arena::{allocator_api::MetricsAlloc, Collect, Mutation};

use crate::{Context, Error, Execution, Function, Stack, Thread};

pub enum SequencePoll<'gc> {
    /// Sequence pending, `Sequence::poll` will be called on the next step with the stack unchanged.
    Pending,
    /// Sequence finished, all of the values in the stack will be returned to the caller.
    Return,
    /// Call the given functions with the arguments in the stack starting at `bottom`. When the
    /// function returns, `Sequence::poll` will be called with the return values will be placed into
    /// the stack starting at `bottom`. If the given function errors, then `Sequence::error` will be
    /// called and the stack will instead be truncated to `bottom`.
    Call {
        function: Function<'gc>,
        bottom: usize,
    },
    /// Call the given function as a tail call with the entire stack as arguments and finish the
    /// sequence.
    TailCall { function: Function<'gc> },
    /// Yield the values in the stack starting at `bottom`. When the `Sequence` is resumed, the
    /// resume arguments will be on the stack starting at `bottom`.
    Yield {
        to_thread: Option<Thread<'gc>>,
        bottom: usize,
    },
    /// Yield all of the values in the stack and finish the sequence.
    TailYield { to_thread: Option<Thread<'gc>> },
    /// Resume the given thread with arguments starting at `bottom`. When the thread returns values,
    /// those values will be placed on the stack starting at `bottom`.
    Resume { thread: Thread<'gc>, bottom: usize },
    /// Resume the given thread with the entire stack as arguments and finish the sequence.
    TailResume { thread: Thread<'gc> },
}

pub trait Sequence<'gc>: Collect {
    /// Called when a `Sequence` is first run with the stack unchanged from the returned `Callback`
    /// that spawned it.
    ///
    /// If a sub-function is called and succeeds, this will be called when that function finishes
    /// successfully with its return values.
    ///
    /// If the `Sequence` yields values, this will suspend the containing coroutine and
    /// `Sequence::poll` will be called again with the resume parameters.
    fn poll(
        &mut self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>>;

    /// Called if a sub-function errors to handle the error, or if a `Sequence` has yielded and the
    /// containing coroutine is resumed with an error.
    fn error(
        &mut self,
        _ctx: Context<'gc>,
        _exec: Execution<'gc, '_>,
        error: Error<'gc>,
        _stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        Err(error)
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct BoxSequence<'gc>(boxed::Box<dyn Sequence<'gc> + 'gc, MetricsAlloc<'gc>>);

impl<'gc> fmt::Debug for BoxSequence<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Sequence")
            .field(&(self.0.as_ref() as *const _))
            .finish()
    }
}

impl<'gc> ops::Deref for BoxSequence<'gc> {
    type Target = dyn Sequence<'gc> + 'gc;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<'gc> ops::DerefMut for BoxSequence<'gc> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}

impl<'gc> BoxSequence<'gc> {
    pub fn new(mc: &Mutation<'gc>, sequence: impl Sequence<'gc> + 'gc) -> Self {
        let b = boxed::Box::new_in(sequence, MetricsAlloc::new(mc));
        let (ptr, alloc) = boxed::Box::into_raw_with_allocator(b);
        // TODO: Required unsafety due to do lack of `CoerceUnsized` on allocator_api2 `Box` type,
        // replace with safe cast when one of allocator_api or CoerceUnsized is stabilized.
        let b = unsafe { boxed::Box::from_raw_in(ptr as *mut dyn Sequence, alloc) };
        Self(b)
    }
}
