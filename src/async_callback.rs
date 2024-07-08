use std::{
    cell::Cell,
    future::{poll_fn, Future},
    marker::PhantomData,
    mem,
    pin::Pin,
    ptr,
    task::{self, Poll, RawWaker, RawWakerVTable, Waker},
};

use allocator_api2::boxed;
use gc_arena::{allocator_api::MetricsAlloc, Collect, DynamicRootSet, Mutation};

use crate::{
    stash::{Fetchable, Stashable, StashedRootSet},
    BoxSequence, Context, Error, Execution, Function, Sequence, SequencePoll, Stack, StashedError,
    StashedFunction, StashedThread, Thread,
};

/// Create a [`Sequence`] impl from a [`Future`] that can suspend, call Lua functions, yield to Lua,
/// and resume threads as async method calls via a held [`AsyncSequence`] proxy.
///
/// Can be used to implement `Sequence` in a way MUCH easier than manual state machines.
///
/// Currently uses `async` to do what in the future could be more directly accomplished with
/// coroutines (see the unstable [`std::ops::Coroutine`] trait). The [`std::task::Context`]
/// available within the created future is **meaningless** and has a NOOP waker; we are only using
/// `async` as a stable way to express what would be better expressed as a simple coroutine.
///
/// It is possible to integrate proper async APIs with `piccolo`, and to even have a method to
/// "wake" Lua coroutines with a *real* [`std::task::Waker`], but simply calling an external async
/// function from the created future here is *not* the way to do it. It will not do what you want,
/// and probably will result in panics.
///
/// The provided `create` function is given two parameters: a [`StashedRootSet`] to stash values
/// that will be owned by the future, and a [`Builder`] object to actually create the resulting
/// [`BoxSequence`]. The callback is called immediately and is not required to be `'static`, it
/// exists only to create the generative `'seq` lifetime and is required for correctness. The
/// function should stash any local variables needed by the future from the outer context, then move
/// those locals into the future provided to [`Builder::build`].
pub fn async_sequence<'gc, R>(
    mc: &Mutation<'gc>,
    f: impl for<'seq> FnOnce(StashedRootSet<'seq, 'gc>, Builder<'seq, 'gc>) -> R,
) -> R {
    let locals = DynamicRootSet::new(mc);
    f(
        StashedRootSet::new(locals),
        Builder {
            alloc: MetricsAlloc::new_static(mc),
            locals,
            _invariant: PhantomData,
        },
    )
}

/// Return type for futures that are driving an async sequence.
///
/// This performs equivalent actions to [`CallbackReturn`](crate::CallbackReturn) and the tail
/// variants of [`SequencePoll`], so check those for more information on precisely what these
/// actions mean.
pub enum SequenceReturn<'seq> {
    /// Sequence finished, all of the values in the stack will be returned to the caller.
    Return,
    /// Call the given function with the values in the stack as arguments.
    Call(StashedFunction<'seq>),
    /// Yield the values in the stack.
    Yield(Option<StashedThread<'seq>>),
    /// Resume the given thread with the values in the stack as arguments.
    Resume(StashedThread<'seq>),
}

/// Provided by [`async_sequence`] as a means to build the final [`BoxSequence`].
pub struct Builder<'seq, 'gc> {
    alloc: MetricsAlloc<'static>,
    locals: DynamicRootSet<'gc>,
    _invariant: Invariant<'seq>,
}

impl<'seq, 'gc> Builder<'seq, 'gc> {
    /// Build a [`BoxSequence`] out of a [`Future`].
    ///
    /// The provided function is given an [`AsyncSequence`] handle, which the future should use to
    /// control the `Sequence` from the inside.
    ///
    /// We accept a function here rather than accepting an `impl Future` directly only because we
    /// need a way to provide an `AsyncSequence` handle that is moved into an `async` block. There
    /// is no reason for the given function to do *anything* other than accept a `seq` parameter and
    /// *immediately* move it into the returned `async` block. It is NOT legal to call methods on
    /// `AsyncSequence` outside of this future, calling methods on this handle directly in the given
    /// callback will panic!
    ///
    /// The returned async block can also capture stashed variables created from the provided
    /// [`StashedRootSet`] object, and things from outer scopes IF they are `'static`. You should
    /// *not* try to move anything branded with a `'gc` lifetime into the future, because this will
    /// result in compiler errors. There is no way in today's Rust for futures created from `async`
    /// blocks to implement the `Collect` trait, so we must accept futures that do *not* implement
    /// `Collect` and *can't* hold `'gc` branded values. This is the reason why we provide a method
    /// to stash values in the first place, to allow the future to hold onto these GC values, if
    /// only indirectly.
    ///
    /// The odd and circumlocutious way that the final `BoxSequence` is constructed is due to
    /// current limitations of Rust and avoiding the need to unnecessarily box the provided
    /// `impl Future`. This may be able to be improved in the future.
    ///
    /// # Panics
    ///
    /// All Rust yields (`.await`) within the created future must occur from calling an async method
    /// on the `AsyncSequence` handle. If some other future is `.await`ed, this will cause the outer
    /// `Sequence` poll methods to panic.
    ///
    /// Methods on `AsyncSequence` must *only* be called from the returned `async` block. Calling
    /// methods on `AsyncSequence` from the provided function directly will result in a panic.
    pub fn build<F>(self, f: impl FnOnce(AsyncSequence<'seq>) -> F) -> BoxSequence<'gc>
    where
        F: Future<Output = Result<SequenceReturn<'seq>, StashedError<'seq>>> + 'seq,
    {
        let fut = f(AsyncSequence {
            _invariant: PhantomData,
        });
        let sequence = boxed::Box::new_in(
            SequenceImpl {
                locals: self.locals,
                fut,
            },
            self.alloc,
        );
        let (ptr, alloc) = boxed::Box::into_raw_with_allocator(sequence);
        // SAFETY: This sequence impl normally has a 'seq lifetime on it, and we are casting it
        // away. The 'seq lifetime does not actually represent a real lifetime of any value, it
        // is purely used as a branding lifetime (the lifetime is actually *always* 'static, but
        // we can't prove that here, and it is not even required for soundness, only to prevent
        // uncollectable cycles).
        BoxSequence::from_box(unsafe { boxed::Box::from_raw_in(ptr as *mut dyn Sequence, alloc) })
    }
}

/// The held state for a `Sequence` being driven by a Rust async block.
///
/// Many methods on `AsyncSequence` are async; `.await`ing them causes the outer [`AsyncSequence`]
/// to return a non-tail [`SequencePoll`] value, triggering the appropriate action. If this action
/// results in an error, the async method will return the [`Error`] provided to [`Sequence::error`].
///
/// GC values stashed with the provided [`StashedRootSet`] return handles that are branded by
/// `'seq`, which ensures that they cannot be stored inside the GC context, nor can they escape the
/// body of the async block driving the [`AsyncSequence`].
///
/// These handles cannot escape their parent future, but they *can* be safely stored outside of
/// [`AsyncSequence::enter`] *and* across await points. Since they cannot be stored inside the GC
/// context *nor* escape their parent future, they are impossible to misuse; they perfectly mimic
/// what we could do if we could actually have the future itself implement `Collect`. If *only*
/// these local handles are used to store all GC values within the future, then the resulting
/// `AsyncSequence` will always be properly garbage collected, *even if* there are reference cycles
/// between the held locals and the sequence itself.
///
/// The same cannot be said for values stashed in the registry! An `AsyncSequence` has its own
/// [`StashedRootSet`] which allows ownership of stashed values to be tied to that *particular*
/// `AsyncSequence`. If GC values are instead stashed in the global registry, then those values will
/// live as long as *the global registry itself*, which is as long as the `Lua` instance itself is
/// alive. If such a handle is stored inside an async sequence and indirectly points back to the
/// sequence holding it, this will result in an uncollectable cycle.
///
/// In summary: Do NOT store handles to values stashed in the registry within async sequences,
/// instead *only* store handles to values stashed using the provided [`StashedRootSet`] object!
pub struct AsyncSequence<'seq> {
    _invariant: Invariant<'seq>,
}

impl<'seq> AsyncSequence<'seq> {
    /// Enter the garbage collector context within an async sequence.
    ///
    /// Unfortunately, today's Rust does not provide any way for generator (async block) state
    /// machines to possibly implement [`gc_arena::Collect`]. Therefore, we must ensure that garbage
    /// collected values **cannot** be directly stored by the enclosing async block. We guard all
    /// access to the garbage collector context to prevent this from happening, similar to the
    /// interface we use from the outside of the `Lua` context (like `Lua::enter`).
    pub fn enter<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(
            Context<'gc>,
            StashedRootSet<'seq, 'gc>,
            Execution<'gc, '_>,
            Stack<'gc, '_>,
        ) -> R,
        R: 'seq,
    {
        visit_shared(move |shared| {
            f(
                shared.ctx,
                StashedRootSet::new(shared.locals),
                shared.exec.reborrow(),
                shared.stack.reborrow(),
            )
        })
    }

    /// A version of [`AsyncSequence::enter`] which supports failure, and automatically turns any
    /// returned error into a [`StashedError`].
    pub fn try_enter<F, R>(&mut self, f: F) -> Result<R, StashedError<'seq>>
    where
        F: for<'gc> FnOnce(
            Context<'gc>,
            StashedRootSet<'seq, 'gc>,
            Execution<'gc, '_>,
            Stack<'gc, '_>,
        ) -> Result<R, Error<'gc>>,
        R: 'seq,
    {
        visit_shared(move |shared| {
            f(
                shared.ctx,
                StashedRootSet::new(shared.locals),
                shared.exec.reborrow(),
                shared.stack.reborrow(),
            )
            .map_err(|e| e.stash(&shared.ctx, StashedRootSet::new(shared.locals)))
        })
    }

    /// Return [`SequencePoll::Pending`] to the code driving the `Sequence`.
    ///
    /// In normal use, this will return control to the calling `Executor` and potentially the
    /// calling Rust code.
    ///
    /// This usually also allows garbage collection to take place, (depending on how the `Executor`
    /// is being driven).
    pub async fn pending(&mut self) {
        visit_shared(move |shared| {
            shared.set_next_op(SequenceOp::Pending);
        });
        wait_once().await;
        visit_shared(move |shared| {
            assert!(
                shared.error.is_none(),
                "SequencePoll::Pending cannot be followed by an error"
            );
        });
    }

    /// Call the given Lua function with arguments / returns starting at `bottom` in the Stack.
    pub async fn call(
        &mut self,
        func: &StashedFunction<'seq>,
        bottom: usize,
    ) -> Result<(), StashedError<'seq>> {
        visit_shared(move |shared| {
            shared.set_next_op(SequenceOp::Call {
                function: func.fetch(StashedRootSet::new(shared.locals)),
                bottom,
            });
        });
        wait_once().await;
        visit_shared(move |shared| {
            if let Some(err) = shared.error.take() {
                Err(err.stash(&shared.ctx, StashedRootSet::new(shared.locals)))
            } else {
                Ok(())
            }
        })
    }

    /// Yield to the calling code (or to `to_thread`) values starting at `bottom` in the stack. When
    /// this `Sequence` is resumed, resume arguments will be placed at `bottom` in the stack.
    pub async fn _yield(
        &mut self,
        to_thread: Option<&StashedThread<'seq>>,
        bottom: usize,
    ) -> Result<(), StashedError<'seq>> {
        visit_shared(move |shared| {
            shared.set_next_op(SequenceOp::Yield {
                to_thread: to_thread.map(|t| t.fetch(StashedRootSet::new(shared.locals))),
                bottom,
            });
        });
        wait_once().await;
        visit_shared(move |shared| {
            if let Some(err) = shared.error.take() {
                Err(err.stash(&shared.ctx, StashedRootSet::new(shared.locals)))
            } else {
                Ok(())
            }
        })
    }

    /// Resume `thread` with arguments starting at `bottom` in the stack. When the thread completes,
    /// return values will be placed at `bottom` in the stack.
    pub async fn resume(
        &mut self,
        thread: &StashedThread<'seq>,
        bottom: usize,
    ) -> Result<(), StashedError<'seq>> {
        visit_shared(move |shared| {
            shared.set_next_op(SequenceOp::Resume {
                thread: thread.fetch(StashedRootSet::new(shared.locals)),
                bottom,
            });
        });
        wait_once().await;
        visit_shared(move |shared| {
            if let Some(err) = shared.error.take() {
                Err(err.stash(&shared.ctx, StashedRootSet::new(shared.locals)))
            } else {
                Ok(())
            }
        })
    }
}

struct SequenceImpl<'gc, F> {
    locals: DynamicRootSet<'gc>,
    fut: F,
}

unsafe impl<'gc, F> Collect for SequenceImpl<'gc, F> {
    fn trace(&self, cc: &gc_arena::Collection) {
        // SAFETY: We ensure that `F` cannot hold `'gc` values elsewhere.
        self.locals.trace(cc);
    }
}

impl<'gc, 'seq, F> SequenceImpl<'gc, F>
where
    F: Future<Output = Result<SequenceReturn<'seq>, StashedError<'seq>>>,
{
    fn poll_fut(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
        error: Option<Error<'gc>>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        // SAFETY: We do not move out of the returned reference.
        let Self { locals, fut } = unsafe { self.get_unchecked_mut() };
        let locals = *locals;

        let mut next_op = None;

        let mut shared = Shared {
            locals,
            ctx,
            exec,
            stack: stack.reborrow(),
            error,
            next_op: &mut next_op,
        };

        let res = with_shared(&mut shared, || {
            // SAFETY: pinning is structural for field `fut`. We do not move it, provide any access
            // to it at all, and our drop impl is trivial.
            unsafe { Pin::new_unchecked(fut).poll(&mut task::Context::from_waker(&noop_waker())) }
        });

        match res {
            Poll::Ready(res) => {
                assert!(
                    next_op.is_none(),
                    "`AsyncSequence` async method not `await`ed"
                );
                match res {
                    Ok(SequenceReturn::Return) => Ok(SequencePoll::Return),
                    Ok(SequenceReturn::Call(function)) => Ok(SequencePoll::TailCall(
                        function.fetch(StashedRootSet::new(locals)),
                    )),
                    Ok(SequenceReturn::Yield(to_thread)) => Ok(SequencePoll::TailYield(
                        to_thread.map(|t| t.fetch(StashedRootSet::new(locals))),
                    )),
                    Ok(SequenceReturn::Resume(thread)) => Ok(SequencePoll::TailResume(
                        thread.fetch(StashedRootSet::new(locals)),
                    )),
                    Err(err) => Err(err.fetch(StashedRootSet::new(locals))),
                }
            }
            Poll::Pending => Ok(
                match next_op.expect("`await` of a future other than `AsyncSequence` methods") {
                    SequenceOp::Pending => SequencePoll::Pending,
                    SequenceOp::Call { function, bottom } => {
                        SequencePoll::Call { function, bottom }
                    }
                    SequenceOp::Yield { to_thread, bottom } => {
                        SequencePoll::Yield { to_thread, bottom }
                    }
                    SequenceOp::Resume { thread, bottom } => {
                        SequencePoll::Resume { thread, bottom }
                    }
                },
            ),
        }
    }
}

impl<'gc, 'seq, F> Sequence<'gc> for SequenceImpl<'gc, F>
where
    F: Future<Output = Result<SequenceReturn<'seq>, StashedError<'seq>>> + 'seq,
{
    fn poll(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        self.poll_fut(ctx, exec, stack, None)
    }

    fn error(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        error: Error<'gc>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        self.poll_fut(ctx, exec, stack, Some(error))
    }
}

enum SequenceOp<'gc> {
    Pending,
    Call {
        function: Function<'gc>,
        bottom: usize,
    },
    Yield {
        to_thread: Option<Thread<'gc>>,
        bottom: usize,
    },
    Resume {
        thread: Thread<'gc>,
        bottom: usize,
    },
}

// Invariant type that is also !Send and !Sync
type Invariant<'a> = PhantomData<*const Cell<&'a ()>>;

struct Shared<'gc, 'a> {
    locals: DynamicRootSet<'gc>,
    ctx: Context<'gc>,
    exec: Execution<'gc, 'a>,
    stack: Stack<'gc, 'a>,
    error: Option<Error<'gc>>,
    next_op: &'a mut Option<SequenceOp<'gc>>,
}

impl<'gc, 'a> Shared<'gc, 'a> {
    fn set_next_op(&mut self, op: SequenceOp<'gc>) {
        assert!(
            self.next_op.is_none(),
            "`AsyncSequence` async method not `await`ed"
        );
        *self.next_op = Some(op);
    }
}

thread_local! {
    static SHARED: Cell<*mut Shared<'static, 'static>> = const { Cell::new(ptr::null_mut()) };
}

fn with_shared<'gc, 'a, R>(shared: &mut Shared<'gc, 'a>, f: impl FnOnce() -> R) -> R {
    // SAFETY: We are erasing the lifetimes of the `Shared` thread local.
    //
    // We know this is sound because the only way we *access* the `Shared` local is through
    // `visit_shared`, which takes a callback which must work for *any* lifetimes 'gc and 'a. In
    // addition, We know the real lifetimes of `Shared` are valid for the body of this function,
    // and this function is the only thing that sets the thread local and it is unset before the
    // function exits using drop guards.
    unsafe {
        SHARED.set(mem::transmute::<
            *mut Shared<'_, '_>,
            *mut Shared<'static, 'static>,
        >(shared));
    }

    struct Guard;

    impl Drop for Guard {
        fn drop(&mut self) {
            SHARED.set(ptr::null_mut());
        }
    }

    let _guard = Guard;

    f()
}

fn visit_shared<R>(f: impl for<'gc, 'a> FnOnce(&'a mut Shared<'gc, 'a>) -> R) -> R {
    // SAFETY: This function must work for any lifetimes 'gc and 'a, so this is sound as long as the
    // call occurs within the callback given to `with_shared` (and this is guarded by setting the
    // SHARED ptr to null outside of `with_shared`). See the safety note in `with_shared`.
    unsafe {
        let shared =
            mem::transmute::<*mut Shared<'static, 'static>, *mut Shared<'_, '_>>(SHARED.get());
        assert!(!shared.is_null(), "AsyncSequence SHARED value unset");
        f(&mut *shared)
    }
}

fn noop_waker() -> Waker {
    const NOOP_RAW_WAKER: RawWaker = {
        const VTABLE: RawWakerVTable =
            RawWakerVTable::new(|_| NOOP_RAW_WAKER, |_| {}, |_| {}, |_| {});
        RawWaker::new(ptr::null(), &VTABLE)
    };

    // SAFETY: NOOP_RAW_WAKER VTable is trivial.
    unsafe { Waker::from_raw(NOOP_RAW_WAKER) }
}

async fn wait_once() {
    let mut done = false;
    poll_fn(move |_| {
        if done {
            Poll::Ready(())
        } else {
            done = true;
            Poll::Pending
        }
    })
    .await;
}
