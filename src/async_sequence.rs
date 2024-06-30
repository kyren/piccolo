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
    stash::{Fetchable, Stashable},
    BoxSequence, Context, Error, Execution, Function, RuntimeError, Sequence, SequencePoll, Stack,
    StashedCallback, StashedClosure, StashedError, StashedFunction, StashedString, StashedTable,
    StashedThread, StashedUserData, StashedValue, Thread,
};

/// Return type for futures that are driving an async sequence.
///
/// This performs equivalent actions to [`CallbackReturn`](crate::CallbackReturn) and the tail
/// variants of [`SequencePoll`], so check those for more information on precisely what these
/// actions mean.
pub enum SequenceReturn<'seq> {
    /// Sequence finished, all of the values in the stack will be returned to the caller.
    Return,
    /// Call the given function with the values in the stack as arguments.
    Call(LocalFunction<'seq>),
    /// Yield the values in the stack.
    Yield(Option<LocalThread<'seq>>),
    /// Resume the given thread with the values in the stack as arguments.
    Resume(LocalThread<'seq>),
}

pub struct SequenceFuture<'seq>(
    boxed::Box<
        dyn Future<Output = Result<SequenceReturn<'seq>, LocalError<'seq>>> + 'seq,
        MetricsAlloc<'static>,
    >,
);

impl<'seq> SequenceFuture<'seq> {
    pub fn new<'gc>(
        mc: &Mutation<'gc>,
        fut: impl Future<Output = Result<SequenceReturn<'seq>, LocalError<'seq>>> + 'seq,
    ) -> Self {
        let b = boxed::Box::new_in(fut, MetricsAlloc::new_static(mc));
        // TODO: Required unsafety due to do lack of `CoerceUnsized` on allocator_api2 `Box` type,
        // replace with safe cast when one of allocator_api or CoerceUnsized is stabilized.
        let (ptr, alloc) = boxed::Box::into_raw_with_allocator(b);
        let b = unsafe {
            boxed::Box::from_raw_in(
                ptr as *mut (dyn Future<Output = Result<SequenceReturn<'seq>, LocalError<'seq>>>
                     + 'seq),
                alloc,
            )
        };
        Self(b)
    }
}

/// An implementation of `Sequence` that drives an inner `async` block.
#[derive(Collect)]
#[collect(no_drop)]
pub struct AsyncSequence<'gc> {
    locals: DynamicRootSet<'gc>,
    #[collect(require_static)]
    fut: Pin<
        boxed::Box<
            dyn Future<Output = Result<SequenceReturn<'static>, LocalError<'static>>>,
            MetricsAlloc<'static>,
        >,
    >,
}

impl<'gc> AsyncSequence<'gc> {
    /// Create a `Sequence` impl from a Rust future that can suspend, call Lua functions, yield to
    /// Lua, and resume threads as async method calls on a held [`SequenceState`].
    ///
    /// Can be used to implement `Sequence` in a way MUCH easier than manual state machines.
    ///
    /// Currently uses `async` to do what in the future could be more directly accomplished with
    /// coroutines (see the unstable [`std::ops::Coroutine`] trait). The [`std::task::Context`]
    /// available within the created future is **meaningless** and has a NOOP waker; we are
    /// only using `async` as a stable way to express what would be better expressed as a simple
    /// coroutine.
    ///
    /// It is possible to integrate proper async APIs with `piccolo`, and to even have a method to
    /// "wake" Lua coroutines with a *real* [`std::task::Waker`], but simply calling an external
    /// async function from the created future here is *not* the way to do it. It will not do what
    /// you want, and probably will result in panics.
    ///
    /// The provided `create` function is given two parameters: a [`Locals`] handle to create
    /// [`Local`]s that will be owned by the future, and a [`SequenceState`] to move into the
    /// future. The callback is called immediately and is not required to be `'static`, it exists
    /// only to make the `'seq` lifetime generative and is required for correctness. The function
    /// should stash any local variables needed by the future from the outer context, then move
    /// *only* those locals and the provided `SequenceState` object into the returned future.
    ///
    /// # Panics
    ///
    /// All Rust yields (`.await`) within the created future must occur from calling an async method
    /// on `SequenceState`. Otherwise, the outer `AsyncSequence` poll methods will panic.
    ///
    /// Methods on `SequenceState` must *only* be called from the returned `SeqFuture`.
    /// `SequenceState` is passed to the provided function only so that it can be *moved into*
    /// the future and called there. Calling methods on `SequenceState` from the provided function
    /// directly will result in a panic.
    pub fn new(
        mc: &Mutation<'gc>,
        create: impl for<'seq> FnOnce(Locals<'seq, 'gc>, SequenceState<'seq>) -> SequenceFuture<'seq>,
    ) -> Self {
        let locals = DynamicRootSet::new(mc);
        let fut = create(
            Locals {
                locals,
                _invariant: PhantomData,
            },
            SequenceState {
                _invariant: PhantomData,
            },
        );

        Self {
            locals,
            fut: boxed::Box::into_pin(fut.0),
        }
    }

    /// A convenience function equivalent to `BoxSequence::new(mc, AsyncSequence::new(mc, create))`.
    pub fn new_box(
        mc: &Mutation<'gc>,
        create: impl for<'seq> FnOnce(Locals<'seq, 'gc>, SequenceState<'seq>) -> SequenceFuture<'seq>,
    ) -> BoxSequence<'gc> {
        BoxSequence::new(mc, AsyncSequence::new(mc, create))
    }

    fn poll_fut(
        &mut self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
        error: Option<Error<'gc>>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        let mut next_op = None;

        let mut shared = Shared {
            locals: self.locals,
            ctx,
            exec,
            stack: stack.reborrow(),
            error,
            next_op: &mut next_op,
        };

        let res = with_shared(&mut shared, || {
            self.fut
                .as_mut()
                .poll(&mut task::Context::from_waker(&noop_waker()))
        });

        match res {
            Poll::Ready(res) => {
                assert!(
                    next_op.is_none(),
                    "`SequenceState` async method not `await`ed"
                );
                match res {
                    Ok(SequenceReturn::Return) => Ok(SequencePoll::Return),
                    Ok(SequenceReturn::Call(function)) => {
                        Ok(SequencePoll::TailCall(function.fetch(self.locals)))
                    }
                    Ok(SequenceReturn::Yield(to_thread)) => Ok(SequencePoll::TailYield(
                        to_thread.map(|t| t.fetch(self.locals)),
                    )),
                    Ok(SequenceReturn::Resume(thread)) => {
                        Ok(SequencePoll::TailResume(thread.fetch(self.locals)))
                    }
                    Err(err) => Err(err.fetch(self.locals)),
                }
            }
            Poll::Pending => Ok(
                match next_op.expect("`await` of a future other than `SequenceState` methods") {
                    SeqOp::Pending => SequencePoll::Pending,
                    SeqOp::Call { function, bottom } => SequencePoll::Call { function, bottom },
                    SeqOp::Yield { to_thread, bottom } => SequencePoll::Yield { to_thread, bottom },
                    SeqOp::Resume { thread, bottom } => SequencePoll::Resume { thread, bottom },
                },
            ),
        }
    }
}

impl<'gc> Sequence<'gc> for AsyncSequence<'gc> {
    fn poll(
        &mut self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        self.poll_fut(ctx, exec, stack, None)
    }

    fn error(
        &mut self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        error: Error<'gc>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        self.poll_fut(ctx, exec, stack, Some(error))
    }
}

/// A collection of stashed values that are local to a specific [`AsyncSequence`].
///
/// [`Local`] values are branded by `'seq` and cannot escape their parent `AsyncSequence` and are
/// (for the purposes of garbage collection) considered *owned* by the parent `AsyncSequence`.
/// Because of this, they correctly mimic what we could do if async blocks themselves could be
/// traced, and so can't lead to uncollectable cycles with their parent.
#[derive(Copy, Clone)]
pub struct Locals<'seq, 'gc> {
    locals: DynamicRootSet<'gc>,
    _invariant: Invariant<'seq>,
}

impl<'seq, 'gc> Locals<'seq, 'gc> {
    /// "Stash" a garbage collected value and return a handle branded with `'seq` that can be stored
    /// in the parent sequence async block.
    pub fn stash<S: Stashable<'gc>>(&self, mc: &Mutation<'gc>, s: S) -> Local<'seq, S::Stashed> {
        Local::stash(mc, self.locals, s)
    }

    /// "Fetch" the real garbage collected value for a handle that has been returned from
    /// [`Locals::stash`].
    pub fn fetch<F: Fetchable<'gc>>(&self, local: &Local<'seq, F>) -> F::Fetched {
        local.fetch(self.locals)
    }
}

/// A local variable for an async sequence.
///
/// Like "stashed values" in the registry, `Local`s are *not* branded with `'gc`. Unlike registry
/// stashed values, they are instead branded by `'seq`, which ensures that they cannot escape the
/// body of the async block driving the [`AsyncSequence`].
///
/// Locals cannot escape their parent future, but they *can* be safely stored outside of
/// [`SequenceState::enter`] *and* across await points. If *only* `Local` variables are used to
/// store all garbage collected values within the future, then resulting `AsyncSequence` will always
/// be properly garbage collected, *even if* there are reference cycles between the held locals and
/// the sequence itself.
///
/// The same cannot be said for registry stashed values! An `AsyncSequence` has its own
/// [`gc_arena::DynamicRootSet`] which allows `Local`s ownership to be tied to that *particular*
/// `AsyncSequence`. If GC values are instead stashed in the global registry, for example with
/// `ctx.stash()`, then those values will live as long as *the global registry itself*, which is as
/// long as the `Lua` instance itself is alive. If such a stashed value indirectly points back to
/// the `AsyncSequence` holding it, this will result in an uncollectable cycle.
///
/// In summary: Do NOT store registry stashed values (`ctx.stash()`) within async sequences, instead
/// only use `Local` variables!
#[derive(Clone)]
pub struct Local<'seq, S> {
    stashed: S,
    _invariant: Invariant<'seq>,
}

impl<'seq, S> Local<'seq, S> {
    fn stash<'gc>(
        mc: &Mutation<'gc>,
        locals: DynamicRootSet<'gc>,
        v: impl Stashable<'gc, Stashed = S>,
    ) -> Self {
        Local {
            stashed: v.stash(mc, locals),
            _invariant: PhantomData,
        }
    }
}

impl<'seq, 'gc, S> Local<'seq, S>
where
    S: Fetchable<'gc>,
{
    fn fetch(&self, locals: DynamicRootSet<'gc>) -> S::Fetched {
        self.stashed.fetch(locals)
    }
}

pub type LocalString<'seq> = Local<'seq, StashedString>;
pub type LocalTable<'seq> = Local<'seq, StashedTable>;
pub type LocalClosure<'seq> = Local<'seq, StashedClosure>;
pub type LocalCallback<'seq> = Local<'seq, StashedCallback>;
pub type LocalThread<'seq> = Local<'seq, StashedThread>;
pub type LocalUserData<'seq> = Local<'seq, StashedUserData>;
pub type LocalFunction<'seq> = Local<'seq, StashedFunction>;
pub type LocalValue<'seq> = Local<'seq, StashedValue>;
pub type LocalError<'seq> = Local<'seq, StashedError>;

impl<'seq> From<RuntimeError> for LocalError<'seq> {
    fn from(error: RuntimeError) -> Self {
        Local {
            stashed: StashedError::Runtime(error),
            _invariant: PhantomData,
        }
    }
}

impl<'seq, E: Into<anyhow::Error>> From<E> for LocalError<'seq> {
    fn from(error: E) -> Self {
        RuntimeError::from(error).into()
    }
}

/// The held state for a `Sequence` being driven by a Rust async block.
///
/// `SequenceState` and [`Local`] are both branded by a generative `'seq` lifetime to ensure that
/// neither can escape their enclosing async block.
///
/// Many methods on `SequenceState` are async; `.await`ing them causes the outer [`AsyncSequence`]
/// to return a non-tail [`SequencePoll`] value, triggering the appropriate action. If this action
/// results in an error, the async method will return the [`Error`] provided to [`Sequence::error`].
pub struct SequenceState<'seq> {
    _invariant: Invariant<'seq>,
}

impl<'seq> SequenceState<'seq> {
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
            Locals<'seq, 'gc>,
            Execution<'gc, '_>,
            Stack<'gc, '_>,
        ) -> R,
        R: 'seq,
    {
        visit_shared(move |shared| {
            f(
                shared.ctx,
                Locals {
                    locals: shared.locals,
                    _invariant: PhantomData,
                },
                shared.exec.reborrow(),
                shared.stack.reborrow(),
            )
        })
    }

    /// A version of [`SequenceState::enter`] which supports failure, and automatically turns any
    /// returned error into a [`LocalError`].
    pub fn try_enter<F, R>(&mut self, f: F) -> Result<R, LocalError<'seq>>
    where
        F: for<'gc> FnOnce(
            Context<'gc>,
            Locals<'seq, 'gc>,
            Execution<'gc, '_>,
            Stack<'gc, '_>,
        ) -> Result<R, Error<'gc>>,
        R: 'seq,
    {
        visit_shared(move |shared| {
            f(
                shared.ctx,
                Locals {
                    locals: shared.locals,
                    _invariant: PhantomData,
                },
                shared.exec.reborrow(),
                shared.stack.reborrow(),
            )
            .map_err(|e| Local::stash(&shared.ctx, shared.locals, e))
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
            shared.set_next_op(SeqOp::Pending);
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
        func: &LocalFunction<'seq>,
        bottom: usize,
    ) -> Result<(), LocalError<'seq>> {
        visit_shared(move |shared| {
            shared.set_next_op(SeqOp::Call {
                function: func.fetch(shared.locals),
                bottom,
            });
        });
        wait_once().await;
        visit_shared(move |shared| {
            if let Some(err) = shared.error.take() {
                Err(Local::stash(&shared.ctx, shared.locals, err))
            } else {
                Ok(())
            }
        })
    }

    /// Yield to the calling code (or to `to_thread`) values starting at `bottom` in the stack. When
    /// this `Sequence` is resumed, resume arguments will be placed at `bottom` in the stack.
    pub async fn _yield(
        &mut self,
        to_thread: Option<&LocalThread<'seq>>,
        bottom: usize,
    ) -> Result<(), LocalError<'seq>> {
        visit_shared(move |shared| {
            shared.set_next_op(SeqOp::Yield {
                to_thread: to_thread.map(|t| t.fetch(shared.locals)),
                bottom,
            });
        });
        wait_once().await;
        visit_shared(move |shared| {
            if let Some(err) = shared.error.take() {
                Err(Local::stash(&shared.ctx, shared.locals, err))
            } else {
                Ok(())
            }
        })
    }

    /// Resume `thread` with arguments starting at `bottom` in the stack. When the thread completes,
    /// return values will be placed at `bottom` in the stack.
    pub async fn resume(
        &mut self,
        thread: &LocalThread<'seq>,
        bottom: usize,
    ) -> Result<(), LocalError<'seq>> {
        visit_shared(move |shared| {
            shared.set_next_op(SeqOp::Resume {
                thread: thread.fetch(shared.locals),
                bottom,
            });
        });
        wait_once().await;
        visit_shared(move |shared| {
            if let Some(err) = shared.error.take() {
                Err(Local::stash(&shared.ctx, shared.locals, err))
            } else {
                Ok(())
            }
        })
    }
}

enum SeqOp<'gc> {
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
    next_op: &'a mut Option<SeqOp<'gc>>,
}

impl<'gc, 'a> Shared<'gc, 'a> {
    fn set_next_op(&mut self, op: SeqOp<'gc>) {
        assert!(
            self.next_op.is_none(),
            "`SequenceState` async method not `await`ed"
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
    poll_fn(|_| {
        if done {
            Poll::Ready(())
        } else {
            done = true;
            Poll::Pending
        }
    })
    .await;
}
