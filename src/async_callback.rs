use std::{
    cell::Cell,
    future::{poll_fn, Future},
    marker::PhantomData,
    mem,
    pin::Pin,
    ptr,
    rc::Rc,
    task::{self, Poll, RawWaker, RawWakerVTable, Waker},
};

use gc_arena::{Collect, DynamicRootSet, Mutation};

use crate::{
    stash::{Fetchable, Stashable},
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
/// The provided `create` function is given two parameters: a [`Locals`] object to stash values
/// that will be owned by the future, and an [`AsyncSequence`] object which the future shuld use to
/// control the `Sequence` from the inside. The callback is called immediately and is not required
/// to be `'static`, it exists only to construct the controlling future.
///
/// The provided `create` function should only stash local variables and move the `AsyncSequence`
/// object into the returned future, nothing more. The `AsyncSequence` handle is `'static` for
/// simplicity, but it should not be moved out of the returned future or have methods called on it
/// outside of the returned future, this will result in panics!
///
/// The returned future can capture `'static` variables from the outer scope as well as stashed
/// local variables, but it *cannot* capture anything branded with a `'gc` lifetime, as this will
/// result in compiler errors. There is no way in today's Rust for futures created from `async`
/// blocks to implement the `Collect` trait, so we must accept futures that do *not* implement
/// `Collect` and *can't* hold `'gc` branded values. This is the reason why we provide a `Locals`:
/// to instead allow the future to hold onto these GC values indirectly.
///
/// # Panics
///
/// All Rust yields (`.await`) within the returned future must occur from calling an async method
/// on the `AsyncSequence` handle. If some other future is `.await`ed, this will cause the outer
/// `Sequence` poll methods to panic.
///
/// Methods on `AsyncSequence` must *only* be called from the returned future. Calling methods on
/// `AsyncSequence` directly from the provided create function or from storing the `AsyncSequence`
/// somewhere else wil result in a panic.
pub fn async_sequence<'gc, F>(
    mc: &Mutation<'gc>,
    create: impl FnOnce(Locals<'gc, '_>, AsyncSequence) -> F,
) -> BoxSequence<'gc>
where
    F: Future<Output = Result<SequenceReturn, StashedError>> + 'static,
{
    // NOTE: Unfortunately, we can't get away with a simple raw pointer to `SharedSlot` in the
    // `AsyncSequence` object here. The reason for this is that the lifetime of `AsyncSequence` is
    // not ACTUALLY bound to the returned `BoxSequence`; there is no way in today's Rust to properly
    // make `AsyncSequence` non-'static and bind this non-'static lifetime to the returned future
    // without requiring that the returned future be boxed, which results in double boxing in the
    // outer `BoxSequence`.
    //
    // Rust may change in the future to allow for a workable lifetime on `AsyncSequence` to prevent
    // this. This should allow soundly giving the `AsyncSequence` object a bare pointer to the
    // internals of its `BoxSequence` parent.
    //
    // NOTE: We are not bothering to mark the `Rc` allocation here in the GC metrics. Since
    // `DynamicRootSet` and `SequenceImpl` are always non-zero size, creating an async sequence
    // always marks some external allocation anyway, so this is just a small inaccuracy rather
    // than a potential way for Lua to allocate an arbitrary amount of un-marked memory.
    //
    // This will be much easier to handle properly when `allocator_api` is stable.
    let shared = Rc::new(SharedSlot::new());

    let roots = DynamicRootSet::new(mc);

    let fut = create(
        Locals {
            roots,
            _marker: PhantomData,
        },
        AsyncSequence {
            shared: shared.clone(),
        },
    );
    BoxSequence::new(mc, SequenceImpl { shared, roots, fut })
}

/// Return type for futures that are driving an async sequence.
///
/// This performs equivalent actions to [`CallbackReturn`](crate::CallbackReturn) and the tail
/// variants of [`SequencePoll`], so check those for more information on precisely what these
/// actions mean.
pub enum SequenceReturn {
    /// Sequence finished, all of the values in the stack will be returned to the caller.
    Return,
    /// Call the given function with the values in the stack as arguments.
    Call(StashedFunction),
    /// Yield the values in the stack.
    Yield(Option<StashedThread>),
    /// Resume the given thread with the values in the stack as arguments.
    Resume(StashedThread),
}

/// The held state for a `Sequence` being driven by a Rust async block.
///
/// Most methods on `AsyncSequence` are async; `.await`ing them causes the outer [`AsyncSequence`]
/// to return a non-tail [`SequencePoll`] value, triggering the appropriate action. If this action
/// results in an error, the async method will return the [`Error`] provided to [`Sequence::error`].
pub struct AsyncSequence {
    shared: Rc<SharedSlot>,
}

impl AsyncSequence {
    /// Enter the garbage collector context within an async sequence.
    ///
    /// Unfortunately, today's Rust does not provide any way for generator (async block) state
    /// machines to possibly implement [`gc_arena::Collect`]. Therefore, we must ensure that garbage
    /// collected values **cannot** be directly stored by the enclosing async block. We guard all
    /// access to the garbage collector context to prevent this from happening, similar to the
    /// interface we use from the outside of the `Lua` context (like `Lua::enter`).
    pub fn enter<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(Context<'gc>, Locals<'gc, '_>, Execution<'gc, '_>, Stack<'gc, '_>) -> R,
    {
        self.shared.visit(move |shared| {
            f(
                shared.ctx,
                Locals {
                    roots: shared.roots,
                    _marker: PhantomData,
                },
                shared.exec.reborrow(),
                shared.stack.reborrow(),
            )
        })
    }

    /// A version of [`AsyncSequence::enter`] which supports failure, and automatically stashes the
    /// returned error in this sequence's [`Locals`] instance.
    pub fn try_enter<F, R>(&mut self, f: F) -> Result<R, StashedError>
    where
        F: for<'gc> FnOnce(
            Context<'gc>,
            Locals<'gc, '_>,
            Execution<'gc, '_>,
            Stack<'gc, '_>,
        ) -> Result<R, Error<'gc>>,
    {
        self.shared.visit(move |shared| {
            f(
                shared.ctx,
                Locals {
                    roots: shared.roots,
                    _marker: PhantomData,
                },
                shared.exec.reborrow(),
                shared.stack.reborrow(),
            )
            .map_err(|e| e.stash(&shared.ctx, shared.roots))
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
        self.shared.visit(move |shared| {
            shared.set_next_op(SequenceOp::Pending);
        });
        wait_once().await;
        self.shared.visit(move |shared| {
            assert!(
                shared.error.is_none(),
                "SequencePoll::Pending cannot be followed by an error"
            );
        });
    }

    /// Call the given Lua function with arguments / returns starting at `bottom` in the Stack.
    pub async fn call(
        &mut self,
        func: &StashedFunction,
        bottom: usize,
    ) -> Result<(), StashedError> {
        self.shared.visit(move |shared| {
            shared.set_next_op(SequenceOp::Call {
                function: func.fetch(shared.roots),
                bottom,
            });
        });
        wait_once().await;
        self.shared.visit(move |shared| {
            if let Some(err) = shared.error.take() {
                Err(err.stash(&shared.ctx, shared.roots))
            } else {
                Ok(())
            }
        })
    }

    /// Yield to the calling code (or to `to_thread`) values starting at `bottom` in the stack. When
    /// this `Sequence` is resumed, resume arguments will be placed at `bottom` in the stack.
    pub async fn _yield(
        &mut self,
        to_thread: Option<&StashedThread>,
        bottom: usize,
    ) -> Result<(), StashedError> {
        self.shared.visit(move |shared| {
            shared.set_next_op(SequenceOp::Yield {
                to_thread: to_thread.map(|t| t.fetch(shared.roots)),
                bottom,
            });
        });
        wait_once().await;
        self.shared.visit(move |shared| {
            if let Some(err) = shared.error.take() {
                Err(err.stash(&shared.ctx, shared.roots))
            } else {
                Ok(())
            }
        })
    }

    /// Resume `thread` with arguments starting at `bottom` in the stack. When the thread completes,
    /// return values will be placed at `bottom` in the stack.
    pub async fn resume(
        &mut self,
        thread: &StashedThread,
        bottom: usize,
    ) -> Result<(), StashedError> {
        self.shared.visit(move |shared| {
            shared.set_next_op(SequenceOp::Resume {
                thread: thread.fetch(shared.roots),
                bottom,
            });
        });
        wait_once().await;
        self.shared.visit(move |shared| {
            if let Some(err) = shared.error.take() {
                Err(err.stash(&shared.ctx, shared.roots))
            } else {
                Ok(())
            }
        })
    }
}

/// A collection of stashed values that are local to a specific [`AsyncSequence`].
///
/// The returned handles can *only* be retrieved using the `Locals` handle provided by methods on
/// the *same* `AsyncSequence` that was used to stash them, they cannot be mixed with handles from
/// the global registry or handles from other async sequences, even though they share the same
/// stashed types.
///
/// Like "stashed values" in the registry, the handles returned by `Locals` are *not* branded with
/// `'gc`, which means the Rust borrow checker will not prevent you from storing them *anywhere*. Do
/// not do this! They are intended to be stored only in their parent future, so that the future can
/// safely store GC values outside of [`AsyncSequence::enter`] and across await points.
///
/// GC values stashed with `Locals` are treated by the garbage collector as being owned by the
/// outer `Sequence` impl. If *only* values stashed with the provided `Locals` object are used to
/// store all garbage collected values within the future, and the returned handles never escape
/// this future, then this perfectly mimics what we could do if we could implement `Collect` on the
/// future itself. If this rule is followed, the async sequence will *always* be properly garbage
/// collected, *even if* there are reference cycles between the held locals and the sequence itself.
///
/// The same cannot be said for registry stashed values! An async sequence has its own
/// [`gc_arena::DynamicRootSet`] which is what allows ownership of stashed values to be tied to
/// that *particular* async sequence. If GC values are instead stashed in the global registry, for
/// example with [`Context::stash`], then those values will live as long as *the global registry
/// itself*, which is as long as the whole `Lua` instance is alive. If such a stashed value is held
/// inside a future and indirectly points back to the outer sequence holding it, this will result in
/// an uncollectable cycle.
///
/// In summary: Do NOT store registry stashed values within the future driving async sequences,
/// instead only use `Locals` to stash values owned by the future and keep them inside the parent
/// future *only*.
#[derive(Copy, Clone)]
pub struct Locals<'gc, 'a> {
    roots: DynamicRootSet<'gc>,
    _marker: PhantomData<&'a ()>,
}

impl<'gc, 'a> Locals<'gc, 'a> {
    /// "Stash" a garbage collected value and return a `'static` handle that can be stored in the
    /// parent sequence async block.
    pub fn stash<S: Stashable<'gc>>(&self, mc: &Mutation<'gc>, s: S) -> S::Stashed {
        s.stash(mc, self.roots)
    }

    /// "Fetch" the real garbage collected value for a handle that has been returned from
    /// [`Locals::stash`].
    pub fn fetch<F: Fetchable>(&self, local: &F) -> F::Fetched<'gc> {
        local.fetch(self.roots)
    }
}

#[derive(Collect)]
#[collect(no_drop)]
struct SequenceImpl<'gc, F> {
    #[collect(require_static)]
    shared: Rc<SharedSlot>,
    roots: DynamicRootSet<'gc>,
    #[collect(require_static)]
    fut: F,
}

impl<'gc, F> SequenceImpl<'gc, F>
where
    F: Future<Output = Result<SequenceReturn, StashedError>>,
{
    fn poll_fut(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
        error: Option<Error<'gc>>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        // SAFETY: We do not move out of the returned reference.
        let Self {
            shared,
            roots: locals,
            fut,
        } = unsafe { self.get_unchecked_mut() };
        let locals = *locals;

        let mut next_op = None;

        let res = shared.with(
            &mut Shared {
                roots: locals,
                ctx,
                exec,
                stack: stack.reborrow(),
                error,
                next_op: &mut next_op,
            },
            || {
                // SAFETY: pinning is structural for field `fut`. We do not move it, provide any access
                // to it at all, and our drop impl is trivial.
                unsafe {
                    Pin::new_unchecked(fut).poll(&mut task::Context::from_waker(&noop_waker()))
                }
            },
        );

        match res {
            Poll::Ready(res) => {
                assert!(
                    next_op.is_none(),
                    "`AsyncSequence` async method not `await`ed"
                );
                match res {
                    Ok(SequenceReturn::Return) => Ok(SequencePoll::Return),
                    Ok(SequenceReturn::Call(function)) => {
                        Ok(SequencePoll::TailCall(function.fetch(locals)))
                    }
                    Ok(SequenceReturn::Yield(to_thread)) => {
                        Ok(SequencePoll::TailYield(to_thread.map(|t| t.fetch(locals))))
                    }
                    Ok(SequenceReturn::Resume(thread)) => {
                        Ok(SequencePoll::TailResume(thread.fetch(locals)))
                    }
                    Err(err) => Err(err.fetch(locals)),
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

impl<'gc, F> Sequence<'gc> for SequenceImpl<'gc, F>
where
    F: Future<Output = Result<SequenceReturn, StashedError>> + 'static,
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

struct Shared<'gc, 'a> {
    roots: DynamicRootSet<'gc>,
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

struct SharedSlot(Cell<*mut Shared<'static, 'static>>);

impl SharedSlot {
    fn new() -> Self {
        Self(Cell::new(ptr::null_mut()))
    }

    fn with<'gc, 'a, R>(&self, shared: &mut Shared<'gc, 'a>, f: impl FnOnce() -> R) -> R {
        // SAFETY: We are erasing the lifetimes of the `Shared` pointer.
        //
        // We know this is sound because the only way we *access* the `Shared` pointer is through
        // `SharedSlot::visit`, which takes a callback which must work for *any* lifetimes 'gc and
        // 'a. In addition, We know the real lifetimes of `Shared` are valid for the body of this
        // function, and this function is the only thing that sets the inner pointer and it is unset
        // before the function exits using drop guards.
        unsafe {
            self.0.set(mem::transmute::<
                *mut Shared<'_, '_>,
                *mut Shared<'static, 'static>,
            >(shared));
        }

        struct Guard<'a>(&'a SharedSlot);

        impl<'a> Drop for Guard<'a> {
            fn drop(&mut self) {
                self.0 .0.set(ptr::null_mut());
            }
        }

        let _guard = Guard(self);

        f()
    }

    fn visit<R>(&self, f: impl for<'gc, 'a> FnOnce(&'a mut Shared<'gc, 'a>) -> R) -> R {
        // SAFETY: This function must work for any lifetimes 'gc and 'a, so this is sound as
        // long as the call occurs within the callback given to `SharedSlot::with` (and this is
        // guarded by setting the ptr to null outside of `SharedSlot::with`). See the safety note
        // in `SharedSlot::with`.
        unsafe {
            let shared =
                mem::transmute::<*mut Shared<'static, 'static>, *mut Shared<'_, '_>>(self.0.get());
            assert!(!shared.is_null(), "AsyncSequence SHARED value unset");
            f(&mut *shared)
        }
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
