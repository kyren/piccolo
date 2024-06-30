use std::{
    fmt,
    hash::{Hash, Hasher},
    pin::Pin,
};

use allocator_api2::boxed;
use gc_arena::{allocator_api::MetricsAlloc, Collect, Gc, Mutation};

use crate::{Context, Error, Execution, Function, Stack, Thread};

/// Describes the next action for an [`Executor`](crate::Executor) to take after a callback has
/// returned.
#[derive(Collect)]
#[collect(no_drop)]
pub enum CallbackReturn<'gc> {
    /// Return all values in the stack to the caller.
    Return,
    /// Start polling the given [`Sequence`].
    ///
    /// The running `Executor` will begin polling the given sequence with the values currently in
    /// the stack. Once the sequence returns, the return values will be returned to the caller.
    Sequence(BoxSequence<'gc>),
    /// Call the given function with the values in the stack.
    ///
    /// Once the called function returns, if a `then` sequence is given, the sequence will begin
    /// being be polled with the values returned by the called function. Otherwise, the return
    /// values of the called function will be returned to the caller of this callback.
    Call {
        function: Function<'gc>,
        then: Option<BoxSequence<'gc>>,
    },
    /// Yield the values in the stack.
    ///
    /// If no `to_thread` is given, then this yields values to the calling thread. If
    /// this is the top thread in an `Executor`, then this will put the `Executor` in the
    /// [`ExecutorMode::Result`](crate::ExecutorMode::Result) mode.
    ///
    /// If `to_thread` is given, then this yields to that thread rather than the caller. This has
    /// no direct equivalent in other Lua variants, but it is roughly equivalent to this Lua code,
    /// *without* keeping the currently running thread in the thread stack:
    ///
    /// ```lua
    /// coroutine.yield(coroutine.resume(to_thread))
    /// ```
    ///
    /// In other words, it *replaces* the currently running thread by resuming the target thread.
    ///
    /// If a `then` sequence is given, then once the current thread is resumed, it will be resumed
    /// by polling the given sequence with the resume arguments in the stack.
    ///
    /// If no `then` sequence is given, then once the current thread is resumed, it will act as
    /// though this callback has returned with the provided resume arguments.
    Yield {
        to_thread: Option<Thread<'gc>>,
        then: Option<BoxSequence<'gc>>,
    },
    /// Resume the given thread, with resume arguments in the current stack.
    ///
    /// If a `then` sequence is given, then once the thread returns, the `Executor` will begin
    /// polling the given sequence with the return values from the thread. Otherwise, the return
    /// values from the thread will be returned directly to the caller.
    Resume {
        thread: Thread<'gc>,
        then: Option<BoxSequence<'gc>>,
    },
}

/// A trait for Lua functions that are implemented in Rust.
///
/// All arguments and returns are handled through the provided `stack`, which avoids allocating
/// space on the heap for them on each call.
pub trait CallbackFn<'gc>: Collect {
    fn call(
        &self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>;
}

/// A garbage collected instance of an object that impelments [`CallbackFn`].
#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Callback<'gc>(Gc<'gc, CallbackInner<'gc>>);

// We represent a callback as a single pointer with an inline VTable header.
pub struct CallbackInner<'gc> {
    call: unsafe fn(
        *const CallbackInner<'gc>,
        Context<'gc>,
        Execution<'gc, '_>,
        Stack<'gc, '_>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
}

impl<'gc> Callback<'gc> {
    pub fn new<C: CallbackFn<'gc> + 'gc>(mc: &Mutation<'gc>, callback: C) -> Self {
        #[repr(C)]
        struct HeaderCallback<'gc, C> {
            header: CallbackInner<'gc>,
            callback: C,
        }

        // SAFETY: We can't auto-implement `Collect` due to the function pointer lifetimes, but
        // function pointers can't hold any data.
        unsafe impl<'gc, C: Collect> Collect for HeaderCallback<'gc, C> {
            fn needs_trace() -> bool
            where
                Self: Sized,
            {
                C::needs_trace()
            }

            fn trace(&self, cc: &gc_arena::Collection) {
                self.callback.trace(cc)
            }
        }

        let hc = Gc::new(
            mc,
            HeaderCallback {
                header: CallbackInner {
                    call: |ptr, ctx, exec, stack| unsafe {
                        let hc = ptr as *const HeaderCallback<C>;
                        ((*hc).callback).call(ctx, exec, stack)
                    },
                },
                callback,
            },
        );

        Self(unsafe { Gc::cast::<CallbackInner>(hc) })
    }

    /// Create a callback from a Rust function.
    ///
    /// The function must be `'static` because Rust closures cannot implement `Collect`. If you need
    /// to associate GC data with this function, use [`Callback::from_fn_with`].
    pub fn from_fn<F>(mc: &Mutation<'gc>, call: F) -> Callback<'gc>
    where
        F: 'static
            + Fn(
                Context<'gc>,
                Execution<'gc, '_>,
                Stack<'gc, '_>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        Self::from_fn_with(mc, (), move |_, ctx, exec, stack| call(ctx, exec, stack))
    }

    /// Create a callback from a Rust function together with a GC object.
    pub fn from_fn_with<R, F>(mc: &Mutation<'gc>, root: R, call: F) -> Callback<'gc>
    where
        R: 'gc + Collect,
        F: 'static
            + Fn(
                &R,
                Context<'gc>,
                Execution<'gc, '_>,
                Stack<'gc, '_>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct RootCallback<R, F> {
            root: R,
            #[collect(require_static)]
            call: F,
        }

        impl<'gc, R, F> CallbackFn<'gc> for RootCallback<R, F>
        where
            R: 'gc + Collect,
            F: 'static
                + Fn(
                    &R,
                    Context<'gc>,
                    Execution<'gc, '_>,
                    Stack<'gc, '_>,
                ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
        {
            fn call(
                &self,
                ctx: Context<'gc>,
                exec: Execution<'gc, '_>,
                stack: Stack<'gc, '_>,
            ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
                (self.call)(&self.root, ctx, exec, stack)
            }
        }

        Callback::new(mc, RootCallback { root, call })
    }

    pub fn from_inner(inner: Gc<'gc, CallbackInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, CallbackInner<'gc>> {
        self.0
    }

    pub fn call(
        self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
        unsafe { (self.0.call)(Gc::as_ptr(self.0), ctx, exec, stack) }
    }
}

impl<'gc> fmt::Debug for Callback<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Callback")
            .field(&Gc::as_ptr(self.0))
            .finish()
    }
}

impl<'gc> PartialEq for Callback<'gc> {
    fn eq(&self, other: &Callback<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Callback<'gc> {}

impl<'gc> Hash for Callback<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Gc::as_ptr(self.0).hash(state)
    }
}

/// Value returned by [`Sequence::poll`], describing the next action that the
/// [`Executor`](crate::Executor) should take.
///
/// These actions mirror the same ones that one-shot callbacks can perform with `CallbackReturn`,
/// see [`CallbackReturn`] for more information.
pub enum SequencePoll<'gc> {
    /// `Sequence` is pending, `Sequence::poll` will be called on the next step with the stack
    /// unchanged.
    Pending,
    /// Call the given functions with the arguments in the stack starting at `bottom`. When the
    /// function returns, `Sequence::poll` will be called with the return values placed into the
    /// stack starting at `bottom`. If the given function errors, then `Sequence::error` will be
    /// called and the stack will instead be truncated to `bottom`.
    Call {
        bottom: usize,
        function: Function<'gc>,
    },
    /// Yield the values in the stack starting at `bottom`. When the `Sequence` is resumed, the
    /// resume arguments will be on the stack starting at `bottom`.
    Yield {
        bottom: usize,
        to_thread: Option<Thread<'gc>>,
    },
    /// Resume the given thread with arguments starting at `bottom`. When the thread returns, the
    /// return values will be placed on the stack starting at `bottom`.
    Resume { bottom: usize, thread: Thread<'gc> },
    /// Finish polling this `Sequence` and return the values in the stack to the caller.
    Return,
    /// Finish polling this `Sequence` by calling the given function with the arguments in the
    /// stack. The return values of the called function will be returned to the caller of this
    /// sequence.
    TailCall(Function<'gc>),
    /// Finish polling this `Sequence` by yielding the values in the stack. Once the current thread
    /// is resumed, the resume arguments will be returned to the caller of this sequence.
    TailYield(Option<Thread<'gc>>),
    /// Finish polling this `Sequence` by resuming the given thread with arguments in the stack.
    /// Once the given thread returns, the return values will be returned to the caller of this
    /// sequence.
    TailResume(Thread<'gc>),
}

/// A callback that can suspend itself, waiting on other actions to complete before being resumed.
///
/// `Sequence`s are started when a [`Callback`] triggers them by returning them inside a
/// [`CallbackReturn`]. Once started, the running [`Executor`](crate::Executor) will poll them to
/// completion.
///
/// Types implementing `Sequence` can trigger actions in the `Executor` by returning
/// [`SequencePoll`] values from [`Sequence::poll`]. Once the triggered action completes, either
/// [`Sequence::poll`] or [`Sequence::error`] will be called, depending on whether the triggered
/// action has completed successfully or errored.
pub trait Sequence<'gc>: Collect {
    /// Called by the running [`Executor`](crate::Executor) when the `Sequence` is first started
    /// with the arguments to the `Sequence`, and whenever a triggered action completes with the
    /// action's return values.
    ///
    /// The [`SequencePoll`] return value tells the running `Executor` what action to take next.
    fn poll(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>>;

    /// Called if triggered action has errored, allowing the `Sequence` to potentially handle the
    /// error.
    ///
    /// By default, this method will simply fail with the provided error, bubbling it up to the
    /// caller.
    fn error(
        self: Pin<&mut Self>,
        _ctx: Context<'gc>,
        _exec: Execution<'gc, '_>,
        error: Error<'gc>,
        _stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        Err(error)
    }
}

/// A `Box` containing a value that implements [`Sequence`].
///
/// We use a `Box` rather than `Gc` because a `Sequence` is mutable and not shared -- it is
/// generally owned only by the `Thread` in which it is running.
pub struct BoxSequence<'gc>(Pin<boxed::Box<dyn Sequence<'gc> + 'gc, MetricsAlloc<'static>>>);

unsafe impl<'gc> Collect for BoxSequence<'gc> {
    fn trace(&self, cc: &gc_arena::Collection) {
        // SAFETY: We have to manually implement `Collect` for `BoxSequence<'gc>` because `gc-arena`
        // does not provide `Collect` impls for `Pin<T>`.
        self.0.as_ref().get_ref().trace(cc);
    }
}

impl<'gc> fmt::Debug for BoxSequence<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Sequence")
            .field(&(self.0.as_ref().get_ref() as *const _))
            .finish()
    }
}

impl<'gc> BoxSequence<'gc> {
    pub fn new(mc: &Mutation<'gc>, sequence: impl Sequence<'gc> + 'gc) -> Self {
        let b = boxed::Box::new_in(sequence, MetricsAlloc::new_static(mc));
        // TODO: Required unsafety due to do lack of `CoerceUnsized` on allocator_api2 `Box` type,
        // replace with safe cast when one of allocator_api or CoerceUnsized is stabilized.
        let (ptr, alloc) = boxed::Box::into_raw_with_allocator(b);
        let b = unsafe { boxed::Box::from_raw_in(ptr as *mut dyn Sequence, alloc) };
        Self(boxed::Box::into_pin(b))
    }

    pub fn from_box(b: boxed::Box<dyn Sequence<'gc> + 'gc, MetricsAlloc<'static>>) -> Self {
        Self(boxed::Box::into_pin(b))
    }

    pub fn poll(
        self: &mut Self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        self.0.as_mut().poll(ctx, exec, stack)
    }

    pub fn error(
        self: &mut Self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        error: Error<'gc>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        self.0.as_mut().error(ctx, exec, error, stack)
    }
}
