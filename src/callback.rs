use std::{
    fmt,
    hash::{Hash, Hasher},
};

use allocator_api2::boxed;
use gc_arena::{allocator_api::MetricsAlloc, Collect, Gc, Mutation};

use crate::{Context, Error, Fuel, Function, Stack, Thread};

pub struct Execution<'gc, 'a> {
    /// The fuel parameter passed to `Executor::step`.
    pub fuel: &'a mut Fuel,
    /// The curently executing Thread.
    pub current_thread: Thread<'gc>,
    /// `true` if this thread is at the top of the execution stack.
    pub is_main: bool,
}

#[derive(Collect)]
#[collect(no_drop)]
pub enum CallbackReturn<'gc> {
    Return,
    Sequence(AnySequence<'gc>),
    Yield {
        to_thread: Option<Thread<'gc>>,
        then: Option<AnySequence<'gc>>,
    },
    Call {
        function: Function<'gc>,
        then: Option<AnySequence<'gc>>,
    },
    Resume {
        thread: Thread<'gc>,
        then: Option<AnySequence<'gc>>,
    },
}

pub trait Callback<'gc>: Collect {
    fn call(
        &self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>;
}

// Represents a callback as a single pointer with an inline VTable header.
#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct AnyCallback<'gc>(Gc<'gc, Header<'gc>>);

struct Header<'gc> {
    call: unsafe fn(
        *const (),
        Context<'gc>,
        Execution<'gc, '_>,
        Stack<'gc, '_>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
}

impl<'gc> AnyCallback<'gc> {
    pub fn new<C: Callback<'gc> + 'gc>(mc: &Mutation<'gc>, callback: C) -> Self {
        #[repr(C)]
        struct HeaderCallback<'gc, C> {
            header: Header<'gc>,
            callback: C,
        }

        // SAFETY: We can't auto-implement `Collect` due to the function pointer lifetimes, but
        // function pointers can't hold any data. It would be nice if function pointers could have
        // higher rank `for<'gc>` lifetimes.
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
                header: Header {
                    call: |ptr, ctx, exec, stack| unsafe {
                        let hc = ptr as *const HeaderCallback<C>;
                        ((*hc).callback).call(ctx, exec, stack)
                    },
                },
                callback,
            },
        );

        Self(unsafe { Gc::cast::<Header>(hc) })
    }

    pub fn from_fn<F>(mc: &Mutation<'gc>, call: F) -> AnyCallback<'gc>
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

    pub fn from_fn_with<R, F>(mc: &Mutation<'gc>, root: R, call: F) -> AnyCallback<'gc>
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

        impl<'gc, R, F> Callback<'gc> for RootCallback<R, F>
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

        AnyCallback::new(mc, RootCallback { root, call })
    }

    pub fn as_ptr(self) -> *const () {
        Gc::as_ptr(self.0) as *const ()
    }

    pub fn call(
        self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>> {
        unsafe { (self.0.call)(Gc::as_ptr(self.0) as *const (), ctx, exec, stack) }
    }
}

impl<'gc> fmt::Debug for AnyCallback<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Callback").field(&self.as_ptr()).finish()
    }
}

impl<'gc> PartialEq for AnyCallback<'gc> {
    fn eq(&self, other: &AnyCallback<'gc>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'gc> Eq for AnyCallback<'gc> {}

impl<'gc> Hash for AnyCallback<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
    }
}

pub enum SequencePoll<'gc> {
    /// Sequence pending, `Sequence::poll` will be called on the next step with the stack unchanged.
    Pending,
    /// Sequence finished, the values in the stack will be returned to the caller.
    Return,
    /// Yield the values in the stack inside a coroutine. If `is_tail` is true, then this also
    /// finishes the sequence, otherwise `Sequence::poll` will be called when the coroutine is
    /// resumed, or `Sequence::error` if the coroutine is resumed with an error.
    Yield {
        to_thread: Option<Thread<'gc>>,
        is_tail: bool,
    },
    /// Call the given function with the arguments in the stack. If `is_tail` is true, then this
    /// is a tail call, and the sequence is now finished, otherwise `Sequence::poll` will be called
    /// with the results of the function call, or if the function errors, `Sequence::error` will be
    /// called with the function error.
    Call {
        function: Function<'gc>,
        is_tail: bool,
    },
    Resume {
        thread: Thread<'gc>,
        is_tail: bool,
    },
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
pub struct AnySequence<'gc>(pub boxed::Box<dyn Sequence<'gc> + 'gc, MetricsAlloc<'gc>>);

impl<'gc> fmt::Debug for AnySequence<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Sequence")
            .field(&(self.0.as_ref() as *const _))
            .finish()
    }
}

impl<'gc> AnySequence<'gc> {
    pub fn new(mc: &Mutation<'gc>, sequence: impl Sequence<'gc> + 'gc) -> Self {
        let b = boxed::Box::new_in(sequence, MetricsAlloc::new(mc));
        let (ptr, alloc) = boxed::Box::into_raw_with_allocator(b);
        // TODO: Required unsafety due to do lack of `CoerceUnsized` on allocator_api2 `Box` type,
        // replace with safe cast when one of allocator_api or CoerceUnsized is stabilized.
        let b = unsafe { boxed::Box::from_raw_in(ptr as *mut dyn Sequence, alloc) };
        Self(b)
    }

    pub fn poll(
        &mut self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        self.0.poll(ctx, exec, stack)
    }

    pub fn error(
        &mut self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        error: Error<'gc>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        self.0.error(ctx, exec, error, stack)
    }
}
