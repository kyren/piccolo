use std::{
    fmt,
    hash::{Hash, Hasher},
    ops,
};

use allocator_api2::boxed;
use gc_arena::{allocator_api::MetricsAlloc, Collect, Gc, Mutation};

use crate::{Context, Error, Execution, Function, Stack, Thread};

#[derive(Collect)]
#[collect(no_drop)]
pub enum CallbackReturn<'gc> {
    Return,
    Sequence(BoxSequence<'gc>),
    Yield {
        to_thread: Option<Thread<'gc>>,
        then: Option<BoxSequence<'gc>>,
    },
    Call {
        function: Function<'gc>,
        then: Option<BoxSequence<'gc>>,
    },
    Resume {
        thread: Thread<'gc>,
        then: Option<BoxSequence<'gc>>,
    },
}

pub trait CallbackFn<'gc>: Collect {
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
pub struct Callback<'gc>(Gc<'gc, Header<'gc>>);

struct Header<'gc> {
    call: unsafe fn(
        *const (),
        Context<'gc>,
        Execution<'gc, '_>,
        Stack<'gc, '_>,
    ) -> Result<CallbackReturn<'gc>, Error<'gc>>,
}

impl<'gc> Callback<'gc> {
    pub fn new<C: CallbackFn<'gc> + 'gc>(mc: &Mutation<'gc>, callback: C) -> Self {
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

impl<'gc> fmt::Debug for Callback<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Callback").field(&self.as_ptr()).finish()
    }
}

impl<'gc> PartialEq for Callback<'gc> {
    fn eq(&self, other: &Callback<'gc>) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<'gc> Eq for Callback<'gc> {}

impl<'gc> Hash for Callback<'gc> {
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
pub struct BoxSequence<'gc>(pub boxed::Box<dyn Sequence<'gc> + 'gc, MetricsAlloc<'gc>>);

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
