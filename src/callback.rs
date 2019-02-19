use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

use gc_arena::{Collect, Gc, MutationContext, StaticCollect};
use gc_sequence::{Sequence, SequenceExt};

use crate::{Error, Function, Value};

// Safe, does not implement drop
#[derive(Collect)]
#[collect(unsafe_drop)]
pub enum CallbackResult<'gc> {
    Return(Vec<Value<'gc>>),
    Yield(Vec<Value<'gc>>),
    TailCall {
        function: Function<'gc>,
        args: Vec<Value<'gc>>,
        continuation: Continuation<'gc>,
    },
}

pub enum CallbackReturn<'gc> {
    Immediate(Result<CallbackResult<'gc>, Error<'gc>>),
    Sequence(Box<dyn Sequence<'gc, Output = Result<CallbackResult<'gc>, Error<'gc>>> + 'gc>),
}

impl<'gc> CallbackReturn<'gc> {
    pub fn immediate<F>(f: F) -> CallbackReturn<'gc>
    where
        F: FnOnce() -> Result<CallbackResult<'gc>, Error<'gc>>,
    {
        CallbackReturn::Immediate(f())
    }

    pub fn sequence<F, S>(f: F) -> CallbackReturn<'gc>
    where
        S: 'gc + Sequence<'gc, Output = Result<CallbackResult<'gc>, Error<'gc>>>,
        F: FnOnce() -> Result<S, Error<'gc>>,
    {
        match f() {
            Ok(res) => CallbackReturn::Sequence(res.boxed()),
            Err(err) => CallbackReturn::Immediate(Err(err)),
        }
    }
}

pub trait ContinuationFn<'gc>: Collect {
    fn call(self: Box<Self>, res: Result<Vec<Value<'gc>>, Error<'gc>>) -> CallbackReturn<'gc>;
}

// Safe, does not implement drop
#[derive(Collect)]
#[collect(unsafe_drop)]
pub struct Continuation<'gc>(Box<dyn ContinuationFn<'gc> + 'gc>);

impl<'gc> Continuation<'gc> {
    pub fn new<F>(cont: F) -> Continuation<'gc>
    where
        F: 'static + FnOnce(Result<Vec<Value<'gc>>, Error<'gc>>) -> CallbackReturn<'gc>,
    {
        #[derive(Collect)]
        #[collect(require_static)]
        struct StaticContinuationFn<F>(F);

        impl<'gc, F> ContinuationFn<'gc> for StaticContinuationFn<F>
        where
            F: 'static + FnOnce(Result<Vec<Value<'gc>>, Error<'gc>>) -> CallbackReturn<'gc>,
        {
            fn call(
                self: Box<Self>,
                res: Result<Vec<Value<'gc>>, Error<'gc>>,
            ) -> CallbackReturn<'gc> {
                self.0(res)
            }
        }

        Continuation(Box::new(StaticContinuationFn(cont)))
    }

    pub fn new_with<C, F>(context: C, continuation: F) -> Continuation<'gc>
    where
        C: 'gc + Collect,
        F: 'static + FnOnce(C, Result<Vec<Value<'gc>>, Error<'gc>>) -> CallbackReturn<'gc>,
    {
        // Safe, does not implement drop
        #[derive(Collect)]
        #[collect(unsafe_drop)]
        struct ContextContinuationFn<C, F>(C, StaticCollect<F>);

        impl<'gc, C, F> ContinuationFn<'gc> for ContextContinuationFn<C, F>
        where
            C: 'gc + Collect,
            F: 'static + FnOnce(C, Result<Vec<Value<'gc>>, Error<'gc>>) -> CallbackReturn<'gc>,
        {
            fn call(
                self: Box<Self>,
                res: Result<Vec<Value<'gc>>, Error<'gc>>,
            ) -> CallbackReturn<'gc> {
                (self.1).0(self.0, res)
            }
        }

        Continuation(Box::new(ContextContinuationFn(
            context,
            StaticCollect(continuation),
        )))
    }

    pub fn call(self, res: Result<Vec<Value<'gc>>, Error<'gc>>) -> CallbackReturn<'gc> {
        self.0.call(res)
    }
}

pub trait CallbackFn<'gc>: Collect {
    fn call(&self, res: Vec<Value<'gc>>) -> CallbackReturn<'gc>;
}

#[derive(Clone, Copy, Collect)]
#[collect(require_copy)]
pub struct Callback<'gc>(pub Gc<'gc, Box<dyn CallbackFn<'gc> + 'gc>>);

impl<'gc> Callback<'gc> {
    pub fn new<F>(mc: MutationContext<'gc, '_>, f: F) -> Callback<'gc>
    where
        F: 'static + Fn(Vec<Value<'gc>>) -> CallbackReturn<'gc>,
    {
        #[derive(Collect)]
        #[collect(require_static)]
        struct StaticCallbackFn<F>(F);

        impl<'gc, F> CallbackFn<'gc> for StaticCallbackFn<F>
        where
            F: 'static + Fn(Vec<Value<'gc>>) -> CallbackReturn<'gc>,
        {
            fn call(&self, res: Vec<Value<'gc>>) -> CallbackReturn<'gc> {
                self.0(res)
            }
        }

        Callback(Gc::allocate(mc, Box::new(StaticCallbackFn(f))))
    }

    pub fn new_with<C, F>(mc: MutationContext<'gc, '_>, c: C, f: F) -> Callback<'gc>
    where
        C: 'gc + Collect,
        F: 'static + Fn(&C, Vec<Value<'gc>>) -> CallbackReturn<'gc>,
    {
        #[derive(Collect)]
        #[collect(empty_drop)]
        struct ContextCallbackFn<C, F>(C, StaticCollect<F>);

        impl<'gc, C, F> CallbackFn<'gc> for ContextCallbackFn<C, F>
        where
            C: 'gc + Collect,
            F: 'static + Fn(&C, Vec<Value<'gc>>) -> CallbackReturn<'gc>,
        {
            fn call(&self, args: Vec<Value<'gc>>) -> CallbackReturn<'gc> {
                (self.1).0(&self.0, args)
            }
        }

        Callback(Gc::allocate(
            mc,
            Box::new(ContextCallbackFn(c, StaticCollect(f))),
        ))
    }

    pub fn call(&self, args: Vec<Value<'gc>>) -> CallbackReturn<'gc> {
        self.0.call(args)
    }
}

impl<'gc> Debug for Callback<'gc> {
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
