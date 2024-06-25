use std::{
    cell::Cell,
    future::{poll_fn, Future},
    marker::PhantomData,
    mem,
    pin::Pin,
    ptr,
    task::{self, Poll, RawWaker, RawWakerVTable, Waker},
};

use gc_arena::{Collect, DynamicRootSet, Gc, Mutation, StaticCollect};

use crate::{
    stash::{Fetchable, Stashable},
    BoxSequence, Callback, CallbackReturn, Context, Error, Execution, Sequence, SequencePoll,
    Stack, StashedCallback, StashedClosure, StashedError, StashedFunction, StashedString,
    StashedTable, StashedThread, StashedUserData, StashedValue,
};

pub type SeqFuture<'seq> = Box<dyn Future<Output = Result<(), LocalError<'seq>>> + 'seq>;

#[derive(Collect)]
#[collect(no_drop)]
pub struct AsyncSequence<'gc> {
    fut: SeqFut<'gc>,
    locals: DynamicRootSet<'gc>,
    _invariant: Invariant<'gc>,
}

impl<'gc> AsyncSequence<'gc> {
    pub fn new_seq<F>(mc: &Mutation<'gc>, create: F) -> BoxSequence<'gc>
    where
        F: for<'seq> FnOnce(SeqState<'seq>) -> SeqFuture<'seq> + 'static,
    {
        Self::new_seq_with(mc, (), move |_, seq| create(seq))
    }

    pub fn new_seq_with<R, F>(mc: &Mutation<'gc>, root: R, create: F) -> BoxSequence<'gc>
    where
        R: Collect + 'gc,
        F: for<'seq> FnOnce(R, SeqState<'seq>) -> SeqFuture<'seq> + 'static,
    {
        BoxSequence::new(
            mc,
            Self {
                fut: SeqFut::new(root, create),
                locals: DynamicRootSet::new(mc),
                _invariant: PhantomData,
            },
        )
    }

    pub fn new_callback<F>(mc: &Mutation<'gc>, create: F) -> Callback<'gc>
    where
        F: for<'seq> Fn(SeqState<'seq>) -> SeqFuture<'seq> + 'static,
    {
        Self::new_callback_with(mc, (), move |_, seq| create(seq))
    }

    pub fn new_callback_with<R, F>(mc: &Mutation<'gc>, root: R, create: F) -> Callback<'gc>
    where
        R: Collect + 'gc,
        F: for<'seq> Fn(&R, SeqState<'seq>) -> SeqFuture<'seq> + 'static,
    {
        let state = Gc::new(mc, (root, StaticCollect(create)));
        Callback::from_fn_with(mc, state, |state, ctx, _, _| {
            Ok(CallbackReturn::Sequence(Self::new_seq_with(
                &ctx,
                *state,
                |state, seq| {
                    let (root, create) = state.as_ref();
                    (create.0)(&root, seq)
                },
            )))
        })
    }

    fn poll_fut(
        &mut self,
        ctx: Context<'gc>,
        exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
        error: Option<Error<'gc>>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        let mut next = SequencePoll::Pending;

        let mut shared = Shared {
            locals: self.locals,
            ctx,
            exec,
            stack: stack.reborrow(),
            error,
            next: &mut next,
        };
        match self.fut.poll(&mut shared) {
            // If our future is finished, turn any pending operation into a tail op.
            Poll::Ready(res) => {
                res?;
                Ok(match next {
                    SequencePoll::Pending => {
                        stack.clear();
                        SequencePoll::Return
                    }
                    SequencePoll::Return => SequencePoll::Return,
                    SequencePoll::Call { function, bottom } => {
                        stack.drain(0..bottom);
                        SequencePoll::TailCall { function }
                    }
                    SequencePoll::TailCall { function } => SequencePoll::TailCall { function },
                    SequencePoll::Yield { to_thread, bottom } => {
                        stack.drain(0..bottom);
                        SequencePoll::TailYield { to_thread }
                    }
                    SequencePoll::TailYield { to_thread } => SequencePoll::TailYield { to_thread },
                    SequencePoll::Resume { thread, bottom } => {
                        stack.drain(0..bottom);
                        SequencePoll::TailResume { thread }
                    }
                    SequencePoll::TailResume { thread } => SequencePoll::TailResume { thread },
                })
            }
            Poll::Pending => Ok(next),
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

pub struct SeqState<'seq> {
    _invariant: Invariant<'seq>,
}

impl<'seq> SeqState<'seq> {
    pub fn enter<F, R>(&mut self, f: F) -> R
    where
        F: for<'gc> FnOnce(Context<'gc>, SeqContext<'seq, 'gc, '_>) -> R,
        R: 'seq,
    {
        visit_shared(move |shared| {
            f(
                shared.ctx,
                SeqContext {
                    shared,
                    _invariant: PhantomData,
                },
            )
        })
    }

    pub fn try_enter<F, R>(&mut self, f: F) -> Result<R, LocalError<'seq>>
    where
        F: for<'gc> FnOnce(Context<'gc>, SeqContext<'seq, 'gc, '_>) -> Result<R, Error<'gc>>,
        R: 'seq,
    {
        visit_shared(move |shared| {
            let locals = shared.locals;
            let ctx = shared.ctx;
            f(
                ctx,
                SeqContext {
                    shared,
                    _invariant: PhantomData,
                },
            )
            .map_err(|e| Local::stash(&ctx, locals, e))
        })
    }

    pub async fn pending(&mut self) {
        wait_once().await;
        visit_shared(move |shared| {
            assert!(
                shared.error.is_none(),
                "SequencePoll::Pending cannot be followed by an error"
            );
        });
    }

    pub fn return_(self) {
        visit_shared(move |shared| {
            *shared.next = SequencePoll::Return;
        });
    }

    pub async fn call(
        &mut self,
        func: &LocalFunction<'seq>,
        bottom: usize,
    ) -> Result<(), LocalError<'seq>> {
        visit_shared(move |shared| {
            *shared.next = SequencePoll::Call {
                function: func.fetch(shared.locals),
                bottom,
            };
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

    pub fn tail_call(self, func: &LocalFunction<'seq>) {
        visit_shared(move |shared| {
            *shared.next = SequencePoll::TailCall {
                function: func.fetch(shared.locals),
            };
        });
    }

    pub async fn yield_(
        &mut self,
        to_thread: Option<&LocalThread<'seq>>,
        bottom: usize,
    ) -> Result<(), LocalError<'seq>> {
        visit_shared(move |shared| {
            *shared.next = SequencePoll::Yield {
                to_thread: to_thread.map(|t| t.fetch(shared.locals)),
                bottom,
            };
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

    pub fn tail_yield(self, to_thread: Option<&LocalThread<'seq>>) {
        visit_shared(move |shared| {
            *shared.next = SequencePoll::TailYield {
                to_thread: to_thread.map(|t| t.fetch(shared.locals)),
            };
        });
    }

    pub async fn resume(
        &mut self,
        thread: &LocalThread<'seq>,
        bottom: usize,
    ) -> Result<(), LocalError<'seq>> {
        visit_shared(move |shared| {
            *shared.next = SequencePoll::Resume {
                thread: thread.fetch(shared.locals),
                bottom,
            };
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

    pub fn tail_resume(self, thread: &LocalThread<'seq>) {
        visit_shared(move |shared| {
            *shared.next = SequencePoll::TailResume {
                thread: thread.fetch(shared.locals),
            };
        });
    }
}

pub struct SeqContext<'seq, 'gc, 'a> {
    shared: &'a mut Shared<'gc, 'a>,
    _invariant: Invariant<'seq>,
}

impl<'seq, 'gc, 'a> SeqContext<'seq, 'gc, 'a> {
    pub fn stash<S: Stashable<'gc>>(&self, s: S) -> Local<'seq, S::Stashed> {
        Local::stash(&self.shared.ctx, self.shared.locals, s)
    }

    pub fn fetch<F: Fetchable<'gc>>(&self, local: &Local<'seq, F>) -> F::Fetched {
        local.fetch(self.shared.locals)
    }

    pub fn exec(&mut self) -> Execution<'gc, '_> {
        self.shared.exec.reborrow()
    }

    pub fn stack(&mut self) -> Stack<'gc, '_> {
        self.shared.stack.reborrow()
    }
}

#[derive(Collect)]
#[collect(no_drop)]
enum SeqFut<'gc> {
    Create {
        root: Box<dyn Collect + 'gc>,
        #[collect(require_static)]
        create: Box<dyn for<'seq> FnOnce(*mut (), SeqState<'seq>) -> SeqFuture<'seq>>,
    },
    Run(#[collect(require_static)] Pin<Box<dyn Future<Output = Result<(), LocalError<'static>>>>>),
    Empty,
}

impl<'gc> SeqFut<'gc> {
    fn new<R, F>(root: R, create: F) -> Self
    where
        R: Collect + 'gc,
        F: for<'seq> FnOnce(R, SeqState<'seq>) -> SeqFuture<'seq> + 'static,
    {
        Self::Create {
            root: Box::new(root),
            create: Box::new(move |cptr, seq| {
                let root = unsafe { Box::from_raw(cptr as *mut R) };
                create(*root, seq)
            }),
        }
    }
}

impl<'gc> SeqFut<'gc> {
    fn poll(&mut self, shared: &mut Shared<'gc, '_>) -> Poll<Result<(), Error<'gc>>> {
        let locals = shared.locals;
        with_shared(shared, || {
            match mem::replace(self, SeqFut::Empty) {
                SeqFut::Create { root, create } => {
                    *self = Self::Run(Box::into_pin(create(
                        Box::into_raw(root) as *mut (),
                        SeqState {
                            _invariant: PhantomData,
                        },
                    )));
                }
                other => *self = other,
            }

            let SeqFut::Run(f) = self else { unreachable!() };
            f.as_mut()
                .poll(&mut task::Context::from_waker(&noop_waker()))
                .map_err(|e| e.fetch(locals))
        })
    }
}

// Invariant type that is also !Send and !Sync
type Invariant<'a> = PhantomData<*const Cell<&'a ()>>;

struct Shared<'gc, 'a> {
    locals: DynamicRootSet<'gc>,
    ctx: Context<'gc>,
    exec: Execution<'gc, 'a>,
    stack: Stack<'gc, 'a>,
    error: Option<Error<'gc>>,
    next: &'a mut SequencePoll<'gc>,
}

thread_local! {
    static SHARED: Cell<*mut Shared<'static, 'static>> = const { Cell::new(ptr::null_mut()) };
}

fn with_shared<'gc, 'a, R>(shared: &mut Shared<'gc, 'a>, f: impl FnOnce() -> R) -> R {
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
