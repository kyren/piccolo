use std::cell::RefMut;

use allocator_api2::vec;
use gc_arena::{allocator_api::MetricsAlloc, lock::RefLock, Collect, Gc, Mutation};

use crate::{
    BadThreadMode, CallbackReturn, Context, Error, FromMultiValue, Fuel, Function, IntoMultiValue,
    SequencePoll, Stack, Thread, ThreadMode, Variadic,
};

use super::{
    thread::{Frame, LuaFrame, ThreadState},
    vm::run_vm,
};

/// The entry-point for the Lua VM.
///
/// `Executor` runs networks of `Threads` that may depend on each other and may pass control
/// back and forth. All Lua code that is run is done so directly or indirectly by calling
/// `Executor::step`.
///
/// # Panics
///
/// `Executor` is dangerous to use from within any kind of Lua callback. It has no protection
/// against re-entrency, and calling `Executor` methods from within a callback that it is running
/// (other than `Executor::mode`) will panic. Additionally, even if an independent `Executor` is
/// used, cross-thread upvalues may cause a panic if one `Executor` is used within the other.
///
/// `Executor`s are not meant to be used from or available to callbacks at all, and `Executor`s
/// should not be nested. Instead, use the normal mechanisms for callbacks to call Lua code so that
/// it is run on the same executor calling the callback.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Executor<'gc>(Gc<'gc, RefLock<vec::Vec<Thread<'gc>, MetricsAlloc<'gc>>>>);

impl<'gc> PartialEq for Executor<'gc> {
    fn eq(&self, other: &Executor<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Executor<'gc> {}

impl<'gc> Executor<'gc> {
    const FUEL_PER_CALLBACK: i32 = 8;
    const FUEL_PER_SEQ_STEP: i32 = 4;
    const FUEL_PER_STEP: i32 = 4;

    /// Creates a new `Executor` with a stopped main thread.
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self::run(mc, Thread::new(mc))
    }

    /// Creates a new `Executor` that begins running the given thread.
    pub fn run(mc: &Mutation<'gc>, thread: Thread<'gc>) -> Self {
        let mut thread_stack = vec::Vec::new_in(MetricsAlloc::new(mc));
        thread_stack.push(thread);
        Executor(Gc::new(mc, RefLock::new(thread_stack)))
    }

    /// Creates a new `Executor` with a new `Thread` running the given function.
    pub fn start(
        ctx: Context<'gc>,
        function: Function<'gc>,
        args: impl IntoMultiValue<'gc>,
    ) -> Self {
        let thread = Thread::new(&ctx);
        thread.start(ctx, function, args).unwrap();
        Self::run(&ctx, thread)
    }

    pub fn mode(self) -> ThreadMode {
        if let Ok(thread_stack) = self.0.try_borrow() {
            if thread_stack.len() > 1 {
                ThreadMode::Normal
            } else {
                thread_stack[0].mode()
            }
        } else {
            ThreadMode::Running
        }
    }

    /// Runs the VM for a period of time controlled by the `fuel` parameter.
    ///
    /// The VM and callbacks will consume fuel as they run, and `Executor::step` will return as soon
    /// as `Fuel::can_continue()` returns false *and some minimal positive progress has been made*.
    ///
    /// Returns `false` if the method has exhausted its fuel, but there is more work to do. If
    /// `true` is returned, there is only one thread remaining on the execution stack and it is no
    /// longer in the 'Normal' state.
    pub fn step(self, ctx: Context<'gc>, fuel: &mut Fuel) -> bool {
        let mut thread_stack = self.0.borrow_mut(&ctx);

        loop {
            let mut top_thread = thread_stack.last().copied().unwrap();
            let mut res_thread = None;
            match top_thread.mode() {
                ThreadMode::Normal => {}
                ThreadMode::Running => {
                    panic!("`Executor` thread already running")
                }
                _ => {
                    if thread_stack.len() == 1 {
                        break true;
                    } else {
                        thread_stack.pop();
                        res_thread = Some(top_thread);
                        top_thread = thread_stack.last().copied().unwrap();
                    }
                }
            }

            let mut top_state = top_thread.0.borrow_mut(&ctx);
            if let Some(res_thread) = res_thread {
                let mode = top_state.mode();
                if mode == ThreadMode::Waiting {
                    assert!(matches!(top_state.frames.pop(), Some(Frame::WaitThread)));
                    match res_thread.mode() {
                        ThreadMode::Result => {
                            // Take the results from the res_thread and return them to our top
                            // thread.
                            let mut res_state = res_thread.0.borrow_mut(&ctx);
                            match res_state.take_result() {
                                Ok(vals) => {
                                    let bottom = top_state.stack.len();
                                    top_state.stack.extend(vals);
                                    top_state.return_to(bottom);
                                }
                                Err(err) => {
                                    top_state.frames.push(Frame::Error(err.into()));
                                }
                            }
                            drop(res_state);
                        }
                        ThreadMode::Normal => unreachable!(),
                        res_mode => top_state.frames.push(Frame::Error(
                            BadThreadMode {
                                found: res_mode,
                                expected: None,
                            }
                            .into(),
                        )),
                    }
                } else {
                    // Shenanigans have happened and the upper thread has had its state externally
                    // changed.
                    top_state.frames.push(Frame::Error(
                        BadThreadMode {
                            found: mode,
                            expected: None,
                        }
                        .into(),
                    ));
                }
            }

            if top_state.mode() == ThreadMode::Normal {
                fn callback_ret<'gc>(
                    ctx: Context<'gc>,
                    thread_stack: &mut vec::Vec<Thread<'gc>, MetricsAlloc<'gc>>,
                    mut top_state: RefMut<ThreadState<'gc>>,
                    stack_bottom: usize,
                    ret: CallbackReturn<'gc>,
                ) {
                    match ret {
                        CallbackReturn::Return => {
                            top_state.return_to(stack_bottom);
                        }
                        CallbackReturn::Sequence(sequence) => {
                            top_state.frames.push(Frame::Sequence {
                                bottom: stack_bottom,
                                sequence,
                                pending_error: None,
                            });
                        }
                        CallbackReturn::Yield { to_thread, then } => {
                            if let Some(sequence) = then {
                                top_state.frames.push(Frame::Sequence {
                                    bottom: stack_bottom,
                                    sequence,
                                    pending_error: None,
                                });
                            }
                            top_state.frames.push(Frame::Yielded);

                            if let Some(to_thread) = to_thread {
                                if let Err(err) = to_thread
                                    .resume(ctx, Variadic(top_state.stack.drain(stack_bottom..)))
                                {
                                    top_state.frames.push(Frame::Error(err.into()));
                                } else {
                                    thread_stack.pop();
                                    thread_stack.push(to_thread);
                                }
                            } else {
                                top_state.frames.push(Frame::Result {
                                    bottom: stack_bottom,
                                });
                            }
                        }
                        CallbackReturn::Call { function, then } => {
                            if let Some(sequence) = then {
                                top_state.frames.push(Frame::Sequence {
                                    bottom: stack_bottom,
                                    sequence,
                                    pending_error: None,
                                });
                            }
                            top_state.push_call(stack_bottom, function);
                        }
                        CallbackReturn::Resume { thread, then } => {
                            if let Some(sequence) = then {
                                top_state.frames.push(Frame::Sequence {
                                    bottom: stack_bottom,
                                    sequence,
                                    pending_error: None,
                                });
                            }
                            top_state.frames.push(Frame::WaitThread);

                            if let Err(err) =
                                thread.resume(ctx, Variadic(top_state.stack.drain(stack_bottom..)))
                            {
                                top_state.frames.push(Frame::Error(err.into()));
                            } else {
                                if top_state.frames.len() == 1 {
                                    // Tail call the thread resume if we can.
                                    assert!(matches!(top_state.frames[0], Frame::WaitThread));
                                    thread_stack.pop();
                                }
                                thread_stack.push(thread);
                            }
                        }
                    }
                }

                match top_state.frames.pop() {
                    Some(Frame::Callback { bottom, callback }) => {
                        fuel.consume_fuel(Self::FUEL_PER_CALLBACK);
                        match callback.call(ctx, fuel, Stack::new(&mut top_state.stack, bottom)) {
                            Ok(ret) => {
                                callback_ret(ctx, &mut *thread_stack, top_state, bottom, ret)
                            }
                            Err(err) => top_state.frames.push(Frame::Error(err)),
                        }
                    }
                    Some(Frame::Sequence {
                        bottom,
                        mut sequence,
                        pending_error,
                    }) => {
                        fuel.consume_fuel(Self::FUEL_PER_SEQ_STEP);
                        let fin = if let Some(err) = pending_error {
                            sequence.error(ctx, fuel, err, Stack::new(&mut top_state.stack, bottom))
                        } else {
                            sequence.poll(ctx, fuel, Stack::new(&mut top_state.stack, bottom))
                        };

                        match fin {
                            Ok(ret) => callback_ret(
                                ctx,
                                &mut *thread_stack,
                                top_state,
                                bottom,
                                match ret {
                                    SequencePoll::Pending => CallbackReturn::Sequence(sequence),
                                    SequencePoll::Return => CallbackReturn::Return,
                                    SequencePoll::Yield { to_thread, is_tail } => {
                                        CallbackReturn::Yield {
                                            to_thread,
                                            then: if is_tail { None } else { Some(sequence) },
                                        }
                                    }
                                    SequencePoll::Call { function, is_tail } => {
                                        CallbackReturn::Call {
                                            function,
                                            then: if is_tail { None } else { Some(sequence) },
                                        }
                                    }
                                    SequencePoll::Resume { thread, is_tail } => {
                                        CallbackReturn::Resume {
                                            thread,
                                            then: if is_tail { None } else { Some(sequence) },
                                        }
                                    }
                                },
                            ),
                            Err(error) => {
                                top_state.frames.push(Frame::Error(error));
                            }
                        }
                    }
                    Some(frame @ Frame::Lua { .. }) => {
                        top_state.frames.push(frame);

                        const VM_GRANULARITY: u32 = 64;

                        let lua_frame = LuaFrame {
                            state: &mut top_state,
                            thread: top_thread,
                            fuel,
                        };
                        match run_vm(ctx, lua_frame, VM_GRANULARITY) {
                            Err(err) => {
                                top_state.frames.push(Frame::Error(err.into()));
                            }
                            Ok(instructions_run) => {
                                fuel.consume_fuel(instructions_run.try_into().unwrap());
                            }
                        }
                    }
                    Some(Frame::Error(err)) => {
                        match top_state
                            .frames
                            .pop()
                            .expect("normal thread must have frame above error")
                        {
                            Frame::Lua { bottom, .. } => {
                                top_state.close_upvalues(&ctx, bottom);
                                top_state.stack.truncate(bottom);
                                top_state.frames.push(Frame::Error(err));
                            }
                            Frame::Sequence {
                                bottom,
                                sequence,
                                pending_error: error,
                            } => {
                                assert!(error.is_none());
                                top_state.frames.push(Frame::Sequence {
                                    bottom,
                                    sequence,
                                    pending_error: Some(err),
                                });
                            }
                            _ => top_state.frames.push(Frame::Error(err)),
                        }
                    }
                    _ => panic!("tried to step invalid frame type"),
                }
            }

            fuel.consume_fuel(Self::FUEL_PER_STEP);

            if !fuel.should_continue() {
                break false;
            }
        }
    }

    pub fn take_result<T: FromMultiValue<'gc>>(
        self,
        ctx: Context<'gc>,
    ) -> Result<Result<T, Error<'gc>>, BadThreadMode> {
        let thread_stack = self.0.borrow();
        if thread_stack.len() > 1 {
            Err(BadThreadMode {
                found: ThreadMode::Normal,
                expected: Some(ThreadMode::Result),
            })
        } else {
            thread_stack[0].take_result(ctx)
        }
    }

    pub fn resume<T: FromMultiValue<'gc>>(
        self,
        ctx: Context<'gc>,
        args: impl IntoMultiValue<'gc>,
    ) -> Result<(), BadThreadMode> {
        let thread_stack = self.0.borrow();
        if thread_stack.len() > 1 {
            Err(BadThreadMode {
                found: ThreadMode::Normal,
                expected: Some(ThreadMode::Suspended),
            })
        } else {
            thread_stack[0].resume(ctx, args)
        }
    }

    pub fn resume_err(self, mc: &Mutation<'gc>, error: Error<'gc>) -> Result<(), BadThreadMode> {
        let thread_stack = self.0.borrow();
        if thread_stack.len() > 1 {
            Err(BadThreadMode {
                found: ThreadMode::Normal,
                expected: Some(ThreadMode::Suspended),
            })
        } else {
            thread_stack[0].resume_err(mc, error)
        }
    }

    /// Reset this `Executor` entirely and begins running the given thread.
    pub fn reset(self, mc: &Mutation<'gc>, thread: Thread<'gc>) {
        let mut thread_stack = self.0.borrow_mut(mc);
        thread_stack.clear();
        thread_stack.push(thread);
    }

    /// Reset this `Executor` entirely and begins running the given function.
    pub fn restart(
        self,
        ctx: Context<'gc>,
        function: Function<'gc>,
        args: impl IntoMultiValue<'gc>,
    ) {
        let mut thread_stack = self.0.borrow_mut(&ctx);
        thread_stack.truncate(1);
        thread_stack[0].reset(&ctx).unwrap();
        thread_stack[0].start(ctx, function, args).unwrap();
    }
}
