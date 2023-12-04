use std::hash::{Hash, Hasher};

use allocator_api2::vec;
use gc_arena::{allocator_api::MetricsAlloc, lock::RefLock, Collect, Gc, Mutation};
use thiserror::Error;

use crate::{
    compiler::{FunctionRef, LineNumber},
    BadThreadMode, CallbackReturn, Context, Error, FromMultiValue, Fuel, Function, FunctionProto,
    IntoMultiValue, SequencePoll, Stack, String, Thread, ThreadMode, Value, Variadic,
};

use super::{
    thread::{Frame, LuaFrame, ThreadState},
    vm::run_vm,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutorMode {
    /// There are no threads being run and the `Executor` must be restarted to do any work.
    Stopped,
    /// Lua has errored or returned (or yielded) values that must be taken to move the `Executor` to
    /// the `Stopped` (or `Suspended`) state.
    Result,
    /// There is an active thread in the `ThreadMode::Normal` state and it is can be run with
    /// `Executor::step`.
    Normal,
    /// The main thread has yielded and is waiting on being resumed.
    Suspended,
    /// The `Executor` is currently inside its own `Executor::step` function.
    Running,
}

#[derive(Debug, Copy, Clone, Error)]
#[error("bad executor mode: {found:?}, expected {expected:?}")]
pub struct BadExecutorMode {
    pub found: ExecutorMode,
    pub expected: ExecutorMode,
}

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
/// `Executor`s are not meant to be used from callbacks at all, and `Executor`s should not be
/// nested. Instead, use the normal mechanisms for callbacks to call Lua code so that it is run on
/// the same executor calling the callback.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Executor<'gc>(Gc<'gc, RefLock<vec::Vec<Thread<'gc>, MetricsAlloc<'gc>>>>);

impl<'gc> PartialEq for Executor<'gc> {
    fn eq(&self, other: &Executor<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Executor<'gc> {}

impl<'gc> Hash for Executor<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

impl<'gc> Executor<'gc> {
    const FUEL_PER_CALLBACK: i32 = 8;
    const FUEL_PER_SEQ_STEP: i32 = 4;
    const FUEL_PER_STEP: i32 = 4;

    /// Creates a new `Executor` with a stopped main thread.
    pub fn new(ctx: Context<'gc>) -> Self {
        Self::run(&ctx, Thread::new(ctx))
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
        let thread = Thread::new(ctx);
        thread.start(ctx, function, args).unwrap();
        Self::run(&ctx, thread)
    }

    pub fn as_ptr(self) -> *const () {
        Gc::as_ptr(self.0) as *const ()
    }

    pub fn mode(self) -> ExecutorMode {
        if let Ok(thread_stack) = self.0.try_borrow() {
            if thread_stack.len() > 1 {
                ExecutorMode::Normal
            } else {
                match thread_stack[0].mode() {
                    ThreadMode::Stopped => ExecutorMode::Stopped,
                    ThreadMode::Result => ExecutorMode::Result,
                    ThreadMode::Normal => ExecutorMode::Normal,
                    ThreadMode::Suspended => ExecutorMode::Suspended,
                    ThreadMode::Waiting => unreachable!(),
                    ThreadMode::Running => ExecutorMode::Running,
                }
            }
        } else {
            ExecutorMode::Running
        }
    }

    /// Runs the VM for a period of time controlled by the `fuel` parameter.
    ///
    /// The VM and callbacks will consume fuel as they run, and `Executor::step` will return as soon
    /// as `Fuel::can_continue()` returns false *and some minimal positive progress has been made*.
    ///
    /// Returns `false` if the method has exhausted its fuel, but there is more work to
    /// do, and returns `true` if no more progress can be made. If `true` is returned, then
    /// `Executor::mode()` will no longer be `ExecutorMode::Normal`.
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
            let top_state = &mut *top_state;
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
                    top_state: &mut ThreadState<'gc>,
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

                fn execution<'gc, 'a>(
                    executor: Executor<'gc>,
                    fuel: &'a mut Fuel,
                    threads: &'a [Thread<'gc>],
                    top_frames: &'a [Frame<'gc>],
                    top_stack: &[Value<'gc>],
                ) -> Execution<'gc, 'a> {
                    let upper_lua = match top_frames.last() {
                        Some(Frame::Lua { bottom, pc, .. }) => {
                            let Value::Function(Function::Closure(closure)) = top_stack[*bottom]
                            else {
                                panic!("lua frame bottom is not a closure");
                            };
                            // Subtract 1 instruction for the Call opcode.
                            Some((closure.0.proto, *pc - 1))
                        }
                        _ => None,
                    };

                    Execution {
                        executor,
                        fuel,
                        upper_lua,
                        threads,
                    }
                }

                match top_state.frames.pop() {
                    Some(Frame::Callback { bottom, callback }) => {
                        fuel.consume(Self::FUEL_PER_CALLBACK);
                        let exec = execution(
                            self,
                            fuel,
                            &thread_stack,
                            &top_state.frames,
                            &top_state.stack,
                        );
                        match callback.call(ctx, exec, Stack::new(&mut top_state.stack, bottom)) {
                            Ok(ret) => {
                                callback_ret(ctx, &mut *thread_stack, top_state, bottom, ret)
                            }
                            Err(err) => {
                                top_state.stack.truncate(bottom);
                                top_state.frames.push(Frame::Error(err))
                            }
                        }
                    }
                    Some(Frame::Sequence {
                        bottom,
                        mut sequence,
                        pending_error,
                    }) => {
                        fuel.consume(Self::FUEL_PER_SEQ_STEP);

                        let exec = execution(
                            self,
                            fuel,
                            &thread_stack,
                            &top_state.frames,
                            &top_state.stack,
                        );
                        let fin = if let Some(err) = pending_error {
                            sequence.error(ctx, exec, err, Stack::new(&mut top_state.stack, bottom))
                        } else {
                            sequence.poll(ctx, exec, Stack::new(&mut top_state.stack, bottom))
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
                                top_state.stack.truncate(bottom);
                                top_state.frames.push(Frame::Error(error));
                            }
                        }
                    }
                    Some(frame @ Frame::Lua { .. }) => {
                        top_state.frames.push(frame);

                        const VM_GRANULARITY: u32 = 64;

                        let lua_frame = LuaFrame {
                            state: top_state,
                            thread: top_thread,
                            fuel,
                        };
                        match run_vm(ctx, lua_frame, VM_GRANULARITY) {
                            Err(err) => {
                                top_state.frames.push(Frame::Error(err.into()));
                            }
                            Ok(instructions_run) => {
                                fuel.consume(instructions_run.try_into().unwrap());
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

            fuel.consume(Self::FUEL_PER_STEP);

            if !fuel.should_continue() {
                break false;
            }
        }
    }

    pub fn take_result<T: FromMultiValue<'gc>>(
        self,
        ctx: Context<'gc>,
    ) -> Result<Result<T, Error<'gc>>, BadExecutorMode> {
        let mode = self.mode();
        if mode == ExecutorMode::Result {
            let thread_stack = self.0.borrow();
            Ok(thread_stack[0].take_result(ctx).unwrap())
        } else {
            Err(BadExecutorMode {
                found: mode,
                expected: ExecutorMode::Result,
            })
        }
    }

    pub fn resume(
        self,
        ctx: Context<'gc>,
        args: impl IntoMultiValue<'gc>,
    ) -> Result<(), BadExecutorMode> {
        let mode = self.mode();
        if mode == ExecutorMode::Suspended {
            let thread_stack = self.0.borrow();
            thread_stack[0].resume(ctx, args).unwrap();
            Ok(())
        } else {
            Err(BadExecutorMode {
                found: mode,
                expected: ExecutorMode::Suspended,
            })
        }
    }

    pub fn resume_err(self, mc: &Mutation<'gc>, error: Error<'gc>) -> Result<(), BadExecutorMode> {
        let mode = self.mode();
        if mode == ExecutorMode::Suspended {
            let thread_stack = self.0.borrow();
            thread_stack[0].resume_err(mc, error).unwrap();
            Ok(())
        } else {
            Err(BadExecutorMode {
                found: mode,
                expected: ExecutorMode::Suspended,
            })
        }
    }

    /// Reset this `Executor` entirely, leaving it with a stopped main thread. Equivalent to
    /// creating a new executor with `Executor::new`.
    pub fn stop(self, mc: &Mutation<'gc>) {
        let mut thread_stack = self.0.borrow_mut(mc);
        thread_stack.truncate(1);
        thread_stack[0].reset(mc).unwrap();
    }

    /// Reset this `Executor` entirely and begins running the given thread. Equivalent to
    /// creating a new executor with `Executor::run`.
    pub fn reset(self, mc: &Mutation<'gc>, thread: Thread<'gc>) {
        let mut thread_stack = self.0.borrow_mut(mc);
        thread_stack.clear();
        thread_stack.push(thread);
    }

    /// Reset this `Executor` entirely and begins running the given function, equivalent to
    /// creating a new executor with `Executor::start`.
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

/// Execution state passed to callbacks when they are run by an `Executor`.
pub struct Execution<'gc, 'a> {
    executor: Executor<'gc>,
    fuel: &'a mut Fuel,
    upper_lua: Option<(Gc<'gc, FunctionProto<'gc>>, usize)>,
    threads: &'a [Thread<'gc>],
}

impl<'gc, 'a> Execution<'gc, 'a> {
    /// The fuel parameter passed to `Executor::step`.
    pub fn fuel(&mut self) -> &mut Fuel {
        self.fuel
    }

    /// The curently executing Thread.
    pub fn current_thread(&self) -> CurrentThread<'gc> {
        CurrentThread {
            thread: *self.threads.last().unwrap(),
            is_main: self.threads.len() == 1,
        }
    }

    /// The curently running Executor.
    ///
    /// Do not call methods on this from callbacks! This is provided only for identification
    /// purposes, so that callbacks can identify which executor that is currently executing them, or
    /// to store the pointer somewhere.
    pub fn executor(&self) -> Executor<'gc> {
        self.executor
    }

    /// If the function we are returning to is Lua, returns information about the Lua frame we are
    /// returning to.
    pub fn upper_lua_frame(&self) -> Option<UpperLuaFrame<'gc>> {
        self.upper_lua.map(|(proto, pc)| UpperLuaFrame {
            chunk_name: proto.chunk_name,
            current_function: proto.reference,
            current_line: match proto
                .opcode_line_numbers
                .binary_search_by_key(&pc, |(opi, _)| *opi)
            {
                Ok(i) => proto.opcode_line_numbers[i].1,
                Err(i) => proto.opcode_line_numbers[i - 1].1,
            },
        })
    }
}

pub struct CurrentThread<'gc> {
    pub thread: Thread<'gc>,
    pub is_main: bool,
}

pub struct UpperLuaFrame<'gc> {
    pub chunk_name: String<'gc>,
    pub current_function: FunctionRef<String<'gc>>,
    pub current_line: LineNumber,
}
