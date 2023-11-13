use std::{
    fmt::{self, Debug},
    hash::{Hash, Hasher},
    mem,
};

use allocator_api2::vec;
use gc_arena::{
    allocator_api::MetricsAlloc,
    lock::{Lock, RefLock},
    Collect, Gc, Mutation,
};
use thiserror::Error;

use crate::{
    closure::{UpValue, UpValueState},
    meta_ops,
    types::{RegisterIndex, VarCount},
    AnyCallback, AnySequence, CallbackReturn, Closure, Context, Error, FromMultiValue, Fuel,
    Function, IntoMultiValue, SequencePoll, Stack, TypeError, VMError, Value, Variadic,
};

use super::vm::run_vm;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadMode {
    /// No frames are on the thread and there are no available results, the thread can be started.
    Stopped,
    /// The thread has an error or has returned (or yielded) values that must be taken to move the
    /// thread back to the `Stopped` (or `Suspended`) state.
    Result,
    /// Thread has an active Lua, Callback, or Sequence frame.
    Normal,
    /// Thread has yielded and is waiting on being resumed.
    Suspended,
    /// The thread is waiting on another thread to finish.
    Waiting,
    /// Thread is currently inside its own `Thread::step` function.
    Running,
}

#[derive(Debug, Copy, Clone, Error)]
#[error("bad thread mode: {found:?}{}", if let Some(expected) = *.expected {
        format!(", expected {:?}", expected)
    } else {
        format!("")
    })]
pub struct BadThreadMode {
    pub found: ThreadMode,
    pub expected: Option<ThreadMode>,
}

#[derive(Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct Thread<'gc>(Gc<'gc, RefLock<ThreadState<'gc>>>);

impl<'gc> Debug for Thread<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Thread")
            .field(&(&self.0 as *const _))
            .finish()
    }
}

impl<'gc> PartialEq for Thread<'gc> {
    fn eq(&self, other: &Thread<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Thread<'gc> {}

impl<'gc> Hash for Thread<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

impl<'gc> Thread<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Thread<'gc> {
        Thread(Gc::new(
            mc,
            RefLock::new(ThreadState {
                frames: vec::Vec::new_in(MetricsAlloc::new(mc)),
                lua_stack: Stack::new(mc),
                open_upvalues: vec::Vec::new_in(MetricsAlloc::new(mc)),
                external_stack: Stack::new(mc),
                error: None,
            }),
        ))
    }

    pub fn as_ptr(self) -> *const () {
        Gc::as_ptr(self.0) as *const ()
    }

    pub fn mode(self) -> ThreadMode {
        self.0.borrow().mode()
    }

    /// If this thread is `Stopped`, start a new function with the given arguments.
    pub fn start(
        self,
        ctx: Context<'gc>,
        function: Function<'gc>,
        args: impl IntoMultiValue<'gc>,
    ) -> Result<(), BadThreadMode> {
        let mut state = self.0.borrow_mut(&ctx);
        state.check_mode(ThreadMode::Stopped)?;

        assert!(state.external_stack.is_empty());
        state.external_stack.replace(ctx, args);

        state.ext_call_function(function);

        Ok(())
    }

    /// If this thread is `Stopped`, start a new suspended function.
    pub fn start_suspended(
        self,
        mc: &Mutation<'gc>,
        function: Function<'gc>,
    ) -> Result<(), BadThreadMode> {
        let mut state = self.0.borrow_mut(mc);
        state.check_mode(ThreadMode::Stopped)?;
        state.frames.push(Frame::Start(function));
        Ok(())
    }

    /// If the thread is in the `Result` mode, take the returned (or yielded) values. Moves the
    /// thread back to the `Stopped` (or `Suspended`) mode.
    pub fn take_return<T: FromMultiValue<'gc>>(
        self,
        ctx: Context<'gc>,
    ) -> Result<Result<T, Error<'gc>>, BadThreadMode> {
        let mut state = self.0.borrow_mut(&ctx);
        state.check_mode(ThreadMode::Result)?;
        Ok(state
            .take_return()
            .and_then(|vals| Ok(T::from_multi_value(ctx, vals)?)))
    }

    /// If the thread is in `Suspended` mode, resume it.
    pub fn resume(
        self,
        ctx: Context<'gc>,
        args: impl IntoMultiValue<'gc>,
    ) -> Result<(), BadThreadMode> {
        let mut state = self.0.borrow_mut(&ctx);
        state.check_mode(ThreadMode::Suspended)?;

        assert!(state.external_stack.is_empty());
        state.external_stack.replace(ctx, args);

        match state.frames.pop().expect("no frame to resume") {
            Frame::Start(function) => {
                assert!(
                    state.lua_stack.is_empty()
                        && state.open_upvalues.is_empty()
                        && state.frames.is_empty()
                        && state.error.is_none()
                );
                state.ext_call_function(function);
            }
            Frame::Yielded => match state.frames.last_mut() {
                Some(Frame::Sequence { .. }) => {}
                Some(Frame::Lua { .. }) => {
                    state.return_to_lua();
                }
                None => {
                    state.frames.push(Frame::HasResult);
                }
                _ => panic!("yielded frame must be above a sequence or lua frame"),
            },
            _ => panic!("top frame not a suspended thread"),
        }
        Ok(())
    }

    /// If the thread is in `Suspended` mode, cause an error wherever the thread was suspended.
    pub fn resume_err(self, mc: &Mutation<'gc>, error: Error<'gc>) -> Result<(), BadThreadMode> {
        let mut state = self.0.borrow_mut(mc);
        state.check_mode(ThreadMode::Suspended)?;

        assert!(state.external_stack.is_empty());
        state.unwind(mc, error);
        Ok(())
    }

    /// If this thread is in any other mode than `Running`, reset the thread completely and restore
    /// it to the `Stopped` state.
    pub fn reset(self, mc: &Mutation<'gc>) -> Result<(), BadThreadMode> {
        let mut state = self.0.borrow_mut(mc);
        if state.mode() == ThreadMode::Running {
            Err(BadThreadMode {
                found: ThreadMode::Running,
                expected: None,
            })
        } else {
            state.close_upvalues(mc, 0);
            assert!(state.open_upvalues.is_empty());

            state.lua_stack.clear();
            state.frames.clear();
            state.external_stack.clear();
            state.error = None;
            Ok(())
        }
    }
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
/// (other than `Executor::mode`) will panic.
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
                            assert!(top_state.external_stack.is_empty());
                            assert!(top_state.error.is_none());

                            // Take the results from the res_thread and return them to our top
                            // thread.
                            let mut res_state = res_thread.0.borrow_mut(&ctx);
                            match res_state.take_return() {
                                Ok(vals) => {
                                    top_state.external_stack.extend(vals);
                                    top_state.return_ext(fuel, ExternalReturn::Return);
                                }
                                Err(err) => {
                                    top_state.unwind(&ctx, err);
                                }
                            }
                            drop(res_state);
                        }
                        ThreadMode::Normal => unreachable!(),
                        mode => top_state.unwind(
                            &ctx,
                            BadThreadMode {
                                found: mode,
                                expected: None,
                            }
                            .into(),
                        ),
                    }
                } else {
                    // Shenanigans have happened and the upper thread has had its state changed.
                    top_state.unwind(
                        &ctx,
                        BadThreadMode {
                            found: mode,
                            expected: None,
                        }
                        .into(),
                    );
                }
            }

            if top_state.mode() == ThreadMode::Normal {
                match top_state.frames.pop().expect("no frame to step") {
                    Frame::Callback(callback) => {
                        fuel.consume_fuel(FUEL_PER_CALLBACK);
                        top_state.frames.push(Frame::Calling);

                        assert!(top_state.error.is_none());
                        let mut stack =
                            mem::replace(&mut top_state.external_stack, Stack::new(&ctx));
                        drop(top_state);
                        let seq = callback.call(ctx, fuel, &mut stack);
                        top_state = top_thread.0.borrow_mut(&ctx);
                        top_state.external_stack = stack;

                        assert!(
                            matches!(top_state.frames.pop(), Some(Frame::Calling)),
                            "thread state has changed while callback was run"
                        );

                        match seq {
                            Ok(ret) => match ret {
                                CallbackReturn::Return => {
                                    top_state.return_ext(fuel, ExternalReturn::Return);
                                }
                                CallbackReturn::Sequence(s) => {
                                    top_state.return_ext(fuel, ExternalReturn::Sequence(s));
                                }
                                CallbackReturn::Yield { to_thread, then } => {
                                    top_state.return_ext(fuel, ExternalReturn::Yield(then));
                                    if let Some(to_thread) = to_thread {
                                        if let Err(err) = to_thread
                                            .resume(ctx, Variadic(top_state.take_return().unwrap()))
                                        {
                                            top_state.unwind(&ctx, err.into());
                                        } else {
                                            thread_stack.pop();
                                            thread_stack.push(to_thread);
                                        }
                                    }
                                }
                                CallbackReturn::Call { function, then } => {
                                    top_state
                                        .return_ext(fuel, ExternalReturn::Call(function, then));
                                }
                                CallbackReturn::Resume { thread, then } => {
                                    top_state.return_ext(fuel, ExternalReturn::Resume(then));
                                    if let Err(err) = thread
                                        .resume(ctx, Variadic(top_state.take_return().unwrap()))
                                    {
                                        top_state.unwind(&ctx, err.into());
                                    } else {
                                        if top_state.frames.len() == 1 {
                                            // Tail call the thread resume if we can.
                                            assert!(matches!(
                                                top_state.frames[0],
                                                Frame::WaitThread
                                            ));
                                            thread_stack.pop();
                                        }
                                        thread_stack.push(thread);
                                    }
                                }
                            },

                            Err(error) => top_state.unwind(&ctx, error),
                        }
                    }
                    Frame::Sequence(mut sequence) => {
                        fuel.consume_fuel(FUEL_PER_SEQ_STEP);
                        top_state.frames.push(Frame::Calling);

                        let mut stack =
                            mem::replace(&mut top_state.external_stack, Stack::new(&ctx));
                        let error = top_state.error.take();
                        drop(top_state);
                        let fin = if let Some(error) = error {
                            assert!(stack.is_empty());
                            sequence.error(ctx, fuel, error, &mut stack)
                        } else {
                            sequence.poll(ctx, fuel, &mut stack)
                        };
                        top_state = top_thread.0.borrow_mut(&ctx);
                        top_state.external_stack = stack;

                        assert!(
                            matches!(top_state.frames.pop(), Some(Frame::Calling)),
                            "thread state has changed while callback was run"
                        );

                        match fin {
                            Ok(ret) => match ret {
                                SequencePoll::Pending => {
                                    top_state.return_ext(fuel, ExternalReturn::Sequence(sequence));
                                }
                                SequencePoll::Return => {
                                    top_state.return_ext(fuel, ExternalReturn::Return);
                                }
                                SequencePoll::Yield { to_thread, is_tail } => {
                                    top_state.return_ext(
                                        fuel,
                                        ExternalReturn::Yield(if is_tail {
                                            None
                                        } else {
                                            Some(sequence)
                                        }),
                                    );

                                    if let Some(to_thread) = to_thread {
                                        if let Err(err) = to_thread
                                            .resume(ctx, Variadic(top_state.take_return().unwrap()))
                                        {
                                            top_state.unwind(&ctx, err.into());
                                        } else {
                                            thread_stack.pop();
                                            thread_stack.push(to_thread);
                                        }
                                    }
                                }
                                SequencePoll::Call { function, is_tail } => {
                                    top_state.return_ext(
                                        fuel,
                                        ExternalReturn::Call(
                                            function,
                                            if is_tail { None } else { Some(sequence) },
                                        ),
                                    );
                                }
                                SequencePoll::Resume { thread, is_tail } => {
                                    top_state.return_ext(
                                        fuel,
                                        ExternalReturn::Resume(if is_tail {
                                            None
                                        } else {
                                            Some(sequence)
                                        }),
                                    );
                                    if let Err(err) = thread
                                        .resume(ctx, Variadic(top_state.take_return().unwrap()))
                                    {
                                        top_state.unwind(&ctx, err.into());
                                    } else {
                                        if top_state.frames.len() == 1 {
                                            // Tail call the thread resume if we can.
                                            assert!(matches!(
                                                top_state.frames[0],
                                                Frame::WaitThread
                                            ));
                                            thread_stack.pop();
                                        }
                                        thread_stack.push(thread);
                                    }
                                }
                            },
                            Err(error) => top_state.unwind(&ctx, error),
                        }
                    }
                    frame @ Frame::Lua { .. } => {
                        top_state.frames.push(frame);
                        assert!(top_state.external_stack.is_empty());
                        assert!(top_state.error.is_none());

                        const VM_GRANULARITY: u32 = 64;

                        let lua_frame = LuaFrame {
                            state: &mut top_state,
                            thread: top_thread,
                            fuel,
                        };
                        match run_vm(ctx, lua_frame, VM_GRANULARITY) {
                            Err(err) => {
                                top_state.unwind(&ctx, err.into());
                            }
                            Ok(instructions_run) => {
                                fuel.consume_fuel(instructions_run.try_into().unwrap());
                            }
                        }
                    }
                    _ => panic!("tried to step invalid frame type"),
                }
            }

            if !fuel.should_continue() {
                break false;
            }
        }
    }

    pub fn take_return<T: FromMultiValue<'gc>>(
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
            thread_stack[0].take_return(ctx)
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

pub(super) struct LuaFrame<'gc, 'a> {
    thread: Thread<'gc>,
    state: &'a mut ThreadState<'gc>,
    fuel: &'a mut Fuel,
}

impl<'gc, 'a> LuaFrame<'gc, 'a> {
    // Returns the active closure for this Lua frame
    pub(super) fn closure(&self) -> Closure<'gc> {
        match self.state.frames.last() {
            Some(Frame::Lua { bottom, .. }) => match self.state.lua_stack[*bottom] {
                Value::Function(Function::Closure(c)) => c,
                _ => panic!("thread bottom is not a closure"),
            },
            _ => panic!("top frame is not lua frame"),
        }
    }

    // returns a view of the Lua frame's registers
    pub(super) fn registers<'b>(&'b mut self) -> LuaRegisters<'gc, 'b> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua {
                bottom, base, pc, ..
            }) => {
                let (upper_stack, stack_frame) = self.state.lua_stack[..].split_at_mut(*base);
                LuaRegisters {
                    pc,
                    stack_frame,
                    upper_stack,
                    bottom: *bottom,
                    base: *base,
                    open_upvalues: &mut self.state.open_upvalues,
                    thread: self.thread,
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Place the current frame's varargs at the given register, expecting the given count
    pub(super) fn varargs(&mut self, dest: RegisterIndex, count: VarCount) -> Result<(), VMError> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua {
                bottom,
                base,
                is_variable,
                ..
            }) => {
                if *is_variable {
                    return Err(VMError::ExpectedVariableStack(false));
                }

                let varargs_start = *bottom + 1;
                let varargs_len = *base - varargs_start;
                let dest = *base + dest.0 as usize;
                if let Some(count) = count.to_constant() {
                    for i in 0..count as usize {
                        self.state.lua_stack[dest + i] = if i < varargs_len {
                            self.state.lua_stack[varargs_start + i]
                        } else {
                            Value::Nil
                        };
                    }
                } else {
                    *is_variable = true;
                    self.state.lua_stack.resize(dest + varargs_len);
                    for i in 0..varargs_len {
                        self.state.lua_stack[dest + i] = self.state.lua_stack[varargs_start + i];
                    }
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
        Ok(())
    }

    pub(super) fn set_table_list(
        &mut self,
        mc: &Mutation<'gc>,
        table_base: RegisterIndex,
        count: VarCount,
    ) -> Result<(), VMError> {
        let Some(&mut Frame::Lua {
            base,
            ref mut is_variable,
            stack_size,
            ..
        }) = self.state.frames.last_mut()
        else {
            panic!("top frame is not lua frame");
        };

        if count.is_variable() != *is_variable {
            return Err(VMError::ExpectedVariableStack(count.is_variable()));
        }

        let table_ind = base + table_base.0 as usize;
        let start_ind = table_ind + 1;
        let table = self.state.lua_stack[table_ind];
        let Value::Table(table) = table else {
            return Err(TypeError {
                expected: "table",
                found: table.type_name(),
            }
            .into());
        };

        let set_count = count
            .to_constant()
            .map(|c| c as usize)
            .unwrap_or(self.state.lua_stack.len() - table_ind - 2);

        let Value::Integer(mut start) = self.state.lua_stack[start_ind] else {
            return Err(TypeError {
                expected: "integer",
                found: self.state.lua_stack[start_ind].type_name(),
            }
            .into());
        };

        for i in 0..set_count {
            if let Some(inc) = start.checked_add(1) {
                start = inc;
                table
                    .set_value(mc, inc.into(), self.state.lua_stack[table_ind + 2 + i])
                    .unwrap();
            } else {
                break;
            }
        }

        self.state.lua_stack[start_ind] = Value::Integer(start);

        if count.is_variable() {
            self.state.lua_stack.resize(base + stack_size);
            *is_variable = false;
        }

        Ok(())
    }

    // Call the function at the given register with the given arguments. On return, results will be
    // placed starting at the function register.
    pub(super) fn call_function(
        self,
        ctx: Context<'gc>,
        func: RegisterIndex,
        args: VarCount,
        returns: VarCount,
    ) -> Result<(), VMError> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua {
                expected_return,
                is_variable,
                base,
                ..
            }) => {
                if *is_variable != args.is_variable() {
                    return Err(VMError::ExpectedVariableStack(args.is_variable()));
                }

                *expected_return = Some(LuaReturn::Normal(returns));
                let function_index = *base + func.0 as usize;
                let arg_count = args
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(self.state.lua_stack.len() - function_index - 1);

                consume_call_fuel(self.fuel, arg_count);

                match meta_ops::call(ctx, self.state.lua_stack[function_index])? {
                    Function::Closure(closure) => {
                        self.state.lua_stack[function_index] = closure.into();
                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if arg_count > fixed_params {
                            self.state.lua_stack.resize(function_index + 1 + arg_count);
                            self.state.lua_stack[function_index + 1..].rotate_left(fixed_params);
                            function_index + 1 + (arg_count - fixed_params)
                        } else {
                            function_index + 1
                        };

                        self.state.lua_stack.resize(base + stack_size);

                        self.state.frames.push(Frame::Lua {
                            bottom: function_index,
                            base,
                            is_variable: false,
                            pc: 0,
                            stack_size,
                            expected_return: None,
                        });
                        Ok(())
                    }
                    Function::Callback(callback) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state.external_stack.extend(
                            &self.state.lua_stack
                                [function_index + 1..function_index + 1 + arg_count],
                        );
                        self.state.frames.push(Frame::Callback(callback));
                        self.state.lua_stack.resize(function_index);
                        Ok(())
                    }
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Calls the function at the given index with a constant number of arguments without
    // invalidating the function or its arguments. Returns are placed *after* the function and its
    // aruments, and all registers past this are invalidated as normal.
    pub(super) fn call_function_keep(
        self,
        ctx: Context<'gc>,
        func: RegisterIndex,
        arg_count: u8,
        returns: VarCount,
    ) -> Result<(), VMError> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua {
                expected_return,
                is_variable,
                base,
                ..
            }) => {
                if *is_variable {
                    return Err(VMError::ExpectedVariableStack(false));
                }

                consume_call_fuel(self.fuel, arg_count as usize);

                let arg_count = arg_count as usize;
                *expected_return = Some(LuaReturn::Normal(returns));
                let function_index = *base + func.0 as usize;
                let top = function_index + 1 + arg_count;

                match meta_ops::call(ctx, self.state.lua_stack[function_index])? {
                    Function::Closure(closure) => {
                        self.state.lua_stack.resize(top + 1 + arg_count);
                        for i in 1..arg_count + 1 {
                            self.state.lua_stack[top + i] =
                                self.state.lua_stack[function_index + i];
                        }

                        self.state.lua_stack[top] = closure.into();
                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if arg_count > fixed_params {
                            self.state.lua_stack[top + 1..].rotate_left(fixed_params);
                            top + 1 + (arg_count - fixed_params)
                        } else {
                            top + 1
                        };

                        self.state.lua_stack.resize(base + stack_size);

                        self.state.frames.push(Frame::Lua {
                            bottom: top,
                            base,
                            is_variable: false,
                            pc: 0,
                            stack_size,
                            expected_return: None,
                        });
                        Ok(())
                    }
                    Function::Callback(callback) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state
                            .external_stack
                            .extend(&self.state.lua_stack[function_index + 1..top]);
                        self.state.lua_stack.resize(top);
                        self.state.frames.push(Frame::Callback(callback));
                        Ok(())
                    }
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Calls an externally defined function in a completely non-destructive way in a new frame, and
    // places an optional single result of this function call at the given register.
    //
    // Nothing at all in the frame is invalidated, other than optionally placing the return value.
    pub(super) fn call_meta_function(
        self,
        ctx: Context<'gc>,
        func: Function<'gc>,
        args: &[Value<'gc>],
        ret_index: Option<RegisterIndex>,
    ) -> Result<(), VMError> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua {
                expected_return,
                is_variable,
                base,
                stack_size,
                ..
            }) => {
                if *is_variable {
                    return Err(VMError::ExpectedVariableStack(false));
                }

                consume_call_fuel(self.fuel, args.len());

                *expected_return = Some(LuaReturn::Meta(ret_index));
                let top = *base + *stack_size;

                match meta_ops::call(ctx, func.into())? {
                    Function::Closure(closure) => {
                        self.state.lua_stack.resize(top + 1 + args.len());
                        self.state.lua_stack[top] = closure.into();
                        self.state.lua_stack[top + 1..top + 1 + args.len()].copy_from_slice(args);

                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if args.len() > fixed_params {
                            self.state.lua_stack[top + 1..].rotate_left(fixed_params);
                            top + 1 + (args.len() - fixed_params)
                        } else {
                            top + 1
                        };

                        self.state.lua_stack.resize(base + stack_size);

                        self.state.frames.push(Frame::Lua {
                            bottom: top,
                            base,
                            is_variable: false,
                            pc: 0,
                            stack_size,
                            expected_return: None,
                        });
                        Ok(())
                    }
                    Function::Callback(callback) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state.external_stack.extend(args);
                        self.state.lua_stack.resize(top);
                        self.state.frames.push(Frame::Callback(callback));
                        Ok(())
                    }
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Tail-call the function at the given register with the given arguments. Pops the current Lua
    // frame, pushing a new frame for the given function.
    pub(super) fn tail_call_function(
        self,
        ctx: Context<'gc>,
        func: RegisterIndex,
        args: VarCount,
    ) -> Result<(), VMError> {
        match self.state.frames.last() {
            Some(&Frame::Lua {
                bottom,
                base,
                is_variable,
                ..
            }) => {
                if is_variable != args.is_variable() {
                    return Err(VMError::ExpectedVariableStack(args.is_variable()));
                }

                self.state.close_upvalues(&ctx, bottom);

                let function_index = base + func.0 as usize;
                let arg_count = args
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(self.state.lua_stack.len() - function_index - 1);

                consume_call_fuel(self.fuel, arg_count);

                match meta_ops::call(ctx, self.state.lua_stack[function_index])? {
                    Function::Closure(closure) => {
                        self.state.lua_stack[bottom] = closure.into();
                        for i in 0..arg_count {
                            self.state.lua_stack[bottom + 1 + i] =
                                self.state.lua_stack[function_index + 1 + i];
                        }

                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if arg_count > fixed_params {
                            self.state.lua_stack.resize(bottom + 1 + arg_count);
                            self.state.lua_stack[bottom + 1..].rotate_left(fixed_params);
                            bottom + 1 + (arg_count - fixed_params)
                        } else {
                            bottom + 1
                        };

                        self.state.lua_stack.resize(base + stack_size);

                        self.state.frames.pop();
                        self.state.frames.push(Frame::Lua {
                            bottom,
                            base,
                            is_variable: false,
                            pc: 0,
                            stack_size,
                            expected_return: None,
                        });
                        Ok(())
                    }
                    Function::Callback(callback) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state.external_stack.extend(
                            &self.state.lua_stack
                                [function_index + 1..function_index + 1 + arg_count],
                        );
                        self.state.frames.pop();
                        self.state.frames.push(Frame::Callback(callback));
                        self.state.lua_stack.resize(bottom);
                        Ok(())
                    }
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Return to the upper frame with results starting at the given register index.
    pub(super) fn return_upper(
        self,
        mc: &Mutation<'gc>,
        start: RegisterIndex,
        count: VarCount,
    ) -> Result<(), VMError> {
        match self.state.frames.pop() {
            Some(Frame::Lua {
                bottom,
                base,
                is_variable,
                ..
            }) => {
                if is_variable != count.is_variable() {
                    return Err(VMError::ExpectedVariableStack(count.is_variable()));
                }
                self.state.close_upvalues(mc, bottom);

                let start = base + start.0 as usize;
                let count = count
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(self.state.lua_stack.len() - start);

                consume_call_fuel(self.fuel, count);

                match self.state.frames.last_mut() {
                    Some(Frame::Sequence { .. }) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state
                            .external_stack
                            .extend(&self.state.lua_stack[start..start + count]);
                        self.state.lua_stack.resize(bottom);
                    }
                    Some(Frame::Lua {
                        expected_return,
                        is_variable,
                        base,
                        stack_size,
                        ..
                    }) => match expected_return {
                        Some(LuaReturn::Normal(expected_return)) => {
                            let returning = expected_return
                                .to_constant()
                                .map(|c| c as usize)
                                .unwrap_or(count);

                            for i in 0..returning.min(count) {
                                self.state.lua_stack[bottom + i] = self.state.lua_stack[start + i]
                            }

                            for i in count..returning {
                                self.state.lua_stack[bottom + i] = Value::Nil;
                            }

                            if expected_return.is_variable() {
                                self.state.lua_stack.resize(bottom + returning);
                                *is_variable = true;
                            } else {
                                self.state.lua_stack.resize(*base + *stack_size);
                                *is_variable = false;
                            }
                        }
                        Some(LuaReturn::Meta(meta_ind)) => {
                            let meta_ret = if count > 0 {
                                self.state.lua_stack[start]
                            } else {
                                Value::Nil
                            };
                            self.state.lua_stack.resize(*base + *stack_size);
                            *is_variable = false;
                            if let Some(meta_ind) = meta_ind {
                                self.state.lua_stack[*base + meta_ind.0 as usize] = meta_ret;
                            }
                        }
                        None => {
                            panic!("no expected returns set for returned to lua frame")
                        }
                    },
                    None => {
                        assert!(self.state.external_stack.is_empty());
                        self.state
                            .external_stack
                            .extend(&self.state.lua_stack[start..start + count]);
                        self.state.frames.push(Frame::HasResult);
                        self.state.lua_stack.clear();
                    }
                    _ => panic!("lua frame must be above a sequence or lua frame"),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
        Ok(())
    }
}

pub(super) struct LuaRegisters<'gc, 'a> {
    pub pc: &'a mut usize,
    pub stack_frame: &'a mut [Value<'gc>],
    upper_stack: &'a mut [Value<'gc>],
    bottom: usize,
    base: usize,
    open_upvalues: &'a mut vec::Vec<UpValue<'gc>, MetricsAlloc<'gc>>,
    thread: Thread<'gc>,
}

impl<'gc, 'a> LuaRegisters<'gc, 'a> {
    pub(super) fn open_upvalue(&mut self, mc: &Mutation<'gc>, reg: RegisterIndex) -> UpValue<'gc> {
        let ind = self.base + reg.0 as usize;
        match self
            .open_upvalues
            .binary_search_by(|&u| open_upvalue_ind(u).cmp(&ind))
        {
            Ok(i) => self.open_upvalues[i],
            Err(i) => {
                let uv = UpValue(Gc::new(mc, Lock::new(UpValueState::Open(self.thread, ind))));
                self.open_upvalues.insert(i, uv);
                uv
            }
        }
    }

    pub(super) fn get_upvalue(&self, upvalue: UpValue<'gc>) -> Value<'gc> {
        match upvalue.0.get() {
            UpValueState::Open(upvalue_thread, ind) => {
                if upvalue_thread == self.thread {
                    assert!(
                        ind < self.bottom,
                        "upvalues must be above the current Lua frame"
                    );
                    self.upper_stack[ind]
                } else {
                    upvalue_thread.0.borrow().lua_stack[ind]
                }
            }
            UpValueState::Closed(v) => v,
        }
    }

    pub(super) fn set_upvalue(
        &mut self,
        mc: &Mutation<'gc>,
        upvalue: UpValue<'gc>,
        value: Value<'gc>,
    ) {
        match upvalue.0.get() {
            UpValueState::Open(upvalue_thread, ind) => {
                if upvalue_thread == self.thread {
                    assert!(
                        ind < self.bottom,
                        "upvalues must be above the current Lua frame"
                    );
                    self.upper_stack[ind] = value;
                } else {
                    upvalue_thread.0.borrow_mut(mc).lua_stack[ind] = value;
                }
            }
            UpValueState::Closed(v) => {
                upvalue.0.set(mc, UpValueState::Closed(v));
            }
        }
    }

    pub(super) fn close_upvalues(&mut self, mc: &Mutation<'gc>, bottom_register: RegisterIndex) {
        let bottom = self.base + bottom_register.0 as usize;
        let start = match self
            .open_upvalues
            .binary_search_by(|&u| open_upvalue_ind(u).cmp(&bottom))
        {
            Ok(i) => i,
            Err(i) => i,
        };

        for &upval in &self.open_upvalues[start..] {
            match upval.0.get() {
                UpValueState::Open(upvalue_thread, ind) => {
                    assert!(upvalue_thread == self.thread);
                    upval.0.set(
                        mc,
                        UpValueState::Closed(if ind < self.base {
                            self.upper_stack[ind]
                        } else {
                            self.stack_frame[ind - self.base]
                        }),
                    );
                }
                UpValueState::Closed(_) => panic!("upvalue is not open"),
            }
        }

        self.open_upvalues.truncate(start);
    }
}

enum ExternalReturn<'gc> {
    Return,
    Sequence(AnySequence<'gc>),
    Yield(Option<AnySequence<'gc>>),
    Call(Function<'gc>, Option<AnySequence<'gc>>),
    Resume(Option<AnySequence<'gc>>),
}

#[derive(Collect)]
#[collect(require_static)]
enum LuaReturn {
    // Normal function call, place return values at the bottom of the returning function's stack,
    // as normal.
    Normal(VarCount),
    // Synthetic metamethod call, place an optional single return value at an index relative to the
    // returned to function's bottom.
    Meta(Option<RegisterIndex>),
}

#[derive(Collect)]
#[collect(no_drop)]
enum Frame<'gc> {
    // A running Lua frame.
    Lua {
        bottom: usize,
        base: usize,
        is_variable: bool,
        pc: usize,
        stack_size: usize,
        expected_return: Option<LuaReturn>,
    },
    // A function call that has not yet been run.
    Start(Function<'gc>),
    // Thread has yielded and is waiting resume.
    Yielded,
    // A callback that has been queued but not called yet. Arguments will be in the external stack.
    Callback(AnyCallback<'gc>),
    // A frame for a running sequence. When it is the top frame, either the `poll` or `error` method
    // will be called on the next call to `Thread::step`, depending on whether there is a pending
    // error.
    Sequence(AnySequence<'gc>),
    // We are waiting on an upper thread to finish.
    WaitThread,
    // A marker frame that marks the thread as having available return values or an error that must
    // be taken.
    HasResult,
    // A marker frame that marks the thread as *actively* calling some external function, a callback
    // or sequence.
    //
    // The thread must be unlocked during external calls to permit cross-thread upvalue handling,
    // but this presents a danger if methods on this thread were to be recursively called at this
    // time. This frame keeps the thread in the `Running` mode during external calls, ensuring the
    // thread cannot be mutated.
    Calling,
}

#[derive(Collect)]
#[collect(no_drop)]
struct ThreadState<'gc> {
    frames: vec::Vec<Frame<'gc>, MetricsAlloc<'gc>>,
    lua_stack: Stack<'gc>,
    open_upvalues: vec::Vec<UpValue<'gc>, MetricsAlloc<'gc>>,
    external_stack: Stack<'gc>,
    error: Option<Error<'gc>>,
}

impl<'gc> ThreadState<'gc> {
    fn mode(&self) -> ThreadMode {
        match self.frames.last() {
            None => {
                debug_assert!(self.lua_stack.is_empty() && self.open_upvalues.is_empty(),);
                ThreadMode::Stopped
            }
            Some(frame) => match frame {
                Frame::HasResult => ThreadMode::Result,
                Frame::Lua { .. } | Frame::Callback { .. } | Frame::Sequence { .. } => {
                    ThreadMode::Normal
                }
                Frame::WaitThread => ThreadMode::Waiting,
                Frame::Calling => ThreadMode::Running,
                Frame::Start(_) | Frame::Yielded => ThreadMode::Suspended,
            },
        }
    }

    fn check_mode(&self, expected_mode: ThreadMode) -> Result<(), BadThreadMode> {
        let mode = self.mode();
        if mode == expected_mode {
            Ok(())
        } else {
            Err(BadThreadMode {
                found: mode,
                expected: Some(expected_mode),
            })
        }
    }

    fn ext_call_function(&mut self, function: Function<'gc>) {
        match function {
            Function::Closure(closure) => {
                let fixed_params = closure.0.proto.fixed_params as usize;
                let stack_size = closure.0.proto.stack_size as usize;

                let var_params = if self.external_stack.len() > fixed_params {
                    self.external_stack.len() - fixed_params
                } else {
                    0
                };
                let bottom = self.lua_stack.len();
                let base = bottom + 1 + var_params;

                self.lua_stack.resize(base + stack_size);

                self.lua_stack[bottom] = Value::Function(Function::Closure(closure));
                for i in 0..fixed_params {
                    self.lua_stack[base + i] = self.external_stack.get(i);
                }
                for i in 0..var_params {
                    self.lua_stack[bottom + 1 + i] = self.external_stack.get(fixed_params + i)
                }

                self.external_stack.clear();

                self.frames.push(Frame::Lua {
                    bottom,
                    base,
                    is_variable: false,
                    pc: 0,
                    stack_size,
                    expected_return: None,
                });
            }
            Function::Callback(callback) => {
                self.frames.push(Frame::Callback(callback));
            }
        }
    }

    fn return_to_lua(&mut self) {
        match self.frames.last_mut() {
            Some(Frame::Lua {
                expected_return,
                is_variable,
                base,
                stack_size,
                ..
            }) => match expected_return {
                Some(LuaReturn::Normal(ret_count)) => {
                    let return_len = ret_count
                        .to_constant()
                        .map(|c| c as usize)
                        .unwrap_or(self.external_stack.len());

                    let bottom = self.lua_stack.len();
                    self.lua_stack.resize(bottom + return_len);

                    for i in 0..return_len.min(self.external_stack.len()) {
                        self.lua_stack[bottom + i] = self.external_stack.get(i);
                    }

                    self.external_stack.clear();

                    *is_variable = ret_count.is_variable();
                    if !ret_count.is_variable() {
                        self.lua_stack.resize(*base + *stack_size);
                    }
                }
                Some(LuaReturn::Meta(meta_ind)) => {
                    let meta_ret = self.external_stack.get(0);
                    self.external_stack.clear();
                    self.lua_stack.resize(*base + *stack_size);
                    *is_variable = false;
                    if let Some(meta_ind) = meta_ind {
                        self.lua_stack[*base + meta_ind.0 as usize] = meta_ret;
                    }
                }
                None => panic!("no expected return set for returned to lua frame"),
            },
            _ => panic!("no lua frame to return to"),
        };
    }

    fn unwind(&mut self, mc: &Mutation<'gc>, error: Error<'gc>) {
        self.external_stack.clear();
        self.error = Some(error);

        while let Some(frame) = self.frames.pop() {
            match frame {
                Frame::Lua { bottom, .. } => {
                    self.close_upvalues(mc, bottom);
                    self.lua_stack.resize(bottom);
                }
                Frame::Sequence(sequence) => {
                    self.frames.push(Frame::Sequence(sequence));
                    return;
                }
                _ => {}
            }
        }
        assert!(self.lua_stack.is_empty());
        self.frames.push(Frame::HasResult);
    }

    fn return_ext(&mut self, fuel: &mut Fuel, ret: ExternalReturn<'gc>) {
        match ret {
            ExternalReturn::Return => match self.frames.last_mut() {
                Some(Frame::Sequence { .. }) => {}
                Some(Frame::Lua { .. }) => {
                    // Consume the per-return fuel for pushing the returns back to Lua.
                    consume_call_fuel(fuel, self.external_stack.len());
                    self.return_to_lua();
                }
                None => {
                    self.frames.push(Frame::HasResult);
                }
                _ => panic!("frame above callback must be sequence or lua frame"),
            },
            ExternalReturn::Sequence(sequence) => {
                self.frames.push(Frame::Sequence(sequence));
            }
            ExternalReturn::Yield(sequence) => {
                if let Some(sequence) = sequence {
                    self.frames.push(Frame::Sequence(sequence));
                }
                self.frames.push(Frame::Yielded);
                self.frames.push(Frame::HasResult);
            }
            ExternalReturn::Call(function, sequence) => {
                if let Some(sequence) = sequence {
                    self.frames.push(Frame::Sequence(sequence));
                }
                if matches!(function, Function::Closure(_)) {
                    // Consume the per-call fuel for pushing the arguments back to lua.
                    consume_call_fuel(fuel, self.external_stack.len());
                }
                self.ext_call_function(function);
            }
            ExternalReturn::Resume(sequence) => {
                if let Some(sequence) = sequence {
                    self.frames.push(Frame::Sequence(sequence));
                }
                self.frames.push(Frame::WaitThread);
                self.frames.push(Frame::HasResult);
            }
        }
    }

    fn take_return(&mut self) -> Result<impl Iterator<Item = Value<'gc>> + '_, Error<'gc>> {
        assert!(matches!(self.frames.pop(), Some(Frame::HasResult)));
        if let Some(error) = self.error.take() {
            assert!(self.external_stack.is_empty());
            Err(error)
        } else {
            Ok(self.external_stack.drain(..))
        }
    }

    fn close_upvalues(&mut self, mc: &Mutation<'gc>, bottom: usize) {
        let start = match self
            .open_upvalues
            .binary_search_by(|&u| open_upvalue_ind(u).cmp(&bottom))
        {
            Ok(i) => i,
            Err(i) => i,
        };

        let this_ptr = self as *mut _;
        for &upval in &self.open_upvalues[start..] {
            match upval.0.get() {
                UpValueState::Open(upvalue_thread, ind) => {
                    assert!(upvalue_thread.0.as_ptr() == this_ptr);
                    upval.0.set(mc, UpValueState::Closed(self.lua_stack[ind]));
                }
                UpValueState::Closed(_) => panic!("upvalue is not open"),
            }
        }

        self.open_upvalues.truncate(start);
    }
}

// Fuel consumed per Lua call and return, to represent the cost from manipulating the Lua stack.
const FUEL_PER_CALL: i32 = 4;
const FUEL_PER_ARG: i32 = 1;

// Implicit cost per callback call. If the callback is Lua -> Rust, or Rust -> Lua, then the
// FUEL_PER_CALL is also added to this for calling / returning.
const FUEL_PER_CALLBACK: i32 = 8;

// Implicit cost per sequence step.
const FUEL_PER_SEQ_STEP: i32 = 4;

fn consume_call_fuel(fuel: &mut Fuel, args: usize) {
    fuel.consume_fuel(FUEL_PER_CALL + i32::try_from(args).unwrap_or(i32::MAX) * FUEL_PER_ARG);
}

fn open_upvalue_ind<'gc>(u: UpValue<'gc>) -> usize {
    match u.0.get() {
        UpValueState::Open(_, ind) => ind,
        UpValueState::Closed(_) => panic!("upvalue is not open"),
    }
}
