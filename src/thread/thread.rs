use std::{
    cell::RefMut,
    collections::{btree_map::Entry as BTreeEntry, BTreeMap},
    fmt::{self, Debug},
    hash::{Hash, Hasher},
    mem,
};

use gc_arena::{
    lock::{Lock, RefLock},
    Collect, Gc, Mutation,
};

use crate::{
    meta_ops,
    thread::run_vm,
    types::{RegisterIndex, VarCount},
    AnyCallback, AnySequence, BadThreadMode, CallbackReturn, Closure, Context, Error,
    FromMultiValue, Function, IntoMultiValue, SequencePoll, Stack, ThreadError, UpValue,
    UpValueState, Value,
};

#[derive(Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct Thread<'gc>(pub(crate) Gc<'gc, RefLock<ThreadState<'gc>>>);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadMode {
    // No frames are on the thread and there are no available results, the thread can be started.
    Stopped,
    // The thread has an error or has returned (or yielded) values that must be taken to move the
    // thread back to the `Stopped` (or `Suspended`) state.
    Result,
    // Thread has an active Lua frame or is waiting for a callback or sequence to finish.
    Normal,
    // Thread is currently inside its own `Thread::step` function.
    Running,
    // Thread has yielded and is waiting on being resumed.
    Suspended,
}

impl<'gc> Thread<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Thread<'gc> {
        Thread(Gc::new(
            mc,
            RefLock::new(ThreadState {
                stack: Vec::new(),
                frames: Vec::new(),
                open_upvalues: BTreeMap::new(),
                external_stack: Stack::new(),
                error: None,
            }),
        ))
    }

    pub fn mode(self) -> ThreadMode {
        if let Ok(state) = self.0.try_borrow() {
            state.mode()
        } else {
            ThreadMode::Running
        }
    }

    /// If this thread is `Stopped`, start a new function with the given arguments.
    pub fn start(
        self,
        ctx: Context<'gc>,
        function: Function<'gc>,
        args: impl IntoMultiValue<'gc>,
    ) -> Result<(), BadThreadMode> {
        let mut state = self.write_state(&ctx, Some(ThreadMode::Stopped))?;

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
        let mut state = self.write_state(mc, Some(ThreadMode::Stopped))?;
        state.frames.push(Frame::StartCoroutine(function));
        Ok(())
    }

    /// If the thread is in the `Result` mode, take the returned (or yielded) values. Moves the
    /// thread back to the `Stopped` (or `Suspended`) mode.
    pub fn take_return<T: FromMultiValue<'gc>>(
        self,
        ctx: Context<'gc>,
    ) -> Result<Result<T, Error<'gc>>, BadThreadMode> {
        let mut state = self.write_state(&ctx, Some(ThreadMode::Result))?;
        assert!(matches!(state.frames.pop(), Some(Frame::HasResult)));

        Ok(if let Some(error) = state.error.take() {
            assert!(state.external_stack.is_empty());
            Err(error)
        } else {
            state.external_stack.consume(ctx).map_err(Error::from)
        })
    }

    /// If the thread is in `Suspended` mode, resume it.
    pub fn resume(
        self,
        ctx: Context<'gc>,
        args: impl IntoMultiValue<'gc>,
    ) -> Result<(), BadThreadMode> {
        let mut state = self.write_state(&ctx, Some(ThreadMode::Suspended))?;

        assert!(state.external_stack.is_empty());
        state.external_stack.replace(ctx, args);

        match state.frames.pop().expect("no frame to resume") {
            Frame::StartCoroutine(function) => {
                assert!(
                    state.stack.is_empty()
                        && state.open_upvalues.is_empty()
                        && state.frames.is_empty()
                        && state.error.is_none()
                );
                state.ext_call_function(function);
            }
            Frame::ResumeCoroutine => match state.frames.last_mut() {
                Some(Frame::Sequence { .. }) => {}
                Some(Frame::Lua { .. }) => {
                    state.return_to_lua();
                }
                None => {
                    state.frames.push(Frame::HasResult);
                }
                _ => panic!("resume coroutine frame must be above a sequence or lua frame"),
            },
            _ => panic!("top frame not a suspended coroutine"),
        }
        Ok(())
    }

    /// If the thread is in `Suspended` mode, cause an error wherever the thread was suspended.
    pub fn resume_err(self, mc: &Mutation<'gc>, error: Error<'gc>) -> Result<(), BadThreadMode> {
        let mut state = self.write_state(mc, Some(ThreadMode::Suspended))?;
        assert!(state.external_stack.is_empty());
        state.unwind(mc, error);
        Ok(())
    }

    /// If the thread is in `Normal` mode, either run the Lua VM for a while or step any callback
    /// that we are waiting on.
    pub fn step(self, ctx: Context<'gc>) -> Result<(), BadThreadMode> {
        let mut state = self.write_state(&ctx, Some(ThreadMode::Normal))?;
        match state.frames.pop().expect("no frame to step") {
            Frame::Callback(callback) => {
                state.frames.push(Frame::Calling);

                assert!(state.error.is_none());
                let mut stack = mem::take(&mut state.external_stack);
                drop(state);
                let seq = callback.call(ctx, &mut stack);
                let mut state = self.0.borrow_mut(&ctx);
                state.external_stack = stack;

                assert!(
                    matches!(state.frames.pop(), Some(Frame::Calling)),
                    "thread state has changed while callback was run"
                );

                match seq {
                    Ok(ret) => state.return_ext(ret),
                    Err(error) => state.unwind(&ctx, error),
                }
            }
            Frame::Sequence(mut sequence) => {
                state.frames.push(Frame::Calling);

                let mut stack = mem::take(&mut state.external_stack);
                let error = state.error.take();
                drop(state);
                let fin = if let Some(error) = error {
                    assert!(stack.is_empty());
                    sequence.error(ctx, error, &mut stack)
                } else {
                    sequence.poll(ctx, &mut stack)
                };
                let mut state = self.0.borrow_mut(&ctx);
                state.external_stack = stack;

                assert!(
                    matches!(state.frames.pop(), Some(Frame::Calling)),
                    "thread state has changed while callback was run"
                );

                match fin {
                    Ok(SequencePoll::Pending) => {
                        state.return_ext(CallbackReturn::Sequence(sequence))
                    }
                    Ok(SequencePoll::Return) => state.return_ext(CallbackReturn::Return),
                    Ok(SequencePoll::Yield { is_tail: tail }) => {
                        state.return_ext(CallbackReturn::Yield(if tail {
                            None
                        } else {
                            Some(sequence)
                        }))
                    }
                    Ok(SequencePoll::Call {
                        function,
                        is_tail: tail,
                    }) => state.return_ext(CallbackReturn::TailCall(
                        function,
                        if tail { None } else { Some(sequence) },
                    )),
                    Err(error) => state.unwind(&ctx, error),
                }
            }
            frame @ Frame::Lua { .. } => {
                state.frames.push(frame);
                assert!(state.external_stack.is_empty());
                assert!(state.error.is_none());

                const VM_GRANULARITY: u32 = 256;
                let mut instructions = VM_GRANULARITY;

                loop {
                    let lua_frame = LuaFrame {
                        state: &mut state,
                        thread: self,
                    };
                    match run_vm(ctx, lua_frame, instructions) {
                        Err(err) => {
                            state.unwind(&ctx, err.into());
                            break;
                        }
                        Ok(i) => {
                            if let Some(Frame::Lua { .. }) = state.frames.last() {
                                instructions = i;
                                if instructions == 0 {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
            _ => panic!("tried to step invalid frame type"),
        }

        Ok(())
    }

    /// If this thread is in any other mode than `Running`, reset the thread completely and restore
    /// it to the `Stopped` state.
    pub fn reset(self, mc: &Mutation<'gc>) -> Result<(), BadThreadMode> {
        let mut state = self.write_state(mc, None)?;

        state.close_upvalues(mc, 0);
        assert!(state.open_upvalues.is_empty());

        state.stack.clear();
        state.frames.clear();
        state.external_stack.clear();
        state.error = None;
        Ok(())
    }

    fn write_state<'a>(
        &'a self,
        mc: &Mutation<'gc>,
        expected_mode: Option<ThreadMode>,
    ) -> Result<RefMut<'a, ThreadState<'gc>>, BadThreadMode> {
        assert!(expected_mode != Some(ThreadMode::Running));
        let state = self.0.try_borrow_mut(mc).map_err(|_| BadThreadMode {
            found: ThreadMode::Running,
            expected: expected_mode,
        })?;

        if expected_mode.is_some_and(|mode| mode != state.mode()) {
            Err(BadThreadMode {
                found: state.mode(),
                expected: expected_mode,
            })
        } else {
            Ok(state)
        }
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub(crate) struct ThreadState<'gc> {
    stack: Vec<Value<'gc>>,
    frames: Vec<Frame<'gc>>,
    open_upvalues: BTreeMap<usize, UpValue<'gc>>,
    external_stack: Stack<'gc>,
    error: Option<Error<'gc>>,
}

pub(crate) struct LuaFrame<'gc, 'a> {
    thread: Thread<'gc>,
    state: &'a mut ThreadState<'gc>,
}

pub(crate) struct LuaRegisters<'gc, 'a> {
    pub pc: &'a mut usize,
    pub stack_frame: &'a mut [Value<'gc>],
    upper_stack: &'a mut [Value<'gc>],
    base: usize,
    open_upvalues: &'a mut BTreeMap<usize, UpValue<'gc>>,
    thread: Thread<'gc>,
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
    // An running Lua frame.
    Lua {
        bottom: usize,
        base: usize,
        is_variable: bool,
        pc: usize,
        stack_size: usize,
        expected_return: Option<LuaReturn>,
    },
    // A suspended coroutine that has not yet been run.
    StartCoroutine(Function<'gc>),
    // A coroutine that has yielded and is waiting resume.
    ResumeCoroutine,
    // A callback that has been queued but not called yet. Arguments will be in the external stack.
    Callback(AnyCallback<'gc>),
    // A frame for a running sequence. When it is the top frame, either the `poll` or `error` method
    // will be called on the next call to `Thread::step`, depending on whether there is a pending
    // error.
    Sequence(AnySequence<'gc>),
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

impl<'gc, 'a> LuaFrame<'gc, 'a> {
    // Returns the active closure for this Lua frame
    pub(crate) fn closure(&self) -> Closure<'gc> {
        match self.state.frames.last() {
            Some(Frame::Lua { bottom, .. }) => match self.state.stack[*bottom] {
                Value::Function(Function::Closure(c)) => c,
                _ => panic!("thread bottom is not a closure"),
            },
            _ => panic!("top frame is not lua frame"),
        }
    }

    // returns a view of the Lua frame's registers
    pub(crate) fn registers<'b>(&'b mut self) -> LuaRegisters<'gc, 'b> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua { base, pc, .. }) => {
                let (upper_stack, stack_frame) = self.state.stack.split_at_mut(*base);
                LuaRegisters {
                    pc,
                    stack_frame,
                    upper_stack,
                    base: *base,
                    open_upvalues: &mut self.state.open_upvalues,
                    thread: self.thread,
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Place the current frame's varargs at the given register, expecting the given count
    pub(crate) fn varargs(
        &mut self,
        dest: RegisterIndex,
        count: VarCount,
    ) -> Result<(), ThreadError> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua {
                bottom,
                base,
                is_variable,
                ..
            }) => {
                if *is_variable {
                    return Err(ThreadError::ExpectedVariable(false));
                }

                let varargs_start = *bottom + 1;
                let varargs_len = *base - varargs_start;
                let dest = *base + dest.0 as usize;
                if let Some(count) = count.to_constant() {
                    for i in 0..count as usize {
                        self.state.stack[dest + i] = if i < varargs_len {
                            self.state.stack[varargs_start + i]
                        } else {
                            Value::Nil
                        };
                    }
                } else {
                    *is_variable = true;
                    self.state.stack.resize(dest + varargs_len, Value::Nil);
                    for i in 0..varargs_len {
                        self.state.stack[dest + i] = self.state.stack[varargs_start + i];
                    }
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
        Ok(())
    }

    // Call the function at the given register with the given arguments. On return, results will be
    // placed starting at the function register.
    pub(crate) fn call_function(
        self,
        ctx: Context<'gc>,
        func: RegisterIndex,
        args: VarCount,
        returns: VarCount,
    ) -> Result<(), ThreadError> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua {
                expected_return,
                is_variable,
                base,
                ..
            }) => {
                if *is_variable != args.is_variable() {
                    return Err(ThreadError::ExpectedVariable(*is_variable));
                }

                *expected_return = Some(LuaReturn::Normal(returns));
                let function_index = *base + func.0 as usize;
                let arg_count = args
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(self.state.stack.len() - function_index - 1);

                match meta_ops::call(ctx, self.state.stack[function_index]) {
                    Ok(Function::Closure(closure)) => {
                        self.state.stack[function_index] = closure.into();
                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if arg_count > fixed_params {
                            self.state.stack.truncate(function_index + 1 + arg_count);
                            self.state.stack[function_index + 1..].rotate_left(fixed_params);
                            function_index + 1 + (arg_count - fixed_params)
                        } else {
                            function_index + 1
                        };

                        self.state.stack.resize(base + stack_size, Value::Nil);

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
                    Ok(Function::Callback(callback)) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state.external_stack.extend(
                            &self.state.stack[function_index + 1..function_index + 1 + arg_count],
                        );
                        self.state.frames.push(Frame::Callback(callback));
                        self.state.stack.resize(function_index, Value::Nil);
                        Ok(())
                    }
                    Err(err) => Err(ThreadError::BadCall(err)),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Calls the function at the given index with a constant number of arguments without
    // invalidating the function or its arguments. Returns are placed *after* the function and its
    // aruments, and all registers past this are invalidated as normal.
    pub(crate) fn call_function_keep(
        self,
        ctx: Context<'gc>,
        func: RegisterIndex,
        arg_count: u8,
        returns: VarCount,
    ) -> Result<(), ThreadError> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua {
                expected_return,
                is_variable,
                base,
                ..
            }) => {
                if *is_variable {
                    return Err(ThreadError::ExpectedVariable(false));
                }

                let arg_count = arg_count as usize;
                *expected_return = Some(LuaReturn::Normal(returns));
                let function_index = *base + func.0 as usize;
                let top = function_index + 1 + arg_count;

                match meta_ops::call(ctx, self.state.stack[function_index]) {
                    Ok(Function::Closure(closure)) => {
                        self.state.stack.resize(top + 1 + arg_count, Value::Nil);
                        for i in 1..arg_count + 1 {
                            self.state.stack[top + i] = self.state.stack[function_index + i];
                        }

                        self.state.stack[top] = closure.into();
                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if arg_count > fixed_params {
                            self.state.stack[top + 1..].rotate_left(fixed_params);
                            top + 1 + (arg_count - fixed_params)
                        } else {
                            top + 1
                        };

                        self.state.stack.resize(base + stack_size, Value::Nil);

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
                    Ok(Function::Callback(callback)) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state
                            .external_stack
                            .extend(&self.state.stack[function_index + 1..top]);
                        self.state.stack.resize(top, Value::Nil);
                        self.state.frames.push(Frame::Callback(callback));
                        Ok(())
                    }
                    Err(err) => Err(ThreadError::BadCall(err)),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Calls an externally defined function in a completely non-destructive way in a new frame, and
    // places an optional single result of this function call at the given register.
    //
    // Nothing at all in the frame is invalidated, other than optionally placing the return value.
    pub(crate) fn call_meta_function(
        self,
        ctx: Context<'gc>,
        func: Function<'gc>,
        args: &[Value<'gc>],
        ret_index: Option<RegisterIndex>,
    ) -> Result<(), ThreadError> {
        match self.state.frames.last_mut() {
            Some(Frame::Lua {
                expected_return,
                is_variable,
                base,
                stack_size,
                ..
            }) => {
                if *is_variable {
                    return Err(ThreadError::ExpectedVariable(false));
                }

                *expected_return = Some(LuaReturn::Meta(ret_index));
                let top = *base + *stack_size;

                match meta_ops::call(ctx, func.into()) {
                    Ok(Function::Closure(closure)) => {
                        self.state.stack.resize(top + 1 + args.len(), Value::Nil);
                        self.state.stack[top] = closure.into();
                        self.state.stack[top + 1..top + 1 + args.len()].copy_from_slice(args);

                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if args.len() > fixed_params {
                            self.state.stack[top + 1..].rotate_left(fixed_params);
                            top + 1 + (args.len() - fixed_params)
                        } else {
                            top + 1
                        };

                        self.state.stack.resize(base + stack_size, Value::Nil);

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
                    Ok(Function::Callback(callback)) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state.external_stack.extend(args);
                        self.state.stack.resize(top, Value::Nil);
                        self.state.frames.push(Frame::Callback(callback));
                        Ok(())
                    }
                    Err(err) => Err(ThreadError::BadCall(err)),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Tail-call the function at the given register with the given arguments. Pops the current Lua
    // frame, pushing a new frame for the given function.
    pub(crate) fn tail_call_function(
        self,
        ctx: Context<'gc>,
        func: RegisterIndex,
        args: VarCount,
    ) -> Result<(), ThreadError> {
        match self.state.frames.last() {
            Some(&Frame::Lua {
                bottom,
                base,
                is_variable,
                ..
            }) => {
                if is_variable != args.is_variable() {
                    return Err(ThreadError::ExpectedVariable(is_variable));
                }

                self.state.close_upvalues(&ctx, bottom);

                let function_index = base + func.0 as usize;
                let arg_count = args
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(self.state.stack.len() - function_index - 1);

                match meta_ops::call(ctx, self.state.stack[function_index]) {
                    Ok(Function::Closure(closure)) => {
                        self.state.stack[bottom] = closure.into();
                        for i in 0..arg_count {
                            self.state.stack[bottom + 1 + i] =
                                self.state.stack[function_index + 1 + i];
                        }

                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if arg_count > fixed_params {
                            self.state.stack.truncate(bottom + 1 + arg_count);
                            self.state.stack[bottom + 1..].rotate_left(fixed_params);
                            bottom + 1 + (arg_count - fixed_params)
                        } else {
                            bottom + 1
                        };

                        self.state.stack.resize(base + stack_size, Value::Nil);

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
                    Ok(Function::Callback(callback)) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state.external_stack.extend(
                            &self.state.stack[function_index + 1..function_index + 1 + arg_count],
                        );
                        self.state.frames.pop();
                        self.state.frames.push(Frame::Callback(callback));
                        self.state.stack.truncate(bottom);
                        Ok(())
                    }
                    Err(err) => Err(ThreadError::BadCall(err)),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Return to the upper frame with results starting at the given register index.
    pub(crate) fn return_upper(
        self,
        mc: &Mutation<'gc>,
        start: RegisterIndex,
        count: VarCount,
    ) -> Result<(), ThreadError> {
        match self.state.frames.pop() {
            Some(Frame::Lua {
                bottom,
                base,
                is_variable,
                ..
            }) => {
                if is_variable != count.is_variable() {
                    return Err(ThreadError::ExpectedVariable(is_variable));
                }
                self.state.close_upvalues(mc, bottom);

                let start = base + start.0 as usize;
                let count = count
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(self.state.stack.len() - start);

                match self.state.frames.last_mut() {
                    Some(Frame::Sequence { .. }) => {
                        assert!(self.state.external_stack.is_empty());
                        self.state
                            .external_stack
                            .extend(&self.state.stack[start..start + count]);
                        self.state.stack.truncate(bottom);
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
                                self.state.stack[bottom + i] = self.state.stack[start + i]
                            }

                            for i in count..returning {
                                self.state.stack[bottom + i] = Value::Nil;
                            }

                            if expected_return.is_variable() {
                                self.state.stack.truncate(bottom + returning);
                                *is_variable = true;
                            } else {
                                self.state.stack.resize(*base + *stack_size, Value::Nil);
                                *is_variable = false;
                            }
                        }
                        Some(LuaReturn::Meta(meta_ind)) => {
                            let meta_ret = if count > 0 {
                                self.state.stack[start]
                            } else {
                                Value::Nil
                            };
                            self.state.stack.resize(*base + *stack_size, Value::Nil);
                            *is_variable = false;
                            if let Some(meta_ind) = meta_ind {
                                self.state.stack[*base + meta_ind.0 as usize] = meta_ret;
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
                            .extend(&self.state.stack[start..start + count]);
                        self.state.frames.push(Frame::HasResult);
                        self.state.stack.clear();
                    }
                    _ => panic!("lua frame must be above a sequence or lua frame"),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
        Ok(())
    }
}

impl<'gc, 'a> LuaRegisters<'gc, 'a> {
    pub(crate) fn open_upvalue(&mut self, mc: &Mutation<'gc>, reg: RegisterIndex) -> UpValue<'gc> {
        let ind = self.base + reg.0 as usize;
        match self.open_upvalues.entry(ind) {
            BTreeEntry::Occupied(occupied) => *occupied.get(),
            BTreeEntry::Vacant(vacant) => {
                let uv = UpValue(Gc::new(mc, Lock::new(UpValueState::Open(self.thread, ind))));
                vacant.insert(uv);
                uv
            }
        }
    }

    pub(crate) fn get_upvalue(&self, upvalue: UpValue<'gc>) -> Value<'gc> {
        match upvalue.0.get() {
            UpValueState::Open(upvalue_thread, ind) => {
                if upvalue_thread == self.thread {
                    if ind < self.base {
                        self.upper_stack[ind]
                    } else {
                        self.stack_frame[ind - self.base]
                    }
                } else {
                    upvalue_thread.0.borrow().stack[ind]
                }
            }
            UpValueState::Closed(v) => v,
        }
    }

    pub(crate) fn set_upvalue(
        &mut self,
        mc: &Mutation<'gc>,
        upvalue: UpValue<'gc>,
        value: Value<'gc>,
    ) {
        match upvalue.0.get() {
            UpValueState::Open(upvalue_thread, ind) => {
                if upvalue_thread == self.thread {
                    if ind < self.base {
                        self.upper_stack[ind] = value;
                    } else {
                        self.stack_frame[ind - self.base] = value;
                    }
                } else {
                    upvalue_thread.0.borrow_mut(mc).stack[ind] = value;
                }
            }
            UpValueState::Closed(v) => {
                upvalue.0.set(mc, UpValueState::Closed(v));
            }
        }
    }

    pub(crate) fn close_upvalues(&mut self, mc: &Mutation<'gc>, register: RegisterIndex) {
        for (_, upval) in self
            .open_upvalues
            .split_off(&(self.base + register.0 as usize))
        {
            if let UpValueState::Open(upvalue_thread, ind) = upval.0.get() {
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
        }
    }
}

impl<'gc> ThreadState<'gc> {
    fn mode(&self) -> ThreadMode {
        match self.frames.last() {
            None => {
                assert!(self.stack.is_empty() && self.open_upvalues.is_empty(),);
                ThreadMode::Stopped
            }
            Some(frame) => match frame {
                Frame::HasResult => ThreadMode::Result,
                Frame::Lua { .. } | Frame::Callback { .. } | Frame::Sequence { .. } => {
                    ThreadMode::Normal
                }
                Frame::Calling => ThreadMode::Running,
                Frame::StartCoroutine(_) | Frame::ResumeCoroutine => ThreadMode::Suspended,
            },
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
                let bottom = self.stack.len();
                let base = bottom + 1 + var_params;

                self.stack.resize(base + stack_size, Value::Nil);

                self.stack[bottom] = Value::Function(Function::Closure(closure));
                for i in 0..fixed_params {
                    self.stack[base + i] = self.external_stack.get(i);
                }
                for i in 0..var_params {
                    self.stack[bottom + 1 + i] = self.external_stack.get(fixed_params + i)
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

                    let bottom = self.stack.len();
                    self.stack.resize(bottom + return_len, Value::Nil);

                    for i in 0..return_len.min(self.external_stack.len()) {
                        self.stack[bottom + i] = self.external_stack.get(i);
                    }

                    self.external_stack.clear();

                    *is_variable = ret_count.is_variable();
                    if !ret_count.is_variable() {
                        self.stack.resize(*base + *stack_size, Value::Nil);
                    }
                }
                Some(LuaReturn::Meta(meta_ind)) => {
                    let meta_ret = self.external_stack.get(0);
                    self.external_stack.clear();
                    self.stack.resize(*base + *stack_size, Value::Nil);
                    *is_variable = false;
                    if let Some(meta_ind) = meta_ind {
                        self.stack[*base + meta_ind.0 as usize] = meta_ret;
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
                    self.stack.truncate(bottom);
                }
                Frame::Sequence(sequence) => {
                    self.frames.push(Frame::Sequence(sequence));
                    return;
                }
                _ => {}
            }
        }
        assert!(self.stack.is_empty());
        self.frames.push(Frame::HasResult);
    }

    fn return_ext(&mut self, ret: CallbackReturn<'gc>) {
        match ret {
            CallbackReturn::Return => match self.frames.last_mut() {
                Some(Frame::Sequence { .. }) => {}
                Some(Frame::Lua { .. }) => {
                    self.return_to_lua();
                }
                None => {
                    self.frames.push(Frame::HasResult);
                }
                _ => panic!("frame above callback must be sequence or lua frame"),
            },
            CallbackReturn::Sequence(sequence) => {
                self.frames.push(Frame::Sequence(sequence));
            }
            CallbackReturn::Yield(sequence) => {
                if let Some(sequence) = sequence {
                    self.frames.push(Frame::Sequence(sequence));
                }
                self.frames.push(Frame::ResumeCoroutine);
                self.frames.push(Frame::HasResult);
            }
            CallbackReturn::TailCall(function, sequence) => {
                if let Some(sequence) = sequence {
                    self.frames.push(Frame::Sequence(sequence));
                }
                self.ext_call_function(function);
            }
        }
    }

    fn close_upvalues(&mut self, mc: &Mutation<'gc>, bottom: usize) {
        for (_, upval) in self.open_upvalues.split_off(&bottom) {
            if let UpValueState::Open(upvalue_thread, ind) = upval.0.get() {
                assert!(upvalue_thread.0.as_ptr() == self);
                upval.0.set(mc, UpValueState::Closed(self.stack[ind]));
            }
        }
    }
}
