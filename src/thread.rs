use std::cell::RefMut;
use std::collections::btree_map::Entry as BTreeEntry;
use std::collections::BTreeMap;
use std::error::Error as StdError;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};

use gc_arena::{Collect, GcCell, MutationContext};
use gc_sequence::Sequence;

use crate::{
    vm::run_vm, CallbackResult, Closure, Continuation, Error, Function, RegisterIndex, TypeError,
    UpValue, UpValueState, Value, VarCount,
};

#[derive(Clone, Copy, Collect)]
#[collect(require_copy)]
pub struct Thread<'gc>(pub GcCell<'gc, ThreadState<'gc>>);

impl<'gc> Debug for Thread<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Thread")
            .field(&(&self.0 as *const _))
            .finish()
    }
}

impl<'gc> PartialEq for Thread<'gc> {
    fn eq(&self, other: &Thread<'gc>) -> bool {
        GcCell::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Hash for Thread<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        GcCell::as_ptr(self.0).hash(state)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadMode {
    // No frames are on the thread
    Stopped,
    // Thread has available results
    Results,
    // Lua code is currently executing
    Lua,
    // Waiting on the results of a callback
    Callback,
    // Currently executing a callback or continuation
    Running,
    // Waiting being resumed
    Suspended,
}

pub struct LuaFrame<'gc, 'a> {
    thread: Thread<'gc>,
    mutation_context: MutationContext<'gc, 'a>,
    state: RefMut<'a, ThreadState<'gc>>,
}

#[derive(Collect)]
#[collect(empty_drop)]
pub struct ThreadState<'gc> {
    values: Vec<Value<'gc>>,
    frames: Vec<Frame<'gc>>,
    open_upvalues: BTreeMap<usize, UpValue<'gc>>,
    result: Option<Result<Vec<Value<'gc>>, Error<'gc>>>,
    allow_yield: bool,
}

pub struct LuaRegisters<'gc, 'a> {
    pub pc: &'a mut usize,
    pub stack_frame: &'a mut [Value<'gc>],
    upper_stack: &'a mut [Value<'gc>],
    base: usize,
    open_upvalues: &'a mut BTreeMap<usize, UpValue<'gc>>,
    thread: Thread<'gc>,
    mutation_context: MutationContext<'gc, 'a>,
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_static)]
pub enum ThreadError {
    ExpectedVariable(bool),
    BadCall(TypeError),
    BadYield,
    BadMode {
        expected: Option<ThreadMode>,
        found: ThreadMode,
    },
}

impl StdError for ThreadError {}

impl fmt::Display for ThreadError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ThreadError::ExpectedVariable(true) => {
                write!(fmt, "operation expects variable lua thread")
            }
            ThreadError::ExpectedVariable(false) => {
                write!(fmt, "operation expects constant lua thread")
            }
            ThreadError::BadCall(type_error) => fmt::Display::fmt(type_error, fmt),
            ThreadError::BadYield => write!(fmt, "yield from unyieldable function"),
            ThreadError::BadMode { expected, found } => {
                write!(fmt, "bad thread mode: {:?}", found)?;
                if let Some(expected) = expected {
                    write!(fmt, " expected: {:?}", expected)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Collect)]
#[collect(empty_drop)]
pub struct ThreadSequence<'gc>(pub Thread<'gc>);

impl<'gc> ThreadSequence<'gc> {
    pub fn call_function(
        mc: MutationContext<'gc, '_>,
        thread: Thread<'gc>,
        function: Function<'gc>,
        args: &[Value<'gc>],
    ) -> Result<ThreadSequence<'gc>, ThreadError> {
        thread.start(mc, function, args)?;
        Ok(ThreadSequence(thread))
    }
}

impl<'gc> Sequence<'gc> for ThreadSequence<'gc> {
    type Output = Result<Vec<Value<'gc>>, Error<'gc>>;

    fn step(&mut self, mc: MutationContext<'gc, '_>) -> Option<Self::Output> {
        match self.0.mode() {
            ThreadMode::Results => self.0.take_results(mc),
            ThreadMode::Callback => {
                self.0.step_callback(mc).unwrap();
                None
            }
            ThreadMode::Lua => {
                const INSTRUCTION_GRANULARITY: u32 = 64;
                self.0.step_lua(mc, INSTRUCTION_GRANULARITY).unwrap();
                None
            }
            mode => Some(Err(ThreadError::BadMode {
                expected: None,
                found: mode,
            }
            .into())),
        }
    }
}

impl<'gc> Thread<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>, allow_yield: bool) -> Thread<'gc> {
        Thread(GcCell::allocate(
            mc,
            ThreadState {
                values: Vec::new(),
                frames: Vec::new(),
                open_upvalues: BTreeMap::new(),
                result: None,
                allow_yield,
            },
        ))
    }

    pub fn mode(self) -> ThreadMode {
        if let Ok(state) = self.0.try_read() {
            get_mode(&state)
        } else {
            ThreadMode::Running
        }
    }

    // If this thread is `Stopped`, start a new function with the given arguments.
    pub fn start(
        self,
        mc: MutationContext<'gc, '_>,
        function: Function<'gc>,
        args: &[Value<'gc>],
    ) -> Result<(), ThreadError> {
        let mut state = self.0.write(mc);
        check_mode(&state, ThreadMode::Stopped)?;
        ext_call_function(&mut state, function, args);
        Ok(())
    }

    // If this thread is `Stopped`, start a new suspended function.
    pub fn start_suspended(
        self,
        mc: MutationContext<'gc, '_>,
        function: Function<'gc>,
    ) -> Result<(), ThreadError> {
        let mut state = self.0.write(mc);
        check_mode(&state, ThreadMode::Stopped)?;
        state.frames.push(Frame::StartCoroutine(function));
        Ok(())
    }

    // Take any results if they are available
    pub fn take_results(
        self,
        mc: MutationContext<'gc, '_>,
    ) -> Option<Result<Vec<Value<'gc>>, Error<'gc>>> {
        self.0.write(mc).result.take()
    }

    // If the thread is in `Suspended` mode, resume it.
    pub fn resume(
        self,
        mc: MutationContext<'gc, '_>,
        args: &[Value<'gc>],
    ) -> Result<(), ThreadError> {
        let mut state = self.0.write(mc);
        check_mode(&state, ThreadMode::Suspended)?;
        match state.frames.pop() {
            Some(Frame::StartCoroutine(function)) => {
                state.frames.pop();
                assert!(
                    state.values.is_empty()
                        && state.open_upvalues.is_empty()
                        && state.frames.is_empty()
                        && state.result.is_none()
                );
                ext_call_function(&mut state, function, args);
            }
            Some(Frame::ResumeCoroutine) => match state.frames.last_mut() {
                Some(Frame::Continuation { continuation, .. }) => {
                    let continuation = continuation.take().expect("continuation missing");
                    let seq = continuation.call(Ok(args.to_vec()));
                    state.frames.pop();
                    state.frames.push(Frame::Callback(Some(seq)));
                }
                Some(Frame::Lua { .. }) => {
                    return_to_lua(&mut state, args);
                }
                None => {
                    state.result = Some(Ok(args.to_vec()));
                }
                _ => panic!("resume coroutine frame must be above a continuation or lua frame"),
            },
            _ => panic!("no suspended coroutine frame"),
        }
        Ok(())
    }

    // If the thread is in `Callback` mode, step the callback
    pub fn step_callback(self, mc: MutationContext<'gc, '_>) -> Result<(), ThreadError> {
        let mut state = self.0.write(mc);
        check_mode(&state, ThreadMode::Callback)?;
        match state.frames.last_mut() {
            Some(Frame::Callback(sequence)) => {
                let mut sequence = sequence.take().expect("pending callback missing");
                drop(state);
                match sequence.step(mc) {
                    None => {
                        let mut state = self.0.write(mc);
                        match state.frames.last_mut() {
                            Some(Frame::Callback(empty_sequence)) => {
                                *empty_sequence = Some(sequence);
                            }
                            _ => panic!("thread left callback state without finishing callback"),
                        }
                    }
                    Some(res) => {
                        let mut state = self.0.write(mc);
                        state.frames.pop();
                        match res {
                            Err(err) => {
                                unwind(self, &mut state, mc, err);
                            }
                            Ok(CallbackResult::Yield(res)) => {
                                if state.allow_yield {
                                    state.frames.push(Frame::ResumeCoroutine);
                                    state.result = Some(Ok(res));
                                } else {
                                    unwind(self, &mut state, mc, ThreadError::BadYield.into());
                                }
                            }
                            Ok(CallbackResult::Return(res)) => match state.frames.last_mut() {
                                Some(Frame::Continuation { continuation, .. }) => {
                                    let continuation =
                                        continuation.take().expect("continuation missing");
                                    let seq = continuation.call(Ok(res));
                                    state.frames.pop();
                                    state.frames.push(Frame::Callback(Some(seq)));
                                }
                                Some(Frame::Lua { .. }) => {
                                    return_to_lua(&mut state, &res);
                                }
                                None => {
                                    state.result = Some(Ok(res));
                                }
                                _ => {
                                    panic!("frame above callback must be continuation or lua frame")
                                }
                            },
                            Ok(CallbackResult::TailCall {
                                function,
                                args,
                                continuation,
                            }) => {
                                let bottom = state.values.len();
                                state.frames.push(Frame::Continuation {
                                    continuation: Some(continuation),
                                    bottom,
                                });
                                ext_call_function(&mut state, function, &args);
                            }
                        }
                    }
                }
            }
            _ => panic!("no callback frame"),
        }
        Ok(())
    }

    // If the thread is in `Lua` mode, run the VM for the given number of instructions, or until the
    // Thread transitions out of `Lua` mode.
    pub fn step_lua(
        self,
        mc: MutationContext<'gc, '_>,
        mut instructions: u32,
    ) -> Result<(), ThreadError> {
        check_mode(&self.0.read(), ThreadMode::Lua)?;
        loop {
            let state = self.0.write(mc);
            match state.frames.last() {
                Some(Frame::Lua { .. }) => {
                    let lua_frame = LuaFrame {
                        state,
                        mutation_context: mc,
                        thread: self,
                    };
                    match run_vm(mc, lua_frame, instructions) {
                        Err(err) => {
                            let mut state = self.0.write(mc);
                            unwind(self, &mut state, mc, err);
                            break;
                        }
                        Ok(i) => {
                            if i == 0 {
                                break;
                            }
                            instructions = i;
                        }
                    }
                }
                _ => break,
            }
        }
        Ok(())
    }
}

impl<'gc, 'a> LuaFrame<'gc, 'a> {
    pub fn closure(&self) -> Closure<'gc> {
        match self.state.frames.last() {
            Some(Frame::Lua { bottom, .. }) => match self.state.values[*bottom] {
                Value::Function(Function::Closure(c)) => c,
                _ => panic!("thread bottom is not a closure"),
            },
            _ => panic!("top frame is not lua frame"),
        }
    }

    // returns a view of the Lua frame's registers
    pub fn registers<'b>(&'b mut self) -> LuaRegisters<'gc, 'b> {
        let state: &mut ThreadState<'gc> = &mut self.state;
        match state.frames.last_mut() {
            Some(Frame::Lua { base, pc, .. }) => {
                let (upper_stack, stack_frame) = state.values.split_at_mut(*base);
                LuaRegisters {
                    pc,
                    stack_frame,
                    upper_stack,
                    base: *base,
                    open_upvalues: &mut state.open_upvalues,
                    thread: self.thread,
                    mutation_context: self.mutation_context,
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Place the current frame's varargs at the given register, expecting the given count
    pub fn varargs(&mut self, dest: RegisterIndex, count: VarCount) -> Result<(), ThreadError> {
        let state: &mut ThreadState<'gc> = &mut self.state;
        match state.frames.last_mut() {
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
                        state.values[dest + i] = if i < varargs_len {
                            state.values[varargs_start + i]
                        } else {
                            Value::Nil
                        };
                    }
                } else {
                    *is_variable = true;
                    state.values.resize(dest + varargs_len, Value::Nil);
                    for i in 0..varargs_len {
                        state.values[dest + i] = state.values[varargs_start + i];
                    }
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
        Ok(())
    }

    // Call the function at the given register with the given arguments.  On return, results will be
    // placed starting at the function register.
    pub fn call_function(
        mut self,
        func: RegisterIndex,
        args: VarCount,
        returns: VarCount,
    ) -> Result<(), ThreadError> {
        let state: &mut ThreadState<'gc> = &mut self.state;
        match state.frames.last_mut() {
            Some(Frame::Lua {
                expected_returns,
                is_variable,
                base,
                ..
            }) => {
                if *is_variable != args.is_variable() {
                    return Err(ThreadError::ExpectedVariable(*is_variable));
                }

                *expected_returns = Some(returns);
                let function_index = *base + func.0 as usize;
                let arg_count = args
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(state.values.len() - function_index - 1);

                match state.values[function_index] {
                    Value::Function(Function::Closure(closure)) => {
                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if arg_count > fixed_params {
                            state.values.truncate(function_index + 1 + arg_count);
                            state.values[function_index + 1..].rotate_left(fixed_params);
                            function_index + 1 + (arg_count - fixed_params)
                        } else {
                            function_index + 1
                        };

                        state.values.resize(base + stack_size, Value::Nil);

                        state.frames.push(Frame::Lua {
                            bottom: function_index,
                            base,
                            is_variable: false,
                            pc: 0,
                            stack_size,
                            expected_returns: None,
                        });
                        Ok(())
                    }
                    Value::Function(Function::Callback(callback)) => {
                        let seq = callback.call(
                            state.values[function_index + 1..function_index + 1 + arg_count]
                                .to_vec(),
                        );
                        state.frames.push(Frame::Callback(Some(seq)));
                        state.values.resize(function_index, Value::Nil);
                        Ok(())
                    }
                    val => Err(ThreadError::BadCall(TypeError {
                        expected: "function",
                        found: val.type_name(),
                    })),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Calls the function at the given index with a constant number of arguments without
    // invalidating the function or its arguments.  Returns are placed *after* the function and its
    // aruments, and all registers past this are invalidated as normal.
    pub fn call_function_non_destructive(
        mut self,
        func: RegisterIndex,
        arg_count: u8,
        returns: VarCount,
    ) -> Result<(), ThreadError> {
        let state: &mut ThreadState<'gc> = &mut self.state;
        match state.frames.last_mut() {
            Some(Frame::Lua {
                expected_returns,
                is_variable,
                base,
                ..
            }) => {
                if *is_variable {
                    return Err(ThreadError::ExpectedVariable(false));
                }

                let arg_count = arg_count as usize;
                *expected_returns = Some(returns);
                let given_function_index = *base + func.0 as usize;
                let function_index = given_function_index + 1 + arg_count;
                state
                    .values
                    .resize(function_index + 1 + arg_count, Value::Nil);
                for i in 0..arg_count + 1 {
                    state.values[function_index + i] = state.values[given_function_index + i];
                }

                match state.values[function_index] {
                    Value::Function(Function::Closure(closure)) => {
                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if arg_count > fixed_params {
                            state.values[function_index + 1..].rotate_left(fixed_params);
                            function_index + 1 + (arg_count - fixed_params)
                        } else {
                            function_index + 1
                        };

                        state.values.resize(base + stack_size, Value::Nil);

                        state.frames.push(Frame::Lua {
                            bottom: function_index,
                            base,
                            is_variable: false,
                            pc: 0,
                            stack_size,
                            expected_returns: None,
                        });
                        Ok(())
                    }
                    Value::Function(Function::Callback(callback)) => {
                        let seq = callback.call(
                            state.values[function_index + 1..function_index + 1 + arg_count]
                                .to_vec(),
                        );
                        state.frames.push(Frame::Callback(Some(seq)));
                        state.values.resize(function_index, Value::Nil);
                        Ok(())
                    }
                    val => Err(ThreadError::BadCall(TypeError {
                        expected: "function",
                        found: val.type_name(),
                    })),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Tail-call the function at the given register with the given arguments.  Pops the current Lua
    // frame, pushing a new frame for the given function.
    pub fn tail_call_function(
        mut self,
        func: RegisterIndex,
        args: VarCount,
    ) -> Result<(), ThreadError> {
        let state: &mut ThreadState<'gc> = &mut self.state;
        match state.frames.pop() {
            Some(Frame::Lua {
                bottom,
                base,
                is_variable,
                ..
            }) => {
                if is_variable != args.is_variable() {
                    return Err(ThreadError::ExpectedVariable(is_variable));
                }

                close_upvalues(self.thread, state, self.mutation_context, bottom);

                let function_index = base + func.0 as usize;
                let arg_count = args
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(state.values.len() - function_index - 1);

                match state.values[function_index] {
                    Value::Function(Function::Closure(closure)) => {
                        state.values[bottom] = state.values[function_index];
                        for i in 0..arg_count {
                            state.values[bottom + 1 + i] = state.values[function_index + 1 + i];
                        }

                        let fixed_params = closure.0.proto.fixed_params as usize;
                        let stack_size = closure.0.proto.stack_size as usize;

                        let base = if arg_count > fixed_params {
                            state.values.truncate(bottom + 1 + arg_count);
                            state.values[bottom + 1..].rotate_left(fixed_params);
                            bottom + 1 + (arg_count - fixed_params)
                        } else {
                            bottom + 1
                        };

                        state.values.resize(base + stack_size, Value::Nil);

                        state.frames.push(Frame::Lua {
                            bottom,
                            base,
                            is_variable: false,
                            pc: 0,
                            stack_size,
                            expected_returns: None,
                        });
                        Ok(())
                    }
                    Value::Function(Function::Callback(callback)) => {
                        let seq = callback.call(
                            state.values[function_index + 1..function_index + 1 + arg_count]
                                .to_vec(),
                        );
                        state.frames.push(Frame::Callback(Some(seq)));
                        state.values.truncate(bottom);
                        Ok(())
                    }
                    val => Err(ThreadError::BadCall(TypeError {
                        expected: "function",
                        found: val.type_name(),
                    })),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
    }

    // Return to the upper frame with results starting at the given register index.
    pub fn return_upper(
        mut self,
        start: RegisterIndex,
        count: VarCount,
    ) -> Result<(), ThreadError> {
        let state: &mut ThreadState<'gc> = &mut self.state;
        match state.frames.pop() {
            Some(Frame::Lua {
                bottom,
                base,
                is_variable,
                ..
            }) => {
                if is_variable != count.is_variable() {
                    return Err(ThreadError::ExpectedVariable(is_variable));
                }
                close_upvalues(self.thread, state, self.mutation_context, bottom);

                let start = base + start.0 as usize;
                let count = count
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(state.values.len() - start);

                match state.frames.last_mut() {
                    Some(Frame::Continuation { continuation, .. }) => {
                        let continuation = continuation.take().expect("continuation missing");
                        let ret_vals = state.values[start..start + count].to_vec();
                        state.values.truncate(bottom);
                        let seq = continuation.call(Ok(ret_vals));
                        state.frames.pop();
                        state.frames.push(Frame::Callback(Some(seq)));
                    }
                    Some(Frame::Lua {
                        expected_returns,
                        is_variable,
                        base,
                        stack_size,
                        ..
                    }) => {
                        let expected_returns =
                            expected_returns.expect("no expected returns for upper lua frame");
                        let returning = expected_returns
                            .to_constant()
                            .map(|c| c as usize)
                            .unwrap_or(count);

                        for i in 0..returning.min(count) {
                            state.values[bottom + i] = state.values[start + i]
                        }

                        for i in count..returning {
                            state.values[bottom + i] = Value::Nil;
                        }

                        if expected_returns.is_variable() {
                            state.values.truncate(bottom + returning);
                            *is_variable = true;
                        } else {
                            state.values.resize(*base + *stack_size, Value::Nil);
                            *is_variable = false;
                        }
                    }
                    None => {
                        let ret_vals = state.values[start..start + count].to_vec();
                        state.result = Some(Ok(ret_vals));
                        state.values.clear();
                    }
                    _ => panic!("lua frame must be above a continuation or lua frame"),
                }
            }
            _ => panic!("top frame is not lua frame"),
        }
        Ok(())
    }
}

impl<'gc, 'a> LuaRegisters<'gc, 'a> {
    pub fn open_upvalue(&mut self, reg: RegisterIndex) -> UpValue<'gc> {
        let ind = self.base + reg.0 as usize;
        match self.open_upvalues.entry(ind) {
            BTreeEntry::Occupied(occupied) => *occupied.get(),
            BTreeEntry::Vacant(vacant) => {
                let uv = UpValue(GcCell::allocate(
                    self.mutation_context,
                    UpValueState::Open(self.thread, ind),
                ));
                vacant.insert(uv);
                uv
            }
        }
    }

    pub fn get_upvalue(&self, upvalue: UpValue<'gc>) -> Value<'gc> {
        match *upvalue.0.read() {
            UpValueState::Open(thread, ind) => {
                if thread == self.thread {
                    if ind < self.base {
                        self.upper_stack[ind]
                    } else {
                        self.stack_frame[ind - self.base]
                    }
                } else {
                    thread.0.read().values[ind]
                }
            }
            UpValueState::Closed(v) => v,
        }
    }

    pub fn set_upvalue(&mut self, upvalue: UpValue<'gc>, value: Value<'gc>) {
        let mut uv = upvalue.0.write(self.mutation_context);
        match &mut *uv {
            UpValueState::Open(thread, ind) => {
                if *thread == self.thread {
                    if *ind < self.base {
                        self.upper_stack[*ind] = value;
                    } else {
                        self.stack_frame[*ind - self.base] = value;
                    }
                } else {
                    thread.0.write(self.mutation_context).values[*ind] = value;
                }
            }
            UpValueState::Closed(v) => *v = value,
        }
    }

    pub fn close_upvalues(&mut self, register: RegisterIndex) {
        for (_, upval) in self
            .open_upvalues
            .split_off(&(self.base + register.0 as usize))
        {
            let mut upval = upval.0.write(self.mutation_context);
            if let UpValueState::Open(upvalue_thread, ind) = *upval {
                assert!(upvalue_thread == self.thread);
                *upval = UpValueState::Closed(if ind < self.base {
                    self.upper_stack[ind]
                } else {
                    self.stack_frame[ind - self.base]
                });
            }
        }
    }
}

#[derive(Collect)]
#[collect(empty_drop)]
enum Frame<'gc> {
    Lua {
        bottom: usize,
        base: usize,
        is_variable: bool,
        pc: usize,
        stack_size: usize,
        expected_returns: Option<VarCount>,
    },
    Continuation {
        bottom: usize,
        continuation: Option<Continuation<'gc>>,
    },
    StartCoroutine(Function<'gc>),
    ResumeCoroutine,
    Callback(
        Option<Box<dyn Sequence<'gc, Output = Result<CallbackResult<'gc>, Error<'gc>>> + 'gc>>,
    ),
}

fn get_mode<'gc>(state: &ThreadState<'gc>) -> ThreadMode {
    if state.result.is_some() {
        ThreadMode::Results
    } else {
        match state.frames.last() {
            None => {
                assert!(
                    state.values.is_empty()
                        && state.open_upvalues.is_empty()
                        && state.result.is_none(),
                );
                ThreadMode::Stopped
            }
            Some(Frame::Lua { .. }) => ThreadMode::Lua,
            Some(Frame::Continuation { .. }) => ThreadMode::Running,
            Some(Frame::StartCoroutine(_)) | Some(Frame::ResumeCoroutine) => ThreadMode::Suspended,
            Some(Frame::Callback(_)) => ThreadMode::Callback,
        }
    }
}

fn check_mode<'gc>(state: &ThreadState<'gc>, expected: ThreadMode) -> Result<(), ThreadError> {
    let found = get_mode(state);
    if found != expected {
        Err(ThreadError::BadMode {
            expected: Some(expected),
            found,
        })
    } else {
        Ok(())
    }
}

fn ext_call_function<'gc>(
    state: &mut ThreadState<'gc>,
    function: Function<'gc>,
    args: &[Value<'gc>],
) {
    match function {
        Function::Closure(closure) => {
            let fixed_params = closure.0.proto.fixed_params as usize;
            let stack_size = closure.0.proto.stack_size as usize;

            let var_params = if args.len() > fixed_params {
                args.len() - fixed_params
            } else {
                0
            };
            let bottom = state.values.len();
            let base = bottom + 1 + var_params;

            state.values.resize(base + stack_size, Value::Nil);

            state.values[bottom] = Value::Function(Function::Closure(closure));
            for i in 0..fixed_params {
                state.values[base + i] = args.get(i).cloned().unwrap_or(Value::Nil);
            }
            for i in 0..var_params {
                state.values[1 + i] = args[fixed_params + i]
            }

            state.frames.push(Frame::Lua {
                bottom,
                base,
                is_variable: false,
                pc: 0,
                stack_size,
                expected_returns: None,
            });
        }
        Function::Callback(callback) => {
            state
                .frames
                .push(Frame::Callback(Some(callback.call(args.to_vec()))));
        }
    }
}

// Return to the top Lua frame from an external call
fn return_to_lua<'gc>(state: &mut ThreadState<'gc>, rets: &[Value<'gc>]) {
    match state.frames.last_mut() {
        Some(Frame::Lua {
            expected_returns,
            is_variable,
            base,
            stack_size,
            ..
        }) => {
            let ret_count = expected_returns
                .take()
                .expect("no expected returns for lua frame");
            let return_len = ret_count
                .to_constant()
                .map(|c| c as usize)
                .unwrap_or(rets.len());

            let bottom = state.values.len();
            state.values.resize(bottom + return_len, Value::Nil);

            for i in 0..return_len.min(rets.len()) {
                state.values[bottom + i] = rets[i];
            }

            *is_variable = ret_count.is_variable();
            if !ret_count.is_variable() {
                state.values.resize(*base + *stack_size, Value::Nil);
            }
        }
        _ => panic!("no lua frame to return to"),
    };
}

fn unwind<'gc>(
    thread: Thread<'gc>,
    state: &mut ThreadState<'gc>,
    mc: MutationContext<'gc, '_>,
    error: Error<'gc>,
) {
    while let Some(mut top_frame) = state.frames.pop() {
        if let Frame::Continuation {
            continuation,
            bottom,
        } = &mut top_frame
        {
            close_upvalues(thread, state, mc, *bottom);
            state.values.truncate(*bottom);
            let continuation = continuation.take().expect("missing continuation");
            let seq = continuation.call(Err(error));
            state.frames.push(Frame::Callback(Some(seq)));
            return;
        }
    }
    close_upvalues(thread, state, mc, 0);
    state.values.clear();
    state.result = Some(Err(error));
}

fn close_upvalues<'gc>(
    thread: Thread<'gc>,
    state: &mut ThreadState<'gc>,
    mc: MutationContext<'gc, '_>,
    bottom: usize,
) {
    for (_, upval) in state.open_upvalues.split_off(&bottom) {
        let mut upval = upval.0.write(mc);
        if let UpValueState::Open(upvalue_thread, ind) = *upval {
            assert!(upvalue_thread == thread);
            *upval = UpValueState::Closed(state.values[ind]);
        }
    }
}
