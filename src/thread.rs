use std::collections::btree_map::Entry as BTreeEntry;
use std::collections::BTreeMap;
use std::error::Error as StdError;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::mem;

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::{
    callback::CallbackSequenceBox, CallbackResult, Closure, ClosureState, Continuation, Error,
    Function, IntoSequence, LuaContext, OpCode, Sequence, String, Table, TypeError, UpValue,
    UpValueDescriptor, UpValueState, Value, VarCount,
};

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_static)]
pub enum ThreadError {
    BadYield,
}

impl StdError for ThreadError {}

impl fmt::Display for ThreadError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ThreadError::BadYield => write!(fmt, "yield from unyieldable function"),
        }
    }
}

#[derive(Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Thread<'gc>(pub GcCell<'gc, ThreadState<'gc>>);

#[derive(Collect)]
#[collect(empty_drop)]
pub struct ThreadState<'gc> {
    stack: Vec<Value<'gc>>,
    frames: Vec<Frame>,
    open_upvalues: BTreeMap<usize, UpValue<'gc>>,
    yieldable: bool,
    pending: Pending<'gc>,
}

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

impl<'gc> Thread<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Thread<'gc> {
        Thread(GcCell::allocate(
            mc,
            ThreadState {
                stack: Vec::new(),
                frames: Vec::new(),
                open_upvalues: BTreeMap::new(),
                yieldable: false,
                pending: Pending::None,
            },
        ))
    }

    /// Create a suspended thread that, once resumed, executes the given function.
    pub fn new_coroutine(mc: MutationContext<'gc, '_>, function: Function<'gc>) -> Thread<'gc> {
        Thread(GcCell::allocate(
            mc,
            ThreadState {
                stack: vec![],
                frames: vec![],
                open_upvalues: BTreeMap::new(),
                yieldable: true,
                pending: Pending::StartCoroutine(function),
            },
        ))
    }

    /// Call a function on this thread, producing a `Sequence`.
    ///
    /// A `Thread` can only have one active call Sequence at a time, `Thread::is_finished` must
    /// return true before `Thread::call` can be called again.  Returns None if the `Thread` is not
    /// in finished.
    pub fn call(
        self,
        mc: MutationContext<'gc, '_>,
        function: Function<'gc>,
        args: &[Value<'gc>],
    ) -> Option<impl Sequence<'gc, Item = Vec<Value<'gc>>, Error = Error<'gc>> + 'gc> {
        let mut state = self.0.write(mc);
        if state.frames.is_empty() {
            state.stack.push(Value::Function(function));
            state.stack.extend(args);
            self.call_function(
                &mut state,
                0,
                VarCount::variable(),
                VarCount::variable(),
                Vec::new(),
            )
            .unwrap();
            Some(ThreadSequence(self))
        } else {
            None
        }
    }

    /// Returns true if the thread has finished its main function
    pub fn is_finished(self) -> bool {
        if let Ok(state) = self.0.try_read() {
            state.frames.is_empty()
        } else {
            false
        }
    }

    /// Returns true if the given thread is suspended in a call to yield, and is waiting on being
    /// resumed.
    pub fn is_suspended(self) -> bool {
        if let Ok(state) = self.0.try_read() {
            match &state.pending {
                Pending::None => false,
                Pending::Working => false,
                Pending::Callback { .. } => false,
                Pending::StartCoroutine(_) => true,
                Pending::ResumeCoroutine { .. } => true,
            }
        } else {
            false
        }
    }

    /// Returns true if this thread is currently executing a callback
    pub fn is_running(self) -> bool {
        if let Ok(state) = self.0.try_read() {
            match &state.pending {
                Pending::Working => true,
                _ => false,
            }
        } else {
            true
        }
    }

    /// If this thread is suspended, returns a sequence to resume the thread, otherwise returns
    /// None.
    pub fn resume(
        self,
        mc: MutationContext<'gc, '_>,
        args: Vec<Value<'gc>>,
    ) -> Option<Box<Sequence<'gc, Item = Vec<Value<'gc>>, Error = Error<'gc>> + 'gc>> {
        let mut state = self.0.write(mc);
        let state: &mut ThreadState<'gc> = &mut state;
        match &mut state.pending {
            Pending::None => None,
            Pending::Working => None,
            Pending::Callback { .. } => None,
            Pending::StartCoroutine(function) => {
                assert!(state.frames.is_empty() && state.stack.is_empty());
                state.stack.push(Value::Function(*function));
                state.stack.extend(args);
                state.pending = Pending::None;
                self.call_function(
                    state,
                    0,
                    VarCount::variable(),
                    VarCount::variable(),
                    Vec::new(),
                )
                .unwrap();
                Some(Box::new(ThreadSequence(self)))
            }
            Pending::ResumeCoroutine {
                returns,
                continuations,
            } => {
                let returns = *returns;
                if let Some(cont) = continuations.pop() {
                    let continuations = mem::replace(continuations, Vec::new());
                    let callback = cont.call(Ok(args));
                    state.pending = Pending::Callback {
                        callback,
                        returns,
                        continuations,
                    };
                    Some(Box::new(ThreadSequence(self)))
                } else {
                    state.pending = Pending::None;
                    if state.frames.is_empty() {
                        Some(Box::new(Ok(args).into_sequence()))
                    } else {
                        self.return_to_lua(state, &args, returns);
                        Some(Box::new(ThreadSequence(self)))
                    }
                }
            }
        }
    }

    pub fn is_yieldable(self) -> bool {
        if let Ok(state) = self.0.try_read() {
            state.yieldable
        } else {
            false
        }
    }

    fn step(
        self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Vec<Value<'gc>>, Error<'gc>>> {
        let mut pending = mem::replace(&mut self.0.write(mc).pending, Pending::Working);
        match &mut pending {
            Pending::None => {
                self.0.write(mc).pending = Pending::None;

                match self.step_lua(mc) {
                    Err(err) => {
                        if let Some(err) = self.unwind(mc, err) {
                            Some(Err(err))
                        } else {
                            None
                        }
                    }
                    Ok(Some(res)) => Some(Ok(res)),
                    Ok(None) => None,
                }
            }
            Pending::Working => panic!("recursive thread step"),
            Pending::StartCoroutine(_) | Pending::ResumeCoroutine { .. } => {
                panic!("cannot step suspended thread")
            }
            Pending::Callback {
                callback,
                returns,
                continuations,
            } => {
                let res = callback.step(mc, lc);

                match res {
                    None => {
                        let mut state = self.0.write(mc);
                        mem::replace(&mut state.pending, pending);
                        None
                    }
                    Some(res) => match res {
                        Err(err) => {
                            if let Some(cont) = continuations.pop() {
                                let mut state = self.0.write(mc);
                                let continuations = mem::replace(continuations, Vec::new());
                                let returns = *returns;
                                let callback = cont.call(Err(err));
                                state.pending = Pending::Callback {
                                    callback,
                                    returns,
                                    continuations,
                                };
                                None
                            } else {
                                self.0.write(mc).pending = Pending::None;
                                if let Some(err) = self.unwind(mc, err) {
                                    Some(Err(err))
                                } else {
                                    None
                                }
                            }
                        }
                        Ok(CallbackResult::Yield(res)) => {
                            let mut state = self.0.write(mc);
                            state.pending = Pending::ResumeCoroutine {
                                returns: *returns,
                                continuations: mem::replace(continuations, Vec::new()),
                            };
                            Some(Ok(res))
                        }
                        Ok(CallbackResult::Return(res)) => {
                            let mut state = self.0.write(mc);
                            if let Some(cont) = continuations.pop() {
                                let continuations = mem::replace(continuations, Vec::new());
                                let returns = *returns;
                                let callback = cont.call(Ok(res));
                                state.pending = Pending::Callback {
                                    callback,
                                    returns,
                                    continuations,
                                };
                                None
                            } else {
                                state.pending = Pending::None;
                                if state.frames.is_empty() {
                                    Some(Ok(res))
                                } else {
                                    self.return_to_lua(&mut state, &res, *returns);
                                    None
                                }
                            }
                        }
                        Ok(CallbackResult::TailCall {
                            function,
                            args,
                            continuation,
                        }) => {
                            let mut state = self.0.write(mc);
                            state.pending = Pending::None;
                            let function_index = state.stack.len();
                            state.stack.push(Value::Function(function));
                            state.stack.extend(args);
                            continuations.push(continuation);
                            self.call_function(
                                &mut state,
                                function_index,
                                VarCount::variable(),
                                *returns,
                                mem::replace(continuations, Vec::new()),
                            )
                            .unwrap();
                            None
                        }
                    },
                }
            }
        }
    }

    fn step_lua(self, mc: MutationContext<'gc, '_>) -> Result<Option<Vec<Value<'gc>>>, Error<'gc>> {
        let mut state = self.0.write(mc);
        let state: &mut ThreadState<'gc> = &mut state;

        const THREAD_GRANULARITY: u32 = 64;
        let mut instructions = THREAD_GRANULARITY;

        'start: loop {
            let current_frame = state
                .frames
                .last_mut()
                .expect("no current ThreadState frame");
            let stack_bottom = current_frame.bottom;
            let stack_base = current_frame.base;
            let pc = &mut current_frame.pc;
            let returns = current_frame.returns;
            let current_function = match state.stack[stack_bottom] {
                Value::Function(Function::Closure(c)) => c,
                _ => panic!("stack bottom is not a closure"),
            };

            let (upper_stack, stack_frame) = state.stack.split_at_mut(stack_base);

            loop {
                let op = current_function.0.proto.opcodes[*pc];
                *pc += 1;

                match op {
                    OpCode::Move { dest, source } => {
                        stack_frame[dest.0 as usize] = stack_frame[source.0 as usize];
                    }

                    OpCode::LoadConstant { dest, constant } => {
                        stack_frame[dest.0 as usize] =
                            current_function.0.proto.constants[constant.0 as usize].to_value();
                    }

                    OpCode::LoadBool {
                        dest,
                        value,
                        skip_next,
                    } => {
                        stack_frame[dest.0 as usize] = Value::Boolean(value);
                        if skip_next {
                            *pc += 1;
                        }
                    }

                    OpCode::LoadNil { dest, count } => {
                        for i in dest.0..dest.0 + count {
                            stack_frame[i as usize] = Value::Nil;
                        }
                    }

                    OpCode::NewTable { dest } => {
                        stack_frame[dest.0 as usize] = Value::Table(Table::new(mc));
                    }

                    OpCode::GetTableR { dest, table, key } => {
                        stack_frame[dest.0 as usize] = get_table(stack_frame[table.0 as usize])?
                            .get(stack_frame[key.0 as usize]);
                    }

                    OpCode::GetTableC { dest, table, key } => {
                        stack_frame[dest.0 as usize] = get_table(stack_frame[table.0 as usize])?
                            .get(current_function.0.proto.constants[key.0 as usize].to_value())
                    }

                    OpCode::SetTableRR { table, key, value } => {
                        get_table(stack_frame[table.0 as usize])?
                            .set(
                                mc,
                                stack_frame[key.0 as usize],
                                stack_frame[value.0 as usize],
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableRC { table, key, value } => {
                        get_table(stack_frame[table.0 as usize])?
                            .set(
                                mc,
                                stack_frame[key.0 as usize],
                                current_function.0.proto.constants[value.0 as usize].to_value(),
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableCR { table, key, value } => {
                        get_table(stack_frame[table.0 as usize])?
                            .set(
                                mc,
                                current_function.0.proto.constants[key.0 as usize].to_value(),
                                stack_frame[value.0 as usize],
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableCC { table, key, value } => {
                        get_table(stack_frame[table.0 as usize])?
                            .set(
                                mc,
                                current_function.0.proto.constants[key.0 as usize].to_value(),
                                current_function.0.proto.constants[value.0 as usize].to_value(),
                            )
                            .expect("could not set table value");
                    }

                    OpCode::GetUpTableR { dest, table, key } => {
                        stack_frame[dest.0 as usize] = get_table(get_upvalue(
                            self,
                            upper_stack,
                            current_function.0.upvalues[table.0 as usize],
                        ))?
                        .get(stack_frame[key.0 as usize]);
                    }

                    OpCode::GetUpTableC { dest, table, key } => {
                        stack_frame[dest.0 as usize] = get_table(get_upvalue(
                            self,
                            upper_stack,
                            current_function.0.upvalues[table.0 as usize],
                        ))?
                        .get(current_function.0.proto.constants[key.0 as usize].to_value())
                    }

                    OpCode::SetUpTableRR { table, key, value } => {
                        get_table(get_upvalue(
                            self,
                            upper_stack,
                            current_function.0.upvalues[table.0 as usize],
                        ))?
                        .set(
                            mc,
                            stack_frame[key.0 as usize],
                            stack_frame[value.0 as usize],
                        )
                        .expect("could not set table value");
                    }

                    OpCode::SetUpTableRC { table, key, value } => {
                        get_table(get_upvalue(
                            self,
                            upper_stack,
                            current_function.0.upvalues[table.0 as usize],
                        ))?
                        .set(
                            mc,
                            stack_frame[key.0 as usize],
                            current_function.0.proto.constants[value.0 as usize].to_value(),
                        )
                        .expect("could not set table value");
                    }

                    OpCode::SetUpTableCR { table, key, value } => {
                        get_table(get_upvalue(
                            self,
                            upper_stack,
                            current_function.0.upvalues[table.0 as usize],
                        ))?
                        .set(
                            mc,
                            current_function.0.proto.constants[key.0 as usize].to_value(),
                            stack_frame[value.0 as usize],
                        )
                        .expect("could not set table value");
                    }

                    OpCode::SetUpTableCC { table, key, value } => {
                        get_table(get_upvalue(
                            self,
                            upper_stack,
                            current_function.0.upvalues[table.0 as usize],
                        ))?
                        .set(
                            mc,
                            current_function.0.proto.constants[key.0 as usize].to_value(),
                            current_function.0.proto.constants[value.0 as usize].to_value(),
                        )
                        .expect("could not set table value");
                    }

                    OpCode::Call {
                        func,
                        args,
                        returns,
                    } => {
                        if self.call_function(
                            state,
                            stack_base + func.0 as usize,
                            args,
                            returns,
                            Vec::new(),
                        )? {
                            return Ok(None);
                        } else {
                            continue 'start;
                        }
                    }

                    OpCode::TailCall { func, args } => {
                        self.close_upvalues(state, mc, stack_bottom);

                        let func = stack_base + func.0 as usize;
                        let arg_len = if let Some(args) = args.to_constant() {
                            args as usize
                        } else {
                            state.stack.len() - func - 1
                        };

                        state.stack[stack_bottom] = state.stack[func];
                        for i in 0..arg_len {
                            state.stack[stack_bottom + 1 + i] = state.stack[func + 1 + i];
                        }
                        state.stack.truncate(stack_bottom + 1 + arg_len);
                        let continuations = state.frames.pop().unwrap().continuations;

                        if self.call_function(state, stack_bottom, args, returns, continuations)? {
                            return Ok(None);
                        } else {
                            continue 'start;
                        }
                    }

                    OpCode::Return { start, count } => {
                        self.close_upvalues(state, mc, stack_bottom);
                        let mut continuations = state.frames.pop().unwrap().continuations;

                        let start = stack_base + start.0 as usize;
                        let count = count
                            .to_constant()
                            .map(|c| c as usize)
                            .unwrap_or(state.stack.len() - start);

                        if let Some(continuation) = continuations.pop() {
                            let ret_vals = state.stack[start..start + count].to_vec();
                            state.stack.truncate(stack_bottom);
                            let seq = continuation.call(Ok(ret_vals));
                            state.pending = Pending::Callback {
                                callback: seq,
                                returns: returns,
                                continuations,
                            };
                            return Ok(None);
                        } else if state.frames.is_empty() {
                            let ret_vals = state.stack[start..start + count].to_vec();
                            state.stack.clear();

                            return Ok(Some(ret_vals));
                        } else {
                            let returning =
                                returns.to_constant().map(|c| c as usize).unwrap_or(count);

                            for i in 0..returning.min(count) {
                                state.stack[stack_bottom + i] = state.stack[start + i]
                            }

                            for i in count..returning {
                                state.stack[stack_bottom + i] = Value::Nil;
                            }

                            // Set the correct stack size for variable returns, otherwise restore
                            // the previous stack top.
                            if returns.is_variable() {
                                state.stack.truncate(stack_bottom + returning);
                            } else {
                                let current_frame_top = state
                                    .frames
                                    .last()
                                    .expect("no upper frame to return to")
                                    .top;
                                state.stack.resize(current_frame_top, Value::Nil);
                            }

                            continue 'start;
                        }
                    }

                    OpCode::VarArgs { dest, count } => {
                        let varargs_start = stack_bottom + 1;
                        let varargs_len = stack_base - varargs_start;
                        let dest = stack_base + dest.0 as usize;
                        if let Some(count) = count.to_constant() {
                            for i in 0..count as usize {
                                state.stack[dest + i] = if i < varargs_len {
                                    state.stack[varargs_start + i]
                                } else {
                                    Value::Nil
                                };
                            }
                        } else {
                            // Similarly to `OpCode::Return`, we set the stack top to indicate the
                            // number of variable arguments.  The next instruction must consume the
                            // variable results, which will reset the stack to the correct size.
                            state.stack.resize(dest + varargs_len, Value::Nil);
                            for i in 0..varargs_len {
                                state.stack[dest + i] = state.stack[varargs_start + i];
                            }
                        }

                        // The `stack_frame` slice is invalidated, so start over from the very top.
                        continue 'start;
                    }

                    OpCode::Jump {
                        offset,
                        close_upvalues,
                    } => {
                        *pc = add_offset(*pc, offset);
                        if let Some(r) = close_upvalues.to_u8() {
                            for (_, upval) in
                                state.open_upvalues.split_off(&(stack_base + r as usize))
                            {
                                let mut upval = upval.0.write(mc);
                                if let UpValueState::Open(thread, ind) = *upval {
                                    *upval = UpValueState::Closed(if thread == self {
                                        stack_frame[ind - stack_base]
                                    } else {
                                        thread.0.read().stack[ind]
                                    });
                                }
                            }
                        }
                    }

                    OpCode::Test { value, is_true } => {
                        let value = stack_frame[value.0 as usize];
                        if value.to_bool() == is_true {
                            *pc += 1;
                        }
                    }

                    OpCode::TestSet {
                        dest,
                        value,
                        is_true,
                    } => {
                        let value = stack_frame[value.0 as usize];
                        if value.to_bool() == is_true {
                            *pc += 1;
                        } else {
                            stack_frame[dest.0 as usize] = value;
                        }
                    }

                    OpCode::Closure { proto, dest } => {
                        let proto = current_function.0.proto.prototypes[proto.0 as usize];
                        let mut upvalues = Vec::new();
                        for &desc in &proto.upvalues {
                            match desc {
                                UpValueDescriptor::Environment => {
                                    panic!("_ENV upvalue is only allowed on top-level closure");
                                }
                                UpValueDescriptor::ParentLocal(reg) => {
                                    let ind = stack_base + reg.0 as usize;
                                    match state.open_upvalues.entry(ind) {
                                        BTreeEntry::Occupied(occupied) => {
                                            upvalues.push(*occupied.get());
                                        }
                                        BTreeEntry::Vacant(vacant) => {
                                            let uv = UpValue(GcCell::allocate(
                                                mc,
                                                UpValueState::Open(self, ind),
                                            ));
                                            vacant.insert(uv);
                                            upvalues.push(uv);
                                        }
                                    }
                                }
                                UpValueDescriptor::Outer(uvindex) => {
                                    upvalues.push(current_function.0.upvalues[uvindex.0 as usize]);
                                }
                            }
                        }

                        let closure = Closure(Gc::allocate(mc, ClosureState { proto, upvalues }));
                        stack_frame[dest.0 as usize] = Value::Function(Function::Closure(closure));
                    }

                    OpCode::NumericForPrep { base, jump } => {
                        stack_frame[base.0 as usize] = stack_frame[base.0 as usize]
                            .subtract(stack_frame[base.0 as usize + 2])
                            .expect("non numeric for loop parameters");
                        *pc = add_offset(*pc, jump);
                    }

                    OpCode::NumericForLoop { base, jump } => {
                        const ERR_MSG: &str = "non numeric for loop parameter";

                        stack_frame[base.0 as usize] = stack_frame[base.0 as usize]
                            .add(stack_frame[base.0 as usize + 2])
                            .expect(ERR_MSG);
                        let past_end = if stack_frame[base.0 as usize + 2]
                            .less_than(Value::Integer(0))
                            .expect(ERR_MSG)
                        {
                            stack_frame[base.0 as usize]
                                .less_than(stack_frame[base.0 as usize + 1])
                                .expect(ERR_MSG)
                        } else {
                            stack_frame[base.0 as usize + 1]
                                .less_than(stack_frame[base.0 as usize])
                                .expect(ERR_MSG)
                        };
                        if !past_end {
                            *pc = add_offset(*pc, jump);
                            stack_frame[base.0 as usize + 3] = stack_frame[base.0 as usize];
                        }
                    }

                    OpCode::GenericForCall { base, var_count } => {
                        let base = stack_base + base.0 as usize;
                        state.stack.resize(base + 6, Value::Nil);
                        for i in 0..3 {
                            state.stack[base + 3 + i] = state.stack[base + i];
                        }
                        if self.call_function(
                            state,
                            base + 3,
                            VarCount::constant(2),
                            VarCount::constant(var_count),
                            Vec::new(),
                        )? {
                            return Ok(None);
                        } else {
                            continue 'start;
                        }
                    }

                    OpCode::GenericForLoop { base, jump } => {
                        if stack_frame[base.0 as usize + 1].to_bool() {
                            stack_frame[base.0 as usize] = stack_frame[base.0 as usize + 1];
                            *pc = add_offset(*pc, jump);
                        }
                    }

                    OpCode::SelfR { base, table, key } => {
                        let table = stack_frame[table.0 as usize];
                        let key = current_function.0.proto.constants[key.0 as usize].to_value();
                        stack_frame[base.0 as usize + 1] = table;
                        stack_frame[base.0 as usize] = get_table(table)?.get(key);
                    }

                    OpCode::SelfC { base, table, key } => {
                        let table = stack_frame[table.0 as usize];
                        let key = current_function.0.proto.constants[key.0 as usize].to_value();
                        stack_frame[base.0 as usize + 1] = table;
                        stack_frame[base.0 as usize] = get_table(table)?.get(key);
                    }

                    OpCode::Concat {
                        dest,
                        source,
                        count,
                    } => {
                        stack_frame[dest.0 as usize] = Value::String(
                            String::concat(
                                mc,
                                &stack_frame[source.0 as usize..source.0 as usize + count as usize],
                            )
                            .unwrap(),
                        );
                    }

                    OpCode::GetUpValue { source, dest } => {
                        stack_frame[dest.0 as usize] = get_upvalue(
                            self,
                            upper_stack,
                            current_function.0.upvalues[source.0 as usize],
                        );
                    }

                    OpCode::SetUpValue { source, dest } => {
                        let val = stack_frame[source.0 as usize];
                        let mut uv = current_function.0.upvalues[dest.0 as usize].0.write(mc);
                        match &mut *uv {
                            UpValueState::Open(thread, ind) => {
                                if *thread == self {
                                    upper_stack[*ind] = val
                                } else {
                                    thread.0.write(mc).stack[*ind] = val;
                                }
                            }
                            UpValueState::Closed(v) => *v = val,
                        }
                    }

                    OpCode::Length { dest, source } => {
                        stack_frame[dest.0 as usize] =
                            Value::Integer(get_table(stack_frame[source.0 as usize])?.length());
                    }

                    OpCode::EqRR {
                        skip_if,
                        left,
                        right,
                    } => {
                        let left = stack_frame[left.0 as usize];
                        let right = stack_frame[right.0 as usize];
                        if (left == right) == skip_if {
                            *pc += 1;
                        }
                    }

                    OpCode::EqRC {
                        skip_if,
                        left,
                        right,
                    } => {
                        let left = stack_frame[left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        if (left == right) == skip_if {
                            *pc += 1;
                        }
                    }

                    OpCode::EqCR {
                        skip_if,
                        left,
                        right,
                    } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = stack_frame[right.0 as usize];
                        if (left == right) == skip_if {
                            *pc += 1;
                        }
                    }

                    OpCode::EqCC {
                        skip_if,
                        left,
                        right,
                    } => {
                        let left = current_function.0.proto.constants[left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize];
                        if (left == right) == skip_if {
                            *pc += 1;
                        }
                    }

                    OpCode::Not { dest, source } => {
                        let source = stack_frame[source.0 as usize];
                        stack_frame[dest.0 as usize] = source.not();
                    }

                    OpCode::AddRR { dest, left, right } => {
                        let left = stack_frame[left.0 as usize];
                        let right = stack_frame[right.0 as usize];
                        stack_frame[dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddRC { dest, left, right } => {
                        let left = stack_frame[left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        stack_frame[dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = stack_frame[right.0 as usize];
                        stack_frame[dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddCC { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        stack_frame[dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::SubRR { dest, left, right } => {
                        let left = stack_frame[left.0 as usize];
                        let right = stack_frame[right.0 as usize];
                        stack_frame[dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::SubRC { dest, left, right } => {
                        let left = stack_frame[left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        stack_frame[dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::SubCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = stack_frame[right.0 as usize];
                        stack_frame[dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::SubCC { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        stack_frame[dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulRR { dest, left, right } => {
                        let left = stack_frame[left.0 as usize];
                        let right = stack_frame[right.0 as usize];
                        stack_frame[dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulRC { dest, left, right } => {
                        let left = stack_frame[left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        stack_frame[dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = stack_frame[right.0 as usize];
                        stack_frame[dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulCC { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        stack_frame[dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }
                }

                if instructions == 0 {
                    return Ok(None);
                } else {
                    instructions -= 1
                }
            }
        }
    }

    // Call a function at the given index, returns true if there is now a pending callback
    fn call_function(
        self,
        state: &mut ThreadState<'gc>,
        function_index: usize,
        args: VarCount,
        returns: VarCount,
        continuations: Vec<Continuation>,
    ) -> Result<bool, TypeError> {
        match state.stack[function_index] {
            Value::Function(Function::Closure(closure)) => {
                let arg_count = args
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(state.stack.len() - function_index - 1);

                let fixed_params = closure.0.proto.fixed_params as usize;

                let base = if arg_count > fixed_params {
                    state.stack.truncate(function_index + 1 + arg_count);
                    state.stack[function_index + 1..].rotate_left(fixed_params);
                    function_index + 1 + (arg_count - fixed_params)
                } else {
                    function_index + 1
                };

                let top = base + closure.0.proto.stack_size as usize;
                state.stack.resize(top, Value::Nil);

                state.frames.push(Frame {
                    bottom: function_index,
                    base,
                    top,
                    pc: 0,
                    returns,
                    continuations,
                });
                Ok(false)
            }
            Value::Function(Function::Callback(callback)) => {
                let arg_count = args
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(state.stack.len() - function_index - 1);

                let seq = callback
                    .call(state.stack[function_index + 1..function_index + 1 + arg_count].to_vec());
                state.pending = Pending::Callback {
                    callback: seq,
                    returns,
                    continuations,
                };
                state.stack.resize(function_index, Value::Nil);
                Ok(true)
            }
            val => Err(TypeError {
                expected: "function",
                found: val.type_name(),
            }),
        }
    }

    // Unwind frames until a continuation is encountered.  If the error is handled returns None,
    // otherwise returns the original error.
    fn unwind(self, mc: MutationContext<'gc, '_>, error: Error<'gc>) -> Option<Error<'gc>> {
        let mut state = self.0.write(mc);
        match state.pending {
            Pending::None => {}
            _ => panic!("cannot unwind with a pending status"),
        }
        while let Some(mut top_frame) = state.frames.pop() {
            if let Some(continuation) = top_frame.continuations.pop() {
                self.close_upvalues(&mut state, mc, top_frame.bottom);
                state.stack.truncate(top_frame.bottom);
                let seq = continuation.call(Err(error));
                state.pending = Pending::Callback {
                    callback: seq,
                    returns: top_frame.returns,
                    continuations: top_frame.continuations,
                };
                return None;
            }
        }
        self.close_upvalues(&mut state, mc, 0);
        state.stack.clear();
        Some(error)
    }

    fn close_upvalues(
        self,
        state: &mut ThreadState<'gc>,
        mc: MutationContext<'gc, '_>,
        bottom: usize,
    ) {
        for (_, upval) in state.open_upvalues.split_off(&bottom) {
            let mut upval = upval.0.write(mc);
            if let UpValueState::Open(thread, ind) = *upval {
                *upval = UpValueState::Closed(if thread == self {
                    state.stack[ind]
                } else {
                    thread.0.read().stack[ind]
                });
            }
        }
    }

    // Return to the top Lua frame from an external call
    fn return_to_lua(self, state: &mut ThreadState<'gc>, rets: &[Value<'gc>], ret_count: VarCount) {
        assert!(!state.frames.is_empty(), "no Lua frame to return to");
        let return_len = ret_count
            .to_constant()
            .map(|c| c as usize)
            .unwrap_or(rets.len());

        let bottom = state.stack.len();
        state.stack.resize(bottom + return_len, Value::Nil);

        for i in 0..return_len.min(rets.len()) {
            state.stack[bottom + i] = rets[i];
        }

        // Stack size is already correct for variable returns, but if we are returning a
        // constant number, we need to restore the previous stack top.
        if !ret_count.is_variable() {
            let current_frame_top = state.frames.last().unwrap().top;
            state.stack.resize(current_frame_top, Value::Nil);
        }
    }
}

#[derive(Collect)]
#[collect(empty_drop)]
struct ThreadSequence<'gc>(Thread<'gc>);

impl<'gc> Sequence<'gc> for ThreadSequence<'gc> {
    type Item = Vec<Value<'gc>>;
    type Error = Error<'gc>;

    fn step(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Self::Error>> {
        self.0.step(mc, lc)
    }
}

#[derive(Collect)]
#[collect(require_static)]
struct Frame {
    bottom: usize,
    base: usize,
    top: usize,
    pc: usize,
    returns: VarCount,
    continuations: Vec<Continuation>,
}

#[derive(Collect)]
#[collect(empty_drop)]
enum Pending<'gc> {
    None,
    Working,
    StartCoroutine(Function<'gc>),
    ResumeCoroutine {
        returns: VarCount,
        continuations: Vec<Continuation>,
    },
    Callback {
        callback: CallbackSequenceBox<'gc>,
        returns: VarCount,
        continuations: Vec<Continuation>,
    },
}

fn get_upvalue<'gc>(
    self_thread: Thread<'gc>,
    self_stack: &[Value<'gc>],
    upvalue: UpValue<'gc>,
) -> Value<'gc> {
    match *upvalue.0.read() {
        UpValueState::Open(thread, ind) => {
            if thread == self_thread {
                self_stack[ind]
            } else {
                thread.0.read().stack[ind]
            }
        }
        UpValueState::Closed(v) => v,
    }
}

fn get_table<'gc>(value: Value<'gc>) -> Result<Table<'gc>, TypeError> {
    match value {
        Value::Table(t) => Ok(t),
        val => Err(TypeError {
            expected: "table",
            found: val.type_name(),
        }),
    }
}

fn add_offset(pc: usize, offset: i16) -> usize {
    if offset > 0 {
        pc.checked_add(offset as usize).unwrap()
    } else if offset < 0 {
        pc.checked_sub(-offset as usize).unwrap()
    } else {
        pc
    }
}
