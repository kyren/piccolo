use std::collections::btree_map::Entry as BTreeEntry;
use std::collections::BTreeMap;
use std::fmt::{self, Debug};

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::{
    Callback, CallbackResult, Closure, ClosureState, ContinuationResult, Error, LuaContext, OpCode,
    RunContinuation, Sequence, SequenceExt, String, Table, UpValue, UpValueDescriptor,
    UpValueState, Value, VarCount,
};

#[derive(Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Thread<'gc>(GcCell<'gc, ThreadState<'gc>>);

impl<'gc> Debug for Thread<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Thread")
            .field(&(&self.0 as *const _))
            .finish()
    }
}

impl<'gc> PartialEq for Thread<'gc> {
    fn eq(&self, other: &Thread<'gc>) -> bool {
        GcCell::ptr_eq(&self.0, &other.0)
    }
}

impl<'gc> Thread<'gc> {
    pub fn new(mc: MutationContext<'gc, '_>) -> Thread<'gc> {
        Thread(GcCell::allocate(mc, ThreadState::new()))
    }

    /// Call a closure on this thread, producing a `Sequence`.  No more than `granularity` VM
    /// instructions will be executed at a time during each `Sequence` step.
    ///
    /// The same `Thread` can be used for multiple function calls, but only the most recently
    /// created unfinished `ThreadSequence` for a `Thread` can be run at any given time.  When a
    /// `ThreadSequence` is constructed, it operates on whatever the top of the stack is at that
    /// time, so any later constructed `ThreadSequence`s must be run to completion before earlier
    /// ones can be completed.
    pub fn call_closure(
        self,
        mc: MutationContext<'gc, '_>,
        closure: Closure<'gc>,
        args: &[Value<'gc>],
        granularity: u32,
    ) -> RunContinuation<'gc, Vec<Value<'gc>>, Error> {
        RunContinuation::from_sequence(Thread::sequence_closure(
            self,
            mc,
            closure,
            args,
            granularity,
        ))
    }

    /// Call a closure on this thread in the context of a callback, produces a continuation that can
    /// be returned inside a `CallbackResult`.
    pub fn continue_closure(
        self,
        mc: MutationContext<'gc, '_>,
        closure: Closure<'gc>,
        args: &[Value<'gc>],
        granularity: u32,
    ) -> Box<Sequence<'gc, Item = CallbackResult<'gc>, Error = Error> + 'gc> {
        fn continuation_to_callback_result<'gc>(
            cont: ContinuationResult<'gc, Vec<Value<'gc>>, Error>,
        ) -> CallbackResult<'gc> {
            match cont {
                ContinuationResult::Continue(cont) => {
                    CallbackResult::Continue(Box::new(cont.map(continuation_to_callback_result)))
                }
                ContinuationResult::Finish(res) => CallbackResult::Return(res),
            }
        }

        Box::new(
            self.sequence_closure(mc, closure, args, granularity)
                .map(continuation_to_callback_result),
        )
    }

    fn sequence_closure(
        self,
        mc: MutationContext<'gc, '_>,
        closure: Closure<'gc>,
        args: &[Value<'gc>],
        granularity: u32,
    ) -> ThreadSequence<'gc> {
        assert_ne!(granularity, 0, "granularity cannot be zero");

        let mut state = self.0.write(mc);
        let closure_index = state.stack.len();
        state.stack.push(Value::Closure(closure));
        state.stack.extend(args);
        let res_pc = state.pc;
        state.call_closure(
            closure_index,
            VarCount::variable(),
            FrameReturn::CallBoundary,
            res_pc,
        );
        let frame_top = state.frames.len();

        ThreadSequence {
            thread: self,
            frame_top,
            granularity,
        }
    }
}

#[derive(Collect)]
#[collect(empty_drop)]
struct ThreadSequence<'gc> {
    thread: Thread<'gc>,
    frame_top: usize,
    granularity: u32,
}

impl<'gc> Sequence<'gc> for ThreadSequence<'gc> {
    type Item = ContinuationResult<'gc, Vec<Value<'gc>>, Error>;
    type Error = Error;

    fn step(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Self::Error>> {
        let mut state = self.thread.0.write(mc);
        if self.frame_top != state.frames.len() {
            panic!("frame mismatch in ThreadSequence, Sequences evaluated out of order");
        }
        let res = state.step(mc, lc, self.thread, self.granularity);
        self.frame_top = state.frames.len();
        res
    }
}

#[derive(Collect)]
#[collect(empty_drop)]
enum FrameType<'gc> {
    Lua {
        base: usize,
    },
    Callback {
        callback: Box<Sequence<'gc, Item = CallbackResult<'gc>, Error = Error> + 'gc>,
    },
    Yield,
}

#[derive(Clone, Copy, PartialEq, Eq, Collect)]
#[collect(require_copy)]
enum FrameReturn {
    // Frame is a Thread entry-point, and returning should return all results to the caller
    CallBoundary,
    // Frame is a normal call frame within a thread, returning should return the given number of
    // results to the frame above
    Upper(VarCount),
}

#[derive(Collect)]
#[collect(empty_drop)]
struct Frame<'gc> {
    bottom: usize,
    top: usize,
    frame_type: FrameType<'gc>,
    frame_return: FrameReturn,
    restore_pc: usize,
}

#[derive(Collect)]
#[collect(empty_drop)]
struct ThreadState<'gc> {
    stack: Vec<Value<'gc>>,
    frames: Vec<Frame<'gc>>,
    pc: usize,
    open_upvalues: BTreeMap<usize, UpValue<'gc>>,
}

impl<'gc> ThreadState<'gc> {
    fn new() -> ThreadState<'gc> {
        ThreadState {
            stack: Vec::new(),
            frames: Vec::new(),
            pc: 0,
            open_upvalues: BTreeMap::new(),
        }
    }

    fn step(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
        self_thread: Thread<'gc>,
        granularity: u32,
    ) -> Option<Result<ContinuationResult<'gc, Vec<Value<'gc>>, Error>, Error>> {
        match self
            .frames
            .last()
            .expect("cannot step a finished thread")
            .frame_type
        {
            FrameType::Lua { .. } => self.step_lua(mc, self_thread, granularity),
            FrameType::Callback { .. } => self.step_callback(mc, lc, self_thread),
            FrameType::Yield => panic!("cannot step a suspended thread"),
        }
    }

    fn step_callback(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
        self_thread: Thread<'gc>,
    ) -> Option<Result<ContinuationResult<'gc, Vec<Value<'gc>>, Error>, Error>> {
        let callback = match &mut self
            .frames
            .last_mut()
            .expect("no current ThreadState frame")
            .frame_type
        {
            FrameType::Callback { callback } => callback,
            _ => panic!("step_callback called when top frame is not a callback"),
        };

        match callback.step(mc, lc) {
            None => None,
            Some(Err(err)) => {
                self.unwind(mc, self_thread);
                Some(Err(err))
            }
            Some(Ok(CallbackResult::Continue(cont))) => {
                *callback = cont;
                None
            }
            Some(Ok(CallbackResult::Yield(res))) => {
                self.frames
                    .last_mut()
                    .expect("no callback frame")
                    .frame_type = FrameType::Yield;
                Some(Ok(ContinuationResult::Finish(res)))
            }
            Some(Ok(CallbackResult::Return(res))) => {
                let top_frame = self.frames.pop().expect("no callback frame");
                self.pc = top_frame.restore_pc;

                let returns = match top_frame.frame_return {
                    FrameReturn::Upper(returns) => returns,
                    FrameReturn::CallBoundary => panic!("no frame to return to from callback"),
                };
                let return_len = returns
                    .to_constant()
                    .map(|c| c as usize)
                    .unwrap_or(res.len());

                self.stack.truncate(top_frame.bottom);
                self.stack.resize(top_frame.bottom + return_len, Value::Nil);

                for i in 0..return_len.min(res.len()) {
                    self.stack[top_frame.bottom + i] = res[i];
                }

                // Stack size is already correct for variable returns, but if we are returning a
                // constant number, we need to restore the previous stack top.
                if !returns.is_variable() {
                    let current_frame_top = self
                        .frames
                        .last()
                        .expect("no frame to return to from callback")
                        .top;
                    self.stack.resize(current_frame_top, Value::Nil);
                }
                None
            }
        }
    }

    fn step_lua(
        &mut self,
        mc: MutationContext<'gc, '_>,
        self_thread: Thread<'gc>,
        mut instructions: u32,
    ) -> Option<Result<ContinuationResult<'gc, Vec<Value<'gc>>, Error>, Error>> {
        'function_start: loop {
            let current_frame = self.frames.last().expect("no current ThreadState frame");
            let stack_bottom = current_frame.bottom;
            let frame_return = current_frame.frame_return;
            let restore_pc = current_frame.restore_pc;
            let stack_base = match current_frame.frame_type {
                FrameType::Lua { base } => base,
                _ => panic!("step_lua called when top frame is not a callback"),
            };
            let current_function = get_closure(self.stack[stack_bottom]);

            loop {
                let op = current_function.0.proto.opcodes[self.pc];
                self.pc += 1;

                match op {
                    OpCode::Move { dest, source } => {
                        self.stack[stack_base + dest.0 as usize] =
                            self.stack[stack_base + source.0 as usize];
                    }

                    OpCode::LoadConstant { dest, constant } => {
                        self.stack[stack_base + dest.0 as usize] =
                            current_function.0.proto.constants[constant.0 as usize].to_value();
                    }

                    OpCode::LoadBool {
                        dest,
                        value,
                        skip_next,
                    } => {
                        self.stack[stack_base + dest.0 as usize] = Value::Boolean(value);
                        if skip_next {
                            self.pc += 1;
                        }
                    }

                    OpCode::LoadNil { dest, count } => {
                        for i in dest.0..dest.0 + count {
                            self.stack[stack_base + i as usize] = Value::Nil;
                        }
                    }

                    OpCode::NewTable { dest } => {
                        self.stack[stack_base + dest.0 as usize] = Value::Table(Table::new(mc));
                    }

                    OpCode::GetTableR { dest, table, key } => {
                        self.stack[stack_base + dest.0 as usize] =
                            get_table(self.stack[stack_base + table.0 as usize])
                                .get(self.stack[stack_base + key.0 as usize]);
                    }

                    OpCode::GetTableC { dest, table, key } => {
                        self.stack[stack_base + dest.0 as usize] =
                            get_table(self.stack[stack_base + table.0 as usize])
                                .get(current_function.0.proto.constants[key.0 as usize].to_value())
                    }

                    OpCode::SetTableRR { table, key, value } => {
                        get_table(self.stack[stack_base + table.0 as usize])
                            .set(
                                mc,
                                self.stack[stack_base + key.0 as usize],
                                self.stack[stack_base + value.0 as usize],
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableRC { table, key, value } => {
                        get_table(self.stack[stack_base + table.0 as usize])
                            .set(
                                mc,
                                self.stack[stack_base + key.0 as usize],
                                current_function.0.proto.constants[value.0 as usize].to_value(),
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableCR { table, key, value } => {
                        get_table(self.stack[stack_base + table.0 as usize])
                            .set(
                                mc,
                                current_function.0.proto.constants[key.0 as usize].to_value(),
                                self.stack[stack_base + value.0 as usize],
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableCC { table, key, value } => {
                        get_table(self.stack[stack_base + table.0 as usize])
                            .set(
                                mc,
                                current_function.0.proto.constants[key.0 as usize].to_value(),
                                current_function.0.proto.constants[value.0 as usize].to_value(),
                            )
                            .expect("could not set table value");
                    }

                    OpCode::GetUpTableR { dest, table, key } => {
                        self.stack[stack_base + dest.0 as usize] = get_table(self.get_upvalue(
                            self_thread,
                            current_function.0.upvalues[table.0 as usize],
                        ))
                        .get(self.stack[stack_base + key.0 as usize]);
                    }

                    OpCode::GetUpTableC { dest, table, key } => {
                        self.stack[stack_base + dest.0 as usize] = get_table(self.get_upvalue(
                            self_thread,
                            current_function.0.upvalues[table.0 as usize],
                        ))
                        .get(current_function.0.proto.constants[key.0 as usize].to_value())
                    }

                    OpCode::SetUpTableRR { table, key, value } => {
                        get_table(self.get_upvalue(
                            self_thread,
                            current_function.0.upvalues[table.0 as usize],
                        ))
                        .set(
                            mc,
                            self.stack[stack_base + key.0 as usize],
                            self.stack[stack_base + value.0 as usize],
                        )
                        .expect("could not set table value");
                    }

                    OpCode::SetUpTableRC { table, key, value } => {
                        get_table(self.get_upvalue(
                            self_thread,
                            current_function.0.upvalues[table.0 as usize],
                        ))
                        .set(
                            mc,
                            self.stack[stack_base + key.0 as usize],
                            current_function.0.proto.constants[value.0 as usize].to_value(),
                        )
                        .expect("could not set table value");
                    }

                    OpCode::SetUpTableCR { table, key, value } => {
                        get_table(self.get_upvalue(
                            self_thread,
                            current_function.0.upvalues[table.0 as usize],
                        ))
                        .set(
                            mc,
                            current_function.0.proto.constants[key.0 as usize].to_value(),
                            self.stack[stack_base + value.0 as usize],
                        )
                        .expect("could not set table value");
                    }

                    OpCode::SetUpTableCC { table, key, value } => {
                        get_table(self.get_upvalue(
                            self_thread,
                            current_function.0.upvalues[table.0 as usize],
                        ))
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
                        if let Some(ret) = self.call_function(
                            stack_base + func.0 as usize,
                            args,
                            FrameReturn::Upper(returns),
                            self.pc,
                        ) {
                            return Some(ret);
                        }
                        continue 'function_start;
                    }

                    OpCode::TailCall { func, args } => {
                        self.close_upvalues(mc, self_thread, stack_bottom);

                        let func = stack_base + func.0 as usize;
                        let arg_len = if let Some(args) = args.to_constant() {
                            args as usize
                        } else {
                            self.stack.len() - func - 1
                        };

                        self.stack[stack_bottom] = self.stack[func];
                        for i in 0..arg_len {
                            self.stack[stack_bottom + 1 + i] = self.stack[func + 1 + i];
                        }
                        self.stack.truncate(stack_bottom + 1 + arg_len);
                        self.frames.pop();

                        if let Some(ret) =
                            self.call_function(stack_bottom, args, frame_return, restore_pc)
                        {
                            return Some(ret);
                        }
                        continue 'function_start;
                    }

                    OpCode::Return { start, count } => {
                        self.close_upvalues(mc, self_thread, stack_bottom);
                        self.pc = restore_pc;
                        self.frames.pop();

                        let start = stack_base + start.0 as usize;
                        let count = count
                            .to_constant()
                            .map(|c| c as usize)
                            .unwrap_or(self.stack.len() - start);

                        match frame_return {
                            FrameReturn::CallBoundary => {
                                let ret_vals = self.stack[start..start + count].to_vec();

                                if let Some(frame) = self.frames.last() {
                                    self.stack.resize(frame.top, Value::Nil);
                                } else {
                                    self.stack.clear();
                                }

                                return Some(Ok(ContinuationResult::Finish(ret_vals)));
                            }
                            FrameReturn::Upper(returns) => {
                                let returning =
                                    returns.to_constant().map(|c| c as usize).unwrap_or(count);

                                for i in 0..returning.min(count) {
                                    self.stack[stack_bottom + i] = self.stack[start + i]
                                }

                                for i in count..returning {
                                    self.stack[stack_bottom + i] = Value::Nil;
                                }

                                // Set the correct stack size for variable returns, otherwise restore
                                // the previous stack top.
                                if returns.is_variable() {
                                    self.stack.truncate(stack_bottom + returning);
                                } else {
                                    let current_frame_top = self
                                        .frames
                                        .last()
                                        .expect("no upper frame to return to")
                                        .top;
                                    self.stack.resize(current_frame_top, Value::Nil);
                                }

                                continue 'function_start;
                            }
                        }
                    }

                    OpCode::VarArgs { dest, count } => {
                        let varargs_start = stack_bottom + 1;
                        let varargs_len = stack_base - varargs_start;
                        let dest = stack_base + dest.0 as usize;
                        if let Some(count) = count.to_constant() {
                            for i in 0..count as usize {
                                self.stack[dest + i] = if i < varargs_len {
                                    self.stack[varargs_start + i]
                                } else {
                                    Value::Nil
                                };
                            }
                        } else {
                            // Similarly to `OpCode::Return`, we set the stack top to indicate the
                            // number of variable arguments.  The next instruction must consume the
                            // variable results, which will reset the stack to the correct size.
                            self.stack.resize(dest + varargs_len, Value::Nil);
                            for i in 0..varargs_len {
                                self.stack[dest + i] = self.stack[varargs_start + i];
                            }
                        }
                    }

                    OpCode::Jump {
                        offset,
                        close_upvalues,
                    } => {
                        self.pc = add_offset(self.pc, offset);
                        if let Some(r) = close_upvalues.to_u8() {
                            self.close_upvalues(mc, self_thread, stack_base + r as usize);
                        }
                    }

                    OpCode::Test { value, is_true } => {
                        let value = self.stack[stack_base + value.0 as usize];
                        if value.to_bool() == is_true {
                            self.pc += 1;
                        }
                    }

                    OpCode::TestSet {
                        dest,
                        value,
                        is_true,
                    } => {
                        let value = self.stack[stack_base + value.0 as usize];
                        if value.to_bool() == is_true {
                            self.pc += 1;
                        } else {
                            self.stack[stack_base + dest.0 as usize] = value;
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
                                    match self.open_upvalues.entry(ind) {
                                        BTreeEntry::Occupied(occupied) => {
                                            upvalues.push(*occupied.get());
                                        }
                                        BTreeEntry::Vacant(vacant) => {
                                            let uv = UpValue(GcCell::allocate(
                                                mc,
                                                UpValueState::Open(self_thread, ind),
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
                        self.stack[stack_base + dest.0 as usize] = Value::Closure(closure);
                    }

                    OpCode::NumericForPrep { base, jump } => {
                        let base = stack_base + base.0 as usize;
                        self.stack[base] = self.stack[base]
                            .subtract(self.stack[base + 2])
                            .expect("non numeric for loop parameters");
                        self.pc = add_offset(self.pc, jump);
                    }

                    OpCode::NumericForLoop { base, jump } => {
                        const ERR_MSG: &str = "non numeric for loop parameter";

                        let base = stack_base + base.0 as usize;
                        self.stack[base] =
                            self.stack[base].add(self.stack[base + 2]).expect(ERR_MSG);
                        let past_end = if self.stack[base + 2]
                            .less_than(Value::Integer(0))
                            .expect(ERR_MSG)
                        {
                            self.stack[base]
                                .less_than(self.stack[base + 1])
                                .expect(ERR_MSG)
                        } else {
                            self.stack[base + 1]
                                .less_than(self.stack[base])
                                .expect(ERR_MSG)
                        };
                        if !past_end {
                            self.pc = add_offset(self.pc, jump);
                            self.stack[base + 3] = self.stack[base];
                        }
                    }

                    OpCode::GenericForCall { base, var_count } => {
                        let base = stack_base + base.0 as usize;
                        self.stack.resize(base + 6, Value::Nil);
                        for i in 0..3 {
                            self.stack[base + 3 + i] = self.stack[base + i];
                        }
                        if let Some(ret) = self.call_function(
                            base + 3,
                            VarCount::constant(2),
                            FrameReturn::Upper(VarCount::constant(var_count)),
                            self.pc,
                        ) {
                            return Some(ret);
                        }
                        continue 'function_start;
                    }

                    OpCode::GenericForLoop { base, jump } => {
                        let base = stack_base + base.0 as usize;
                        if self.stack[base + 1].to_bool() {
                            self.stack[base] = self.stack[base + 1];
                            self.pc = add_offset(self.pc, jump);
                        }
                    }

                    OpCode::SelfR { base, table, key } => {
                        let base = stack_base + base.0 as usize;
                        let table = self.stack[stack_base + table.0 as usize];
                        let key = current_function.0.proto.constants[key.0 as usize].to_value();
                        self.stack[base + 1] = table;
                        self.stack[base] = get_table(table).get(key);
                    }

                    OpCode::SelfC { base, table, key } => {
                        let base = stack_base + base.0 as usize;
                        let table = self.stack[stack_base + table.0 as usize];
                        let key = current_function.0.proto.constants[key.0 as usize].to_value();
                        self.stack[base + 1] = table;
                        self.stack[base] = get_table(table).get(key);
                    }

                    OpCode::Concat {
                        dest,
                        source,
                        count,
                    } => {
                        self.stack[stack_base + dest.0 as usize] = Value::String(
                            String::concat(
                                mc,
                                &self.stack[stack_base + source.0 as usize
                                    ..stack_base + source.0 as usize + count as usize],
                            )
                            .unwrap(),
                        );
                    }

                    OpCode::GetUpValue { source, dest } => {
                        self.stack[stack_base + dest.0 as usize] = self.get_upvalue(
                            self_thread,
                            current_function.0.upvalues[source.0 as usize],
                        );
                    }

                    OpCode::SetUpValue { source, dest } => {
                        let val = self.stack[stack_base + source.0 as usize];
                        let mut uv = current_function.0.upvalues[dest.0 as usize].0.write(mc);
                        match &mut *uv {
                            UpValueState::Open(thread, ind) => {
                                if *thread == self_thread {
                                    self.stack[*ind] = val
                                } else {
                                    thread.0.write(mc).stack[*ind] = val;
                                }
                            }
                            UpValueState::Closed(v) => *v = val,
                        }
                    }

                    OpCode::Length { dest, source } => {
                        self.stack[stack_base + dest.0 as usize] = Value::Integer(
                            get_table(self.stack[stack_base + source.0 as usize]).length(),
                        );
                    }

                    OpCode::EqRR {
                        skip_if,
                        left,
                        right,
                    } => {
                        let left = self.stack[stack_base + left.0 as usize];
                        let right = self.stack[stack_base + right.0 as usize];
                        if (left == right) == skip_if {
                            self.pc += 1;
                        }
                    }

                    OpCode::EqRC {
                        skip_if,
                        left,
                        right,
                    } => {
                        let left = self.stack[stack_base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        if (left == right) == skip_if {
                            self.pc += 1;
                        }
                    }

                    OpCode::EqCR {
                        skip_if,
                        left,
                        right,
                    } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = self.stack[stack_base + right.0 as usize];
                        if (left == right) == skip_if {
                            self.pc += 1;
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
                            self.pc += 1;
                        }
                    }

                    OpCode::Not { dest, source } => {
                        let source = self.stack[stack_base + source.0 as usize];
                        self.stack[stack_base + dest.0 as usize] = source.not();
                    }

                    OpCode::AddRR { dest, left, right } => {
                        let left = self.stack[stack_base + left.0 as usize];
                        let right = self.stack[stack_base + right.0 as usize];
                        self.stack[stack_base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddRC { dest, left, right } => {
                        let left = self.stack[stack_base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[stack_base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = self.stack[stack_base + right.0 as usize];
                        self.stack[stack_base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddCC { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[stack_base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::SubRR { dest, left, right } => {
                        let left = self.stack[stack_base + left.0 as usize];
                        let right = self.stack[stack_base + right.0 as usize];
                        self.stack[stack_base + dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::SubRC { dest, left, right } => {
                        let left = self.stack[stack_base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[stack_base + dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::SubCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = self.stack[stack_base + right.0 as usize];
                        self.stack[stack_base + dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::SubCC { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[stack_base + dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulRR { dest, left, right } => {
                        let left = self.stack[stack_base + left.0 as usize];
                        let right = self.stack[stack_base + right.0 as usize];
                        self.stack[stack_base + dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulRC { dest, left, right } => {
                        let left = self.stack[stack_base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[stack_base + dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = self.stack[stack_base + right.0 as usize];
                        self.stack[stack_base + dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulCC { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[stack_base + dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }
                }

                if instructions == 0 {
                    return None;
                } else {
                    instructions -= 1
                }
            }
        }
    }

    // Unwind frames up to and including the most recent call boundary
    fn unwind(&mut self, mc: MutationContext<'gc, '_>, self_thread: Thread<'gc>) {
        loop {
            let frame = self
                .frames
                .pop()
                .expect("no call boundary found during unwind");
            if frame.frame_return == FrameReturn::CallBoundary {
                self.close_upvalues(mc, self_thread, frame.bottom);
                break;
            }
        }

        if let Some(top) = self.frames.last().map(|f| f.top) {
            self.stack.resize(top, Value::Nil);
        }
    }

    fn call_function(
        &mut self,
        function_index: usize,
        args: VarCount,
        frame_return: FrameReturn,
        restore_pc: usize,
    ) -> Option<Result<ContinuationResult<'gc, Vec<Value<'gc>>, Error>, Error>> {
        match self.stack[function_index] {
            Value::Closure(_) => {
                self.call_closure(function_index, args, frame_return, restore_pc);
                None
            }
            Value::Callback(_) => {
                self.call_callback(function_index, args, frame_return, restore_pc)
            }
            _ => panic!("not a closure or callback"),
        }
    }

    fn call_closure(
        &mut self,
        function_index: usize,
        args: VarCount,
        frame_return: FrameReturn,
        restore_pc: usize,
    ) {
        let closure = get_closure(self.stack[function_index]);
        let arg_count = args
            .to_constant()
            .map(|c| c as usize)
            .unwrap_or(self.stack.len() - function_index - 1);

        let fixed_params = closure.0.proto.fixed_params as usize;

        let base = if arg_count > fixed_params {
            self.stack.truncate(function_index + 1 + arg_count);
            self.stack[function_index + 1..].rotate_left(fixed_params);
            function_index + 1 + (arg_count - fixed_params)
        } else {
            function_index + 1
        };

        let top = base + closure.0.proto.stack_size as usize;
        self.stack.resize(top, Value::Nil);

        self.frames.push(Frame {
            bottom: function_index,
            top,
            frame_type: FrameType::Lua { base },
            frame_return,
            restore_pc,
        });

        self.pc = 0;
    }

    fn call_callback(
        &mut self,
        function_index: usize,
        args: VarCount,
        frame_return: FrameReturn,
        restore_pc: usize,
    ) -> Option<Result<ContinuationResult<'gc, Vec<Value<'gc>>, Error>, Error>> {
        let callback = get_callback(self.stack[function_index]);
        let arg_count = args
            .to_constant()
            .map(|c| c as usize)
            .unwrap_or(self.stack.len() - function_index - 1);

        match callback.call(&self.stack[function_index + 1..function_index + 1 + arg_count]) {
            Err(err) => Some(Err(err)),
            Ok(res) => match res {
                CallbackResult::Return(res) => match frame_return {
                    FrameReturn::CallBoundary => Some(Ok(ContinuationResult::Finish(res))),
                    FrameReturn::Upper(returns) => {
                        let count = res.len();
                        if let Some(returning) = returns.to_constant() {
                            if let Some(current_frame) = self.frames.last() {
                                self.stack.resize(current_frame.top, Value::Nil);
                            }

                            let returning = returning as usize;
                            for i in 0..returning.min(count) {
                                self.stack[function_index + i] = res[i];
                            }
                            for i in count..returning {
                                self.stack[function_index + i] = Value::Nil;
                            }
                        } else {
                            self.stack.resize(function_index + count, Value::Nil);
                            for i in 0..count {
                                self.stack[function_index + i] = res[i];
                            }
                        }

                        self.pc = restore_pc;
                        None
                    }
                },
                CallbackResult::Yield(res) => {
                    self.frames.push(Frame {
                        bottom: function_index,
                        top: function_index,
                        frame_type: FrameType::Yield,
                        frame_return,
                        restore_pc,
                    });
                    self.stack.resize(function_index, Value::Nil);
                    Some(Ok(ContinuationResult::Finish(res)))
                }
                CallbackResult::Continue(cont) => match frame_return {
                    FrameReturn::CallBoundary => {
                        fn callback_to_continuation_result<'gc>(
                            cont: Result<CallbackResult<'gc>, Error>,
                        ) -> Result<ContinuationResult<'gc, Vec<Value<'gc>>, Error>, Error>
                        {
                            cont.and_then(|cont| match cont {
                                CallbackResult::Return(res) => Ok(ContinuationResult::Finish(res)),
                                CallbackResult::Yield(_) => Err(Error::RuntimeError(Some(
                                    "yield from unyieldable function".into(),
                                ))),
                                CallbackResult::Continue(cont) => Ok(ContinuationResult::Continue(
                                    Box::new(cont.map_result(callback_to_continuation_result)),
                                )),
                            })
                        }

                        Some(Ok(ContinuationResult::Continue(Box::new(
                            cont.map_result(callback_to_continuation_result),
                        ))))
                    }
                    FrameReturn::Upper(returns) => {
                        self.frames.push(Frame {
                            bottom: function_index,
                            top: function_index,
                            frame_type: FrameType::Callback { callback: cont },
                            frame_return: FrameReturn::Upper(returns),
                            restore_pc,
                        });
                        self.stack.resize(function_index, Value::Nil);
                        None
                    }
                },
            },
        }
    }

    fn get_upvalue(&self, self_thread: Thread<'gc>, upvalue: UpValue<'gc>) -> Value<'gc> {
        match *upvalue.0.read() {
            UpValueState::Open(thread, ind) => {
                if thread == self_thread {
                    self.stack[ind]
                } else {
                    thread.0.read().stack[ind]
                }
            }
            UpValueState::Closed(v) => v,
        }
    }

    fn close_upvalues(
        &mut self,
        mc: MutationContext<'gc, '_>,
        self_thread: Thread<'gc>,
        bottom: usize,
    ) {
        for (_, upval) in self.open_upvalues.split_off(&bottom) {
            let mut upval = upval.0.write(mc);
            if let UpValueState::Open(thread, ind) = *upval {
                *upval = UpValueState::Closed(if thread == self_thread {
                    self.stack[ind]
                } else {
                    thread.0.read().stack[ind]
                });
            }
        }
    }
}

fn get_closure<'gc>(value: Value<'gc>) -> Closure<'gc> {
    match value {
        Value::Closure(c) => c,
        _ => panic!("value is not a closure"),
    }
}

fn get_callback<'gc>(value: Value<'gc>) -> Callback<'gc> {
    match value {
        Value::Callback(c) => c,
        _ => panic!("value is not a callback"),
    }
}

fn get_table<'gc>(value: Value<'gc>) -> Table<'gc> {
    match value {
        Value::Table(t) => t,
        _ => panic!("value is not a table"),
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
