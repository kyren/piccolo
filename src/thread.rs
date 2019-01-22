use std::collections::btree_map::Entry as BTreeEntry;
use std::collections::BTreeMap;

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::{
    Callback, Closure, ClosureState, Continuation, ContinuationResult, Error, LuaContext, OpCode,
    RunContinuation, Sequence, String, Table, UpValue, UpValueDescriptor, UpValueState, Value,
    VarCount,
};

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Thread<'gc>(GcCell<'gc, ThreadState<'gc>>);

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
    ///
    /// The return value of `ThreadSequence` follows the "continuation" sequence pattern, in order
    /// to get a finaly result it must ultimately be wrapped in a `RunContinuation`.  You can call
    /// `run_closure` instead to do this automatically.
    pub fn call_closure(
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
            VarCount::variable(),
            res_pc,
            true,
        );

        ThreadSequence {
            thread: Some(self),
            callback: None,
            granularity,
        }
    }

    /// Wraps the return value of `call_closure` in a `RunContinuation` so it will be run to
    /// completion.
    pub fn run_closure(
        self,
        mc: MutationContext<'gc, '_>,
        closure: Closure<'gc>,
        args: &[Value<'gc>],
        granularity: u32,
    ) -> RunContinuation<'gc, Vec<Value<'gc>>, Error> {
        RunContinuation::from_sequence(Thread::call_closure(self, mc, closure, args, granularity))
    }
}

#[derive(Collect)]
#[collect(empty_drop)]
pub struct ThreadSequence<'gc> {
    thread: Option<Thread<'gc>>,
    // If this is set, then this thread is currently waiting on the results of this callback.
    callback: Option<RunContinuation<'gc, Vec<Value<'gc>>, Error>>,
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
        let thread = self.thread.expect("cannot step a finished ThreadSequence");
        let mut state = thread.0.write(mc);

        if let Some(callback) = self.callback.as_mut() {
            match callback.step(mc, lc) {
                None => {}
                Some(res) => {
                    self.callback = None;
                    let res = res.expect("callback errors not handled yet");

                    let top_frame = state.frames.pop().expect("no callback frame");
                    state.pc = top_frame.restore_pc;

                    let count = res.len();
                    let returning = top_frame
                        .returns
                        .to_constant()
                        .map(|c| c as usize)
                        .unwrap_or(res.len());

                    state.stack.truncate(top_frame.bottom);
                    state.stack.resize(top_frame.bottom + returning, Value::Nil);

                    for i in 0..returning.min(count) {
                        state.stack[top_frame.bottom + i] = res[i];
                    }

                    if !top_frame.returns.is_variable() {
                        let current_frame_top = state
                            .frames
                            .last()
                            .expect("no frame to return to from callback")
                            .top;
                        state.stack.resize(current_frame_top, Value::Nil);
                    }
                }
            }
            None
        } else {
            match state.run(mc, thread, self.granularity) {
                ThreadReturn::Unfinished => None,
                ThreadReturn::Finished(res) => {
                    self.thread = None;
                    Some(Ok(ContinuationResult::Finish(res)))
                }
                ThreadReturn::Callback(callback) => {
                    self.callback = Some(RunContinuation::new(callback));
                    None
                }
                ThreadReturn::TailCallback(callback) => {
                    // A tail callback is one that we do not need to wait for the results of, so we
                    // simply continue by with it instead of this thread.
                    self.thread = None;
                    Some(Ok(ContinuationResult::Continue(callback)))
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_static)]
struct Frame {
    bottom: usize,
    base: usize,
    top: usize,
    returns: VarCount,
    restore_pc: usize,
    call_boundary: bool,
}

#[derive(Debug, Collect)]
#[collect(empty_drop)]
struct ThreadState<'gc> {
    stack: Vec<Value<'gc>>,
    frames: Vec<Frame>,
    pc: usize,
    open_upvalues: BTreeMap<usize, UpValue<'gc>>,
}

enum ThreadReturn<'gc> {
    Unfinished,
    Finished(Vec<Value<'gc>>),
    Callback(Continuation<'gc, Vec<Value<'gc>>, Error>),
    TailCallback(Continuation<'gc, Vec<Value<'gc>>, Error>),
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

    fn run(
        &mut self,
        mc: MutationContext<'gc, '_>,
        self_thread: Thread<'gc>,
        mut instructions: u32,
    ) -> ThreadReturn<'gc> {
        'function_start: loop {
            let current_frame = *self.frames.last().expect("no current ThreadState frame");
            let current_function = get_closure(self.stack[current_frame.bottom]);

            loop {
                let op = current_function.0.proto.opcodes[self.pc];
                self.pc += 1;

                match op {
                    OpCode::Move { dest, source } => {
                        self.stack[current_frame.base + dest.0 as usize] =
                            self.stack[current_frame.base + source.0 as usize];
                    }

                    OpCode::LoadConstant { dest, constant } => {
                        self.stack[current_frame.base + dest.0 as usize] =
                            current_function.0.proto.constants[constant.0 as usize].to_value();
                    }

                    OpCode::LoadBool {
                        dest,
                        value,
                        skip_next,
                    } => {
                        self.stack[current_frame.base + dest.0 as usize] = Value::Boolean(value);
                        if skip_next {
                            self.pc += 1;
                        }
                    }

                    OpCode::LoadNil { dest, count } => {
                        for i in dest.0..dest.0 + count {
                            self.stack[current_frame.base + i as usize] = Value::Nil;
                        }
                    }

                    OpCode::NewTable { dest } => {
                        self.stack[current_frame.base + dest.0 as usize] =
                            Value::Table(Table::new(mc));
                    }

                    OpCode::GetTableR { dest, table, key } => {
                        self.stack[current_frame.base + dest.0 as usize] =
                            get_table(self.stack[current_frame.base + table.0 as usize])
                                .get(self.stack[current_frame.base + key.0 as usize]);
                    }

                    OpCode::GetTableC { dest, table, key } => {
                        self.stack[current_frame.base + dest.0 as usize] =
                            get_table(self.stack[current_frame.base + table.0 as usize])
                                .get(current_function.0.proto.constants[key.0 as usize].to_value())
                    }

                    OpCode::SetTableRR { table, key, value } => {
                        get_table(self.stack[current_frame.base + table.0 as usize])
                            .set(
                                mc,
                                self.stack[current_frame.base + key.0 as usize],
                                self.stack[current_frame.base + value.0 as usize],
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableRC { table, key, value } => {
                        get_table(self.stack[current_frame.base + table.0 as usize])
                            .set(
                                mc,
                                self.stack[current_frame.base + key.0 as usize],
                                current_function.0.proto.constants[value.0 as usize].to_value(),
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableCR { table, key, value } => {
                        get_table(self.stack[current_frame.base + table.0 as usize])
                            .set(
                                mc,
                                current_function.0.proto.constants[key.0 as usize].to_value(),
                                self.stack[current_frame.base + value.0 as usize],
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableCC { table, key, value } => {
                        get_table(self.stack[current_frame.base + table.0 as usize])
                            .set(
                                mc,
                                current_function.0.proto.constants[key.0 as usize].to_value(),
                                current_function.0.proto.constants[value.0 as usize].to_value(),
                            )
                            .expect("could not set table value");
                    }

                    OpCode::GetUpTableR { dest, table, key } => {
                        self.stack[current_frame.base + dest.0 as usize] =
                            get_table(self.get_upvalue(
                                self_thread,
                                current_function.0.upvalues[table.0 as usize],
                            ))
                            .get(self.stack[current_frame.base + key.0 as usize]);
                    }

                    OpCode::GetUpTableC { dest, table, key } => {
                        self.stack[current_frame.base + dest.0 as usize] =
                            get_table(self.get_upvalue(
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
                            self.stack[current_frame.base + key.0 as usize],
                            self.stack[current_frame.base + value.0 as usize],
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
                            self.stack[current_frame.base + key.0 as usize],
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
                            self.stack[current_frame.base + value.0 as usize],
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
                        match self.call_function(
                            current_frame.base + func.0 as usize,
                            args,
                            returns,
                            self.pc,
                            false,
                        ) {
                            ThreadReturn::Unfinished => continue 'function_start,
                            ret => return ret,
                        }
                    }

                    OpCode::TailCall { func, args } => {
                        self.close_upvalues(mc, self_thread, current_frame.bottom);

                        let func = current_frame.base + func.0 as usize;
                        let arg_len = if let Some(args) = args.to_constant() {
                            args as usize
                        } else {
                            self.stack.len() - func - 1
                        };

                        self.stack[current_frame.bottom] = self.stack[func];
                        for i in 0..arg_len {
                            self.stack[current_frame.bottom + 1 + i] = self.stack[func + 1 + i];
                        }
                        self.stack.truncate(current_frame.bottom + 1 + arg_len);
                        self.frames.pop();

                        match self.call_function(
                            current_frame.bottom,
                            args,
                            current_frame.returns,
                            current_frame.restore_pc,
                            current_frame.call_boundary,
                        ) {
                            ThreadReturn::Unfinished => continue 'function_start,
                            ret => return ret,
                        }
                    }

                    OpCode::Return { start, count } => {
                        self.close_upvalues(mc, self_thread, current_frame.bottom);
                        self.pc = current_frame.restore_pc;
                        self.frames.pop();

                        let start = current_frame.base + start.0 as usize;
                        let count = count
                            .to_constant()
                            .map(|c| c as usize)
                            .unwrap_or(self.stack.len() - start);

                        let returning = current_frame
                            .returns
                            .to_constant()
                            .map(|c| c as usize)
                            .unwrap_or(count);

                        if current_frame.call_boundary {
                            let ret_vals = self.stack[start..start + returning].to_vec();

                            if let Some(frame) = self.frames.last() {
                                self.stack.resize(frame.top, Value::Nil);
                            } else {
                                self.stack.clear();
                            }

                            return ThreadReturn::Finished(ret_vals);
                        } else {
                            for i in 0..returning.min(count) {
                                self.stack[current_frame.bottom + i] = self.stack[start + i]
                            }

                            for i in count..returning {
                                self.stack[current_frame.bottom + i] = Value::Nil;
                            }

                            // Set the correct stack size for variable returns, otherwise restore
                            // the previous stack top.
                            if current_frame.returns.is_variable() {
                                self.stack.truncate(current_frame.bottom + returning);
                            } else {
                                let current_frame = self
                                    .frames
                                    .last()
                                    .expect("top frame was not a call boundary");
                                self.stack.resize(current_frame.top, Value::Nil);
                            }

                            continue 'function_start;
                        }
                    }

                    OpCode::VarArgs { dest, count } => {
                        let varargs_start = current_frame.bottom + 1;
                        let varargs_len = current_frame.base - varargs_start;
                        let dest = current_frame.base + dest.0 as usize;
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
                            self.close_upvalues(mc, self_thread, current_frame.base + r as usize);
                        }
                    }

                    OpCode::Test { value, is_true } => {
                        let value = self.stack[current_frame.base + value.0 as usize];
                        if value.to_bool() == is_true {
                            self.pc += 1;
                        }
                    }

                    OpCode::TestSet {
                        dest,
                        value,
                        is_true,
                    } => {
                        let value = self.stack[current_frame.base + value.0 as usize];
                        if value.to_bool() == is_true {
                            self.pc += 1;
                        } else {
                            self.stack[current_frame.base + dest.0 as usize] = value;
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
                                    let ind = current_frame.base + reg.0 as usize;
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
                        self.stack[current_frame.base + dest.0 as usize] = Value::Closure(closure);
                    }

                    OpCode::NumericForPrep { base, jump } => {
                        let base = current_frame.base + base.0 as usize;
                        self.stack[base] = self.stack[base]
                            .subtract(self.stack[base + 2])
                            .expect("non numeric for loop parameters");
                        self.pc = add_offset(self.pc, jump);
                    }

                    OpCode::NumericForLoop { base, jump } => {
                        const ERR_MSG: &str = "non numeric for loop parameter";

                        let base = current_frame.base + base.0 as usize;
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
                        let base = current_frame.base + base.0 as usize;
                        self.stack.resize(base + 6, Value::Nil);
                        for i in 0..3 {
                            self.stack[base + 3 + i] = self.stack[base + i];
                        }
                        match self.call_function(
                            base + 3,
                            VarCount::constant(2),
                            VarCount::constant(var_count),
                            self.pc,
                            false,
                        ) {
                            ThreadReturn::Unfinished => continue 'function_start,
                            ret => return ret,
                        }
                    }

                    OpCode::GenericForLoop { base, jump } => {
                        let base = current_frame.base + base.0 as usize;
                        if self.stack[base + 1].to_bool() {
                            self.stack[base] = self.stack[base + 1];
                            self.pc = add_offset(self.pc, jump);
                        }
                    }

                    OpCode::SelfR { base, table, key } => {
                        let base = current_frame.base + base.0 as usize;
                        let table = self.stack[current_frame.base + table.0 as usize];
                        let key = current_function.0.proto.constants[key.0 as usize].to_value();
                        self.stack[base + 1] = table;
                        self.stack[base] = get_table(table).get(key);
                    }

                    OpCode::SelfC { base, table, key } => {
                        let base = current_frame.base + base.0 as usize;
                        let table = self.stack[current_frame.base + table.0 as usize];
                        let key = current_function.0.proto.constants[key.0 as usize].to_value();
                        self.stack[base + 1] = table;
                        self.stack[base] = get_table(table).get(key);
                    }

                    OpCode::Concat {
                        dest,
                        source,
                        count,
                    } => {
                        self.stack[current_frame.base + dest.0 as usize] = Value::String(
                            String::concat(
                                mc,
                                &self.stack[current_frame.base + source.0 as usize
                                    ..current_frame.base + source.0 as usize + count as usize],
                            )
                            .unwrap(),
                        );
                    }

                    OpCode::GetUpValue { source, dest } => {
                        self.stack[current_frame.base + dest.0 as usize] = self.get_upvalue(
                            self_thread,
                            current_function.0.upvalues[source.0 as usize],
                        );
                    }

                    OpCode::SetUpValue { source, dest } => {
                        let val = self.stack[current_frame.base + source.0 as usize];
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
                        self.stack[current_frame.base + dest.0 as usize] = Value::Integer(
                            get_table(self.stack[current_frame.base + source.0 as usize]).length(),
                        );
                    }

                    OpCode::EqRR {
                        skip_if,
                        left,
                        right,
                    } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        if (left == right) == skip_if {
                            self.pc += 1;
                        }
                    }

                    OpCode::EqRC {
                        skip_if,
                        left,
                        right,
                    } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
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
                        let right = self.stack[current_frame.base + right.0 as usize];
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
                        let source = self.stack[current_frame.base + source.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] = source.not();
                    }

                    OpCode::AddRR { dest, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddRC { dest, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[current_frame.base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = self.stack[current_frame.base + right.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddCC { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[current_frame.base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::SubRR { dest, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::SubRC { dest, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[current_frame.base + dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::SubCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = self.stack[current_frame.base + right.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::SubCC { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[current_frame.base + dest.0 as usize] = left
                            .subtract(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulRR { dest, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulRC { dest, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[current_frame.base + dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = self.stack[current_frame.base + right.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }

                    OpCode::MulCC { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize].to_value();
                        let right = current_function.0.proto.constants[right.0 as usize].to_value();
                        self.stack[current_frame.base + dest.0 as usize] = left
                            .multiply(right)
                            .expect("could not apply binary operator");
                    }
                }

                if instructions == 0 {
                    return ThreadReturn::Unfinished;
                } else {
                    instructions -= 1
                }
            }
        }
    }

    fn call_function(
        &mut self,
        function_index: usize,
        args: VarCount,
        returns: VarCount,
        restore_pc: usize,
        call_boundary: bool,
    ) -> ThreadReturn<'gc> {
        match self.stack[function_index] {
            Value::Closure(_) => {
                self.call_closure(function_index, args, returns, restore_pc, call_boundary);
                ThreadReturn::Unfinished
            }
            Value::Callback(_) => {
                self.call_callback(function_index, args, returns, restore_pc, call_boundary)
            }
            _ => panic!("not a closure or callback"),
        }
    }

    fn call_closure(
        &mut self,
        function_index: usize,
        args: VarCount,
        returns: VarCount,
        restore_pc: usize,
        call_boundary: bool,
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
            base,
            top,
            returns,
            restore_pc,
            call_boundary,
        });

        self.pc = 0;
    }

    fn call_callback(
        &mut self,
        function_index: usize,
        args: VarCount,
        returns: VarCount,
        restore_pc: usize,
        call_boundary: bool,
    ) -> ThreadReturn<'gc> {
        let callback = get_callback(self.stack[function_index]);
        let arg_count = args
            .to_constant()
            .map(|c| c as usize)
            .unwrap_or(self.stack.len() - function_index - 1);

        match callback
            .call(&self.stack[function_index + 1..function_index + 1 + arg_count])
            .expect("callback errors not handled yet")
        {
            ContinuationResult::Finish(res) => {
                if call_boundary {
                    ThreadReturn::Finished(res)
                } else {
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
                    ThreadReturn::Unfinished
                }
            }
            ContinuationResult::Continue(cont) => {
                if call_boundary {
                    ThreadReturn::TailCallback(cont)
                } else {
                    self.frames.push(Frame {
                        bottom: function_index,
                        base: function_index + 1,
                        top: function_index + 1,
                        returns,
                        restore_pc,
                        call_boundary,
                    });
                    self.stack.resize(function_index + 1, Value::Nil);
                    ThreadReturn::Callback(cont)
                }
            }
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
