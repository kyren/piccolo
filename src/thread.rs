use std::collections::btree_map::Entry as BTreeEntry;
use std::collections::BTreeMap;
use std::fmt::{self, Debug};

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::{
    Callback, CallbackResult, CallbackReturn, Closure, ClosureState, Error, LuaContext, OpCode,
    Sequence, String, Table, UpValue, UpValueDescriptor, UpValueState, Value, VarCount,
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
        Thread(GcCell::allocate(
            mc,
            ThreadState {
                stack: Vec::new(),
                frames: Vec::new(),
                open_upvalues: BTreeMap::new(),
            },
        ))
    }

    /// Call a closure on this thread, producing a `Sequence`.
    ///
    /// The same `Thread` can be used for multiple function calls, but only the most recently
    /// created unfinished `Sequence` for a `Thread` can be run at any given time.  When such a
    /// sequence is constructed, it operates on whatever the top of the stack is at that time, so
    /// any later constructed sequences must be run to completion before earlier ones can be
    /// completed.
    pub fn call_closure(
        self,
        mc: MutationContext<'gc, '_>,
        closure: Closure<'gc>,
        args: &[Value<'gc>],
    ) -> impl Sequence<'gc, Item = Vec<Value<'gc>>, Error = Error> {
        let mut state = self.0.write(mc);
        let closure_index = state.stack.len();
        state.stack.push(Value::Closure(closure));
        state.stack.extend(args);
        self.closure_call(
            &mut state,
            closure_index,
            VarCount::variable(),
            FrameReturn::CallBoundary,
        );

        ThreadSequence {
            thread: self,
            pending_callback: None,
            current_frame: Some(state.frames.len() - 1),
        }
    }

    fn step_lua(
        self,
        state: &mut ThreadState<'gc>,
        mc: MutationContext<'gc, '_>,
    ) -> Result<ThreadResult<'gc>, Error> {
        const THREAD_GRANULARITY: u32 = 64;
        let mut instructions = THREAD_GRANULARITY;

        'start: loop {
            let current_frame = state
                .frames
                .last_mut()
                .expect("no current ThreadState frame");
            let stack_bottom = current_frame.bottom;
            let frame_return = current_frame.frame_return;
            let (stack_base, pc) = match &mut current_frame.frame_type {
                FrameType::Lua { base, pc } => (*base, pc),
                _ => panic!("step_lua called when top frame is not a callback"),
            };
            let current_function = get_closure(state.stack[stack_bottom]);
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
                        stack_frame[dest.0 as usize] = get_table(stack_frame[table.0 as usize])
                            .get(stack_frame[key.0 as usize]);
                    }

                    OpCode::GetTableC { dest, table, key } => {
                        stack_frame[dest.0 as usize] = get_table(stack_frame[table.0 as usize])
                            .get(current_function.0.proto.constants[key.0 as usize].to_value())
                    }

                    OpCode::SetTableRR { table, key, value } => {
                        get_table(stack_frame[table.0 as usize])
                            .set(
                                mc,
                                stack_frame[key.0 as usize],
                                stack_frame[value.0 as usize],
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableRC { table, key, value } => {
                        get_table(stack_frame[table.0 as usize])
                            .set(
                                mc,
                                stack_frame[key.0 as usize],
                                current_function.0.proto.constants[value.0 as usize].to_value(),
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableCR { table, key, value } => {
                        get_table(stack_frame[table.0 as usize])
                            .set(
                                mc,
                                current_function.0.proto.constants[key.0 as usize].to_value(),
                                stack_frame[value.0 as usize],
                            )
                            .expect("could not set table value");
                    }

                    OpCode::SetTableCC { table, key, value } => {
                        get_table(stack_frame[table.0 as usize])
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
                        ))
                        .get(stack_frame[key.0 as usize]);
                    }

                    OpCode::GetUpTableC { dest, table, key } => {
                        stack_frame[dest.0 as usize] = get_table(get_upvalue(
                            self,
                            upper_stack,
                            current_function.0.upvalues[table.0 as usize],
                        ))
                        .get(current_function.0.proto.constants[key.0 as usize].to_value())
                    }

                    OpCode::SetUpTableRR { table, key, value } => {
                        get_table(get_upvalue(
                            self,
                            upper_stack,
                            current_function.0.upvalues[table.0 as usize],
                        ))
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
                        ))
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
                        ))
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
                        match self.function_call(
                            state,
                            stack_base + func.0 as usize,
                            args,
                            FrameReturn::Upper(returns),
                        )? {
                            ThreadResult::None => continue 'start,
                            ret => return Ok(ret),
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
                        state.frames.pop();

                        match self.function_call(state, stack_bottom, args, frame_return)? {
                            ThreadResult::None => continue 'start,
                            ret => return Ok(ret),
                        }
                    }

                    OpCode::Return { start, count } => {
                        self.close_upvalues(state, mc, stack_bottom);
                        state.frames.pop();

                        let start = stack_base + start.0 as usize;
                        let count = count
                            .to_constant()
                            .map(|c| c as usize)
                            .unwrap_or(state.stack.len() - start);

                        match frame_return {
                            FrameReturn::CallBoundary => {
                                let ret_vals = state.stack[start..start + count].to_vec();

                                if let Some(frame) = state.frames.last() {
                                    state.stack.resize(frame.top, Value::Nil);
                                } else {
                                    state.stack.clear();
                                }

                                return Ok(ThreadResult::Finished(ret_vals));
                            }
                            FrameReturn::Upper(returns) => {
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
                        stack_frame[dest.0 as usize] = Value::Closure(closure);
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
                        match self.function_call(
                            state,
                            base + 3,
                            VarCount::constant(2),
                            FrameReturn::Upper(VarCount::constant(var_count)),
                        )? {
                            ThreadResult::None => continue 'start,
                            ret => return Ok(ret),
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
                        stack_frame[base.0 as usize] = get_table(table).get(key);
                    }

                    OpCode::SelfC { base, table, key } => {
                        let table = stack_frame[table.0 as usize];
                        let key = current_function.0.proto.constants[key.0 as usize].to_value();
                        stack_frame[base.0 as usize + 1] = table;
                        stack_frame[base.0 as usize] = get_table(table).get(key);
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
                            Value::Integer(get_table(stack_frame[source.0 as usize]).length());
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
                    return Ok(ThreadResult::None);
                } else {
                    instructions -= 1
                }
            }
        }
    }

    fn function_call(
        self,
        state: &mut ThreadState<'gc>,
        function_index: usize,
        args: VarCount,
        frame_return: FrameReturn,
    ) -> Result<ThreadResult<'gc>, Error> {
        match state.stack[function_index] {
            Value::Closure(_) => {
                self.closure_call(state, function_index, args, frame_return);
                Ok(ThreadResult::None)
            }
            Value::Callback(_) => self.callback_call(state, function_index, args, frame_return),
            _ => panic!("not a closure or callback"),
        }
    }

    fn closure_call(
        self,
        state: &mut ThreadState<'gc>,
        function_index: usize,
        args: VarCount,
        frame_return: FrameReturn,
    ) {
        let closure = get_closure(state.stack[function_index]);
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
            top,
            frame_type: FrameType::Lua { base, pc: 0 },
            frame_return,
        });
    }

    fn callback_call(
        self,
        state: &mut ThreadState<'gc>,
        function_index: usize,
        args: VarCount,
        frame_return: FrameReturn,
    ) -> Result<ThreadResult<'gc>, Error> {
        let callback = get_callback(state.stack[function_index]);
        let arg_count = args
            .to_constant()
            .map(|c| c as usize)
            .unwrap_or(state.stack.len() - function_index - 1);

        match callback.call(
            self,
            &state.stack[function_index + 1..function_index + 1 + arg_count],
        ) {
            Err(err) => Err(err),
            Ok(res) => match res {
                CallbackReturn::Immediate(CallbackResult::Return(res)) => match frame_return {
                    FrameReturn::CallBoundary => Ok(ThreadResult::Finished(res)),
                    FrameReturn::Upper(returns) => {
                        let count = res.len();
                        if let Some(returning) = returns.to_constant() {
                            if let Some(current_frame) = state.frames.last() {
                                state.stack.resize(current_frame.top, Value::Nil);
                            }

                            let returning = returning as usize;
                            for i in 0..returning.min(count) {
                                state.stack[function_index + i] = res[i];
                            }
                            for i in count..returning {
                                state.stack[function_index + i] = Value::Nil;
                            }
                        } else {
                            state.stack.resize(function_index + count, Value::Nil);
                            for i in 0..count {
                                state.stack[function_index + i] = res[i];
                            }
                        }

                        Ok(ThreadResult::None)
                    }
                },
                CallbackReturn::Immediate(CallbackResult::Yield(res)) => {
                    state.frames.push(Frame {
                        bottom: function_index,
                        top: function_index,
                        frame_type: FrameType::Yield,
                        frame_return,
                    });
                    state.stack.resize(function_index, Value::Nil);
                    Ok(ThreadResult::Finished(res))
                }
                CallbackReturn::Sequence(seq) => {
                    state.frames.push(Frame {
                        bottom: function_index,
                        top: function_index,
                        frame_type: FrameType::Callback,
                        frame_return,
                    });
                    state.stack.resize(function_index, Value::Nil);
                    Ok(ThreadResult::PendingCallback(seq))
                }
            },
        }
    }

    // Unwind frames up to and including the most recent call boundary
    fn unwind(self, state: &mut ThreadState<'gc>, mc: MutationContext<'gc, '_>) {
        loop {
            let frame = state
                .frames
                .pop()
                .expect("no call boundary found during unwind");
            if frame.frame_return == FrameReturn::CallBoundary {
                self.close_upvalues(state, mc, frame.bottom);
                break;
            }
        }

        if let Some(top) = state.frames.last().map(|f| f.top) {
            state.stack.resize(top, Value::Nil);
        }
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
}

enum ThreadResult<'gc> {
    None,
    Finished(Vec<Value<'gc>>),
    PendingCallback(Box<Sequence<'gc, Item = CallbackResult<'gc>, Error = Error> + 'gc>),
}

#[derive(Collect)]
#[collect(empty_drop)]
struct ThreadSequence<'gc> {
    thread: Thread<'gc>,
    pending_callback: Option<Box<Sequence<'gc, Item = CallbackResult<'gc>, Error = Error> + 'gc>>,
    current_frame: Option<usize>,
}

impl<'gc> Sequence<'gc> for ThreadSequence<'gc> {
    type Item = Vec<Value<'gc>>;
    type Error = Error;

    fn step(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Self::Error>> {
        let current_frame = self.current_frame.expect("cannot step finished sequence");
        let mut state = self.thread.0.write(mc);

        if let Some(callback) = self.pending_callback.as_mut() {
            let callback_frame = state
                .frames
                .get_mut(current_frame)
                .expect("no callback frame");
            assert_eq!(callback_frame.frame_type, FrameType::Callback);

            match callback.step(mc, lc) {
                None => None,
                Some(Err(err)) => {
                    self.thread.unwind(&mut state, mc);
                    self.pending_callback = None;
                    self.current_frame = None;
                    Some(Err(err))
                }
                Some(Ok(CallbackResult::Yield(res))) => {
                    callback_frame.frame_type = FrameType::Yield;
                    assert_eq!(
                        current_frame + 1,
                        state.frames.len(),
                        "cannot yield across callbacks"
                    );
                    self.pending_callback = None;
                    self.current_frame = None;
                    Some(Ok(res))
                }
                Some(Ok(CallbackResult::Return(res))) => {
                    assert_eq!(
                        current_frame + 1,
                        state.frames.len(),
                        "cannot return from lower frame"
                    );
                    let top_frame = state.frames.pop().unwrap();

                    let returns = match top_frame.frame_return {
                        FrameReturn::Upper(returns) => returns,
                        FrameReturn::CallBoundary => panic!("no frame to return to from callback"),
                    };
                    let return_len = returns
                        .to_constant()
                        .map(|c| c as usize)
                        .unwrap_or(res.len());

                    state.stack.truncate(top_frame.bottom);
                    state
                        .stack
                        .resize(top_frame.bottom + return_len, Value::Nil);

                    for i in 0..return_len.min(res.len()) {
                        state.stack[top_frame.bottom + i] = res[i];
                    }

                    // Stack size is already correct for variable returns, but if we are returning a
                    // constant number, we need to restore the previous stack top.
                    if !returns.is_variable() {
                        let current_frame_top = state
                            .frames
                            .last()
                            .expect("no frame to return to from callback")
                            .top;
                        state.stack.resize(current_frame_top, Value::Nil);
                    }
                    self.pending_callback = None;
                    self.current_frame = Some(state.frames.len() - 1);
                    None
                }
            }
        } else {
            assert_eq!(current_frame + 1, state.frames.len());
            match self.thread.step_lua(&mut state, mc) {
                Err(err) => {
                    self.thread.unwind(&mut state, mc);
                    self.current_frame = None;
                    Some(Err(err))
                }
                Ok(ThreadResult::None) => {
                    self.current_frame = Some(state.frames.len() - 1);
                    None
                }
                Ok(ThreadResult::Finished(res)) => {
                    self.current_frame = None;
                    Some(Ok(res))
                }
                Ok(ThreadResult::PendingCallback(callback)) => {
                    self.pending_callback = Some(callback);
                    self.current_frame = Some(state.frames.len() - 1);
                    None
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Collect)]
#[collect(require_copy)]
enum FrameType {
    Lua { base: usize, pc: usize },
    Callback,
    Yield,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Collect)]
#[collect(require_copy)]
enum FrameReturn {
    // Frame is a Thread entry-point, and returning should return all results to the caller
    CallBoundary,
    // Frame is a normal call frame within a thread, returning should return the given number of
    // results to the frame above
    Upper(VarCount),
}

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_copy)]
struct Frame {
    bottom: usize,
    top: usize,
    frame_type: FrameType,
    frame_return: FrameReturn,
}

#[derive(Collect)]
#[collect(empty_drop)]
struct ThreadState<'gc> {
    stack: Vec<Value<'gc>>,
    frames: Vec<Frame>,
    open_upvalues: BTreeMap<usize, UpValue<'gc>>,
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
