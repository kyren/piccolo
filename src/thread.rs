use std::collections::btree_map::Entry as BTreeEntry;
use std::collections::BTreeMap;

use failure::Error;

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::function::{Closure, ClosureState, UpValue, UpValueDescriptor, UpValueState};
use crate::opcode::{OpCode, VarCount};
use crate::sequence::Sequence;
use crate::value::Value;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Thread<'gc>(GcCell<'gc, ThreadState<'gc>>);

impl<'gc> PartialEq for Thread<'gc> {
    fn eq(&self, other: &Thread<'gc>) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
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
    pub fn call_function(
        &self,
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
        state.call_function(
            closure_index,
            VarCount::make_variable(),
            VarCount::make_variable(),
            res_pc,
            true,
        );

        ThreadSequence {
            thread: Some(*self),
            granularity,
        }
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct ThreadSequence<'gc> {
    thread: Option<Thread<'gc>>,
    granularity: u32,
}

impl<'gc> Sequence<'gc> for ThreadSequence<'gc> {
    type Item = Vec<Value<'gc>>;

    fn pump(&mut self, mc: MutationContext<'gc, '_>) -> Option<Result<Vec<Value<'gc>>, Error>> {
        let thread = self.thread.expect("cannot pump a finished ThreadSequence");
        let mut state = thread.0.write(mc);
        if let Some(res) = state.run(mc, thread, self.granularity) {
            self.thread = None;
            Some(Ok(res))
        } else {
            None
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
    ) -> Option<Vec<Value<'gc>>> {
        'function_start: loop {
            let current_frame = self
                .frames
                .last()
                .expect("no current ThreadState frame")
                .clone();

            let current_function = match self.stack[current_frame.bottom] {
                Value::Closure(c) => c,
                _ => unreachable!(),
            };

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
                            current_function.0.proto.constants[constant.0 as usize];
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

                    OpCode::Call {
                        func,
                        args,
                        returns,
                    } => {
                        self.call_function(
                            current_frame.base + func.0 as usize,
                            args,
                            returns,
                            self.pc,
                            false,
                        );
                        continue 'function_start;
                    }

                    OpCode::Return { start, count } => {
                        for (_, upval) in self.open_upvalues.split_off(&current_frame.bottom) {
                            let mut upval = upval.0.write(mc);
                            if let UpValueState::Open(thread, ind) = *upval {
                                *upval = UpValueState::Closed(if thread == self_thread {
                                    self.stack[ind]
                                } else {
                                    thread.0.read().stack[ind]
                                });
                            }
                        }

                        let start = current_frame.base + start.0 as usize;
                        let count = count
                            .get_constant()
                            .map(|c| c as usize)
                            .unwrap_or(self.stack.len() - start);

                        let returning = current_frame
                            .returns
                            .get_constant()
                            .map(|c| c as usize)
                            .unwrap_or(count);

                        if current_frame.call_boundary {
                            let ret_vals = self.stack[start..start + returning].to_vec();

                            self.pc = current_frame.restore_pc;
                            self.frames.pop();
                            if let Some(frame) = self.frames.last() {
                                self.stack.truncate(frame.top);
                            } else {
                                self.stack.clear();
                            }

                            return Some(ret_vals);
                        } else {
                            for i in 0..returning.min(count) {
                                self.stack[current_frame.bottom + i] = self.stack[start + i]
                            }

                            for i in count..returning {
                                self.stack[current_frame.bottom + i] = Value::Nil;
                            }

                            self.pc = current_frame.restore_pc;
                            self.frames.pop();

                            // If variable returns were expected, then we set the stack top to
                            // indicate the number of variable returns.  If we are returning with an
                            // expected number of restuls, then we should reset the stack size to
                            // the size expeted by the previous frame.  The top set when there are
                            // variable results may be lower than the top expected by the previous
                            // frame, but this is okay because all variable results ops are
                            // immediately followed by subsequent ops that consume the variable
                            // results.  Any operation that consumes variable results without
                            // producing variable results is expected to reset the stack to the
                            // correct normal top.
                            if current_frame.returns.is_variable() {
                                self.stack.truncate(current_frame.bottom + returning);
                            } else {
                                let current_frame =
                                    self.frames.last().expect("top frame is not call boundary");
                                self.stack.truncate(current_frame.top);
                            }

                            continue 'function_start;
                        }
                    }

                    OpCode::Jump { offset } => {
                        if offset > 0 {
                            self.pc = self.pc.checked_add(offset as usize).unwrap();
                        } else if offset < 0 {
                            self.pc = self.pc.checked_sub(-offset as usize).unwrap();
                        }
                    }

                    OpCode::Test { value, is_true } => {
                        let value = self.stack[current_frame.base + value.0 as usize];
                        if value.as_bool() == is_true {
                            self.pc += 1;
                        }
                    }

                    OpCode::TestSet {
                        dest,
                        value,
                        is_true,
                    } => {
                        let value = self.stack[current_frame.base + value.0 as usize];
                        if value.as_bool() == is_true {
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

                    OpCode::GetUpValue { source, dest } => {
                        self.stack[current_frame.base + dest.0 as usize] =
                            match *current_function.0.upvalues[source.0 as usize].0.read() {
                                UpValueState::Open(thread, ind) => {
                                    if thread == self_thread {
                                        self.stack[ind]
                                    } else {
                                        thread.0.read().stack[ind]
                                    }
                                }
                                UpValueState::Closed(v) => v,
                            };
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

                    OpCode::EqRR { equal, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        if (left == right) != equal {
                            self.pc += 1;
                        }
                    }

                    OpCode::EqRC { equal, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize];
                        if (left == right) != equal {
                            self.pc += 1;
                        }
                    }

                    OpCode::EqCR { equal, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        if (left == right) != equal {
                            self.pc += 1;
                        }
                    }
                    OpCode::Not { dest, source } => {
                        let source = self.stack[current_frame.base + source.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] = source.negate();
                    }

                    OpCode::AddRR { dest, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddRC { dest, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
                    }

                    OpCode::AddCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        self.stack[current_frame.base + dest.0 as usize] =
                            left.add(right).expect("could not apply binary operator");
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

    fn call_function(
        &mut self,
        closure_index: usize,
        args: VarCount,
        returns: VarCount,
        restore_pc: usize,
        call_boundary: bool,
    ) {
        let closure = match self.stack[closure_index] {
            Value::Closure(c) => c,
            _ => panic!("not a closure"),
        };

        let arg_count = if let Some(constant) = args.get_constant() {
            let constant = constant as usize;
            assert!(self.stack.len() - closure_index - 1 >= constant);
            self.stack.truncate(closure_index + constant + 1);
            constant
        } else {
            self.stack.len() - closure_index - 1
        };

        let fixed_params = closure.0.proto.fixed_params as usize;

        let base = if arg_count <= fixed_params {
            if arg_count < fixed_params {
                let len = self.stack.len();
                self.stack
                    .resize(len + fixed_params - arg_count, Value::Nil);
            }
            closure_index + 1
        } else {
            self.stack[closure_index + 1..].rotate_left(fixed_params);
            closure_index + 1 + (arg_count - fixed_params)
        };

        let top = base + closure.0.proto.stack_size as usize;
        self.stack.resize(top, Value::Nil);

        self.frames.push(Frame {
            bottom: closure_index,
            base,
            top,
            returns,
            restore_pc,
            call_boundary,
        });

        self.pc = 0;
    }
}
