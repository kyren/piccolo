use std::collections::btree_map::Entry as BTreeEntry;
use std::collections::BTreeMap;

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::function::{Closure, ClosureState, UpValue, UpValueDescriptor, UpValueState};
use crate::opcode::{OpCode, VarCount};
use crate::value::Value;

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_copy)]
pub struct Thread<'gc>(GcCell<'gc, ThreadState<'gc>>);

impl<'gc> Thread<'gc> {
    pub fn new(
        mc: MutationContext<'gc, '_>,
        closure: Closure<'gc>,
        args: &[Value<'gc>],
    ) -> Thread<'gc> {
        Thread(GcCell::allocate(mc, ThreadState::new(closure, args)))
    }

    pub fn run(&self, mc: MutationContext<'gc, '_>, instruction_limit: Option<u64>) -> bool {
        self.0.write(mc).run(mc, instruction_limit)
    }

    pub fn results(&self) -> Option<Vec<Value<'gc>>> {
        self.0.read().results().map(|v| v.to_vec())
    }
}

#[derive(Debug, Clone, Copy, Collect)]
struct Frame {
    bottom: usize,
    base: usize,
    top: usize,
    returns: VarCount,
    restore_pc: usize,
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
    fn new(closure: Closure<'gc>, args: &[Value<'gc>]) -> ThreadState<'gc> {
        let mut stack = vec![Value::Closure(closure)];
        stack.extend(args);
        let mut state = ThreadState {
            stack,
            frames: Vec::new(),
            pc: 0,
            open_upvalues: BTreeMap::new(),
        };

        state.call_function(0, VarCount::make_variable(), VarCount::make_variable(), 0);

        state
    }

    fn run(&mut self, mc: MutationContext<'gc, '_>, mut instruction_limit: Option<u64>) -> bool {
        'function_start: loop {
            let current_frame = if let Some(top) = self.frames.last().cloned() {
                top
            } else {
                return true;
            };

            let current_function = match self.stack[current_frame.bottom] {
                Value::Closure(c) => c,
                _ => unreachable!(),
            };

            loop {
                match current_function.0.proto.opcodes[self.pc] {
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
                            self.pc = self.pc.checked_add(1).unwrap();
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
                        let ret_pc = self.pc + 1;
                        self.call_function(
                            current_frame.base + func.0 as usize,
                            args,
                            returns,
                            ret_pc,
                        );
                        self.pc = 0;
                        continue 'function_start;
                    }

                    OpCode::Return { start, count } => {
                        for (_, upval) in self.open_upvalues.split_off(&current_frame.bottom) {
                            let mut upval = upval.0.write(mc);
                            if let UpValueState::Open(ind) = *upval {
                                *upval = UpValueState::Closed(self.stack[ind]);
                            }
                        }

                        let return_count = count
                            .get_constant()
                            .map(|c| c as usize)
                            .unwrap_or(self.stack.len() - current_frame.base - start.0 as usize);
                        let expected = current_frame
                            .returns
                            .get_constant()
                            .map(|c| c as usize)
                            .unwrap_or(return_count);
                        for i in 0..expected.min(return_count) {
                            self.stack[current_frame.bottom + i] =
                                self.stack[current_frame.base + start.0 as usize + i]
                        }

                        for i in return_count..expected {
                            self.stack[current_frame.bottom + i] = Value::Nil;
                        }

                        self.pc = current_frame.restore_pc;
                        self.frames.pop();

                        if let Some(frame) = self.frames.last() {
                            let last_return = current_frame.bottom + expected;
                            self.stack.truncate(frame.top.max(last_return));
                        }
                        continue 'function_start;
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
                            self.pc = self.pc.checked_add(1).unwrap();
                        }
                    }

                    OpCode::TestSet {
                        dest,
                        value,
                        is_true,
                    } => {
                        let value = self.stack[current_frame.base + value.0 as usize];
                        if value.as_bool() == is_true {
                            self.pc = self.pc.checked_add(1).unwrap();
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
                                                UpValueState::Open(ind),
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
                                UpValueState::Open(ind) => self.stack[ind],
                                UpValueState::Closed(v) => v,
                            };
                    }

                    OpCode::SetUpValue { source, dest } => {
                        let val = self.stack[current_frame.base + source.0 as usize];
                        let mut uv = current_function.0.upvalues[dest.0 as usize].0.write(mc);
                        match &mut *uv {
                            UpValueState::Open(ind) => self.stack[*ind] = val,
                            UpValueState::Closed(v) => *v = val,
                        }
                    }

                    OpCode::EqRR { equal, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        if (left == right) != equal {
                            self.pc = self.pc.checked_add(1).unwrap();
                        }
                    }

                    OpCode::EqRC { equal, left, right } => {
                        let left = self.stack[current_frame.base + left.0 as usize];
                        let right = current_function.0.proto.constants[right.0 as usize];
                        if (left == right) != equal {
                            self.pc = self.pc.checked_add(1).unwrap();
                        }
                    }

                    OpCode::EqCR { equal, left, right } => {
                        let left = current_function.0.proto.constants[left.0 as usize];
                        let right = self.stack[current_frame.base + right.0 as usize];
                        if (left == right) != equal {
                            self.pc = self.pc.checked_add(1).unwrap();
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

                self.pc = self.pc.checked_add(1).unwrap();

                if let Some(instruction_limit) = instruction_limit.as_mut() {
                    if *instruction_limit == 0 {
                        return false;
                    } else {
                        *instruction_limit -= 1
                    }
                }
            }
        }
    }

    fn results(&self) -> Option<&[Value<'gc>]> {
        if self.frames.is_empty() {
            Some(&self.stack)
        } else {
            None
        }
    }

    fn call_function(
        &mut self,
        closure_index: usize,
        args: VarCount,
        returns: VarCount,
        restore_pc: usize,
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
        });
    }
}
