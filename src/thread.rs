use std::collections::btree_map::Entry as BTreeEntry;
use std::collections::BTreeMap;

use gc_arena::{Gc, GcCell, MutationContext};

use function::{Closure, ClosureState, UpValue, UpValueDescriptor, UpValueState};
use opcode::{OpCode, VarCount};
use operators::{apply_binop, BinaryOperator};
use value::Value;

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
                        self.stack[current_frame.base + dest as usize] =
                            self.stack[current_frame.base + source as usize];
                    }

                    OpCode::LoadConstant { dest, constant } => {
                        self.stack[current_frame.base + dest as usize] =
                            current_function.0.proto.constants[constant as usize];
                    }

                    OpCode::LoadBool {
                        dest,
                        value,
                        skip_next,
                    } => {
                        self.stack[current_frame.base + dest as usize] = Value::Boolean(value);
                        if skip_next {
                            self.pc += 1;
                        }
                    }

                    OpCode::LoadNil { dest, count } => {
                        for i in dest..dest + count {
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
                            current_frame.base + func as usize,
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
                            .unwrap_or(self.stack.len() - current_frame.base - start as usize);
                        let expected = current_frame
                            .returns
                            .get_constant()
                            .map(|c| c as usize)
                            .unwrap_or(return_count);
                        for i in 0..expected.min(return_count) {
                            self.stack[current_frame.bottom + i] =
                                self.stack[current_frame.base + start as usize + i]
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

                    OpCode::Closure { proto, dest } => {
                        let proto = current_function.0.proto.prototypes[proto as usize];
                        let mut upvalues = Vec::new();
                        for &desc in &proto.upvalues {
                            match desc {
                                UpValueDescriptor::ParentLocal(reg) => {
                                    let ind = current_frame.base + reg as usize;
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
                                    upvalues.push(current_function.0.upvalues[uvindex as usize]);
                                }
                            }
                        }

                        let closure = Closure(Gc::allocate(mc, ClosureState { proto, upvalues }));
                        self.stack[current_frame.base + dest as usize] = Value::Closure(closure);
                    }

                    OpCode::GetUpValue { source, dest } => {
                        self.stack[current_frame.base + dest as usize] =
                            match *current_function.0.upvalues[source as usize].0.read() {
                                UpValueState::Open(ind) => self.stack[ind],
                                UpValueState::Closed(v) => v,
                            };
                    }

                    OpCode::SetUpValue { source, dest } => {
                        let val = self.stack[current_frame.base + source as usize];
                        let mut uv = current_function.0.upvalues[dest as usize].0.write(mc);
                        match &mut *uv {
                            UpValueState::Open(ind) => self.stack[*ind] = val,
                            UpValueState::Closed(v) => *v = val,
                        }
                    }

                    OpCode::AddRR { dest, left, right } => {
                        let left = self.stack[current_frame.base + left as usize];
                        let right = self.stack[current_frame.base + right as usize];
                        self.stack[current_frame.base + dest as usize] =
                            apply_binop(BinaryOperator::Add, left, right)
                                .expect("could not apply add operator");
                    }

                    OpCode::AddRC { dest, left, right } => {
                        let left = self.stack[current_frame.base + left as usize];
                        let right = current_function.0.proto.constants[right as usize];
                        self.stack[current_frame.base + dest as usize] =
                            apply_binop(BinaryOperator::Add, left, right)
                                .expect("could not apply add operator");
                    }

                    OpCode::AddCR { dest, left, right } => {
                        let left = current_function.0.proto.constants[left as usize];
                        let right = self.stack[current_frame.base + right as usize];
                        self.stack[current_frame.base + dest as usize] =
                            apply_binop(BinaryOperator::Add, left, right)
                                .expect("could not apply add operator");
                    }
                }

                self.pc += 1;

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
