use gc_arena::{GcCell, MutationContext};

use function::Closure;
use opcode::{OpCode, VarCount};
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
        self.0.write(mc).run(instruction_limit)
    }

    pub fn results(&self) -> Option<Vec<Value<'gc>>> {
        self.0.read().results().map(|v| v.to_vec())
    }
}

#[derive(Debug, Clone, Copy, Collect)]
struct Frame {
    pc: usize,
    bottom: usize,
    varargs: usize,
    base: usize,
    returns: VarCount,
}

#[derive(Debug, Collect)]
#[collect(empty_drop)]
struct ThreadState<'gc> {
    stack: Vec<Value<'gc>>,
    frames: Vec<Frame>,
}

impl<'gc> ThreadState<'gc> {
    fn new(closure: Closure<'gc>, args: &[Value<'gc>]) -> ThreadState<'gc> {
        let mut stack = vec![Value::Closure(closure)];
        stack.extend(args);
        let mut state = ThreadState {
            stack,
            frames: Vec::new(),
        };

        state.call_function(0, VarCount::make_variable(), VarCount::make_variable());

        state
    }

    fn run(&mut self, mut instruction_limit: Option<u64>) -> bool {
        'function_start: loop {
            let mut current_frame = if let Some(top) = self.frames.last().cloned() {
                top
            } else {
                return true;
            };

            let current_function = match self.stack[current_frame.bottom] {
                Value::Closure(c) => c,
                _ => unreachable!(),
            };

            loop {
                match current_function.0.proto.opcodes[current_frame.pc] {
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
                            current_frame.pc += 1;
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
                        self.call_function(current_frame.base + func as usize, args, returns);
                        continue 'function_start;
                    }

                    OpCode::Return { start, count } => {
                        let return_count = count
                            .get_constant()
                            .map(|c| c as usize)
                            .unwrap_or(self.stack.len() - start as usize);
                        let expected = current_frame
                            .returns
                            .get_constant()
                            .map(|c| c as usize)
                            .unwrap_or(return_count);
                        for i in 0..expected.min(return_count) {
                            self.stack[current_frame.bottom + i] =
                                self.stack[current_frame.base + start as usize + i]
                        }
                        self.stack
                            .resize(current_frame.bottom + expected, Value::Nil);
                        for i in return_count..expected {
                            self.stack[current_frame.bottom + i] = Value::Nil;
                        }

                        self.frames.pop();
                        continue 'function_start;
                    }
                }

                current_frame.pc += 1;

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

    fn call_function(&mut self, closure_index: usize, args: VarCount, returns: VarCount) {
        let closure = match self.stack[closure_index] {
            Value::Closure(c) => c,
            _ => panic!("not a closure"),
        };
        let proto = closure.0.proto;

        let arg_count = if let Some(constant) = args.get_constant() {
            let constant = constant as usize;
            assert!(self.stack.len() - closure_index - 1 >= constant);
            self.stack.truncate(closure_index + constant + 1);
            constant
        } else {
            self.stack.len() - closure_index - 1
        };

        let fixed_params = closure.0.proto.fixed_params as usize;

        if arg_count < fixed_params {
            self.stack.resize(fixed_params - arg_count, Value::Nil);
        } else if fixed_params < arg_count {
            for i in 0..fixed_params {
                let v = self.stack[closure_index + 1 + i];
                self.stack.push(v);
            }
        }

        let stack_top = self.stack.len() + proto.max_register as usize + 1;
        self.stack.resize(stack_top, Value::Nil);

        self.frames.push(Frame {
            pc: 0,
            bottom: closure_index,
            varargs: closure_index + 1 + fixed_params,
            base: closure_index + 1 + arg_count,
            returns,
        });
    }
}
