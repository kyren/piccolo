use gc_arena::{GcCell, MutationContext};

use function::Closure;
use opcode::OpCode;
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
    base: usize,
}

#[derive(Debug, Collect)]
#[collect(empty_drop)]
struct ThreadState<'gc> {
    stack: Vec<Value<'gc>>,
    frames: Vec<Frame>,
}

impl<'gc> ThreadState<'gc> {
    fn new(closure: Closure<'gc>, args: &[Value<'gc>]) -> ThreadState<'gc> {
        let proto = closure.0.proto;
        let fixed_params = proto.fixed_params as usize;
        let var_params = if args.len() > fixed_params {
            args.len() - fixed_params
        } else {
            0
        };

        let mut stack =
            vec![Value::Nil; 1 + var_params + fixed_params + proto.max_register as usize + 1];

        stack[0] = Value::Closure(closure);

        for i in 0..var_params {
            stack[1 + i] = args[fixed_params + i];
        }

        for i in 0..fixed_params {
            stack[1 + var_params + i] = args.get(i).cloned().unwrap_or(Value::Nil);
        }

        let frames = vec![Frame {
            pc: 0,
            bottom: 0,
            base: 1 + var_params,
        }];

        ThreadState { stack, frames }
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
                        results,
                    } => {
                        unimplemented!("no function call");
                    }
                    OpCode::Return { start, count } => {
                        if let Some(count) = count.get_count() {
                            for i in 0..count {
                                self.stack[current_frame.bottom + i as usize] =
                                    self.stack[current_frame.base + start as usize + i as usize]
                            }
                            self.stack.truncate(current_frame.bottom + count as usize);
                        } else {
                            unimplemented!("no variable return");
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
}
