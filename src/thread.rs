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

#[derive(Debug, Collect)]
#[collect(empty_drop)]
struct ThreadState<'gc> {
    closure: Closure<'gc>,
    stack: Vec<Value<'gc>>,
    pc: usize,
    base: usize,
    result: Option<(usize, usize)>,
}

impl<'gc> ThreadState<'gc> {
    fn new(closure: Closure<'gc>, args: &[Value<'gc>]) -> ThreadState<'gc> {
        let mut stack = vec![Value::Nil; args.len() + 256];

        let base = if closure.proto.has_varargs && args.len() > closure.proto.fixed_params as usize
        {
            let vararg_count = args.len() - closure.proto.fixed_params as usize;
            for i in 0..vararg_count {
                stack[i] = args[closure.proto.fixed_params as usize + i];
            }
            vararg_count
        } else {
            0
        };

        for i in 0..closure.proto.fixed_params as usize {
            stack[base + i] = args.get(i).cloned().unwrap_or(Value::Nil);
        }

        ThreadState {
            closure,
            stack,
            pc: 0,
            base,
            result: None,
        }
    }

    fn run(&mut self, mut instruction_limit: Option<u64>) -> bool {
        if self.result.is_some() {
            return true;
        }

        let frame = &mut self.stack[self.base..self.base + 256];
        loop {
            match self.closure.proto.opcodes[self.pc] {
                OpCode::Move { dest, source } => {
                    frame[dest as usize] = frame[source as usize];
                }
                OpCode::LoadConstant { dest, constant } => {
                    frame[dest as usize] = self.closure.proto.constants[constant as usize];
                }
                OpCode::LoadBool {
                    dest,
                    value,
                    skip_next,
                } => {
                    frame[dest as usize] = Value::Boolean(value);
                    if skip_next {
                        self.pc += 1;
                    }
                }
                OpCode::LoadNil { dest, count } => {
                    for i in dest..dest + count {
                        frame[i as usize] = Value::Nil;
                    }
                }
                OpCode::Return { start, count } => {
                    if let Some(count) = count.get_count() {
                        self.result = Some((self.base + start as usize, count as usize));
                    } else {
                        unimplemented!("no variable return");
                    }
                    break;
                }
            }
            self.pc += 1;

            if let Some(instruction_limit) = instruction_limit.as_mut() {
                if *instruction_limit == 0 {
                    break;
                } else {
                    *instruction_limit -= 1
                }
            }
        }

        self.result.is_some()
    }

    fn results(&self) -> Option<&[Value<'gc>]> {
        self.result
            .map(|(beg, count)| &self.stack[beg..beg + count])
    }
}
