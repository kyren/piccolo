use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::mem;

use failure::{err_msg, Error};

use gc_arena::MutationContext;

use function::FunctionProto;
use opcode::{Constant, OpCode, Register, VarCount, MAX_VAR_COUNT};
use parser::{Chunk, Expression, HeadExpression, LocalStatement, SimpleExpression, Statement};
use string::String;
use value::Value;

pub fn compile_chunk<'gc>(
    mc: MutationContext<'gc, '_>,
    chunk: &Chunk,
) -> Result<FunctionProto<'gc>, Error> {
    Compiler::compile(mc, &chunk)
}

#[derive(Fail, Debug)]
#[fail(display = "insufficient available registers")]
struct InsufficientRegisters;

struct Compiler<'gc, 'a> {
    mutation_context: MutationContext<'gc, 'a>,

    constants: Vec<Value<'gc>>,
    constant_table: HashMap<ConstantValue<'gc>, Constant>,

    last_allocated_register: Option<u8>,
    locals: Vec<(&'a [u8], Register)>,

    opcodes: Vec<OpCode>,
}

impl<'gc, 'a> Compiler<'gc, 'a> {
    fn compile(
        mc: MutationContext<'gc, '_>,
        chunk: &'a Chunk,
    ) -> Result<FunctionProto<'gc>, Error> {
        let mut compiler = Compiler {
            mutation_context: mc,
            constants: Vec::new(),
            constant_table: HashMap::new(),
            last_allocated_register: None,
            locals: Vec::new(),
            opcodes: Vec::new(),
        };

        compiler.chunk(chunk)?;

        Ok(FunctionProto {
            fixed_params: 0,
            has_varargs: false,
            constants: compiler.constants,
            opcodes: compiler.opcodes,
        })
    }

    fn chunk(&mut self, chunk: &'a Chunk) -> Result<(), Error> {
        for statement in &chunk.block.statements {
            match statement {
                Statement::LocalStatement(local_statement) => {
                    self.local_statement(local_statement)?
                }
                _ => bail!("unsupported statement type"),
            }
        }

        if let Some(return_statement) = &chunk.block.return_statement {
            let ret_count = return_statement.returns.len() as u8;
            if ret_count > MAX_VAR_COUNT {
                bail!("too many return expressions");
            }
            let ret_start = self.allocate_registers(ret_count)?;
            for i in 0..ret_count {
                self.expression(&return_statement.returns[i as usize], Some(ret_start + i))?;
            }
            self.opcodes.push(OpCode::Return {
                start: ret_start,
                count: VarCount::make_count(ret_count).unwrap(),
            });
        } else {
            self.opcodes.push(OpCode::Return {
                start: 0,
                count: VarCount::make_count(0).unwrap(),
            });
        }

        Ok(())
    }

    fn local_statement(&mut self, local_statement: &'a LocalStatement) -> Result<(), Error> {
        for (i, expr) in local_statement.values.iter().enumerate() {
            if local_statement.names.len() > i {
                let reg = self.allocate_registers(1)?;
                self.expression(expr, Some(reg))?;
                self.locals.push((&local_statement.names[i], reg));
            } else {
                self.expression(expr, None)?;
            }
        }
        for i in local_statement.values.len()..local_statement.names.len() {
            let reg = self.allocate_registers(1)?;
            self.load_nil(reg)?;
            self.locals.push((&local_statement.names[i], reg));
        }
        Ok(())
    }

    fn expression(
        &mut self,
        expression: &'a Expression,
        dest_register: Option<Register>,
    ) -> Result<(), Error> {
        if expression.tail.len() > 0 {
            bail!("no binary operator support yet");
        }
        self.head_expression(&expression.head, dest_register)
    }

    fn head_expression(
        &mut self,
        head_expression: &'a HeadExpression,
        dest_register: Option<Register>,
    ) -> Result<(), Error> {
        match head_expression {
            HeadExpression::Simple(simple_expression) => {
                self.simple_expression(simple_expression, dest_register)
            }
            HeadExpression::UnaryOperator(_, _) => bail!("no unary operator support yet"),
        }
    }

    fn simple_expression(
        &mut self,
        simple_expression: &'a SimpleExpression,
        dest_register: Option<Register>,
    ) -> Result<(), Error> {
        match simple_expression {
            SimpleExpression::Float(f) => {
                if let Some(dest_register) = dest_register {
                    let op = OpCode::LoadConstant {
                        dest: dest_register,
                        constant: self.get_constant(Value::Number(*f))?,
                    };
                    self.opcodes.push(op);
                }
            }
            SimpleExpression::Integer(i) => {
                if let Some(dest_register) = dest_register {
                    let op = OpCode::LoadConstant {
                        dest: dest_register,
                        constant: self.get_constant(Value::Integer(*i))?,
                    };
                    self.opcodes.push(op);
                }
            }
            SimpleExpression::String(s) => {
                if let Some(dest_register) = dest_register {
                    let string = String::new(self.mutation_context, &*s);
                    let op = OpCode::LoadConstant {
                        dest: dest_register,
                        constant: self.get_constant(Value::String(string))?,
                    };
                    self.opcodes.push(op);
                }
            }
            SimpleExpression::Nil => {
                if let Some(dest_register) = dest_register {
                    self.load_nil(dest_register)?;
                }
            }
            SimpleExpression::True => {
                if let Some(dest_register) = dest_register {
                    let op = OpCode::LoadBool {
                        dest: dest_register,
                        value: true,
                        skip_next: false,
                    };
                    self.opcodes.push(op);
                }
            }
            SimpleExpression::False => {
                if let Some(dest_register) = dest_register {
                    let op = OpCode::LoadBool {
                        dest: dest_register,
                        value: false,
                        skip_next: false,
                    };
                    self.opcodes.push(op);
                }
            }
            _ => bail!("unsupported simple expression"),
        }

        Ok(())
    }

    // Emit a LoadNil opcode, possibly combining several sequential LoadNil opcodes into one.
    fn load_nil(&mut self, dest: Register) -> Result<(), Error> {
        match self.opcodes.last().cloned() {
            Some(OpCode::LoadNil {
                dest: prev_dest,
                count: prev_count,
            }) if prev_dest + prev_count == dest =>
            {
                self.opcodes.push(OpCode::LoadNil {
                    dest: prev_dest,
                    count: prev_count + 1,
                });
            }
            _ => {
                self.opcodes.push(OpCode::LoadNil { dest, count: 1 });
            }
        }
        Ok(())
    }

    fn get_constant(&mut self, constant: Value<'gc>) -> Result<Constant, Error> {
        if let Some(constant) = self.constant_table.get(&ConstantValue(constant)).cloned() {
            Ok(constant)
        } else {
            let c = self.constants.len();
            if c < 65536 {
                let c = c as Constant;
                self.constants.push(constant);
                self.constant_table.insert(ConstantValue(constant), c);
                Ok(c)
            } else {
                Err(err_msg("function needs too many constants"))
            }
        }
    }

    fn allocate_registers(&mut self, count: u8) -> Result<Register, Error> {
        assert_ne!(count, 0, "cannot allocate 0 registers");

        let start = if let Some(last_allocated) = self.last_allocated_register {
            if count > 255 - last_allocated {
                return Err(InsufficientRegisters.into());
            }
            last_allocated + 1
        } else {
            0
        };

        self.last_allocated_register = Some(start + (count - 1));
        Ok(start)
    }

    #[allow(unused)]
    fn free_registers(&mut self, start: Register, count: u8) {
        assert_ne!(count, 0, "cannot free 0 registers");
        let last_allocated = self
            .last_allocated_register
            .expect("no registers allocated to free");
        assert_eq!(
            start + count - 1,
            last_allocated,
            "Only most recently allocated registers can be freed"
        );
        if start == 0 {
            self.last_allocated_register = None;
        } else {
            self.last_allocated_register = Some(start - 1);
        }
    }
}

// Value which implements Hash and Eq, where values are equal only when they are bit for bit
// identical.
struct ConstantValue<'gc>(Value<'gc>);

impl<'gc> PartialEq for ConstantValue<'gc> {
    fn eq(&self, other: &ConstantValue<'gc>) -> bool {
        match (self.0, other.0) {
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) => false,

            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Boolean(_), _) => false,

            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Integer(_), _) => false,

            (Value::Number(a), Value::Number(b)) => float_bytes(a) == float_bytes(b),
            (Value::Number(_), _) => false,

            (Value::String(a), Value::String(b)) => a == b,
            (Value::String(_), _) => false,

            (Value::Table(a), Value::Table(b)) => a == b,
            (Value::Table(_), _) => false,

            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Function(_), _) => false,
        }
    }
}

impl<'gc> Eq for ConstantValue<'gc> {}

impl<'gc> Hash for ConstantValue<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Value::Nil => {
                Hash::hash(&0, state);
            }
            Value::Boolean(b) => {
                Hash::hash(&1, state);
                b.hash(state);
            }
            Value::Integer(i) => {
                Hash::hash(&2, state);
                i.hash(state);
            }
            Value::Number(n) => {
                Hash::hash(&3, state);
                float_bytes(*n).hash(state);
            }
            Value::String(s) => {
                Hash::hash(&4, state);
                s.hash(state);
            }
            Value::Table(t) => {
                Hash::hash(&5, state);
                t.hash(state);
            }
            Value::Function(f) => {
                Hash::hash(&6, state);
                f.hash(state);
            }
        }
    }
}

fn float_bytes(f: f64) -> u64 {
    unsafe { mem::transmute(f) }
}
