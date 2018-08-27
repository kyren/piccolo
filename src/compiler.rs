use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::mem;

use failure::{err_msg, Error};

use gc_arena::MutationContext;

use function::FunctionProto;
use opcode::{Constant, OpCode, Register, VarCount};
use parser::{
    Chunk, Expression, HeadExpression, LocalStatement, PrimaryExpression, SimpleExpression,
    Statement, SuffixedExpression,
};
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
            max_register: compiler.last_allocated_register.unwrap_or(0),
            constants: compiler.constants,
            opcodes: compiler.opcodes,
            upvalues: Vec::new(),
            functions: Vec::new(),
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
            let var_count =
                VarCount::make_constant(ret_count).ok_or_else(|| err_msg("too many expressions"))?;

            if ret_count == 0 {
                self.opcodes.push(OpCode::Return {
                    start: 0,
                    count: var_count,
                });
            } else if ret_count == 1 {
                let mut expr = self.expression(&return_statement.returns[0])?;
                let reg = self.expr_any_register(&mut expr)?;
                self.opcodes.push(OpCode::Return {
                    start: reg,
                    count: var_count,
                });
            } else {
                let expr_start = self.expression(&return_statement.returns[0])?;
                let ret_start = self.expr_allocate_register(expr_start)?;
                for i in 1..ret_count {
                    let expr = self.expression(&return_statement.returns[i as usize])?;
                    self.expr_allocate_register(expr)?;
                }
                self.opcodes.push(OpCode::Return {
                    start: ret_start,
                    count: var_count,
                });
            }
        } else {
            self.opcodes.push(OpCode::Return {
                start: 0,
                count: VarCount::make_constant(0).unwrap(),
            });
        }

        Ok(())
    }

    fn local_statement(&mut self, local_statement: &'a LocalStatement) -> Result<(), Error> {
        for (i, expr) in local_statement.values.iter().enumerate() {
            if local_statement.names.len() > i {
                let expr = self.expression(expr)?;
                let reg = self.expr_allocate_register(expr)?;
                self.locals.push((&local_statement.names[i], reg));
            } else {
                let expr = self.expression(expr)?;
                self.free_expr(expr);
            }
        }
        for i in local_statement.values.len()..local_statement.names.len() {
            let reg = self.allocate_register()?;
            self.load_nil(reg)?;
            self.locals.push((&local_statement.names[i], reg));
        }
        Ok(())
    }

    fn expression(&mut self, expression: &'a Expression) -> Result<ExprDescriptor, Error> {
        if expression.tail.len() > 0 {
            bail!("no binary operator support yet");
        }
        self.head_expression(&expression.head)
    }

    fn head_expression(
        &mut self,
        head_expression: &'a HeadExpression,
    ) -> Result<ExprDescriptor, Error> {
        match head_expression {
            HeadExpression::Simple(simple_expression) => self.simple_expression(simple_expression),
            HeadExpression::UnaryOperator(_, _) => bail!("no unary operator support yet"),
        }
    }

    fn simple_expression(
        &mut self,
        simple_expression: &'a SimpleExpression,
    ) -> Result<ExprDescriptor, Error> {
        Ok(match simple_expression {
            SimpleExpression::Float(f) => {
                ExprDescriptor::Constant(self.get_constant(Value::Number(*f))?)
            }
            SimpleExpression::Integer(i) => {
                ExprDescriptor::Constant(self.get_constant(Value::Integer(*i))?)
            }
            SimpleExpression::String(s) => {
                let string = String::new(self.mutation_context, &*s);
                ExprDescriptor::Constant(self.get_constant(Value::String(string))?)
            }
            SimpleExpression::Nil => ExprDescriptor::Nil,
            SimpleExpression::True => ExprDescriptor::Bool(true),
            SimpleExpression::False => ExprDescriptor::Bool(false),
            SimpleExpression::Suffixed(suffixed) => self.suffixed_expression(suffixed)?,
            _ => bail!("unsupported simple expression"),
        })
    }

    fn suffixed_expression(
        &mut self,
        suffixed_expression: &'a SuffixedExpression,
    ) -> Result<ExprDescriptor, Error> {
        if !suffixed_expression.suffixes.is_empty() {
            bail!("no support for expression suffixes yet");
        }
        self.primary_expression(&suffixed_expression.primary)
    }

    fn primary_expression(
        &mut self,
        primary_expression: &'a PrimaryExpression,
    ) -> Result<ExprDescriptor, Error> {
        match primary_expression {
            PrimaryExpression::Name(name) => {
                for &(local, register) in self.locals.iter().rev() {
                    if name.as_ref() == local {
                        return Ok(ExprDescriptor::Local(register));
                    }
                }
                bail!("no support for _ENV yet");
            }
            PrimaryExpression::GroupedExpression(expr) => self.expression(expr),
        }
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

    // Modify an expression to contain its result in any register, and return that register
    fn expr_any_register(&mut self, expr: &mut ExprDescriptor) -> Result<Register, Error> {
        let (new_expr, reg) = match *expr {
            ExprDescriptor::Register(reg) => (ExprDescriptor::Register(reg), reg),
            ExprDescriptor::Constant(constant) => {
                let dest = self.allocate_register()?;
                self.opcodes.push(OpCode::LoadConstant { dest, constant });
                (ExprDescriptor::Register(dest), dest)
            }
            ExprDescriptor::Local(reg) => (ExprDescriptor::Local(reg), reg),
            ExprDescriptor::Bool(value) => {
                let dest = self.allocate_register()?;
                self.opcodes.push(OpCode::LoadBool {
                    dest,
                    value,
                    skip_next: false,
                });
                (ExprDescriptor::Register(dest), dest)
            }
            ExprDescriptor::Nil => {
                let dest = self.allocate_register()?;
                self.load_nil(dest)?;
                (ExprDescriptor::Register(dest), dest)
            }
        };

        *expr = new_expr;
        Ok(reg)
    }

    // Consume an expression, ensuring that its result is stored in an allocated register
    fn expr_allocate_register(&mut self, expr: ExprDescriptor) -> Result<Register, Error> {
        match expr {
            ExprDescriptor::Register(register) => Ok(register),
            ExprDescriptor::Constant(constant) => {
                let dest = self.allocate_register()?;
                self.opcodes.push(OpCode::LoadConstant { dest, constant });
                Ok(dest)
            }
            ExprDescriptor::Local(source) => {
                let dest = self.allocate_register()?;
                self.opcodes.push(OpCode::Move { source, dest });
                Ok(dest)
            }
            ExprDescriptor::Bool(value) => {
                let dest = self.allocate_register()?;
                self.opcodes.push(OpCode::LoadBool {
                    dest,
                    value,
                    skip_next: false,
                });
                Ok(dest)
            }
            ExprDescriptor::Nil => {
                let dest = self.allocate_register()?;
                self.load_nil(dest)?;
                Ok(dest)
            }
        }
    }

    fn free_expr(&mut self, expr: ExprDescriptor) {
        if let ExprDescriptor::Register(r) = expr {
            self.free_register(r);
        }
    }

    fn allocate_register(&mut self) -> Result<Register, Error> {
        let reg = if let Some(last_allocated) = self.last_allocated_register {
            if last_allocated == 255 {
                return Err(InsufficientRegisters.into());
            }
            last_allocated + 1
        } else {
            0
        };

        self.last_allocated_register = Some(reg);
        Ok(reg)
    }

    #[allow(unused)]
    fn free_register(&mut self, reg: Register) {
        let last_allocated = self
            .last_allocated_register
            .expect("no registers allocated to free");
        assert_eq!(
            reg, last_allocated,
            "Only the most recently allocated register can be freed"
        );
        if reg == 0 {
            self.last_allocated_register = None;
        } else {
            self.last_allocated_register = Some(reg - 1);
        }
    }
}

enum ExprDescriptor {
    Register(Register),
    Local(Register),
    Constant(Constant),
    Bool(bool),
    Nil,
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

            (Value::Closure(a), Value::Closure(b)) => a == b,
            (Value::Closure(_), _) => false,
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
            Value::Closure(c) => {
                Hash::hash(&6, state);
                c.hash(state);
            }
        }
    }
}

fn float_bytes(f: f64) -> u64 {
    unsafe { mem::transmute(f) }
}
