use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::mem;

use failure::Error;
use num_traits::cast;

use gc_arena::MutationContext;

use function::{FunctionProto, UpValueDescriptor};
use opcode::{Constant, OpCode, Register, UpValueIndex, VarCount};
use parser::{
    AssignmentStatement, AssignmentTarget, Block, Chunk, Expression, FunctionStatement,
    HeadExpression, LocalStatement, PrimaryExpression, ReturnStatement, SimpleExpression,
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
enum CompilerLimit {
    #[fail(display = "insufficient available registers")]
    Registers,
    #[fail(display = "too many upvalues")]
    UpValues,
    #[fail(display = "too many returns")]
    Returns,
    #[fail(display = "too many fixed parameters")]
    FixedParameters,
    #[fail(display = "too many inner functions")]
    Functions,
    #[fail(display = "too many constants")]
    Constants,
}

struct Compiler<'gc, 'a> {
    mutation_context: MutationContext<'gc, 'a>,
    function_stack: Vec<CompilerFunction<'gc, 'a>>,
}

impl<'gc, 'a> Compiler<'gc, 'a> {
    fn compile(
        mc: MutationContext<'gc, '_>,
        chunk: &'a Chunk,
    ) -> Result<FunctionProto<'gc>, Error> {
        let mut compiler = Compiler {
            mutation_context: mc,
            function_stack: vec![CompilerFunction::default()],
        };

        compiler.block(&chunk.block)?;
        let function = compiler.function_stack.pop().unwrap();

        Ok(FunctionProto {
            fixed_params: 0,
            has_varargs: false,
            stack_size: function.stack_size,
            constants: function.constants,
            opcodes: function.opcodes,
            upvalues: function.upvalues.iter().map(|(_, d)| *d).collect(),
            functions: function.functions,
        })
    }

    fn block(&mut self, block: &'a Block) -> Result<(), Error> {
        for statement in &block.statements {
            self.statement(statement)?;
        }

        if let Some(return_statement) = &block.return_statement {
            self.return_statement(return_statement)?;
        } else {
            self.current_function().opcodes.push(OpCode::Return {
                start: 0,
                count: VarCount::make_constant(0).unwrap(),
            });
        }

        Ok(())
    }

    fn statement(&mut self, statement: &'a Statement) -> Result<(), Error> {
        match statement {
            Statement::LocalStatement(local_statement) => {
                self.local_statement(local_statement)?;
            }
            Statement::LocalFunction(local_function) => {
                self.local_function(local_function)?;
            }
            Statement::Assignment(assignment) => {
                self.assignment_statement(assignment)?;
            }
            _ => bail!("unsupported statement type"),
        }

        Ok(())
    }

    fn return_statement(&mut self, return_statement: &'a ReturnStatement) -> Result<(), Error> {
        let ret_count: u8 = cast(return_statement.returns.len()).ok_or(CompilerLimit::Returns)?;
        let var_count = VarCount::make_constant(ret_count).ok_or(CompilerLimit::Returns)?;

        if ret_count == 0 {
            self.current_function().opcodes.push(OpCode::Return {
                start: 0,
                count: var_count,
            });
        } else if ret_count == 1 {
            let mut expr = self.expression(&return_statement.returns[0])?;
            let reg = self.expr_any_register(&mut expr)?;
            self.current_function().opcodes.push(OpCode::Return {
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
            self.current_function().opcodes.push(OpCode::Return {
                start: ret_start,
                count: var_count,
            });
        }

        Ok(())
    }

    fn local_statement(&mut self, local_statement: &'a LocalStatement) -> Result<(), Error> {
        for (i, expr) in local_statement.values.iter().enumerate() {
            if local_statement.names.len() > i {
                let expr = self.expression(expr)?;
                let reg = self.expr_allocate_register(expr)?;
                self.current_function()
                    .locals
                    .push((&local_statement.names[i], reg));
            } else {
                let expr = self.expression(expr)?;
                self.free_expr(expr);
            }
        }
        for i in local_statement.values.len()..local_statement.names.len() {
            let reg = self.allocate_register()?;
            self.load_nil(reg)?;
            self.current_function()
                .locals
                .push((&local_statement.names[i], reg));
        }
        Ok(())
    }

    fn assignment_statement(&mut self, assignment: &'a AssignmentStatement) -> Result<(), Error> {
        for (i, target) in assignment.targets.iter().enumerate() {
            let mut expr = if i < assignment.values.len() {
                self.expression(&assignment.values[i])?
            } else {
                ExprDescriptor::Nil
            };

            match target {
                AssignmentTarget::Name(name) => match self.find_variable(name)? {
                    VariableDescriptor::Local(dest) => {
                        self.expr_to_register(expr, dest)?;
                    }
                    VariableDescriptor::UpValue(dest) => {
                        let source = self.expr_any_register(&mut expr)?;
                        self.current_function()
                            .opcodes
                            .push(OpCode::SetUpValue { source, dest });
                        self.free_expr(expr);
                    }
                    VariableDescriptor::Global(_) => bail!("global variables unsupported"),
                },
                AssignmentTarget::Field(_, _) => bail!("unimplemented assignment target"),
            }
        }

        Ok(())
    }

    fn local_function(&mut self, local_function: &'a FunctionStatement) -> Result<(), Error> {
        if local_function.definition.has_varargs {
            bail!("no varargs support");
        }
        if !local_function.name.fields.is_empty() {
            bail!("no function name fields support");
        }
        if local_function.name.method.is_some() {
            bail!("no method support");
        }

        self.function_stack.push(CompilerFunction::default());

        let fixed_params: u8 =
            cast(local_function.definition.parameters.len()).ok_or(CompilerLimit::FixedParameters)?;
        {
            let current_function = self.current_function();
            current_function.stack_top = fixed_params as u16;
            current_function.stack_size = current_function.stack_top;
            for (i, name) in local_function.definition.parameters.iter().enumerate() {
                current_function.locals.push((name, cast(i).unwrap()));
            }
        }

        self.block(&local_function.definition.body)?;

        let new_function = self.function_stack.pop().unwrap();
        let new_function = FunctionProto {
            fixed_params,
            has_varargs: false,
            stack_size: new_function.stack_size,
            constants: new_function.constants,
            opcodes: new_function.opcodes,
            upvalues: new_function.upvalues.iter().map(|(_, d)| *d).collect(),
            functions: new_function.functions,
        };

        self.current_function().functions.push(new_function);
        let dest = self.allocate_register()?;

        {
            let current_function = self.current_function();
            let proto = cast(current_function.functions.len() - 1).ok_or(CompilerLimit::Functions)?;
            current_function
                .opcodes
                .push(OpCode::Closure { proto, dest });
            current_function
                .locals
                .push((&local_function.name.name, dest));
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
            PrimaryExpression::Name(name) => Ok(match self.find_variable(name)? {
                VariableDescriptor::Local(reg) => ExprDescriptor::Local(reg),
                VariableDescriptor::UpValue(upvalue) => ExprDescriptor::UpValue(upvalue),
                VariableDescriptor::Global(_) => {
                    bail!("no support for globals");
                }
            }),
            PrimaryExpression::GroupedExpression(expr) => self.expression(expr),
        }
    }

    fn find_variable(&mut self, name: &'a [u8]) -> Result<VariableDescriptor<'a>, Error> {
        for i in (0..self.function_stack.len()).rev() {
            for j in (0..self.function_stack[i].locals.len()).rev() {
                let (local_name, register) = self.function_stack[i].locals[j];
                if name == local_name {
                    if i == self.function_stack.len() - 1 {
                        return Ok(VariableDescriptor::Local(register));
                    } else {
                        self.function_stack[i + 1]
                            .upvalues
                            .push((name, UpValueDescriptor::ParentLocal(register)));
                        let mut upvalue_index = cast(self.function_stack[i + 1].upvalues.len() - 1)
                            .ok_or(CompilerLimit::UpValues)?;
                        for k in i + 2..self.function_stack.len() {
                            self.function_stack[k]
                                .upvalues
                                .push((name, UpValueDescriptor::Outer(upvalue_index)));
                            upvalue_index = cast(self.function_stack[k].upvalues.len() - 1)
                                .ok_or(CompilerLimit::UpValues)?;
                        }
                        return Ok(VariableDescriptor::UpValue(upvalue_index));
                    }
                }
            }

            for j in 0..self.function_stack[i].upvalues.len() {
                if name == self.function_stack[i].upvalues[j].0 {
                    if i == self.function_stack.len() - 1 {
                        return Ok(VariableDescriptor::UpValue(cast(j).unwrap()));
                    } else {
                        let mut upvalue_index = cast(j).unwrap();
                        for k in i + 1..self.function_stack.len() {
                            self.function_stack[k]
                                .upvalues
                                .push((name, UpValueDescriptor::Outer(upvalue_index)));
                            upvalue_index = cast(self.function_stack[k].upvalues.len() - 1)
                                .ok_or(CompilerLimit::UpValues)?;
                        }
                        return Ok(VariableDescriptor::UpValue(upvalue_index));
                    }
                }
            }
        }

        Ok(VariableDescriptor::Global(name))
    }

    // Emit a LoadNil opcode, possibly combining several sequential LoadNil opcodes into one.
    fn load_nil(&mut self, dest: Register) -> Result<(), Error> {
        let current_function = self.current_function();
        match current_function.opcodes.last().cloned() {
            Some(OpCode::LoadNil {
                dest: prev_dest,
                count: prev_count,
            }) if prev_dest + prev_count == dest =>
            {
                current_function.opcodes.push(OpCode::LoadNil {
                    dest: prev_dest,
                    count: prev_count + 1,
                });
            }
            _ => {
                current_function
                    .opcodes
                    .push(OpCode::LoadNil { dest, count: 1 });
            }
        }
        Ok(())
    }

    fn get_constant(&mut self, constant: Value<'gc>) -> Result<Constant, Error> {
        let current_function = self.current_function();
        if let Some(constant) = current_function
            .constant_table
            .get(&ConstantValue(constant))
            .cloned()
        {
            Ok(constant)
        } else {
            let c: Constant =
                cast(current_function.constants.len()).ok_or(CompilerLimit::Constants)?;
            current_function.constants.push(constant);
            current_function
                .constant_table
                .insert(ConstantValue(constant), c);
            Ok(c)
        }
    }

    // Modify an expression to contain its result in any register, and return that register
    fn expr_any_register(&mut self, expr: &mut ExprDescriptor) -> Result<Register, Error> {
        let (new_expr, reg) = match *expr {
            ExprDescriptor::Register(reg) => (ExprDescriptor::Register(reg), reg),
            ExprDescriptor::Constant(constant) => {
                let dest = self.allocate_register()?;
                self.current_function()
                    .opcodes
                    .push(OpCode::LoadConstant { dest, constant });
                (ExprDescriptor::Register(dest), dest)
            }
            ExprDescriptor::Local(reg) => (ExprDescriptor::Local(reg), reg),
            ExprDescriptor::UpValue(source) => {
                let dest = self.allocate_register()?;
                self.current_function()
                    .opcodes
                    .push(OpCode::GetUpValue { source, dest });
                (ExprDescriptor::Register(dest), dest)
            }
            ExprDescriptor::Bool(value) => {
                let dest = self.allocate_register()?;
                self.current_function().opcodes.push(OpCode::LoadBool {
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

    // Consume an expression and store the result in the given register
    fn expr_to_register(&mut self, expr: ExprDescriptor, dest: Register) -> Result<(), Error> {
        match expr {
            ExprDescriptor::Register(register) => {
                self.current_function().opcodes.push(OpCode::Move {
                    dest,
                    source: register,
                });
            }
            ExprDescriptor::Constant(constant) => {
                self.current_function()
                    .opcodes
                    .push(OpCode::LoadConstant { dest, constant });
            }
            ExprDescriptor::Local(source) => {
                self.current_function()
                    .opcodes
                    .push(OpCode::Move { source, dest });
            }
            ExprDescriptor::UpValue(source) => {
                self.current_function()
                    .opcodes
                    .push(OpCode::GetUpValue { source, dest });
            }
            ExprDescriptor::Bool(value) => {
                self.current_function().opcodes.push(OpCode::LoadBool {
                    dest,
                    value,
                    skip_next: false,
                });
            }
            ExprDescriptor::Nil => {
                self.load_nil(dest)?;
            }
        }

        Ok(())
    }

    // Consume an expression, ensuring that its result is stored in a newly allocated register
    fn expr_allocate_register(&mut self, expr: ExprDescriptor) -> Result<Register, Error> {
        match expr {
            ExprDescriptor::Register(register) => Ok(register),
            expr => {
                let dest = self.allocate_register()?;
                self.expr_to_register(expr, dest)?;
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
        let current_function = self.current_function();
        let r = cast(current_function.stack_top).ok_or(CompilerLimit::Registers)?;
        current_function.stack_top += 1;
        current_function.stack_size = current_function.stack_size.max(current_function.stack_top);
        Ok(r)
    }

    fn free_register(&mut self, reg: Register) {
        let current_function = self.current_function();
        assert_eq!(
            reg as u16 + 1,
            current_function.stack_top,
            "Only the most recently allocated register can be freed"
        );
        current_function.stack_top -= 1;
    }

    fn current_function(&mut self) -> &mut CompilerFunction<'gc, 'a> {
        self.function_stack.last_mut().unwrap()
    }
}

#[derive(Default)]
struct CompilerFunction<'gc, 'a> {
    constants: Vec<Value<'gc>>,
    constant_table: HashMap<ConstantValue<'gc>, Constant>,

    upvalues: Vec<(&'a [u8], UpValueDescriptor)>,
    functions: Vec<FunctionProto<'gc>>,

    stack_top: u16,
    stack_size: u16,
    locals: Vec<(&'a [u8], Register)>,

    opcodes: Vec<OpCode>,
}

enum VariableDescriptor<'a> {
    Local(Register),
    UpValue(UpValueIndex),
    Global(&'a [u8]),
}

enum ExprDescriptor {
    Register(Register),
    Local(Register),
    UpValue(UpValueIndex),
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
