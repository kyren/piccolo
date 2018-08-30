use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::mem;

use failure::Error;
use num_traits::cast;

use gc_arena::MutationContext;

use function::{FunctionProto, UpValueDescriptor};
use opcode::{Constant, OpCode, Register, UpValueIndex, VarCount};
use parser::{
    AssignmentStatement, AssignmentTarget, BinaryOperator, Block, CallSuffix, Chunk, Expression,
    FunctionCallStatement, FunctionStatement, HeadExpression, LocalStatement, PrimaryExpression,
    ReturnStatement, SimpleExpression, Statement, SuffixedExpression,
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
        Ok(compiler.pop_function_proto())
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
                count: VarCount::make_zero(),
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
            Statement::FunctionCall(function_call) => {
                self.function_call(function_call)?;
            }
            Statement::Assignment(assignment) => {
                self.assignment(assignment)?;
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
            self.free_expr(expr);
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
            for i in (0..ret_count).rev() {
                self.free_register(ret_start + i);
            }
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

    fn function_call(&mut self, function_call: &'a FunctionCallStatement) -> Result<(), Error> {
        let func_expr = self.suffixed_expression(&function_call.head)?;
        let f_reg = self.expr_allocate_register(func_expr)?;

        match &function_call.call {
            CallSuffix::Function(exprs) => {
                let arg_count: u8 = cast(exprs.len()).ok_or(CompilerLimit::FixedParameters)?;
                for expr in exprs {
                    let expr = self.expression(expr)?;
                    self.expr_allocate_register(expr)?;
                }
                self.current_function().opcodes.push(OpCode::Call {
                    func: f_reg,
                    args: VarCount::make_constant(arg_count).ok_or(CompilerLimit::FixedParameters)?,
                    returns: VarCount::make_zero(),
                });
                for i in (1..=arg_count).rev() {
                    self.free_register(f_reg + i);
                }
            }
            CallSuffix::Method(_, _) => bail!("method unsupported"),
        }

        self.free_register(f_reg);

        Ok(())
    }

    fn assignment(&mut self, assignment: &'a AssignmentStatement) -> Result<(), Error> {
        for (i, target) in assignment.targets.iter().enumerate() {
            let mut expr = if i < assignment.values.len() {
                self.expression(&assignment.values[i])?
            } else {
                ExprDescriptor::Value(Value::Nil)
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
            current_function.fixed_params = fixed_params;
            for (i, name) in local_function.definition.parameters.iter().enumerate() {
                current_function.locals.push((name, cast(i).unwrap()));
            }
        }

        self.block(&local_function.definition.body)?;

        let new_function = self.pop_function_proto();
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

    fn expression(&mut self, expression: &'a Expression) -> Result<ExprDescriptor<'gc>, Error> {
        let mut expr = self.head_expression(&expression.head)?;
        for (binop, right) in &expression.tail {
            let right = self.expression(&right)?;
            expr = self.binop(expr, *binop, right)?;
        }
        Ok(expr)
    }

    fn head_expression(
        &mut self,
        head_expression: &'a HeadExpression,
    ) -> Result<ExprDescriptor<'gc>, Error> {
        match head_expression {
            HeadExpression::Simple(simple_expression) => self.simple_expression(simple_expression),
            HeadExpression::UnaryOperator(_, _) => bail!("no unary operator support yet"),
        }
    }

    fn simple_expression(
        &mut self,
        simple_expression: &'a SimpleExpression,
    ) -> Result<ExprDescriptor<'gc>, Error> {
        Ok(match simple_expression {
            SimpleExpression::Float(f) => ExprDescriptor::Value(Value::Number(*f)),
            SimpleExpression::Integer(i) => ExprDescriptor::Value(Value::Integer(*i)),
            SimpleExpression::String(s) => {
                let string = String::new(self.mutation_context, &*s);
                ExprDescriptor::Value(Value::String(string))
            }
            SimpleExpression::Nil => ExprDescriptor::Value(Value::Nil),
            SimpleExpression::True => ExprDescriptor::Value(Value::Boolean(true)),
            SimpleExpression::False => ExprDescriptor::Value(Value::Boolean(false)),
            SimpleExpression::Suffixed(suffixed) => self.suffixed_expression(suffixed)?,
            _ => bail!("unsupported simple expression"),
        })
    }

    fn suffixed_expression(
        &mut self,
        suffixed_expression: &'a SuffixedExpression,
    ) -> Result<ExprDescriptor<'gc>, Error> {
        if !suffixed_expression.suffixes.is_empty() {
            bail!("no support for expression suffixes yet");
        }
        self.primary_expression(&suffixed_expression.primary)
    }

    fn primary_expression(
        &mut self,
        primary_expression: &'a PrimaryExpression,
    ) -> Result<ExprDescriptor<'gc>, Error> {
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

    fn binop(
        &mut self,
        left: ExprDescriptor<'gc>,
        binop: BinaryOperator,
        right: ExprDescriptor<'gc>,
    ) -> Result<ExprDescriptor<'gc>, Error> {
        match binop {
            BinaryOperator::Add => {
                if let (ExprDescriptor::Value(a), ExprDescriptor::Value(b)) = (left, right) {
                    if let Some(v) = match (a, b) {
                        (Value::Integer(a), Value::Integer(b)) => Some(Value::Integer(a + b)),
                        (Value::Number(a), Value::Number(b)) => Some(Value::Number(a + b)),
                        (Value::Integer(a), Value::Number(b)) => Some(Value::Number(a as f64 + b)),
                        (Value::Number(a), Value::Integer(b)) => Some(Value::Number(a + b as f64)),
                        _ => None,
                    } {
                        return Ok(ExprDescriptor::Value(v));
                    }
                }

                match (left, right) {
                    (mut left_expr, ExprDescriptor::Value(right_const)) => {
                        let left = self.expr_any_register(&mut left_expr)?;
                        let right = self.get_constant(right_const)?;
                        self.free_expr(left_expr);
                        let dest = self.allocate_register()?;
                        self.current_function()
                            .opcodes
                            .push(OpCode::AddRC { dest, left, right });
                        Ok(ExprDescriptor::Temporary(dest))
                    }
                    (ExprDescriptor::Value(left_const), mut right_expr) => {
                        let left = self.get_constant(left_const)?;
                        let right = self.expr_any_register(&mut right_expr)?;
                        self.free_expr(right_expr);
                        let dest = self.allocate_register()?;
                        self.current_function()
                            .opcodes
                            .push(OpCode::AddCR { dest, left, right });
                        Ok(ExprDescriptor::Temporary(dest))
                    }
                    (mut left_expr, mut right_expr) => {
                        let left = self.expr_any_register(&mut left_expr)?;
                        let right = self.expr_any_register(&mut right_expr)?;
                        self.free_expr(left_expr);
                        self.free_expr(right_expr);
                        let dest = self.allocate_register()?;
                        self.current_function()
                            .opcodes
                            .push(OpCode::AddRR { dest, left, right });
                        Ok(ExprDescriptor::Temporary(dest))
                    }
                }
            }
            _ => bail!("unsupported binary operator"),
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

    fn pop_function_proto(&mut self) -> FunctionProto<'gc> {
        let function = self.function_stack.pop().unwrap();
        assert_eq!(
            function.stack_top as usize,
            function.locals.len(),
            "register leak"
        );
        FunctionProto {
            fixed_params: function.fixed_params,
            has_varargs: false,
            stack_size: function.stack_size,
            constants: function.constants,
            opcodes: function.opcodes,
            upvalues: function.upvalues.iter().map(|(_, d)| *d).collect(),
            functions: function.functions,
        }
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
    fn expr_any_register(&mut self, expr: &mut ExprDescriptor<'gc>) -> Result<Register, Error> {
        let (new_expr, reg) = match *expr {
            ExprDescriptor::Temporary(reg) => (ExprDescriptor::Temporary(reg), reg),
            ExprDescriptor::Local(reg) => (ExprDescriptor::Local(reg), reg),
            ExprDescriptor::UpValue(source) => {
                let dest = self.allocate_register()?;
                self.current_function()
                    .opcodes
                    .push(OpCode::GetUpValue { source, dest });
                (ExprDescriptor::Temporary(dest), dest)
            }
            ExprDescriptor::Value(value) => {
                let dest = self.allocate_register()?;
                match value {
                    Value::Nil => {
                        self.load_nil(dest)?;
                    }
                    Value::Boolean(value) => {
                        self.current_function().opcodes.push(OpCode::LoadBool {
                            dest,
                            value,
                            skip_next: false,
                        });
                    }
                    val => {
                        let constant = self.get_constant(val)?;
                        self.current_function()
                            .opcodes
                            .push(OpCode::LoadConstant { dest, constant });
                    }
                }
                (ExprDescriptor::Temporary(dest), dest)
            }
        };

        *expr = new_expr;
        Ok(reg)
    }

    // Consume an expression and store the result in the given register
    fn expr_to_register(&mut self, expr: ExprDescriptor<'gc>, dest: Register) -> Result<(), Error> {
        match expr {
            ExprDescriptor::Temporary(source) => {
                self.current_function()
                    .opcodes
                    .push(OpCode::Move { dest, source });
            }
            ExprDescriptor::Local(source) => {
                self.current_function()
                    .opcodes
                    .push(OpCode::Move { dest, source });
            }
            ExprDescriptor::UpValue(source) => {
                self.current_function()
                    .opcodes
                    .push(OpCode::GetUpValue { source, dest });
            }
            ExprDescriptor::Value(value) => match value {
                Value::Nil => {
                    self.load_nil(dest)?;
                }
                Value::Boolean(value) => {
                    self.current_function().opcodes.push(OpCode::LoadBool {
                        dest,
                        value,
                        skip_next: false,
                    });
                }
                val => {
                    let constant = self.get_constant(val)?;
                    self.current_function()
                        .opcodes
                        .push(OpCode::LoadConstant { dest, constant });
                }
            },
        }

        self.free_expr(expr);

        Ok(())
    }

    // Consume an expression, ensuring that its result is stored in a newly allocated register
    fn expr_allocate_register(&mut self, expr: ExprDescriptor<'gc>) -> Result<Register, Error> {
        match expr {
            ExprDescriptor::Temporary(register) => {
                assert!(
                    register as u16 == self.current_function().stack_top - 1,
                    "only top expression can be converted to register"
                );
                Ok(register)
            }
            expr => {
                let dest = self.allocate_register()?;
                self.expr_to_register(expr, dest)?;
                Ok(dest)
            }
        }
    }

    fn free_expr(&mut self, expr: ExprDescriptor<'gc>) {
        if let ExprDescriptor::Temporary(r) = expr {
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

    fixed_params: u8,
    locals: Vec<(&'a [u8], Register)>,

    opcodes: Vec<OpCode>,
}

enum VariableDescriptor<'a> {
    Local(Register),
    UpValue(UpValueIndex),
    Global(&'a [u8]),
}

#[derive(Copy, Clone)]
enum ExprDescriptor<'gc> {
    Temporary(Register),
    Local(Register),
    UpValue(UpValueIndex),
    Value(Value<'gc>),
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
