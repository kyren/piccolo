use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::mem;

use failure::{bail, err_msg, Error, Fail};
use num_traits::cast;

use gc_arena::{Gc, MutationContext};

use crate::function::{FunctionProto, UpValueDescriptor};
use crate::opcode::{
    ConstantIndex16, ConstantIndex8, OpCode, PrototypeIndex, RegisterIndex, UpValueIndex, VarCount,
};
use crate::operators::{
    categorize_binop, BinOpArgs, BinOpCategory, ShortCircuitBinOp, COMPARISON_BINOPS,
    SIMPLE_BINOPS, UNOPS,
};
use crate::parser::{
    AssignmentStatement, AssignmentTarget, BinaryOperator, Block, CallSuffix, Chunk, Expression,
    FunctionCallStatement, FunctionStatement, HeadExpression, LocalStatement, PrimaryExpression,
    ReturnStatement, SimpleExpression, Statement, SuffixPart, SuffixedExpression, UnaryOperator,
};
use crate::string::String;
use crate::value::Value;

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
    #[fail(display = "too many opcodes")]
    OpCodes,
}

struct Compiler<'gc, 'a> {
    mutation_context: MutationContext<'gc, 'a>,
    functions: TopStack<CompilerFunction<'gc, 'a>>,
}

impl<'gc, 'a> Compiler<'gc, 'a> {
    fn compile(
        mc: MutationContext<'gc, '_>,
        chunk: &'a Chunk,
    ) -> Result<FunctionProto<'gc>, Error> {
        let mut compiler = Compiler {
            mutation_context: mc,
            functions: TopStack::new(CompilerFunction::default()),
        };

        compiler.block(&chunk.block)?;
        Ok(compiler.functions.top.to_proto(mc))
    }

    fn block(&mut self, block: &'a Block) -> Result<(), Error> {
        for statement in &block.statements {
            self.statement(statement)?;
        }

        if let Some(return_statement) = &block.return_statement {
            self.return_statement(return_statement)?;
        } else {
            self.functions.top.opcodes.push(OpCode::Return {
                start: RegisterIndex(0),
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
        let ret_len = return_statement.returns.len();

        if ret_len == 0 {
            self.functions.top.opcodes.push(OpCode::Return {
                start: RegisterIndex(0),
                count: VarCount::make_zero(),
            });
        } else {
            let ret_start = cast(self.functions.top.register_allocator.stack_top)
                .ok_or(CompilerLimit::Registers)?;

            for i in 0..ret_len - 1 {
                let expr = self.expression(&return_statement.returns[i])?;
                self.expr_push_register(expr)?;
            }

            let ret_count = match self.expression(&return_statement.returns[ret_len - 1])? {
                ExprDescriptor::FunctionCall { func, args } => {
                    self.expr_function_call(*func, args, VarCount::make_variable())?;
                    VarCount::make_variable()
                }
                expr => {
                    self.expr_push_register(expr)?;
                    cast(ret_len)
                        .and_then(VarCount::make_constant)
                        .ok_or(CompilerLimit::Returns)?
                }
            };

            self.functions.top.opcodes.push(OpCode::Return {
                start: RegisterIndex(ret_start),
                count: ret_count,
            });

            // Free all allocated return registers so that we do not fail the register leak check
            self.functions
                .top
                .register_allocator
                .pop_to(ret_start as u16);
        }

        Ok(())
    }

    fn local_statement(&mut self, local_statement: &'a LocalStatement) -> Result<(), Error> {
        let name_len = local_statement.names.len();
        let val_len = local_statement.values.len();

        for i in 0..val_len {
            let expr = self.expression(&local_statement.values[i])?;

            if i >= name_len {
                self.expr_finish(expr)?;
            } else if i == val_len - 1 {
                match expr {
                    ExprDescriptor::FunctionCall { func, args } => {
                        let num_returns =
                            cast(1 + name_len - val_len).ok_or(CompilerLimit::Registers)?;
                        self.expr_function_call(
                            *func,
                            args,
                            VarCount::make_constant(num_returns).ok_or(CompilerLimit::Returns)?,
                        )?;
                        let reg = self
                            .functions
                            .top
                            .register_allocator
                            .push(num_returns)
                            .ok_or(CompilerLimit::Registers)?;
                        for j in 0..num_returns {
                            self.functions.top.locals.push((
                                &local_statement.names[i + j as usize],
                                RegisterIndex(reg.0 + j),
                            ));
                        }

                        return Ok(());
                    }
                    expr => {
                        let reg = self.expr_allocate_register(expr)?;
                        self.functions
                            .top
                            .locals
                            .push((&local_statement.names[i], reg));
                    }
                }
            } else {
                let reg = self.expr_allocate_register(expr)?;
                self.functions
                    .top
                    .locals
                    .push((&local_statement.names[i], reg));
            }
        }

        for i in local_statement.values.len()..local_statement.names.len() {
            let reg = self
                .functions
                .top
                .register_allocator
                .allocate()
                .ok_or(CompilerLimit::Registers)?;
            self.load_nil(reg)?;
            self.functions
                .top
                .locals
                .push((&local_statement.names[i], reg));
        }

        Ok(())
    }

    fn function_call(&mut self, function_call: &'a FunctionCallStatement) -> Result<(), Error> {
        let func_expr = self.suffixed_expression(&function_call.head)?;
        match &function_call.call {
            CallSuffix::Function(args) => {
                let arg_exprs = args
                    .iter()
                    .map(|arg| self.expression(arg))
                    .collect::<Result<_, Error>>()?;
                self.expr_function_call(func_expr, arg_exprs, VarCount::make_zero())?;
            }
            CallSuffix::Method(_, _) => bail!("method unsupported"),
        }
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
                        self.functions
                            .top
                            .opcodes
                            .push(OpCode::SetUpValue { source, dest });
                        self.expr_finish(expr)?;
                    }
                    VariableDescriptor::Global(name) => {
                        let mut env = self.get_environment()?;
                        let mut key = ExprDescriptor::Value(Value::String(String::new(
                            self.mutation_context,
                            name,
                        )));
                        self.set_table(&mut env, &mut key, &mut expr)?;
                        self.expr_finish(env)?;
                        self.expr_finish(key)?;
                        self.expr_finish(expr)?;
                    }
                },
                AssignmentTarget::Field(_, _) => bail!("no support for field assignment"),
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

        self.functions.push(CompilerFunction::default());

        let fixed_params: u8 = cast(local_function.definition.parameters.len())
            .ok_or(CompilerLimit::FixedParameters)?;
        {
            self.functions.top.register_allocator.push(fixed_params);
            self.functions.top.fixed_params = fixed_params;
            for (i, name) in local_function.definition.parameters.iter().enumerate() {
                self.functions
                    .top
                    .locals
                    .push((name, RegisterIndex(cast(i).unwrap())));
            }
        }

        self.block(&local_function.definition.body)?;

        let new_function = self.functions.pop();
        self.functions
            .top
            .prototypes
            .push(new_function.to_proto(self.mutation_context));
        let dest = self
            .functions
            .top
            .register_allocator
            .allocate()
            .ok_or(CompilerLimit::Registers)?;

        {
            let proto = PrototypeIndex(
                cast(self.functions.top.prototypes.len() - 1).ok_or(CompilerLimit::Functions)?,
            );
            self.functions
                .top
                .opcodes
                .push(OpCode::Closure { proto, dest });
            self.functions
                .top
                .locals
                .push((&local_function.name.name, dest));
        }

        Ok(())
    }

    fn expression(&mut self, expression: &'a Expression) -> Result<ExprDescriptor<'gc, 'a>, Error> {
        let mut expr = self.head_expression(&expression.head)?;
        for (binop, right) in &expression.tail {
            expr = self.binary_operator(expr, *binop, right)?;
        }
        Ok(expr)
    }

    fn head_expression(
        &mut self,
        head_expression: &'a HeadExpression,
    ) -> Result<ExprDescriptor<'gc, 'a>, Error> {
        match head_expression {
            HeadExpression::Simple(simple_expression) => self.simple_expression(simple_expression),
            HeadExpression::UnaryOperator(unop, expr) => {
                let expr = self.expression(expr)?;
                self.unary_operator(*unop, expr)
            }
        }
    }

    fn simple_expression(
        &mut self,
        simple_expression: &'a SimpleExpression,
    ) -> Result<ExprDescriptor<'gc, 'a>, Error> {
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
    ) -> Result<ExprDescriptor<'gc, 'a>, Error> {
        let mut expr = self.primary_expression(&suffixed_expression.primary)?;
        for suffix in &suffixed_expression.suffixes {
            match suffix {
                SuffixPart::Field(_) => bail!("no support for fields yet"),
                SuffixPart::Call(call_suffix) => match call_suffix {
                    CallSuffix::Function(args) => {
                        let args = args
                            .iter()
                            .map(|arg| self.expression(arg))
                            .collect::<Result<_, Error>>()?;
                        expr = ExprDescriptor::FunctionCall {
                            func: Box::new(expr),
                            args,
                        };
                    }
                    CallSuffix::Method(_, _) => bail!("methods not supported yet"),
                },
            }
        }
        Ok(expr)
    }

    fn primary_expression(
        &mut self,
        primary_expression: &'a PrimaryExpression,
    ) -> Result<ExprDescriptor<'gc, 'a>, Error> {
        match primary_expression {
            PrimaryExpression::Name(name) => Ok(match self.find_variable(name)? {
                VariableDescriptor::Local(register) => ExprDescriptor::Register {
                    register,
                    is_temporary: false,
                },
                VariableDescriptor::UpValue(upvalue) => ExprDescriptor::UpValue(upvalue),
                VariableDescriptor::Global(name) => {
                    let mut env = self.get_environment()?;
                    let mut key = ExprDescriptor::Value(Value::String(String::new(
                        self.mutation_context,
                        name,
                    )));
                    let res = self.get_table(&mut env, &mut key)?;
                    self.expr_finish(env)?;
                    self.expr_finish(key)?;
                    res
                }
            }),
            PrimaryExpression::GroupedExpression(expr) => self.expression(expr),
        }
    }

    fn unary_operator(
        &mut self,
        unop: UnaryOperator,
        mut expr: ExprDescriptor<'gc, 'a>,
    ) -> Result<ExprDescriptor<'gc, 'a>, Error> {
        let unop_entry = UNOPS
            .get(&unop)
            .ok_or_else(|| err_msg("unsupported unary operator"))?;

        if let &ExprDescriptor::Value(v) = &expr {
            if let Some(v) = (unop_entry.constant_fold)(v) {
                return Ok(ExprDescriptor::Value(v));
            }
        }

        let source = self.expr_any_register(&mut expr)?;
        self.expr_finish(expr)?;
        let dest = self
            .functions
            .top
            .register_allocator
            .allocate()
            .ok_or(CompilerLimit::Registers)?;
        self.functions
            .top
            .opcodes
            .push((unop_entry.make_opcode)(dest, source));
        Ok(ExprDescriptor::Register {
            register: dest,
            is_temporary: true,
        })
    }

    fn binary_operator(
        &mut self,
        left: ExprDescriptor<'gc, 'a>,
        binop: BinaryOperator,
        right: &'a Expression,
    ) -> Result<ExprDescriptor<'gc, 'a>, Error> {
        fn make_binop_args<'gc, 'a>(
            comp: &mut Compiler<'gc, 'a>,
            mut left: ExprDescriptor<'gc, 'a>,
            mut right: ExprDescriptor<'gc, 'a>,
        ) -> Result<BinOpArgs, Error> {
            let left_cons = comp.as_constant8(&left)?;
            let right_cons = comp.as_constant8(&right)?;

            Ok(if let Some(left_cons) = left_cons {
                let right_reg = comp.expr_any_register(&mut right)?;
                comp.expr_finish(right)?;
                BinOpArgs::CR(left_cons, right_reg)
            } else if let Some(right_cons) = right_cons {
                let left_reg = comp.expr_any_register(&mut left)?;
                comp.expr_finish(left)?;
                BinOpArgs::RC(left_reg, right_cons)
            } else {
                let left_reg = comp.expr_any_register(&mut left)?;
                let right_reg = comp.expr_any_register(&mut right)?;
                comp.expr_finish(right)?;
                comp.expr_finish(left)?;
                BinOpArgs::RR(left_reg, right_reg)
            })
        };

        match categorize_binop(binop) {
            BinOpCategory::Simple(simple_binop) => {
                let binop_entry = SIMPLE_BINOPS
                    .get(&simple_binop)
                    .ok_or_else(|| err_msg("unsupported binary operator"))?;
                let right = self.expression(right)?;

                if let (&ExprDescriptor::Value(a), &ExprDescriptor::Value(b)) = (&left, &right) {
                    if let Some(v) = (binop_entry.constant_fold)(a, b) {
                        return Ok(ExprDescriptor::Value(v));
                    }
                }

                let binop_args = make_binop_args(self, left, right)?;
                let dest = self
                    .functions
                    .top
                    .register_allocator
                    .allocate()
                    .ok_or(CompilerLimit::Registers)?;
                self.functions
                    .top
                    .opcodes
                    .push((binop_entry.make_opcode)(dest, binop_args));
                Ok(ExprDescriptor::Register {
                    register: dest,
                    is_temporary: true,
                })
            }
            BinOpCategory::Comparison(comparison_binop) => {
                let binop_entry = COMPARISON_BINOPS
                    .get(&comparison_binop)
                    .ok_or_else(|| err_msg("unsupported binary operator"))?;
                let right = self.expression(right)?;

                if let (&ExprDescriptor::Value(a), &ExprDescriptor::Value(b)) = (&left, &right) {
                    if let Some(v) = (binop_entry.constant_fold)(a, b) {
                        return Ok(ExprDescriptor::Value(v));
                    }
                }

                let binop_args = make_binop_args(self, left, right)?;
                let dest = self
                    .functions
                    .top
                    .register_allocator
                    .allocate()
                    .ok_or(CompilerLimit::Registers)?;
                self.functions
                    .top
                    .opcodes
                    .extend(&(binop_entry.make_opcodes)(dest, binop_args));
                Ok(ExprDescriptor::Register {
                    register: dest,
                    is_temporary: true,
                })
            }
            BinOpCategory::ShortCircuit(op) => Ok(ExprDescriptor::ShortCircuitBinOp {
                left: Box::new(left),
                op,
                right,
            }),
            _ => bail!("unsupported binary operator"),
        }
    }

    fn find_variable(&mut self, name: &'a [u8]) -> Result<VariableDescriptor<'a>, Error> {
        let function_len = self.functions.len();

        for i in (0..function_len).rev() {
            for j in (0..self.functions.get(i).locals.len()).rev() {
                let (local_name, register) = self.functions.get(i).locals[j];
                if name == local_name {
                    if i == function_len - 1 {
                        return Ok(VariableDescriptor::Local(register));
                    } else {
                        self.functions
                            .get_mut(i + 1)
                            .upvalues
                            .push((name, UpValueDescriptor::ParentLocal(register)));
                        let mut upvalue_index = UpValueIndex(
                            cast(self.functions.get(i + 1).upvalues.len() - 1)
                                .ok_or(CompilerLimit::UpValues)?,
                        );
                        for k in i + 2..function_len {
                            self.functions
                                .get_mut(k)
                                .upvalues
                                .push((name, UpValueDescriptor::Outer(upvalue_index)));
                            upvalue_index = UpValueIndex(
                                cast(self.functions.get(k).upvalues.len() - 1)
                                    .ok_or(CompilerLimit::UpValues)?,
                            );
                        }
                        return Ok(VariableDescriptor::UpValue(upvalue_index));
                    }
                }
            }

            for j in 0..self.functions.get(i).upvalues.len() {
                if name == self.functions.get(i).upvalues[j].0 {
                    let mut upvalue_index = UpValueIndex(cast(j).unwrap());
                    if i == function_len - 1 {
                        return Ok(VariableDescriptor::UpValue(upvalue_index));
                    } else {
                        for k in i + 1..function_len {
                            self.functions
                                .get_mut(k)
                                .upvalues
                                .push((name, UpValueDescriptor::Outer(upvalue_index)));
                            upvalue_index = UpValueIndex(
                                cast(self.functions.get(k).upvalues.len() - 1)
                                    .ok_or(CompilerLimit::UpValues)?,
                            );
                        }
                        return Ok(VariableDescriptor::UpValue(upvalue_index));
                    }
                }
            }
        }

        Ok(VariableDescriptor::Global(name))
    }

    // Get a reference to the variable _ENV in scope, or if that is not in scope, the implicit chunk
    // _ENV.
    fn get_environment(&mut self) -> Result<ExprDescriptor<'gc, 'a>, Error> {
        Ok(match self.find_variable(b"_ENV")? {
            VariableDescriptor::Local(register) => ExprDescriptor::Register {
                register,
                is_temporary: false,
            },
            VariableDescriptor::UpValue(upvalue) => ExprDescriptor::UpValue(upvalue),
            VariableDescriptor::Global(_) => {
                let chunk_function = self.functions.get_mut(0);
                assert!(chunk_function.upvalues.is_empty());
                chunk_function
                    .upvalues
                    .push((b"_ENV", UpValueDescriptor::Environment));
                match self.find_variable(b"_ENV")? {
                    VariableDescriptor::Local(register) => ExprDescriptor::Register {
                        register,
                        is_temporary: false,
                    },
                    VariableDescriptor::UpValue(upvalue) => ExprDescriptor::UpValue(upvalue),
                    VariableDescriptor::Global(_) => unreachable!(),
                }
            }
        })
    }

    // Emit a LoadNil opcode, possibly combining several sequential LoadNil opcodes into one.
    fn load_nil(&mut self, dest: RegisterIndex) -> Result<(), Error> {
        match self.functions.top.opcodes.last().cloned() {
            Some(OpCode::LoadNil {
                dest: prev_dest,
                count: prev_count,
            }) if prev_dest.0 + prev_count == dest.0 => {
                self.functions.top.opcodes.push(OpCode::LoadNil {
                    dest: prev_dest,
                    count: prev_count + 1,
                });
            }
            _ => {
                self.functions
                    .top
                    .opcodes
                    .push(OpCode::LoadNil { dest, count: 1 });
            }
        }
        Ok(())
    }

    fn get_constant(&mut self, constant: Value<'gc>) -> Result<ConstantIndex16, Error> {
        if let Some(constant) = self
            .functions
            .top
            .constant_table
            .get(&ConstantValue(constant))
            .cloned()
        {
            Ok(constant)
        } else {
            let c = ConstantIndex16(
                cast(self.functions.top.constants.len()).ok_or(CompilerLimit::Constants)?,
            );
            self.functions.top.constants.push(constant);
            self.functions
                .top
                .constant_table
                .insert(ConstantValue(constant), c);
            Ok(c)
        }
    }

    fn as_constant8(
        &mut self,
        expr: &ExprDescriptor<'gc, 'a>,
    ) -> Result<Option<ConstantIndex8>, Error> {
        if let &ExprDescriptor::Value(cons) = expr {
            if let Some(c8) = cast(self.get_constant(cons)?.0) {
                return Ok(Some(ConstantIndex8(c8)));
            }
        }
        Ok(None)
    }

    fn get_table(
        &mut self,
        table: &mut ExprDescriptor<'gc, 'a>,
        key: &mut ExprDescriptor<'gc, 'a>,
    ) -> Result<ExprDescriptor<'gc, 'a>, Error> {
        let dest = self
            .functions
            .top
            .register_allocator
            .allocate()
            .ok_or(CompilerLimit::Registers)?;
        let op = match table {
            &mut ExprDescriptor::UpValue(table) => match self.as_constant8(key)? {
                Some(key) => OpCode::GetUpTableC { dest, table, key },
                None => {
                    let key = self.expr_any_register(key)?;
                    OpCode::GetUpTableR { dest, table, key }
                }
            },
            table => {
                let table = self.expr_any_register(table)?;
                match self.as_constant8(key)? {
                    Some(key) => OpCode::GetTableC { dest, table, key },
                    None => {
                        let key = self.expr_any_register(key)?;
                        OpCode::GetTableR { dest, table, key }
                    }
                }
            }
        };

        self.functions.top.opcodes.push(op);
        Ok(ExprDescriptor::Register {
            register: dest,
            is_temporary: true,
        })
    }

    fn set_table(
        &mut self,
        table: &mut ExprDescriptor<'gc, 'a>,
        key: &mut ExprDescriptor<'gc, 'a>,
        value: &mut ExprDescriptor<'gc, 'a>,
    ) -> Result<(), Error> {
        let op = match table {
            &mut ExprDescriptor::UpValue(table) => {
                match (self.as_constant8(key)?, self.as_constant8(value)?) {
                    (None, None) => OpCode::SetUpTableRR {
                        table,
                        key: self.expr_any_register(key)?,
                        value: self.expr_any_register(value)?,
                    },
                    (None, Some(value)) => OpCode::SetUpTableRC {
                        table,
                        key: self.expr_any_register(key)?,
                        value,
                    },
                    (Some(key), None) => OpCode::SetUpTableCR {
                        table,
                        key,
                        value: self.expr_any_register(value)?,
                    },
                    (Some(key), Some(value)) => OpCode::SetUpTableCC { table, key, value },
                }
            }
            table => {
                let table = self.expr_any_register(table)?;
                match (self.as_constant8(key)?, self.as_constant8(value)?) {
                    (None, None) => OpCode::SetTableRR {
                        table,
                        key: self.expr_any_register(key)?,
                        value: self.expr_any_register(value)?,
                    },
                    (None, Some(cval)) => OpCode::SetTableRC {
                        table,
                        key: self.expr_any_register(key)?,
                        value: cval,
                    },
                    (Some(ckey), None) => OpCode::SetTableCR {
                        table,
                        key: ckey,
                        value: self.expr_any_register(value)?,
                    },
                    (Some(ckey), Some(cval)) => OpCode::SetTableCC {
                        table,
                        key: ckey,
                        value: cval,
                    },
                }
            }
        };

        self.functions.top.opcodes.push(op);
        Ok(())
    }

    // Modify an expression to contain its result in any register, and return that register
    fn expr_any_register(
        &mut self,
        expr: &mut ExprDescriptor<'gc, 'a>,
    ) -> Result<RegisterIndex, Error> {
        match *expr {
            ExprDescriptor::Register { register, .. } => return Ok(register),
            _ => {}
        }

        let register = self
            .functions
            .top
            .register_allocator
            .allocate()
            .ok_or(CompilerLimit::Registers)?;
        let old_expr = mem::replace(
            expr,
            ExprDescriptor::Register {
                register,
                is_temporary: true,
            },
        );
        self.expr_to_register(old_expr, register)?;
        Ok(register)
    }

    // Consume an expression, ensuring that its result is stored in a newly allocated register
    fn expr_allocate_register(
        &mut self,
        expr: ExprDescriptor<'gc, 'a>,
    ) -> Result<RegisterIndex, Error> {
        match expr {
            ExprDescriptor::Register {
                register,
                is_temporary: true,
            } => Ok(register),
            expr => {
                let dest = self
                    .functions
                    .top
                    .register_allocator
                    .allocate()
                    .ok_or(CompilerLimit::Registers)?;
                self.expr_to_register(expr, dest)?;
                Ok(dest)
            }
        }
    }

    // Consume an expression, ensuring that its result is stored in a newly allocated register at
    // the top of the stack.
    fn expr_push_register(
        &mut self,
        expr: ExprDescriptor<'gc, 'a>,
    ) -> Result<RegisterIndex, Error> {
        if let ExprDescriptor::Register {
            register,
            is_temporary: true,
        } = expr
        {
            if register.0 as u16 + 1 == self.functions.top.register_allocator.stack_top {
                return Ok(register);
            }
        }

        let dest = self
            .functions
            .top
            .register_allocator
            .push(1)
            .ok_or(CompilerLimit::Registers)?;
        self.expr_to_register(expr, dest)?;
        Ok(dest)
    }

    // Consume an expression and store the result in the given register
    fn expr_to_register(
        &mut self,
        expr: ExprDescriptor<'gc, 'a>,
        dest: RegisterIndex,
    ) -> Result<(), Error> {
        match expr {
            ExprDescriptor::Register {
                register: source,
                is_temporary,
            } => {
                if dest != source {
                    self.functions
                        .top
                        .opcodes
                        .push(OpCode::Move { dest, source });
                }
                if is_temporary {
                    self.functions.top.register_allocator.free(source);
                }
            }
            ExprDescriptor::UpValue(source) => {
                self.functions
                    .top
                    .opcodes
                    .push(OpCode::GetUpValue { source, dest });
            }
            ExprDescriptor::Value(value) => match value {
                Value::Nil => {
                    self.load_nil(dest)?;
                }
                Value::Boolean(value) => {
                    self.functions.top.opcodes.push(OpCode::LoadBool {
                        dest,
                        value,
                        skip_next: false,
                    });
                }
                val => {
                    let constant = self.get_constant(val)?;
                    self.functions
                        .top
                        .opcodes
                        .push(OpCode::LoadConstant { dest, constant });
                }
            },
            ExprDescriptor::FunctionCall { func, args } => {
                let source = self.expr_function_call(*func, args, VarCount::make_one())?;
                self.functions
                    .top
                    .opcodes
                    .push(OpCode::Move { dest, source });
            }
            ExprDescriptor::ShortCircuitBinOp { left, op, right } => {
                self.expr_short_circuit(*left, op, right, Some(dest))?;
            }
        }

        Ok(())
    }

    // Performs a function call, consuming the func and args registers.  At the end of the function
    // call, the return values will be left at the top of the stack, and this method does not mark
    // the return registers as allocated.  Returns the register at which the returns (if any) are
    // placed (which will also be the current register allocator top).
    fn expr_function_call(
        &mut self,
        func: ExprDescriptor<'gc, 'a>,
        mut args: Vec<ExprDescriptor<'gc, 'a>>,
        returns: VarCount,
    ) -> Result<RegisterIndex, Error> {
        let top_reg = self.expr_push_register(func)?;
        let arg_count: u8 = cast(args.len()).ok_or(CompilerLimit::FixedParameters)?;
        let last_arg = args.pop();
        for arg in args {
            self.expr_push_register(arg)?;
        }

        if let Some(ExprDescriptor::FunctionCall { func, args }) = last_arg {
            self.expr_function_call(*func, args, VarCount::make_variable())?;
            self.functions.top.opcodes.push(OpCode::Call {
                func: top_reg,
                args: VarCount::make_variable(),
                returns,
            });
        } else {
            if let Some(last_arg) = last_arg {
                self.expr_push_register(last_arg)?;
            }
            self.functions.top.opcodes.push(OpCode::Call {
                func: top_reg,
                args: VarCount::make_constant(arg_count).ok_or(CompilerLimit::FixedParameters)?,
                returns,
            });
        }
        self.functions
            .top
            .register_allocator
            .pop_to(top_reg.0 as u16);

        Ok(top_reg)
    }

    // Consume a short circuit expression, placing the result in the given destination, if provided.
    fn expr_short_circuit(
        &mut self,
        mut left_expr: ExprDescriptor<'gc, 'a>,
        op: ShortCircuitBinOp,
        right_expr: &'a Expression,
        dest: Option<RegisterIndex>,
    ) -> Result<(), Error> {
        let left = self.expr_any_register(&mut left_expr)?;

        let test_op_true = op == ShortCircuitBinOp::And;
        let test_op = if let Some(dest) = dest {
            if left == dest {
                OpCode::Test {
                    value: left,
                    is_true: test_op_true,
                }
            } else {
                OpCode::TestSet {
                    dest,
                    value: left,
                    is_true: test_op_true,
                }
            }
        } else {
            OpCode::Test {
                value: left,
                is_true: test_op_true,
            }
        };
        self.functions.top.opcodes.push(test_op);

        self.expr_finish(left_expr)?;

        let jmp_inst = self.functions.top.opcodes.len();
        self.functions.top.opcodes.push(OpCode::Jump { offset: 0 });

        let right_expr = self.expression(right_expr)?;
        if let Some(dest) = dest {
            self.expr_to_register(right_expr, dest)?;
        } else {
            self.expr_finish(right_expr)?;
        }

        let jmp_offset =
            cast(self.functions.top.opcodes.len() - jmp_inst - 1).ok_or(CompilerLimit::OpCodes)?;
        match &mut self.functions.top.opcodes[jmp_inst] {
            OpCode::Jump { offset } => {
                *offset = jmp_offset;
            }
            _ => panic!("Jump opcode for short circuit binary operation is misplaced"),
        }

        Ok(())
    }

    // Consume an expression and ignore the result.  If the expression is a pending function call,
    // performs the function call with no expected returns, if it is held in a temporary register,
    // frees that register.  All created `ExprDescriptor`s must be consumed by one of the methods
    // that handles them, if no other method has consumed an expr, `expr_finish` must be called on
    // it.
    fn expr_finish(&mut self, expr: ExprDescriptor<'gc, 'a>) -> Result<(), Error> {
        match expr {
            ExprDescriptor::Register {
                register,
                is_temporary,
            } => {
                if is_temporary {
                    self.functions.top.register_allocator.free(register);
                }
            }
            ExprDescriptor::UpValue(_) => {}
            ExprDescriptor::Value(_) => {}
            ExprDescriptor::FunctionCall { func, args } => {
                self.expr_function_call(*func, args, VarCount::make_zero())?;
            }
            ExprDescriptor::ShortCircuitBinOp { left, op, right } => {
                self.expr_short_circuit(*left, op, right, None)?;
            }
        }

        Ok(())
    }
}

#[derive(Default)]
struct CompilerFunction<'gc, 'a> {
    constants: Vec<Value<'gc>>,
    constant_table: HashMap<ConstantValue<'gc>, ConstantIndex16>,

    upvalues: Vec<(&'a [u8], UpValueDescriptor)>,
    prototypes: Vec<FunctionProto<'gc>>,

    register_allocator: RegisterAllocator,

    fixed_params: u8,
    locals: Vec<(&'a [u8], RegisterIndex)>,

    opcodes: Vec<OpCode>,
}

impl<'gc, 'a> CompilerFunction<'gc, 'a> {
    fn to_proto(self, mc: MutationContext<'gc, 'a>) -> FunctionProto<'gc> {
        assert_eq!(
            self.register_allocator.stack_top as usize,
            self.locals.len(),
            "register leak detected"
        );
        FunctionProto {
            fixed_params: self.fixed_params,
            has_varargs: false,
            stack_size: self.register_allocator.stack_size,
            constants: self.constants,
            opcodes: self.opcodes,
            upvalues: self.upvalues.iter().map(|(_, d)| *d).collect(),
            prototypes: self
                .prototypes
                .into_iter()
                .map(|f| Gc::allocate(mc, f))
                .collect(),
        }
    }
}

#[derive(Debug)]
enum VariableDescriptor<'a> {
    Local(RegisterIndex),
    UpValue(UpValueIndex),
    Global(&'a [u8]),
}

#[derive(Debug)]
enum ExprDescriptor<'gc, 'a> {
    Register {
        register: RegisterIndex,
        is_temporary: bool,
    },
    UpValue(UpValueIndex),
    Value(Value<'gc>),
    FunctionCall {
        func: Box<ExprDescriptor<'gc, 'a>>,
        args: Vec<ExprDescriptor<'gc, 'a>>,
    },
    ShortCircuitBinOp {
        left: Box<ExprDescriptor<'gc, 'a>>,
        op: ShortCircuitBinOp,
        right: &'a Expression,
    },
}

struct RegisterAllocator {
    // The total array of registers, marking whether they are allocated
    registers: [bool; 256],
    // The first free register
    first_free: u16,
    // The free register after the last used register
    stack_top: u16,
    // The index of the largest used register + 1 (e.g. the stack size required for the function)
    stack_size: u16,
}

impl Default for RegisterAllocator {
    fn default() -> RegisterAllocator {
        RegisterAllocator {
            registers: [false; 256],
            first_free: 0,
            stack_top: 0,
            stack_size: 0,
        }
    }
}

impl RegisterAllocator {
    // Allocates any single available register, returns it if one is available.
    fn allocate(&mut self) -> Option<RegisterIndex> {
        if self.first_free < 256 {
            let register = self.first_free as u8;
            self.registers[register as usize] = true;

            if self.first_free == self.stack_top {
                self.stack_top += 1;
            }
            self.stack_size = self.stack_size.max(self.stack_top);

            let mut i = self.first_free;
            self.first_free = loop {
                if i == 256 || !self.registers[i as usize] {
                    break i;
                }
                i += 1;
            };

            Some(RegisterIndex(register))
        } else {
            None
        }
    }

    // Free a single register.
    fn free(&mut self, register: RegisterIndex) {
        assert!(
            self.registers[register.0 as usize],
            "cannot free unallocated register"
        );
        self.registers[register.0 as usize] = false;
        self.first_free = self.first_free.min(register.0 as u16);
        if register.0 as u16 + 1 == self.stack_top {
            self.stack_top -= 1;
        }
    }

    // Allocates a block of registers of the given size (which must be > 0) always at the end of the
    // allocated area.  If successful, returns the starting register of the block.
    fn push(&mut self, size: u8) -> Option<RegisterIndex> {
        if size == 0 {
            None
        } else if size as u16 <= 256 - self.stack_top {
            let rbegin = self.stack_top as u8;
            for i in rbegin..rbegin + size {
                self.registers[i as usize] = true;
            }
            if self.first_free == self.stack_top {
                self.first_free += size as u16;
            }
            self.stack_top += size as u16;
            self.stack_size = self.stack_size.max(self.stack_top);
            Some(RegisterIndex(rbegin))
        } else {
            None
        }
    }

    // Free all registers past the given register, making the given register the new top of the
    // stack.  If the given register is >= to the current top, this will have no effect.
    fn pop_to(&mut self, new_top: u16) {
        if self.stack_top > new_top {
            for i in new_top..self.stack_top {
                self.registers[i as usize] = false;
            }
            self.stack_top = new_top;
            self.first_free = self.first_free.min(self.stack_top);
        }
    }
}

// A stack which is guaranteed always to have a top value
struct TopStack<T> {
    top: T,
    lower: Vec<T>,
}

impl<T> TopStack<T> {
    fn new(top: T) -> TopStack<T> {
        TopStack {
            top,
            lower: Vec::new(),
        }
    }

    fn push(&mut self, t: T) {
        self.lower.push(mem::replace(&mut self.top, t));
    }

    fn pop(&mut self) -> T {
        mem::replace(
            &mut self.top,
            self.lower
                .pop()
                .expect("TopStack must always have one entry"),
        )
    }

    fn len(&self) -> usize {
        self.lower.len() + 1
    }

    fn get(&self, i: usize) -> &T {
        let lower_len = self.lower.len();
        if i < lower_len {
            &self.lower[i]
        } else if i == lower_len {
            &self.top
        } else {
            panic!("TopStack index {} out of range", i);
        }
    }

    fn get_mut(&mut self, i: usize) -> &mut T {
        let lower_len = self.lower.len();
        if i < lower_len {
            &mut self.lower[i]
        } else if i == lower_len {
            &mut self.top
        } else {
            panic!("TopStack index {} out of range", i);
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
