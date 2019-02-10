use std::error::Error as StdError;
use std::fmt;

use gc_arena::{Collect, Gc, MutationContext};

use crate::{
    thread::LuaFrame, Closure, ClosureState, Error, Function, OpCode, RegisterIndex, String, Table,
    TypeError, UpValueDescriptor, Value, VarCount,
};

#[derive(Debug, Clone, Copy, Collect)]
#[collect(require_static)]
pub enum BinaryOperatorError {
    Add,
    Subtract,
    Multiply,
    LessThan,
}

impl StdError for BinaryOperatorError {}

impl fmt::Display for BinaryOperatorError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOperatorError::Add => write!(fmt, "cannot add values"),
            BinaryOperatorError::Subtract => write!(fmt, "cannot subtract values"),
            BinaryOperatorError::Multiply => write!(fmt, "cannot multiply values"),
            BinaryOperatorError::LessThan => write!(fmt, "cannot compare values with <"),
        }
    }
}

// Runs the VM until the current LuaFrame may have been changed.  Returns the number of instructions
// left to run (or 0 if all requested instructions were run).
pub fn run_vm<'gc>(
    mc: MutationContext<'gc, '_>,
    mut lua_frame: LuaFrame<'gc, '_>,
    mut instructions: u32,
) -> Result<u32, Error<'gc>> {
    let current_function = lua_frame.closure();
    let mut registers = lua_frame.registers();

    loop {
        let op = current_function.0.proto.opcodes[*registers.pc];
        *registers.pc += 1;

        match op {
            OpCode::Move { dest, source } => {
                registers.stack_frame[dest.0 as usize] = registers.stack_frame[source.0 as usize];
            }

            OpCode::LoadConstant { dest, constant } => {
                registers.stack_frame[dest.0 as usize] =
                    current_function.0.proto.constants[constant.0 as usize].to_value();
            }

            OpCode::LoadBool {
                dest,
                value,
                skip_next,
            } => {
                registers.stack_frame[dest.0 as usize] = Value::Boolean(value);
                if skip_next {
                    *registers.pc += 1;
                }
            }

            OpCode::LoadNil { dest, count } => {
                for i in dest.0..dest.0 + count {
                    registers.stack_frame[i as usize] = Value::Nil;
                }
            }

            OpCode::NewTable { dest } => {
                registers.stack_frame[dest.0 as usize] = Value::Table(Table::new(mc));
            }

            OpCode::GetTableR { dest, table, key } => {
                registers.stack_frame[dest.0 as usize] =
                    get_table(registers.stack_frame[table.0 as usize])?
                        .get(registers.stack_frame[key.0 as usize]);
            }

            OpCode::GetTableC { dest, table, key } => {
                registers.stack_frame[dest.0 as usize] =
                    get_table(registers.stack_frame[table.0 as usize])?
                        .get(current_function.0.proto.constants[key.0 as usize].to_value())
            }

            OpCode::SetTableRR { table, key, value } => {
                get_table(registers.stack_frame[table.0 as usize])?.set(
                    mc,
                    registers.stack_frame[key.0 as usize],
                    registers.stack_frame[value.0 as usize],
                )?;
            }

            OpCode::SetTableRC { table, key, value } => {
                get_table(registers.stack_frame[table.0 as usize])?.set(
                    mc,
                    registers.stack_frame[key.0 as usize],
                    current_function.0.proto.constants[value.0 as usize].to_value(),
                )?;
            }

            OpCode::SetTableCR { table, key, value } => {
                get_table(registers.stack_frame[table.0 as usize])?.set(
                    mc,
                    current_function.0.proto.constants[key.0 as usize].to_value(),
                    registers.stack_frame[value.0 as usize],
                )?;
            }

            OpCode::SetTableCC { table, key, value } => {
                get_table(registers.stack_frame[table.0 as usize])?.set(
                    mc,
                    current_function.0.proto.constants[key.0 as usize].to_value(),
                    current_function.0.proto.constants[value.0 as usize].to_value(),
                )?;
            }

            OpCode::GetUpTableR { dest, table, key } => {
                registers.stack_frame[dest.0 as usize] = get_table(
                    registers.get_upvalue(current_function.0.upvalues[table.0 as usize]),
                )?
                .get(registers.stack_frame[key.0 as usize]);
            }

            OpCode::GetUpTableC { dest, table, key } => {
                registers.stack_frame[dest.0 as usize] =
                    get_table(registers.get_upvalue(current_function.0.upvalues[table.0 as usize]))?
                        .get(current_function.0.proto.constants[key.0 as usize].to_value())
            }

            OpCode::SetUpTableRR { table, key, value } => {
                get_table(registers.get_upvalue(current_function.0.upvalues[table.0 as usize]))?
                    .set(
                        mc,
                        registers.stack_frame[key.0 as usize],
                        registers.stack_frame[value.0 as usize],
                    )?;
            }

            OpCode::SetUpTableRC { table, key, value } => {
                get_table(registers.get_upvalue(current_function.0.upvalues[table.0 as usize]))?
                    .set(
                        mc,
                        registers.stack_frame[key.0 as usize],
                        current_function.0.proto.constants[value.0 as usize].to_value(),
                    )?;
            }

            OpCode::SetUpTableCR { table, key, value } => {
                get_table(registers.get_upvalue(current_function.0.upvalues[table.0 as usize]))?
                    .set(
                        mc,
                        current_function.0.proto.constants[key.0 as usize].to_value(),
                        registers.stack_frame[value.0 as usize],
                    )?;
            }

            OpCode::SetUpTableCC { table, key, value } => {
                get_table(registers.get_upvalue(current_function.0.upvalues[table.0 as usize]))?
                    .set(
                        mc,
                        current_function.0.proto.constants[key.0 as usize].to_value(),
                        current_function.0.proto.constants[value.0 as usize].to_value(),
                    )?;
            }

            OpCode::Call {
                func,
                args,
                returns,
            } => {
                lua_frame.call_function(func, args, returns)?;
                break;
            }

            OpCode::TailCall { func, args } => {
                lua_frame.tail_call_function(func, args)?;
                break;
            }

            OpCode::Return { start, count } => {
                lua_frame.return_upper(start, count)?;
                break;
            }

            OpCode::VarArgs { dest, count } => {
                lua_frame.varargs(dest, count)?;
                break;
            }

            OpCode::Jump {
                offset,
                close_upvalues,
            } => {
                *registers.pc = add_offset(*registers.pc, offset);
                if let Some(r) = close_upvalues.to_u8() {
                    registers.close_upvalues(RegisterIndex(r));
                }
            }

            OpCode::Test { value, is_true } => {
                let value = registers.stack_frame[value.0 as usize];
                if value.to_bool() == is_true {
                    *registers.pc += 1;
                }
            }

            OpCode::TestSet {
                dest,
                value,
                is_true,
            } => {
                let value = registers.stack_frame[value.0 as usize];
                if value.to_bool() == is_true {
                    *registers.pc += 1;
                } else {
                    registers.stack_frame[dest.0 as usize] = value;
                }
            }

            OpCode::Closure { proto, dest } => {
                let proto = current_function.0.proto.prototypes[proto.0 as usize];
                let mut upvalues = Vec::new();
                for &desc in &proto.upvalues {
                    match desc {
                        UpValueDescriptor::Environment => {
                            panic!("_ENV upvalue is only allowed on top-level closure");
                        }
                        UpValueDescriptor::ParentLocal(reg) => {
                            upvalues.push(registers.open_upvalue(reg));
                        }
                        UpValueDescriptor::Outer(uvindex) => {
                            upvalues.push(current_function.0.upvalues[uvindex.0 as usize]);
                        }
                    }
                }

                let closure = Closure(Gc::allocate(mc, ClosureState { proto, upvalues }));
                registers.stack_frame[dest.0 as usize] =
                    Value::Function(Function::Closure(closure));
            }

            OpCode::NumericForPrep { base, jump } => {
                registers.stack_frame[base.0 as usize] = registers.stack_frame[base.0 as usize]
                    .subtract(registers.stack_frame[base.0 as usize + 2])
                    .ok_or(BinaryOperatorError::Subtract)?;
                *registers.pc = add_offset(*registers.pc, jump);
            }

            OpCode::NumericForLoop { base, jump } => {
                registers.stack_frame[base.0 as usize] = registers.stack_frame[base.0 as usize]
                    .add(registers.stack_frame[base.0 as usize + 2])
                    .ok_or(BinaryOperatorError::Add)?;
                let past_end = if registers.stack_frame[base.0 as usize + 2]
                    .less_than(Value::Integer(0))
                    .ok_or(BinaryOperatorError::LessThan)?
                {
                    registers.stack_frame[base.0 as usize]
                        .less_than(registers.stack_frame[base.0 as usize + 1])
                        .ok_or(BinaryOperatorError::LessThan)?
                } else {
                    registers.stack_frame[base.0 as usize + 1]
                        .less_than(registers.stack_frame[base.0 as usize])
                        .ok_or(BinaryOperatorError::LessThan)?
                };
                if !past_end {
                    *registers.pc = add_offset(*registers.pc, jump);
                    registers.stack_frame[base.0 as usize + 3] =
                        registers.stack_frame[base.0 as usize];
                }
            }

            OpCode::GenericForCall { base, var_count } => {
                lua_frame.call_function_non_destructive(base, 2, VarCount::constant(var_count))?;
                break;
            }

            OpCode::GenericForLoop { base, jump } => {
                if registers.stack_frame[base.0 as usize + 1].to_bool() {
                    registers.stack_frame[base.0 as usize] =
                        registers.stack_frame[base.0 as usize + 1];
                    *registers.pc = add_offset(*registers.pc, jump);
                }
            }

            OpCode::SelfR { base, table, key } => {
                let table = registers.stack_frame[table.0 as usize];
                let key = current_function.0.proto.constants[key.0 as usize].to_value();
                registers.stack_frame[base.0 as usize + 1] = table;
                registers.stack_frame[base.0 as usize] = get_table(table)?.get(key);
            }

            OpCode::SelfC { base, table, key } => {
                let table = registers.stack_frame[table.0 as usize];
                let key = current_function.0.proto.constants[key.0 as usize].to_value();
                registers.stack_frame[base.0 as usize + 1] = table;
                registers.stack_frame[base.0 as usize] = get_table(table)?.get(key);
            }

            OpCode::Concat {
                dest,
                source,
                count,
            } => {
                registers.stack_frame[dest.0 as usize] = Value::String(
                    String::concat(
                        mc,
                        &registers.stack_frame
                            [source.0 as usize..source.0 as usize + count as usize],
                    )
                    .unwrap(),
                );
            }

            OpCode::GetUpValue { source, dest } => {
                registers.stack_frame[dest.0 as usize] =
                    registers.get_upvalue(current_function.0.upvalues[source.0 as usize]);
            }

            OpCode::SetUpValue { source, dest } => {
                registers.set_upvalue(
                    current_function.0.upvalues[dest.0 as usize],
                    registers.stack_frame[source.0 as usize],
                );
            }

            OpCode::Length { dest, source } => {
                registers.stack_frame[dest.0 as usize] =
                    Value::Integer(get_table(registers.stack_frame[source.0 as usize])?.length());
            }

            OpCode::EqRR {
                skip_if,
                left,
                right,
            } => {
                let left = registers.stack_frame[left.0 as usize];
                let right = registers.stack_frame[right.0 as usize];
                if (left == right) == skip_if {
                    *registers.pc += 1;
                }
            }

            OpCode::EqRC {
                skip_if,
                left,
                right,
            } => {
                let left = registers.stack_frame[left.0 as usize];
                let right = current_function.0.proto.constants[right.0 as usize].to_value();
                if (left == right) == skip_if {
                    *registers.pc += 1;
                }
            }

            OpCode::EqCR {
                skip_if,
                left,
                right,
            } => {
                let left = current_function.0.proto.constants[left.0 as usize].to_value();
                let right = registers.stack_frame[right.0 as usize];
                if (left == right) == skip_if {
                    *registers.pc += 1;
                }
            }

            OpCode::EqCC {
                skip_if,
                left,
                right,
            } => {
                let left = current_function.0.proto.constants[left.0 as usize];
                let right = current_function.0.proto.constants[right.0 as usize];
                if (left == right) == skip_if {
                    *registers.pc += 1;
                }
            }

            OpCode::Not { dest, source } => {
                let source = registers.stack_frame[source.0 as usize];
                registers.stack_frame[dest.0 as usize] = source.not();
            }

            OpCode::AddRR { dest, left, right } => {
                let left = registers.stack_frame[left.0 as usize];
                let right = registers.stack_frame[right.0 as usize];
                registers.stack_frame[dest.0 as usize] =
                    left.add(right).ok_or(BinaryOperatorError::Add)?;
            }

            OpCode::AddRC { dest, left, right } => {
                let left = registers.stack_frame[left.0 as usize];
                let right = current_function.0.proto.constants[right.0 as usize].to_value();
                registers.stack_frame[dest.0 as usize] =
                    left.add(right).ok_or(BinaryOperatorError::Add)?;
            }

            OpCode::AddCR { dest, left, right } => {
                let left = current_function.0.proto.constants[left.0 as usize].to_value();
                let right = registers.stack_frame[right.0 as usize];
                registers.stack_frame[dest.0 as usize] =
                    left.add(right).ok_or(BinaryOperatorError::Add)?;
            }

            OpCode::AddCC { dest, left, right } => {
                let left = current_function.0.proto.constants[left.0 as usize].to_value();
                let right = current_function.0.proto.constants[right.0 as usize].to_value();
                registers.stack_frame[dest.0 as usize] =
                    left.add(right).ok_or(BinaryOperatorError::Add)?;
            }

            OpCode::SubRR { dest, left, right } => {
                let left = registers.stack_frame[left.0 as usize];
                let right = registers.stack_frame[right.0 as usize];
                registers.stack_frame[dest.0 as usize] =
                    left.subtract(right).ok_or(BinaryOperatorError::Add)?;
            }

            OpCode::SubRC { dest, left, right } => {
                let left = registers.stack_frame[left.0 as usize];
                let right = current_function.0.proto.constants[right.0 as usize].to_value();
                registers.stack_frame[dest.0 as usize] =
                    left.subtract(right).ok_or(BinaryOperatorError::Subtract)?;
            }

            OpCode::SubCR { dest, left, right } => {
                let left = current_function.0.proto.constants[left.0 as usize].to_value();
                let right = registers.stack_frame[right.0 as usize];
                registers.stack_frame[dest.0 as usize] =
                    left.subtract(right).ok_or(BinaryOperatorError::Subtract)?;
            }

            OpCode::SubCC { dest, left, right } => {
                let left = current_function.0.proto.constants[left.0 as usize].to_value();
                let right = current_function.0.proto.constants[right.0 as usize].to_value();
                registers.stack_frame[dest.0 as usize] =
                    left.subtract(right).ok_or(BinaryOperatorError::Subtract)?;
            }

            OpCode::MulRR { dest, left, right } => {
                let left = registers.stack_frame[left.0 as usize];
                let right = registers.stack_frame[right.0 as usize];
                registers.stack_frame[dest.0 as usize] =
                    left.multiply(right).ok_or(BinaryOperatorError::Multiply)?;
            }

            OpCode::MulRC { dest, left, right } => {
                let left = registers.stack_frame[left.0 as usize];
                let right = current_function.0.proto.constants[right.0 as usize].to_value();
                registers.stack_frame[dest.0 as usize] =
                    left.multiply(right).ok_or(BinaryOperatorError::Multiply)?;
            }

            OpCode::MulCR { dest, left, right } => {
                let left = current_function.0.proto.constants[left.0 as usize].to_value();
                let right = registers.stack_frame[right.0 as usize];
                registers.stack_frame[dest.0 as usize] =
                    left.multiply(right).ok_or(BinaryOperatorError::Multiply)?;
            }

            OpCode::MulCC { dest, left, right } => {
                let left = current_function.0.proto.constants[left.0 as usize].to_value();
                let right = current_function.0.proto.constants[right.0 as usize].to_value();
                registers.stack_frame[dest.0 as usize] =
                    left.multiply(right).ok_or(BinaryOperatorError::Multiply)?;
            }
        }

        if instructions == 0 {
            break;
        } else {
            instructions -= 1
        }
    }

    Ok(instructions)
}

fn get_table<'gc>(value: Value<'gc>) -> Result<Table<'gc>, TypeError> {
    match value {
        Value::Table(t) => Ok(t),
        val => Err(TypeError {
            expected: "table",
            found: val.type_name(),
        }),
    }
}

fn add_offset(pc: usize, offset: i16) -> usize {
    if offset > 0 {
        pc.checked_add(offset as usize).unwrap()
    } else if offset < 0 {
        pc.checked_sub(-offset as usize).unwrap()
    } else {
        pc
    }
}
