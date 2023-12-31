use allocator_api2::vec;
use gc_arena::allocator_api::MetricsAlloc;
use thiserror::Error;

use crate::{
    meta_ops::{self, MetaResult},
    opcode::{Operation, RCIndex},
    raw_ops,
    table::RawTable,
    thread::thread::MetaReturn,
    types::{RegisterIndex, UpValueDescriptor, VarCount},
    Closure, Constant, Context, Function, RuntimeError, String, Table, Value,
};

use super::{thread::LuaFrame, VMError};

#[derive(Debug, Copy, Clone, Error)]
pub enum BinaryOperatorError {
    #[error("cannot add values")]
    Add,
    #[error("cannot subtract values")]
    Subtract,
    #[error("cannot multiply values")]
    Multiply,
    #[error("cannot float divide values")]
    FloatDivide,
    #[error("cannot floor divide values")]
    FloorDivide,
    #[error("cannot modulo values")]
    Modulo,
    #[error("cannot exponentiate values")]
    Exponentiate,
    #[error("cannot negate value")]
    UnaryNegate,
    #[error("cannot bitwise AND values")]
    BitAnd,
    #[error("cannot bitwise OR values")]
    BitOr,
    #[error("cannot bitwise XOR values")]
    BitXor,
    #[error("cannot bitwise NOT value")]
    BitNot,
    #[error("cannot shift value left")]
    ShiftLeft,
    #[error("cannot shift value right")]
    ShiftRight,
    #[error("cannot compare values with <")]
    LessThan,
    #[error("cannot compare values with <=")]
    LessEqual,
}

// Runs the VM for the given number of instructions or until the current LuaFrame may have been
// changed.
//
// Returns the number of instructions that were run.
pub(super) fn run_vm<'gc>(
    ctx: Context<'gc>,
    mut lua_frame: LuaFrame<'gc, '_>,
    max_instructions: u32,
) -> Result<u32, RuntimeError> {
    if max_instructions == 0 {
        return Ok(0);
    }

    let current_function = lua_frame.closure();
    let current_prototype = current_function.prototype();
    let current_upvalues = current_function.upvalues();
    let mut registers = lua_frame.registers();
    let mut instructions_run = 0;

    fn get_rc<'gc>(
        stack_frame: &[Value<'gc>],
        constants: &[Constant<String<'gc>>],
        rc: RCIndex,
    ) -> Value<'gc> {
        match rc {
            RCIndex::Register(r) => stack_frame[r.0 as usize],
            RCIndex::Constant(c) => constants[c.0 as usize].into(),
        }
    }

    loop {
        let op = current_prototype.opcodes[*registers.pc].decode();
        *registers.pc += 1;

        match op {
            Operation::Move { dest, source } => {
                registers.stack_frame[dest.0 as usize] = registers.stack_frame[source.0 as usize];
            }

            Operation::LoadConstant { dest, constant } => {
                registers.stack_frame[dest.0 as usize] =
                    current_prototype.constants[constant.0 as usize].into();
            }

            Operation::LoadBool {
                dest,
                value,
                skip_next,
            } => {
                registers.stack_frame[dest.0 as usize] = Value::Boolean(value);
                if skip_next {
                    *registers.pc += 1;
                }
            }

            Operation::LoadNil { dest, count } => {
                for i in dest.0..dest.0 + count {
                    registers.stack_frame[i as usize] = Value::Nil;
                }
            }

            Operation::NewTable {
                dest,
                array_size,
                map_size,
            } => {
                let mut entries = RawTable::new(&ctx);
                entries.reserve_array(array_size as usize);
                entries.reserve_map(map_size as usize);
                let table = Table::from_parts(&ctx, entries, None);
                registers.stack_frame[dest.0 as usize] = Value::Table(table);
            }

            Operation::GetTable { dest, table, key } => {
                let table = registers.stack_frame[table.0 as usize];
                let key = get_rc(&registers.stack_frame, &current_prototype.constants, key);
                match meta_ops::index(ctx, table, key)? {
                    MetaResult::Value(v) => {
                        registers.stack_frame[dest.0 as usize] = v;
                    }
                    MetaResult::Call(call) => {
                        lua_frame.call_meta_function(
                            ctx,
                            call.function,
                            &call.args,
                            MetaReturn::Register(dest),
                        )?;
                        break;
                    }
                }
            }

            Operation::SetTable { table, key, value } => {
                let table = registers.stack_frame[table.0 as usize];
                let key = get_rc(&registers.stack_frame, &current_prototype.constants, key);
                let value = get_rc(&registers.stack_frame, &current_prototype.constants, value);
                if let Some(call) = meta_ops::new_index(ctx, table, key, value)? {
                    lua_frame.call_meta_function(
                        ctx,
                        call.function,
                        &call.args,
                        MetaReturn::None,
                    )?;
                    break;
                }
            }

            Operation::GetUpTable { dest, table, key } => {
                let table = registers.get_upvalue(&ctx, current_upvalues[table.0 as usize]);
                let key = get_rc(&registers.stack_frame, &current_prototype.constants, key);
                match meta_ops::index(ctx, table, key)? {
                    MetaResult::Value(v) => {
                        registers.stack_frame[dest.0 as usize] = v;
                    }
                    MetaResult::Call(call) => {
                        lua_frame.call_meta_function(
                            ctx,
                            call.function,
                            &call.args,
                            MetaReturn::Register(dest),
                        )?;
                        break;
                    }
                }
            }

            Operation::SetUpTable { table, key, value } => {
                let table = registers.get_upvalue(&ctx, current_upvalues[table.0 as usize]);
                let key = get_rc(&registers.stack_frame, &current_prototype.constants, key);
                let value = get_rc(&registers.stack_frame, &current_prototype.constants, value);
                if let Some(call) = meta_ops::new_index(ctx, table, key, value)? {
                    lua_frame.call_meta_function(
                        ctx,
                        call.function,
                        &call.args,
                        MetaReturn::None,
                    )?;
                    break;
                }
            }

            Operation::SetList { base, count } => {
                lua_frame.set_table_list(&ctx, base, count)?;
                registers = lua_frame.registers();
            }

            Operation::Call {
                func,
                args,
                returns,
            } => {
                lua_frame.call_function(ctx, func, args, returns)?;
                break;
            }

            Operation::TailCall { func, args } => {
                lua_frame.tail_call_function(ctx, func, args)?;
                break;
            }

            Operation::Return { start, count } => {
                lua_frame.return_upper(&ctx, start, count)?;
                break;
            }

            Operation::VarArgs { dest, count } => {
                lua_frame.varargs(dest, count)?;
                registers = lua_frame.registers();
            }

            Operation::Jump {
                offset,
                close_upvalues,
            } => {
                *registers.pc = add_offset(*registers.pc, offset);
                if let Some(r) = close_upvalues.to_u8() {
                    registers.close_upvalues(&ctx, RegisterIndex(r));
                }
            }

            Operation::Test { value, is_true } => {
                let value = registers.stack_frame[value.0 as usize];
                if value.to_bool() == is_true {
                    *registers.pc += 1;
                }
            }

            Operation::TestSet {
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

            Operation::Closure { proto, dest } => {
                let proto = current_prototype.prototypes[proto.0 as usize];
                let mut upvalues = vec::Vec::new_in(MetricsAlloc::new(&ctx));
                for &desc in proto.upvalues.iter() {
                    match desc {
                        UpValueDescriptor::Environment => {
                            return Err(VMError::BadEnvUpValue.into());
                        }
                        UpValueDescriptor::ParentLocal(reg) => {
                            upvalues.push(registers.open_upvalue(&ctx, reg));
                        }
                        UpValueDescriptor::Outer(uvindex) => {
                            upvalues.push(current_upvalues[uvindex.0 as usize]);
                        }
                    }
                }

                let closure = Closure::from_parts(&ctx, proto, upvalues);
                registers.stack_frame[dest.0 as usize] =
                    Value::Function(Function::Closure(closure));
            }

            Operation::NumericForPrep { base, jump } => {
                registers.stack_frame[base.0 as usize] = raw_ops::subtract(
                    registers.stack_frame[base.0 as usize],
                    registers.stack_frame[base.0 as usize + 2],
                )
                .ok_or(BinaryOperatorError::Subtract)?;
                *registers.pc = add_offset(*registers.pc, jump);
            }

            Operation::NumericForLoop { base, jump } => {
                match (
                    registers.stack_frame[base.0 as usize],
                    registers.stack_frame[base.0 as usize + 1],
                    registers.stack_frame[base.0 as usize + 2],
                ) {
                    (Value::Integer(index), Value::Integer(limit), Value::Integer(step)) => {
                        let index = index + step;
                        registers.stack_frame[base.0 as usize] = Value::Integer(index);

                        let past_end = if step < 0 {
                            index < limit
                        } else {
                            limit < index
                        };
                        if !past_end {
                            *registers.pc = add_offset(*registers.pc, jump);
                            registers.stack_frame[base.0 as usize + 3] = Value::Integer(index);
                        }
                    }
                    (index, limit, step) => {
                        if let (Some(index), Some(limit), Some(step)) =
                            (index.to_number(), limit.to_number(), step.to_number())
                        {
                            let index = index + step;
                            registers.stack_frame[base.0 as usize] = Value::Number(index);

                            let past_end = if step < 0.0 {
                                index < limit
                            } else {
                                limit < index
                            };
                            if !past_end {
                                *registers.pc = add_offset(*registers.pc, jump);
                                registers.stack_frame[base.0 as usize + 3] = Value::Number(index);
                            }
                        } else {
                            return Err(BinaryOperatorError::Add.into());
                        }
                    }
                }
            }

            Operation::GenericForCall { base, var_count } => {
                lua_frame.call_function_keep(ctx, base, 2, VarCount::constant(var_count))?;
                break;
            }

            Operation::GenericForLoop { base, jump } => {
                if registers.stack_frame[base.0 as usize + 1].to_bool() {
                    registers.stack_frame[base.0 as usize] =
                        registers.stack_frame[base.0 as usize + 1];
                    *registers.pc = add_offset(*registers.pc, jump);
                }
            }

            Operation::Method { base, table, key } => {
                let table = registers.stack_frame[table.0 as usize];
                let key = get_rc(&registers.stack_frame, &current_prototype.constants, key);
                registers.stack_frame[base.0 as usize + 1] = table;
                match meta_ops::index(ctx, table, key)? {
                    MetaResult::Value(v) => {
                        registers.stack_frame[base.0 as usize] = v;
                    }
                    MetaResult::Call(call) => {
                        lua_frame.call_meta_function(
                            ctx,
                            call.function,
                            &call.args,
                            MetaReturn::Register(base),
                        )?;
                        break;
                    }
                }
            }

            Operation::Concat {
                dest,
                source,
                count,
            } => {
                registers.stack_frame[dest.0 as usize] = Value::String(String::concat(
                    ctx,
                    &registers.stack_frame[source.0 as usize..source.0 as usize + count as usize],
                )?);
            }

            Operation::GetUpValue { source, dest } => {
                registers.stack_frame[dest.0 as usize] =
                    registers.get_upvalue(&ctx, current_upvalues[source.0 as usize]);
            }

            Operation::SetUpValue { source, dest } => {
                registers.set_upvalue(
                    &ctx,
                    current_upvalues[dest.0 as usize],
                    registers.stack_frame[source.0 as usize],
                );
            }

            Operation::Length { dest, source } => {
                match meta_ops::len(ctx, registers.stack_frame[source.0 as usize])? {
                    MetaResult::Value(v) => {
                        registers.stack_frame[dest.0 as usize] = v;
                    }
                    MetaResult::Call(call) => {
                        lua_frame.call_meta_function(
                            ctx,
                            call.function,
                            &call.args,
                            MetaReturn::Register(dest),
                        )?;
                        break;
                    }
                }
            }

            Operation::Eq {
                skip_if,
                left,
                right,
            } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                match meta_ops::equal(ctx, left, right)? {
                    MetaResult::Value(v) => {
                        if v.to_bool() == skip_if {
                            *registers.pc += 1;
                        }
                    }
                    MetaResult::Call(call) => {
                        lua_frame.call_meta_function(
                            ctx,
                            call.function,
                            &call.args,
                            MetaReturn::SkipIf(skip_if),
                        )?;
                        break;
                    }
                }
            }

            Operation::Less {
                skip_if,
                left,
                right,
            } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                if (raw_ops::less_than(left, right).ok_or(BinaryOperatorError::LessThan)?)
                    == skip_if
                {
                    *registers.pc += 1;
                }
            }

            Operation::LessEq {
                skip_if,
                left,
                right,
            } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                if (raw_ops::less_equal(left, right).ok_or(BinaryOperatorError::LessEqual)?)
                    == skip_if
                {
                    *registers.pc += 1;
                }
            }

            Operation::Not { dest, source } => {
                let source = registers.stack_frame[source.0 as usize];
                registers.stack_frame[dest.0 as usize] = source.not();
            }

            Operation::Minus { dest, source } => {
                let value = registers.stack_frame[source.0 as usize];
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::negate(value).ok_or(BinaryOperatorError::UnaryNegate)?;
            }

            Operation::BitNot { dest, source } => {
                let value = registers.stack_frame[source.0 as usize];
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::bitwise_not(value).ok_or(BinaryOperatorError::BitNot)?;
            }

            Operation::Add { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::add(left, right).ok_or(BinaryOperatorError::Add)?;
            }

            Operation::Sub { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::subtract(left, right).ok_or(BinaryOperatorError::Add)?;
            }

            Operation::Mul { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::multiply(left, right).ok_or(BinaryOperatorError::Multiply)?;
            }

            Operation::Div { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::float_divide(left, right).ok_or(BinaryOperatorError::FloatDivide)?;
            }

            Operation::IDiv { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::floor_divide(left, right).ok_or(BinaryOperatorError::FloorDivide)?;
            }

            Operation::Mod { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::modulo(left, right).ok_or(BinaryOperatorError::Modulo)?;
            }

            Operation::Pow { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::exponentiate(left, right).ok_or(BinaryOperatorError::Exponentiate)?;
            }

            Operation::BitAnd { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::bitwise_and(left, right).ok_or(BinaryOperatorError::BitAnd)?;
            }

            Operation::BitOr { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::bitwise_or(left, right).ok_or(BinaryOperatorError::BitOr)?;
            }

            Operation::BitXor { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::bitwise_xor(left, right).ok_or(BinaryOperatorError::BitXor)?;
            }

            Operation::ShiftLeft { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::shift_left(left, right).ok_or(BinaryOperatorError::ShiftLeft)?;
            }

            Operation::ShiftRight { dest, left, right } => {
                let left = get_rc(&registers.stack_frame, &current_prototype.constants, left);
                let right = get_rc(&registers.stack_frame, &current_prototype.constants, right);
                registers.stack_frame[dest.0 as usize] =
                    raw_ops::shift_right(left, right).ok_or(BinaryOperatorError::ShiftRight)?;
            }
        }

        instructions_run += 1;
        if instructions_run >= max_instructions {
            break;
        }
    }

    Ok(instructions_run)
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
