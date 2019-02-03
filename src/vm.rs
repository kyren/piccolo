use gc_arena::{Collect, Gc, MutationContext};

use crate::{
    thread::{ThreadError, ThreadMode},
    Closure, ClosureState, Error, Function, LuaContext, OpCode, RegisterIndex, Sequence, String,
    Table, Thread, TypeError, UpValueDescriptor, Value, VarCount,
};

#[derive(Collect)]
#[collect(empty_drop)]
pub struct ThreadSequence<'gc>(pub Thread<'gc>);

impl<'gc> ThreadSequence<'gc> {
    pub fn call_function(
        mc: MutationContext<'gc, '_>,
        thread: Thread<'gc>,
        function: Function<'gc>,
        args: &[Value<'gc>],
    ) -> Result<ThreadSequence<'gc>, ThreadError> {
        thread.start(mc, function, args)?;
        Ok(ThreadSequence(thread))
    }
}

impl<'gc> Sequence<'gc> for ThreadSequence<'gc> {
    type Item = Vec<Value<'gc>>;
    type Error = Error<'gc>;

    fn step(
        &mut self,
        mc: MutationContext<'gc, '_>,
        lc: LuaContext<'gc>,
    ) -> Option<Result<Self::Item, Self::Error>> {
        match self.0.mode() {
            ThreadMode::Results => self.0.take_results(mc),
            ThreadMode::Callback => {
                self.0.step_callback(mc, lc).unwrap();
                None
            }
            ThreadMode::Lua => match step_vm(mc, self.0, 64) {
                Err(err) => Some(Err(err)),
                Ok(()) => None,
            },
            mode => Some(Err(ThreadError::BadMode {
                expected: None,
                found: mode,
            }
            .into())),
        }
    }
}

pub fn step_vm<'gc>(
    mc: MutationContext<'gc, '_>,
    thread: Thread<'gc>,
    mut instructions: u32,
) -> Result<(), Error<'gc>> {
    'outer: while let Ok(mut lua_frame) = thread.lua_frame(mc) {
        let current_function = lua_frame.closure();
        let mut registers = lua_frame.registers();

        loop {
            let op = current_function.0.proto.opcodes[*registers.pc];
            *registers.pc += 1;

            match op {
                OpCode::Move { dest, source } => {
                    registers.stack_frame[dest.0 as usize] =
                        registers.stack_frame[source.0 as usize];
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
                    get_table(registers.stack_frame[table.0 as usize])?
                        .set(
                            mc,
                            registers.stack_frame[key.0 as usize],
                            registers.stack_frame[value.0 as usize],
                        )
                        .expect("could not set table value");
                }

                OpCode::SetTableRC { table, key, value } => {
                    get_table(registers.stack_frame[table.0 as usize])?
                        .set(
                            mc,
                            registers.stack_frame[key.0 as usize],
                            current_function.0.proto.constants[value.0 as usize].to_value(),
                        )
                        .expect("could not set table value");
                }

                OpCode::SetTableCR { table, key, value } => {
                    get_table(registers.stack_frame[table.0 as usize])?
                        .set(
                            mc,
                            current_function.0.proto.constants[key.0 as usize].to_value(),
                            registers.stack_frame[value.0 as usize],
                        )
                        .expect("could not set table value");
                }

                OpCode::SetTableCC { table, key, value } => {
                    get_table(registers.stack_frame[table.0 as usize])?
                        .set(
                            mc,
                            current_function.0.proto.constants[key.0 as usize].to_value(),
                            current_function.0.proto.constants[value.0 as usize].to_value(),
                        )
                        .expect("could not set table value");
                }

                OpCode::GetUpTableR { dest, table, key } => {
                    registers.stack_frame[dest.0 as usize] = get_table(
                        registers.get_upvalue(current_function.0.upvalues[table.0 as usize]),
                    )?
                    .get(registers.stack_frame[key.0 as usize]);
                }

                OpCode::GetUpTableC { dest, table, key } => {
                    registers.stack_frame[dest.0 as usize] = get_table(
                        registers.get_upvalue(current_function.0.upvalues[table.0 as usize]),
                    )?
                    .get(current_function.0.proto.constants[key.0 as usize].to_value())
                }

                OpCode::SetUpTableRR { table, key, value } => {
                    get_table(
                        registers.get_upvalue(current_function.0.upvalues[table.0 as usize]),
                    )?
                    .set(
                        mc,
                        registers.stack_frame[key.0 as usize],
                        registers.stack_frame[value.0 as usize],
                    )
                    .expect("could not set table value");
                }

                OpCode::SetUpTableRC { table, key, value } => {
                    get_table(
                        registers.get_upvalue(current_function.0.upvalues[table.0 as usize]),
                    )?
                    .set(
                        mc,
                        registers.stack_frame[key.0 as usize],
                        current_function.0.proto.constants[value.0 as usize].to_value(),
                    )
                    .expect("could not set table value");
                }

                OpCode::SetUpTableCR { table, key, value } => {
                    get_table(
                        registers.get_upvalue(current_function.0.upvalues[table.0 as usize]),
                    )?
                    .set(
                        mc,
                        current_function.0.proto.constants[key.0 as usize].to_value(),
                        registers.stack_frame[value.0 as usize],
                    )
                    .expect("could not set table value");
                }

                OpCode::SetUpTableCC { table, key, value } => {
                    get_table(
                        registers.get_upvalue(current_function.0.upvalues[table.0 as usize]),
                    )?
                    .set(
                        mc,
                        current_function.0.proto.constants[key.0 as usize].to_value(),
                        current_function.0.proto.constants[value.0 as usize].to_value(),
                    )
                    .expect("could not set table value");
                }

                OpCode::Call {
                    func,
                    args,
                    returns,
                } => {
                    lua_frame.call_function(func, args, returns)?;
                    continue 'outer;
                }

                OpCode::TailCall { func, args } => {
                    lua_frame.tail_call_function(func, args)?;
                    continue 'outer;
                }

                OpCode::Return { start, count } => {
                    lua_frame.return_upper(start, count)?;
                    continue 'outer;
                }

                OpCode::VarArgs { dest, count } => {
                    lua_frame.varargs(dest, count)?;
                    continue 'outer;
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
                        .expect("non numeric for loop parameters");
                    *registers.pc = add_offset(*registers.pc, jump);
                }

                OpCode::NumericForLoop { base, jump } => {
                    const ERR_MSG: &str = "non numeric for loop parameter";

                    registers.stack_frame[base.0 as usize] = registers.stack_frame[base.0 as usize]
                        .add(registers.stack_frame[base.0 as usize + 2])
                        .expect(ERR_MSG);
                    let past_end = if registers.stack_frame[base.0 as usize + 2]
                        .less_than(Value::Integer(0))
                        .expect(ERR_MSG)
                    {
                        registers.stack_frame[base.0 as usize]
                            .less_than(registers.stack_frame[base.0 as usize + 1])
                            .expect(ERR_MSG)
                    } else {
                        registers.stack_frame[base.0 as usize + 1]
                            .less_than(registers.stack_frame[base.0 as usize])
                            .expect(ERR_MSG)
                    };
                    if !past_end {
                        *registers.pc = add_offset(*registers.pc, jump);
                        registers.stack_frame[base.0 as usize + 3] =
                            registers.stack_frame[base.0 as usize];
                    }
                }

                OpCode::GenericForCall { base, var_count } => {
                    lua_frame.call_function_non_destructive(
                        base,
                        2,
                        VarCount::constant(var_count),
                    )?;
                    continue 'outer;
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
                    registers.stack_frame[dest.0 as usize] = Value::Integer(
                        get_table(registers.stack_frame[source.0 as usize])?.length(),
                    );
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
                        left.add(right).expect("could not apply binary operator");
                }

                OpCode::AddRC { dest, left, right } => {
                    let left = registers.stack_frame[left.0 as usize];
                    let right = current_function.0.proto.constants[right.0 as usize].to_value();
                    registers.stack_frame[dest.0 as usize] =
                        left.add(right).expect("could not apply binary operator");
                }

                OpCode::AddCR { dest, left, right } => {
                    let left = current_function.0.proto.constants[left.0 as usize].to_value();
                    let right = registers.stack_frame[right.0 as usize];
                    registers.stack_frame[dest.0 as usize] =
                        left.add(right).expect("could not apply binary operator");
                }

                OpCode::AddCC { dest, left, right } => {
                    let left = current_function.0.proto.constants[left.0 as usize].to_value();
                    let right = current_function.0.proto.constants[right.0 as usize].to_value();
                    registers.stack_frame[dest.0 as usize] =
                        left.add(right).expect("could not apply binary operator");
                }

                OpCode::SubRR { dest, left, right } => {
                    let left = registers.stack_frame[left.0 as usize];
                    let right = registers.stack_frame[right.0 as usize];
                    registers.stack_frame[dest.0 as usize] = left
                        .subtract(right)
                        .expect("could not apply binary operator");
                }

                OpCode::SubRC { dest, left, right } => {
                    let left = registers.stack_frame[left.0 as usize];
                    let right = current_function.0.proto.constants[right.0 as usize].to_value();
                    registers.stack_frame[dest.0 as usize] = left
                        .subtract(right)
                        .expect("could not apply binary operator");
                }

                OpCode::SubCR { dest, left, right } => {
                    let left = current_function.0.proto.constants[left.0 as usize].to_value();
                    let right = registers.stack_frame[right.0 as usize];
                    registers.stack_frame[dest.0 as usize] = left
                        .subtract(right)
                        .expect("could not apply binary operator");
                }

                OpCode::SubCC { dest, left, right } => {
                    let left = current_function.0.proto.constants[left.0 as usize].to_value();
                    let right = current_function.0.proto.constants[right.0 as usize].to_value();
                    registers.stack_frame[dest.0 as usize] = left
                        .subtract(right)
                        .expect("could not apply binary operator");
                }

                OpCode::MulRR { dest, left, right } => {
                    let left = registers.stack_frame[left.0 as usize];
                    let right = registers.stack_frame[right.0 as usize];
                    registers.stack_frame[dest.0 as usize] = left
                        .multiply(right)
                        .expect("could not apply binary operator");
                }

                OpCode::MulRC { dest, left, right } => {
                    let left = registers.stack_frame[left.0 as usize];
                    let right = current_function.0.proto.constants[right.0 as usize].to_value();
                    registers.stack_frame[dest.0 as usize] = left
                        .multiply(right)
                        .expect("could not apply binary operator");
                }

                OpCode::MulCR { dest, left, right } => {
                    let left = current_function.0.proto.constants[left.0 as usize].to_value();
                    let right = registers.stack_frame[right.0 as usize];
                    registers.stack_frame[dest.0 as usize] = left
                        .multiply(right)
                        .expect("could not apply binary operator");
                }

                OpCode::MulCC { dest, left, right } => {
                    let left = current_function.0.proto.constants[left.0 as usize].to_value();
                    let right = current_function.0.proto.constants[right.0 as usize].to_value();
                    registers.stack_frame[dest.0 as usize] = left
                        .multiply(right)
                        .expect("could not apply binary operator");
                }
            }

            if instructions == 0 {
                break 'outer;
            } else {
                instructions -= 1
            }
        }
    }
    Ok(())
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
