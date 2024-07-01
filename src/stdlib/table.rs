use std::pin::Pin;

use gc_arena::Collect;

use crate::async_sequence::{
    AsyncSequence, LocalError, LocalFunction, LocalTable, LocalValue, Locals,
};
use crate::fuel::count_fuel;
use crate::meta_ops::{self, MetaResult};
use crate::table::RawArrayOpResult;
use crate::{
    async_sequence, BoxSequence, Callback, CallbackReturn, Context, Error, Execution, IntoValue,
    MetaMethod, Sequence, SequencePoll, SequenceReturn, Stack, Table, Value,
};

pub fn load_table<'gc>(ctx: Context<'gc>) {
    let table = Table::new(&ctx);

    table.set_field(
        ctx,
        "pack",
        Callback::from_fn(&ctx, |ctx, _, stack| {
            Ok(CallbackReturn::Sequence(BoxSequence::new(
                &ctx,
                Pack::SetLength {
                    table: Table::new(&ctx).into(),
                    length: stack.len(),
                },
            )))
        }),
    );

    table.set_field(
        ctx,
        "unpack",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (table, start_arg, end_arg): (Value<'gc>, Option<i64>, Option<i64>) =
                stack.consume(ctx)?;

            let start = start_arg.unwrap_or(1);
            let seq = if let Some(end) = end_arg {
                if start > end {
                    return Ok(CallbackReturn::Return);
                }

                let length = try_compute_length(start, end)
                    .ok_or_else(|| "Too many values to unpack".into_value(ctx))?;
                Unpack::MainLoop {
                    start,
                    table,
                    length,
                    index: 0,
                    batch_end: 0,
                    callback_return: false,
                }
            } else {
                Unpack::FindLength { start, table }
            };

            Ok(CallbackReturn::Sequence(BoxSequence::new(&ctx, seq)))
        }),
    );

    fn prep_metaop_call<'seq, 'gc, const N: usize>(
        ctx: Context<'gc>,
        mut stack: Stack<'gc, '_>,
        locals: Locals<'seq, 'gc>,
        res: MetaResult<'gc, N>,
    ) -> Option<LocalFunction<'seq>> {
        match res {
            MetaResult::Value(v) => {
                stack.push_back(v);
                None
            }
            MetaResult::Call(call) => {
                stack.extend(call.args);
                Some(locals.stash(&ctx, call.function))
            }
        }
    }

    async fn index_helper<'seq>(
        seq: &mut AsyncSequence<'seq>,
        table: &LocalTable<'seq>,
        key: i64,
        bottom: usize,
    ) -> Result<(), LocalError<'seq>> {
        let call = seq.try_enter(|ctx, locals, _, stack| {
            let table = locals.fetch(table);
            let call = meta_ops::index(ctx, Value::Table(table), Value::Integer(key))?;
            Ok(prep_metaop_call(ctx, stack, locals, call))
        })?;
        if let Some(call) = call {
            seq.call(&call, bottom).await?;
            seq.enter(|_, _, _, mut stack| {
                stack.resize(bottom + 1); // Truncate stack
            });
        }
        Ok(())
    }

    async fn index_set_helper<'seq>(
        seq: &mut AsyncSequence<'seq>,
        table: &LocalTable<'seq>,
        key: i64,
        value: LocalValue<'seq>,
        bottom: usize,
    ) -> Result<(), LocalError<'seq>> {
        let call = seq.try_enter(|ctx, locals, _, mut stack| {
            let table = locals.fetch(table);
            let value = locals.fetch(&value);
            let call = meta_ops::new_index(ctx, Value::Table(table), Value::Integer(key), value)?;
            match call {
                None => Ok(None),
                Some(call) => {
                    stack.extend(call.args);
                    Ok(Some(locals.stash(&ctx, call.function)))
                }
            }
        })?;
        if let Some(call) = call {
            seq.call(&call, bottom).await?;
            seq.enter(|_, _, _, mut stack| {
                stack.resize(bottom); // Truncate stack
            });
        }
        Ok(())
    }

    const FUEL_PER_SHIFTED_ITEM: i32 = 1;

    // Minor difference from PRLua: When the table is empty, table.remove(t, #t),
    // will return nil, even if the table has an element at index 0.
    table.set_field(
        ctx,
        "remove",
        Callback::from_fn(&ctx, |ctx, mut exec, mut stack| {
            let (table, index): (Table, Option<i64>) = stack.consume(ctx)?;
            let length;

            let metatable = table.metatable();
            let use_fallback = metatable
                .map(|mt| {
                    !mt.get(ctx, MetaMethod::Len).is_nil()
                        || !mt.get(ctx, MetaMethod::Index).is_nil()
                        || !mt.get(ctx, MetaMethod::NewIndex).is_nil()
                })
                .unwrap_or(false);

            if !use_fallback {
                // Try the fast path
                let mut inner = table.into_inner().borrow_mut(&ctx);
                match inner.raw_table.array_remove_shift(index) {
                    (RawArrayOpResult::Success(val), len) => {
                        // Consume fuel after the operation to avoid computing length twice
                        let shifted_items =
                            len.saturating_sub(index.unwrap_or(len as i64).try_into().unwrap_or(0));
                        exec.fuel()
                            .consume(count_fuel(FUEL_PER_SHIFTED_ITEM, shifted_items));

                        stack.push_back(val);
                        return Ok(CallbackReturn::Return);
                    }
                    (RawArrayOpResult::Possible, len) => {
                        length = Some(len);
                    }
                    (RawArrayOpResult::Failed, _) => {
                        return Err("Invalid index passed to table.remove"
                            .into_value(ctx)
                            .into());
                    }
                }
            } else {
                length = None;
            }

            // Fast path failed, fall back to direct indexing
            let s = async_sequence(&ctx, |locals, builder| {
                let table = locals.stash(&ctx, table);
                builder.build(|mut seq| async move {
                    let length = if let Some(len) = length {
                        len as i64
                    } else {
                        let call = seq.try_enter(|ctx, locals, _, stack| {
                            let table = locals.fetch(&table);
                            let call = meta_ops::len(ctx, Value::Table(table))?;
                            Ok(prep_metaop_call(ctx, stack, locals, call))
                        })?;
                        if let Some(call) = call {
                            seq.call(&call, 0).await?;
                        }
                        let len = seq.try_enter(|ctx, _, _, mut stack| {
                            stack.consume::<i64>(ctx).map_err(|e| e.into())
                        })?;
                        len
                    };

                    let index = index.unwrap_or(length);

                    if index == 0 && length == 0 || index == length + 1 {
                        seq.enter(|_, _, _, mut stack| {
                            stack.push_back(Value::Nil);
                        });
                        Ok(SequenceReturn::Return)
                    } else if index >= 1 && index <= length {
                        // Get the value of the element to remove; we'll keep it on the stack.
                        index_helper(&mut seq, &table, index, 0).await?;

                        // Could make this more efficient by inlining the stack manipulation;
                        // only pushing the table once.
                        for i in index..length {
                            // Push table[i + 1] onto stack
                            index_helper(&mut seq, &table, i + 1, 1).await?;
                            let value = seq.enter(|ctx, locals, _, mut stack| {
                                locals.stash(&ctx, stack.pop_back().unwrap_or_default())
                            });
                            // table[i] = table[i + 1]
                            index_set_helper(&mut seq, &table, i, value, 1).await?;

                            seq.enter(|_, _, mut exec, _| {
                                exec.fuel().consume(FUEL_PER_SHIFTED_ITEM as i32);
                            });
                        }

                        let nil = seq.enter(|ctx, locals, _, _| locals.stash(&ctx, Value::Nil));
                        // table[length] = nil
                        index_set_helper(&mut seq, &table, length, nil, 1).await?;

                        // The last value is still on the stack
                        Ok(SequenceReturn::Return)
                    } else {
                        seq.try_enter(|ctx, _, _, _| {
                            Err("Invalid index passed to table.remove"
                                .into_value(ctx)
                                .into())
                        })
                    }
                })
            });
            Ok(CallbackReturn::Sequence(s))
        }),
    );

    table.set_field(
        ctx,
        "insert",
        Callback::from_fn(&ctx, |ctx, mut exec, mut stack| {
            let table: Table;
            let index: Option<i64>;
            let value: Value;
            match stack.len() {
                0..=1 => return Err("Missing arguments to insert".into_value(ctx).into()),
                2 => {
                    (table, value) = stack.consume(ctx)?;
                    index = None;
                }
                _ => (table, index, value) = stack.consume(ctx)?,
            }
            let length;

            let metatable = table.metatable();
            let use_fallback = metatable
                .map(|mt| {
                    !mt.get(ctx, MetaMethod::Len).is_nil()
                        || !mt.get(ctx, MetaMethod::Index).is_nil()
                        || !mt.get(ctx, MetaMethod::NewIndex).is_nil()
                })
                .unwrap_or(false);

            if !use_fallback {
                // Try the fast path
                match table
                    .into_inner()
                    .borrow_mut(&ctx)
                    .raw_table
                    .array_insert_shift(index, value)
                {
                    (RawArrayOpResult::Success(_), len) => {
                        // Consume fuel after the operation to avoid computing length twice
                        let shifted_items = len.saturating_sub(
                            index
                                .unwrap_or(len.saturating_add(1) as i64)
                                .saturating_sub(1)
                                .try_into()
                                .unwrap_or(0),
                        );
                        exec.fuel()
                            .consume(count_fuel(FUEL_PER_SHIFTED_ITEM, shifted_items));

                        return Ok(CallbackReturn::Return);
                    }
                    (RawArrayOpResult::Possible, len) => {
                        length = Some(len);
                    }
                    (RawArrayOpResult::Failed, _) => {
                        return Err("Invalid index passed to table.insert"
                            .into_value(ctx)
                            .into());
                    }
                }
            } else {
                length = None;
            }

            // Fast path failed, fall back to direct indexing
            let s = async_sequence(&ctx, |locals, builder| {
                let table = locals.stash(&ctx, table);
                let value = locals.stash(&ctx, value);
                builder.build(|mut seq| async move {
                    let length = if let Some(len) = length {
                        len as i64
                    } else {
                        let call = seq.try_enter(|ctx, locals, _, stack| {
                            let table = locals.fetch(&table);
                            let call = meta_ops::len(ctx, Value::Table(table))?;
                            Ok(prep_metaop_call(ctx, stack, locals, call))
                        })?;
                        if let Some(call) = call {
                            seq.call(&call, 0).await?;
                        }
                        let len = seq.try_enter(|ctx, _, _, mut stack| {
                            stack.consume::<i64>(ctx).map_err(|e| e.into())
                        })?;
                        len
                    };

                    let index = index.unwrap_or(length + 1);

                    if index >= 1 && index <= length + 1 {
                        // Could make this more efficient by inlining the stack manipulation;
                        // only pushing the table once.
                        for i in (index + 1..=length + 1).rev() {
                            // Push table[i - 1] onto the stack
                            index_helper(&mut seq, &table, i - 1, 0).await?;
                            let value = seq.enter(|ctx, locals, _, mut stack| {
                                locals.stash(&ctx, stack.pop_back().unwrap_or_default())
                            });
                            // table[i] = table[i - 1]
                            index_set_helper(&mut seq, &table, i, value, 0).await?;

                            seq.enter(|_, _, mut exec, _| {
                                exec.fuel().consume(FUEL_PER_SHIFTED_ITEM as i32);
                            });
                        }

                        // table[index] = value
                        index_set_helper(&mut seq, &table, index, value, 1).await?;

                        Ok(SequenceReturn::Return)
                    } else {
                        seq.try_enter(|ctx, _, _, _| {
                            Err("Invalid index passed to table.insert"
                                .into_value(ctx)
                                .into())
                        })
                    }
                })
            });
            Ok(CallbackReturn::Sequence(s))
        }),
    );

    ctx.set_global("table", table);
}

const PACK_ELEMS_PER_FUEL: usize = 8;
const PACK_MIN_BATCH_SIZE: usize = 4096;

#[derive(Collect)]
#[collect(no_drop)]
enum Pack<'gc> {
    SetLength {
        table: Value<'gc>,
        length: usize,
    },
    MainLoop {
        table: Value<'gc>,
        length: usize,
        index: usize,
        batch_end: usize,
    },
}

impl<'gc> Sequence<'gc> for Pack<'gc> {
    fn poll(
        mut self: Pin<&mut Self>,
        ctx: Context<'gc>,
        mut exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        if let Pack::SetLength { table, length } = *self {
            *self = Pack::MainLoop {
                table,
                length,
                index: 0,
                batch_end: 0,
            };

            if let Some(call) =
                meta_ops::new_index(ctx, table, "n".into_value(ctx), (length as i64).into())?
            {
                stack.extend(call.args);
                return Ok(SequencePoll::Call {
                    function: call.function,
                    bottom: length,
                });
            }
        }

        let Pack::MainLoop {
            table,
            length,
            ref mut index,
            ref mut batch_end,
        } = *self
        else {
            unreachable!();
        };

        assert!(stack.len() >= length);
        // Clear out return values from any called meta_ops::new_index methods.
        stack.resize(length);

        let fuel = exec.fuel();
        while *index < length {
            if index == batch_end {
                let remaining_fuel = fuel.remaining().max(0) as usize;
                let available_elems = remaining_fuel.saturating_mul(PACK_ELEMS_PER_FUEL);

                let remaining_elems = length - *index;
                let batch_size = available_elems
                    .max(PACK_MIN_BATCH_SIZE)
                    .min(remaining_elems);
                stack.reserve(batch_size);
                *batch_end = *index + batch_size;

                fuel.consume((batch_size / PACK_ELEMS_PER_FUEL) as i32);
            }

            while *index < *batch_end {
                if let Some(call) =
                    meta_ops::new_index(ctx, table, (*index as i64 + 1).into(), stack[*index])?
                {
                    stack.extend(call.args);
                    return Ok(SequencePoll::Call {
                        function: call.function,
                        bottom: length,
                    });
                }
                *index += 1;
            }

            if !fuel.should_continue() {
                break;
            }
        }

        if *index < length {
            Ok(SequencePoll::Pending)
        } else {
            stack.replace(ctx, table);
            Ok(SequencePoll::Return)
        }
    }
}

// Try to compute the length of a range for unpack, accounting for potential overflow.
fn try_compute_length(start: i64, end: i64) -> Option<usize> {
    assert!(start <= end);
    end.checked_sub(start)
        .and_then(|l| l.checked_add(1))
        .and_then(|l| usize::try_from(l).ok())
}

const UNPACK_ELEMS_PER_FUEL: usize = 8;
const UNPACK_MIN_BATCH_SIZE: usize = 4096;

#[derive(Collect)]
#[collect(no_drop)]
enum Unpack<'gc> {
    FindLength {
        start: i64,
        table: Value<'gc>,
    },
    LengthFound {
        start: i64,
        table: Value<'gc>,
    },
    MainLoop {
        start: i64,
        table: Value<'gc>,
        length: usize,
        index: usize,
        batch_end: usize,
        callback_return: bool,
    },
}

impl<'gc> Sequence<'gc> for Unpack<'gc> {
    fn poll(
        mut self: Pin<&mut Self>,
        ctx: Context<'gc>,
        mut exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        if let Unpack::FindLength { start, table } = *self {
            *self = Unpack::LengthFound { start, table };
            // We match PUC-Rio Lua here by finding the length at the *start* of the loop only. If
            // the __index metamethod or some other triggered Lua code changes the length of the
            // table, this will not be considered.
            match meta_ops::len(ctx, table)? {
                MetaResult::Value(v) => stack.push_back(v),
                MetaResult::Call(call) => {
                    stack.extend(call.args);
                    return Ok(SequencePoll::Call {
                        function: call.function,
                        bottom: 0,
                    });
                }
            }
        }

        if let Unpack::LengthFound { start, table } = *self {
            let end: i64 = stack.consume(ctx)?;
            if start > end {
                return Ok(SequencePoll::Return);
            }
            let length = try_compute_length(start, end)
                .ok_or_else(|| "Too many values to unpack".into_value(ctx))?;
            *self = Unpack::MainLoop {
                start,
                table,
                length,
                index: 0,
                batch_end: 0,
                callback_return: false,
            };
        }

        let Unpack::MainLoop {
            start,
            table,
            length,
            ref mut index,
            ref mut batch_end,
            ref mut callback_return,
        } = *self
        else {
            unreachable!();
        };

        if *callback_return {
            *callback_return = false;
            // The return value for __index was pushed onto the top of the stack, precisely where
            // it's needed.
            *index += 1;
            // truncate stack to the current height
            stack.resize(*index);
        }
        debug_assert_eq!(stack.len(), *index, "index must match stack height");

        let fuel = exec.fuel();
        while *index < length {
            let batch_remaining = *batch_end - *index;
            if batch_remaining == 0 {
                let remaining_fuel = fuel.remaining().max(0) as usize;
                let available_elems = remaining_fuel.saturating_mul(UNPACK_ELEMS_PER_FUEL);

                let remaining_elems = length - *index;
                let batch_size = available_elems
                    .max(UNPACK_MIN_BATCH_SIZE)
                    .min(remaining_elems);
                stack.reserve(batch_size);
                *batch_end = *index + batch_size;

                fuel.consume((batch_size / UNPACK_ELEMS_PER_FUEL) as i32);
            }

            while *index < *batch_end {
                match meta_ops::index(ctx, table, (start + *index as i64).into())? {
                    MetaResult::Value(v) => {
                        stack.push_back(v);
                    }
                    MetaResult::Call(call) => {
                        *callback_return = true;
                        stack.extend(call.args);
                        return Ok(SequencePoll::Call {
                            function: call.function,
                            bottom: *index,
                        });
                    }
                }
                *index += 1;
            }

            if *index < length && !fuel.should_continue() {
                return Ok(SequencePoll::Pending);
            }
        }

        debug_assert_eq!(*index, length, "all elements must have been accessed");
        debug_assert_eq!(length, stack.len(), "all elements must be on the stack");
        // Return values are already in-place on the stack
        Ok(SequencePoll::Return)
    }
}
