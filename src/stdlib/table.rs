use std::pin::Pin;

use gc_arena::Collect;

use crate::meta_ops::{self, MetaResult};
use crate::{
    BoxSequence, Callback, CallbackReturn, Context, Error, Execution, IntoValue, Sequence,
    SequencePoll, Stack, Table, Value,
};

pub fn load_table<'gc>(ctx: Context<'gc>) {
    let table = Table::new(&ctx);

    table
        .set(
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
        )
        .unwrap();

    table
        .set(
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
        )
        .unwrap();

    ctx.set_global("table", table).unwrap();
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
