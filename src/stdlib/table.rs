use gc_arena::Collect;

use crate::meta_ops::{self, MetaResult};
use crate::{
    BoxSequence, Callback, CallbackReturn, Context, Error, Execution, IntoValue, MetaMethod,
    Sequence, SequencePoll, Stack, Table,
};

pub fn load_table<'gc>(ctx: Context<'gc>) {
    let table = Table::new(&ctx);

    table
        .set(
            ctx,
            "pack",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let t = Table::new(&ctx);
                for i in 0..stack.len() {
                    t.set(ctx, i as i64 + 1, stack[i]).unwrap();
                }
                t.set(ctx, "n", stack.len() as i64).unwrap();
                stack.replace(ctx, t);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    table
        .set(
            ctx,
            "unpack",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let (table, start_arg, end_arg): (Table<'gc>, Option<i64>, Option<i64>) =
                    stack.consume(ctx)?;

                let start = start_arg.unwrap_or(1);
                let end = end_arg.unwrap_or_else(|| table.length());

                let metatable = table.metatable();
                let has_len = end_arg.is_none()
                    && metatable
                        .map(|mt| !mt.get(ctx, MetaMethod::Len).is_nil())
                        .unwrap_or(false);

                let seq = if has_len {
                    Unpack::FindLength { start, table }
                } else {
                    if start > end {
                        return Ok(CallbackReturn::Return);
                    }

                    let length = try_compute_length(start, end)
                        .ok_or_else(|| "Too many values to unpack".into_value(ctx))?;
                    Unpack::MainLoop {
                        callback_return: false,
                        start,
                        length,
                        index: 0,
                        reserved: 0,
                        table,
                    }
                };
                Ok(CallbackReturn::Sequence(BoxSequence::new(&ctx, seq)))
            }),
        )
        .unwrap();

    ctx.set_global("table", table).unwrap();
}

// PUC-Rio Lua's maximum argument count, on my machine, is about 1000000; this is slightly larger.
const MAXIMUM_UNPACK_ARGS: usize = 1 << 20;

// Try to compute the length of a range for unpack, accounting for potential overflow and limiting
// the length to MAXIMUM_UNPACK_ARGS
fn try_compute_length(start: i64, end: i64) -> Option<usize> {
    assert!(start <= end);
    end.checked_sub(start)
        .and_then(|l| l.checked_add(1))
        .and_then(|l| usize::try_from(l).ok())
        .filter(|&l| matches!(l, 0..=MAXIMUM_UNPACK_ARGS))
}

const UNPACK_ELEMS_PER_FUEL: usize = 8;
const UNPACK_MIN_BATCH_SIZE: usize = 4096;

#[derive(Collect)]
#[collect(no_drop)]
enum Unpack<'gc> {
    FindLength {
        start: i64,
        table: Table<'gc>,
    },
    LengthFound {
        start: i64,
        table: Table<'gc>,
    },
    MainLoop {
        callback_return: bool,
        start: i64,
        length: usize,
        index: usize,
        reserved: usize,
        table: Table<'gc>,
    },
}

impl<'gc> Sequence<'gc> for Unpack<'gc> {
    fn poll(
        &mut self,
        ctx: Context<'gc>,
        mut exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        if let Unpack::FindLength { start, table } = *self {
            *self = Unpack::LengthFound { start, table };
            // We match PUC-Rio Lua here by finding the length at the *start* of the loop
            // only. If the __index metamethod or some other triggered Lua code changes the
            // length of the table, this will not be considered.
            match meta_ops::len(ctx, table.into())? {
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
                callback_return: false,
                start,
                length,
                index: 0,
                reserved: 0,
                table,
            };
        }

        let Unpack::MainLoop {
            ref mut callback_return,
            start,
            length,
            ref mut index,
            ref mut reserved,
            table,
        } = *self
        else {
            unreachable!();
        };

        if *callback_return {
            *callback_return = false;
            // The return value for __index was pushed onto the top of the stack,
            // precisely where it's needed.
            *index += 1;
            // truncate stack to the current height
            stack.resize(*index);
        }
        debug_assert_eq!(stack.len(), *index, "index must match stack height");

        let fuel = exec.fuel();
        while *index < length {
            let batch_remaining = *reserved - *index;
            if batch_remaining == 0 {
                let remaining_fuel = fuel.remaining().clamp(0, i32::MAX) as usize;
                let available_elems = remaining_fuel / UNPACK_ELEMS_PER_FUEL;

                let remaining_elems = length - *index;
                let batch_size = available_elems
                    .max(UNPACK_MIN_BATCH_SIZE)
                    .min(remaining_elems);
                stack.reserve(batch_size);
                *reserved = *index + batch_size;

                fuel.consume((batch_size / UNPACK_ELEMS_PER_FUEL) as i32);
            }

            for i in *index..*reserved {
                // It would be nice to be able to cache the index metamethod here, but
                // that would require tracking infrastructure elsewhere. (In theory
                // this *could* cache it for the case where __index is a table and never
                // calls back into Lua code, but it's not worth splitting the logic.)
                match meta_ops::index(ctx, table.into(), (start + i as i64).into())? {
                    MetaResult::Value(v) => {
                        stack.push_back(v);
                    }
                    MetaResult::Call(call) => {
                        *callback_return = true;
                        *index = i;
                        stack.extend(call.args);
                        return Ok(SequencePoll::Call {
                            function: call.function,
                            bottom: *index,
                        });
                    }
                }
            }
            *index = *reserved;

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
