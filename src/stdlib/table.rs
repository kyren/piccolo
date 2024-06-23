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

                // Respect the user provided __index and __len metamethods, if they exist
                let metatable = table.metatable();
                let has_len = end_arg.is_none()
                    && metatable
                        .map(|mt| !mt.get(ctx, MetaMethod::Len).is_nil())
                        .unwrap_or(false);

                let has_index = metatable
                    .map(|mt| !mt.get(ctx, MetaMethod::Index).is_nil())
                    .unwrap_or(false);

                if has_index || has_len {
                    // This will have some messy edge-cases:
                    // - if the index metamethod redefines the metatable or index metamethod for the struct, should it be respected?
                    // - if the index metamethod redefines the length of the table, should it be respected?
                    let seq = if has_len {
                        UnpackMeta::FindLength { start, table }
                    } else {
                        if start > end {
                            return Ok(CallbackReturn::Return);
                        }
                        let length = try_compute_length(start, end)
                            .ok_or_else(|| "Too many values to unpack".into_value(ctx))?;
                        UnpackMeta::MainLoop {
                            callback_return: false,
                            start,
                            length,
                            index: 0,
                            reserved: 0,
                            table,
                        }
                    };
                    return Ok(CallbackReturn::Sequence(BoxSequence::new(&ctx, seq)));
                }

                if start <= end {
                    let length = try_compute_length(start, end)
                        .ok_or_else(|| "Too many values to unpack".into_value(ctx))?;
                    let seq = Unpack {
                        start,
                        length,
                        index: 0,
                        table,
                    };
                    return Ok(CallbackReturn::Sequence(BoxSequence::new(&ctx, seq)));
                }

                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.set_global("table", table).unwrap();
}

// PUC-Rio Lua's maximum argument count, on my machine, is about 1000000;
// this is slightly larger.
const MAXIMUM_UNPACK_ARGS: usize = 1 << 20;

// Try to compute the length of a range for unpack, accounting for
// potential overflow and limiting the length to MAXIMUM_UNPACK_ARGS
//
// Without this, users can relatively easily hang the VM (or OOM) with large ranges:
// table.unpack({}, 1, 1 << 32)
fn try_compute_length(start: i64, end: i64) -> Option<usize> {
    end.checked_sub(start)
        .and_then(|l| l.checked_add(1))
        .and_then(|l| usize::try_from(l).ok())
        .filter(|&l| matches!(l, 0..=MAXIMUM_UNPACK_ARGS))
}

const RAW_ELEMS_PER_FUEL: usize = 8;
const RAW_MIN_BATCH_SIZE: usize = 4096;

#[derive(Collect)]
#[collect(no_drop)]
struct Unpack<'gc> {
    start: i64,
    length: usize,
    index: usize,
    table: Table<'gc>,
}

impl<'gc> Sequence<'gc> for Unpack<'gc> {
    fn poll(
        &mut self,
        _ctx: Context<'gc>,
        mut exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        let Self {
            start,
            length,
            ref mut index,
            table,
        } = *self;

        let fuel = exec.fuel();
        while *index < length && fuel.should_continue() {
            let remaining_fuel = fuel.remaining().clamp(0, i32::MAX) as usize;
            let available_elems = remaining_fuel / RAW_ELEMS_PER_FUEL;

            let remaining = length - *index;
            let batch_size = available_elems.max(RAW_MIN_BATCH_SIZE).min(remaining);
            stack.resize(*index + batch_size);
            fuel.consume((batch_size / RAW_ELEMS_PER_FUEL) as i32);

            for i in *index..*index + batch_size {
                stack[i] = table.get_value((start + i as i64).into());
            }
            *index += batch_size;
        }
        if !fuel.should_continue() {
            return Ok(SequencePoll::Pending);
        }
        debug_assert_eq!(*index, length, "all elements must have been accessed");
        debug_assert_eq!(length, stack.len(), "all elements must be on the stack");
        // Return values are already in-place on the stack
        return Ok(SequencePoll::Return);
    }
}

const META_ELEMS_PER_FUEL: usize = 8;
const META_MIN_BATCH_SIZE: usize = 4096;

#[derive(Collect)]
#[collect(no_drop)]
enum UnpackMeta<'gc> {
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
impl<'gc> Sequence<'gc> for UnpackMeta<'gc> {
    fn poll(
        &mut self,
        ctx: Context<'gc>,
        mut exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        loop {
            match *self {
                UnpackMeta::FindLength { start, table } => {
                    *self = UnpackMeta::LengthFound { start, table };
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
                UnpackMeta::LengthFound { start, table } => {
                    let end: i64 = stack.consume(ctx)?;
                    if start > end {
                        return Ok(SequencePoll::Return);
                    }
                    let length = try_compute_length(start, end)
                        .ok_or_else(|| "Too many values to unpack".into_value(ctx))?;
                    *self = UnpackMeta::MainLoop {
                        callback_return: false,
                        start,
                        length,
                        index: 0,
                        reserved: 0,
                        table,
                    };
                }
                UnpackMeta::MainLoop {
                    ref mut callback_return,
                    start,
                    length,
                    ref mut index,
                    ref mut reserved,
                    table,
                } => {
                    if *callback_return {
                        *callback_return = false;
                        // The return value for __index was pushed onto the top
                        // of the stack, precisely where it's needed.
                        *index += 1;
                        // truncate stack to the current height
                        stack.resize(*index);
                    }
                    debug_assert_eq!(stack.len(), *index, "index must match stack height");

                    let fuel = exec.fuel();
                    while *index < length && fuel.should_continue() {
                        let batch_remaining = *reserved - *index;
                        if batch_remaining == 0 {
                            let remaining_fuel = fuel.remaining().clamp(0, i32::MAX) as usize;
                            let available_elems = remaining_fuel / META_ELEMS_PER_FUEL;

                            let remaining_elems = length - *index;
                            let batch_size = available_elems
                                .max(META_MIN_BATCH_SIZE)
                                .min(remaining_elems);
                            stack.reserve(batch_size);
                            *reserved = *index + batch_size;

                            fuel.consume((batch_size / META_ELEMS_PER_FUEL) as i32);
                        }

                        for i in *index..*reserved {
                            // It would be nice to be able to cache the index metamethod here,
                            // but that would require tracking infrastructure elsewhere.
                            // (In theory this *could* cache it for the case where __index is
                            // a table and never calls back into Lua code, but it's not worth
                            // splitting the logic.)
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
                    }
                    if !fuel.should_continue() {
                        return Ok(SequencePoll::Pending);
                    }
                    debug_assert_eq!(*index, length, "all elements must have been accessed");
                    debug_assert_eq!(length, stack.len(), "all elements must be on the stack");
                    // Return values are already in-place on the stack
                    return Ok(SequencePoll::Return);
                }
            }
        }
    }
}
