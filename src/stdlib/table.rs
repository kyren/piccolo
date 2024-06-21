use allocator_api2::vec;
use gc_arena::{allocator_api::MetricsAlloc, Collect};

use crate::meta_ops::{self, MetaResult};
use crate::{
    Callback, CallbackReturn, Context, Error, Execution, MetaMethod, Sequence, SequencePoll, Stack,
    Table, Value,
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

                // TODO: limit maximum size
                // Users can relatively easily hang the VM (or OOM) with large ranges:
                // table.unpack({}, 1, 1 << 32)

                // Respect the user provided __index and __len metamethods, if they exist
                let metatable = table.metatable();
                let has_len = end_arg.is_none()
                    && metatable
                        .map(|mt| !mt.get(ctx, MetaMethod::Len).is_nil())
                        .unwrap_or(false);

                // If the default length returns the first border, as it currently does,
                // a custom index metamethod is only observable if and end argument is
                // provided or the length metamethod is overridden.
                let has_index = (end_arg.is_some() || has_len)
                    && metatable
                        .map(|mt| !mt.get(ctx, MetaMethod::Index).is_nil())
                        .unwrap_or(false);

                if has_index || has_len {
                    // This will have some messy edge-cases:
                    // - if the index metamethod redefines the metatable or index metamethod for the struct, should it be respected?
                    // - if the index metamethod redefines the length of the table, should it be respected?
                    let seq = if has_len {
                        Unpack::FindLength { cur: start, table }
                    } else {
                        let mut inner_stack = vec::Vec::new_in(MetricsAlloc::new(&ctx));
                        inner_stack.reserve_exact((end - start + 1) as usize);
                        Unpack::MainLoop {
                            first: true,
                            cur: start,
                            end,
                            table,
                            inner_stack,
                        }
                    };
                    return Ok(CallbackReturn::Sequence(crate::BoxSequence::new(&ctx, seq)));
                }

                if start <= end {
                    stack.resize((end - start + 1) as usize);
                    for i in start..=end {
                        stack[(i - start) as usize] = table.get_value(i.into());
                    }
                }

                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.set_global("table", table).unwrap();
}

#[derive(Collect)]
#[collect(no_drop)]
enum Unpack<'gc> {
    FindLength {
        cur: i64,
        table: Table<'gc>,
    },
    LengthFound {
        cur: i64,
        table: Table<'gc>,
    },
    MainLoop {
        first: bool,
        cur: i64,
        end: i64,
        table: Table<'gc>,
        inner_stack: vec::Vec<Value<'gc>, MetricsAlloc<'gc>>,
    },
}
impl<'gc> Sequence<'gc> for Unpack<'gc> {
    fn poll(
        &mut self,
        ctx: Context<'gc>,
        _exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        loop {
            match *self {
                Unpack::FindLength { cur, table } => {
                    *self = Unpack::LengthFound { cur, table };
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
                Unpack::LengthFound { cur, table } => {
                    let end: i64 = stack.consume(ctx)?;
                    let length = (end - cur + 1).max(0) as usize;
                    let mut inner_stack = vec::Vec::new_in(MetricsAlloc::new(&ctx));
                    inner_stack.reserve_exact(length);
                    *self = Unpack::MainLoop {
                        first: true,
                        cur,
                        end,
                        table,
                        inner_stack,
                    };
                }
                Unpack::MainLoop {
                    ref mut first,
                    ref mut cur,
                    end,
                    table,
                    ref mut inner_stack,
                } => {
                    if !*first {
                        let value: Value = stack.consume(ctx)?;
                        inner_stack.push(value);
                    } else {
                        *first = false;
                    }

                    while *cur <= end {
                        let index = *cur;
                        *cur += 1;
                        match meta_ops::index(ctx, table.into(), index.into())? {
                            MetaResult::Value(v) => inner_stack.push(v),
                            MetaResult::Call(call) => {
                                stack.extend(call.args);
                                return Ok(SequencePoll::Call {
                                    function: call.function,
                                    bottom: 0,
                                });
                            }
                        }
                    }
                    stack.extend(inner_stack.iter().copied());
                    return Ok(SequencePoll::Return);
                }
            }
        }
    }
}
