use gc_arena::Collect;

use crate::{
    meta_ops::{self, MetaResult},
    table::NextValue,
    BoxSequence, Callback, CallbackReturn, Context, Error, Execution, IntoValue, MetaMethod,
    Sequence, SequencePoll, Stack, String, Table, Value, Variadic,
};

pub fn load_base<'gc>(ctx: Context<'gc>) {
    ctx.set_global(
        "tonumber",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (prenumber, maybe_base) = stack.consume::<(Value, Option<u8>)>(ctx)?;
            stack.replace(
                ctx,
                match prenumber {
                    v @ (Value::Integer(_) | Value::Number(_)) => v,
                    Value::String(s) => {
                        let is_neg = s.first().is_some_and(|b| *b == b'-');
                        let bytes = if is_neg {
                            &s.as_bytes()[1..]
                        } else {
                            s.as_bytes()
                        };

                        let sign = if is_neg { -1 } else { 1 };
                        if let Some(base) = maybe_base {
                            if !(2..=36).contains(&base) {
                                Err("base out of range".into_value(ctx))?;
                            }
                            // Since a base [2 - 36 are the only valid values] was specified, this *must*
                            // represent an integer of the base
                            const BASE_ELEMENTS: &[u8; 36] =
                                b"0123456789abcdefghijklmnopqrstuvwxyz";
                            let byte_values = bytes
                                .iter()
                                .map(|b| {
                                    BASE_ELEMENTS
                                        .iter()
                                        .position(|i| b.to_ascii_lowercase() == *i)
                                })
                                .collect::<Vec<_>>();
                            if byte_values
                                .iter()
                                .any(|v| v.is_none() || v.is_some_and(|v| v >= base as usize))
                            {
                                Value::Nil
                            } else {
                                let mut value: i64 = 0;
                                for (idx, elem) in byte_values.into_iter().rev().enumerate() {
                                    let Some(elem) = elem else {
                                        // The earlier byte value check means that this should not be hit
                                        unreachable!();
                                    };
                                    value += (base as i64).pow(idx.try_into()?) * elem as i64;
                                }
                                Value::Integer(value)
                            }
                        } else {
                            // Just try to decimal-numberify it
                            let dot_positions = bytes
                                .iter()
                                .copied()
                                .enumerate()
                                .filter_map(|(idx, b)| if b == b'.' { Some(idx) } else { None })
                                .collect::<Vec<_>>();
                            let e_positions = bytes
                                .iter()
                                .copied()
                                .enumerate()
                                .filter_map(|(idx, b)| if b == b'e' { Some(idx) } else { None })
                                .collect::<Vec<_>>();

                            if dot_positions.len() > 1 && e_positions.len() > 1 {
                                Value::Nil
                            } else {
                                let dot_position = dot_positions.first();
                                let e_position = e_positions.first();
                                if bytes
                                    .iter()
                                    .enumerate()
                                    .filter(|(idx, _)| {
                                        !dot_position.is_some_and(|dp| dp == idx)
                                            && !e_position.is_some_and(|ep| ep == idx)
                                    })
                                    .any(|(_, b)| !b.is_ascii_digit())
                                {
                                    Value::Nil
                                } else {
                                    fn parse_whole_value(data: &[u8]) -> u64 {
                                        let mut whole_value = 0;
                                        for b in data {
                                            whole_value = (whole_value * 10) + (b - b'0') as u64;
                                        }
                                        whole_value
                                    }
                                    fn parse_fract_value(data: &[u8]) -> f64 {
                                        let mut fract_value = 0.0f64;
                                        for (idx, b) in data.iter().enumerate() {
                                            fract_value +=
                                                (b - b'0') as f64 / (10 * (idx + 1)) as f64;
                                        }
                                        fract_value
                                    }
                                    match (dot_position, e_position) {
                                        (Some(dp), Some(ep)) => {
                                            if ep < dp {
                                                Value::Nil
                                            } else {
                                                let whole_value = parse_whole_value(&bytes[..*dp]);
                                                let fract_value = parse_fract_value(
                                                    &bytes[(*dp + 1).min(bytes.len() - 1)..*ep],
                                                );
                                                let exponent = parse_whole_value(
                                                    &bytes[(*ep + 1).min(bytes.len() - 1)..],
                                                );
                                                Value::Number(
                                                    sign as f64
                                                        * (whole_value as f64 + fract_value)
                                                        * 10f64.powf(exponent as f64),
                                                )
                                            }
                                        }
                                        (Some(dp), None) => {
                                            let whole_value = parse_whole_value(&bytes[..*dp]);
                                            let fract_value = parse_fract_value(
                                                &bytes[(*dp + 1).min(bytes.len() - 1)..],
                                            );
                                            Value::Number(
                                                sign as f64 * (whole_value as f64 + fract_value),
                                            )
                                        }
                                        (None, Some(ep)) => {
                                            let whole_value = parse_whole_value(&bytes[..*ep]);
                                            let exponent = parse_whole_value(
                                                &bytes[(*ep + 1).min(bytes.len() - 1)..],
                                            );
                                            Value::Number(
                                                sign as f64
                                                    * whole_value as f64
                                                    * 10f64.powf(exponent as f64),
                                            )
                                        }
                                        (None, None) => {
                                            let whole_value: i64 =
                                                parse_whole_value(bytes).try_into()?;
                                            Value::Integer(sign * whole_value)
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => Value::Nil,
                },
            );
            Ok(CallbackReturn::Return)
        }),
    )
    .unwrap();

    ctx.set_global(
        "tostring",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                Err("Bad argument to tostring".into_value(ctx).into())
            } else {
                match meta_ops::tostring(ctx, stack.get(0))? {
                    MetaResult::Value(v) => {
                        stack[0] = v;
                        stack.drain(1..);
                        Ok(CallbackReturn::Return)
                    }
                    MetaResult::Call(call) => {
                        stack.replace(ctx, Variadic(call.args));
                        Ok(CallbackReturn::Call {
                            function: call.function,
                            then: None,
                        })
                    }
                }
            }
        }),
    )
    .unwrap();

    ctx.set_global(
        "error",
        Callback::from_fn(&ctx, |_, _, stack| Err(stack.get(0).into())),
    )
    .unwrap();

    ctx.set_global(
        "assert",
        Callback::from_fn(&ctx, |ctx, _, stack| {
            if stack.get(0).to_bool() {
                Ok(CallbackReturn::Return)
            } else if stack.get(1).is_nil() {
                Err("assertion failed!".into_value(ctx).into())
            } else {
                Err(stack.get(1).into())
            }
        }),
    )
    .unwrap();

    ctx.set_global(
        "pcall",
        Callback::from_fn(&ctx, move |ctx, _, mut stack| {
            #[derive(Collect)]
            #[collect(require_static)]
            struct PCall;

            impl<'gc> Sequence<'gc> for PCall {
                fn poll(
                    &mut self,
                    _ctx: Context<'gc>,
                    _exec: Execution<'gc, '_>,
                    mut stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    stack.push_front(Value::Boolean(true));
                    Ok(SequencePoll::Return)
                }

                fn error(
                    &mut self,
                    ctx: Context<'gc>,
                    _exec: Execution<'gc, '_>,
                    error: Error<'gc>,
                    mut stack: Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    stack.clear();
                    stack.extend([Value::Boolean(false), error.to_value(ctx)]);
                    Ok(SequencePoll::Return)
                }
            }

            let function = meta_ops::call(ctx, stack.get(0))?;
            stack.pop_front();
            Ok(CallbackReturn::Call {
                function,
                then: Some(BoxSequence::new(&ctx, PCall)),
            })
        }),
    )
    .unwrap();

    ctx.set_global(
        "type",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                Err("Missing argument to type".into_value(ctx).into())
            } else {
                stack.replace(ctx, stack.get(0).type_name());
                Ok(CallbackReturn::Return)
            }
        }),
    )
    .unwrap();

    ctx.set_global(
        "select",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let ind = stack.get(0);
            if let Some(n) = ind.to_integer() {
                if n >= 1 {
                    let last = (n as usize).min(stack.len());
                    stack.drain(0..last);
                    return Ok(CallbackReturn::Return);
                } else if n < 0 {
                    let inverse_index = n.unsigned_abs() as usize;
                    let len = stack.len();
                    if inverse_index < len {
                        stack.drain(0..len - inverse_index);
                        return Ok(CallbackReturn::Return);
                    }
                }
            }

            if matches!(ind, Value::String(s) if s == b"#") {
                stack.replace(ctx, stack.len() as i64 - 1);
                return Ok(CallbackReturn::Return);
            }

            Err("Bad argument to 'select'".into_value(ctx).into())
        }),
    )
    .unwrap();

    ctx.set_global(
        "rawget",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (table, key): (Table, Value) = stack.consume(ctx)?;
            stack.replace(ctx, table.get(ctx, key));
            Ok(CallbackReturn::Return)
        }),
    )
    .unwrap();

    ctx.set_global(
        "rawset",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (table, key, value): (Table, Value, Value) = stack.consume(ctx)?;
            table.set(ctx, key, value)?;
            stack.replace(ctx, table);
            Ok(CallbackReturn::Return)
        }),
    )
    .unwrap();

    ctx.set_global(
        "getmetatable",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if let Value::Table(t) = stack.get(0) {
                stack.replace(ctx, t.metatable());
                Ok(CallbackReturn::Return)
            } else {
                Err("'getmetatable' can only be used on table types"
                    .into_value(ctx)
                    .into())
            }
        }),
    )
    .unwrap();

    ctx.set_global(
        "setmetatable",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (t, mt): (Table, Option<Table>) = stack.consume(ctx)?;
            t.set_metatable(&ctx, mt);
            stack.replace(ctx, t);
            Ok(CallbackReturn::Return)
        }),
    )
    .unwrap();

    fn next<'gc>(
        ctx: Context<'gc>,
        table: Table<'gc>,
        index: Value<'gc>,
    ) -> Result<(Value<'gc>, Value<'gc>), Value<'gc>> {
        match table.next(index) {
            NextValue::Found { key, value } => Ok((key, value)),
            NextValue::Last => Ok((Value::Nil, Value::Nil)),
            NextValue::NotFound => Err("invalid table key".into_value(ctx)),
        }
    }

    let next = Callback::from_fn(&ctx, |ctx, _, mut stack| {
        let (table, index): (Table, Value) = stack.consume(ctx)?;
        stack.replace(ctx, next(ctx, table, index)?);
        Ok(CallbackReturn::Return)
    });

    ctx.set_global("next", next).unwrap();

    ctx.set_global(
        "pairs",
        Callback::from_fn_with(&ctx, next, move |next, ctx, _, mut stack| {
            let table = stack.get(0);
            if let Some(mt) = match table {
                Value::Table(t) => t.metatable(),
                Value::UserData(u) => u.metatable(),
                _ => None,
            } {
                let pairs = mt.get(ctx, MetaMethod::Pairs);
                if !pairs.is_nil() {
                    let function = meta_ops::call(ctx, pairs)?;
                    stack.replace(ctx, (table, Value::Nil));
                    return Ok(CallbackReturn::Call {
                        function,
                        then: None,
                    });
                }
            }

            stack.replace(ctx, (*next, table));
            Ok(CallbackReturn::Return)
        }),
    )
    .unwrap();

    let inext = Callback::from_fn(&ctx, |ctx, _, mut stack| {
        let (table, index): (Value, Option<i64>) = stack.consume(ctx)?;
        let next_index = index.unwrap_or(0) + 1;
        Ok(match meta_ops::index(ctx, table, next_index.into())? {
            MetaResult::Value(v) => {
                if !v.is_nil() {
                    stack.extend([next_index.into(), v]);
                }
                CallbackReturn::Return
            }
            MetaResult::Call(call) => {
                #[derive(Collect)]
                #[collect(require_static)]
                struct INext(i64);

                impl<'gc> Sequence<'gc> for INext {
                    fn poll(
                        &mut self,
                        _ctx: Context<'gc>,
                        _exec: Execution<'gc, '_>,
                        mut stack: Stack<'gc, '_>,
                    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                        if !stack.get(0).is_nil() {
                            stack.push_front(self.0.into());
                        }
                        Ok(SequencePoll::Return)
                    }
                }

                stack.extend(call.args);
                CallbackReturn::Call {
                    function: call.function,
                    then: Some(BoxSequence::new(&ctx, INext(next_index))),
                }
            }
        })
    });

    ctx.set_global(
        "ipairs",
        Callback::from_fn_with(&ctx, inext, move |inext, ctx, _, mut stack| {
            stack.into_front(ctx, *inext);
            Ok(CallbackReturn::Return)
        }),
    )
    .unwrap();

    ctx.set_global(
        "collectgarbage",
        Callback::from_fn(&ctx, move |ctx, _, mut stack| {
            match stack.consume::<Option<String>>(ctx)? {
                Some(arg) if arg == "count" => {
                    stack.into_back(ctx, ctx.metrics().total_allocation() as f64 / 1024.0);
                }
                Some(_) => {
                    return Err("bad argument to 'collectgarbage'".into_value(ctx).into());
                }
                None => {}
            }
            Ok(CallbackReturn::Return)
        }),
    )
    .unwrap();
}
