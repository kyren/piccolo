use std::pin::Pin;

use gc_arena::Collect;

use crate::{
    meta_ops::{self, MetaResult},
    table::NextValue,
    BoxSequence, Callback, CallbackReturn, Context, Error, Execution, IntoValue, MetaMethod,
    Sequence, SequencePoll, Stack, String, Table, TypeError, Value, Variadic,
};

pub fn load_base<'gc>(ctx: Context<'gc>) {
    ctx.set_global("_G", ctx.globals());

    ctx.set_global(
        "tonumber",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            use crate::compiler::string_utils::{read_neg, trim_whitespace};

            fn extract_number_data(bytes: &[u8]) -> (&[u8], bool) {
                let bytes = trim_whitespace(bytes);
                let (is_neg, bytes) = read_neg(bytes);
                (bytes, is_neg)
            }

            if stack.is_empty() {
                Err("Missing argument(s) to tonumber".into_value(ctx))?
            } else if stack.len() == 1 || stack.get(1).is_nil() {
                let prenumber = stack.consume::<Value>(ctx)?;
                stack.replace(ctx, prenumber.to_numeric().unwrap_or(Value::Nil));
            } else {
                let (value, base) = stack.consume::<(Value, i64)>(ctx)?;
                // Avoid implicitly converting value to a string
                let s = match value {
                    Value::String(s) => s,
                    _ => {
                        return Err(TypeError {
                            expected: "string",
                            found: value.type_name(),
                        }
                        .into())
                    }
                };
                if !(2..=36).contains(&base) {
                    Err("base out of range".into_value(ctx))?;
                }
                let (bytes, is_neg) = extract_number_data(s.as_bytes());
                let result = bytes
                    .iter()
                    .map(|b| {
                        if b.is_ascii_digit() {
                            Some((*b - b'0') as i64)
                        } else if b.is_ascii_lowercase() {
                            Some((*b - b'a') as i64 + 10)
                        } else if b.is_ascii_uppercase() {
                            Some((*b - b'A') as i64 + 10)
                        } else {
                            None
                        }
                    })
                    .try_fold(0i64, |acc, v| match v {
                        Some(v) if v < base => Some(acc.wrapping_mul(base).wrapping_add(v)),
                        _ => None,
                    })
                    .map(|v| if is_neg { v.wrapping_neg() } else { v });
                stack.replace(ctx, result.map(Value::Integer).unwrap_or(Value::Nil));
            }

            Ok(CallbackReturn::Return)
        }),
    );

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
    );

    ctx.set_global(
        "error",
        Callback::from_fn(&ctx, |_, _, stack| Err(stack.get(0).into())),
    );

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
    );

    ctx.set_global(
        "pcall",
        Callback::from_fn(&ctx, move |ctx, _, mut stack| {
            let function = meta_ops::call(ctx, stack.get(0))?;
            stack.pop_front();
            Ok(CallbackReturn::Call {
                function,
                then: Some(BoxSequence::new(&ctx, PCall)),
            })
        }),
    );

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
    );

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
    );

    ctx.set_global(
        "rawget",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (table, key): (Table, Value) = stack.consume(ctx)?;
            stack.replace(ctx, table.get_value(ctx, key));
            Ok(CallbackReturn::Return)
        }),
    );

    ctx.set_global(
        "rawlen",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let table: Table = stack.consume(ctx)?;
            stack.replace(ctx, table.length());
            Ok(CallbackReturn::Return)
        }),
    );

    ctx.set_global(
        "rawset",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (table, key, value): (Table, Value, Value) = stack.consume(ctx)?;
            table.set(ctx, key, value)?;
            stack.replace(ctx, table);
            Ok(CallbackReturn::Return)
        }),
    );

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
    );

    ctx.set_global(
        "setmetatable",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (t, mt): (Table, Option<Table>) = stack.consume(ctx)?;
            t.set_metatable(&ctx, mt);
            stack.replace(ctx, t);
            Ok(CallbackReturn::Return)
        }),
    );

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

    ctx.set_global("next", next);

    ctx.set_global(
        "pairs",
        Callback::from_fn_with(&ctx, next, move |next, ctx, _, mut stack| {
            let table = stack.get(0);
            if let Some(mt) = match table {
                Value::Table(t) => t.metatable(),
                Value::UserData(u) => u.metatable(),
                _ => None,
            } {
                let pairs = mt.get_value(ctx, MetaMethod::Pairs);
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
    );

    let inext = Callback::from_fn(&ctx, |ctx, _, mut stack| {
        let (table, index): (Value, Option<i64>) = stack.consume(ctx)?;
        let next_index = index.unwrap_or(0).wrapping_add(1);
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
                        self: Pin<&mut Self>,
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
    );

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
    );

    ctx.set_global("_VERSION", "piccolo");
}

#[derive(Collect)]
#[collect(require_static)]
pub struct PCall;

impl<'gc> Sequence<'gc> for PCall {
    fn poll(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        _exec: Execution<'gc, '_>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        stack.into_front(ctx, true);
        Ok(SequencePoll::Return)
    }

    fn error(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        _exec: Execution<'gc, '_>,
        error: Error<'gc>,
        mut stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        stack.replace(ctx, (false, error));
        Ok(SequencePoll::Return)
    }
}
