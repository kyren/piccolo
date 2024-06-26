use std::borrow::Cow;

use crate::async_callback::{async_sequence, Locals};
use crate::meta_ops::MetaResult;
use crate::{
    meta_ops, Callback, CallbackReturn, Context, FromValue, IntoValue, SequenceReturn, Stack,
    StashedFunction, String, Table, Value,
};

mod pattern;
mod pattern_stack;

pub fn load_string<'gc>(ctx: Context<'gc>) {
    let string = Table::new(&ctx);

    string.set_field(
        ctx,
        "len",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let string = stack.consume::<String>(ctx)?;
            let len = string.len();
            stack.replace(ctx, len);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "sub",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            fn operate_sub(
                string: &[u8],
                i: i64,
                j: Option<i64>,
            ) -> Result<&[u8], std::num::TryFromIntError> {
                let i = match i {
                    i if i > 0 => i.saturating_sub(1).try_into()?,
                    0 => 0,
                    i => string.len().saturating_sub(i.unsigned_abs().try_into()?),
                };
                let j = if let Some(j) = j {
                    if j >= 0 {
                        j.try_into()?
                    } else {
                        let j: usize = j.unsigned_abs().try_into()?;
                        string.len().saturating_sub(j.saturating_sub(1))
                    }
                } else {
                    string.len()
                }
                .clamp(0, string.len());

                Ok(if i >= j || i >= string.len() {
                    &[]
                } else {
                    &string[i..j]
                })
            }

            let (string, i, j) = stack.consume::<(String, i64, Option<i64>)>(ctx)?;
            let substr = ctx.intern(operate_sub(string.as_bytes(), i, j)?);
            stack.replace(ctx, substr);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "lower",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let string = stack.consume::<String>(ctx)?;
            let lowered = ctx.intern(
                &string
                    .as_bytes()
                    .iter()
                    .map(u8::to_ascii_lowercase)
                    .collect::<Vec<_>>(),
            );
            stack.replace(ctx, lowered);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "reverse",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let string = stack.consume::<String>(ctx)?;
            let reversed = ctx.intern(&string.as_bytes().iter().copied().rev().collect::<Vec<_>>());
            stack.replace(ctx, reversed);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "upper",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let string = stack.consume::<String>(ctx)?;
            let uppered = ctx.intern(
                &string
                    .as_bytes()
                    .iter()
                    .map(u8::to_ascii_uppercase)
                    .collect::<Vec<_>>(),
            );
            stack.replace(ctx, uppered);
            Ok(CallbackReturn::Return)
        }),
    );

    string
        .set(
            ctx,
            "char",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                // TODO: fuel usage
                let bytes = stack
                    .drain(..)
                    .map(|v| try_to_byte(v))
                    .collect::<Result<Vec<u8>, _>>()
                    .map_err(|e| e.into_value(ctx))?;
                stack.replace(ctx, ctx.intern(&bytes));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    fn try_to_byte<'gc>(v: Value<'gc>) -> Result<u8, &'static str> {
        match v.to_integer() {
            Some(n @ 0..=255) => Ok(n as u8),
            Some(_) => Err("value out of range"),
            None => Err("expected integer"),
        }
    }

    string
        .set(
            ctx,
            "rep",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                // TODO: fuel usage
                let (string, count) = stack.consume::<(String, i64)>(ctx)?;
                let repeated = string.repeat(count as usize);
                stack.replace(ctx, ctx.intern(&repeated));
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    ctx.set_global("string", string);

    if matches!(std::env::var("STACK").as_deref(), Ok("1" | "true")) {
        load_pattern_stack(ctx)
    } else {
        load_pattern_seq(ctx)
    }
}

pub fn load_pattern_stack<'gc>(ctx: Context<'gc>) {
    let table: Option<Table> = ctx.get_global("string").unwrap();
    let string = match table {
        Some(t) => t,
        None => {
            let string = Table::new(&ctx);
            ctx.set_global("string", string);
            string
        }
    };

    string
        .set(
            ctx,
            "find",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                pattern_stack::lua_find(ctx, &mut stack)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "match",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                pattern_stack::lua_match(ctx, &mut stack)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "gmatch",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let (str, pat, init) = stack.consume::<(String, String, Option<i64>)>(ctx)?;
                // TODO: overflow checks on 32 bit
                let start = match init {
                    Some(0) | None => 0,
                    Some(v @ 1..) => (v - 1) as usize,
                    Some(v @ ..=-1) => (str.len() + v).max(0) as usize,
                };

                let cur_idx = std::cell::Cell::new(start);
                let last_match_end = std::cell::Cell::new(None);

                let gmatch = Callback::from_fn_with(
                    &ctx,
                    (str, pat, (cur_idx, last_match_end)),
                    |(str, pat, (cur_idx, last_match_end)), ctx, _, mut stack| {
                        stack.clear();
                        loop {
                            let start = cur_idx.get();
                            let res = pattern_stack::str_find(&pat, &str, start, false)?;

                            if let Some(m) = res {
                                if last_match_end.get() == Some(m.end) {
                                    // TODO: does this work? (ref 6.4.1 mult matches)
                                    cur_idx.set(m.end + 1);
                                    continue;
                                } else {
                                    cur_idx.set(m.end);
                                }
                                last_match_end.set(Some(m.end));
                                if m.captures.is_empty() {
                                    stack
                                        .push_back(Value::String(ctx.intern(&str[m.start..m.end])));
                                } else {
                                    stack.extend(m.captures.iter().map(|m| {
                                        if m.pos {
                                            Value::Integer(m.start as i64 + 1)
                                        } else {
                                            Value::String(ctx.intern(&str[m.start..m.end]))
                                        }
                                    }));
                                }
                            } else {
                                cur_idx.set(str.as_bytes().len());
                                stack.push_back(Value::Nil);
                            }
                            break Ok(CallbackReturn::Return);
                        }
                    },
                );

                stack.replace(ctx, gmatch);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    fn prep_metaop_call<'gc, const N: usize>(
        ctx: Context<'gc>,
        mut stack: Stack<'gc, '_>,
        locals: Locals<'gc, '_>,
        res: MetaResult<'gc, N>,
    ) -> Option<StashedFunction> {
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

    let gsub_impl = Callback::from_fn(&ctx, |ctx, _, mut stack| {
        let (str, pat, repl, end) = stack.consume::<(String, String, Value, Option<i64>)>(ctx)?;
        let match_limit = end;
        let s = async_sequence(&ctx, |locals, mut seq| {
            let (str, pat, repl) = (
                locals.stash(&ctx, str),
                locals.stash(&ctx, pat),
                locals.stash(&ctx, repl),
            );
            async move {
                let mut match_count = 0;
                let mut cur_idx = 0;
                let mut last_end = None;
                let mut output_end = 0;
                let mut buffer: Option<Vec<u8>> = None;

                loop {
                    let match_res = seq.try_enter(|_ctx, locals, _exec, _stack| {
                        let (pat, str) = (locals.fetch(&pat), locals.fetch(&str));
                        let res = pattern_stack::str_find(&pat, &str, cur_idx, false)?;
                        Ok(res)
                    })?;

                    let m = if let Some(m) = match_res {
                        m
                    } else {
                        break;
                    };

                    if last_end == Some(m.end) {
                        // TODO: does this work? (ref 6.4.1 mult matches)
                        cur_idx = m.end + 1;
                        continue;
                    } else {
                        cur_idx = m.end;
                    }
                    last_end = Some(m.end);

                    let first_capture = m
                        .captures
                        .get(0)
                        .map(|c| (c.start, c.end, c.pos))
                        .unwrap_or((m.start, m.end, false));

                    let func = seq.try_enter(|ctx, locals, _exec, mut stack| {
                        let (repl, str) = (locals.fetch(&repl), locals.fetch(&str));
                        Ok(match repl {
                            Value::String(repl_pattern) => {
                                let replaced = pattern_stack::expand_substitution(
                                    &m,
                                    str.as_bytes(),
                                    repl_pattern.as_bytes(),
                                )?;
                                let string = match replaced {
                                    Cow::Borrowed(_) => repl_pattern, // unmodified
                                    Cow::Owned(bytes) => ctx.intern(&bytes),
                                };
                                stack.push_back(Value::String(string));
                                None
                            }
                            Value::Table(table) => {
                                let value = if !first_capture.2 {
                                    let substr = ctx.intern(&str[first_capture.0..first_capture.1]);
                                    Value::String(substr)
                                } else {
                                    Value::Integer(first_capture.0 as i64 + 1)
                                };
                                let res = meta_ops::index(ctx, Value::Table(table), value)?;
                                prep_metaop_call(ctx, stack, locals, res)
                            }
                            Value::Function(_) => {
                                let call = meta_ops::call(ctx, repl)?;
                                if m.captures.is_empty() {
                                    stack
                                        .push_back(Value::String(ctx.intern(&str[m.start..m.end])));
                                } else {
                                    stack.extend(m.captures.iter().map(|m| {
                                        if m.pos {
                                            Value::Integer(m.start as i64 + 1)
                                        } else {
                                            Value::String(ctx.intern(&str[m.start..m.end]))
                                        }
                                    }));
                                }
                                Some(locals.stash(&ctx, call))
                            }
                            _ => {
                                return Err("expected string, table, or function"
                                    .into_value(ctx)
                                    .into());
                            }
                        })
                    })?;

                    if let Some(func) = func {
                        seq.call(&func, 0).await?;
                    }

                    seq.try_enter(|ctx, locals, _exec, mut stack| {
                        let str = locals.fetch(&str);

                        let replacement = stack.consume::<Value>(ctx)?;

                        if let Value::Nil | Value::Boolean(false) = replacement {
                        } else {
                            let replacement = String::from_value(ctx, replacement)?;
                            let buf = match buffer.as_mut() {
                                None => {
                                    buffer = Some(str[0..m.start].to_owned());
                                    buffer.as_mut().unwrap()
                                }
                                Some(o) => {
                                    o.extend_from_slice(&str[output_end..m.start]);
                                    o
                                }
                            };
                            buf.extend_from_slice(replacement.as_bytes());
                            output_end = m.end;
                        }
                        Ok(())
                    })?;

                    match_count += 1;
                    if match_limit.is_some() && Some(match_count) >= match_limit {
                        break;
                    }
                }

                seq.enter(|ctx, locals, _exec, mut stack| {
                    let str = locals.fetch(&str);
                    let result = match buffer {
                        None => str,
                        Some(mut o) => {
                            o.extend_from_slice(&str[output_end..str.len() as usize]);
                            ctx.intern(&o)
                        }
                    };
                    stack.push_back(Value::String(result));
                    stack.push_back(Value::Integer(match_count));
                });

                Ok(SequenceReturn::Return)
            }
        });
        Ok(CallbackReturn::Sequence(s))
    });

    string.set(ctx, "gsub", gsub_impl).unwrap();
}

pub fn load_pattern_seq<'gc>(ctx: Context<'gc>) {
    let table: Option<Table> = ctx.get_global("string").unwrap();
    let string = match table {
        Some(t) => t,
        None => {
            let string = Table::new(&ctx);
            ctx.set_global("string", string);
            string
        }
    };

    string
        .set(
            ctx,
            "find",
            Callback::from_fn(&ctx, |ctx, _, mut stack| pattern::lua_find(ctx, &mut stack)),
        )
        .unwrap();

    string
        .set(
            ctx,
            "match",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                pattern::lua_match(ctx, &mut stack)
            }),
        )
        .unwrap();

    string
        .set(
            ctx,
            "gmatch",
            Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let (str, pat, init) = stack.consume::<(String, String, Option<i64>)>(ctx)?;
                // TODO: overflow checks on 32 bit
                let start = match init {
                    Some(0) | None => 0,
                    Some(v @ 1..) => (v - 1) as usize,
                    Some(v @ ..=-1) => (str.len() + v).max(0) as usize,
                };

                let cur_idx = std::cell::Cell::new(start);
                let last_match_end = std::cell::Cell::new(None);

                let gmatch = Callback::from_fn_with(
                    &ctx,
                    (str, pat, (cur_idx, last_match_end)),
                    |(str, pat, (cur_idx, last_match_end)), ctx, _, mut stack| {
                        stack.clear();
                        loop {
                            let start = cur_idx.get();
                            let res = pattern::str_find(&pat, &str, start, false)?;

                            if let Some(m) = res {
                                if last_match_end.get() == Some(m.end) {
                                    // TODO: does this work? (ref 6.4.1 mult matches)
                                    cur_idx.set(m.end + 1);
                                    continue;
                                } else {
                                    cur_idx.set(m.end);
                                }
                                last_match_end.set(Some(m.end));
                                if m.captures.is_empty() {
                                    stack
                                        .push_back(Value::String(ctx.intern(&str[m.start..m.end])));
                                } else {
                                    stack.extend(m.captures.iter().map(|m| {
                                        if m.pos {
                                            Value::Integer(m.start as i64 + 1)
                                        } else {
                                            Value::String(ctx.intern(&str[m.start..m.end]))
                                        }
                                    }));
                                }
                            } else {
                                cur_idx.set(str.as_bytes().len());
                                stack.push_back(Value::Nil);
                            }
                            break Ok(CallbackReturn::Return);
                        }
                    },
                );

                stack.replace(ctx, gmatch);
                Ok(CallbackReturn::Return)
            }),
        )
        .unwrap();

    fn prep_metaop_call<'gc, const N: usize>(
        ctx: Context<'gc>,
        mut stack: Stack<'gc, '_>,
        locals: Locals<'gc, '_>,
        res: MetaResult<'gc, N>,
    ) -> Option<StashedFunction> {
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

    let gsub_impl = Callback::from_fn(&ctx, |ctx, _, mut stack| {
        let (str, pat, repl, end) = stack.consume::<(String, String, Value, Option<i64>)>(ctx)?;
        let match_limit = end;
        let s = async_sequence(&ctx, |locals, mut seq| {
            let (str, pat, repl) = (
                locals.stash(&ctx, str),
                locals.stash(&ctx, pat),
                locals.stash(&ctx, repl),
            );
            async move {
                let mut match_count = 0;
                let mut cur_idx = 0;
                let mut last_end = None;
                let mut output_end = 0;
                let mut buffer: Option<Vec<u8>> = None;

                loop {
                    let match_res = seq.try_enter(|_ctx, locals, _exec, _stack| {
                        let (pat, str) = (locals.fetch(&pat), locals.fetch(&str));
                        let res = pattern::str_find(&pat, &str, cur_idx, false)?;
                        Ok(res)
                    })?;

                    let m = if let Some(m) = match_res {
                        m
                    } else {
                        break;
                    };

                    if last_end == Some(m.end) {
                        // TODO: does this work? (ref 6.4.1 mult matches)
                        cur_idx = m.end + 1;
                        continue;
                    } else {
                        cur_idx = m.end;
                    }
                    last_end = Some(m.end);

                    let first_capture = m
                        .captures
                        .get(0)
                        .map(|c| (c.start, c.end, c.pos))
                        .unwrap_or((m.start, m.end, false));

                    let func = seq.try_enter(|ctx, locals, _exec, mut stack| {
                        let (repl, str) = (locals.fetch(&repl), locals.fetch(&str));
                        Ok(match repl {
                            Value::String(repl_pattern) => {
                                let replaced = pattern::expand_substitution(
                                    &m,
                                    str.as_bytes(),
                                    repl_pattern.as_bytes(),
                                )?;
                                let string = match replaced {
                                    Cow::Borrowed(_) => repl_pattern, // unmodified
                                    Cow::Owned(bytes) => ctx.intern(&bytes),
                                };
                                stack.push_back(Value::String(string));
                                None
                            }
                            Value::Table(table) => {
                                let value = if !first_capture.2 {
                                    let substr = ctx.intern(&str[first_capture.0..first_capture.1]);
                                    Value::String(substr)
                                } else {
                                    Value::Integer(first_capture.0 as i64 + 1)
                                };
                                let res = meta_ops::index(ctx, Value::Table(table), value)?;
                                prep_metaop_call(ctx, stack, locals, res)
                            }
                            Value::Function(_) => {
                                let call = meta_ops::call(ctx, repl)?;
                                if m.captures.is_empty() {
                                    stack
                                        .push_back(Value::String(ctx.intern(&str[m.start..m.end])));
                                } else {
                                    stack.extend(m.captures.iter().map(|m| {
                                        if m.pos {
                                            Value::Integer(m.start as i64 + 1)
                                        } else {
                                            Value::String(ctx.intern(&str[m.start..m.end]))
                                        }
                                    }));
                                }
                                Some(locals.stash(&ctx, call))
                            }
                            _ => {
                                return Err("expected string, table, or function"
                                    .into_value(ctx)
                                    .into());
                            }
                        })
                    })?;

                    if let Some(func) = func {
                        seq.call(&func, 0).await?;
                    }

                    seq.try_enter(|ctx, locals, _exec, mut stack| {
                        let str = locals.fetch(&str);

                        let replacement = stack.consume::<Value>(ctx)?;

                        if let Value::Nil | Value::Boolean(false) = replacement {
                        } else {
                            let replacement = String::from_value(ctx, replacement)?;
                            let buf = match buffer.as_mut() {
                                None => {
                                    buffer = Some(str[0..m.start].to_owned());
                                    buffer.as_mut().unwrap()
                                }
                                Some(o) => {
                                    o.extend_from_slice(&str[output_end..m.start]);
                                    o
                                }
                            };
                            buf.extend_from_slice(replacement.as_bytes());
                            output_end = m.end;
                        }
                        Ok(())
                    })?;

                    match_count += 1;
                    if match_limit.is_some() && Some(match_count) >= match_limit {
                        break;
                    }
                }

                seq.enter(|ctx, locals, _exec, mut stack| {
                    let str = locals.fetch(&str);
                    let result = match buffer {
                        None => str,
                        Some(mut o) => {
                            o.extend_from_slice(&str[output_end..str.len() as usize]);
                            ctx.intern(&o)
                        }
                    };
                    stack.push_back(Value::String(result));
                    stack.push_back(Value::Integer(match_count));
                });

                Ok(SequenceReturn::Return)
            }
        });
        Ok(CallbackReturn::Sequence(s))
    });

    string.set(ctx, "gsub", gsub_impl).unwrap();
}
