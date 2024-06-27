use super::{expand_substitution, Capture, FindBackend};
use crate::async_callback::Locals;
use crate::meta_ops::MetaResult;
use crate::stdlib::string::convert_index;
use crate::{
    async_sequence, meta_ops, string, Callback, CallbackReturn, Context, Error, FromValue,
    IntoValue, SequenceReturn, Stack, StashedFunction, String, Value,
};
use std::borrow::Cow;

fn lua_capture_value<'gc>(ctx: Context<'gc>, cap: &Capture, str: &[u8]) -> Value<'gc> {
    if cap.pos {
        Value::Integer(cap.start as i64 + 1)
    } else {
        Value::String(ctx.intern(&str[cap.start..cap.end]))
    }
}

pub fn lua_find<'gc, F: FindBackend>(
    ctx: Context<'gc>,
    stack: &mut Stack<'gc, '_>,
) -> Result<CallbackReturn<'gc>, Error<'gc>> {
    let (str, pat, init, plain) =
        stack.consume::<(string::String, string::String, Option<i64>, Option<bool>)>(ctx)?;
    let start = convert_index(init.unwrap_or(1), str.len())?;

    let res = F::str_find(&pat, &str, start, plain.unwrap_or(false))?;

    if let Some(m) = res {
        // Lua expects inclusive 1-indexed ranges
        stack.push_back(Value::Integer(m.start as i64 + 1));
        stack.push_back(Value::Integer(m.end as i64));

        stack.extend(m.captures.iter().map(|c| lua_capture_value(ctx, &c, &str)));
    } else {
        stack.push_back(Value::Nil);
    }
    Ok(CallbackReturn::Return)
}

pub fn lua_match<'gc, F: FindBackend>(
    ctx: Context<'gc>,
    stack: &mut Stack<'gc, '_>,
) -> Result<CallbackReturn<'gc>, Error<'gc>> {
    let (str, pat, init) = stack.consume::<(string::String, string::String, Option<i64>)>(ctx)?;
    let start = convert_index(init.unwrap_or(1), str.len())?;

    let res = F::str_find(&pat, &str, start, false)?;

    if let Some(m) = res {
        if m.captures.is_empty() {
            stack.push_back(Value::String(ctx.intern(&str[m.start..m.end])));
        } else {
            stack.extend(m.captures.iter().map(|c| lua_capture_value(ctx, &c, &str)));
        }
    } else {
        stack.push_back(Value::Nil);
    }
    Ok(CallbackReturn::Return)
}

pub fn lua_gmatch<'gc, F: FindBackend>(
    ctx: Context<'gc>,
    stack: &mut Stack<'gc, '_>,
) -> Result<CallbackReturn<'gc>, Error<'gc>> {
    let (str, pat, init) = stack.consume::<(String, String, Option<i64>)>(ctx)?;
    let start = convert_index(init.unwrap_or(1), str.len() as i64)?;

    let cur_idx = std::cell::Cell::new(start);
    let last_match_end = std::cell::Cell::new(None);

    let gmatch = Callback::from_fn_with(
        &ctx,
        (str, pat, (cur_idx, last_match_end)),
        |(str, pat, (cur_idx, last_match_end)), ctx, _, mut stack| {
            stack.clear();
            loop {
                let start = cur_idx.get();
                let res = F::str_find(&pat, &str, start, false)?;

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
                        stack.push_back(Value::String(ctx.intern(&str[m.start..m.end])));
                    } else {
                        stack.extend(m.captures.iter().map(|c| lua_capture_value(ctx, &c, &str)));
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
}

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

pub fn lua_gsub_impl<'gc, F: FindBackend>(ctx: Context<'gc>) -> Callback<'gc> {
    Callback::from_fn(&ctx, |ctx, _, mut stack| {
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
                        let res = F::str_find(&pat, &str, cur_idx, false)?;
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
                                let replaced = expand_substitution(
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
                                    stack.extend(
                                        m.captures.iter().map(|c| lua_capture_value(ctx, &c, &str)),
                                    );
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
    })
}
