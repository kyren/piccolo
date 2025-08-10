use std::collections::HashMap;

use crate::{
    meta_ops, BoxSequence, Callback, CallbackReturn, Context, Error, FromValue, IntoValue,
    Sequence, String, Table, Value,
};

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
        "byte",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (string, i, j) = stack.consume::<(String, Option<i64>, Option<i64>)>(ctx)?;
            let i = i.unwrap_or(1);
            let substr = sub(string.as_bytes(), i, j.or(Some(i)))?;
            stack.extend(substr.iter().map(|b| Value::Integer(i64::from(*b))));
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "char",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let string = ctx.intern(
                &stack
                    .into_iter()
                    .map(|c| u8::from_value(ctx, c))
                    .collect::<Result<Vec<_>, _>>()?,
            );
            stack.replace(ctx, string);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "sub",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (string, i, j) = stack.consume::<(String, i64, Option<i64>)>(ctx)?;
            let substr = ctx.intern(sub(string.as_bytes(), i, j)?);
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

    string.set_field(
        ctx,
        "find",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (s, pattern, init, plain) =
                stack.consume::<(String, String, Option<i64>, Option<bool>)>(ctx)?;
            let plain = plain.unwrap_or(false);

            let pattern = pattern.to_str()?;
            let s = s.to_str()?;

            let Some((start, end, captures)) =
                lsonar::find(s, pattern, init.map(|i| i as isize), plain).map_err(|err| {
                    let err = err.to_string();
                    err.into_value(ctx)
                })?
            else {
                stack.replace(ctx, Value::Nil);
                return Ok(CallbackReturn::Return);
            };

            stack.clear();
            stack.into_back(ctx, start as i64);
            stack.into_back(ctx, end as i64);

            for capture in captures {
                stack.into_back(ctx, capture)
            }

            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "match",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (s, pattern, init) = stack.consume::<(String, String, Option<i64>)>(ctx)?;

            let pattern = pattern.to_str()?;
            let s = s.to_str()?;

            let Some(captures) =
                lsonar::r#match(s, pattern, init.map(|i| i as isize)).map_err(|err| {
                    let err = err.to_string();
                    err.into_value(ctx)
                })?
            else {
                stack.replace(ctx, Value::Nil);
                return Ok(CallbackReturn::Return);
            };

            stack.clear();
            for capture in captures {
                stack.into_back(ctx, capture)
            }

            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "gmatch",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            use std::{rc::Rc, sync::Mutex};

            #[derive(gc_arena::Collect, Clone)]
            #[collect(require_static)]
            struct GMatchIteratorWrapper(Rc<Mutex<lsonar::gmatch::GMatchIterator>>);

            impl GMatchIteratorWrapper {
                fn new(iter: lsonar::gmatch::GMatchIterator) -> Self {
                    Self(Rc::new(Mutex::new(iter)))
                }
            }

            impl<'gc> Sequence<'gc> for GMatchIteratorWrapper {
                fn poll(
                    self: std::pin::Pin<&mut Self>,
                    ctx: Context<'gc>,
                    _exec: crate::Execution<'gc, '_>,
                    mut stack: crate::Stack<'gc, '_>,
                ) -> Result<crate::SequencePoll<'gc>, Error<'gc>> {
                    stack.clear();
                    let root = Rc::clone(&self.0);
                    let mut root = root.lock().map_err(|err| {
                        let err = err.to_string();
                        err.into_value(ctx)
                    })?;
                    match root.next() {
                        Some(captures) => {
                            let captures = captures.map_err(|err| {
                                let err = err.to_string();
                                err.into_value(ctx)
                            })?;
                            for capture in captures {
                                stack.into_back(ctx, capture)
                            }
                            Ok(crate::SequencePoll::Return)
                        }
                        None => {
                            stack.into_back(ctx, Value::Nil);
                            Ok(crate::SequencePoll::Return)
                        }
                    }
                }
            }

            let (s, pattern) = stack.consume::<(String, String)>(ctx)?;

            let s = s.to_str()?;
            let pattern = pattern.to_str()?;

            let iter = lsonar::gmatch(s, pattern).map_err(|err| {
                let err = err.to_string();
                err.into_value(ctx)
            })?;

            let root = GMatchIteratorWrapper::new(iter);

            let gmatch = Callback::from_fn_with(&ctx, root, |root, ctx, _, _| {
                Ok(CallbackReturn::Sequence(BoxSequence::new(
                    &ctx,
                    root.clone(),
                )))
            });

            stack.replace(ctx, gmatch);
            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "gsub",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (s, pattern, repl, n) =
                stack.consume::<(String, String, Value, Option<i64>)>(ctx)?;

            let pattern = pattern.to_str()?;
            let s = s.to_str()?;

            let (value, n) = match repl {
                Value::String(repl) => lsonar::gsub(
                    s,
                    pattern,
                    lsonar::Repl::String(repl.to_str()?),
                    n.map(|n| n as usize),
                )
                .map_err(|err| {
                    let err = err.to_string();
                    err.into_value(ctx)
                })?,
                Value::Table(repl) => {
                    let mut map = HashMap::with_capacity(repl.length() as usize); // TODO: we need work with `Table` directly
                    for (key, value) in repl.iter() {
                        let key = key.into_string(ctx).ok_or_else(|| {
                            Error::from_value(
                                "key must be a `string`, `number` or `integer`".into_value(ctx),
                            )
                        })?;
                        let value = value.into_string(ctx).ok_or_else(|| {
                            Error::from_value(
                                "value must be a `string`, `number`, or `integer`".into_value(ctx),
                            )
                        })?;
                        map.insert(
                            key.display_lossy().to_string(),
                            value.display_lossy().to_string(),
                        );
                    }
                    lsonar::gsub(s, pattern, lsonar::Repl::Table(&map), n.map(|n| n as usize))
                        .map_err(|err| {
                            let err = err.to_string();
                            err.into_value(ctx)
                        })?
                }
                Value::Function(_) => {
                    // TODO: we need to implement this, but i don't know how to do it
                    let _call = meta_ops::call(ctx, repl)?;
                    return Err("not implemented".into_value(ctx).into());
                }
                _ => {
                    return Err(format!(
                        "invalid `repl` value, expected `string`, `table` or `function`"
                    )
                    .into_value(ctx)
                    .into())
                }
            };

            stack.clear();
            stack.into_back(ctx, value);
            stack.into_back(ctx, n as i64);

            Ok(CallbackReturn::Return)
        }),
    );

    ctx.set_global("string", string);
}

fn sub(string: &[u8], i: i64, j: Option<i64>) -> Result<&[u8], std::num::TryFromIntError> {
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
