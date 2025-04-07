use crate::{Callback, CallbackReturn, Context, IntoValue, String, Table, Value};

pub fn load_string(ctx: Context) {
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

    string.set_field(
        ctx,
        "byte",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (string, i, j) = stack.consume::<(String, Option<i64>, Option<i64>)>(ctx)?;
            let bytes = string.as_bytes();
            let len = string.len();

            if string.is_empty() {
                return Ok(CallbackReturn::Return);
            }

            let i = match i {
                Some(index) if index < 0 => (len + index + 1).max(1) as usize - 1,
                Some(index) if index > 0 => (index - 1) as usize,
                _ => 0
            };

            let j = match j {
                Some(index) if index < 0 => (len + index + 1).max(1) as usize - 1,
                Some(index) if index > 0 => (index - 1) as usize,
                None => i,
                _ => 0,
            }.min(len as usize - 1);

            if i > len as usize {
                stack.replace(ctx, Value::Nil);
                return Ok(CallbackReturn::Return);
            }

            if i == j {
                stack.replace(ctx, bytes[i]);
                return Ok(CallbackReturn::Return);
            }

            if i > j {
                return Ok(CallbackReturn::Return);
            }

            stack.replace(ctx, &bytes[i..=j]);
            Ok(CallbackReturn::Return)
        })
    );

    string.set_field(
        ctx,
        "char",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let iter = stack.into_iter();

            if iter.is_empty() {
                return Ok(CallbackReturn::Return);
            }

            let mut result = std::string::String::with_capacity(iter.len());

            for ch in iter {
                let number = match ch.to_integer() {
                    Some(number) => number,
                    None => return Err("invalid value, expected `integer` or `string` or `number`".into_value(ctx).into())
                };
                let code = if number < 0 {
                    return Err(format!("value (`{}`) cannot be negative", number).into_value(ctx).into())
                } else if number > i64::from(u32::MAX) {
                    (number as u64 % 0x110000) as u32
                } else {
                    (number as u32) % 0x110000
                };
                match std::char::from_u32(code) {
                    Some(ch) => result.push(ch),
                    None => return Err(format!("invalid symbol code (`{}`)", code).into_value(ctx).into())
                }
            }

            stack.replace(ctx, result);
            Ok(CallbackReturn::Return)
        })
    );

    string.set_field(
        ctx,
        "find",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (s, pattern, init, plain) = stack.consume::<(String, String, Option<i64>, Option<bool>)>(ctx)?;
            let len = s.len();
            let plain = plain.unwrap_or(false);
            let init = init.unwrap_or(1);
            let init = if init < 0 {
                (len + init + 1).max(1)
            } else {
                init
            };

            if 1 > init || init > len {
                stack.replace(ctx, Value::Nil);
                return Ok(CallbackReturn::Return);
            }

            let init = init - 1;

            let pattern = pattern.to_str()?;
            let s = s.to_str()?;
            let s = &s[init as usize..];
            if plain {
                let index = s.find(pattern);
                if let Some(index) = index {
                    let start = init + index as i64 + 1;
                    let end = start + pattern.len() as i64 - 1;
                    stack.replace(ctx, [start, end]);
                    Ok(CallbackReturn::Return)
                } else {
                    stack.replace(ctx, Value::Nil);
                    Ok(CallbackReturn::Return)
                }
            } else {
                let mut pat = lua_patterns::LuaPattern::new(pattern);
                if pat.matches(s) {
                    let range = pat.range();
                    let start = init + range.start as i64 + 1;
                    let end = init + range.end as i64;
                    let captures = pat.captures(s).iter().map(ToString::to_string).collect::<Vec<_>>();
                    stack.push_back(Value::Integer(start));
                    stack.push_back(Value::Integer(end));
                    for capture in captures {
                        // TODO: mismatched groups?? maybe???
                        stack.push_back(capture.into_value(ctx))
                    }
                    Ok(CallbackReturn::Return)
                } else {
                    stack.replace(ctx, Value::Nil);
                    Ok(CallbackReturn::Return)
                }
            }
        })
    );

    ctx.set_global("string", string);
}
