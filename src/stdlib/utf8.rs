use crate::{Callback, CallbackReturn, Context, IntoValue, String as LuaString, Table, Value};

fn convert_index(i: i64, len: usize) -> Option<usize> {
    let val = match i {
        0 => 0,
        v @ 1.. => v - 1,
        v @ ..=-1 => (len as i64 + v).max(0),
    };
    usize::try_from(val).ok()
}

fn convert_index_end(i: i64, len: usize) -> Option<usize> {
    let val = match i {
        v @ 0.. => v,
        v @ ..=-1 => (len as i64 + v + 1).max(0),
    };
    usize::try_from(val).ok()
}

pub fn load_utf8(ctx: Context) {
    let utf8 = Table::new(&ctx);

    utf8.set_field(
        ctx,
        "char",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let mut bytes = Vec::with_capacity(stack.len() * 4);
            let iter = stack.into_iter().enumerate();

            for (idx, i) in iter {
                let code = match i.to_integer() {
                    Some(code) => code as u32,
                    None => {
                        return Err(format!(
                            "bad argument #{} to 'char' (number expected, got {})",
                            idx + 1,
                            i.type_name()
                        )
                        .into_value(ctx)
                        .into())
                    }
                };

                if let Some(c) = char::from_u32(code) {
                    let mut buf = [0; 4];
                    let utf8_bytes = c.encode_utf8(&mut buf).as_bytes();
                    bytes.extend_from_slice(utf8_bytes);
                } else {
                    return Err(format!(
                        "bad argument #{} to 'char' (value out of range)",
                        idx + 1
                    )
                    .into_value(ctx)
                    .into());
                }
            }

            let result = ctx.intern(&bytes);
            stack.replace(ctx, result);

            Ok(CallbackReturn::Return)
        }),
    );

    let _ = utf8.set(ctx, "charpattern", r"[\0-\x7F\xC2-\xF4][\x80-\xBF]*");

    utf8.set_field(
        ctx,
        "codes",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let s = stack.consume::<LuaString>(ctx)?;

            let callback = Callback::from_fn(&ctx, |ctx, _, mut stack| {
                let (s, n) = stack.consume::<(LuaString, i64)>(ctx)?;

                let bytes = s.as_bytes();
                let n = n as usize;

                if n >= bytes.len() {
                    stack.replace(ctx, (Value::Nil, Value::Nil));
                    return Ok(CallbackReturn::Return);
                }

                let bytes = &bytes[n..];

                let mut chunks = bytes.utf8_chunks();

                if let Some(chunk) = chunks.next() {
                    if !chunk.invalid().is_empty() {
                        return Err("Invalid UTF-8 byte sequence".into_value(ctx).into());
                    }

                    if let Some(c) = chunk.valid().chars().next() {
                        if n == 0 {
                            stack.replace(ctx, (1, c as i64));
                        } else {
                            if c.is_ascii() {
                                stack.replace(ctx, (n as i64 + 1, c as i64));
                            } else {
                                let len = c.len_utf8();
                                stack.replace(ctx, ((n + len) as i64, c as i64));
                            }
                        }
                        Ok(CallbackReturn::Return)
                    } else {
                        stack.replace(ctx, (Value::Nil, Value::Nil));
                        Ok(CallbackReturn::Return)
                    }
                } else {
                    stack.replace(ctx, (Value::Nil, Value::Nil));
                    Ok(CallbackReturn::Return)
                }
            });

            stack.replace(ctx, (callback, s, 0));

            Ok(CallbackReturn::Return)
        }),
    );

    utf8.set_field(
        ctx,
        "len",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (s, i, j) = stack.consume::<(String, Option<i64>, Option<i64>)>(ctx)?;

            let s = match std::str::from_utf8(s.as_bytes()) {
                Ok(s) => s,
                Err(err) => {
                    let position = err.error_len().unwrap_or_default();
                    stack.replace(ctx, (false, position as i64 + 1));
                    return Ok(CallbackReturn::Return);
                }
            };
            let len = s.len();

            let start = convert_index(i.unwrap_or(1), len).unwrap_or(usize::MAX);
            let end = convert_index_end(j.unwrap_or(len as i64), len)
                .unwrap_or(usize::MAX)
                .min(len);

            // TODO: we need to check this conditions
            if start >= len || (end < start && end != 0) {
                stack.replace(ctx, 0);
                return Ok(CallbackReturn::Return);
            }

            let s = &s[start..=end];

            stack.replace(ctx, s.chars().count() as i64);

            Ok(CallbackReturn::Return)
        }),
    );

    utf8.set_field(
        ctx,
        "codepoint",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (s, i, j) = stack.consume::<(String, Option<i64>, Option<i64>)>(ctx)?;

            let s = std::str::from_utf8(s.as_bytes()).map_err(|err| {
                format!(
                    "bad argument #1 to 'codepoint' (invalid byte sequence at {})",
                    err.error_len().unwrap_or_default()
                )
                .into_value(ctx)
            })?;
            let len = s.len();

            let i = i.unwrap_or(1);
            let j = j.unwrap_or(i);

            let start = convert_index(i, len).unwrap_or(usize::MAX);
            let end = convert_index_end(j, len).unwrap_or(usize::MAX).min(len);

            if start > len {
                stack.replace(ctx, Value::Nil);
                return Ok(CallbackReturn::Return);
            }

            if start < 1 {
                return Err("bad argument #2 (out of range)".into_value(ctx).into());
            }

            if start > end {
                return Ok(CallbackReturn::Return);
            }

            let s = &s[start..=end];

            stack.extend(s.chars().map(|c| Value::Integer(c as i64)));

            Ok(CallbackReturn::Return)
        }),
    );

    utf8.set_field(
        ctx,
        "offset",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (s, n, i): (String, i64, Option<i64>) = stack.consume(ctx)?;
            let bytes = s.as_bytes();
            let len = bytes.len();

            let i = i.unwrap_or(if n >= 0 { 1 } else { len as i64 + 1 });

            if i == 0 {
                return Err("bad argument #3 to 'offset' (position out of bounds)"
                    .into_value(ctx)
                    .into());
            }

            let mut position = convert_index(i, len).unwrap_or(usize::MAX);

            if n != 0 && position < len && (bytes[position] & 0xC0) == 0x80 {
                return Err("initial position is a continuation byte"
                    .into_value(ctx)
                    .into());
            }

            if n == 0 {
                if position >= len {
                    stack.replace(ctx, Value::Nil);
                    return Ok(CallbackReturn::Return);
                }

                while position > 0 && (bytes[position] & 0xC0) == 0x80 {
                    position -= 1;
                }

                stack.replace(ctx, (position as i64) + 1);
                return Ok(CallbackReturn::Return);
            }

            if n > 0 {
                let mut count = 0;

                while count < n && position < len {
                    if (bytes[position] & 0xC0) != 0x80 {
                        count += 1;
                    }

                    if count == n {
                        break;
                    }

                    position += 1;
                }

                if count == n {
                    stack.replace(ctx, (position as i64) + 1);
                    return Ok(CallbackReturn::Return);
                }

                if count == n - 1 && position == len {
                    stack.replace(ctx, (position as i64) + 1);
                    return Ok(CallbackReturn::Return);
                } else if count < n {
                    stack.replace(ctx, Value::Nil);
                    return Ok(CallbackReturn::Return);
                }
            } else if n < 0 {
                let target_count = -n;
                let mut count = 0i64;

                let mut current_byte_index = convert_index(i, len).unwrap_or(usize::MAX);

                while count < target_count {
                    if current_byte_index == 0 {
                        stack.replace(ctx, Value::Nil);
                        return Ok(CallbackReturn::Return);
                    }
                    current_byte_index -= 1;
                    if (bytes[current_byte_index] & 0xC0) != 0x80 {
                        count += 1;
                    }
                }
                stack.replace(ctx, (current_byte_index as i64) + 1);
                return Ok(CallbackReturn::Return);
            }

            Ok(CallbackReturn::Return)
        }),
    );

    ctx.set_global("utf8", utf8);
}
