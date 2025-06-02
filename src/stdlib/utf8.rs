use crate::{
    Callback, CallbackReturn, Context, Error, IntoValue, String as LuaString, Table, Value,
};

fn utf8_sequence_length<'gc>(
    ctx: Context<'gc>,
    byte: u8,
    position: usize,
) -> Result<usize, Error<'gc>> {
    if byte & 0x80 == 0 {
        Ok(1)
    } else if byte & 0xE0 == 0xC0 {
        Ok(2)
    } else if byte & 0xF0 == 0xE0 {
        Ok(3)
    } else if byte & 0xF8 == 0xF0 {
        Ok(4)
    } else {
        Err(
            format!("invalid UTF-8 sequence at position {}", position + 1)
                .into_value(ctx)
                .into(),
        )
    }
}

fn validate_utf8_sequence<'gc>(
    ctx: Context<'gc>,
    position: usize,
    expected_bytes: usize,
    bytes: &[u8],
) -> Result<(), Error<'gc>> {
    if position + expected_bytes > bytes.len() {
        return Err(
            format!("incomplete UTF-8 code at position {}", position + 1)
                .into_value(ctx)
                .into(),
        );
    }

    for i in 1..expected_bytes {
        if bytes[position + i] & 0xC0 != 0x80 {
            return Err(format!("invalid UTF-8 code at position {}", position + 1)
                .into_value(ctx)
                .into());
        }
    }

    Ok(())
}

fn decode_utf8_codepoint(position: usize, expected_bytes: usize, bytes: &[u8]) -> u32 {
    match expected_bytes {
        1 => bytes[position] as u32,
        2 => ((bytes[position] & 0x1F) as u32) << 6 | ((bytes[position + 1] & 0x3F) as u32),
        3 => {
            ((bytes[position] & 0x0F) as u32) << 12
                | ((bytes[position + 1] & 0x3F) as u32) << 6
                | ((bytes[position + 2] & 0x3F) as u32)
        }
        4 => {
            ((bytes[position] & 0x07) as u32) << 18
                | ((bytes[position + 1] & 0x3F) as u32) << 12
                | ((bytes[position + 2] & 0x3F) as u32) << 6
                | ((bytes[position + 3] & 0x3F) as u32)
        }
        _ => unreachable!(), // this should never happen!!
    }
}

fn adjust_index(index: i64, len: usize) -> usize {
    if index > 0 {
        index.saturating_sub(1) as usize
    } else if index < 0 {
        len.saturating_sub(index.unsigned_abs() as usize)
    } else {
        0
    }
}

fn calculate_string_range(start: usize, end: usize, len: usize) -> Option<(usize, usize)> {
    if start >= len || (end < start && end != 0) {
        None
    } else {
        Some((start, end.min(len)))
    }
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

            let callback = Callback::from_fn_with(&ctx, None, |first_call, ctx, _, mut stack| {
                let (s, n) = stack.consume::<(LuaString, i64)>(ctx)?;

                if n == 0 {
                    stack.into_back(ctx, 1);
                } else {
                    stack.into_back(ctx, n + 1);
                }

                let s = s.to_str()?;
                let n = n as usize;

                if n >= s.len() {
                    stack.replace(ctx, (Value::Nil, Value::Nil));
                    return Ok(CallbackReturn::Return);
                }

                let bytes = &s.as_bytes()[n..];

                let mut chunks = bytes.utf8_chunks();

                if let Some(chunk) = chunks.next() {
                    if !chunk.invalid().is_empty() {
                        return Err("Invalid UTF-8 byte sequence".into_value(ctx).into());
                    }

                    if let Some(c) = chunk.valid().chars().next() {
                        if c.is_ascii() {
                            stack.into_back(ctx, c as i64);
                        } else {
                            let len = c.len_utf8();
                            let n = stack.consume::<i64>(ctx)?;
                            stack.replace(ctx, (n + len as i64, c as i64));
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
            let bytes = s.as_bytes();
            let len = bytes.len();

            let i = i.unwrap_or(1);
            let j = j.unwrap_or(-1);

            let start = adjust_index(i, len);
            let end = adjust_index(j, len);

            let (start, end) = match calculate_string_range(start, end, len) {
                Some(range) => range,
                None => {
                    stack.replace(ctx, 0);
                    return Ok(CallbackReturn::Return);
                }
            };

            let mut char_count = 0;
            let mut position = start;

            while position <= end {
                if position >= len {
                    break;
                }

                let byte = bytes[position];

                let expected_bytes = match utf8_sequence_length(ctx, byte, position) {
                    Ok(len) => len,
                    Err(_) => {
                        stack.clear();
                        stack.into_back(ctx, Value::Nil);
                        stack.into_back(ctx, position as i64 + 1);
                        return Ok(CallbackReturn::Return);
                    }
                };

                match validate_utf8_sequence(ctx, position, expected_bytes, bytes) {
                    Ok(_) => {}
                    Err(_) => {
                        stack.clear();
                        stack.into_back(ctx, Value::Nil);
                        stack.into_back(ctx, position as i64 + 1);
                        return Ok(CallbackReturn::Return);
                    }
                }

                char_count += 1;
                position += expected_bytes;
            }

            stack.replace(ctx, char_count);
            Ok(CallbackReturn::Return)
        }),
    );

    utf8.set_field(
        ctx,
        "codepoint",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            fn is_valid_lua_index(index: i64, length: i64) -> bool {
                if index == 0 {
                    false
                } else if index > 0 {
                    index <= length
                } else {
                    index >= -length
                }
            }

            let (s, i, j) = stack.consume::<(String, Option<i64>, Option<i64>)>(ctx)?;
            let bytes = s.as_bytes();
            let len = bytes.len();

            let i = i.unwrap_or(1);
            let j = j.unwrap_or(i);

            if !is_valid_lua_index(j, len as i64) {
                return Err("bad argument #3 to 'codepoint' (out of bounds)"
                    .into_value(ctx)
                    .into());
            }

            if !is_valid_lua_index(i, len as i64) {
                return Err(format!("bad argument #2 to 'codepoint' (out of bounds)",)
                    .into_value(ctx)
                    .into());
            }

            let start = adjust_index(i, len);
            let end = adjust_index(j, len);

            if start >= len || end < start {
                // Return empty result if normalized range is invalid
                return Ok(CallbackReturn::Return);
            }

            let mut position = start;
            let mut codepoints = Vec::new();

            while position <= end {
                if position >= len {
                    break;
                }

                let byte = bytes[position];

                let expected_bytes = utf8_sequence_length(ctx, byte, position)?;

                validate_utf8_sequence(ctx, position, expected_bytes, bytes)?;

                let code_point = decode_utf8_codepoint(position, expected_bytes, bytes);

                if position <= end {
                    codepoints.push(code_point as i64);
                }

                position += expected_bytes;
            }

            stack.clear();
            for codepoint in codepoints {
                stack.push_back(Value::Integer(codepoint));
            }

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

            let mut position = adjust_index(i, len);

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

                let mut current_byte_index = adjust_index(i, len);

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
