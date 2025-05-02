use gc_arena::Collect;

use crate::{
    BoxSequence, Callback, CallbackReturn, Context, Error, IntoValue, Sequence, SequencePoll,
    Table, Value,
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
                    return Err(
                        format!("bad argument #{} to 'char' (value out of range)", idx + 1)
                            .into_value(ctx)
                            .into(),
                    );
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
            #[derive(Collect, Clone)]
            #[collect(require_static)]
            struct Codes {
                s: String,
                pos: usize,
            }

            impl<'gc> Sequence<'gc> for Codes {
                fn poll(
                    mut self: std::pin::Pin<&mut Self>,
                    ctx: Context<'gc>,
                    _exec: crate::Execution<'gc, '_>,
                    mut stack: crate::Stack<'gc, '_>,
                ) -> Result<SequencePoll<'gc>, Error<'gc>> {
                    let position = self.pos;
                    let bytes = self.s.as_bytes();
                    let len = bytes.len();

                    if position >= len {
                        stack.replace(ctx, Value::Nil);
                        return Ok(SequencePoll::Return);
                    }

                    let byte = bytes[position];

                    let expected_bytes = utf8_sequence_length(ctx, byte, position)?;

                    validate_utf8_sequence(ctx, position, expected_bytes, bytes)?;

                    let code_point = decode_utf8_codepoint(position, expected_bytes, bytes);

                    stack.clear();
                    stack.into_back(ctx, position as i64 + 1);
                    stack.into_back(ctx, code_point as i64);

                    self.pos += expected_bytes;

                    Ok(SequencePoll::Return)
                }
            }

            let s = stack.consume::<String>(ctx)?;

            let root = Codes {
                s: s.to_owned(),
                pos: 0,
            };

            let codes = Callback::from_fn_with(&ctx, root, |root, ctx, _, _| {
                Ok(CallbackReturn::Sequence(BoxSequence::new(
                    &ctx,
                    root.clone(),
                )))
            });

            stack.replace(ctx, codes);

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

            while position < end {
                let byte = bytes[position];

                let expected_bytes = match utf8_sequence_length(ctx, byte, position) {
                    Ok(len) => len,
                    Err(_) => {
                        stack.clear();
                        stack.push_back(Value::Boolean(false));
                        stack.push_back(Value::Integer(position as i64 + 1));
                        return Ok(CallbackReturn::Return);
                    }
                };

                if position + expected_bytes > end {
                    break;
                }

                match validate_utf8_sequence(ctx, position, expected_bytes, bytes) {
                    Ok(_) => {}
                    Err(_) => {
                        stack.clear();
                        stack.push_back(Value::Boolean(false));
                        stack.push_back(Value::Integer(position as i64 + 1));
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
            let (s, i, j) = stack.consume::<(String, Option<i64>, Option<i64>)>(ctx)?;
            let bytes = s.as_bytes();
            let len = bytes.len();

            let i = i.unwrap_or(1);
            let j = j.unwrap_or(i);

            let start = adjust_index(i, len);
            let end = adjust_index(j, len);

            if start >= len || end >= len || end < start {
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

            let mut pos = adjust_index(i, len);

            if n == 0 {
                if pos >= len {
                    stack.replace(ctx, Value::Nil);
                    return Ok(CallbackReturn::Return);
                }

                while pos > 0 && (bytes[pos] & 0xC0) == 0x80 {
                    pos -= 1;
                }

                stack.replace(ctx, (pos as i64) + 1);
                return Ok(CallbackReturn::Return);
            }

            if n > 0 {
                let mut count = 0;

                while count < n && pos < len {
                    if (bytes[pos] & 0xC0) != 0x80 {
                        count += 1;
                    }

                    if count == n {
                        break;
                    }

                    pos += 1;
                }

                if count == n - 1 && pos == len {
                    stack.replace(ctx, (pos as i64) + 1);
                    return Ok(CallbackReturn::Return);
                } else if count < n {
                    stack.replace(ctx, Value::Nil);
                    return Ok(CallbackReturn::Return);
                }
            } else if n < 0 {
                let mut count = 0;

                if pos > 0 && (bytes[pos - 1] & 0xC0) == 0x80 {
                    while pos > 0 && (bytes[pos - 1] & 0xC0) == 0x80 {
                        pos -= 1;
                    }
                    if pos > 0 {
                        pos -= 1;
                    }
                } else if pos > 0 {
                    pos -= 1;
                }

                while count < (-n) && pos > 0 {
                    pos -= 1;

                    while pos > 0 && (bytes[pos] & 0xC0) == 0x80 {
                        pos -= 1;
                    }

                    count += 1;
                }

                if count < (-n) {
                    stack.replace(ctx, Value::Nil);
                    return Ok(CallbackReturn::Return);
                }
            }

            stack.replace(ctx, (pos as i64) + 1);
            Ok(CallbackReturn::Return)
        }),
    );

    ctx.set_global("utf8", utf8);
}
