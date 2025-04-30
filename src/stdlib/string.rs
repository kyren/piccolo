use crate::{
    meta_ops, BoxSequence, Callback, CallbackReturn, Context, Error, IntoMultiValue, IntoValue,
    Sequence, String, Table, Value,
};
use lsonar::{find, gmatch, gsub, r#match};
use std::{
    collections::HashMap,
    io::{self, Cursor, Write},
    mem,
    rc::Rc,
    sync::Mutex,
};

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
            let (s, i, j) = stack.consume::<(String, i64, Option<i64>)>(ctx)?;
            let bytes = s.as_bytes();

            let i = match i {
                i if i > 0 => i.saturating_sub(1).try_into()?,
                0 => 0,
                i => bytes.len().saturating_sub(i.unsigned_abs().try_into()?),
            };
            let j = if let Some(j) = j {
                if j >= 0 {
                    j.try_into()?
                } else {
                    let j: usize = j.unsigned_abs().try_into()?;
                    bytes.len().saturating_sub(j.saturating_sub(1))
                }
            } else {
                bytes.len()
            }
            .clamp(0, bytes.len());

            if i >= j {
                return Ok(CallbackReturn::Return);
            }

            stack.clear();
            for index in i..j {
                stack.into_back(ctx, bytes[index]);
            }

            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "char",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            if stack.is_empty() {
                return Ok(CallbackReturn::Return);
            }

            let iter = stack.into_iter();

            let mut result = std::string::String::with_capacity(iter.len());

            for ch in iter {
                let number = match ch.to_integer() {
                    Some(number) => number,
                    None => {
                        return Err("invalid value, expected `integer`, `string` or `number`"
                            .into_value(ctx)
                            .into())
                    }
                };
                let code = match u32::try_from(number) {
                    Ok(c) if c <= 0x10FFFF => c,
                    _ => {
                        return Err(format!("value out of range (`{}`)", number)
                            .into_value(ctx)
                            .into());
                    }
                };
                match std::char::from_u32(code) {
                    Some(ch) => result.push(ch),
                    None => {
                        return Err(format!("invalid code point (`{}`)", code)
                            .into_value(ctx)
                            .into())
                    }
                }
            }

            stack.replace(ctx, result);
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

            let Some((start, end, captures)) = find(s, pattern, init.map(|i| i as isize), plain)
                .map_err(|err| {
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

            let Some(captures) = r#match(s, pattern, init.map(|i| i as isize)).map_err(|err| {
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

            let iter = gmatch(s, pattern).map_err(|err| {
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
                Value::String(repl) => gsub(
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
                            key.to_str_lossy().into_owned(),
                            value.to_str_lossy().into_owned(),
                        );
                    }
                    gsub(s, pattern, lsonar::Repl::Table(&map), n.map(|n| n as usize)).map_err(
                        |err| {
                            let err = err.to_string();
                            err.into_value(ctx)
                        },
                    )?
                }
                Value::Function(_) => {
                    // TODO: implement this
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

    string.set_field(
        ctx,
        "pack",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            enum Endianness {
                Little,
                Big,
                Native,
            }

            impl Default for Endianness {
                fn default() -> Self {
                    Endianness::Native
                }
            }

            #[derive(Debug, Clone, Copy)]
            struct FormatState {
                endianness: Endianness,
                max_alignment: usize,
            }

            impl Default for FormatState {
                fn default() -> Self {
                    FormatState {
                        endianness: Endianness::default(),
                        max_alignment: 1,
                    }
                }
            }

            fn check_pack_arg(ctx: Context, stack_len: usize, index: usize, op: char) -> Result<(), Error> {
                if index >= stack_len {
                    Err(format!(
                        "missing argument for format '{}'",
                        op
                    ).into_value(ctx).into())
                } else {
                    Ok(())
                }
            }

            fn parse_optional_int(
                chars: &mut std::iter::Peekable<std::str::Chars>,
                max_val: usize,
            ) -> Result<Option<usize>, std::string::String> {
                let mut n_str = std::string::String::new();
                while let Some(c) = chars.peek() {
                    if c.is_ascii_digit() {
                        n_str.push(*c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                if n_str.is_empty() {
                    Ok(None)
                } else {
                    let n = n_str.parse::<usize>().map_err(|_| {
                        format!(
                            "invalid number '{}' in format string",
                            n_str
                        )
                    })?;
                    if n == 0 || n > max_val {
                        Err(format!(
                            "number '{}' out of range [1, {}]",
                            n, max_val
                        ))
                    } else {
                        Ok(Some(n))
                    }
                }
            }

            fn calculate_padding(
                current_pos: usize,
                data_size: usize,
                max_alignment: usize,
            ) -> usize {
                if max_alignment == 0 || data_size == 0 {
                    return 0;
                }

                let alignment = std::cmp::min(data_size, max_alignment);
                if alignment == 0 {
                    return 0;
                }
                (alignment - (current_pos % alignment)) % alignment
            }

            fn write_padding(
                writer: &mut impl Write,
                padding: usize,
            ) -> Result<(), std::io::Error> {
                for _ in 0..padding {
                    writer.write_all(&[0])?
                }
                Ok(())
            }

            fn write_int_n<W: Write>(
                writer: &mut W,
                value: i64,
                size: usize,
                state: &FormatState,
            ) -> Result<(), std::string::String> {
                 if !(1..=16).contains(&size) {
                     return Err("integer size must be between 1 and 16".to_string());
                 }

                 let min_val = -(1i128 << (size * 8 - 1));
                 let max_val = (1i128 << (size * 8 - 1)) - 1;

                 if (value as i128) < min_val || (value as i128) > max_val {
                     return Err(format!(
                         "integer {} does not fit in {} signed bytes",
                         value, size
                     ));
                 }

                 let mut bytes = [0u8; 16];
                 let src_bytes = value.to_ne_bytes();

                 bytes[..8].copy_from_slice(&src_bytes);

                 if size > 8 {
                     let sign_byte = if value < 0 { 0xff } else { 0x00 };
                     for i in 8..size {
                         bytes[i] = sign_byte;
                     }
                 }

                 write_bytes_endian(writer, &bytes[..size], state.endianness)
                     .map_err(|e| e.to_string())
             }

            fn write_uint_n<W: Write>(
                writer: &mut W,
                value: i64,
                size: usize,
                state: &FormatState,
            ) -> Result<(), std::string::String> {
                if !(1..=16).contains(&size) {
                     return Err("integer size must be between 1 and 16".to_string());
                 }
                 if value < 0 {
                     return Err(format!("negative value {} provided for unsigned format", value));
                 }
                 let u_value = value as u64;

                 let max_val = if size == 16 { u128::MAX } else { (1u128 << (size * 8)) - 1 };

                 if (u_value as u128) > max_val {
                     return Err(format!(
                         "unsigned integer {} does not fit in {} bytes",
                         u_value, size
                     ));
                 }

                 let mut bytes = [0u8; 16];
                 let src_bytes = u_value.to_ne_bytes();

                 bytes[..8].copy_from_slice(&src_bytes);

                 write_bytes_endian(writer, &bytes[..size], state.endianness)
                     .map_err(|e| e.to_string())
             }

            fn write_bytes_endian<W: Write>(
                writer: &mut W,
                bytes_to_write: &[u8],
                endianness: Endianness,
            ) -> io::Result<()> {
                 match endianness {
                    Endianness::Little => {
                        if cfg!(target_endian = "little") {
                            writer.write_all(bytes_to_write)
                        } else {
                            writer.write_all(&bytes_to_write.iter().rev().copied().collect::<Vec<_>>())
                        }
                    }
                    Endianness::Big => {
                         if cfg!(target_endian = "big") {
                             writer.write_all(bytes_to_write)
                         } else {
                             writer.write_all(&bytes_to_write.iter().rev().copied().collect::<Vec<_>>())
                         }
                     }
                    Endianness::Native => writer.write_all(bytes_to_write),
                 }
            }

            fn write_float<W: Write>(
                writer: &mut W,
                value: f32,
                state: &FormatState,
            ) -> io::Result<()> {
                let bytes = match state.endianness {
                    Endianness::Little => value.to_le_bytes(),
                    Endianness::Big => value.to_be_bytes(),
                    Endianness::Native => value.to_ne_bytes(),
                };
                writer.write_all(&bytes)
            }

            fn write_double<W: Write>(
                writer: &mut W,
                value: f64,
                state: &FormatState,
            ) -> io::Result<()> {
                let bytes = match state.endianness {
                    Endianness::Little => value.to_le_bytes(),
                    Endianness::Big => value.to_be_bytes(),
                    Endianness::Native => value.to_ne_bytes(),
                };
                writer.write_all(&bytes)
            }

            fn get_format_size(format_char: char, num_opt: Option<usize>) -> Option<usize> {
                match format_char {
                    'b' | 'B' | 'x' => Some(1),
                    'h' | 'H' => Some(mem::size_of::<i16>()),
                    'l' | 'L' => Some(mem::size_of::<i64>()),
                    'j' => Some(mem::size_of::<i64>()),
                    'J' => Some(mem::size_of::<u64>()),
                    'T' => Some(mem::size_of::<usize>()),
                    'i' | 'I' => num_opt.or(Some(mem::size_of::<i32>())),
                    'f' => Some(mem::size_of::<f32>()),
                    'd' | 'n' => Some(mem::size_of::<f64>()),
                    'c' => num_opt,
                    'z' => None,
                    's' => None,
                    _ => None,
                }
            }

            let fmt = stack.get(0);
            let fmt = match fmt {
                Value::String(s) => s.to_str()?,
                _ => return Err("`fmt` must be a `string`".into_value(ctx).into()),
            };

            let mut state = FormatState::default();
            let mut writer = Cursor::new(Vec::<u8>::new());
            let mut current_arg_idx = 1;

            let mut chars = fmt.chars().peekable();

            while let Some(format_char) = chars.next() {
                let current_pos = writer.position() as usize;
                 let num_opt = parse_optional_int(&mut chars, 16).map_err(|err| Error::from_value(err.into_value(ctx)))?;

                match format_char {
                    '<' => state.endianness = Endianness::Little,
                    '>' => state.endianness = Endianness::Big,
                    '=' => state.endianness = Endianness::Native,
                    '!' => {
                        let n = num_opt.ok_or_else(||
                            Error::from_value("missing number for '!' option".into_value(ctx))
                        )?;
                        if n < 1 || n > 16 || (n & (n - 1)) != 0 {
                            return Err(format!(
                               "alignment option '!' requires a power of 2 between 1 and 16 (got {})",
                               n
                           ).into_value(ctx).into());
                       }
                        state.max_alignment = n;
                    }
                    ' ' => {}
                    'x' => {
                        write_padding(&mut writer, 1)?;
                    }
                    'X' => {
                         let mut chars_peek = chars.clone();
                         let align_char = chars_peek.next().ok_or_else(||
                             Error::from_value("'X' must be followed by an option character".into_value(ctx))
                         )?;
                         let align_num_opt = parse_optional_int(&mut chars_peek, 16).map_err(|err| Into::<Error>::into(err.into_value(ctx)))?;

                         let data_size_for_align = match align_char {
                            's' => {
                                let len_size = align_num_opt.unwrap_or(mem::size_of::<usize>());
                                if !(1..=16).contains(&len_size) {
                                    return Err(Error::from_value("size for 's' in X must be 1-16".into_value(ctx)));
                                }
                                Some(len_size)
                            },
                            'c' | 'z' => Some(0),
                            _ => get_format_size(align_char, align_num_opt)
                         };

                         let data_size = data_size_for_align.ok_or_else(|| {
                             Error::from_value(format!("invalid option '{}' following 'X'", align_char).into_value(ctx))
                         })?;

                         let padding = calculate_padding(current_pos, data_size, state.max_alignment);
                         write_padding(&mut writer, padding)?;
                    }
                    op @ ('b' | 'B' | 'h' | 'H' | 'l' | 'L' | 'j' | 'J' | 'T' | 'i' | 'I' | 'f' | 'd' | 'n') => {
                        check_pack_arg(ctx, stack.len(), current_arg_idx, op)?;
                        let arg_val = stack.get(current_arg_idx);
                        current_arg_idx += 1;

                         let data_size = get_format_size(op, num_opt).unwrap();
                         let padding = calculate_padding(current_pos, data_size, state.max_alignment);
                         write_padding(&mut writer, padding)?;

                         match op {
                             'b' => {
                                 let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                 )?;
                                 write_int_n(&mut writer, val, 1, &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'B' => {
                                 let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                 )?;
                                 if val < 0 {
                                      return Err(Error::from_value(format!(
                                        "negative value {} provided for unsigned format '{}'",
                                        val, op
                                    ).into_value(ctx)));
                                 }
                                  write_uint_n(&mut writer, val, 1, &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'h' => {
                                let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                write_int_n(&mut writer, val, mem::size_of::<i16>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'H' => {
                                let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                if val < 0 {
                                      return Err(Error::from_value(format!(
                                        "negative value {} provided for unsigned format '{}'",
                                        val, op
                                    ).into_value(ctx)));
                                }
                                write_uint_n(&mut writer, val, mem::size_of::<u16>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'l' => {
                                let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                write_int_n(&mut writer, val, mem::size_of::<i64>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                              }
                             'L' => {
                                let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                if val < 0 {
                                      return Err(Error::from_value(format!(
                                        "negative value {} provided for unsigned format '{}'",
                                        val, op
                                    ).into_value(ctx)));
                                }
                                write_uint_n(&mut writer, val, mem::size_of::<u64>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'j' => {
                                let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                write_int_n(&mut writer, val, mem::size_of::<i64>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'J' => {
                                let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                if val < 0 {
                                      return Err(Error::from_value(format!(
                                        "negative value {} provided for unsigned format '{}'",
                                        val, op
                                    ).into_value(ctx)));
                                }
                                write_uint_n(&mut writer, val, mem::size_of::<u64>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'T' => {
                                let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                if val < 0 {
                                      return Err(Error::from_value(format!(
                                        "negative value {} provided for unsigned format '{}'",
                                        val, op
                                    ).into_value(ctx)));
                                }
                                write_uint_n(&mut writer, val, mem::size_of::<usize>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'i' => {
                                 let size = num_opt.unwrap_or(mem::size_of::<i32>());
                                 let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                 )?;
                                 write_int_n(&mut writer, val, size, &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                              }
                             'I' => {
                                 let size = num_opt.unwrap_or(mem::size_of::<u32>());
                                 let val = arg_val.to_integer().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                 )?;
                                 if val < 0 {
                                      return Err(Error::from_value(format!(
                                        "negative value {} provided for unsigned format '{}'",
                                        val, op
                                    ).into_value(ctx)));
                                 }
                                 write_uint_n(&mut writer, val, size, &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                              'f' => {
                                 let val = arg_val.to_number().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be a `number`", op).into_value(ctx))
                                 )?;
                                 write_float(&mut writer, val as f32, &state).map_err(|e| Error::from_value(e.to_string().into_value(ctx)))?;
                              }
                              'd' => {
                                 let val = arg_val.to_number().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be a `number`", op).into_value(ctx))
                                 )?;
                                 write_double(&mut writer, val, &state).map_err(|e| Error::from_value(e.to_string().into_value(ctx)))?;
                             }
                              'n' => {
                                 let val = arg_val.to_number().ok_or_else(||
                                    Error::from_value(format!("argument for format '{}' must be a `number`", op).into_value(ctx))
                                 )?;
                                 write_double(&mut writer, val, &state).map_err(|e| Error::from_value(e.to_string().into_value(ctx)))?;
                             }
                             _ => unreachable!(),
                         }

                    }
                    'c' => {
                        let n = num_opt.ok_or_else(||
                            Into::<Error>::into("missing number for 'c' option".into_value(ctx))
                        )?;
                        check_pack_arg(ctx, stack.len(), current_arg_idx, 'c')?;
                        let arg_val = stack.get(current_arg_idx);
                        current_arg_idx += 1;

                        let s = arg_val.into_string(ctx).ok_or_else(||
                            Error::from_value("argument for format 'c' must be a `string`".into_value(ctx))
                        )?;
                        let bytes = s.as_bytes();

                        if bytes.len() >= n {
                            writer.write_all(&bytes[..n])?
                        } else {
                            writer.write_all(bytes)?;
                            write_padding(&mut writer, n - bytes.len())?;
                        }
                    }
                     'z' => {
                        check_pack_arg(ctx, stack.len(), current_arg_idx, 'z')?;
                        let arg_val = stack.get(current_arg_idx);
                         current_arg_idx += 1;

                         let s = arg_val.into_string(ctx).ok_or_else(||
                            Error::from_value("argument for format 'z' must be a `string`".into_value(ctx))
                        )?;
                        let bytes = s.as_bytes();

                         writer.write_all(bytes)?;
                         writer.write_all(&[0])?
                    }
                    's' => {
                         let len_size = num_opt.unwrap_or(mem::size_of::<usize>());
                          if len_size < 1 || len_size > 16 {
                             return Err(
                                 Error::from_value("string length size must be between 1 and 16 bytes".into_value(ctx))
                             );
                         }

                        check_pack_arg(ctx, stack.len(), current_arg_idx, 's')?;
                        let arg_val = stack.get(current_arg_idx);
                         current_arg_idx += 1;

                         let s = arg_val.into_string(ctx).ok_or_else(||
                            Error::from_value("argument for format 's' must be a `string`".into_value(ctx))
                        )?;
                         let bytes = s.as_bytes();
                         let str_len = bytes.len() as u64;

                         let padding = calculate_padding(current_pos, len_size, state.max_alignment);
                         write_padding(&mut writer, padding)?;
                         let str_len_i64 = i64::try_from(str_len).map_err(|_| Error::from_value("string length too large to represent as i64".into_value(ctx)))?;
                         write_uint_n(&mut writer, str_len_i64, len_size, &state)
                             .map_err(|err| Error::from_value(err.into_value(ctx)))?;

                         writer.write_all(bytes)?
                    }
                    invalid => {
                        return Err(Error::from_value(format!(
                            "invalid conversion option '{}' in format string",
                            invalid
                        ).into_value(ctx)));
                    }
                }
            }

            let packed_bytes = writer.into_inner();
            stack.replace(ctx, ctx.intern(&packed_bytes));

            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "unpack",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            enum Endianness {
                Little,
                Big,
                Native,
            }

            impl Default for Endianness {
                fn default() -> Self {
                    Endianness::Native
                }
            }

            #[derive(Debug, Clone, Copy)]
            struct FormatState {
                endianness: Endianness,
                max_alignment: usize,
            }

            impl Default for FormatState {
                fn default() -> Self {
                    FormatState {
                        endianness: Endianness::default(),
                        max_alignment: 1,
                    }
                }
            }

             fn parse_optional_int(
                chars: &mut std::iter::Peekable<std::str::Chars>,
                max_val: usize,
            ) -> Result<Option<usize>, std::string::String> {
                let mut n_str = std::string::String::new();
                while let Some(c) = chars.peek() {
                    if c.is_ascii_digit() {
                        n_str.push(*c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                if n_str.is_empty() {
                    Ok(None)
                } else {
                    let n = n_str.parse::<usize>().map_err(|_| {
                        format!(
                            "invalid number '{}' in format string",
                            n_str
                        )
                    })?;
                    if n == 0 || n > max_val {
                        Err(format!(
                            "number '{}' out of range [1, {}]",
                            n, max_val
                        ))
                    } else {
                        Ok(Some(n))
                    }
                }
            }

            fn calculate_padding(
                current_pos: usize,
                data_size: usize,
                max_alignment: usize,
            ) -> usize {
                if max_alignment == 0 || data_size == 0 {
                    return 0;
                }

                let alignment = std::cmp::min(data_size, max_alignment);
                 if alignment == 0 || !alignment.is_power_of_two() {
                    return 0;
                }

                (alignment - (current_pos % alignment)) % alignment
            }

            fn get_format_size(format_char: char, num_opt: Option<usize>) -> Option<usize> {
                match format_char {
                    'b' | 'B' | 'x' => Some(1),
                    'h' | 'H' => Some(mem::size_of::<i16>()),
                    'l' | 'L' => Some(mem::size_of::<i64>()),
                    'j' => Some(mem::size_of::<i64>()),
                    'J' => Some(mem::size_of::<u64>()),
                    'T' => Some(mem::size_of::<usize>()),
                    'i' | 'I' => num_opt.or(Some(mem::size_of::<i32>())),
                    'f' => Some(mem::size_of::<f32>()),
                    'd' | 'n' => Some(mem::size_of::<f64>()),
                    'c' => num_opt,
                    'z' => None,
                    's' => None,
                    _ => None,
                }
            }

            fn read_exact_bytes<'a, 'gc>(reader: &mut Cursor<&'a [u8]>, count: usize, op: char, ctx: Context<'gc>) -> Result<&'a [u8], Error<'gc>> {
                let start = reader.position() as usize;
                if start + count > reader.get_ref().len() {
                     return Err(Error::from_value(format!("data string too short for format '{}'", op).into_value(ctx)));
                }
                reader.set_position( (start + count) as u64);
                Ok(&reader.get_ref()[start..start+count])
             }

             fn read_padding<'gc>(reader: &mut Cursor<&[u8]>, padding: usize, ctx: Context<'gc>) -> Result<(), Error<'gc>> {
                if padding > 0 {
                     read_exact_bytes(reader, padding, 'X', ctx)?;
                 }
                 Ok(())
             }

            fn read_int_n<'gc>(
                reader: &mut Cursor<&[u8]>,
                size: usize,
                state: &FormatState,
                op: char,
                ctx: Context<'gc>,
            ) -> Result<i128, Error<'gc>> {
                if !(1..=16).contains(&size) {
                    return Err(Error::from_value("integer size must be between 1 and 16".into_value(ctx)));
                }
                let read_bytes = read_exact_bytes(reader, size, op, ctx)?;
                let mut bytes = [0u8; 16];

                match state.endianness {
                    Endianness::Little => {
                        if cfg!(target_endian = "little") {
                            bytes[..size].copy_from_slice(read_bytes);
                        } else {
                            // Target is big-endian, read little-endian bytes. Reverse into beginning.
                            for (i, byte) in read_bytes.iter().rev().enumerate() {
                                if i < size { bytes[i] = *byte; }
                            }
                        }
                    }
                    Endianness::Big => {
                         if cfg!(target_endian = "big") {
                            bytes[..size].copy_from_slice(read_bytes);
                         } else {
                             // Target is little-endian, read big-endian bytes. Reverse into beginning.
                             for (i, byte) in read_bytes.iter().rev().enumerate() {
                                 if i < size { bytes[i] = *byte; }
                             }
                         }
                     }
                    Endianness::Native => {
                         // Copy directly regardless of target endianness, conversion uses from_ne_bytes
                         bytes[..size].copy_from_slice(read_bytes);
                    }
                }

                let mut value = i128::from_ne_bytes(bytes);

                if size < 16 {
                     let shift = 128 - (size * 8);
                     value = (value << shift) >> shift;
                 }

                Ok(value)
            }

            fn read_uint_n<'gc>(
                reader: &mut Cursor<&[u8]>,
                size: usize,
                state: &FormatState,
                op: char,
                ctx: Context<'gc>,
            ) -> Result<u128, Error<'gc>> {
                 if !(1..=16).contains(&size) {
                     return Err(Error::from_value("integer size must be between 1 and 16".into_value(ctx)));
                 }
                let read_bytes = read_exact_bytes(reader, size, op, ctx)?;
                let mut bytes = [0u8; 16];

                match state.endianness {
                    Endianness::Little => {
                        if cfg!(target_endian = "little") {
                            bytes[..size].copy_from_slice(read_bytes);
                        } else {
                            // Target is big-endian, read little-endian bytes. Reverse into beginning.
                             for (i, byte) in read_bytes.iter().rev().enumerate() {
                                 if i < size { bytes[i] = *byte; }
                            }
                        }
                    }
                     Endianness::Big => {
                         if cfg!(target_endian = "big") {
                             bytes[..size].copy_from_slice(read_bytes);
                         } else {
                             // Target is little-endian, read big-endian bytes. Reverse into beginning.
                             for (i, byte) in read_bytes.iter().rev().enumerate() {
                                 if i < size { bytes[i] = *byte; }
                             }
                         }
                     }
                     Endianness::Native => {
                         // Copy directly regardless of target endianness, conversion uses from_ne_bytes
                          bytes[..size].copy_from_slice(read_bytes);
                     }
                 }

                let value = u128::from_ne_bytes(bytes);

                Ok(value)
            }

            fn read_float<'gc>(reader: &mut Cursor<&[u8]>, state: &FormatState, ctx: Context<'gc>) -> Result<f32, Error<'gc>> {
                let bytes = read_exact_bytes(reader, mem::size_of::<f32>(), 'f', ctx)?;
                let arr: [u8; 4] = bytes.try_into().map_err(|_| Error::from_value("internal error: float size mismatch".into_value(ctx)))?;
                Ok(match state.endianness {
                    Endianness::Little => f32::from_le_bytes(arr),
                    Endianness::Big => f32::from_be_bytes(arr),
                    Endianness::Native => f32::from_ne_bytes(arr),
                })
            }

            fn read_double<'gc>(reader: &mut Cursor<&[u8]>, state: &FormatState, op: char, ctx: Context<'gc>) -> Result<f64, Error<'gc>> {
                let bytes = read_exact_bytes(reader, mem::size_of::<f64>(), op, ctx)?;
                let arr: [u8; 8] = bytes.try_into().map_err(|_| Error::from_value("internal error: double size mismatch".into_value(ctx)))?;
                Ok(match state.endianness {
                    Endianness::Little => f64::from_le_bytes(arr),
                    Endianness::Big => f64::from_be_bytes(arr),
                    Endianness::Native => f64::from_ne_bytes(arr),
                })
            }

            let (fmt, s, init) = stack.consume::<(String, String, Option<i64>)>(ctx)?;

            let fmt = fmt.to_str()?;
            let bytes = s.as_bytes();
            let init = init.unwrap_or(1);

            let start_pos = if init >= 0 {
                init.saturating_sub(1) as usize
            } else {
                bytes.len().saturating_sub(init.unsigned_abs() as usize)
            };

            if start_pos > bytes.len() {
                return Err("initial position out of string bounds".into_value(ctx).into());
            }

            let mut reader = Cursor::new(bytes);
            reader.set_position(start_pos as u64);

            let mut state = FormatState::default();
            let mut results: Vec<Value> = Vec::new();
            let mut chars = fmt.chars().peekable();

            while let Some(format_char) = chars.next() {
                 let initial_read_pos = reader.position() as usize;
                 let num_opt = parse_optional_int(&mut chars, 16).map_err(|err| Error::from_value(err.into_value(ctx)))?;

                match format_char {
                    '<' => state.endianness = Endianness::Little,
                    '>' => state.endianness = Endianness::Big,
                    '=' => state.endianness = Endianness::Native,
                    '!' => {
                        let n = num_opt.ok_or_else(||
                             Error::from_value("missing number for '!' option".into_value(ctx))
                        )?;
                        if !n.is_power_of_two() || n > 16 {
                            return Err(format!(
                                "alignment option '!' requires a power of 2 between 1 and 16 (got {})",
                                n
                            ).into_value(ctx).into());
                        }
                        state.max_alignment = n;
                    }
                    ' ' => {}
                    'x' => {
                        read_exact_bytes(&mut reader, 1, 'x', ctx)?;
                    }
                    'X' => {
                        let mut chars_peek = chars.clone();
                        let align_char = chars_peek.next().ok_or_else(||
                             Error::from_value("'X' must be followed by an option character".into_value(ctx))
                         )?;
                         let align_num_opt = parse_optional_int(&mut chars_peek, 16).map_err(|err| Into::<Error>::into(err.into_value(ctx)))?;

                         let data_size_for_align = match align_char {
                            's' => {
                                let len_size = align_num_opt.unwrap_or(mem::size_of::<usize>());
                                if !(1..=16).contains(&len_size) {
                                    return Err(Error::from_value("size for 's' in X must be 1-16".into_value(ctx)));
                                }
                                Some(len_size)
                            },
                            'c' | 'z' => Some(0),
                            _ => get_format_size(align_char, align_num_opt)
                         };

                         let data_size = data_size_for_align.ok_or_else(|| {
                             Error::from_value(format!("invalid option '{}' following 'X'", align_char).into_value(ctx))
                         })?;

                         let padding = calculate_padding(initial_read_pos, data_size, state.max_alignment);
                         read_padding(&mut reader, padding, ctx)?;
                     }
                    op @ ('b' | 'B' | 'h' | 'H' | 'l' | 'L' | 'j' | 'J' | 'T' | 'i' | 'I' | 'f' | 'd' | 'n') => {
                        let data_size = get_format_size(op, num_opt).ok_or_else(|| {
                             // Should not happen for these options
                             Error::from_value(format!("internal error getting size for '{}'", op).into_value(ctx))
                         })?;

                        let padding = calculate_padding(initial_read_pos, data_size, state.max_alignment);
                        read_padding(&mut reader, padding, ctx)?;

                        let value = match op {
                            'b' => {
                                let val128 = read_int_n(&mut reader, 1, &state, op, ctx)?;
                                (val128 as i64).into_value(ctx)
                            },
                            'B' => {
                                let val128 = read_uint_n(&mut reader, 1, &state, op, ctx)?;
                                if val128 > i64::MAX as u128 {
                                    return Err(Error::from_value(format!(
                                        "unsigned value {} read for format '{}' does not fit in `integer`",
                                        val128, op
                                    ).into_value(ctx)));
                                }
                                (val128 as i64).into_value(ctx)
                            },
                            'h' => {
                                let size = mem::size_of::<i16>();
                                let val128 = read_int_n(&mut reader, size, &state, op, ctx)?;
                                (val128 as i64).into_value(ctx)
                             },
                            'H' => {
                                let size = mem::size_of::<u16>();
                                let val128 = read_uint_n(&mut reader, size, &state, op, ctx)?;
                                if val128 > i64::MAX as u128 {
                                    return Err(Error::from_value(format!(
                                        "unsigned value {} read for format '{}' does not fit in `integer`",
                                        val128, op
                                    ).into_value(ctx)));
                                }
                                (val128 as i64).into_value(ctx)
                            },
                            'l' => {
                                let size = mem::size_of::<i64>();
                                let val128 = read_int_n(&mut reader, size, &state, op, ctx)?;
                                (val128 as i64).into_value(ctx)
                            },
                            'L' => {
                                let size = mem::size_of::<u64>();
                                let val128 = read_uint_n(&mut reader, size, &state, op, ctx)?;
                                if val128 > i64::MAX as u128 {
                                     return Err(Error::from_value(format!(
                                        "unsigned value {} read for format '{}' does not fit in `integer`",
                                        val128, op
                                    ).into_value(ctx)));
                                }
                                (val128 as i64).into_value(ctx)
                             },
                            'j' => {
                                let size = mem::size_of::<i64>();
                                let val128 = read_int_n(&mut reader, size, &state, op, ctx)?;
                                (val128 as i64).into_value(ctx)
                             },
                            'J' => {
                                let size = mem::size_of::<u64>();
                                let val128 = read_uint_n(&mut reader, size, &state, op, ctx)?;
                                if val128 > i64::MAX as u128 {
                                     return Err(Error::from_value(format!(
                                        "unsigned value {} read for format '{}' does not fit in `integer`",
                                        val128, op
                                    ).into_value(ctx)));
                                }
                                (val128 as i64).into_value(ctx)
                             },
                            'T' => {
                                let size = mem::size_of::<usize>();
                                let val128 = read_uint_n(&mut reader, size, &state, op, ctx)?;
                                if val128 > i64::MAX as u128 {
                                     return Err(Error::from_value(format!(
                                        "unsigned value {} read for format '{}' does not fit in `integer`",
                                        val128, op
                                    ).into_value(ctx)));
                                }
                                (val128 as i64).into_value(ctx)
                             },
                            'i' => {
                                 let size = num_opt.unwrap_or(mem::size_of::<i32>());
                                 let val128 = read_int_n(&mut reader, size, &state, op, ctx)?;
                                 if val128 < i64::MIN as i128 || val128 > i64::MAX as i128 {
                                      return Err(Error::from_value(format!(
                                          "integer value {} read for format '{}' does not fit in `integer`",
                                         val128, op
                                     ).into_value(ctx)));
                                 }
                                 (val128 as i64).into_value(ctx)
                              },
                            'I' => {
                                 let size = num_opt.unwrap_or(mem::size_of::<u32>());
                                 let val128 = read_uint_n(&mut reader, size, &state, op, ctx)?;
                                 if val128 > i64::MAX as u128 {
                                      return Err(Error::from_value(format!(
                                          "unsigned value {} read for format '{}' does not fit in `integer`",
                                          val128, op
                                      ).into_value(ctx)));
                                 }
                                 (val128 as i64).into_value(ctx)
                              },
                            'f' => read_float(&mut reader, &state, ctx)?.into_value(ctx),
                            'd' => read_double(&mut reader, &state, op, ctx)?.into_value(ctx),
                            'n' => read_double(&mut reader, &state, op, ctx)?.into_value(ctx),
                            _ => unreachable!(),
                        };
                        results.push(value);
                    }
                    'c' => {
                        let n = num_opt.ok_or_else(||
                            Into::<Error>::into("missing number for 'c' option".into_value(ctx))
                        )?;
                        let bytes = read_exact_bytes(&mut reader, n, 'c', ctx)?;
                        results.push(ctx.intern(bytes).into_value(ctx));
                    }
                     'z' => {
                        let buffer = reader.get_ref();
                        let current_pos = reader.position() as usize;
                        let remaining_bytes = &buffer[current_pos..];
                        let null_pos = remaining_bytes.iter().position(|&b| b == 0);

                        match null_pos {
                            Some(pos) => {
                                let str_bytes = &remaining_bytes[..pos];
                                results.push(ctx.intern(str_bytes).into_value(ctx));
                                reader.set_position((current_pos + pos + 1) as u64);
                            }
                            None => {
                                 return Err(Error::from_value("missing null terminator for 'z' format".into_value(ctx)));
                            }
                        }
                     }
                    's' => {
                         let len_size = num_opt.unwrap_or(mem::size_of::<usize>());
                          if len_size < 1 || len_size > 16 {
                             return Err(
                                 Error::from_value("string length size must be between 1 and 16 bytes".into_value(ctx))
                             );
                         }

                         let padding = calculate_padding(initial_read_pos, len_size, state.max_alignment);
                         read_padding(&mut reader, padding, ctx)?;

                         let str_len_u128 = read_uint_n(&mut reader, len_size, &state, 's', ctx)?;
                         let str_len = usize::try_from(str_len_u128).map_err(|_| Error::from_value("string length too large for usize".into_value(ctx)))?;
                         if str_len_u128 > i64::MAX as u128 {
                             return Err(Error::from_value("string length value does not fit in `integer`".into_value(ctx)));
                         }

                         let str_bytes = read_exact_bytes(&mut reader, str_len, 's', ctx)?;
                         results.push(ctx.intern(str_bytes).into_value(ctx));
                    }
                    invalid => {
                         return Err(Error::from_value(format!(
                            "invalid conversion option '{}' in format string",
                            invalid
                        ).into_value(ctx)));
                    }
                }
            }

            stack.clear();
            stack.extend(results);
            stack.into_back(ctx, reader.position() as i64 + 1);

            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "packsize",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            enum Endianness {
                Little,
                Big,
                Native,
            }

            impl Default for Endianness {
                fn default() -> Self {
                    Endianness::Native
                }
            }

            #[derive(Debug, Clone, Copy)]
            struct FormatState {
                endianness: Endianness,
                max_alignment: usize,
            }

            impl Default for FormatState {
                fn default() -> Self {
                    FormatState {
                        endianness: Endianness::default(),
                        max_alignment: 1,
                    }
                }
            }

            fn parse_optional_int(
                chars: &mut std::iter::Peekable<std::str::Chars>,
                max_val: usize,
            ) -> Result<Option<usize>, std::string::String> {
                let mut n_str = std::string::String::new();
                while let Some(c) = chars.peek() {
                    if c.is_ascii_digit() {
                        n_str.push(*c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                if n_str.is_empty() {
                    Ok(None)
                } else {
                    let n = n_str.parse::<usize>().map_err(|_| {
                        format!(
                            "invalid number '{}' in format string",
                            n_str
                        )
                    })?;
                    if n == 0 || n > max_val {
                        Err(format!(
                            "number '{}' out of range [1, {}]",
                            n, max_val
                        ))
                    } else {
                        Ok(Some(n))
                    }
                }
            }

            fn calculate_padding(
                current_pos: usize,
                data_size: usize,
                max_alignment: usize,
            ) -> usize {
                if max_alignment == 0 || data_size == 0 {
                    return 0;
                }
                let alignment = std::cmp::min(data_size, max_alignment);
                 if alignment == 0 || !alignment.is_power_of_two() {
                    return 0;
                 }
                (alignment - (current_pos % alignment)) % alignment
            }

            fn get_format_size(format_char: char, num_opt: Option<usize>) -> Option<usize> {
                match format_char {
                    'b' | 'B' | 'x' => Some(1),
                    'h' | 'H' => Some(mem::size_of::<i16>()),
                    'l' | 'L' => Some(mem::size_of::<i64>()),
                    'j' => Some(mem::size_of::<i64>()),
                    'J' => Some(mem::size_of::<u64>()),
                    'T' => Some(mem::size_of::<usize>()),
                    'i' | 'I' => num_opt.or(Some(mem::size_of::<i32>())),
                    'f' => Some(mem::size_of::<f32>()),
                    'd' | 'n' => Some(mem::size_of::<f64>()),
                    'c' => num_opt,
                    'z' => None,
                    's' => num_opt.or(Some(mem::size_of::<usize>())),
                    _ => None,
                }
            }

            let fmt = stack.consume::<String>(ctx)?;
            let fmt = fmt.to_str()?;

            let mut state = FormatState::default();
            let mut total_size: usize = 0;
            let mut current_offset: usize = 0;
            let mut chars = fmt.chars().peekable();

            while let Some(format_char) = chars.next() {
                 let num_opt = parse_optional_int(&mut chars, 16).map_err(|err| Error::from_value(err.into_value(ctx)))?;

                match format_char {
                    '<' => state.endianness = Endianness::Little,
                    '>' => state.endianness = Endianness::Big,
                    '=' => state.endianness = Endianness::Native,
                    '!' => {
                        let n = num_opt.ok_or_else(||
                             Error::from_value("missing number for '!' option".into_value(ctx))
                        )?;
                        if !n.is_power_of_two() || n > 16 {
                            return Err(format!(
                                "alignment option '!' requires a power of 2 between 1 and 16 (got {})",
                                n
                            ).into_value(ctx).into());
                        }
                        state.max_alignment = n;
                    }
                    ' ' => {}
                    'x' => {
                        total_size += 1;
                        current_offset += 1;
                    }
                    'X' => {
                        let mut chars_peek = chars.clone();
                        let align_char = chars_peek.next().ok_or_else(||
                             Error::from_value("'X' must be followed by an option character".into_value(ctx))
                         )?;
                         let align_num_opt = parse_optional_int(&mut chars_peek, 16).map_err(|err| Into::<Error>::into(err.into_value(ctx)))?;

                         let data_size_for_align = match align_char {
                            's' => {
                                let len_size = align_num_opt.unwrap_or(mem::size_of::<usize>());
                                if !(1..=16).contains(&len_size) {
                                    return Err(Error::from_value("size for 's' in X must be 1-16".into_value(ctx)));
                                }
                                Some(len_size)
                            },
                            'c' | 'z' => Some(0),
                            _ => get_format_size(align_char, align_num_opt)
                         };

                         let data_size = data_size_for_align.ok_or_else(|| {
                             Error::from_value(format!("invalid option '{}' following 'X'", align_char).into_value(ctx))
                         })?;

                         let padding = calculate_padding(current_offset, data_size, state.max_alignment);
                         total_size += padding;
                         current_offset += padding;
                    }
                    op @ ('b' | 'B' | 'h' | 'H' | 'l' | 'L' | 'j' | 'J' | 'T' | 'i' | 'I' | 'f' | 'd' | 'n') => {
                        let data_size = get_format_size(op, num_opt).ok_or_else(|| {
                             Error::from_value(format!("internal error getting size for '{}'", op).into_value(ctx))
                         })?;

                        let padding = calculate_padding(current_offset, data_size, state.max_alignment);
                        total_size += padding + data_size;
                        current_offset += padding + data_size;
                    }
                    'c' => {
                        let n = num_opt.ok_or_else(||
                            Into::<Error>::into("missing number for 'c' option".into_value(ctx))
                        )?;
                        total_size += n;
                        current_offset += n;
                    }
                     'z' => {
                         return Err(Error::from_value("variable-length format ('z')".into_value(ctx)));
                    }
                    's' => {
                         let len_size = num_opt.unwrap_or(mem::size_of::<usize>());
                          if len_size < 1 || len_size > 16 {
                             return Err(
                                 Error::from_value("string length size must be between 1 and 16 bytes".into_value(ctx))
                             );
                         }
                         let padding = calculate_padding(current_offset, len_size, state.max_alignment);
                         total_size += padding + len_size;
                         current_offset += padding + len_size;
                    }
                    invalid => {
                         return Err(Error::from_value(format!(
                            "invalid conversion option '{}' in format string",
                            invalid
                        ).into_value(ctx)));
                    }
                }
            }

            stack.replace(ctx, total_size as i64);
            Ok(CallbackReturn::Return)
        }),
    );

    ctx.set_global("string", string);
}
