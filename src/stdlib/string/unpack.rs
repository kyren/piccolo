use std::{io::Cursor, mem};

use crate::{Context, Error, IntoValue, Value};

use super::{calculate_padding, get_format_size, parse_optional_int, Endianness, FormatState};

pub fn process<'gc>(
    fmt: &str,
    bytes: &[u8],
    start_pos: u64,
    ctx: Context<'gc>,
) -> Result<(Vec<Value<'gc>>, u64), Error<'gc>> {
    let mut reader = Cursor::new(bytes);
    reader.set_position(start_pos as u64);

    let mut state = FormatState::default();
    let mut values: Vec<Value> = Vec::new();
    let mut chars = fmt.chars().peekable();

    while let Some(format_char) = chars.next() {
        let initial_read_pos = reader.position() as usize;
        let num_opt = parse_optional_int(&mut chars, 16)
            .map_err(|err| Error::from_value(err.into_value(ctx)))?;

        match format_char {
            '<' => state.endianness = Endianness::Little,
            '>' => state.endianness = Endianness::Big,
            '=' => state.endianness = Endianness::Native,
            '!' => {
                let n = num_opt.ok_or_else(|| {
                    Error::from_value("missing number for '!' option".into_value(ctx))
                })?;
                if !n.is_power_of_two() || n > 16 {
                    return Err(format!(
                        "alignment option '!' requires a power of 2 between 1 and 16 (got {})",
                        n
                    )
                    .into_value(ctx)
                    .into());
                }
                state.max_alignment = n;
            }
            ' ' => {}
            'x' => {
                read_exact_bytes(&mut reader, 1, 'x', ctx)?;
            }
            'X' => {
                let mut chars_peek = chars.clone();
                let align_char = chars_peek.next().ok_or_else(|| {
                    Error::from_value("'X' must be followed by an option character".into_value(ctx))
                })?;
                let align_num_opt = parse_optional_int(&mut chars_peek, 16)
                    .map_err(|err| Into::<Error>::into(err.into_value(ctx)))?;

                let data_size_for_align = match align_char {
                    's' => {
                        let len_size = align_num_opt.unwrap_or(mem::size_of::<usize>());
                        if !(1..=16).contains(&len_size) {
                            return Err(Error::from_value(
                                "size for 's' in X must be 1-16".into_value(ctx),
                            ));
                        }
                        Some(len_size)
                    }
                    'c' | 'z' => Some(0),
                    _ => get_format_size(align_char, align_num_opt),
                };

                let data_size = data_size_for_align.ok_or_else(|| {
                    Error::from_value(
                        format!("invalid option '{}' following 'X'", align_char).into_value(ctx),
                    )
                })?;

                let padding = calculate_padding(initial_read_pos, data_size, state.max_alignment);
                read_padding(&mut reader, padding, ctx)?;
            }
            op @ ('b' | 'B' | 'h' | 'H' | 'l' | 'L' | 'j' | 'J' | 'T' | 'i' | 'I' | 'f' | 'd'
            | 'n') => {
                let data_size = get_format_size(op, num_opt).ok_or_else(|| {
                    // Should not happen for these options
                    Error::from_value(
                        format!("internal error getting size for '{}'", op).into_value(ctx),
                    )
                })?;

                let padding = calculate_padding(initial_read_pos, data_size, state.max_alignment);
                read_padding(&mut reader, padding, ctx)?;

                let value = match op {
                    'b' => {
                        let val128 = read_int_n(&mut reader, 1, &state, op, ctx)?;
                        (val128 as i64).into_value(ctx)
                    }
                    'B' => {
                        let val128 = read_uint_n(&mut reader, 1, &state, op, ctx)?;
                        if val128 > i64::MAX as u128 {
                            return Err(Error::from_value(
                                format!(
                                "unsigned value {} read for format '{}' does not fit in `integer`",
                                val128, op
                            )
                                .into_value(ctx),
                            ));
                        }
                        (val128 as i64).into_value(ctx)
                    }
                    'h' => {
                        let size = mem::size_of::<i16>();
                        let val128 = read_int_n(&mut reader, size, &state, op, ctx)?;
                        (val128 as i64).into_value(ctx)
                    }
                    'H' => {
                        let size = mem::size_of::<u16>();
                        let val128 = read_uint_n(&mut reader, size, &state, op, ctx)?;
                        if val128 > i64::MAX as u128 {
                            return Err(Error::from_value(
                                format!(
                                "unsigned value {} read for format '{}' does not fit in `integer`",
                                val128, op
                            )
                                .into_value(ctx),
                            ));
                        }
                        (val128 as i64).into_value(ctx)
                    }
                    'l' => {
                        let size = mem::size_of::<i64>();
                        let val128 = read_int_n(&mut reader, size, &state, op, ctx)?;
                        (val128 as i64).into_value(ctx)
                    }
                    'L' => {
                        let size = mem::size_of::<u64>();
                        let val128 = read_uint_n(&mut reader, size, &state, op, ctx)?;
                        if val128 > i64::MAX as u128 {
                            return Err(Error::from_value(
                                format!(
                                "unsigned value {} read for format '{}' does not fit in `integer`",
                                val128, op
                            )
                                .into_value(ctx),
                            ));
                        }
                        (val128 as i64).into_value(ctx)
                    }
                    'j' => {
                        let size = mem::size_of::<i64>();
                        let val128 = read_int_n(&mut reader, size, &state, op, ctx)?;
                        (val128 as i64).into_value(ctx)
                    }
                    'J' => {
                        let size = mem::size_of::<u64>();
                        let val128 = read_uint_n(&mut reader, size, &state, op, ctx)?;
                        if val128 > i64::MAX as u128 {
                            return Err(Error::from_value(
                                format!(
                                "unsigned value {} read for format '{}' does not fit in `integer`",
                                val128, op
                            )
                                .into_value(ctx),
                            ));
                        }
                        (val128 as i64).into_value(ctx)
                    }
                    'T' => {
                        let size = mem::size_of::<usize>();
                        let val128 = read_uint_n(&mut reader, size, &state, op, ctx)?;
                        if val128 > i64::MAX as u128 {
                            return Err(Error::from_value(
                                format!(
                                "unsigned value {} read for format '{}' does not fit in `integer`",
                                val128, op
                            )
                                .into_value(ctx),
                            ));
                        }
                        (val128 as i64).into_value(ctx)
                    }
                    'i' => {
                        let size = num_opt.unwrap_or(mem::size_of::<i32>());
                        let val128 = read_int_n(&mut reader, size, &state, op, ctx)?;
                        if val128 < i64::MIN as i128 || val128 > i64::MAX as i128 {
                            return Err(Error::from_value(
                                format!(
                                  "integer value {} read for format '{}' does not fit in `integer`",
                                 val128, op
                             )
                                .into_value(ctx),
                            ));
                        }
                        (val128 as i64).into_value(ctx)
                    }
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
                    }
                    'f' => read_float(&mut reader, &state, ctx)?.into_value(ctx),
                    'd' => read_double(&mut reader, &state, op, ctx)?.into_value(ctx),
                    'n' => read_double(&mut reader, &state, op, ctx)?.into_value(ctx),
                    _ => unreachable!(),
                };
                values.push(value);
            }
            'c' => {
                let n = num_opt.ok_or_else(|| {
                    Into::<Error>::into("missing number for 'c' option".into_value(ctx))
                })?;
                let bytes = read_exact_bytes(&mut reader, n, 'c', ctx)?;
                values.push(ctx.intern(bytes).into_value(ctx));
            }
            'z' => {
                let buffer = reader.get_ref();
                let current_pos = reader.position() as usize;
                let remaining_bytes = &buffer[current_pos..];
                let null_pos = remaining_bytes.iter().position(|&b| b == 0);

                match null_pos {
                    Some(pos) => {
                        let str_bytes = &remaining_bytes[..pos];
                        values.push(ctx.intern(str_bytes).into_value(ctx));
                        reader.set_position((current_pos + pos + 1) as u64);
                    }
                    None => {
                        return Err(Error::from_value(
                            "missing null terminator for 'z' format".into_value(ctx),
                        ));
                    }
                }
            }
            's' => {
                let len_size = num_opt.unwrap_or(mem::size_of::<usize>());
                if len_size < 1 || len_size > 16 {
                    return Err(Error::from_value(
                        "string length size must be between 1 and 16 bytes".into_value(ctx),
                    ));
                }

                let padding = calculate_padding(initial_read_pos, len_size, state.max_alignment);
                read_padding(&mut reader, padding, ctx)?;

                let str_len_u128 = read_uint_n(&mut reader, len_size, &state, 's', ctx)?;
                let str_len = usize::try_from(str_len_u128).map_err(|_| {
                    Error::from_value("string length too large for usize".into_value(ctx))
                })?;
                if str_len_u128 > i64::MAX as u128 {
                    return Err(Error::from_value(
                        "string length value does not fit in `integer`".into_value(ctx),
                    ));
                }

                let str_bytes = read_exact_bytes(&mut reader, str_len, 's', ctx)?;
                values.push(ctx.intern(str_bytes).into_value(ctx));
            }
            invalid => {
                return Err(Error::from_value(
                    format!("invalid conversion option '{}' in format string", invalid)
                        .into_value(ctx),
                ));
            }
        }
    }

    Ok((values, reader.position().wrapping_add(1)))
}

fn read_exact_bytes<'a, 'gc>(
    reader: &mut Cursor<&'a [u8]>,
    count: usize,
    op: char,
    ctx: Context<'gc>,
) -> Result<&'a [u8], Error<'gc>> {
    let start = reader.position() as usize;
    if start + count > reader.get_ref().len() {
        return Err(Error::from_value(
            format!("data string too short for format '{}'", op).into_value(ctx),
        ));
    }
    reader.set_position((start + count) as u64);
    Ok(&reader.get_ref()[start..start + count])
}

fn read_padding<'gc>(
    reader: &mut Cursor<&[u8]>,
    padding: usize,
    ctx: Context<'gc>,
) -> Result<(), Error<'gc>> {
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
        return Err(Error::from_value(
            "integer size must be between 1 and 16".into_value(ctx),
        ));
    }
    let read_bytes = read_exact_bytes(reader, size, op, ctx)?;
    let mut bytes = [0u8; 16];

    match state.endianness {
        Endianness::Little => {
            if cfg!(target_endian = "little") {
                bytes[..size].copy_from_slice(read_bytes);
            } else {
                for (i, byte) in read_bytes.iter().rev().enumerate() {
                    if i < size {
                        bytes[i] = *byte;
                    }
                }
            }
        }
        Endianness::Big => {
            if cfg!(target_endian = "big") {
                bytes[..size].copy_from_slice(read_bytes);
            } else {
                for (i, byte) in read_bytes.iter().rev().enumerate() {
                    if i < size {
                        bytes[i] = *byte;
                    }
                }
            }
        }
        Endianness::Native => {
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
        return Err(Error::from_value(
            "integer size must be between 1 and 16".into_value(ctx),
        ));
    }
    let read_bytes = read_exact_bytes(reader, size, op, ctx)?;
    let mut bytes = [0u8; 16];

    match state.endianness {
        Endianness::Little => {
            if cfg!(target_endian = "little") {
                bytes[..size].copy_from_slice(read_bytes);
            } else {
                for (i, byte) in read_bytes.iter().rev().enumerate() {
                    if i < size {
                        bytes[i] = *byte;
                    }
                }
            }
        }
        Endianness::Big => {
            if cfg!(target_endian = "big") {
                bytes[..size].copy_from_slice(read_bytes);
            } else {
                for (i, byte) in read_bytes.iter().rev().enumerate() {
                    if i < size {
                        bytes[i] = *byte;
                    }
                }
            }
        }
        Endianness::Native => {
            bytes[..size].copy_from_slice(read_bytes);
        }
    }

    let value = u128::from_ne_bytes(bytes);

    Ok(value)
}

fn read_float<'gc>(
    reader: &mut Cursor<&[u8]>,
    state: &FormatState,
    ctx: Context<'gc>,
) -> Result<f32, Error<'gc>> {
    let bytes = read_exact_bytes(reader, mem::size_of::<f32>(), 'f', ctx)?;
    let arr: [u8; 4] = bytes
        .try_into()
        .map_err(|_| Error::from_value("internal error: float size mismatch".into_value(ctx)))?;
    Ok(match state.endianness {
        Endianness::Little => f32::from_le_bytes(arr),
        Endianness::Big => f32::from_be_bytes(arr),
        Endianness::Native => f32::from_ne_bytes(arr),
    })
}

fn read_double<'gc>(
    reader: &mut Cursor<&[u8]>,
    state: &FormatState,
    op: char,
    ctx: Context<'gc>,
) -> Result<f64, Error<'gc>> {
    let bytes = read_exact_bytes(reader, mem::size_of::<f64>(), op, ctx)?;
    let arr: [u8; 8] = bytes
        .try_into()
        .map_err(|_| Error::from_value("internal error: double size mismatch".into_value(ctx)))?;
    Ok(match state.endianness {
        Endianness::Little => f64::from_le_bytes(arr),
        Endianness::Big => f64::from_be_bytes(arr),
        Endianness::Native => f64::from_ne_bytes(arr),
    })
}
