use std::{
    io::{self, Cursor, Write},
    mem,
};

use crate::{Context, Error, IntoValue, Stack};

use super::{calculate_padding, get_format_size, parse_optional_int, Endianness, FormatState};

pub fn process<'gc>(
    fmt: &str,
    ctx: Context<'gc>,
    stack: &Stack<'gc, '_>,
) -> Result<Vec<u8>, Error<'gc>> {
    let mut state = FormatState::default();
    let mut writer = Cursor::new(Vec::new());
    let mut current_argument_index = 0;

    let mut chars = fmt.chars().peekable();

    while let Some(format_char) = chars.next() {
        let current_writer_position = writer.position() as usize;
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
                if n < 1 || n > 16 || (n & (n - 1)) != 0 {
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
                write_padding(&mut writer, 1)?;
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

                let padding =
                    calculate_padding(current_writer_position, data_size, state.max_alignment);
                write_padding(&mut writer, padding)?;
            }
            op @ ('b' | 'B' | 'h' | 'H' | 'l' | 'L' | 'j' | 'J' | 'T' | 'i' | 'I' | 'f' | 'd'
            | 'n') => {
                check_pack_arg(ctx, stack.len(), current_argument_index, op)?;
                let arg_val = stack.get(current_argument_index);
                current_argument_index += 1;

                let data_size = get_format_size(op, num_opt).unwrap();
                let padding =
                    calculate_padding(current_writer_position, data_size, state.max_alignment);
                write_padding(&mut writer, padding)?;

                match op {
                    'b' => {
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        write_int_n(&mut writer, val, 1, &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'B' => {
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        if val < 0 {
                            return Err(Error::from_value(
                                format!(
                                    "negative value {} provided for unsigned format '{}'",
                                    val, op
                                )
                                .into_value(ctx),
                            ));
                        }
                        write_uint_n(&mut writer, val, 1, &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'h' => {
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        write_int_n(&mut writer, val, mem::size_of::<i16>(), &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'H' => {
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        if val < 0 {
                            return Err(Error::from_value(
                                format!(
                                    "negative value {} provided for unsigned format '{}'",
                                    val, op
                                )
                                .into_value(ctx),
                            ));
                        }
                        write_uint_n(&mut writer, val, mem::size_of::<u16>(), &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'l' => {
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        write_int_n(&mut writer, val, mem::size_of::<i64>(), &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'L' => {
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        if val < 0 {
                            return Err(Error::from_value(
                                format!(
                                    "negative value {} provided for unsigned format '{}'",
                                    val, op
                                )
                                .into_value(ctx),
                            ));
                        }
                        write_uint_n(&mut writer, val, mem::size_of::<u64>(), &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'j' => {
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        write_int_n(&mut writer, val, mem::size_of::<i64>(), &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'J' => {
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        if val < 0 {
                            return Err(Error::from_value(
                                format!(
                                    "negative value {} provided for unsigned format '{}'",
                                    val, op
                                )
                                .into_value(ctx),
                            ));
                        }
                        write_uint_n(&mut writer, val, mem::size_of::<u64>(), &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'T' => {
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        if val < 0 {
                            return Err(Error::from_value(
                                format!(
                                    "negative value {} provided for unsigned format '{}'",
                                    val, op
                                )
                                .into_value(ctx),
                            ));
                        }
                        write_uint_n(&mut writer, val, mem::size_of::<usize>(), &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'i' => {
                        let size = num_opt.unwrap_or(mem::size_of::<i32>());
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        write_int_n(&mut writer, val, size, &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'I' => {
                        let size = num_opt.unwrap_or(mem::size_of::<u32>());
                        let val = arg_val.to_integer().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be an `integer`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        if val < 0 {
                            return Err(Error::from_value(
                                format!(
                                    "negative value {} provided for unsigned format '{}'",
                                    val, op
                                )
                                .into_value(ctx),
                            ));
                        }
                        write_uint_n(&mut writer, val, size, &state)
                            .map_err(|err| Error::from_value(err.into_value(ctx)))?;
                    }
                    'f' => {
                        let val = arg_val.to_number().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be a `number`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        write_float(&mut writer, val as f32, &state)
                            .map_err(|e| Error::from_value(e.to_string().into_value(ctx)))?;
                    }
                    'd' => {
                        let val = arg_val.to_number().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be a `number`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        write_double(&mut writer, val, &state)
                            .map_err(|e| Error::from_value(e.to_string().into_value(ctx)))?;
                    }
                    'n' => {
                        let val = arg_val.to_number().ok_or_else(|| {
                            Error::from_value(
                                format!("argument for format '{}' must be a `number`", op)
                                    .into_value(ctx),
                            )
                        })?;
                        write_double(&mut writer, val, &state)
                            .map_err(|e| Error::from_value(e.to_string().into_value(ctx)))?;
                    }
                    _ => unreachable!(),
                }
            }
            'c' => {
                let n = num_opt.ok_or_else(|| {
                    Into::<Error>::into("missing number for 'c' option".into_value(ctx))
                })?;
                check_pack_arg(ctx, stack.len(), current_argument_index, 'c')?;
                let arg_val = stack.get(current_argument_index);
                current_argument_index += 1;

                let s = arg_val.into_string(ctx).ok_or_else(|| {
                    Error::from_value("argument for format 'c' must be a `string`".into_value(ctx))
                })?;
                let bytes = s.as_bytes();

                if bytes.len() >= n {
                    writer.write_all(&bytes[..n])?
                } else {
                    writer.write_all(bytes)?;
                    write_padding(&mut writer, n - bytes.len())?;
                }
            }
            'z' => {
                check_pack_arg(ctx, stack.len(), current_argument_index, 'z')?;
                let arg_val = stack.get(current_argument_index);
                current_argument_index += 1;

                let s = arg_val.into_string(ctx).ok_or_else(|| {
                    Error::from_value("argument for format 'z' must be a `string`".into_value(ctx))
                })?;
                let bytes = s.as_bytes();

                writer.write_all(bytes)?;
                writer.write_all(&[0])?
            }
            's' => {
                let len_size = num_opt.unwrap_or(mem::size_of::<usize>());
                if len_size < 1 || len_size > 16 {
                    return Err(Error::from_value(
                        "string length size must be between 1 and 16 bytes".into_value(ctx),
                    ));
                }

                check_pack_arg(ctx, stack.len(), current_argument_index, 's')?;
                let arg_val = stack.get(current_argument_index);
                current_argument_index += 1;

                let s = arg_val.into_string(ctx).ok_or_else(|| {
                    Error::from_value("argument for format 's' must be a `string`".into_value(ctx))
                })?;
                let bytes = s.as_bytes();
                let str_len = bytes.len() as u64;

                let padding =
                    calculate_padding(current_writer_position, len_size, state.max_alignment);
                write_padding(&mut writer, padding)?;
                let str_len_i64 = i64::try_from(str_len).map_err(|_| {
                    Error::from_value("string length too large to represent as i64".into_value(ctx))
                })?;
                write_uint_n(&mut writer, str_len_i64, len_size, &state)
                    .map_err(|err| Error::from_value(err.into_value(ctx)))?;

                writer.write_all(bytes)?
            }
            invalid => {
                return Err(Error::from_value(
                    format!("invalid conversion option '{}' in format string", invalid)
                        .into_value(ctx),
                ));
            }
        }
    }

    Ok(writer.into_inner())
}

fn check_pack_arg(ctx: Context, stack_len: usize, index: usize, op: char) -> Result<(), Error> {
    if index >= stack_len {
        Err(format!("missing argument for format '{}'", op)
            .into_value(ctx)
            .into())
    } else {
        Ok(())
    }
}

fn write_padding(writer: &mut impl Write, padding: usize) -> Result<(), std::io::Error> {
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

    write_bytes_endian(writer, &bytes[..size], state.endianness).map_err(|e| e.to_string())
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
        return Err(format!(
            "negative value {} provided for unsigned format",
            value
        ));
    }
    let u_value = value as u64;

    let max_val = if size == 16 {
        u128::MAX
    } else {
        (1u128 << (size * 8)) - 1
    };

    if (u_value as u128) > max_val {
        return Err(format!(
            "unsigned integer {} does not fit in {} bytes",
            u_value, size
        ));
    }

    let mut bytes = [0u8; 16];
    let src_bytes = u_value.to_ne_bytes();

    bytes[..8].copy_from_slice(&src_bytes);

    write_bytes_endian(writer, &bytes[..size], state.endianness).map_err(|e| e.to_string())
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

fn write_float<W: Write>(writer: &mut W, value: f32, state: &FormatState) -> io::Result<()> {
    let bytes = match state.endianness {
        Endianness::Little => value.to_le_bytes(),
        Endianness::Big => value.to_be_bytes(),
        Endianness::Native => value.to_ne_bytes(),
    };
    writer.write_all(&bytes)
}

fn write_double<W: Write>(writer: &mut W, value: f64, state: &FormatState) -> io::Result<()> {
    let bytes = match state.endianness {
        Endianness::Little => value.to_le_bytes(),
        Endianness::Big => value.to_be_bytes(),
        Endianness::Native => value.to_ne_bytes(),
    };
    writer.write_all(&bytes)
}
