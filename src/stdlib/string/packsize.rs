use super::{calculate_padding, get_format_size, parse_optional_int, Endianness, FormatState};
use crate::{Context, Error, IntoValue};
use std::mem;

pub fn process<'gc>(fmt: &str, ctx: Context<'gc>) -> Result<usize, Error<'gc>> {
    let mut state = FormatState::default();
    let mut total_size: usize = 0;
    let mut current_offset: usize = 0;
    let mut chars = fmt.chars().peekable();

    while let Some(format_char) = chars.next() {
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
                total_size += 1;
                current_offset += 1;
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

                let padding = calculate_padding(current_offset, data_size, state.max_alignment);
                total_size += padding;
                current_offset += padding;
            }
            op @ ('b' | 'B' | 'h' | 'H' | 'l' | 'L' | 'j' | 'J' | 'T' | 'i' | 'I' | 'f' | 'd'
            | 'n') => {
                let data_size = get_format_size(op, num_opt).ok_or_else(|| {
                    Error::from_value(
                        format!("internal error getting size for '{}'", op).into_value(ctx),
                    )
                })?;

                let padding = calculate_padding(current_offset, data_size, state.max_alignment);
                total_size += padding + data_size;
                current_offset += padding + data_size;
            }
            'c' => {
                let n = num_opt.ok_or_else(|| {
                    Into::<Error>::into("missing number for 'c' option".into_value(ctx))
                })?;
                total_size += n;
                current_offset += n;
            }
            'z' => {
                return Err(Error::from_value(
                    "variable-length format ('z')".into_value(ctx),
                ));
            }
            's' => {
                let len_size = num_opt.unwrap_or(mem::size_of::<usize>());
                if len_size < 1 || len_size > 16 {
                    return Err(Error::from_value(
                        "string length size must be between 1 and 16 bytes".into_value(ctx),
                    ));
                }
                let padding = calculate_padding(current_offset, len_size, state.max_alignment);
                total_size += padding + len_size;
                current_offset += padding + len_size;
            }
            invalid => {
                return Err(Error::from_value(
                    format!("invalid conversion option '{}' in format string", invalid)
                        .into_value(ctx),
                ));
            }
        }
    }

    Ok(total_size)
}
