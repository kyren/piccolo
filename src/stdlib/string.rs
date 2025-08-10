use crate::{Callback, CallbackReturn, Context, Error, FromValue, IntoValue, String, Table, Value};

mod pack;
mod packsize;
mod unpack;

use std::mem;

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
        "pack",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let fmt = stack.consume::<String>(ctx)?;

            let fmt = fmt.to_str()?;

            let bytes = pack::process(fmt, ctx, &stack)?;

            stack.replace(ctx, ctx.intern(&bytes));

            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "unpack",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
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
                return Err("initial position out of string bounds"
                    .into_value(ctx)
                    .into());
            }

            let (values, position) = unpack::process(fmt, bytes, start_pos as u64, ctx)?;

            stack.replace(ctx, values);
            stack.into_back(ctx, position as i64);

            Ok(CallbackReturn::Return)
        }),
    );

    string.set_field(
        ctx,
        "packsize",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let fmt = stack.consume::<String>(ctx)?;
            let fmt = fmt.to_str()?;

            let total_size = packsize::process(fmt, ctx)?;

            stack.replace(ctx, total_size as i64);
            Ok(CallbackReturn::Return)
        }),
    );

    ctx.set_global("string", string);
}

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
        let n = n_str
            .parse::<usize>()
            .map_err(|_| format!("invalid number '{}' in format string", n_str))?;
        if n == 0 || n > max_val {
            Err(format!("number '{}' out of range [1, {}]", n, max_val))
        } else {
            Ok(Some(n))
        }
    }
}

fn calculate_padding(current_pos: usize, data_size: usize, max_alignment: usize) -> usize {
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
