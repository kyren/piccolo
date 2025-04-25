use std::{
    io::{self, Cursor, Write},
    mem,
};

use crate::{Callback, CallbackReturn, Context, Error, IntoValue, String, Table, Value};
use lsonar::{find, gsub, r#match};

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

            stack.replace(ctx, [start as i64, end as i64]);

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

            stack.replace(ctx, captures);

            Ok(CallbackReturn::Return)
        }),
    );

    // TODO: implement `gmatch`, which should return a function iterator

    string.set_field(
        ctx,
        "gsub",
        Callback::from_fn(&ctx, |ctx, _, mut stack| {
            let (s, pattern, repl, n) =
                stack.consume::<(String, String, String, Option<i64>)>(ctx)?;

            let pattern = pattern.to_str()?;
            let s = s.to_str()?;
            let repl = repl.to_str()?;

            // TODO: we need to support [`Repl::Function`] and [`Repl::Table`]
            let (value, n) = gsub(
                s,
                pattern,
                lsonar::Repl::String(repl),
                n.map(|n| n as usize),
            )
            .map_err(|err| {
                let err = err.to_string();
                err.into_value(ctx)
            })?;

            stack.clear();
            stack.into_back(ctx, value);
            stack.into_back(ctx, n as i64);

            Ok(CallbackReturn::Return)
        }),
    );

    /* lua 5.3 manuals
        `string.pack`

    ```

    Returns a binary string containing the values v1, v2, etc.

    packed (that is, serialized in binary form)

    according to the format string fmt (see §6.4.2).


    §6.4.2


    The first argument to string.pack,string.packsize, and string.unpack

    is a format string,

    which describes the layout of the structure being created or read.


    A format string is a sequence of conversion options.

    The conversion options are as follows:


    <: sets little endian

    >: sets big endian

    =: sets native endian

    ![n]: sets maximum alignment to n

    (default is native alignment)

    b: a signed byte (char)

    B: an unsigned byte (char)

    h: a signed short (native size)

    H: an unsigned short (native size)

    l: a signed long (native size)

    L: an unsigned long (native size)

    j: a lua_Integer

    J: a lua_Unsigned

    T: a size_t (native size)

    i[n]: a signed int with n bytes

    (default is native size)

    I[n]: an unsigned int with n bytes

    (default is native size)

    f: a float (native size)

    d: a double (native size)

    n: a lua_Number

    cn: a fixed-sized string with n bytes

    z: a zero-terminated string

    s[n]: a string preceded by its length

    coded as an unsigned integer with n bytes

    (default is a size_t)

    x: one byte of padding

    Xop: an empty item that aligns

    according to option op

    (which is otherwise ignored)

    ' ': (empty space) ignored


    (A "[n]" means an optional integral numeral.)

    Except for padding, spaces, and configurations

    (options "xX <=>!"),

    each option corresponds to an argument (in string.pack)

    or a result (in string.unpack).


    For options "!n", "sn", "in", and "In",n can be any integer between 1 and 16.

    All integral options check overflows;string.pack checks whether the given value fits in the given size;string.unpack checks whether the read value fits in a Lua integer.


    Any format string starts as if prefixed by "!1=",

    that is,

    with maximum alignment of 1 (no alignment)

    and native endianness.


    Alignment works as follows:

    For each option,

    the format gets extra padding until the data starts

    at an offset that is a multiple of the minimum between the

    option size and the maximum alignment;

    this minimum must be a power of 2.

    Options "c" and "z" are not aligned;

    option "s" follows the alignment of its starting integer.


    All padding is filled with zeros by string.pack

    (and ignored by string.unpack).

    ```


    `string.unpack`

    ```

    Returns the values packed in string s (see string.pack)

    according to the format string fmt (see §6.4.2).

    An optional pos marks where

    to start reading in s (default is 1).

    After the read values,

    this function also returns the index of the first unread byte in s.

    ```

         */

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

            fn write_int<W: Write>(
                writer: &mut W,
                value: i64,
                size: usize,
                state: &FormatState,
            ) -> Result<(), std::string::String> {
                let min_val = match size {
                    1 => i8::MIN as i64,
                    2 => i16::MIN as i64,
                    4 => i32::MIN as i64,
                    8 => i64::MIN,
                    s if s < 8 => -(1i64 << (s * 8 - 1)),
                    _ => {
                        return Err(
                            "unsupported integer size > 8".into(),
                        )
                    }
                };
                let max_val = match size {
                    1 => i8::MAX as i64,
                    2 => i16::MAX as i64,
                    4 => i32::MAX as i64,
                    8 => i64::MAX,
                    s if s < 8 => (1i64 << (s * 8 - 1)) - 1,
                    _ => {
                        return Err(
                            "unsupported integer size > 8".into(),
                        )
                    }
                };

                if value < min_val || value > max_val {
                    return Err(format!(
                        "integer {} does not fit in {} signed bytes",
                        value, size
                    ));
                }

                let bytes = value.to_ne_bytes();

                let write_bytes = |writer: &mut W,
                                       size: usize,
                                       is_little: bool|
                 -> std::io::Result<()> {
                    if cfg!(target_endian = "little") == is_little {
                        writer.write_all(&bytes[0..size])
                    } else {
                        writer.write_all(&bytes[0..size].iter().rev().copied().collect::<Vec<_>>())
                    }
                };

                match state.endianness {
                    Endianness::Little => write_bytes(writer, size, true),
                    Endianness::Big => write_bytes(writer, size, false),
                    Endianness::Native => writer.write_all(&bytes[0..size]),
                }
                .map_err(|e|e.to_string())
            }

            fn write_uint<W: Write>(
                writer: &mut W,
                value: u64,
                size: usize,
                state: &FormatState,
            ) -> Result<(), std::string::String> {
                let max_val = match size {
                    1 => u8::MAX as u64,
                    2 => u16::MAX as u64,
                    4 => u32::MAX as u64,
                    8 => u64::MAX,
                    s if s < 8 => (1u64 << (s * 8)) - 1,
                    _ => {
                        return Err(
                            "unsupported integer size > 8".into(),
                        )
                    }
                };

                if value > max_val {
                    return Err(format!(
                        "unsigned integer {} does not fit in {} bytes",
                        value, size
                    ))
                }

                let bytes = value.to_ne_bytes();

                let write_bytes = |writer: &mut W,
                                       size: usize,
                                       is_little: bool|
                 -> std::io::Result<()> {
                    if cfg!(target_endian = "little") == is_little {
                        writer.write_all(&bytes[0..size])
                    } else {
                        writer.write_all(&bytes[0..size].iter().rev().copied().collect::<Vec<_>>())
                    }
                };

                match state.endianness {
                    Endianness::Little => write_bytes(writer, size, true),
                    Endianness::Big => write_bytes(writer, size, false),
                    Endianness::Native => writer.write_all(&bytes[0..size]),
                }
                .map_err(|e| e.to_string())
            }

            fn write_float<W: Write>(
                writer: &mut W,
                value: f32,
                state: &FormatState,
            ) -> io::Result<()> {
                match state.endianness {
                    Endianness::Little => writer.write_all(&value.to_le_bytes()),
                    Endianness::Big => writer.write_all(&value.to_be_bytes()),
                    Endianness::Native => writer.write_all(&value.to_ne_bytes()),
                }
            }

            fn write_double<W: Write>(
                writer: &mut W,
                value: f64,
                state: &FormatState,
            ) -> io::Result<()> {
                match state.endianness {
                    Endianness::Little => writer.write_all(&value.to_le_bytes()),
                    Endianness::Big => writer.write_all(&value.to_be_bytes()),
                    Endianness::Native => writer.write_all(&value.to_ne_bytes()),
                }
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
                 let num_opt = parse_optional_int(&mut chars, 16).map_err(|err| Into::<Error>::into(err.into_value(ctx)))?;

                match format_char {
                    '<' => state.endianness = Endianness::Little,
                    '>' => state.endianness = Endianness::Big,
                    '=' => state.endianness = Endianness::Native,
                    '!' => {
                        let n = num_opt.ok_or_else(|| 
                            Into::<Error>::into("missing number for '!' option".into_value(ctx))
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
                         let align_char = chars.next().ok_or_else(||
                             Into::<Error>::into("'X' must be followed by an option character".into_value(ctx))
                         )?;
                         let align_num_opt = parse_optional_int(&mut chars, 16).map_err(|err| Into::<Error>::into(err.into_value(ctx)))?;
                         let data_size = get_format_size(align_char, align_num_opt).ok_or_else(|| 
                             Into::<Error>::into(format!("invalid option '{}' following 'X'", align_char).into_value(ctx))
                         )?;

                         let padding = calculate_padding(current_pos, data_size, state.max_alignment);
                         write_padding(&mut writer, padding)?;

                         match align_char {
                             ' ' | 'x' | 'X' | '<' | '>' | '=' | '!' => {
                                 // These options don't consume arguments and were handled or ignored
                             },
                              _ => {
                                 // It's a data type, it needs an arg, handle it like below
                                 // BUT WE DON'T WANT TO WRITE THE DATA HERE, just align.
                                 // The Lua manual says Xop "is otherwise ignored". This implies we only pad.
                              }
                         }
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
                                 write_int(&mut writer, val, 1, &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
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
                                  write_uint(&mut writer, val as u64, 1, &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'h' => {
                                let val = arg_val.to_integer().ok_or_else(|| 
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                write_int(&mut writer, val, mem::size_of::<i16>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
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
                                write_uint(&mut writer, val as u64, mem::size_of::<u16>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'l' => {
                                let val = arg_val.to_integer().ok_or_else(|| 
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                write_int(&mut writer, val, mem::size_of::<i64>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
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
                                write_uint(&mut writer, val as u64, mem::size_of::<u64>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'j' => {
                                let val = arg_val.to_integer().ok_or_else(|| 
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                )?;
                                write_int(&mut writer, val, mem::size_of::<i64>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
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
                                write_uint(&mut writer, val as u64, mem::size_of::<u64>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
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
                                write_uint(&mut writer, val as u64, mem::size_of::<usize>(), &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
                             }
                             'i' => {
                                 let size = num_opt.unwrap_or(mem::size_of::<i32>());
                                 let val = arg_val.to_integer().ok_or_else(|| 
                                    Error::from_value(format!("argument for format '{}' must be an `integer`", op).into_value(ctx))
                                 )?;
                                 write_int(&mut writer, val, size, &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
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
                                 write_uint(&mut writer, val as u64, size, &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;
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
                         if len_size > 8 {
                             return Err(
                                 Error::from_value("string length size cannot exceed 8 bytes".into_value(ctx))
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
                         write_uint(&mut writer, str_len, len_size, &state).map_err(|err| Error::from_value(err.into_value(ctx)))?;

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
        Callback::from_fn(&ctx, |ctx, _, mut _stack| {
            // TODO: Implement string.unpack
            Err("string.unpack not yet implemented".into_value(ctx).into())
        }),
    );

    ctx.set_global("string", string);
}
