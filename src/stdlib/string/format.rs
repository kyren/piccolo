use fhex::ToHex;
use parser::{
    parse_format_string, ConversionSpecifier, ConversionType, FormatElement, NumericParam,
};
use std::convert::TryInto;
use thiserror::Error;

use crate::{
    meta_ops::{self, MetaResult},
    Context, Value,
};

mod parser;

#[derive(Debug, Clone, Copy, Error, PartialEq, Eq)]
pub enum FormatError {
    #[error("Error parsing the format string")]
    ParseError,
    #[error("Incorrect type passed as an argument")]
    WrongType,
    #[error("Too many arguments passed")]
    TooManyArgs,
    #[error("Too few arguments passed")]
    NotEnoughArgs,
    #[error("Other error (should never happen)")]
    Unknown,
}

pub fn format<'gc>(
    ctx: &Context<'gc>,
    format_str: &str,
    args: &[Value<'gc>],
) -> Result<String, FormatError> {
    let format_elements = parse_format_string(format_str)?;
    let mut res = String::new();
    let mut args = args;

    let mut pop_arg = || {
        if args.is_empty() {
            Err(FormatError::NotEnoughArgs)
        } else {
            let a = args[0];
            args = &args[1..];
            Ok(a)
        }
    };

    for element in format_elements {
        match element {
            FormatElement::Verbatim(s) => {
                res.push_str(s);
            }
            FormatElement::Format(spec) => {
                if spec.conversion_type == ConversionType::PercentSign {
                    res.push('%');
                } else {
                    let arg = pop_arg()?;
                    res.push_str(&format_value(*ctx, arg, &spec)?);
                }
            }
        }
    }

    if args.is_empty() {
        Ok(res)
    } else {
        Err(FormatError::TooManyArgs)
    }
}

pub fn format_value<'gc>(
    ctx: Context<'gc>,
    value: Value<'gc>,
    spec: &ConversionSpecifier,
) -> Result<String, FormatError> {
    match spec.conversion_type {
        ConversionType::String => {
            let s = match value {
                Value::Nil => "nil".to_string(),
                Value::Boolean(_) | Value::Integer(_) | Value::Number(_) => {
                    value.display().to_string()
                }
                Value::String(s) => s.display_lossy().to_string(),
                Value::Table(_) | Value::Function(_) | Value::Thread(_) | Value::UserData(_) => {
                    match meta_ops::tostring(ctx, value) {
                        Ok(meta_result) => match meta_result {
                            MetaResult::Value(value) => value.display().to_string(),
                            MetaResult::Call(_) => return Err(FormatError::Unknown),
                        },
                        Err(_) => format!("{:p}", value_address(&value)),
                    }
                }
            };
            format_string(&s, spec)
        }

        ConversionType::Pointer => format_string(&format!("{:p}", value_address(&value)), spec),

        ConversionType::DecInt
        | ConversionType::OctInt
        | ConversionType::HexIntLower
        | ConversionType::HexIntUpper => {
            let i = match value {
                Value::Integer(i) => i,
                Value::Number(n) => n as i64,
                Value::Boolean(false) => 0,
                Value::Boolean(true) => 1,
                _ => return Err(FormatError::WrongType),
            };
            format_integer(i, spec)
        }

        ConversionType::DecFloatLower
        | ConversionType::DecFloatUpper
        | ConversionType::SciFloatLower
        | ConversionType::SciFloatUpper
        | ConversionType::CompactFloatLower
        | ConversionType::CompactFloatUpper
        | ConversionType::HexFloatLower
        | ConversionType::HexFloatUpper => {
            let f = match value {
                Value::Number(n) => n,
                Value::Integer(i) => i as f64,
                _ => return Err(FormatError::WrongType),
            };
            format_float(f, spec)
        }

        ConversionType::Char => {
            let c = match value {
                Value::Integer(i) => {
                    if i >= 0 && i <= 0x10FFFF {
                        if let Some(c) = char::from_u32(i as u32) {
                            c
                        } else {
                            return Err(FormatError::WrongType);
                        }
                    } else {
                        return Err(FormatError::WrongType);
                    }
                }
                _ => return Err(FormatError::WrongType),
            };
            format_char(c, spec)
        }

        ConversionType::QuotedString => {
            let string_val = match value {
                Value::String(s) => s.display_lossy().to_string(),
                _ => return Err(FormatError::WrongType),
            };
            format_quoted_string(&string_val, spec)
        }
        ConversionType::PercentSign => Ok("%".to_string()),
    }
}

fn value_address<'gc>(value: &Value<'gc>) -> *const () {
    value as *const Value as *const ()
}

fn format_string(input: &str, spec: &ConversionSpecifier) -> Result<String, FormatError> {
    let mut s = String::new();

    let width: usize = match spec.width {
        NumericParam::Literal(w) => w,
    }
    .try_into()
    .unwrap_or_default();

    if spec.left_adj {
        s.push_str(input);
        while s.len() < width {
            s.push(' ');
        }
    } else {
        while s.len() + s.len() < width {
            s.push(' ');
        }
        s.push_str(input);
    }
    Ok(s)
}

fn format_integer(value: i64, spec: &ConversionSpecifier) -> Result<String, FormatError> {
    match spec.conversion_type {
        ConversionType::DecInt => {
            let negative = value < 0;
            let abs_val = value.abs();
            let sign_prefix = if negative {
                "-"
            } else if spec.force_sign {
                "+"
            } else if spec.space_sign {
                " "
            } else {
                ""
            }
            .to_owned();

            let base = 10;
            let digits: Vec<char> = "0123456789".chars().collect();
            let alt_prefix = "";
            let mut prefix = String::new();

            if spec.alt_form && base != 10 {
                prefix = alt_prefix.to_owned();
            }

            prefix = sign_prefix + &prefix;

            let mut rev_num = String::new();
            let mut n = abs_val as u64;
            while n > 0 {
                let digit = n % (base as u64);
                n /= base as u64;
                rev_num.push(digits[digit as usize]);
            }
            if rev_num.is_empty() {
                rev_num.push('0');
            }

            let width: usize = match spec.width {
                NumericParam::Literal(w) => w,
            }
            .try_into()
            .unwrap_or_default();

            let formatted = if spec.left_adj {
                let mut num_str = prefix + &rev_num.chars().rev().collect::<String>();
                while num_str.len() < width {
                    num_str.push(' ');
                }
                num_str
            } else if spec.zero_pad {
                while prefix.len() + rev_num.len() < width {
                    rev_num.push('0');
                }
                prefix + &rev_num.chars().rev().collect::<String>()
            } else {
                let mut num_str = prefix + &rev_num.chars().rev().collect::<String>();
                while num_str.len() < width {
                    num_str = " ".to_owned() + &num_str;
                }
                num_str
            };

            Ok(formatted)
        }
        ConversionType::HexIntLower | ConversionType::HexIntUpper | ConversionType::OctInt => {
            let mut base = 10;
            let mut digits: Vec<char> = "0123456789".chars().collect();
            let mut alt_prefix = "";

            match spec.conversion_type {
                ConversionType::HexIntLower => {
                    base = 16;
                    digits = "0123456789abcdef".chars().collect();
                    alt_prefix = "0x";
                }
                ConversionType::HexIntUpper => {
                    base = 16;
                    digits = "0123456789ABCDEF".chars().collect();
                    alt_prefix = "0X";
                }
                ConversionType::OctInt => {
                    base = 8;
                    digits = "01234567".chars().collect();
                    alt_prefix = "0";
                }
                _ => {}
            }

            let prefix = if spec.alt_form {
                alt_prefix.to_owned()
            } else {
                String::new()
            };

            let sign_prefix = if value < 0 { "-" } else { "" }.to_owned();

            let prefix = sign_prefix + &prefix;

            let mut rev_num = String::new();
            let mut n = value.unsigned_abs();
            while n > 0 {
                let digit = n % base;
                n /= base;
                rev_num.push(digits[digit as usize]);
            }
            if rev_num.is_empty() {
                rev_num.push('0');
            }

            let width: usize = match spec.width {
                NumericParam::Literal(w) => w,
            }
            .try_into()
            .unwrap_or_default();
            let formatted = if spec.left_adj {
                let mut num_str = prefix + &rev_num.chars().rev().collect::<String>();
                while num_str.len() < width {
                    num_str.push(' ');
                }
                num_str
            } else if spec.zero_pad {
                while prefix.len() + rev_num.len() < width {
                    rev_num.push('0');
                }
                prefix + &rev_num.chars().rev().collect::<String>()
            } else {
                let mut num_str = prefix + &rev_num.chars().rev().collect::<String>();
                while num_str.len() < width {
                    num_str = " ".to_owned() + &num_str;
                }
                num_str
            };

            Ok(formatted)
        }
        ConversionType::Char => {
            if value >= 0 && value <= 0x10FFFF {
                if let Some(c) = char::from_u32(value as u32) {
                    return format_char(c, spec);
                }
            }
            Err(FormatError::WrongType)
        }
        ConversionType::DecFloatLower
        | ConversionType::DecFloatUpper
        | ConversionType::SciFloatLower
        | ConversionType::SciFloatUpper
        | ConversionType::CompactFloatLower
        | ConversionType::CompactFloatUpper
        | ConversionType::HexFloatLower
        | ConversionType::HexFloatUpper => format_float(value as f64, spec),
        _ => Err(FormatError::WrongType),
    }
}

fn format_float(value: f64, spec: &ConversionSpecifier) -> Result<String, FormatError> {
    let mut prefix = String::new();
    let mut number;

    if value.is_sign_negative() {
        prefix.push('-');
    } else if spec.space_sign {
        prefix.push(' ');
    } else if spec.force_sign {
        prefix.push('+');
    }

    if value.is_finite() {
        let mut use_scientific = false;
        let mut exp_symb = 'e';
        let mut strip_trailing_0s = false;
        let mut use_hex = false;
        let mut hex_uppercase = false;
        let abs = value.abs();
        let mut exponent;
        let NumericParam::Literal(mut precision) = spec.precision;
        if precision <= 0 {
            precision = 0;
        }

        match spec.conversion_type {
            ConversionType::DecFloatLower | ConversionType::DecFloatUpper => {}
            ConversionType::SciFloatLower => {
                use_scientific = true;
            }
            ConversionType::SciFloatUpper => {
                use_scientific = true;
                exp_symb = 'E';
            }
            ConversionType::CompactFloatLower | ConversionType::CompactFloatUpper => {
                if spec.conversion_type == ConversionType::CompactFloatUpper {
                    exp_symb = 'E';
                }
                strip_trailing_0s = true;
                if precision == 0 {
                    precision = 1;
                }
                exponent = abs.log10().floor() as i32;
                let rounding_factor = 10.0_f64.powf((precision - 1 - exponent) as f64);
                let rounded_fixed = (abs * rounding_factor).round();
                let abs_rounded = rounded_fixed / rounding_factor;
                exponent = abs_rounded.log10().floor() as i32;
                if exponent < -4 || exponent >= precision {
                    use_scientific = true;
                    precision -= 1;
                } else {
                    precision -= 1 + exponent;
                }
            }
            ConversionType::HexFloatLower => {
                use_hex = true;
            }
            ConversionType::HexFloatUpper => {
                use_hex = true;
                hex_uppercase = true;
            }
            _ => {
                return Err(FormatError::WrongType);
            }
        }

        if use_hex {
            number = abs.to_hex();

            if number.starts_with('-') {
                number = number[1..].to_string();
            }

            if hex_uppercase {
                number = number.to_uppercase();
            }
        } else if use_scientific {
            exponent = abs.log10().floor() as i32;
            let mut normal = abs / 10.0_f64.powf(exponent as f64);
            number = String::new();

            if precision > 0 {
                let mut int_part = normal.trunc();
                let mut exp_factor = 10.0_f64.powf(precision as f64);
                let mut tail = ((normal - int_part) * exp_factor).round() as u64;
                while tail >= exp_factor as u64 {
                    int_part += 1.0;
                    tail -= exp_factor as u64;
                    if int_part >= 10.0 {
                        exponent += 1;
                        exp_factor /= 10.0;
                        normal /= 10.0;
                        int_part = normal.trunc();
                        tail = ((normal - int_part) * exp_factor).round() as u64;
                    }
                }

                let mut rev_tail_str = String::new();
                for _ in 0..precision {
                    rev_tail_str.push((b'0' + (tail % 10) as u8) as char);
                    tail /= 10;
                }
                number.push_str(&format!("{}", int_part));
                number.push('.');
                number.push_str(&rev_tail_str.chars().rev().collect::<String>());
                if strip_trailing_0s {
                    number = number.trim_end_matches('0').to_owned();
                }
            } else {
                number.push_str(&format!("{}", normal.round()));
            }
            number.push(exp_symb);
            number.push_str(&format!("{:+03}", exponent));
        } else {
            number = String::new();
            if precision > 0 {
                let mut int_part = abs.trunc();
                let exp_factor = 10.0_f64.powf(precision as f64);
                let mut tail = ((abs - int_part) * exp_factor).round() as u64;
                let mut rev_tail_str = String::new();
                if tail >= exp_factor as u64 {
                    int_part += 1.0;
                    tail -= exp_factor as u64;
                }
                for _ in 0..precision {
                    rev_tail_str.push((b'0' + (tail % 10) as u8) as char);
                    tail /= 10;
                }
                number.push_str(&format!("{}", int_part));
                number.push('.');
                number.push_str(&rev_tail_str.chars().rev().collect::<String>());
                if strip_trailing_0s {
                    number = number.trim_end_matches('0').to_owned();
                    if number.ends_with('.') {
                        number.pop();
                    }
                }
            } else {
                number.push_str(&format!("{}", abs.round()));
            }
        }
    } else {
        number = String::new();
        match spec.conversion_type {
            ConversionType::DecFloatLower
            | ConversionType::SciFloatLower
            | ConversionType::CompactFloatLower
            | ConversionType::HexFloatLower => {
                if value.is_infinite() {
                    number.push_str("inf")
                } else {
                    number.push_str("nan")
                }
            }
            ConversionType::DecFloatUpper
            | ConversionType::SciFloatUpper
            | ConversionType::CompactFloatUpper
            | ConversionType::HexFloatUpper => {
                if value.is_infinite() {
                    number.push_str("INF")
                } else {
                    number.push_str("NAN")
                }
            }
            _ => {
                return Err(FormatError::WrongType);
            }
        }
    }

    let width: usize = match spec.width {
        NumericParam::Literal(w) => w,
    }
    .try_into()
    .unwrap_or_default();
    let formatted = if spec.left_adj {
        let mut full_num = prefix + &number;
        while full_num.len() < width {
            full_num.push(' ');
        }
        full_num
    } else if spec.zero_pad && value.is_finite() {
        while prefix.len() + number.len() < width {
            prefix.push('0');
        }
        prefix + &number
    } else {
        let mut full_num = prefix + &number;
        while full_num.len() < width {
            full_num = " ".to_owned() + &full_num;
        }
        full_num
    };
    Ok(formatted)
}

fn format_char(c: char, spec: &ConversionSpecifier) -> Result<String, FormatError> {
    if spec.conversion_type == ConversionType::Char {
        let mut s = String::new();

        let width: usize = match spec.width {
            NumericParam::Literal(w) => w,
        }
        .try_into()
        .unwrap_or_default();

        if spec.left_adj {
            s.push(c);
            while s.len() < width {
                s.push(' ');
            }
        } else {
            while s.len() + c.len_utf8() < width {
                s.push(' ');
            }
            s.push(c);
        }
        Ok(s)
    } else {
        Err(FormatError::WrongType)
    }
}

fn format_quoted_string(s: &str, _spec: &ConversionSpecifier) -> Result<String, FormatError> {
    let mut quoted = String::with_capacity(s.len() + 2);
    quoted.push('"');
    for byte in s.bytes() {
        match byte {
            b'"' => quoted.push_str("\\\""),
            b'\\' => quoted.push_str("\\\\"),
            b'\n' => quoted.push_str("\\n"),
            b'\r' => quoted.push_str("\\r"),
            b'\0' => quoted.push_str("\\0"),
            b if (1..=31).contains(&b) || b == 127 => {
                quoted.push_str(&format!("\\{}", b));
            }
            b => quoted.push(b as char),
        }
    }
    quoted.push('"');
    Ok(quoted)
}
