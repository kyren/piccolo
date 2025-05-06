use core::cmp::Ordering;
use std::io::Write;

use super::{integer_length, FormatArgs, FormatError};
use crate::Error;

pub enum FloatMode {
    Normal,
    Exponent,
    Compact,
    Hex,
}

pub fn write_float<'gc, W: Write>(
    w: &mut W,
    float: f64,
    mode: FloatMode,
    args: FormatArgs,
    float_buf: &mut [u8],
) -> Result<(), Error<'gc>> {
    let sign = args.sign_char(float.is_sign_negative());

    let preserve_decimal = args.alternate;
    let width = args.width;
    let precision = args.precision.unwrap_or(6);

    if !float.is_finite() {
        return write_nonfinite_float(w, float, args, sign).map_err(Into::into);
    }

    if matches!(mode, FloatMode::Hex) {
        return write_hex_float(w, float, args).map_err(Into::into);
    }

    if matches!(mode, FloatMode::Compact | FloatMode::Exponent) {
        let p = if matches!(mode, FloatMode::Compact) {
            precision.saturating_sub(1)
        } else {
            precision
        };
        let formatted = format_into_buffer(&mut *float_buf, format_args!("{:+.p$e}", float))?;

        let idx = formatted.rfind('e').ok_or(FormatError::BadFloat)?;
        let exp = formatted[idx + 1..]
            .parse::<i16>()
            .map_err(|_| FormatError::BadFloat)?;

        // Note: Rust does not include a leading '+' in the exponent notation, but Lua does,
        // so we must calculate the length manually.
        let exp_len = 1 + integer_length(exp.unsigned_abs() as u64);

        // Implementation of %g, following the description of the algorithm
        // in Python's documentation:
        // https://docs.python.org/3/library/string.html#format-specification-mini-language
        if matches!(mode, FloatMode::Compact) && exp >= -4 && (exp as i64) < (precision as i64) {
            let p = (precision as i64 - 1 - exp as i64) as usize;

            let formatted_compact;
            if preserve_decimal {
                // Add a decimal at the end, in case Rust doesn't generate one; then strip it out
                let s = format_into_buffer(&mut *float_buf, format_args!("{:+.p$}.", float))?;
                if s[1..s.len() - 1].contains('.') {
                    formatted_compact = &s[1..s.len() - 1];
                } else {
                    formatted_compact = &s[1..];
                }
            } else {
                let s = format_into_buffer(&mut *float_buf, format_args!("{:+.p$}", float))?;
                formatted_compact = strip_nonsignificant_zeroes(&s[1..]);
            }

            let len = formatted_compact.len();
            let zero_width = if args.zero_pad { width } else { 0 };

            let pad = args.pad_num_before(w, len, zero_width, sign)?;
            write!(w, "{}", formatted_compact)?;
            pad.finish_pad(w)?;
        } else {
            // exponent mode:
            // [   ][-][000][a.bbb][e][+EE]

            let mut mantissa = &formatted[1..idx];
            if matches!(mode, FloatMode::Compact) && !preserve_decimal {
                mantissa = strip_nonsignificant_zeroes(mantissa);
            }
            let e = if args.upper { 'E' } else { 'e' };

            let exp_len = exp_len.max(3);
            let len = mantissa.len() + 1 + exp_len;
            let zero_width = if args.zero_pad { width } else { 0 };

            if preserve_decimal && !formatted.contains('.') {
                let pad = args.pad_num_before(w, len + 1, zero_width, sign)?;
                write!(w, "{mantissa}.{e}{exp:+03}")?;
                pad.finish_pad(w)?;
            } else {
                let pad = args.pad_num_before(w, len, zero_width, sign)?;
                write!(w, "{mantissa}{e}{exp:+03}")?;
                pad.finish_pad(w)?;
            }
        }
    } else {
        // normal float
        // This can be larger than any reasonable buffer, so we have
        // to forward everything to std (or find a float serialization
        // library with custom formatting support.)

        // TODO: cannot support the '#' preserving decimal mode
        // string.format("'%#.0f'", 1) should result in "1."
        match (args.left_align, args.zero_pad, sign) {
            (false, false, b"" | b"-") => write!(w, "{float:width$.precision$}")?,
            (false, true, b"" | b"-") => write!(w, "{float:>0width$.precision$}")?,
            (false, false, b"+") => write!(w, "{float:+width$.precision$}")?,
            (false, true, b"+") => write!(w, "{float:>+0width$.precision$}")?,
            (false, false, b" ") => write!(w, " {float:width$.precision$}")?,
            (false, true, b" ") => write!(w, " {float:>0width$.precision$}")?,
            (true, _, b"" | b"-") => write!(w, "{float:<width$.precision$}")?,
            (true, _, b"+") => write!(w, "{float:<+width$.precision$}")?,
            (true, _, b" ") => write!(w, " {float:<width$.precision$}")?,
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn write_nonfinite_float<W: Write>(
    w: &mut W,
    float: f64,
    args: FormatArgs,
    sign: &[u8],
) -> Result<(), std::io::Error> {
    let s = match (float.is_infinite(), args.upper) {
        (true, false) => "inf",
        (true, true) => "INF",
        (false, false) => "nan",
        (false, true) => "NAN",
    };
    let pad = args.pad_num_before(w, s.len(), 0, sign)?;
    write!(w, "{s}")?;
    pad.finish_pad(w)?;
    Ok(())
}

const F64_EXPONENT_BITS: u32 = 11;
const F64_MANTISSA_BITS: u32 = 52;
const F64_EXP_OFFSET: i16 = -(1 << (F64_EXPONENT_BITS - 1)) + 1;

#[inline]
const fn bitselect(n: u64, off: u32, count: u32) -> u64 {
    (n >> off) & ((1 << count) - 1)
}

fn round_mantissa(mantissa: u64, exp_bits: u16, precision: usize) -> (u64, u64) {
    let leading_bit = (exp_bits != 0) as u64;
    let mantissa = mantissa | (leading_bit << F64_MANTISSA_BITS);
    let used_mantissa_bits = (precision as u32 * 4).min(F64_MANTISSA_BITS);

    let remainder_bits = F64_MANTISSA_BITS - used_mantissa_bits;
    let quotient = mantissa >> remainder_bits;
    let remainder = bitselect(mantissa, 0, remainder_bits);
    let rounded_quotient = match remainder.cmp(&(1 << remainder_bits.saturating_sub(1))) {
        Ordering::Less => quotient,
        Ordering::Equal => (quotient + 1) & !1, // Round to even
        Ordering::Greater => quotient + 1,
    };

    let head = rounded_quotient >> used_mantissa_bits;
    let rounded_div_mantissa = bitselect(rounded_quotient, 0, used_mantissa_bits);
    (head, rounded_div_mantissa)
}

pub fn write_hex_float<W: Write>(
    w: &mut W,
    float: f64,
    args: FormatArgs,
) -> Result<(), std::io::Error> {
    let sign = args.sign_char(float.is_sign_negative());
    let preserve_decimal = args.alternate;

    if !float.is_finite() {
        return write_nonfinite_float(w, float, args, sign);
    }

    let width = args.width;
    let mut precision = args
        .precision
        .unwrap_or(F64_MANTISSA_BITS.div_ceil(4) as usize);

    let bits = f64::to_bits(float);
    let exp_bits = bitselect(bits, F64_MANTISSA_BITS, F64_EXPONENT_BITS);
    // clamp exponent to -1022 for subnormals
    let mut exp = (exp_bits as i16 + F64_EXP_OFFSET).max(-1022);
    let mantissa = bitselect(bits, 0, F64_MANTISSA_BITS);

    if float == 0.0 {
        exp = 0;
    }

    let (head, mut mantissa) = round_mantissa(mantissa, exp_bits as u16, precision);

    let prefix: &[u8] = match (sign, args.upper) {
        (b"", false) => b"0x",
        (b"-", false) => b"-0x",
        (b"+", false) => b"+0x",
        (b" ", false) => b" 0x",
        (b"", true) => b"0X",
        (b"-", true) => b"-0X",
        (b"+", true) => b"+0X",
        (b" ", true) => b" 0X",
        _ => unreachable!(),
    };
    let zero_width = if args.zero_pad {
        width.saturating_sub(prefix.len())
    } else {
        0
    };

    if args.precision.is_none() {
        let trailing_zero_digits = mantissa.trailing_zeros().min(F64_MANTISSA_BITS) / 4;
        mantissa = mantissa >> (trailing_zero_digits * 4);
        precision = precision.saturating_sub(trailing_zero_digits as usize);
    }

    if precision != 0 {
        let m_width = precision;
        let len = 2 + m_width + 1 + 1 + integer_length(exp.unsigned_abs() as u64);

        let pad = args.pad_num_before(w, len, zero_width, prefix)?;
        if args.upper {
            write!(w, "{head}.{mantissa:0m_width$X}P{exp:+}")?;
        } else {
            write!(w, "{head}.{mantissa:0m_width$x}p{exp:+}")?;
        }
        pad.finish_pad(w)?;
    } else {
        let len = 3 + preserve_decimal as usize + integer_length(exp.unsigned_abs() as u64);

        let p = if args.upper { 'P' } else { 'p' };
        let pad = args.pad_num_before(w, len, zero_width, prefix)?;
        if preserve_decimal {
            write!(w, "{head}.{p}{exp:+}")?;
        } else {
            write!(w, "{head}{p}{exp:+}")?;
        }
        pad.finish_pad(w)?;
    }
    Ok(())
}

fn format_into_buffer<'a>(
    buf: &'a mut [u8],
    args: core::fmt::Arguments<'_>,
) -> Result<&'a str, core::fmt::Error> {
    use core::fmt::Write;

    struct BufferWriter<'a> {
        buffer: &'a mut [u8],
        offset: usize,
    }

    impl<'a> BufferWriter<'a> {
        fn new(buffer: &'a mut [u8]) -> Self {
            Self { buffer, offset: 0 }
        }
        fn into_str(self) -> &'a str {
            let slice = &self.buffer[..self.offset];
            // Safety: `buffer` can only be filled by write_str,
            // which requires valid utf8 strings.
            unsafe { core::str::from_utf8_unchecked(slice) }
        }
    }

    impl Write for BufferWriter<'_> {
        fn write_str(&mut self, s: &str) -> core::fmt::Result {
            let bytes = s.as_bytes();
            let dest = self.buffer[self.offset..]
                .get_mut(..bytes.len())
                .ok_or(core::fmt::Error)?;
            dest.copy_from_slice(bytes);
            self.offset += bytes.len();
            Ok(())
        }
    }

    let mut writer = BufferWriter::new(buf);
    write!(writer, "{}", args)?;
    Ok(writer.into_str())
}

/// Remove trailing zeroes from a formatted number without changing its value.
fn strip_nonsignificant_zeroes(str: &str) -> &str {
    if let Some(last_nonzero) = str.rfind(|p| p != '0') {
        if let Some(decimal) = str[..=last_nonzero].rfind('.') {
            // If the number ends with a trailing decimal point, remove it.
            if decimal == last_nonzero {
                return &str[..last_nonzero];
            } else {
                return &str[..=last_nonzero];
            }
        }
    }
    str
}
