use std::{
    fmt::{self, Write as _},
    str,
};

pub fn trim_whitespace(mut s: &[u8]) -> &[u8] {
    s = &s[s.iter().position(|&c| !is_space(c)).unwrap_or(s.len())..];
    &s[0..s
        .iter()
        .rposition(|&c| !is_space(c))
        .map(|i| i + 1)
        .unwrap_or(0)]
}

/// Display potentially non-UTF8 string in a lossy way, *without* allocating a new buffer to hold
/// the string.
pub fn display_utf8_lossy(input: &[u8]) -> impl fmt::Display + '_ {
    struct StringDisplay<'a>(&'a [u8]);

    impl<'a> fmt::Display for StringDisplay<'a> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut input = self.0;
            loop {
                match str::from_utf8(input) {
                    Ok(valid) => {
                        f.write_str(valid)?;
                        break;
                    }
                    Err(error) => {
                        let (valid, after_valid) = input.split_at(error.valid_up_to());
                        f.write_str(str::from_utf8(valid).unwrap())?;
                        f.write_str("\u{FFFD}")?;

                        if let Some(invalid_sequence_length) = error.error_len() {
                            input = &after_valid[invalid_sequence_length..]
                        } else {
                            break;
                        }
                    }
                }
            }
            Ok(())
        }
    }

    StringDisplay(input)
}

/// Like [`display_utf8_lossy`], returns an `impl fmt::Debug` type that uses debug character escapes
/// and surrounds the string by '"' characters.
pub fn debug_utf8_lossy(input: &[u8]) -> impl fmt::Debug + '_ {
    struct StringDebug<'a>(&'a [u8]);

    impl<'a> fmt::Debug for StringDebug<'a> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fn write_escape_str(f: &mut fmt::Formatter, s: &str) -> fmt::Result {
                for c in s.chars() {
                    for c in c.escape_debug() {
                        f.write_char(c)?;
                    }
                }
                Ok(())
            }

            fn write_escape_bytes(f: &mut fmt::Formatter, s: &[u8]) -> fmt::Result {
                fn to_hex_digit(b: u8) -> Option<char> {
                    if b < 10 {
                        char::from_u32('0' as u32 + b as u32)
                    } else if b < 16 {
                        char::from_u32('a' as u32 + (b as u32 - 10))
                    } else {
                        None
                    }
                }

                for b in s {
                    f.write_str("\\x")?;
                    f.write_char(to_hex_digit(b >> 4).unwrap())?;
                    f.write_char(to_hex_digit(b & 0xf).unwrap())?;
                }
                Ok(())
            }

            f.write_char('"')?;
            let mut input = self.0;
            loop {
                match str::from_utf8(input) {
                    Ok(valid) => {
                        write_escape_str(f, valid)?;
                        break;
                    }
                    Err(error) => {
                        let (valid, after_valid) = input.split_at(error.valid_up_to());
                        write_escape_str(f, str::from_utf8(valid).unwrap())?;

                        if let Some(invalid_sequence_length) = error.error_len() {
                            write_escape_bytes(f, &after_valid[0..invalid_sequence_length])?;
                            input = &after_valid[invalid_sequence_length..];
                        } else {
                            write_escape_bytes(f, &after_valid[0..])?;
                            break;
                        }
                    }
                }
            }
            f.write_char('"')?;

            Ok(())
        }
    }

    StringDebug(input)
}

pub fn read_integer(s: &[u8]) -> Option<i64> {
    read_hex_integer(s).or_else(|| read_dec_integer(s))
}

pub fn read_dec_integer(s: &[u8]) -> Option<i64> {
    let (is_neg, s) = read_neg(s);

    if s.is_empty() {
        return None;
    }

    let mut i: u64 = 0;
    for &c in s {
        let d = from_digit(c)? as u64;
        i = i.checked_mul(10)?.checked_add(d)?;
    }

    if is_neg {
        if i <= i64::MAX as u64 {
            Some(-(i as i64))
        } else if i == i64::MAX as u64 + 1 {
            Some(i64::MIN)
        } else {
            None
        }
    } else {
        i.try_into().ok()
    }
}

pub fn read_hex_integer(s: &[u8]) -> Option<i64> {
    let (is_neg, s) = read_neg(s);

    if s.len() < 3 || s[0] != b'0' || (s[1] != b'x' && s[1] != b'X') {
        return None;
    }

    let mut i: u64 = 0;
    for &c in &s[2..] {
        let d = from_hex_digit(c)? as u64;
        i = i.checked_mul(16)?.checked_add(d)?;
    }

    if is_neg {
        if i <= i64::MAX as u64 {
            Some(-(i as i64))
        } else if i == i64::MAX as u64 + 1 {
            Some(i64::MIN)
        } else {
            None
        }
    } else {
        i.try_into().ok()
    }
}

pub fn read_float(s: &[u8]) -> Option<f64> {
    read_hex_float(s).or_else(|| read_dec_float(s))
}

pub fn read_dec_float(s: &[u8]) -> Option<f64> {
    let s = str::from_utf8(s).ok()?;
    str::parse(s).ok()
}

pub fn read_hex_float(s: &[u8]) -> Option<f64> {
    const MAX_SIGNIFICANT_DIGITS: u32 = 30;

    let (is_neg, s) = read_neg(s);

    if s.len() < 3 || s[0] != b'0' || (s[1] != b'x' && s[1] != b'X') {
        return None;
    }

    let mut significant_digits: u32 = 0;
    let mut non_significant_digits: u32 = 0;
    let mut found_dot = false;
    let mut base: f64 = 0.0;
    let mut exp: i32 = 0;
    let mut i = 2;

    while i < s.len() {
        let c = s[i];
        if c == b'.' {
            if found_dot {
                return None;
            }
            found_dot = true;
        } else if let Some(d) = from_hex_digit(c) {
            if significant_digits == 0 && d == 0 {
                non_significant_digits += 1;
            } else if significant_digits < MAX_SIGNIFICANT_DIGITS {
                significant_digits += 1;
                base = (base * 16.0) + d as f64;
            } else {
                // ignore the digit, but count it towards the expontent
                exp = exp.checked_add(4)?;
            }
            if found_dot {
                // Correct exponent for the fractional part
                exp = exp.checked_sub(4)?;
            }
        } else {
            break;
        }
        i += 1;
    }

    if non_significant_digits + significant_digits == 0 {
        return None;
    }

    if i + 1 < s.len() && (s[i] == b'p' || s[i] == b'P') {
        let (exp_neg, exp_s) = read_neg(&s[i + 1..]);
        let mut exp1: i32 = 0;
        for &c in exp_s {
            let d = from_digit(c)?;
            exp1 = exp1.saturating_mul(10).saturating_add(d as i32);
        }
        if exp_neg {
            exp1 = -exp1;
        }
        exp = exp.saturating_add(exp1);
    } else if i != s.len() {
        return None;
    }

    if is_neg {
        base = -base;
    }

    Some(ldexp(base, exp))
}

fn ldexp(mut val: f64, exp: i32) -> f64 {
    fn iexp2(exp: i32) -> f64 {
        assert!(exp >= -1022 && exp <= 1023);
        f64::from_bits(((exp + 1023) as u64) << 52)
    }
    fn extract_exp(val: f64) -> u64 {
        (val.to_bits() >> 52) & 0x7ff
    }
    fn extract_mantissa(val: f64) -> u64 {
        val.to_bits() & ((1 << 52) - 1)
    }

    let mut orig_exp = extract_exp(val) as i32;
    let mantissa = extract_mantissa(val);
    if orig_exp == 0 {
        if mantissa == 0 {
            return val;
        }
        // input is subnormal
        val *= iexp2(54);
        orig_exp = extract_exp(val) as i32 - 54;
    } else if orig_exp == 2047 {
        // input is NaN of Inf
        return val;
    }

    let new_exp = orig_exp.saturating_add(exp);
    if new_exp >= 2047 {
        f64::copysign(f64::INFINITY, val) // overflow
    } else if new_exp <= -54 {
        f64::copysign(0.0, val) // underflow
    } else if new_exp <= 0 {
        // output is subnormal
        let sign = val.is_sign_negative() as u64;
        f64::from_bits((sign << 63) | (((new_exp + 54) as u64) << 52) | mantissa) * iexp2(-54)
    } else {
        let sign = val.is_sign_negative() as u64;
        f64::from_bits((sign << 63) | ((new_exp as u64) << 52) | mantissa)
    }
}

/// Read an optional '-' or '+' prefix and return whether the value is negated (starts with a '-'
/// prefix).
pub fn read_neg(s: &[u8]) -> (bool, &[u8]) {
    if s.len() > 0 {
        if s[0] == b'-' {
            (true, &s[1..])
        } else if s[0] == b'+' {
            (false, &s[1..])
        } else {
            (false, s)
        }
    } else {
        (false, s)
    }
}

pub const ALERT_BEEP: u8 = 0x07;
pub const BACKSPACE: u8 = 0x08;
pub const VERTICAL_TAB: u8 = 0x0b;
pub const FORM_FEED: u8 = 0x0c;

pub fn is_newline(c: u8) -> bool {
    c == b'\n' || c == b'\r'
}

pub fn is_space(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == VERTICAL_TAB || c == FORM_FEED || is_newline(c)
}

/// Is this character a Lua alpha, which is A-Z, a-z, and _
pub fn is_alpha(c: u8) -> bool {
    (c >= b'a' && c <= b'z') || (c >= b'A' && c <= b'Z') || c == b'_'
}

pub fn from_digit(c: u8) -> Option<u8> {
    if c >= b'0' && c <= b'9' {
        Some(c - b'0')
    } else {
        None
    }
}

pub fn is_digit(c: u8) -> bool {
    from_digit(c).is_some()
}

pub fn from_hex_digit(c: u8) -> Option<u8> {
    if c >= b'0' && c <= b'9' {
        Some(c - b'0')
    } else if c >= b'a' && c <= b'f' {
        Some(10 + c - b'a')
    } else if c >= b'A' && c <= b'F' {
        Some(10 + c - b'A')
    } else {
        None
    }
}

pub fn is_hex_digit(c: u8) -> bool {
    from_hex_digit(c).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trim_whitespace_works() {
        assert_eq!(trim_whitespace(b"foo  "), b"foo");
        assert_eq!(trim_whitespace(b"  foo"), b"foo");
        assert_eq!(trim_whitespace(b"  foo  "), b"foo");
        assert_eq!(trim_whitespace(b"   "), b"");
        assert_eq!(trim_whitespace(b""), b"");
        assert_eq!(trim_whitespace(b" . "), b".");
    }
}
