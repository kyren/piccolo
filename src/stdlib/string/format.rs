//! An implementation of C's `sprintf` / Lua's `string.format`
//!
//! References:
//! - [Lua 5.4 Manual on `string.format`](https://www.lua.org/manual/5.4/manual.html#pdf-string.format)
//! - [glibc manual 12.12: Formatted Output](https://www.gnu.org/software/libc/manual/html_node/Formatted-Output.html)
//! - [Documentation of specific meaning of `%g`](https://stackoverflow.com/a/54162153)
//! - [Python's formatting docs](https://docs.python.org/3/library/string.html#format-specification-mini-language)
//!
//! Specifier syntax: `"%" [flags] [width] ["." precision] spec`
//!
//! Supported specifiers:
//! - `%%` - a literal `%` character
//! - `%c` - output a raw byte (modulo 256 for integers larger than 255)
//! - `%C` - format a unicode code point as utf8
//! - `%s` - string (prints raw bytes, does not reinterpret as utf8)
//! - `%S` - utf8 string; width and precision are in terms of codepoints
//! - `%d`, `%i` - signed integer
//! - `%u` - unsigned integer (converted to 64 bit signed integer, then interpreted as unsigned)
//! - `%o` - usigned octal integer
//! - `%x`, `%X` - unsigned hexidecimal integer
//! - `%b`, `%B` - unsigned binary integer
//! - `%f`, `%F` - normal form floating point
//! - `%g`, `%G` - compact floating point
//! - `%e`, `%E` - exponential form floating point
//! - `%a`, `%A` - hexidecimal floating point
//! - `%p` - format a value as a pointer, for non-literal values
//! - `%q` - format a value as an escaped Lua literal; supports
//!         `nil`, `bool`, `string`, `integer`, or `float` (formatted as a hex float)
//!
//! Supported flags:
//! - `-`: left align
//! - `0`: zero pad
//! - ` ` (space): include space in sign position for positive numbers
//! - `+`: include sign for positive numbers, overriding space if both are specified
//! - `#`: alternate mode
//!     - On floats, preserve a trailing decimal point
//!     - On hex/octal/binary integers, prefix with the format
//!       (`0x`, `0`, and `0b`, respectively)
//!
//! Width and precision are supported, but are limited to `ARG_MAX` (99).
//! - Width: specify the minimum width to pad to
//!     - Ignored on `%q`
//! - Precision:
//!     - For `%s`, truncates the string to the specified length
//!     - For integer specs, zero-pads the number to the specified length
//!       (may differ from `width`, which is still padded with spaces)
//!     - For floats, specifies the number of digits of precision to use
//! - This implementation supports using `*` to read width/precision
//!   from the argument list. The argument is converted to an integer;
//!   for `width`, if the argument is negative, the value will be left
//!   aligned, and use `abs(arg)` as the width.
//!
//! Compatibility notes:
//! - This should match output of PRLua's `string.format` / POSIX sprintf
//!   in the vast majority of cases, but there will be differences.
//! - This implementation is not locale-aware, and assumes LC_ALL=C.
//! - Floating point formatting may differ slightly:
//!     - `%f` specifier does not support `#` to require a trailing decimal
//!        point, due to implementation limitations
//!     - formatting of subnormal numbers has not been thoroughly tested,
//!       may have rounding errors.
//! - PRLua does not support `%F` (uppercase float; only differs for inf/nan)
//! - PRLua does not support the C23 `%b`/`%B` (binary unsigned int) specifiers
//! - PRLua's `%q` represents `math.mininteger` as `0x8000000000000000`, but
//!   piccolo represents it as `(-9223372036854775807-1)`
//! - PRLua's `%q` passes any byte above 127 through as a raw byte; this
//!   implementation passes through valid UTF-8 codepoints, but escapes
//!   other bytes.
//! - (Matching PRLua) No support for C style value length specifiers.
//!   (such `%lld` for `u64`s)
//! - (Matching PRLua) No support for `%n` (length write-back)
//! - Supports `%C` to format a unicode code point as UTF-8.
//!   (in C, this is either `%C` or `%lc`, if the locale supports it)
//! - Supports `%S`, a unicode-aware variant of `%s`; width and precision
//!   are specified in terms of codepoints rather than bytes.

use core::{char, pin::Pin};
use std::io::Write;

use gc_arena::{Collect, Gc};
use thiserror::Error;

use crate::meta_ops;
use crate::{Context, Error, Execution, FromValue, Function, Sequence, SequencePoll, Stack, Value};

mod float;
mod parse;

use float::FloatMode;

const FMT_SPEC: u8 = b'%';
const ARG_MAX: u32 = 99;

#[derive(Debug, Error)]
enum FormatError {
    #[error("invalid format specifier {:?}", *.0 as char)]
    BadSpec(u8),
    #[error("invalid format specifier; precision is limited to {}", ARG_MAX)]
    BadPrecision,
    #[error("invalid format specifier; width is limited to {}", ARG_MAX)]
    BadWidth,
    #[error("invalid format specifier; flag {:?} is not supported for {}", *.1, *.0 as char)]
    BadFlag(u8, Flags),
    #[error("missing value for format specifier {:?}", *.0 as char)]
    MissingValue(u8),
    #[error("value of wrong type for format specifier {:?}; expected {}, found {}", *.0 as char, .1, .2)]
    BadValueType(u8, &'static str, &'static str),
    #[error("value out of range for format specifier {:?}", *.0 as char)]
    ValueOutOfRange(u8),
    #[error("weird floating point value?")]
    BadFloat,
    #[error("Non-utf8 string passed to specifier {:?}", *.0 as char)]
    NonUnicodeString(u8),
}

#[derive(Default, Copy, Clone)]
pub struct Flags(u8);

impl Flags {
    const ALTERNATE: Self = Self(1 << 0);
    const LEFT_ALIGN: Self = Self(1 << 1);
    const ZERO_PAD: Self = Self(1 << 2);
    const SIGN_FORCE: Self = Self(1 << 3);
    const SIGN_SPACE: Self = Self(1 << 4);
    const WIDTH: Self = Self(1 << 5);
    const PRECISION: Self = Self(1 << 6);

    const NONE: Self = Self(0);
    const ALL: Self = Self(0b01111111);

    const UINT_ALLOWED: Self =
        Self(Self::LEFT_ALIGN.0 | Self::ZERO_PAD.0 | Self::WIDTH.0 | Self::PRECISION.0);
    const SINT_ALLOWED: Self = Self(Self::UINT_ALLOWED.0 | Self::SIGN_FORCE.0 | Self::SIGN_SPACE.0);
}

impl Flags {
    fn has(self, flag: Flags) -> bool {
        self.0 & flag.0 == flag.0
    }
}

impl core::ops::BitOr for Flags {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }
}

impl core::ops::BitOrAssign for Flags {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl core::fmt::Debug for Flags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        write!(f, "Flags(")?;
        for (flag, name) in [
            (Self::ALTERNATE, "ALTERNATE"),
            (Self::LEFT_ALIGN, "LEFT_ALIGN"),
            (Self::ZERO_PAD, "ZERO_PAD"),
            (Self::SIGN_FORCE, "SIGN_FORCE"),
            (Self::SIGN_SPACE, "SIGN_SPACE"),
            (Self::WIDTH, "WIDTH"),
            (Self::PRECISION, "PRECISION"),
        ] {
            if self.has(flag) {
                if first {
                    first = false;
                    write!(f, "{}", name)?;
                } else {
                    write!(f, " | {}", name)?;
                }
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}

// Note: if width is specified by a argument, it will be interpreted
// as its absolute value, setting the left align flag if negative.
#[derive(Copy, Clone)]
struct FormatSpecifier {
    spec: u8,
    flags: Flags,
    width: OptionalArg,
    precision: OptionalArg,
}

#[derive(Copy, Clone)]
enum OptionalArg {
    None,
    Arg,
    Specified(u32),
}

#[derive(Default, Clone, Copy)]
struct FormatArgs {
    width: usize,
    precision: Option<usize>,
    left_align: bool,
    zero_pad: bool,
    alternate: bool,
    upper: bool,
    flags: Flags,
}

impl FormatSpecifier {
    fn check_flags(&self, allowed: Flags) -> Result<(), FormatError> {
        let leftover = self.flags.0 & !allowed.0;
        if leftover != 0 {
            Err(FormatError::BadFlag(self.spec, Flags(leftover)))
        } else {
            Ok(())
        }
    }

    fn get_arg<'gc>(
        &self,
        arg: OptionalArg,
        values: &mut impl Iterator<Item = Value<'gc>>,
    ) -> Result<(Option<usize>, bool), FormatError> {
        match arg {
            OptionalArg::None => Ok((None, false)),
            OptionalArg::Arg => {
                let int = self.next_int(values)?;
                let negative = int < 0;
                let abs = int.unsigned_abs();
                if abs > ARG_MAX as u64 {
                    return Err(FormatError::ValueOutOfRange(self.spec));
                }
                Ok((Some(abs as usize), negative))
            }
            OptionalArg::Specified(val) => Ok((Some(val as usize), false)),
        }
    }

    fn common_args<'gc>(
        &self,
        values: &mut impl Iterator<Item = Value<'gc>>,
    ) -> Result<FormatArgs, FormatError> {
        let (width, width_neg) = self.get_arg(self.width, values)?;
        let (precision, _) = self.get_arg(self.precision, values)?;
        Ok(FormatArgs {
            width: width.unwrap_or(0),
            precision,
            left_align: self.flags.has(Flags::LEFT_ALIGN) || width_neg,
            zero_pad: self.flags.has(Flags::ZERO_PAD)
                && !(self.flags.has(Flags::LEFT_ALIGN) || width_neg),
            alternate: self.flags.has(Flags::ALTERNATE),
            upper: self.spec.is_ascii_uppercase(),
            flags: self.flags,
        })
    }

    fn next_value<'gc>(
        &self,
        values: &mut impl Iterator<Item = Value<'gc>>,
    ) -> Result<Value<'gc>, FormatError> {
        values.next().ok_or(FormatError::MissingValue(self.spec))
    }

    fn next_int<'gc>(
        &self,
        values: &mut impl Iterator<Item = Value<'gc>>,
    ) -> Result<i64, FormatError> {
        let val = self.next_value(values)?;
        let int = val
            .to_integer()
            .ok_or_else(|| FormatError::BadValueType(self.spec, "integer", val.type_name()))?;
        Ok(int)
    }

    fn next_float<'gc>(
        &self,
        values: &mut impl Iterator<Item = Value<'gc>>,
    ) -> Result<f64, FormatError> {
        let val = self.next_value(values)?;
        let float = val
            .to_number()
            .ok_or_else(|| FormatError::BadValueType(self.spec, "number", val.type_name()))?;
        Ok(float)
    }
}

impl FormatArgs {
    fn sign_char(&self, negative: bool) -> &'static [u8] {
        if negative {
            b"-"
        } else if self.flags.has(Flags::SIGN_FORCE) {
            b"+"
        } else if self.flags.has(Flags::SIGN_SPACE) {
            b" "
        } else {
            b""
        }
    }

    /// Returns the width to which an integer should be zero-padded to,
    /// if zero-padding is requested.
    fn integer_zeroed_width(&self, prefix: &[u8]) -> usize {
        if let Some(p) = self.precision {
            p
        } else if self.zero_pad {
            self.width.saturating_sub(prefix.len())
        } else {
            0
        }
    }

    /// Write the initial space padding, prefix (sign) and zero padding
    /// for a format specifier.  This returns a [`PadScope`], which must
    /// be used to finish the trailing padding by calling [`PadScope::finish_pad`].
    fn pad_num_before<W: Write>(
        &self,
        w: &mut W,
        len: usize,
        zeroed_width: usize,
        prefix: &[u8],
    ) -> Result<PadScope, std::io::Error> {
        // right: [    ][-][0000][nnnn]
        // left:  [-][0000][nnnn][    ]
        let zero_padding = zeroed_width.saturating_sub(len);
        let space_padding = self.width.saturating_sub(zero_padding + prefix.len() + len);
        if space_padding > 0 && !self.left_align {
            write_padding(w, b' ', space_padding)?;
        }
        if !prefix.is_empty() {
            w.write_all(prefix)?;
        }
        if zero_padding > 0 {
            write_padding(w, b'0', zero_padding)?;
        }
        let trailing_padding = if self.left_align { space_padding } else { 0 };
        Ok(PadScope { trailing_padding })
    }
}

#[must_use]
struct PadScope {
    trailing_padding: usize,
}

impl PadScope {
    fn finish_pad<W: Write>(self, w: &mut W) -> Result<(), std::io::Error> {
        if self.trailing_padding > 0 {
            write_padding(w, b' ', self.trailing_padding)?;
        }
        Ok(())
    }
}

/// Write the given byte repeated `count` times.
fn write_padding<W: Write>(w: &mut W, byte: u8, count: usize) -> Result<(), std::io::Error> {
    let buf = [byte; 16];
    let mut remaining = count;
    while remaining > 0 {
        match w.write(&buf[..remaining.min(buf.len())]) {
            Ok(n) => remaining -= n,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        }
    }
    Ok(())
}

fn integer_length(i: u64) -> usize {
    1 + i.checked_ilog10().unwrap_or(0) as usize
}
fn integer_length_hex(i: u64) -> usize {
    1 + i.checked_ilog2().unwrap_or(0) as usize / 4
}
fn integer_length_octal(i: u64) -> usize {
    1 + i.checked_ilog2().unwrap_or(0) as usize / 3
}
fn integer_length_binary(i: u64) -> usize {
    1 + i.checked_ilog2().unwrap_or(0) as usize
}

fn memchr(needle: u8, haystack: &[u8]) -> Option<usize> {
    haystack.iter().position(|&b| b == needle)
}

pub fn string_format<'gc>(
    ctx: Context<'gc>,
    stack: Stack<'gc, '_>,
) -> Result<impl Sequence<'gc>, Error<'gc>> {
    let str = crate::string::String::from_value(ctx, stack.get(0))?;
    Ok(FormatState {
        buf: Vec::new(),
        arg_count: stack.len(),
        str,
        index: 0,
        value_index: 1,
        inner: FormatStateInner::Start,
    })
}

#[derive(Collect)]
#[collect(no_drop)]
struct FormatState<'gc> {
    buf: Vec<u8>,
    arg_count: usize,
    str: crate::string::String<'gc>,
    index: usize,
    value_index: usize,
    #[collect(require_static)]
    inner: FormatStateInner,
}

enum FormatStateInner {
    Start,
    EvaluateCallback {
        spec: FormatSpecifier,
        dest: EvalContinuation,
    },
    End,
}

enum EvalPoll<'gc> {
    Done,
    PassValue {
        value: Value<'gc>,
        then: EvalContinuation,
    },
    Call {
        call: meta_ops::MetaCall<'gc, 1>,
        then: EvalContinuation,
    },
}

#[derive(Copy, Clone)]
enum EvalContinuation {
    Init,
    ToStringResult(FormatArgs),
    UnicodeToStringResult(FormatArgs),
}

impl<'gc> Sequence<'gc> for FormatState<'gc> {
    fn poll(
        self: Pin<&mut Self>,
        ctx: Context<'gc>,
        _exec: Execution<'gc, '_>,
        stack: Stack<'gc, '_>,
    ) -> Result<SequencePoll<'gc>, Error<'gc>> {
        step(ctx, self.get_mut(), stack)
    }
}

impl<'gc> EvalPoll<'gc> {
    fn from_metaresult(res: meta_ops::MetaResult<'gc, 1>, then: EvalContinuation) -> Self {
        match res {
            meta_ops::MetaResult::Value(value) => EvalPoll::PassValue { value, then },
            meta_ops::MetaResult::Call(call) => EvalPoll::Call { call, then },
        }
    }
}

fn step<'gc>(
    ctx: Context<'gc>,
    state: &mut FormatState<'gc>,
    mut stack: Stack<'gc, '_>,
) -> Result<SequencePoll<'gc>, Error<'gc>> {
    let mut float_buf = [0u8; 300];

    loop {
        match state.inner {
            FormatStateInner::Start => {
                if let Some(next) =
                    memchr(FMT_SPEC, &state.str[state.index..]).map(|n| n + state.index)
                {
                    if next != state.index {
                        state.buf.write_all(&state.str[state.index..next])?;
                    }

                    let (spec, spec_end) = parse::parse_specifier(state.str.as_bytes(), next)?;
                    state.index = spec_end;
                    assert!(state.index > next);

                    state.inner = FormatStateInner::EvaluateCallback {
                        spec,
                        dest: EvalContinuation::Init,
                    };
                } else {
                    if state.index < state.str.as_bytes().len() {
                        state.buf.write_all(&state.str[state.index..])?;
                    }
                    state.inner = FormatStateInner::End;
                }
            }
            FormatStateInner::EvaluateCallback { spec, dest } => {
                let result = stack.get(state.arg_count);
                stack.resize(state.arg_count);

                let remaining_args = state.arg_count - state.value_index;
                let mut values_iter = stack[state.value_index..state.arg_count].iter();
                let poll = evaluate_continuation(
                    ctx,
                    &mut state.buf,
                    dest,
                    spec,
                    Some(result),
                    &mut (&mut values_iter).copied(),
                    &mut float_buf,
                )?;
                state.value_index += remaining_args - values_iter.as_slice().len();

                match poll {
                    EvalPoll::PassValue { value, then } => {
                        state.inner = FormatStateInner::EvaluateCallback { spec, dest: then };
                        stack.push_back(value);
                        continue;
                    }
                    EvalPoll::Call { call, then } => {
                        state.inner = FormatStateInner::EvaluateCallback { spec, dest: then };
                        let bottom = stack.len();
                        stack.extend(call.args);
                        return Ok(SequencePoll::Call {
                            function: call.function,
                            bottom,
                        });
                    }
                    EvalPoll::Done => {
                        state.inner = FormatStateInner::Start;
                    }
                }
            }
            FormatStateInner::End => {
                stack.replace(ctx, ctx.intern(&state.buf));
                return Ok(SequencePoll::Return);
            }
        };
    }
}

fn evaluate_continuation<'gc, W: Write>(
    ctx: Context<'gc>,
    w: &mut W,
    cont: EvalContinuation,
    spec: FormatSpecifier,
    result: Option<Value<'gc>>,
    values: &mut impl Iterator<Item = Value<'gc>>,
    float_buf: &mut [u8; 300],
) -> Result<EvalPoll<'gc>, Error<'gc>> {
    match cont {
        EvalContinuation::ToStringResult(args) => {
            let val = result.unwrap_or_default();
            let string = val
                .into_string(ctx)
                .ok_or_else(|| FormatError::BadValueType(spec.spec, "string", val.type_name()))?;

            let len = string.len() as usize;
            let truncated_len = args.precision.unwrap_or(len).min(len);

            let pad = args.pad_num_before(w, truncated_len, 0, b"")?;
            w.write_all(&string[..truncated_len])?;
            pad.finish_pad(w)?;
            Ok(EvalPoll::Done)
        }
        EvalContinuation::UnicodeToStringResult(args) => {
            let val = result.unwrap_or_default();
            let string = val
                .into_string(ctx)
                .ok_or_else(|| FormatError::BadValueType(spec.spec, "string", val.type_name()))?;

            let string = core::str::from_utf8(string.as_bytes())
                .map_err(|_| FormatError::NonUnicodeString(spec.spec))?;

            let precision = args.precision.unwrap_or(string.len());

            // Find the end byte of the string when truncated to `precision` chars.
            let (end_byte, end_char) = string
                .char_indices()
                .map(|(i, c)| i + c.len_utf8())
                .zip(1..precision + 1)
                .last()
                .unwrap_or((0, 0));

            let pad = args.pad_num_before(w, end_char, 0, b"")?;
            w.write_all(string[..end_byte].as_bytes())?;
            pad.finish_pad(w)?;
            Ok(EvalPoll::Done)
        }
        EvalContinuation::Init => evaluate_specifier(ctx, w, spec, values, float_buf),
    }
}

fn evaluate_specifier<'gc, W: Write>(
    ctx: Context<'gc>,
    w: &mut W,
    spec: FormatSpecifier,
    values: &mut impl Iterator<Item = Value<'gc>>,
    float_buf: &mut [u8; 300],
) -> Result<EvalPoll<'gc>, Error<'gc>> {
    match spec.spec {
        b'%' => {
            // escaped %
            spec.check_flags(Flags::NONE)?;
            w.write_all(b"%")?;
        }
        b'c' => {
            // char
            spec.check_flags(Flags::LEFT_ALIGN | Flags::WIDTH)?;
            let args = spec.common_args(values)?;

            let int = spec.next_int(values)?;
            let byte = (int % 256) as u8;

            let pad = args.pad_num_before(w, 1, 0, b"")?;
            w.write_all(&[byte])?;
            pad.finish_pad(w)?;
        }
        b'C' => {
            // wide char
            spec.check_flags(Flags::LEFT_ALIGN | Flags::WIDTH)?;
            let args = spec.common_args(values)?;

            let int = spec.next_int(values)?;
            let c: char = (u32::try_from(int).ok().and_then(char::from_u32))
                .ok_or(FormatError::ValueOutOfRange(spec.spec))?;

            let pad = args.pad_num_before(w, 1, 0, b"")?;
            write!(w, "{}", c)?;
            pad.finish_pad(w)?;
        }
        b's' => {
            // string
            spec.check_flags(Flags::LEFT_ALIGN | Flags::WIDTH | Flags::PRECISION)?;
            let args = spec.common_args(values)?;
            let val = spec.next_value(values)?;
            // Continue in `evaluate_continuation`
            return Ok(EvalPoll::from_metaresult(
                meta_ops::tostring(ctx, val)?,
                EvalContinuation::ToStringResult(args),
            ));
        }
        b'S' => {
            // utf-8 string
            spec.check_flags(Flags::LEFT_ALIGN | Flags::WIDTH | Flags::PRECISION)?;
            let args = spec.common_args(values)?;
            let val = spec.next_value(values)?;
            // Continue in `evaluate_continuation`
            return Ok(EvalPoll::from_metaresult(
                meta_ops::tostring(ctx, val)?,
                EvalContinuation::UnicodeToStringResult(args),
            ));
        }
        b'd' | b'i' => {
            // signed int
            spec.check_flags(Flags::SINT_ALLOWED)?;
            let args = spec.common_args(values)?;

            let int = spec.next_int(values)?;
            let value = int.unsigned_abs();
            let len = integer_length(value);
            let sign = args.sign_char(int < 0);

            let zeroed_width = args.integer_zeroed_width(sign);
            let pad = args.pad_num_before(w, len, zeroed_width, sign)?;
            write!(w, "{}", value)?;
            pad.finish_pad(w)?;
        }
        s @ (b'u' | b'o' | b'x' | b'X' | b'b' | b'B') => {
            // unsigned int
            if s == b'u' {
                spec.check_flags(Flags::UINT_ALLOWED)?;
            } else {
                spec.check_flags(Flags::UINT_ALLOWED | Flags::ALTERNATE)?;
            }
            let args = spec.common_args(values)?;
            let int = spec.next_int(values)? as u64;

            let len = match s {
                b'x' | b'X' => integer_length_hex(int),
                b'b' | b'B' => integer_length_binary(int),
                b'o' => integer_length_octal(int),
                b'u' => integer_length(int),
                _ => unreachable!(),
            };
            let prefix: &[u8] = match (args.alternate, s) {
                (true, b'x') => b"0x",
                (true, b'X') => b"0X",
                (true, b'b') => b"0b",
                (true, b'B') => b"0B",
                (true, b'o') => b"0",
                (true, b'u') => b"",
                (_, _) => b"",
            };

            let zeroed_width = args.integer_zeroed_width(prefix);
            let pad = args.pad_num_before(w, len, zeroed_width, prefix)?;
            match s {
                b'x' => write!(w, "{:x}", int)?,
                b'X' => write!(w, "{:X}", int)?,
                b'b' => write!(w, "{:b}", int)?,
                b'B' => write!(w, "{:b}", int)?,
                b'o' => write!(w, "{:o}", int)?,
                b'u' => write!(w, "{}", int)?,
                _ => unreachable!(),
            }
            pad.finish_pad(w)?;
        }
        c @ (b'g' | b'G' | b'e' | b'E' | b'f' | b'F' | b'a' | b'A') => {
            // floating point number
            spec.check_flags(Flags::ALL)?;
            let args = spec.common_args(values)?;

            let mode = match c {
                b'g' | b'G' => FloatMode::Compact,
                b'e' | b'E' => FloatMode::Exponent,
                b'f' | b'F' => FloatMode::Normal,
                b'a' | b'A' => FloatMode::Hex,
                _ => unreachable!(),
            };
            let float = spec.next_float(values)?;
            float::write_float(w, float, mode, args, float_buf)?;
        }
        b'p' => {
            // object pointer
            spec.check_flags(Flags::LEFT_ALIGN | Flags::WIDTH)?;
            let args = spec.common_args(values)?;

            // TODO: Intentionally leaking addresses is a bad idea
            // This defeats ASLR and simplifies potential exploits.
            // (though addrs are currently already exposed through tostring on fns/tables)
            let val = spec.next_value(values)?;
            let ptr = match val {
                Value::Nil | Value::Boolean(_) | Value::Integer(_) | Value::Number(_) => 0,
                Value::String(str) => str.as_ptr() as usize,
                Value::Table(t) => Gc::as_ptr(t.into_inner()) as usize,
                Value::Function(Function::Closure(c)) => Gc::as_ptr(c.into_inner()) as usize,
                Value::Function(Function::Callback(c)) => Gc::as_ptr(c.into_inner()) as usize,
                Value::Thread(t) => Gc::as_ptr(t.into_inner()) as usize,
                Value::UserData(u) => Gc::as_ptr(u.into_inner()) as usize,
            };

            if ptr != 0 {
                let len = integer_length_hex(ptr as u64);
                let pad = args.pad_num_before(w, len, 0, b"0x")?;
                write!(w, "{:x}", ptr)?;
                pad.finish_pad(w)?;
            } else {
                let null_str = "(null)";
                let pad = args.pad_num_before(w, null_str.len(), 0, b"")?;
                write!(w, "{}", null_str)?;
                pad.finish_pad(w)?;
            }
        }
        b'q' => {
            // Lua escape
            spec.check_flags(Flags::NONE)?;
            let val = spec.next_value(values)?;
            write_escaped_value(w, val, spec)?;
        }
        c => return Err(FormatError::BadSpec(c).into()),
    }
    Ok(EvalPoll::Done)
}

fn write_escaped_value<'gc, W: Write>(
    w: &mut W,
    val: Value<'gc>,
    spec: FormatSpecifier,
) -> Result<(), Error<'gc>> {
    match val {
        Value::Nil => {
            write!(w, "nil")?;
            Ok(())
        }
        Value::Boolean(b) => {
            write!(w, "{}", b)?;
            Ok(())
        }
        Value::Integer(i) => {
            if i == i64::MIN {
                // MIN is not representable as positive, would be lexed as float
                // PRLua outputs 0x8000000000000000 here, which is interpreted as
                // a signed integer, but piccolo doesn't; instead we output a simple
                // expression to avoid lexer issues.
                write!(w, "({}-1)", i + 1)?;
            } else {
                write!(w, "{}", i)?;
            }
            Ok(())
        }
        Value::Number(n) => {
            // These encodings match PRLua's %q, but by the spec they just
            // need to be able to round-trip as Lua expressions.
            if n.is_finite() {
                float::write_hex_float(w, n, FormatArgs::default())?;
            } else if n.is_nan() {
                write!(w, "(0/0)")?;
            } else {
                // +/- infinity
                let sign = if n.is_sign_negative() { "-" } else { "" };
                write!(w, "{}1e9999", sign)?;
            }
            Ok(())
        }
        Value::String(str) => {
            write!(w, "\"")?;
            for seg in str.as_bytes().utf8_chunks() {
                let mut valid = seg.valid().chars().peekable();
                while let Some(c) = valid.next() {
                    match c {
                        c @ ('\\' | '"') => write!(w, "\\{}", c)?,
                        '\n' => write!(w, "\\\n")?,
                        c if c.is_ascii_control() => {
                            if matches!(valid.peek(), Some('0'..='9')) {
                                write!(w, "\\{:03}", c as u32)?
                            } else {
                                write!(w, "\\{}", c as u32)?
                            }
                        }
                        c => write!(w, "{}", c)?,
                    }
                }
                for c in seg.invalid() {
                    write!(w, "\\{}", *c)?;
                }
            }
            write!(w, "\"")?;
            Ok(())
        }
        _ => Err(FormatError::BadValueType(spec.spec, "constant", val.type_name()).into()),
    }
}
