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
//! - `%%`
//! - `%c` - raw byte (mod 256)
//! - `%C` - format a unicode code point as utf8
//! - `%s` - string (prints raw bytes, does not reinterpret as utf8)
//! - `%S` - utf8 string; width and precision are in terms of codepoints
//! - `%d`, `%i` - signed integer
//! - `%u` - unsigned int (converted to 64 bit signed integer, then
//!          interpreted as unsigned)
//! - `%o` - usigned octal int
//! - `%x`, `%X` - unsigned hex int
//! - `%b`, `%B` - unsigned binary int
//! - `%g`, `%G` - compact floating point
//! - `%f`, `%F` - normal form floating point
//! - `%e`, `%E` - exponential form floating point
//! - `%a`, `%A` - hexidecimal floating point
//! - `%p` - format a value as a pointer, for non-literal values
//! - `%q` - format a value as an escaped lua literal; nil, bool,
//!          string, integer, or float (formatted as a hex float)
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
//! Width and precision are supported, but are limited to 255.
//! - Width: specify the minimum width to pad to
//!     - Ignored on `%q`
//! - Precision:
//!     - For `%s`, truncates the string to the specified length
//!     - For integer specs, zero-pad the number to the specified length
//!       (may differ from `width`, which is still padded with spaces)
//!     - For floats, specify the number of digits of precision to use
//! - This implementation supports using `*` to read width/precision
//!   from the argument list. The argument is converted to an integer;
//!   for `width`, if the argument is negative, the value will be left
//!   aligned, and use `abs(arg)` as the width.
//!
//! Compatability notes:
//! - Should match output of PRLua's `string.format` / POSIX sprintf
//!   in the vast majority of cases, but there will be differences.
//! - Floating point formatting may differ slightly:
//!     - `%f` specifier does not support `#` to require trailing decimal,
//!        due to implementation limitations
//!     - formatting of subnormal numbers has not been thoroughly tested,
//!       may have rounding errors.
//! - PRLua does not support `%F` (uppercase float; only differs in inf/nan case)
//! - PRLua does not support the C23 `%b`/`%B` (binary unsigned int) specifiers
//! - PRLua's `%q` represents math.mininteger as `0x8000000000000000`, but
//!   piccolo represents it as `(-9223372036854775807-1)`
//! - PRLua's `%q` passes any byte above 127 through as a raw byte; this
//!   implementation passes through valid UTF-8 codepoints, but escapes
//!   other bytes.
//! - (Matching PRLua) No support for C style value length specifiers.
//! - (Matching PRLua) No support for `%n` (length write-back)
//! - Supports `%C` to format a unicode code point as UTF-8.
//!   (in C, this is either `%C` or `%lc`, if the locale supports it)
//! - Supports `%S`, a unicode-aware variant of `%s`; width and precision
//!   are specified in terms of codepoints rather than bytes.

use core::{char, pin::Pin};
use std::io::Write;

use float::FloatMode;
use gc_arena::{Collect, Gc};
use thiserror::Error;

use crate::{
    meta_ops::{self, MetaResult},
    Context, Error, Execution, FromValue, Function, Sequence, SequencePoll, Stack, Value,
};

const FMT_SPEC: u8 = b'%';
const ARG_MAX: u32 = 99;

#[derive(Debug, Error)]
enum FormatError {
    #[error("invalid format specifier {:?}", *.0 as char)]
    BadSpec(u8),
    #[error("invalid format specifier; precision is limited to {}", u8::MAX)]
    BadPrecision,
    #[error("invalid format specifier; width is limited to {}", u8::MAX)]
    BadWidth,
    #[error("invalid format specifier; flag is not supported for {}", *.0 as char)]
    BadFlag(u8),
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

// TODO: useful Debug impl for flags for errors?
#[derive(Debug, Default, Copy, Clone)]
pub struct Flags(u8);

impl Flags {
    const NONE: Self = Self(0);
    const ALL: Self = Self(u8::MAX);
    const UINT: Self =
        Self(Self::LEFT_ALIGN.0 | Self::ZERO_PAD.0 | Self::WIDTH.0 | Self::PRECISION.0);
    const SINT: Self = Self(Self::UINT.0 | Self::SIGN_FORCE.0 | Self::SIGN_SPACE.0);

    const ALTERNATE: Self = Self(1 << 0);
    const LEFT_ALIGN: Self = Self(1 << 1);
    const ZERO_PAD: Self = Self(1 << 2);
    const SIGN_FORCE: Self = Self(1 << 3);
    const SIGN_SPACE: Self = Self(1 << 4);
    const WIDTH: Self = Self(1 << 5);
    const PRECISION: Self = Self(1 << 6);
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
        if self.flags.0 & !allowed.0 != 0 {
            Err(FormatError::BadFlag(self.spec))
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
    fn sign_char(&self, is_negative: bool) -> &'static [u8] {
        if is_negative {
            b"-"
        } else if self.flags.has(Flags::SIGN_FORCE) {
            b"+"
        } else if self.flags.has(Flags::SIGN_SPACE) {
            b" "
        } else {
            b""
        }
    }

    fn integer_zeroed_width(&self, prefix: &[u8]) -> usize {
        if let Some(p) = self.precision {
            p
        } else if self.zero_pad {
            self.width.saturating_sub(prefix.len())
        } else {
            0
        }
    }

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

fn utf8_width(b: u8) -> Option<usize> {
    match b {
        _ if b & 0b1000_0000 == 0b0000_0000 => Some(1),
        _ if b & 0b1110_0000 == 0b1100_0000 => Some(2),
        _ if b & 0b1111_0000 == 0b1110_0000 => Some(3),
        _ if b & 0b1111_1000 == 0b1111_0000 => Some(4),
        _ => None,
    }
}

fn is_utf8_trailer(b: u8) -> bool {
    b & 0b1100_0000 == 0b1000_0000
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

// write_all, but for a single byte repeated `count` times.
fn write_padding<W>(w: &mut W, byte: u8, count: usize) -> Result<(), std::io::Error>
where
    W: Write,
{
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

// Remove trailing zeroes from a formatted number without changing its value.
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
            let mut i = 0;
            let bytes = str.as_bytes();
            while let Some(c) = bytes.get(i).copied() {
                match c {
                    c @ (b' ' | b'!' | b'#'..=b'[' | b']'..=b'~') => {
                        w.write_all(&[c])?;
                    }
                    c @ (b'\\' | b'"') => write!(w, "\\{}", c as char)?,
                    b'\n' => write!(w, "\\\n")?,
                    // TODO: is \r handling locale-dependent? (needs testing on windows)
                    b'\r' => write!(w, "\\13")?,
                    c @ 0..=127 => write!(w, "\\{}", c)?,
                    c => {
                        if let Some(utf8) = utf8_width(c)
                            .and_then(|len| bytes.get(i..i + len))
                            .filter(|b| b[1..].iter().all(|c| is_utf8_trailer(*c)))
                        {
                            w.write_all(utf8)?;
                            i += utf8.len();
                            continue;
                        } else {
                            write!(w, "\\{}", c)?;
                        }
                    }
                }
                i += 1;
            }
            write!(w, "\"")?;
            Ok(())
        }
        _ => Err(FormatError::BadValueType(spec.spec, "constant", val.type_name()).into()),
    }
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

impl<'gc> EvalPoll<'gc> {
    fn from_metaresult(res: MetaResult<'gc, 1>, then: EvalContinuation) -> Self {
        match res {
            MetaResult::Value(value) => EvalPoll::PassValue { value, then },
            MetaResult::Call(call) => EvalPoll::Call { call, then },
        }
    }
}

#[derive(Copy, Clone)]
enum EvalContinuation {
    Init,
    ToStringResult(FormatArgs),
    UnicodeToStringResult(FormatArgs),
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
            w.write_all(&string[..end_byte].as_bytes())?;
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
            let c: char = u32::try_from(int)
                .ok()
                .and_then(char::from_u32)
                .ok_or(FormatError::ValueOutOfRange(spec.spec))?;

            let pad = args.pad_num_before(w, 1, 0, b"")?;
            let s = c.encode_utf8(float_buf);
            w.write_all(s.as_bytes())?;
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
            spec.check_flags(Flags::SINT)?;
            let args = spec.common_args(values)?;

            let int = spec.next_int(values)?;
            let len = integer_length(int.unsigned_abs());
            let sign = args.sign_char(int < 0);

            let zeroed_width = args.integer_zeroed_width(sign);
            let pad = args.pad_num_before(w, len, zeroed_width, sign)?;
            write!(w, "{}", int.unsigned_abs())?;
            pad.finish_pad(w)?;
        }
        b'u' => {
            // unsigned int
            spec.check_flags(Flags::UINT)?;
            let args = spec.common_args(values)?;

            let int = spec.next_int(values)? as u64;
            let len = integer_length(int);

            let zeroed_width = args.integer_zeroed_width(b"");
            let pad = args.pad_num_before(w, len, zeroed_width, b"")?;
            write!(w, "{}", int)?;
            pad.finish_pad(w)?;
        }
        b'o' => {
            // octal unsigned int
            spec.check_flags(Flags::UINT | Flags::ALTERNATE)?;
            let args = spec.common_args(values)?;

            let prefix: &[u8] = match spec.flags.has(Flags::ALTERNATE) {
                true => b"0",
                false => b"",
            };

            let int = spec.next_int(values)? as u64;
            let len = integer_length_octal(int);

            let zeroed_width = args.integer_zeroed_width(prefix);
            let pad = args.pad_num_before(w, len, zeroed_width, prefix)?;
            write!(w, "{:o}", int as u64)?;
            pad.finish_pad(w)?;
        }
        b'x' | b'X' => {
            // hex unsigned int
            spec.check_flags(Flags::UINT | Flags::ALTERNATE)?;
            let args = spec.common_args(values)?;

            let prefix: &[u8] = match (spec.flags.has(Flags::ALTERNATE), args.upper) {
                (true, false) => b"0x",
                (true, true) => b"0X",
                (false, _) => b"",
            };

            let int = spec.next_int(values)? as u64;
            let len = integer_length_hex(int);

            let zeroed_width = args.integer_zeroed_width(prefix);
            let pad = args.pad_num_before(w, len, zeroed_width, prefix)?;
            match args.upper {
                false => write!(w, "{:x}", int as u64)?,
                true => write!(w, "{:X}", int as u64)?,
            }
            pad.finish_pad(w)?;
        }
        b'b' | b'B' => {
            // binary unsigned int
            spec.check_flags(Flags::UINT | Flags::ALTERNATE)?;
            let args = spec.common_args(values)?;

            let prefix: &[u8] = match (spec.flags.has(Flags::ALTERNATE), args.upper) {
                (true, false) => b"0b",
                (true, true) => b"0B",
                (false, _) => b"",
            };

            let int = spec.next_int(values)? as u64;
            let len = integer_length_binary(int);

            let zeroed_width = args.integer_zeroed_width(prefix);
            let pad = args.pad_num_before(w, len, zeroed_width, prefix)?;
            write!(w, "{:b}", int as u64)?;
            pad.finish_pad(w)?;
        }
        c @ (b'g' | b'G' | b'e' | b'E' | b'f' | b'F' | b'a' | b'A') => {
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
            // pointer
            spec.check_flags(Flags::LEFT_ALIGN | Flags::WIDTH)?;
            let args = spec.common_args(values)?;

            // TODO: is an intentional address-leak a bad idea?
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

            let len = integer_length_hex(ptr as u64);
            let pad = args.pad_num_before(w, len, 0, b"0x")?;
            write!(w, "{:x}", ptr)?;
            pad.finish_pad(w)?;
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

mod parse {
    use super::{Flags, FormatError, FormatSpecifier, OptionalArg, ARG_MAX, FMT_SPEC};

    struct PeekableIter<'a> {
        base: &'a [u8],
        cur: &'a [u8],
    }

    impl<'a> PeekableIter<'a> {
        fn new(s: &'a [u8]) -> Self {
            Self { base: s, cur: s }
        }
        fn peek(&mut self) -> Option<u8> {
            self.cur.first().copied()
        }
        fn advance(&mut self) {
            self.cur = &self.cur[1..];
        }
        fn cur_index(&self) -> usize {
            self.base.len() - self.cur.len()
        }
    }

    pub fn parse_specifier(
        str: &[u8],
        next: usize,
    ) -> Result<(FormatSpecifier, usize), FormatError> {
        let mut iter = PeekableIter::new(&str[next + 1..]);

        let mut flags = Flags::NONE;
        #[rustfmt::skip]
        loop {
            match iter.peek() {
                Some(b'#') => { iter.advance(); flags |= Flags::ALTERNATE; },
                Some(b'-') => { iter.advance(); flags |= Flags::LEFT_ALIGN; },
                Some(b'+') => { iter.advance(); flags |= Flags::SIGN_FORCE; },
                Some(b' ') => { iter.advance(); flags |= Flags::SIGN_SPACE; },
                Some(b'0') => { iter.advance(); flags |= Flags::ZERO_PAD; },
                _ => break,
            }
        };

        let width = try_parse_optional_arg(&mut iter).map_err(|_| FormatError::BadWidth)?;
        if !matches!(width, OptionalArg::None) {
            flags |= Flags::WIDTH;
        }

        let precision = if let Some(b'.') = iter.peek() {
            iter.advance();
            flags |= Flags::PRECISION;
            let arg = try_parse_optional_arg(&mut iter).map_err(|_| FormatError::BadPrecision)?;
            match arg {
                OptionalArg::None => OptionalArg::Specified(0),
                arg => arg,
            }
        } else {
            OptionalArg::None
        };

        let spec = iter.peek().ok_or(FormatError::BadSpec(FMT_SPEC))?;
        iter.advance();
        let spec_end = next + 1 + iter.cur_index();

        let specifier = FormatSpecifier {
            spec,
            flags,
            width,
            precision,
        };
        Ok((specifier, spec_end))
    }

    fn try_parse_optional_arg(iter: &mut PeekableIter<'_>) -> Result<OptionalArg, ()> {
        match iter.peek() {
            Some(b'*') => {
                iter.advance();
                Ok(OptionalArg::Arg)
            }
            Some(b'0'..=b'9') => {
                let rest = &iter.cur[1..];
                let len = 1 + rest
                    .iter()
                    .position(|c| !c.is_ascii_digit())
                    .unwrap_or(rest.len());

                // We just verified that the slice is only composed of '0'..'9'
                let slice = core::str::from_utf8(&iter.cur[..len]).map_err(|_| ())?;

                let num = slice.parse::<u32>().map_err(|_| ())?;
                if num > ARG_MAX {
                    return Err(());
                }
                iter.cur = &iter.cur[len..];
                Ok(OptionalArg::Specified(num))
            }
            _ => Ok(OptionalArg::None),
        }
    }
}

mod float {
    use core::cmp::Ordering;
    use std::io::Write;

    use super::{
        format_into_buffer, integer_length, strip_nonsignificant_zeroes, FormatArgs, FormatError,
    };
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
            if matches!(mode, FloatMode::Compact) && exp >= -4 && (exp as i64) < (precision as i64)
            {
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

        let remainder_bits = F64_MANTISSA_BITS - used_mantissa_bits as u32;
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
}
