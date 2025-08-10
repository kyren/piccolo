use super::FormatError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatElement<'a> {
    /// Some characters that are copied to the output as-is
    Verbatim(&'a str),
    /// A format specifier
    Format(ConversionSpecifier),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConversionSpecifier {
    /// flag `#`: use `0x`, etc?
    pub alt_form: bool,
    /// flag `0`: left-pad with zeros?
    pub zero_pad: bool,
    /// flag `-`: left-adjust (pad with spaces on the right)
    pub left_adj: bool,
    /// flag `' '` (space): indicate sign with a space?
    pub space_sign: bool,
    /// flag `+`: Always show sign? (for signed numbers)
    pub force_sign: bool,
    /// field width
    pub width: NumericParam,
    /// floating point field precision
    pub precision: NumericParam,
    /// data type
    pub conversion_type: ConversionType,
}

/// Width / precision parameter
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericParam {
    /// The literal width
    Literal(i32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConversionType {
    /// `d`, `i`, or `u`
    DecInt,
    /// `o`
    OctInt,
    /// `x`
    HexIntLower,
    /// `X`
    HexIntUpper,
    /// `e`
    SciFloatLower,
    /// `E`
    SciFloatUpper,
    /// `f`
    DecFloatLower,
    /// `F`
    DecFloatUpper,
    /// `g`
    CompactFloatLower,
    /// `G`
    CompactFloatUpper,
    /// `a`
    HexFloatLower,
    /// `A`
    HexFloatUpper,
    /// `c`
    Char,
    /// `s`
    String,
    /// `q`
    QuotedString,
    /// `p`
    Pointer,
    /// `%`
    PercentSign,
}

pub fn parse_format_string(fmt: &str) -> Result<Vec<FormatElement>, FormatError> {
    let mut res = Vec::new();

    let mut rem = fmt;

    while !rem.is_empty() {
        if let Some((verbatim_prefix, rest)) = rem.split_once('%') {
            if !verbatim_prefix.is_empty() {
                res.push(FormatElement::Verbatim(verbatim_prefix));
            }
            let (spec, rest) = take_conversion_specifier(rest)?;
            res.push(FormatElement::Format(spec));
            rem = rest;
        } else {
            res.push(FormatElement::Verbatim(rem));
            break;
        }
    }

    Ok(res)
}

fn take_conversion_specifier(s: &str) -> Result<(ConversionSpecifier, &str), FormatError> {
    let mut spec = ConversionSpecifier {
        alt_form: false,
        zero_pad: false,
        left_adj: false,
        space_sign: false,
        force_sign: false,
        width: NumericParam::Literal(0),
        precision: NumericParam::Literal(6),
        // ignore length modifier
        conversion_type: ConversionType::DecInt,
    };

    let mut s = s;

    loop {
        match s.chars().next() {
            Some('#') => {
                spec.alt_form = true;
            }
            Some('0') => {
                spec.zero_pad = true;
            }
            Some('-') => {
                spec.left_adj = true;
            }
            Some(' ') => {
                spec.space_sign = true;
            }
            Some('+') => {
                spec.force_sign = true;
            }
            _ => {
                break;
            }
        }
        s = &s[1..];
    }

    let (w, mut s) = take_numeric_param(s);
    spec.width = w;

    if matches!(s.chars().next(), Some('.')) {
        s = &s[1..];
        let (p, s2) = take_numeric_param(s);
        spec.precision = p;
        s = s2;
    }

    spec.conversion_type = match s.chars().next() {
        Some('i') | Some('d') | Some('u') => ConversionType::DecInt,
        Some('o') => ConversionType::OctInt,
        Some('x') => ConversionType::HexIntLower,
        Some('X') => ConversionType::HexIntUpper,
        Some('e') => ConversionType::SciFloatLower,
        Some('E') => ConversionType::SciFloatUpper,
        Some('f') => ConversionType::DecFloatLower,
        Some('F') => ConversionType::DecFloatUpper,
        Some('g') => ConversionType::CompactFloatLower,
        Some('G') => ConversionType::CompactFloatUpper,
        Some('c') | Some('C') => ConversionType::Char,
        Some('s') | Some('S') => ConversionType::String,
        Some('a') => ConversionType::HexFloatLower,
        Some('A') => ConversionType::HexFloatUpper,
        Some('q') => ConversionType::QuotedString,
        Some('p') => ConversionType::Pointer,
        Some('%') => ConversionType::PercentSign,
        Some(_) => {
            return Err(FormatError::ParseError);
        }
        None => {
            return Err(FormatError::ParseError);
        }
    };

    Ok((spec, &s[1..]))
}

fn take_numeric_param(s: &str) -> (NumericParam, &str) {
    match s.chars().next() {
        Some(digit) if ('1'..='9').contains(&digit) => {
            let mut s = s;
            let mut w = 0;
            loop {
                match s.chars().next() {
                    Some(digit) if digit.is_ascii_digit() => {
                        w = 10 * w + (digit as i32 - '0' as i32);
                    }
                    _ => {
                        break;
                    }
                }
                s = &s[1..];
            }
            (NumericParam::Literal(w), s)
        }
        _ => (NumericParam::Literal(0), s),
    }
}
