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

pub fn parse_specifier(str: &[u8], next: usize) -> Result<(FormatSpecifier, usize), FormatError> {
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
