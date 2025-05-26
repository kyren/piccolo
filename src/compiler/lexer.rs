use std::{char, fmt};

use gc_arena::Collect;
use thiserror::Error;

use crate::compiler::string_utils::{
    from_digit, from_hex_digit, is_hex_digit, is_space, read_dec_float, read_dec_integer,
    read_hex_float, read_hex_integer, ALERT_BEEP, BACKSPACE,
};

use super::{
    string_utils::{debug_utf8_lossy, is_alpha, is_digit, is_newline, FORM_FEED, VERTICAL_TAB},
    StringInterner,
};

#[derive(Clone)]
pub enum Token<S> {
    Break,
    Do,
    Else,
    ElseIf,
    End,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    For,
    While,
    Repeat,
    Until,
    Return,
    Then,
    True,
    False,
    Not,
    And,
    Or,
    Minus,
    Add,
    Mul,
    Div,
    IDiv,
    Pow,
    Mod,
    Len,
    BitNotXor,
    BitAnd,
    BitOr,
    ShiftRight,
    ShiftLeft,
    Concat,
    Dots,
    Assign,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    NotEqual,
    Dot,
    SemiColon,
    Colon,
    DoubleColon,
    Comma,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    /// Numerals are only lexed as integers in the range [-(2^63-1), 2^63-1], otherwise they will be
    /// lexed as floats.
    Integer(i64),
    Float(f64),
    Name(S),
    String(S),
}

impl<S: AsRef<[u8]>> PartialEq for Token<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Break, Token::Break) => true,
            (Token::Do, Token::Do) => true,
            (Token::Else, Token::Else) => true,
            (Token::ElseIf, Token::ElseIf) => true,
            (Token::End, Token::End) => true,
            (Token::Function, Token::Function) => true,
            (Token::Goto, Token::Goto) => true,
            (Token::If, Token::If) => true,
            (Token::In, Token::In) => true,
            (Token::Local, Token::Local) => true,
            (Token::Nil, Token::Nil) => true,
            (Token::For, Token::For) => true,
            (Token::While, Token::While) => true,
            (Token::Repeat, Token::Repeat) => true,
            (Token::Until, Token::Until) => true,
            (Token::Return, Token::Return) => true,
            (Token::Then, Token::Then) => true,
            (Token::True, Token::True) => true,
            (Token::False, Token::False) => true,
            (Token::Not, Token::Not) => true,
            (Token::And, Token::And) => true,
            (Token::Or, Token::Or) => true,
            (Token::Minus, Token::Minus) => true,
            (Token::Add, Token::Add) => true,
            (Token::Mul, Token::Mul) => true,
            (Token::Div, Token::Div) => true,
            (Token::IDiv, Token::IDiv) => true,
            (Token::Pow, Token::Pow) => true,
            (Token::Mod, Token::Mod) => true,
            (Token::Len, Token::Len) => true,
            (Token::BitNotXor, Token::BitNotXor) => true,
            (Token::BitAnd, Token::BitAnd) => true,
            (Token::BitOr, Token::BitOr) => true,
            (Token::ShiftRight, Token::ShiftRight) => true,
            (Token::ShiftLeft, Token::ShiftLeft) => true,
            (Token::Concat, Token::Concat) => true,
            (Token::Dots, Token::Dots) => true,
            (Token::Assign, Token::Assign) => true,
            (Token::LessThan, Token::LessThan) => true,
            (Token::LessEqual, Token::LessEqual) => true,
            (Token::GreaterThan, Token::GreaterThan) => true,
            (Token::GreaterEqual, Token::GreaterEqual) => true,
            (Token::Equal, Token::Equal) => true,
            (Token::NotEqual, Token::NotEqual) => true,
            (Token::Dot, Token::Dot) => true,
            (Token::SemiColon, Token::SemiColon) => true,
            (Token::Colon, Token::Colon) => true,
            (Token::DoubleColon, Token::DoubleColon) => true,
            (Token::Comma, Token::Comma) => true,
            (Token::LeftParen, Token::LeftParen) => true,
            (Token::RightParen, Token::RightParen) => true,
            (Token::LeftBracket, Token::LeftBracket) => true,
            (Token::RightBracket, Token::RightBracket) => true,
            (Token::LeftBrace, Token::LeftBrace) => true,
            (Token::RightBrace, Token::RightBrace) => true,
            (Token::Integer(a), Token::Integer(b)) => a == b,
            (Token::Float(a), Token::Float(b)) => a.total_cmp(b).is_eq(),
            (Token::Name(a), Token::Name(b)) => a.as_ref() == b.as_ref(),
            (Token::String(a), Token::String(b)) => a.as_ref() == b.as_ref(),
            _ => false,
        }
    }
}

impl<S: AsRef<[u8]>> fmt::Debug for Token<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Break => write!(f, "Break"),
            Token::Do => write!(f, "Do"),
            Token::Else => write!(f, "Else"),
            Token::ElseIf => write!(f, "ElseIf"),
            Token::End => write!(f, "End"),
            Token::Function => write!(f, "Function"),
            Token::Goto => write!(f, "Goto"),
            Token::If => write!(f, "If"),
            Token::In => write!(f, "In"),
            Token::Local => write!(f, "Local"),
            Token::Nil => write!(f, "Nil"),
            Token::For => write!(f, "For"),
            Token::While => write!(f, "While"),
            Token::Repeat => write!(f, "Repeat"),
            Token::Until => write!(f, "Until"),
            Token::Return => write!(f, "Return"),
            Token::Then => write!(f, "Then"),
            Token::True => write!(f, "True"),
            Token::False => write!(f, "False"),
            Token::Not => write!(f, "Not"),
            Token::And => write!(f, "And"),
            Token::Or => write!(f, "Or"),
            Token::Minus => write!(f, "Minus"),
            Token::Add => write!(f, "Add"),
            Token::Mul => write!(f, "Mul"),
            Token::Div => write!(f, "Div"),
            Token::IDiv => write!(f, "IDiv"),
            Token::Pow => write!(f, "Pow"),
            Token::Mod => write!(f, "Mod"),
            Token::Len => write!(f, "Len"),
            Token::BitNotXor => write!(f, "BitNotXor"),
            Token::BitAnd => write!(f, "BitAnd"),
            Token::BitOr => write!(f, "BitOr"),
            Token::ShiftRight => write!(f, "ShiftRight"),
            Token::ShiftLeft => write!(f, "ShiftLeft"),
            Token::Concat => write!(f, "Concat"),
            Token::Dots => write!(f, "Dots"),
            Token::Assign => write!(f, "Assign"),
            Token::LessThan => write!(f, "LessThan"),
            Token::LessEqual => write!(f, "LessEqual"),
            Token::GreaterThan => write!(f, "GreaterThan"),
            Token::GreaterEqual => write!(f, "GreaterEqual"),
            Token::Equal => write!(f, "Equal"),
            Token::NotEqual => write!(f, "NotEqual"),
            Token::Dot => write!(f, "Dot"),
            Token::SemiColon => write!(f, "SemiColon"),
            Token::Colon => write!(f, "Colon"),
            Token::DoubleColon => write!(f, "DoubleColon"),
            Token::Comma => write!(f, "Comma"),
            Token::LeftParen => write!(f, "LeftParen"),
            Token::RightParen => write!(f, "RightParen"),
            Token::LeftBracket => write!(f, "LeftBracket"),
            Token::RightBracket => write!(f, "RightBracket"),
            Token::LeftBrace => write!(f, "LeftBrace"),
            Token::RightBrace => write!(f, "RightBrace"),
            Token::Integer(i) => write!(f, "Integer({})", *i),
            Token::Float(d) => write!(f, "Float({})", *d),
            Token::Name(n) => write!(f, "Name({:?})", debug_utf8_lossy(n.as_ref())),
            Token::String(s) => write!(f, "String({:?})", debug_utf8_lossy(s.as_ref())),
        }
    }
}

fn print_char(c: u8) -> char {
    char::from_u32(c as u32).unwrap_or(char::REPLACEMENT_CHARACTER)
}

#[derive(Debug, Error)]
pub enum LexError {
    #[error("short string not finished, expected matching {}", print_char(*.0))]
    UnfinishedShortString(u8),
    #[error("unexpected character: {}", print_char(*.0))]
    UnexpectedCharacter(u8),
    #[error("hexadecimal digit expected")]
    HexDigitExpected,
    #[error("missing '{{' in \\u{{xxxx}} escape")]
    EscapeUnicodeStart,
    #[error("missing '}}' in \\u{{xxxx}} escape")]
    EscapeUnicodeEnd,
    #[error("invalid unicode value in \\u{{xxxx}} escape")]
    EscapeUnicodeInvalid,
    #[error("\\ddd escape out of 0-255 range")]
    EscapeDecimalTooLarge,
    #[error("invalid escape sequence")]
    InvalidEscape,
    #[error("invalid long string delimiter")]
    InvalidLongStringDelimiter,
    #[error("unfinished long string")]
    UnfinishedLongString,
    #[error("malformed number")]
    BadNumber,
}

/// A 0-indexed line number of the current source input.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Collect)]
#[collect(require_static)]
pub struct LineNumber(pub u64);

impl fmt::Display for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", u128::from(self.0) + 1)
    }
}

pub struct Lexer<'a, S> {
    source: &'a [u8],
    interner: S,
    peek_count: usize,
    string_buffer: Vec<u8>,
    line_number: u64,
}

impl<'a, S> Lexer<'a, S>
where
    S: StringInterner,
{
    pub fn new(source: &'a [u8], interner: S) -> Lexer<'a, S> {
        Lexer {
            source,
            interner,
            peek_count: 0,
            string_buffer: Vec::new(),
            line_number: 0,
        }
    }

    /// Current line number of the source file.
    pub fn line_number(&self) -> LineNumber {
        LineNumber(self.line_number)
    }

    pub fn skip_whitespace(&mut self) -> Result<(), LexError> {
        let mut do_skip_whitespace = || {
            while let Some(c) = self.peek(0)? {
                match c {
                    b' ' | b'\t' | VERTICAL_TAB | FORM_FEED => {
                        self.advance(1);
                    }

                    b'\n' | b'\r' => {
                        self.read_line_end(false)?;
                    }

                    b'-' => {
                        if self.peek(1)? != Some(b'-') {
                            break;
                        } else {
                            self.advance(2);

                            match (self.peek(0)?, self.peek(1)?) {
                                (Some(b'['), Some(b'=')) | (Some(b'['), Some(b'[')) => {
                                    // long comment
                                    self.read_long_string(false)?;
                                }
                                _ => {
                                    // Short comment, read until end of line
                                    while let Some(c) = self.peek(0)? {
                                        if is_newline(c) {
                                            break;
                                        } else {
                                            self.advance(1);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    _ => break,
                }
            }

            Ok(())
        };

        match do_skip_whitespace() {
            Ok(()) => Ok(()),
            Err(err) => {
                self.reset();
                Err(err)
            }
        }
    }

    /// Reads the next token, or None if the end of the source has been reached.
    pub fn read_token(&mut self) -> Result<Option<Token<S::String>>, LexError> {
        self.skip_whitespace()?;

        let mut do_read_token = || {
            if let Some(c) = self.peek(0)? {
                Ok(Some(match c {
                    b' ' | b'\t' | VERTICAL_TAB | FORM_FEED | b'\n' | b'\r' => {
                        unreachable!("whitespace should have been skipped");
                    }

                    b'-' => {
                        if self.peek(1)? != Some(b'-') {
                            self.advance(1);
                            Token::Minus
                        } else {
                            unreachable!("whitespace should have been skipped");
                        }
                    }

                    b'[' => {
                        let next = self.peek(1)?;
                        if next == Some(b'=') || next == Some(b'[') {
                            self.read_long_string(true)?;
                            Token::String(self.take_string())
                        } else {
                            self.advance(1);
                            Token::LeftBracket
                        }
                    }

                    b'=' => {
                        self.advance(1);
                        if self.peek(0)? == Some(b'=') {
                            self.advance(1);
                            Token::Equal
                        } else {
                            Token::Assign
                        }
                    }

                    b'<' => {
                        self.advance(1);
                        let next = self.peek(0)?;
                        if next == Some(b'=') {
                            self.advance(1);
                            Token::LessEqual
                        } else if next == Some(b'<') {
                            self.advance(1);
                            Token::ShiftLeft
                        } else {
                            Token::LessThan
                        }
                    }

                    b'>' => {
                        self.advance(1);
                        let next = self.peek(0)?;
                        if next == Some(b'=') {
                            self.advance(1);
                            Token::GreaterEqual
                        } else if next == Some(b'>') {
                            self.advance(1);
                            Token::ShiftRight
                        } else {
                            Token::GreaterThan
                        }
                    }

                    b'/' => {
                        self.advance(1);
                        if self.peek(0)? == Some(b'/') {
                            self.advance(1);
                            Token::IDiv
                        } else {
                            Token::Div
                        }
                    }

                    b'~' => {
                        self.advance(1);
                        if self.peek(0)? == Some(b'=') {
                            self.advance(1);
                            Token::NotEqual
                        } else {
                            Token::BitNotXor
                        }
                    }

                    b':' => {
                        self.advance(1);
                        if self.peek(0)? == Some(b':') {
                            self.advance(1);
                            Token::DoubleColon
                        } else {
                            Token::Colon
                        }
                    }

                    b'"' | b'\'' => {
                        self.read_short_string()?;
                        Token::String(self.take_string())
                    }

                    b'.' => {
                        if self.peek(1)? == Some(b'.') {
                            if self.peek(2)? == Some(b'.') {
                                self.advance(3);
                                Token::Dots
                            } else {
                                self.advance(2);
                                Token::Concat
                            }
                        } else if self.peek(1)?.map(is_digit).unwrap_or(false) {
                            self.read_numeral()?
                        } else {
                            self.advance(1);
                            Token::Dot
                        }
                    }

                    c => {
                        if is_digit(c) {
                            self.read_numeral()?
                        } else if let Some(t) = get_char_token(c) {
                            self.advance(1);
                            t
                        } else if is_alpha(c) {
                            self.string_buffer.clear();
                            self.string_buffer.push(c);
                            self.advance(1);

                            while let Some(c) = self.peek(0)? {
                                if is_alpha(c) || is_digit(c) {
                                    self.string_buffer.push(c);
                                    self.advance(1);
                                } else {
                                    break;
                                }
                            }

                            if let Some(t) = get_reserved_word_token(self.string_buffer.as_slice())
                            {
                                t
                            } else {
                                Token::Name(self.take_string())
                            }
                        } else {
                            return Err(LexError::UnexpectedCharacter(c));
                        }
                    }
                }))
            } else {
                Ok(None)
            }
        };

        match do_read_token() {
            Ok(Some(token)) => Ok(Some(token)),
            res => {
                self.reset();
                res
            }
        }
    }

    // End of stream encountered, clear any input handles and temp buffers
    fn reset(&mut self) {
        self.source = &[];
        self.peek_count = 0;
        self.string_buffer.clear();
    }

    // Read any of "\n", "\r", "\n\r", or "\r\n" as a single newline, and increment the current line
    // number. If `append_buffer` is true, then appends the read newline to the string buffer.
    fn read_line_end(&mut self, append_string: bool) -> Result<(), LexError> {
        let newline = self.peek(0).unwrap().unwrap();
        assert!(is_newline(newline));
        self.advance(1);
        // We always append a single plain `\n` character for any newline characters, matching the
        // behavior of PUC-Rio Lua.
        if append_string {
            self.string_buffer.push(b'\n');
        }

        if let Some(next_newline) = self.peek(0)? {
            if is_newline(next_newline) && next_newline != newline {
                self.advance(1);
            }
        }

        self.line_number += 1;
        Ok(())
    }

    // Read a string on a single line delimited by ' or " that allows for \ escaping of certain
    // characters. Always reads the contained string into the string buffer.
    fn read_short_string(&mut self) -> Result<(), LexError> {
        let start_quote = self.peek(0).unwrap().unwrap();
        assert!(start_quote == b'\'' || start_quote == b'"');
        self.advance(1);

        self.string_buffer.clear();

        loop {
            let c = if let Some(c) = self.peek(0)? {
                c
            } else {
                return Err(LexError::UnfinishedShortString(start_quote));
            };

            if is_newline(c) {
                return Err(LexError::UnfinishedShortString(start_quote));
            }

            self.advance(1);
            if c == b'\\' {
                match self
                    .peek(0)?
                    .ok_or_else(|| LexError::UnfinishedShortString(start_quote))?
                {
                    b'a' => {
                        self.advance(1);
                        self.string_buffer.push(ALERT_BEEP);
                    }

                    b'b' => {
                        self.advance(1);
                        self.string_buffer.push(BACKSPACE);
                    }

                    b'f' => {
                        self.advance(1);
                        self.string_buffer.push(FORM_FEED);
                    }

                    b'n' => {
                        self.advance(1);
                        self.string_buffer.push(b'\n');
                    }

                    b'r' => {
                        self.advance(1);
                        self.string_buffer.push(b'\r');
                    }

                    b't' => {
                        self.advance(1);
                        self.string_buffer.push(b'\t');
                    }

                    b'v' => {
                        self.advance(1);
                        self.string_buffer.push(VERTICAL_TAB);
                    }

                    b'\\' => {
                        self.advance(1);
                        self.string_buffer.push(b'\\');
                    }

                    b'\'' => {
                        self.advance(1);
                        self.string_buffer.push(b'\'');
                    }

                    b'"' => {
                        self.advance(1);
                        self.string_buffer.push(b'"');
                    }

                    b'\n' | b'\r' => {
                        self.read_line_end(true)?;
                    }

                    b'x' => {
                        self.advance(1);
                        let first = self
                            .peek(0)?
                            .and_then(from_hex_digit)
                            .ok_or(LexError::HexDigitExpected)?;
                        let second = self
                            .peek(1)?
                            .and_then(from_hex_digit)
                            .ok_or(LexError::HexDigitExpected)?;
                        self.string_buffer.push(first << 4 | second);
                        self.advance(2);
                    }

                    b'u' => {
                        if self.peek(1)? != Some(b'{') {
                            return Err(LexError::EscapeUnicodeStart);
                        }
                        self.advance(2);

                        let mut u: u32 = 0;
                        loop {
                            if let Some(c) = self.peek(0)? {
                                if c == b'}' {
                                    self.advance(1);
                                    break;
                                } else if let Some(h) = from_hex_digit(c) {
                                    u = (u << 4) | h as u32;
                                    self.advance(1);
                                } else {
                                    return Err(LexError::EscapeUnicodeEnd);
                                }
                            } else {
                                return Err(LexError::EscapeUnicodeEnd);
                            }
                        }

                        let c = char::from_u32(u).ok_or(LexError::EscapeUnicodeInvalid)?;
                        let mut buf = [0; 4];
                        for &b in c.encode_utf8(&mut buf).as_bytes() {
                            self.string_buffer.push(b);
                        }
                    }

                    b'z' => {
                        self.advance(1);
                        while let Some(c) = self.peek(0)? {
                            if is_newline(c) {
                                self.read_line_end(false)?;
                            } else if is_space(c) {
                                self.advance(1);
                            } else {
                                break;
                            }
                        }
                    }

                    c => {
                        if is_digit(c) {
                            let mut u: u16 = 0;
                            for _ in 0..3 {
                                if let Some(d) = self.peek(0)?.and_then(from_digit) {
                                    u = 10 * u + d as u16;
                                    self.advance(1);
                                } else {
                                    break;
                                }
                            }
                            if u > 255 {
                                return Err(LexError::EscapeDecimalTooLarge);
                            }

                            self.string_buffer.push(u as u8);
                        } else {
                            return Err(LexError::InvalidEscape);
                        }
                    }
                }
            } else if c == start_quote {
                break;
            } else {
                self.string_buffer.push(c);
            }
        }

        Ok(())
    }

    // Read a [=*[...]=*] sequence with matching numbers of '='. If `into_string` is true, writes
    // the contained string into the string buffer.
    fn read_long_string(&mut self, into_string: bool) -> Result<(), LexError> {
        assert_eq!(self.peek(0).unwrap().unwrap(), b'[');
        self.advance(1);

        if into_string {
            self.string_buffer.clear();
        }

        let mut open_sep_length = 0;
        while self.peek(0)? == Some(b'=') {
            self.advance(1);
            open_sep_length += 1;
        }

        if self.peek(0)? != Some(b'[') {
            return Err(LexError::InvalidLongStringDelimiter);
        }
        self.advance(1);

        if matches!(self.peek(0)?, Some(b'\n' | b'\r')) {
            // If the long string starts imediately with a newline, we read it and do *not* put it
            // into the string buffer, matching the behavior of PUC-Rio Lua.
            self.read_line_end(false)?;
        }

        loop {
            let c = if let Some(c) = self.peek(0)? {
                c
            } else {
                return Err(LexError::UnfinishedLongString);
            };

            match c {
                b'\n' | b'\r' => {
                    self.read_line_end(into_string)?;
                }

                b']' => {
                    let mut close_sep_length = 0;
                    self.advance(1);
                    while self.peek(0)? == Some(b'=') {
                        self.advance(1);
                        close_sep_length += 1;
                    }

                    if open_sep_length == close_sep_length && self.peek(0)? == Some(b']') {
                        self.advance(1);
                        break;
                    } else {
                        // If it turns out this is not a valid long string close delimiter, we need
                        // to add the invalid close delimiter to the string.
                        if into_string {
                            self.string_buffer.push(b']');
                            for _ in 0..close_sep_length {
                                self.string_buffer.push(b'=');
                            }
                        }
                    }
                }

                c => {
                    if into_string {
                        self.string_buffer.push(c);
                    }
                    self.advance(1);
                }
            }
        }

        Ok(())
    }

    // Reads a hex or decimal integer or floating point identifier. Allows decimal integers (123),
    // hex integers (0xdeadbeef), decimal floating point with optional exponent and exponent sign
    // (3.21e+1), and hex floats with optional exponent and exponent sign (0xe.2fp-1c).
    fn read_numeral(&mut self) -> Result<Token<S::String>, LexError> {
        let p1 = self.peek(0).unwrap().unwrap();
        assert!(p1 == b'.' || is_digit(p1));

        self.string_buffer.clear();

        let p2 = self.peek(1)?;
        let is_hex = p1 == b'0' && (p2 == Some(b'x') || p2 == Some(b'X'));
        if is_hex {
            self.string_buffer.push(p1);
            self.string_buffer.push(p2.unwrap());
            self.advance(2);
        }

        let mut has_radix = false;
        while let Some(c) = self.peek(0)? {
            if c == b'.' && !has_radix {
                self.string_buffer.push(b'.');
                has_radix = true;
                self.advance(1);
            } else if (!is_hex && is_digit(c)) || (is_hex && is_hex_digit(c)) {
                self.string_buffer.push(c);
                self.advance(1);
            } else {
                break;
            }
        }

        let mut has_exp = false;
        if let Some(exp_begin) = self.peek(0)? {
            if (is_hex && (exp_begin == b'p' || exp_begin == b'P'))
                || (!is_hex && (exp_begin == b'e' || exp_begin == b'E'))
            {
                self.string_buffer.push(exp_begin);
                has_exp = true;
                self.advance(1);

                if let Some(sign) = self.peek(0)? {
                    if sign == b'+' || sign == b'-' {
                        self.string_buffer.push(sign);
                        self.advance(1);
                    }
                }

                while let Some(c) = self.peek(0)? {
                    if is_digit(c) {
                        self.string_buffer.push(c);
                        self.advance(1);
                    } else {
                        break;
                    }
                }
            }
        }

        if !has_exp && !has_radix {
            if is_hex {
                if let Some(i) = read_hex_integer(&self.string_buffer) {
                    return Ok(Token::Integer(i));
                }
            }
            if let Some(i) = read_dec_integer(&self.string_buffer) {
                return Ok(Token::Integer(i));
            }
        }

        Ok(Token::Float(
            if is_hex {
                read_hex_float(&self.string_buffer)
            } else {
                read_dec_float(&self.string_buffer)
            }
            .ok_or(LexError::BadNumber)?,
        ))
    }

    fn peek(&mut self, n: usize) -> Result<Option<u8>, LexError> {
        self.peek_count = (n + 1).max(self.peek_count).min(self.source.len());
        Ok(self.source.get(n).copied())
    }

    fn advance(&mut self, n: usize) {
        assert!(
            n <= self.peek_count,
            "cannot advance over un-peeked characters"
        );
        self.peek_count -= n;
        self.source = &self.source[n..];
    }

    fn take_string(&mut self) -> S::String {
        let s = self.interner.intern(&self.string_buffer);
        self.string_buffer.clear();
        s
    }
}

fn get_char_token<S>(c: u8) -> Option<Token<S>> {
    match c {
        b'-' => Some(Token::Minus),
        b'+' => Some(Token::Add),
        b'*' => Some(Token::Mul),
        b'^' => Some(Token::Pow),
        b'%' => Some(Token::Mod),
        b'&' => Some(Token::BitAnd),
        b'|' => Some(Token::BitOr),
        b',' => Some(Token::Comma),
        b';' => Some(Token::SemiColon),
        b'#' => Some(Token::Len),
        b'(' => Some(Token::LeftParen),
        b')' => Some(Token::RightParen),
        b']' => Some(Token::RightBracket),
        b'{' => Some(Token::LeftBrace),
        b'}' => Some(Token::RightBrace),
        _ => None,
    }
}

fn get_reserved_word_token<S>(word: &[u8]) -> Option<Token<S>> {
    match word {
        b"break" => Some(Token::Break),
        b"do" => Some(Token::Do),
        b"else" => Some(Token::Else),
        b"elseif" => Some(Token::ElseIf),
        b"end" => Some(Token::End),
        b"function" => Some(Token::Function),
        b"goto" => Some(Token::Goto),
        b"if" => Some(Token::If),
        b"in" => Some(Token::In),
        b"local" => Some(Token::Local),
        b"nil" => Some(Token::Nil),
        b"for" => Some(Token::For),
        b"while" => Some(Token::While),
        b"repeat" => Some(Token::Repeat),
        b"until" => Some(Token::Until),
        b"return" => Some(Token::Return),
        b"then" => Some(Token::Then),
        b"true" => Some(Token::True),
        b"false" => Some(Token::False),
        b"not" => Some(Token::Not),
        b"and" => Some(Token::And),
        b"or" => Some(Token::Or),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::compiler::interning::BasicInterner;

    use super::*;

    fn test_tokens(source: &str, tokens: &[Token<Rc<[u8]>>]) {
        let mut lexer = Lexer::new(source.as_bytes(), BasicInterner::default());
        let mut i = 0;
        while let Some(token) = lexer.read_token().unwrap() {
            assert!(i < tokens.len(), "too many tokens");
            assert_eq!(token, tokens[i], "tokens not equal");
            i += 1;
        }
        assert!(i == tokens.len(), "not enough tokens");
    }

    fn test_tokens_lines(source: &str, tokens: &[(Token<Rc<[u8]>>, u64)]) {
        let mut lexer = Lexer::new(source.as_bytes(), BasicInterner::default());
        let mut i = 0;
        loop {
            lexer.skip_whitespace().unwrap();
            let line_number = lexer.line_number().0;
            if let Some(token) = lexer.read_token().unwrap() {
                assert!(i < tokens.len(), "too many tokens");
                assert_eq!(token, tokens[i].0, "tokens not equal");
                assert_eq!(line_number, tokens[i].1, "line numbers do not match");
                i += 1;
            } else {
                break;
            }
        }
        assert!(i == tokens.len(), "not enough tokens");
    }

    fn str_token(s: &str) -> Token<Rc<[u8]>> {
        Token::String(s.as_bytes().to_vec().into_boxed_slice().into())
    }

    fn name_token(s: &str) -> Token<Rc<[u8]>> {
        Token::Name(s.as_bytes().to_vec().into_boxed_slice().into())
    }

    #[test]
    fn comments() {
        test_tokens_lines(
            r#"
                -- this is a comment
                -- this is also -- a comment
                --[[ long comment ]]
                --[==[ longer comment ]==]

                -- Real token
                -

                --[====[ longest comment
                    these shouldn't trigger the end of comments
                    ]=] ]==] ]===]
                ]====]

                -- Real token
                =
            "#,
            &[(Token::Minus, 7), (Token::Assign, 15)],
        );
    }

    #[test]
    fn long_string() {
        test_tokens(
            r#"
                [====[ [==[ this is a [[]] long string ]== ]==] ]====]
                [[ [=] [==] another long string [==] [=] ]]
            "#,
            &[
                str_token(" [==[ this is a [[]] long string ]== ]==] "),
                str_token(" [=] [==] another long string [==] [=] "),
            ],
        );

        test_tokens(
            "[==[\nfoo\nbar\rbaz\r\nbaf\rquux]==]",
            &[str_token("foo\nbar\nbaz\nbaf\nquux")],
        );
    }

    #[test]
    fn short_string() {
        test_tokens_lines(
            r#"
                "\\ \" '"
                '\n \t "'
                "begin \z
                end"
                'state\u{2e}'
                "question\x3f"
                "exclaim\33"
            "#,
            &[
                (str_token("\\ \" '"), 1),
                (str_token("\n \t \""), 2),
                (str_token("begin end"), 3),
                (str_token("state."), 5),
                (str_token("question?"), 6),
                (str_token("exclaim!"), 7),
            ],
        );
    }

    #[test]
    fn numerals() {
        test_tokens(
            r#"
                0xdeadbeef
                12345
                12345.
                3.1415e-2
                0x22.4p+1
                0Xaa.8P-2
                0x8.4P0
                .123E-10
                0x99999999999999999999999999999999p999999999999999999999999999999
                9223372036854775807
                9223372036854775808
            "#,
            &[
                Token::Integer(0xdeadbeef),
                Token::Integer(12345),
                Token::Float(12345.0),
                Token::Float(3.1415e-2),
                Token::Float(68.5),
                Token::Float(42.625),
                Token::Float(8.25),
                Token::Float(0.123e-10),
                Token::Float(f64::INFINITY),
                Token::Integer(9223372036854775807),
                Token::Float(9223372036854775808.0),
            ],
        );
    }

    #[test]
    fn words() {
        test_tokens(
            r#"
                break do else elseif end function goto if in local nil for while repeat until return
                then true false not and or
                custom names
            "#,
            &[
                Token::Break,
                Token::Do,
                Token::Else,
                Token::ElseIf,
                Token::End,
                Token::Function,
                Token::Goto,
                Token::If,
                Token::In,
                Token::Local,
                Token::Nil,
                Token::For,
                Token::While,
                Token::Repeat,
                Token::Until,
                Token::Return,
                Token::Then,
                Token::True,
                Token::False,
                Token::Not,
                Token::And,
                Token::Or,
                name_token("custom"),
                name_token("names"),
            ],
        );
    }

    #[test]
    fn ops() {
        test_tokens(
            r#"- + * / // ^ % & ~ | , ; >> << . .. ... = < <= > >= == ~= : :: # ( ) [ ] { }"#,
            &[
                Token::Minus,
                Token::Add,
                Token::Mul,
                Token::Div,
                Token::IDiv,
                Token::Pow,
                Token::Mod,
                Token::BitAnd,
                Token::BitNotXor,
                Token::BitOr,
                Token::Comma,
                Token::SemiColon,
                Token::ShiftRight,
                Token::ShiftLeft,
                Token::Dot,
                Token::Concat,
                Token::Dots,
                Token::Assign,
                Token::LessThan,
                Token::LessEqual,
                Token::GreaterThan,
                Token::GreaterEqual,
                Token::Equal,
                Token::NotEqual,
                Token::Colon,
                Token::DoubleColon,
                Token::Len,
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBracket,
                Token::RightBracket,
                Token::LeftBrace,
                Token::RightBrace,
            ],
        );
    }
}
