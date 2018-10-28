use std::collections::HashMap;
use std::io::{self, Read};
use std::{char, i32, i64, mem, str};

use failure::{err_msg, Error};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
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
    Name(Box<[u8]>),
    String(Box<[u8]>),
}

pub struct Lexer<R: Read> {
    source: Option<R>,
    peek_buffer: Vec<u8>,
    output_buffer: Vec<u8>,
    line_number: u64,
    char_tokens: &'static HashMap<u8, Token>,
    reserved_words: &'static HashMap<&'static [u8], Token>,
}

impl<R: Read> Lexer<R> {
    pub fn new(source: R) -> Lexer<R> {
        lazy_static! {
            static ref CHAR_TOKEN_MAP: HashMap<u8, Token> = {
                let mut m = HashMap::new();
                for &(c, ref t) in SINGLE_CHAR_TOKENS {
                    m.insert(c, t.clone());
                }
                m
            };
            static ref RESERVED_WORD_MAP: HashMap<&'static [u8], Token> = {
                let mut m = HashMap::new();
                for &(n, ref t) in RESERVED_WORDS {
                    m.insert(n.as_bytes(), t.clone());
                }
                m
            };
        }

        Lexer {
            source: Some(source),
            peek_buffer: Vec::new(),
            output_buffer: Vec::new(),
            line_number: 0,
            char_tokens: &*CHAR_TOKEN_MAP,
            reserved_words: &*RESERVED_WORD_MAP,
        }
    }

    /// Current line number of the source file, 0-indexed
    pub fn line_number(&self) -> u64 {
        self.line_number
    }

    /// Consumes any whitespace that is next in the stream, the line number will now be the starting
    /// line number of the next token.
    pub fn skip_whitespace(&mut self) -> Result<(), Error> {
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
    }

    /// Reads the next token, or None if the end of the source has been reached.
    pub fn read_token<'a>(&mut self) -> Result<Option<Token>, Error> {
        self.skip_whitespace()?;

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
                        Token::String(self.take_output())
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
                    Token::String(self.take_output())
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
                    } else {
                        if self.peek(1)?.map(|c| is_digit(c)).unwrap_or(false) {
                            self.read_numeral()?
                        } else {
                            self.advance(1);
                            Token::Dot
                        }
                    }
                }

                c => {
                    if is_digit(c) {
                        self.read_numeral()?
                    } else if let Some(t) = self.char_tokens.get(&c).cloned() {
                        self.advance(1);
                        t
                    } else if is_alpha(c) {
                        self.output_buffer.clear();
                        self.output_buffer.push(c);
                        self.advance(1);

                        while let Some(c) = self.peek(0)? {
                            if is_alpha(c) || is_digit(c) {
                                self.output_buffer.push(c);
                                self.advance(1);
                            } else {
                                break;
                            }
                        }

                        if let Some(t) = self
                            .reserved_words
                            .get(self.output_buffer.as_slice())
                            .cloned()
                        {
                            t
                        } else {
                            Token::Name(self.take_output())
                        }
                    } else {
                        return Err(format_err!(
                            "unexpected character: '{}'",
                            char::from_u32(c as u32).unwrap_or(char::REPLACEMENT_CHARACTER)
                        ));
                    }
                }
            }))
        } else {
            Ok(None)
        }
    }

    // Read any of "\n", "\r", "\n\r", or "\r\n" as a single newline, and increment the current line
    // number.  If `append_buffer` is true, then appends the read newline to the output buffer.
    fn read_line_end(&mut self, append_output: bool) -> Result<(), Error> {
        let newline = self.peek(0).unwrap().unwrap();
        assert!(is_newline(newline));
        self.advance(1);
        if append_output {
            self.output_buffer.push(newline);
        }

        if let Some(next_newline) = self.peek(0)? {
            if is_newline(next_newline) && next_newline != newline {
                self.advance(1);
                if append_output {
                    self.output_buffer.push(next_newline);
                }
            }
        }

        self.line_number += 1;
        Ok(())
    }

    // Read a string on a single line delimited by ' or " that allows for \ escaping of certain
    // characters.  Always reads the contained string into the output buffer.
    fn read_short_string(&mut self) -> Result<(), Error> {
        let start_quote = self.peek(0).unwrap().unwrap();
        assert!(start_quote == b'\'' || start_quote == b'"');
        self.advance(1);

        self.output_buffer.clear();

        loop {
            let c = if let Some(c) = self.peek(0)? {
                c
            } else {
                return Err(err_msg("unfinished short string"));
            };

            if is_newline(c) {
                return Err(err_msg("unfinished short string"));
            }

            self.advance(1);
            if c == b'\\' {
                match self
                    .peek(0)?
                    .ok_or_else(|| err_msg("unfinished short string"))?
                {
                    b'a' => {
                        self.advance(1);
                        self.output_buffer.push(ALERT_BEEP);
                    }

                    b'b' => {
                        self.advance(1);
                        self.output_buffer.push(BACKSPACE);
                    }

                    b'f' => {
                        self.advance(1);
                        self.output_buffer.push(FORM_FEED);
                    }

                    b'n' => {
                        self.advance(1);
                        self.output_buffer.push(b'\n');
                    }

                    b'r' => {
                        self.advance(1);
                        self.output_buffer.push(b'\r');
                    }

                    b't' => {
                        self.advance(1);
                        self.output_buffer.push(b'\t');
                    }

                    b'v' => {
                        self.advance(1);
                        self.output_buffer.push(VERTICAL_TAB);
                    }

                    b'\\' => {
                        self.advance(1);
                        self.output_buffer.push(b'\\');
                    }

                    b'\'' => {
                        self.advance(1);
                        self.output_buffer.push(b'\'');
                    }

                    b'"' => {
                        self.advance(1);
                        self.output_buffer.push(b'"');
                    }

                    b'\n' | b'\r' => {
                        self.read_line_end(true)?;
                    }

                    b'x' => {
                        self.advance(1);
                        let first = self
                            .peek(0)?
                            .and_then(from_hex_digit)
                            .ok_or_else(|| err_msg("hexadecimal digit expected"))?;
                        let second = self
                            .peek(1)?
                            .and_then(from_hex_digit)
                            .ok_or_else(|| err_msg("hexadecimal digit expected"))?;
                        self.output_buffer.push(first << 4 | second);
                        self.advance(2);
                    }

                    b'u' => {
                        if self.peek(1)? != Some(b'{') {
                            return Err(err_msg("missing '{'"));
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
                                    return Err(err_msg("missing '}'"));
                                }
                            } else {
                                return Err(err_msg("missing '}'"));
                            }
                        }

                        let c = char::from_u32(u).ok_or_else(|| err_msg("invalid UTF-8 value"))?;
                        let mut buf = [0; 4];
                        for &b in c.encode_utf8(&mut buf).as_bytes() {
                            self.output_buffer.push(b);
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
                                return Err(err_msg("decimal escape too large"));
                            }

                            self.output_buffer.push(u as u8);
                        } else {
                            return Err(err_msg("invalid escape sequence"));
                        }
                    }
                }
            } else if c == start_quote {
                break;
            } else {
                self.output_buffer.push(c);
            }
        }

        Ok(())
    }

    // Read a [=*[...]=*] sequence with matching numbers of '='.  If `into_output` is true, writes
    // the contained string into the output buffer.
    fn read_long_string(&mut self, into_output: bool) -> Result<(), Error> {
        assert_eq!(self.peek(0).unwrap().unwrap(), b'[');
        self.advance(1);

        if into_output {
            self.output_buffer.clear();
        }

        let mut open_sep_length = 0;
        while self.peek(0)? == Some(b'=') {
            self.advance(1);
            open_sep_length += 1;
        }

        if self.peek(0)? != Some(b'[') {
            return Err(err_msg("invalid long string delimiter"));
        }
        self.advance(1);

        loop {
            let c = if let Some(c) = self.peek(0)? {
                c
            } else {
                return Err(err_msg("unfinished long string"));
            };

            match c {
                b'\n' | b'\r' => {
                    self.read_line_end(into_output)?;
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
                        // to add the invalid close delimiter to the output.
                        if into_output {
                            self.output_buffer.push(b']');
                            for _ in 0..close_sep_length {
                                self.output_buffer.push(b'=');
                            }
                        }
                    }
                }

                c => {
                    if into_output {
                        self.output_buffer.push(c);
                    }
                    self.advance(1);
                }
            }
        }

        Ok(())
    }

    // Reads a hex or decimal integer or floating point identifier.  Allows decimal integers (123),
    // hex integers (0xdeadbeef), decimal floating point with optional exponent and exponent sign
    // (3.21e+1), and hex floats with optional exponent and exponent sign (0xe.2fp-1c).
    fn read_numeral(&mut self) -> Result<Token, Error> {
        let p1 = self.peek(0).unwrap().unwrap();
        assert!(p1 == b'.' || is_digit(p1));

        self.output_buffer.clear();

        let p2 = self.peek(1)?;
        let is_hex = p1 == b'0' && (p2 == Some(b'x') || p2 == Some(b'X'));
        if is_hex {
            self.output_buffer.push(p1);
            self.output_buffer.push(p2.unwrap());
            self.advance(2);
        }

        let mut has_radix = false;
        while let Some(c) = self.peek(0)? {
            if c == b'.' && !has_radix {
                self.output_buffer.push(b'.');
                has_radix = true;
                self.advance(1);
            } else if !is_hex && is_digit(c) {
                self.output_buffer.push(c);
                self.advance(1);
            } else if is_hex && is_hex_digit(c) {
                self.output_buffer.push(c);
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
                self.output_buffer.push(exp_begin);
                has_exp = true;
                self.advance(1);

                if let Some(sign) = self.peek(0)? {
                    if sign == b'+' || sign == b'-' {
                        self.output_buffer.push(sign);
                        self.advance(1);
                    }
                }

                while let Some(c) = self.peek(0)? {
                    if is_digit(c) {
                        self.output_buffer.push(c);
                        self.advance(1);
                    } else {
                        break;
                    }
                }
            }
        }

        if !has_exp && !has_radix {
            if is_hex {
                if let Some(i) = read_hex_integer(&self.output_buffer) {
                    return Ok(Token::Integer(i));
                }
            } else {
                if let Some(i) = read_integer(&self.output_buffer) {
                    return Ok(Token::Integer(i));
                }
            }
        }

        Ok(Token::Float(
            if is_hex {
                read_hex_float(&self.output_buffer)
            } else {
                read_float(&self.output_buffer)
            }.ok_or_else(|| err_msg("malformed number"))?,
        ))
    }

    fn peek(&mut self, n: usize) -> Result<Option<u8>, io::Error> {
        let mut at_end = false;
        let mut err = None;

        if let Some(source) = self.source.as_mut() {
            while self.peek_buffer.len() <= n {
                let mut c = [0];
                match source.read(&mut c) {
                    Ok(0) => {
                        at_end = true;
                        break;
                    }
                    Ok(_) => {
                        self.peek_buffer.push(c[0]);
                    }
                    Err(e) => {
                        if e.kind() != io::ErrorKind::Interrupted {
                            at_end = true;
                            err = Some(e);
                            break;
                        }
                    }
                }
            }
        }

        if at_end {
            self.source = None;
            self.peek_buffer.clear();
        }

        if let Some(err) = err {
            return Err(err);
        }

        Ok(self.peek_buffer.get(n).cloned())
    }

    fn advance(&mut self, n: usize) {
        assert!(
            n <= self.peek_buffer.len(),
            "cannot advance over un-peeked characters"
        );
        self.peek_buffer.drain(0..n);
    }

    fn take_output(&mut self) -> Box<[u8]> {
        mem::replace(&mut self.output_buffer, Vec::new()).into_boxed_slice()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Numeral {
    Integer(i64),
    Float(f64),
}

pub fn read_integer(s: &[u8]) -> Option<i64> {
    let (is_neg, s) = read_neg(s);

    let mut i: i64 = 0;
    for &c in s {
        let d = from_digit(c)? as i64;
        i = i.checked_mul(10)?.checked_add(d)?;
    }

    if is_neg {
        i = i.checked_neg()?;
    }

    if i == i64::MIN {
        None
    } else {
        Some(i)
    }
}

pub fn read_hex_integer(s: &[u8]) -> Option<i64> {
    let (is_neg, s) = read_neg(s);

    if s[0] != b'0' || (s[1] != b'x' && s[1] != b'X') {
        return None;
    }

    let mut i: i64 = 0;
    for &c in &s[2..] {
        let d = from_hex_digit(c)? as i64;
        i = i.checked_mul(16)?.checked_add(d)?;
    }

    if is_neg {
        i = i.checked_neg()?;
    }

    if i == i64::MIN {
        None
    } else {
        Some(i)
    }
}

pub fn read_float(s: &[u8]) -> Option<f64> {
    let s = str::from_utf8(s).ok()?;
    str::parse(s).ok()
}

pub fn read_hex_float(s: &[u8]) -> Option<f64> {
    const MAX_SIGNIFICANT_DIGITS: u32 = 30;

    let (is_neg, s) = read_neg(s);

    if s.len() < 2 {
        return None;
    }

    if s[0] != b'0' || (s[1] != b'x' && s[1] != b'X') {
        return None;
    }

    let mut significant_digits: u32 = 0;
    let mut non_significant_digits: u32 = 0;
    let mut found_dot = false;
    let mut r: f64 = 0.0;
    let mut e: i32 = 0;
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
                r = (r * 16.0) + d as f64;
            } else {
                e = e.checked_add(1)?;
            }
            if found_dot {
                e = e.checked_sub(1)?;
            }
        } else {
            break;
        }
        i += 1;
    }

    if non_significant_digits + significant_digits == 0 {
        return None;
    }

    e = e.checked_mul(4)?;

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
        e = e.saturating_add(exp1);
    } else if i != s.len() {
        return None;
    }

    if is_neg {
        r = -r;
    }

    Some(r * (e as f64).exp2())
}

fn read_neg(s: &[u8]) -> (bool, &[u8]) {
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

const ALERT_BEEP: u8 = 0x07;
const BACKSPACE: u8 = 0x08;
const VERTICAL_TAB: u8 = 0x0b;
const FORM_FEED: u8 = 0x0c;

// Tokens that are a single character and never the beginning of another longer token
const SINGLE_CHAR_TOKENS: &[(u8, Token)] = &[
    (b'-', Token::Minus),
    (b'+', Token::Add),
    (b'*', Token::Mul),
    (b'^', Token::Pow),
    (b'%', Token::Mod),
    (b'&', Token::BitAnd),
    (b'|', Token::BitOr),
    (b',', Token::Comma),
    (b';', Token::SemiColon),
    (b'#', Token::Len),
    (b'(', Token::LeftParen),
    (b')', Token::RightParen),
    (b']', Token::RightBracket),
    (b'{', Token::LeftBrace),
    (b'}', Token::RightBrace),
];

// Identifiers which should be interpreted as keyword tokens
const RESERVED_WORDS: &[(&str, Token)] = &[
    ("break", Token::Break),
    ("do", Token::Do),
    ("else", Token::Else),
    ("elseif", Token::ElseIf),
    ("end", Token::End),
    ("function", Token::Function),
    ("goto", Token::Goto),
    ("if", Token::If),
    ("in", Token::In),
    ("local", Token::Local),
    ("nil", Token::Nil),
    ("for", Token::For),
    ("while", Token::While),
    ("repeat", Token::Repeat),
    ("until", Token::Until),
    ("return", Token::Return),
    ("then", Token::Then),
    ("true", Token::True),
    ("false", Token::False),
    ("not", Token::Not),
    ("and", Token::And),
    ("or", Token::Or),
];

fn is_newline(c: u8) -> bool {
    c == b'\n' || c == b'\r'
}

fn is_space(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == VERTICAL_TAB || c == FORM_FEED || is_newline(c)
}

// Is this character a lua alpha, which is A-Z, a-z, and _
fn is_alpha(c: u8) -> bool {
    (c >= b'a' && c <= b'z') || (c >= b'A' && c <= b'Z') || c == b'_'
}

fn from_digit(c: u8) -> Option<u8> {
    if c >= b'0' && c <= b'9' {
        Some(c - b'0')
    } else {
        None
    }
}

fn is_digit(c: u8) -> bool {
    from_digit(c).is_some()
}

fn from_hex_digit(c: u8) -> Option<u8> {
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

fn is_hex_digit(c: u8) -> bool {
    from_hex_digit(c).is_some()
}
