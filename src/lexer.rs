use std::{char, mem, str, i32, i64};
use std::collections::HashMap;
use std::io::{self, Read};

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
    first_whitespace: bool,
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
            first_whitespace: true,
        }
    }

    /// Current line number of the source file, 0-indexed
    pub fn line_number(&self) -> u64 {
        self.line_number
    }

    /// Consumes any whitespace that is next in the stream, the line number will now be the starting
    /// line number of the next token.
    pub fn skip_whitespace(&mut self) -> Result<(), Error> {
        if self.first_whitespace {
            if self.peek(0)? == Some(OCTOTHORPE) {
                while let Some(c) = self.peek(0)? {
                    if is_newline(c) {
                        break;
                    } else {
                        self.advance(1);
                    }
                }
            }
            self.first_whitespace = false;
        }
        while let Some(c) = self.peek(0)? {
            match c {
                SPACE | HORIZONTAL_TAB | VERTICAL_TAB | FORM_FEED => {
                    self.advance(1);
                }

                NEWLINE | CARRIAGE_RETURN => {
                    self.read_line_end(false)?;
                }

                MINUS => {
                    if self.peek(1)? != Some(MINUS) {
                        break;
                    } else {
                        self.advance(2);

                        match (self.peek(0)?, self.peek(1)?) {
                            (Some(LEFT_BRACKET), Some(EQUALS)) |
                            (Some(LEFT_BRACKET), Some(LEFT_BRACKET)) => {
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
                SPACE | HORIZONTAL_TAB | VERTICAL_TAB | FORM_FEED | NEWLINE | CARRIAGE_RETURN => {
                    unreachable!("whitespace should have been skipped");
                }

                MINUS => {
                    if self.peek(1)? != Some(MINUS) {
                        self.advance(1);
                        Token::Minus
                    } else {
                        unreachable!("whitespace should have been skipped");
                    }
                }

                LEFT_BRACKET => {
                    let next = self.peek(1)?;
                    if next == Some(EQUALS) || next == Some(LEFT_BRACKET) {
                        self.read_long_string(true)?;
                        Token::String(self.take_output())
                    } else {
                        self.advance(1);
                        Token::LeftBracket
                    }
                }

                EQUALS => {
                    self.advance(1);
                    if self.peek(0)? == Some(EQUALS) {
                        self.advance(1);
                        Token::Equal
                    } else {
                        Token::Assign
                    }
                }

                LESS_THAN => {
                    self.advance(1);
                    let next = self.peek(0)?;
                    if next == Some(EQUALS) {
                        self.advance(1);
                        Token::LessEqual
                    } else if next == Some(LESS_THAN) {
                        self.advance(1);
                        Token::ShiftLeft
                    } else {
                        Token::LessThan
                    }
                }

                GREATER_THAN => {
                    self.advance(1);
                    let next = self.peek(0)?;
                    if next == Some(EQUALS) {
                        self.advance(1);
                        Token::GreaterEqual
                    } else if next == Some(GREATER_THAN) {
                        self.advance(1);
                        Token::ShiftRight
                    } else {
                        Token::GreaterThan
                    }
                }

                FORWARD_SLASH => {
                    self.advance(1);
                    if self.peek(0)? == Some(FORWARD_SLASH) {
                        self.advance(1);
                        Token::IDiv
                    } else {
                        Token::Div
                    }
                }

                TILDE => {
                    self.advance(1);
                    if self.peek(0)? == Some(EQUALS) {
                        self.advance(1);
                        Token::NotEqual
                    } else {
                        Token::BitNotXor
                    }
                }

                COLON => {
                    self.advance(1);
                    if self.peek(0)? == Some(COLON) {
                        self.advance(1);
                        Token::DoubleColon
                    } else {
                        Token::Colon
                    }
                }

                DOUBLE_QUOTE | SINGLE_QUOTE => {
                    self.read_short_string()?;
                    Token::String(self.take_output())
                }

                PERIOD => {
                    if self.peek(1)? == Some(PERIOD) {
                        if self.peek(2)? == Some(PERIOD) {
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

                        if let Some(t) = self.reserved_words
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
        assert!(start_quote == SINGLE_QUOTE || start_quote == DOUBLE_QUOTE);
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
            if c == BACKSLASH {
                match self.peek(0)?
                    .ok_or_else(|| err_msg("unfinished short string"))?
                {
                    LOWER_A => {
                        self.advance(1);
                        self.output_buffer.push(ALERT_BEEP);
                    }

                    LOWER_B => {
                        self.advance(1);
                        self.output_buffer.push(BACKSPACE);
                    }

                    LOWER_F => {
                        self.advance(1);
                        self.output_buffer.push(FORM_FEED);
                    }

                    LOWER_N => {
                        self.advance(1);
                        self.output_buffer.push(NEWLINE);
                    }

                    LOWER_R => {
                        self.advance(1);
                        self.output_buffer.push(CARRIAGE_RETURN);
                    }

                    LOWER_T => {
                        self.advance(1);
                        self.output_buffer.push(HORIZONTAL_TAB);
                    }

                    LOWER_V => {
                        self.advance(1);
                        self.output_buffer.push(VERTICAL_TAB);
                    }

                    BACKSLASH => {
                        self.advance(1);
                        self.output_buffer.push(BACKSLASH);
                    }

                    SINGLE_QUOTE => {
                        self.advance(1);
                        self.output_buffer.push(SINGLE_QUOTE);
                    }

                    DOUBLE_QUOTE => {
                        self.advance(1);
                        self.output_buffer.push(DOUBLE_QUOTE);
                    }

                    NEWLINE | CARRIAGE_RETURN => {
                        self.read_line_end(true)?;
                    }

                    LOWER_X => {
                        self.advance(1);
                        let first = self.peek(0)?
                            .and_then(from_hex_digit)
                            .ok_or_else(|| err_msg("hexadecimal digit expected"))?;
                        let second = self.peek(1)?
                            .and_then(from_hex_digit)
                            .ok_or_else(|| err_msg("hexadecimal digit expected"))?;
                        self.output_buffer.push(first << 4 | second);
                        self.advance(2);
                    }

                    LOWER_U => {
                        if self.peek(1)? != Some(LEFT_BRACE) {
                            return Err(err_msg("missing '{'"));
                        }
                        self.advance(2);

                        let mut u: u32 = 0;
                        loop {
                            if let Some(c) = self.peek(0)? {
                                if c == RIGHT_BRACE {
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

                    LOWER_Z => {
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
        assert_eq!(self.peek(0).unwrap().unwrap(), LEFT_BRACKET);
        self.advance(1);

        if into_output {
            self.output_buffer.clear();
        }

        let mut open_sep_length = 0;
        while self.peek(0)? == Some(EQUALS) {
            self.advance(1);
            open_sep_length += 1;
        }

        if self.peek(0)? != Some(LEFT_BRACKET) {
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
                NEWLINE | CARRIAGE_RETURN => {
                    self.read_line_end(into_output)?;
                }

                RIGHT_BRACKET => {
                    let mut close_sep_length = 0;
                    self.advance(1);
                    while self.peek(0)? == Some(EQUALS) {
                        self.advance(1);
                        close_sep_length += 1;
                    }

                    if open_sep_length == close_sep_length && self.peek(0)? == Some(RIGHT_BRACKET) {
                        self.advance(1);
                        break;
                    } else {
                        // If it turns out this is not a valid long string close delimiter, we need
                        // to add the invalid close delimiter to the output.
                        if into_output {
                            self.output_buffer.push(RIGHT_BRACKET);
                            for _ in 0..close_sep_length {
                                self.output_buffer.push(EQUALS);
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
        assert!(p1 == PERIOD || is_digit(p1));

        self.output_buffer.clear();

        let p2 = self.peek(1)?;
        let is_hex = p1 == NUM_0 && (p2 == Some(LOWER_X) || p2 == Some(UPPER_X));
        if is_hex {
            self.output_buffer.push(p1);
            self.output_buffer.push(p2.unwrap());
            self.advance(2);
        }

        let mut has_radix = false;
        while let Some(c) = self.peek(0)? {
            if c == PERIOD && !has_radix {
                self.output_buffer.push(PERIOD);
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
            if (is_hex && (exp_begin == LOWER_P || exp_begin == UPPER_P))
                || (!is_hex && (exp_begin == LOWER_E || exp_begin == UPPER_E))
            {
                self.output_buffer.push(exp_begin);
                has_exp = true;
                self.advance(1);

                if let Some(sign) = self.peek(0)? {
                    if sign == PLUS || sign == MINUS {
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

        Ok(Token::Float(if is_hex {
            read_hex_float(&self.output_buffer)
        } else {
            read_float(&self.output_buffer)
        }.ok_or_else(|| {
            err_msg("malformed number")
        })?))
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

    if s[0] != NUM_0 || (s[1] != LOWER_X && s[1] != UPPER_X) {
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

    if s[0] != NUM_0 || (s[1] != LOWER_X && s[1] != UPPER_X) {
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
        if c == PERIOD {
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

    if i + 1 < s.len() && (s[i] == LOWER_P || s[i] == UPPER_P) {
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
        if s[0] == MINUS {
            (true, &s[1..])
        } else if s[0] == PLUS {
            (false, &s[1..])
        } else {
            (false, s)
        }
    } else {
        (false, s)
    }
}

const SPACE: u8 = ' ' as u8;
const NEWLINE: u8 = '\n' as u8;
const CARRIAGE_RETURN: u8 = '\r' as u8;
const HORIZONTAL_TAB: u8 = '\t' as u8;
const ALERT_BEEP: u8 = 0x07;
const BACKSPACE: u8 = 0x08;
const VERTICAL_TAB: u8 = 0x0b;
const FORM_FEED: u8 = 0x0c;
const LEFT_PAREN: u8 = '(' as u8;
const RIGHT_PAREN: u8 = ')' as u8;
const LEFT_BRACKET: u8 = '[' as u8;
const RIGHT_BRACKET: u8 = ']' as u8;
const LEFT_BRACE: u8 = '{' as u8;
const RIGHT_BRACE: u8 = '}' as u8;
const EQUALS: u8 = '=' as u8;
const LESS_THAN: u8 = '<' as u8;
const GREATER_THAN: u8 = '>' as u8;
const FORWARD_SLASH: u8 = '/' as u8;
const BACKSLASH: u8 = '\\' as u8;
const TILDE: u8 = '~' as u8;
const COLON: u8 = ':' as u8;
const SEMICOLON: u8 = ';' as u8;
const DOUBLE_QUOTE: u8 = '"' as u8;
const SINGLE_QUOTE: u8 = '\'' as u8;
const PERIOD: u8 = '.' as u8;
const COMMA: u8 = ',' as u8;
const PLUS: u8 = '+' as u8;
const MINUS: u8 = '-' as u8;
const ASTERISK: u8 = '*' as u8;
const CARET: u8 = '^' as u8;
const PERCENT: u8 = '%' as u8;
const AMPERSAND: u8 = '&' as u8;
const OCTOTHORPE: u8 = '#' as u8;
const PIPE: u8 = '|' as u8;
const UNDERSCORE: u8 = '_' as u8;
const LOWER_A: u8 = 'a' as u8;
const LOWER_B: u8 = 'b' as u8;
const LOWER_E: u8 = 'e' as u8;
const LOWER_F: u8 = 'f' as u8;
const LOWER_N: u8 = 'n' as u8;
const LOWER_P: u8 = 'p' as u8;
const LOWER_R: u8 = 'r' as u8;
const LOWER_T: u8 = 't' as u8;
const LOWER_U: u8 = 'u' as u8;
const LOWER_V: u8 = 'v' as u8;
const LOWER_X: u8 = 'x' as u8;
const LOWER_Z: u8 = 'z' as u8;
const UPPER_A: u8 = 'A' as u8;
const UPPER_E: u8 = 'E' as u8;
const UPPER_F: u8 = 'F' as u8;
const UPPER_P: u8 = 'P' as u8;
const UPPER_X: u8 = 'X' as u8;
const UPPER_Z: u8 = 'Z' as u8;
const NUM_0: u8 = '0' as u8;
const NUM_9: u8 = '9' as u8;

// Tokens that are a single character and never the beginning of another longer token
const SINGLE_CHAR_TOKENS: &[(u8, Token)] = &[
    (MINUS, Token::Minus),
    (PLUS, Token::Add),
    (ASTERISK, Token::Mul),
    (CARET, Token::Pow),
    (PERCENT, Token::Mod),
    (AMPERSAND, Token::BitAnd),
    (PIPE, Token::BitOr),
    (COMMA, Token::Comma),
    (SEMICOLON, Token::SemiColon),
    (OCTOTHORPE, Token::Len),
    (LEFT_PAREN, Token::LeftParen),
    (RIGHT_PAREN, Token::RightParen),
    (RIGHT_BRACKET, Token::RightBracket),
    (LEFT_BRACE, Token::LeftBrace),
    (RIGHT_BRACE, Token::RightBrace),
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
    c == NEWLINE || c == CARRIAGE_RETURN
}

fn is_space(c: u8) -> bool {
    c == SPACE || c == HORIZONTAL_TAB || c == VERTICAL_TAB || c == FORM_FEED || is_newline(c)
}

// Is this character a lua alpha, which is A-Z, a-z, and _
fn is_alpha(c: u8) -> bool {
    (c >= LOWER_A && c <= LOWER_Z) || (c >= UPPER_A && c <= UPPER_Z) || c == UNDERSCORE
}

fn from_digit(c: u8) -> Option<u8> {
    if c >= NUM_0 && c <= NUM_9 {
        Some(c - NUM_0)
    } else {
        None
    }
}

fn is_digit(c: u8) -> bool {
    from_digit(c).is_some()
}

fn from_hex_digit(c: u8) -> Option<u8> {
    if c >= NUM_0 && c <= NUM_9 {
        Some(c - NUM_0)
    } else if c >= LOWER_A && c <= LOWER_F {
        Some(10 + c - LOWER_A)
    } else if c >= UPPER_A && c <= UPPER_F {
        Some(10 + c - UPPER_A)
    } else {
        None
    }
}

fn is_hex_digit(c: u8) -> bool {
    from_hex_digit(c).is_some()
}
