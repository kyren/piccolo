use std::char;
use std::collections::HashMap;
use std::io::{self, Read};

use failure::{err_msg, Error};

#[derive(Debug, Clone, Eq, PartialEq)]
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
    Plus,
    Minus,
    Times,
    Div,
    IDiv,
    Pow,
    Mod,
    Len,
    BitAnd,
    BitNot,
    BitOr,
    ShiftRight,
    ShiftLeft,
    Concat,
    Dots,
    Assign,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    Dot,
    SemiColon,
    Colon,
    DoubleColon,
    Comma,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Name(Box<[u8]>),
    String(Box<[u8]>),
    /// Digits will contain digit values 0-9, or 0-15 if the integer is in hex.
    Integer {
        is_hex: bool,
        digits: Box<[u8]>,
    },
    /// Whole and fractional part contains digits values 0-9, or 0-15 if the float is in hex.
    /// Exponent is always digital regardless of whether the float is in hex or not.
    Float {
        is_hex: bool,
        whole_part: Box<[u8]>,
        fractional_part: Box<[u8]>,
        exponent_part: Box<[u8]>,
        exponent_negative: bool,
    },
}

pub struct Lexer<R: Read> {
    source: Option<R>,
    peek_buffer: Vec<u8>,
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
                SPACE | HORIZONTAL_TAB | VERTICAL_TAB | FORM_FEED => {
                    self.advance(1);
                }

                NEWLINE | CARRIAGE_RETURN => {
                    self.line_end(None)?;
                }

                MINUS => {
                    if self.peek(1)? != Some(MINUS) {
                        break;
                    } else {
                        self.advance(2);

                        if self.peek(0)? == Some(LEFT_BRACKET) {
                            // long comment
                            self.long_string(None)?;
                        } else {
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
                        let mut output = Vec::new();
                        self.long_string(Some(&mut output))?;
                        Token::String(output.into_boxed_slice())
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
                        Token::Less
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
                        Token::Greater
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
                        Token::BitNot
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

                DOUBLE_QUOTE | SINGLE_QUOTE => Token::String(self.short_string()?),

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
                            self.numeral()?
                        } else {
                            self.advance(1);
                            Token::Dot
                        }
                    }
                }

                c => {
                    if is_digit(c) {
                        self.numeral()?
                    } else if let Some(t) = self.char_tokens.get(&c).cloned() {
                        self.advance(1);
                        t
                    } else if is_alpha(c) {
                        let mut buffer = Vec::new();
                        buffer.push(c);
                        self.advance(1);

                        while let Some(c) = self.peek(0)? {
                            if is_alpha(c) || is_digit(c) {
                                buffer.push(c);
                                self.advance(1);
                            } else {
                                break;
                            }
                        }

                        if let Some(t) = self.reserved_words.get(buffer.as_slice()).cloned() {
                            t
                        } else {
                            Token::Name(buffer.into_boxed_slice())
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
    // number.
    fn line_end(&mut self, mut output: Option<&mut Vec<u8>>) -> Result<(), Error> {
        let newline = self.peek(0).unwrap().unwrap();
        assert!(is_newline(newline));
        self.advance(1);
        if let Some(o) = output.as_mut() {
            o.push(newline);
        }

        if let Some(next_newline) = self.peek(0)? {
            if is_newline(next_newline) && next_newline != newline {
                self.advance(1);
                if let Some(o) = output.as_mut() {
                    o.push(next_newline);
                }
            }
        }

        self.line_number += 1;
        Ok(())
    }

    // Read a string on a single line delimited by ' or " that allows for \ escaping of certain
    // characters.
    fn short_string(&mut self) -> Result<Box<[u8]>, Error> {
        let mut output = Vec::new();
        let start_quote = self.peek(0).unwrap().unwrap();
        assert!(start_quote == SINGLE_QUOTE || start_quote == DOUBLE_QUOTE);
        self.advance(1);

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
                        output.push(ALERT_BEEP);
                    }

                    LOWER_B => {
                        self.advance(1);
                        output.push(BACKSPACE);
                    }

                    LOWER_F => {
                        self.advance(1);
                        output.push(FORM_FEED);
                    }

                    LOWER_N => {
                        self.advance(1);
                        output.push(NEWLINE);
                    }

                    LOWER_R => {
                        self.advance(1);
                        output.push(CARRIAGE_RETURN);
                    }

                    LOWER_T => {
                        self.advance(1);
                        output.push(HORIZONTAL_TAB);
                    }

                    LOWER_V => {
                        self.advance(1);
                        output.push(VERTICAL_TAB);
                    }

                    BACKSLASH => {
                        self.advance(1);
                        output.push(BACKSLASH);
                    }

                    SINGLE_QUOTE => {
                        self.advance(1);
                        output.push(SINGLE_QUOTE);
                    }

                    DOUBLE_QUOTE => {
                        self.advance(1);
                        output.push(DOUBLE_QUOTE);
                    }

                    NEWLINE | CARRIAGE_RETURN => {
                        self.line_end(Some(&mut output))?;
                    }

                    LOWER_X => {
                        self.advance(1);
                        let first = self.peek(0)?
                            .and_then(from_hex_digit)
                            .ok_or_else(|| err_msg("hexadecimal digit expected"))?;
                        let second = self.peek(1)?
                            .and_then(from_hex_digit)
                            .ok_or_else(|| err_msg("hexadecimal digit expected"))?;
                        output.push(first << 4 | second);
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
                            output.push(b);
                        }
                    }

                    LOWER_Z => {
                        self.advance(1);
                        while let Some(c) = self.peek(0)? {
                            if is_newline(c) {
                                self.line_end(None)?;
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

                            output.push(u as u8);
                        } else {
                            return Err(err_msg("invalid escape sequence"));
                        }
                    }
                }
            } else if c == start_quote {
                break;
            } else {
                output.push(c);
            }
        }

        Ok(output.into_boxed_slice())
    }

    // Read a [=*[...]=*] sequence with matching numbers of '='.
    fn long_string(&mut self, mut output: Option<&mut Vec<u8>>) -> Result<(), Error> {
        assert_eq!(self.peek(0).unwrap().unwrap(), LEFT_BRACKET);
        self.advance(1);

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
                    self.line_end(output.as_mut().map(|o| &mut **o))?;
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
                        if let Some(o) = output.as_mut() {
                            o.push(RIGHT_BRACKET);
                            for _ in 0..close_sep_length {
                                o.push(EQUALS);
                            }
                        }
                    }
                }

                c => {
                    if let Some(o) = output.as_mut() {
                        o.push(c);
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
    fn numeral(&mut self) -> Result<Token, Error> {
        let p1 = self.peek(0).unwrap().unwrap();
        assert!(p1 == PERIOD || is_digit(p1));

        let p2 = self.peek(1)?;
        let is_hex = p1 == NUM_0 && (p2 == Some(LOWER_X) || p2 == Some(UPPER_X));
        if is_hex {
            self.advance(2);
        }

        let mut whole_part = Vec::new();
        let mut frac_part = Vec::new();
        let mut exp_part = Vec::new();

        let mut has_radix = false;
        while let Some(c) = self.peek(0)? {
            if c == PERIOD && !has_radix {
                has_radix = true;
                self.advance(1);
            } else if !is_hex && is_digit(c) {
                if has_radix {
                    frac_part.push(from_digit(c).unwrap());
                } else {
                    whole_part.push(from_digit(c).unwrap());
                }
                self.advance(1);
            } else if is_hex && is_hex_digit(c) {
                if has_radix {
                    frac_part.push(from_hex_digit(c).unwrap());
                } else {
                    whole_part.push(from_hex_digit(c).unwrap());
                }
                self.advance(1);
            } else {
                break;
            }
        }

        let mut has_exp = false;
        let mut exp_neg = false;
        if let Some(exp_begin) = self.peek(0)? {
            if (is_hex && (exp_begin == LOWER_P || exp_begin == UPPER_P))
                || (!is_hex && (exp_begin == LOWER_E || exp_begin == UPPER_E))
            {
                has_exp = true;
                self.advance(1);

                if let Some(sign) = self.peek(0)? {
                    if sign == PLUS {
                        self.advance(1);
                    } else if sign == MINUS {
                        exp_neg = true;
                        self.advance(1);
                    }
                }

                while let Some(c) = self.peek(0)? {
                    if let Some(d) = from_digit(c) {
                        exp_part.push(d);
                        self.advance(1);
                    } else {
                        break;
                    }
                }
            }
        }

        if has_exp && exp_part.is_empty() {
            return Err(err_msg("malformed number"));
        }

        Ok(if has_exp || has_radix {
            Token::Float {
                is_hex,
                whole_part: whole_part.into_boxed_slice(),
                fractional_part: frac_part.into_boxed_slice(),
                exponent_part: exp_part.into_boxed_slice(),
                exponent_negative: exp_neg,
            }
        } else {
            Token::Integer {
                is_hex,
                digits: whole_part.into_boxed_slice(),
            }
        })
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
}

const SPACE: u8 = ' ' as u8;
const NEWLINE: u8 = '\n' as u8;
const CARRIAGE_RETURN: u8 = '\r' as u8;
const HORIZONTAL_TAB: u8 = '\t' as u8;
const ALERT_BEEP: u8 = 0x07;
const BACKSPACE: u8 = 0x08;
const VERTICAL_TAB: u8 = 0x0b;
const FORM_FEED: u8 = 0x0c;
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
    (PLUS, Token::Plus),
    (MINUS, Token::Minus),
    (ASTERISK, Token::Times),
    (CARET, Token::Pow),
    (PERCENT, Token::Mod),
    (AMPERSAND, Token::BitAnd),
    (PIPE, Token::BitOr),
    (COMMA, Token::Comma),
    (SEMICOLON, Token::SemiColon),
    (OCTOTHORPE, Token::Len),
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
