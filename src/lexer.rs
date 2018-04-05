use std::char;
use std::io::{self, Read};

use failure::{err_msg, Error};

#[derive(Debug)]
pub enum Token {
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
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
    Colon,
    DoubleColon,
    Len,
    LeftBracket,
    RightBracket,
    Number(Box<[u8]>),
    Integer(Box<[u8]>),
    Name(Box<[u8]>),
    String(Box<[u8]>),
}

pub struct Lexer<R: Read> {
    // When the source end is reached, this will be set to None
    source: Option<R>,
    peek: Vec<u8>,
    line_number: usize,
}

#[derive(Debug)]
pub enum NextToken {
    Token {
        token: Token,
        /// Line number that the token *begins* on, 0-indexed
        line_number: usize,
    },
    End,
}

impl<R: Read> Lexer<R> {
    pub fn new(source: R) -> Lexer<R> {
        Lexer {
            source: Some(source),
            peek: Vec::new(),
            line_number: 0,
        }
    }

    pub fn next(&mut self) -> Result<NextToken, Error> {
        let mut start_line;

        while self.source.is_some() {
            start_line = self.line_number;
            match self.lex() {
                Ok(Some(token)) => {
                    return Ok(NextToken::Token {
                        token,
                        line_number: start_line,
                    });
                }
                Ok(None) => {}
                Err(e) => {
                    self.source = None;
                    self.peek.clear();
                    return Err(e.context(format!("starting at line: {}", start_line))
                        .into());
                }
            }
        }

        Ok(NextToken::End)
    }

    fn lex(&mut self) -> Result<Option<Token>, Error> {
        let c = if let Some(c) = self.peek(0)? {
            c
        } else {
            return Ok(None);
        };

        Ok(match c {
            SPACE | HORIZONTAL_TAB | VERTICAL_TAB | FORM_FEED => {
                self.advance(1);
                None
            }

            NEWLINE | CARRIAGE_RETURN => {
                self.line_end(None)?;
                None
            }

            MINUS => {
                self.advance(1);
                if self.peek(0)? != Some(MINUS) {
                    Some(Token::Minus)
                } else {
                    self.advance(1);

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
                    None
                }
            }

            LEFT_BRACKET => {
                let next = self.peek(1)?;
                if next == Some(EQUALS) || next == Some(LEFT_BRACKET) {
                    let mut s = Vec::new();
                    self.long_string(Some(&mut s))?;
                    Some(Token::String(s.into_boxed_slice()))
                } else {
                    self.advance(1);
                    Some(Token::LeftBracket)
                }
            }

            EQUALS => {
                self.advance(1);
                if self.peek(0)? == Some(EQUALS) {
                    self.advance(1);
                    Some(Token::Equal)
                } else {
                    Some(Token::Assign)
                }
            }

            LESS_THAN => {
                self.advance(1);
                let next = self.peek(0)?;
                if next == Some(EQUALS) {
                    self.advance(1);
                    Some(Token::LessEqual)
                } else if next == Some(LESS_THAN) {
                    self.advance(1);
                    Some(Token::ShiftLeft)
                } else {
                    Some(Token::Less)
                }
            }

            GREATER_THAN => {
                self.advance(1);
                let next = self.peek(0)?;
                if next == Some(EQUALS) {
                    self.advance(1);
                    Some(Token::GreaterEqual)
                } else if next == Some(GREATER_THAN) {
                    self.advance(1);
                    Some(Token::ShiftRight)
                } else {
                    Some(Token::Greater)
                }
            }

            FORWARD_SLASH => {
                self.advance(1);
                if self.peek(0)? == Some(FORWARD_SLASH) {
                    self.advance(1);
                    Some(Token::IDiv)
                } else {
                    Some(Token::Div)
                }
            }

            TILDE => {
                self.advance(1);
                if self.peek(0)? == Some(EQUALS) {
                    self.advance(1);
                    Some(Token::NotEqual)
                } else {
                    Some(Token::BitNot)
                }
            }

            COLON => {
                self.advance(1);
                if self.peek(0)? == Some(COLON) {
                    self.advance(1);
                    Some(Token::DoubleColon)
                } else {
                    Some(Token::Colon)
                }
            }

            DOUBLE_QUOTE | SINGLE_QUOTE => {
                let mut s = Vec::new();
                self.short_string(&mut s)?;
                Some(Token::String(s.into_boxed_slice()))
            }

            c => unimplemented!("{:?} {:?}", self.line_number, c),
        })
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
    fn short_string(&mut self, output: &mut Vec<u8>) -> Result<(), Error> {
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
                        self.line_end(Some(output))?;
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
                            if is_space(c) {
                                self.advance(1);
                            } else if is_newline(c) {
                                self.line_end(None)?;
                            } else {
                                break;
                            }
                        }
                    }

                    c => {
                        if from_digit(c).is_some() {
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

        Ok(())
    }

    // Read a [=*[...]=*] sequence with matching numbers of '='.  If output is given, will fill
    // output with the inside of the long string delimiters.
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
                    while self.peek(close_sep_length + 1)? == Some(EQUALS) {
                        close_sep_length += 1;
                    }

                    if open_sep_length == close_sep_length
                        && self.peek(close_sep_length + 1)? == Some(RIGHT_BRACKET)
                    {
                        self.advance(close_sep_length + 2);
                        break;
                    } else {
                        if let Some(o) = output.as_mut() {
                            o.push(RIGHT_BRACKET);
                        }
                        self.advance(1);
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

    // Advance the read position by the given amount.  All characters advanced over must have been
    // previously peeked.  `advance(1)` advances the read position by 1, `advance(0)` does nothing.
    fn advance(&mut self, n: usize) {
        assert!(
            n <= self.peek.len(),
            "cannot advance over un-peeked characters"
        );
        self.peek.drain(0..n);
    }

    // Look at the nth character after the current read position.  `peek(0)` looks at the
    // immediately next character, `peek(1)` the character after that, and so on.
    fn peek(&mut self, skip: usize) -> Result<Option<u8>, io::Error> {
        let mut at_end = false;
        if let Some(source) = self.source.as_mut() {
            while self.peek.len() <= skip {
                if let Some(c) = read_char(source)? {
                    self.peek.push(c);
                } else {
                    at_end = true;
                    break;
                }
            }
        }

        if at_end {
            self.source = None;
        }

        Ok(self.peek.get(skip).cloned())
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

const MINUS: u8 = '-' as u8;
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
const DOUBLE_QUOTE: u8 = '"' as u8;
const SINGLE_QUOTE: u8 = '\'' as u8;

const LOWER_A: u8 = 'a' as u8;
const LOWER_B: u8 = 'b' as u8;
const LOWER_F: u8 = 'f' as u8;
const LOWER_N: u8 = 'n' as u8;
const LOWER_R: u8 = 'r' as u8;
const LOWER_T: u8 = 't' as u8;
const LOWER_V: u8 = 'v' as u8;
const LOWER_X: u8 = 'x' as u8;
const LOWER_U: u8 = 'u' as u8;
const LOWER_Z: u8 = 'z' as u8;

const UPPER_A: u8 = 'A' as u8;
const UPPER_F: u8 = 'F' as u8;

const NUM_0: u8 = '0' as u8;
const NUM_9: u8 = '9' as u8;

fn is_newline(c: u8) -> bool {
    c == NEWLINE || c == CARRIAGE_RETURN
}

fn is_space(c: u8) -> bool {
    c == SPACE || c == HORIZONTAL_TAB || c == VERTICAL_TAB || c == FORM_FEED
}

fn from_digit(c: u8) -> Option<u8> {
    if c >= NUM_0 && c <= NUM_9 {
        Some(c - NUM_0)
    } else {
        None
    }
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

// Read a single character from a Read implementation with retry on io::ErrorKind::Interrupted
fn read_char<R: Read>(r: &mut R) -> Result<Option<u8>, io::Error> {
    let mut c = [0];
    loop {
        match r.read(&mut c) {
            Ok(0) => break Ok(None),
            Ok(_) => {
                break Ok(Some(c[0]));
            }
            Err(e) => {
                if e.kind() != io::ErrorKind::Interrupted {
                    break Err(e);
                }
            }
        }
    }
}
