// This is a port of part of PUC-Rio Lua 5.4.6's lstrlib.c to Rust
// The license of the original implementation is included below:

/******************************************************************************
* Copyright (C) 1994-2024 Lua.org, PUC-Rio.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/

pub mod lua;
pub mod seq;
pub mod stack;

use std::borrow::Cow;
use std::io::Write;
use thiserror::Error;

const ESCAPE: u8 = b'%';

pub trait FindBackend {
    fn str_find(
        pat: &[u8],
        str: &[u8],
        start: usize,
        plain: bool,
    ) -> Result<Option<Match>, MatchError> {
        str_find::<Self>(pat, str, start, plain)
    }

    fn str_find_impl(
        pat: &[u8],
        str: &[u8],
        start: usize,
        anchored: bool,
    ) -> Result<Option<Match>, MatchError>;
}

pub struct SeqBackend;
impl FindBackend for SeqBackend {
    fn str_find_impl(
        pat: &[u8],
        str: &[u8],
        start: usize,
        anchored: bool,
    ) -> Result<Option<Match>, MatchError> {
        seq::str_find_impl(pat, str, start, anchored)
    }
}

pub struct StackBackend;
impl FindBackend for StackBackend {
    fn str_find_impl(
        pat: &[u8],
        str: &[u8],
        start: usize,
        anchored: bool,
    ) -> Result<Option<Match>, MatchError> {
        stack::str_find_impl(pat, str, start, anchored)
    }
}

#[derive(Debug)]
pub struct Match {
    pub start: usize,
    pub end: usize,
    pub captures: Vec<Capture>,
}

#[derive(Debug)]
pub struct Capture {
    pub start: usize,
    pub end: usize,
    pub pos: bool,
}

#[derive(Debug, Error)]
pub enum MatchError {
    #[error("Unclosed capture group")]
    UnclosedCapture,
    #[error("Mismatched parentheses")]
    CapMismatch,
    #[error("Invalid balanced pattern")]
    Balanced,
    #[error("Invalid frontier pattern")]
    Frontier,
    #[error("Invalid backreference")]
    Backref,
    #[error("Invalid class in pattern")]
    Class,
}

/// Finds the first match of `pat` within `str`, starting at index `start`
/// in the string. If `plain` is true, then the pattern is treated as a literal
/// string with no special meaning.
///
/// Returns a [`Match`], if one is found, containing the associated capture
/// group indices.
///
/// If the pattern is invalid, this may return an error, though it is not
/// guaranteed to.
///
/// Anchored patterns (patterns starting with `^`) will only return a match
/// it it starts exactly at the provided `start` parameter.  Methods that
/// require different behavior, such as `gmatch` and `gsub`, must handle
/// anchored patterns explicitly.
pub fn str_find<F: FindBackend + ?Sized>(
    pat: &[u8],
    str: &[u8],
    start: usize,
    plain: bool,
) -> Result<Option<Match>, MatchError> {
    if start > str.len() {
        Ok(None)
    } else if plain || pat.is_empty() || !has_specials(pat) {
        // If the pattern is plain, fall back to a simple string search
        let found = plain_find(pat, &str[start..]);
        Ok(found.map(|idx| Match {
            start: start + idx,
            end: start + idx + pat.len(),
            captures: Vec::new(),
        }))
    } else {
        // a '^' a the start of a pattern means that it is anchored:
        // - for `match` and `find`, any match must start at the start of the string,
        //   or the `start` parameter if provided.
        // - for `gmatch`, anchored patterns return no results, regardless of `start`
        // - for `gsub`, anchored patterns only replace the match that starts at the
        //   start of the string.

        // If the pattern is anchored, skip the anchor character.
        let (anchored, pat_base) = match pat.split_first() {
            Some((b'^', slice)) => (true, slice),
            _ => (false, pat),
        };

        F::str_find_impl(pat_base, str, start, anchored)
    }
}

#[derive(Debug, Error)]
pub enum SubstituteError {
    #[error("Replacement uses non-existant capture {0}")]
    MissingCapture(usize),
    #[error("Invalid escape in replacement pattern")]
    InvalidEscape,
}

pub fn expand_substitution<'a>(
    m: &'_ Match,
    str: &'a [u8],
    replace: &'a [u8],
) -> Result<Cow<'a, [u8]>, SubstituteError> {
    let mut output: Option<Vec<u8>> = None;
    let mut i = 0;
    while let Some(j) = memchr::memchr(b'%', &replace[i..]).map(|j| i + j) {
        let mut o = output.get_or_insert_with(|| Vec::with_capacity(replace.len()));
        o.extend_from_slice(&replace[i..j]);
        match replace.get(j + 1) {
            Some(b'%') => {
                o.push(b'%');
                i = j + 2;
            }
            Some(n @ b'0'..=b'9') => {
                let index = (*n - b'0') as usize;
                let (start, end, is_pos) = if index == 0 || m.captures.is_empty() && index == 1 {
                    (m.start, m.end, false)
                } else {
                    let c = m
                        .captures
                        .get(index - 1)
                        .ok_or(SubstituteError::MissingCapture(index))?;
                    (c.start, c.end, c.pos)
                };

                if is_pos {
                    write!(&mut o, "{}", start + 1).unwrap();
                } else {
                    o.extend_from_slice(&str[start..end]);
                }
                i = j + 2;
            }
            Some(_) | None => return Err(SubstituteError::InvalidEscape),
        }
    }
    Ok(match output {
        None => Cow::Borrowed(replace),
        Some(mut o) => {
            o.extend_from_slice(&replace[i..replace.len()]);
            Cow::Owned(o)
        }
    })
}

/// Run a plain string-search of `pat within `str`; currently
/// implemented as [`memchr::memmem::find`].
fn plain_find(pat: &[u8], str: &[u8]) -> Option<usize> {
    memchr::memmem::find(str, pat)
}

/// Returns whether a string has any pattern special characters
fn has_specials(pat: &[u8]) -> bool {
    pat.iter().any(|b| {
        matches!(
            *b,
            b'%' | b'^' | b'$' | b'*' | b'+' | b'?' | b'.' | b'(' | b')' | b'[' | b']' | b'-'
        )
    })
}

/// Parse the given slice as a set, assuming it starts the character
/// after the initial `[`.  Returns whether the set starts with `^`,
/// and the index of end of the set.
fn parse_set(s: &[u8]) -> Option<(bool, usize)> {
    let invert = matches!(s.get(0), Some(b'^'));
    let mut i = invert as usize;
    while let Some(&c) = s.get(i) {
        match c {
            b']' => {
                if i != invert as usize {
                    return Some((invert, i + 1));
                } else {
                    // first actual char in the set
                    // for  cursed reasons, []] is a set containing ']'
                    i += 1;
                }
            }
            ESCAPE => i += 2,
            _ => i += 1,
        }
    }
    None
}

/// Check whether the given character is contained within the given set.
/// The set slice should be the range `s[inverted .. end - 1]`, from the
/// results of a [`parse_set`] call.
fn match_set(char: u8, invert: bool, set: &[u8]) -> bool {
    // Get the optimizer to remove overflow checks on i + 2 < set.len()
    if set.len() == usize::MAX {
        return false;
    }

    let mut i = 0;
    while i < set.len() {
        if set[i] == ESCAPE {
            i += 1;
            if i < set.len() {
                if match_char_class(set[i], char) {
                    return !invert;
                }
                i += 1;
            }
        } else if i + 2 < set.len() && set[i + 1] == b'-' {
            if char >= set[i] && char <= set[i + 2] {
                return !invert;
            }
            i += 3;
        } else if set[i] == char {
            return !invert;
        } else {
            i += 1;
        }
    }
    return invert;
}

/// Match a sequence of nested balanced pairs of the given `start` and
/// `end` chars, with a `start` at the current index.  If successful,
/// return an index one past the last `end` char.
fn match_balanced(str: &[u8], start: u8, end: u8, str_idx: usize) -> Option<usize> {
    if str.get(str_idx) != Some(&start) {
        return None;
    }
    let mut count = 1;
    for i in str_idx + 1..str.len() {
        let c = str[i];
        // Prioritize end, when start == end
        if c == end {
            count -= 1;
            if count == 0 {
                return Some(i + 1);
            }
        } else if c == start {
            count += 1;
        }
    }
    None
}

/// Returns whether the character matches the given character class,
/// of the form `%[c]`; if the class char is not recognized, it is
/// compared directly with the char; otherwise, if the class char is
/// uppercase, the match is inverted.
fn match_char_class(class: u8, c: u8) -> bool {
    let invert = class.is_ascii_uppercase();
    (match class.to_ascii_lowercase() {
        b'a' => u8::is_ascii_alphabetic(&c),
        b'c' => u8::is_ascii_control(&c),
        b'd' => u8::is_ascii_digit(&c),
        b'g' => u8::is_ascii_graphic(&c),
        b'l' => u8::is_ascii_lowercase(&c),
        b'p' => u8::is_ascii_punctuation(&c),
        b's' => u8::is_ascii_whitespace(&c),
        b'u' => u8::is_ascii_uppercase(&c),
        b'w' => u8::is_ascii_alphanumeric(&c),
        b'x' => u8::is_ascii_hexdigit(&c),
        b'z' => c == 0, // deprecated is-zero class
        _ => return class == c,
    } ^ invert)
}
