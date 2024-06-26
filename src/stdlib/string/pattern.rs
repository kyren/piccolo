// This is a direct port of PUC-Rio Lua 5.4.6's lstrlib.c to Rust
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

use crate::{string, CallbackReturn, Context, Error, Stack, Value};
/// Syntax:
///
/// class :=
///     | '[' ('^')? set_inner ']'
///     | '%' classchar
///     | '.'
///
/// set_inner := ('-')? ( '%' classchar | setchar '-' setchar | setchar )* ('-')?
///
/// classchar := one of "acdglpsuwxz" or "ACDGLPSUWXZ"
///            | any char (interpreted as itself)
/// setchar := not

/// This is what you get for not having defined a syntax...
///
/// string.match("$", "[#-%%]") -> "$"
/// string.match("&", "[%-']") -> nil
/// string.match("&", "[%%-']") -> nil
/// string.match("]", "[]]") -> "]"
use std::borrow::Cow;
use std::io::Write;
use thiserror::Error;

const ESCAPE: u8 = b'%';

const MAX_PAT_LENGTH: usize = 1 << 24;

fn has_specials(pat: &[u8]) -> bool {
    pat.iter().any(|b| {
        matches!(
            *b,
            b'%' | b'^' | b'$' | b'*' | b'+' | b'?' | b'.' | b'(' | b')' | b'[' | b']' | b'-'
        )
    })
}

fn plain_find(pat: &[u8], str: &[u8]) -> Option<usize> {
    memchr::memmem::find(str, pat)
}

#[derive(Debug)]
struct MatchResult {
    str_idx_end: usize,
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

#[derive(Debug)]
struct PartialCapture {
    start: usize,
    end: usize,
    pos: bool,
    closed: bool,
}

struct MatchState<'a> {
    pat: &'a [u8],
    str: &'a [u8],
    captures: Vec<PartialCapture>,
}

#[derive(Debug, Error)]
pub enum MatchError {
    #[error("Unclosed capture group")]
    UnclosedCapture,
    #[error("Mismatched parentheses")]
    CaptureMismatch,
    #[error("Invalid balanced pattern")]
    Balanced,
    #[error("Invalid frontier pattern")]
    Frontier,
    #[error("Invalid backreference")]
    Backref,
    #[error("Invalid class in pattern")]
    Class,
}

pub fn str_find(
    pat: &[u8],
    str: &[u8],
    start: usize,
    plain: bool,
) -> Result<Option<Match>, MatchError> {
    if start > str.len() {
        Ok(None)
    } else if plain || pat.is_empty() || !has_specials(pat) {
        let res = plain_find(pat, &str[start..]).map(|idx| Match {
            start: start + idx,
            end: start + idx + pat.len(),
            captures: Vec::new(),
        });
        Ok(res)
    } else {
        // In gmatch, '^' is nominally not treated as an anchor because of iteration;
        // however, this implementation matches the behavior of PRLua...
        let (anchored, pat_base) = if let Some(b'^') = pat.first() {
            (true, &pat[1..])
        } else {
            (false, pat)
        };
        let mut state = MatchState {
            pat: pat_base,
            str,
            captures: Vec::new(),
        };

        let mut stack = Vec::with_capacity(4);

        for base in start..=str.len() {
            let result;
            (result, stack) = do_match(&mut state, base, stack)?;
            if let Some(m) = result {
                if !state.captures.iter().all(|cs| cs.closed) {
                    return Err(MatchError::UnclosedCapture);
                }
                return Ok(Some(Match {
                    start: base,
                    end: m.str_idx_end,
                    captures: state
                        .captures
                        .into_iter()
                        .map(|c| Capture {
                            start: c.start,
                            end: c.end,
                            pos: c.pos,
                        })
                        .collect(),
                }));
            } else {
                assert!(state.captures.is_empty());
            }
            if anchored {
                break;
            }
            stack.clear();
        }
        Ok(None)
    }
}

pub fn lua_find<'gc>(
    ctx: Context<'gc>,
    stack: &mut Stack<'gc, '_>,
) -> Result<CallbackReturn<'gc>, Error<'gc>> {
    let (str, pat, init, plain) =
        stack.consume::<(string::String, string::String, Option<i64>, Option<bool>)>(ctx)?;
    // TODO: overflow checks on 32 bit
    let start = match init {
        Some(0) | None => 0,
        Some(v @ 1..) => (v - 1) as usize,
        Some(v @ ..=-1) => (str.len() + v).max(0) as usize,
    };

    let res = str_find(&pat, &str, start, plain.unwrap_or(false))?;

    if let Some(m) = res {
        // Lua expects inclusive ranges
        stack.push_back(Value::Integer(m.start as i64 + 1));
        stack.push_back(Value::Integer(m.end as i64));
    } else {
        stack.push_back(Value::Nil);
    }
    Ok(CallbackReturn::Return)
}

pub fn lua_match<'gc>(
    ctx: Context<'gc>,
    stack: &mut Stack<'gc, '_>,
) -> Result<CallbackReturn<'gc>, Error<'gc>> {
    let (str, pat, init) = stack.consume::<(string::String, string::String, Option<i64>)>(ctx)?;
    // TODO: overflow checks on 32 bit
    let start = match init {
        Some(0) | None => 0,
        Some(v @ 1..) => (v - 1) as usize,
        Some(v @ ..=-1) => (str.len() + v).max(0) as usize,
    };

    let res = str_find(&pat, &str, start, false)?;

    if let Some(m) = res {
        if m.captures.is_empty() {
            stack.push_back(Value::String(ctx.intern(&str[m.start..m.end])));
        } else {
            stack.extend(m.captures.iter().map(|c| {
                if c.pos {
                    Value::Integer(c.start as i64 + 1)
                } else {
                    Value::String(ctx.intern(&str[c.start..c.end]))
                }
            }));
        }
    } else {
        stack.push_back(Value::Nil);
    }
    Ok(CallbackReturn::Return)
}

fn last_unclosed_capture_idx(state: &MatchState<'_>) -> Option<usize> {
    for i in (0..state.captures.len()).rev() {
        if !state.captures[i].closed {
            return Some(i);
        }
    }
    None
}

// on success = if rest of pattern matches from last call
// on failure = if rest of pattern does not match from last call
enum StackFrame {
    // on success, return success
    // on failure, continue with str_idx, pos_idx
    Run {
        pat_idx: usize,
        str_idx: usize,
    },

    // on success, check that the given capture is closed and return success
    // on failure, pop the top capture and return failure
    FinalizeCapture,
    // on success, return success
    // on failure, reopen the given capture and return failure
    ReopenCapture(usize),
    // on success, return success
    // on failure:
    // - if class matches, advance str_idx by one and try matching rest of pattern again
    // - if class does not match or past end of string, return failure
    RepeatMin {
        pat_idx: usize,
        str_idx: usize,
        class: Class,
        pat_start: usize,
    },
    // on success, return success
    // on failure:
    // - decrement i, try matching rest of pattern again
    // - if i ==(?) 0 return failure
    RepeatMax {
        pat_idx: usize,
        str_idx: usize,
        i: usize,
    },
}

// Only returns at the end of a match
fn do_match(
    state: &mut MatchState,
    str_idx: usize,
    mut stack: Vec<StackFrame>,
) -> Result<(Option<MatchResult>, Vec<StackFrame>), MatchError> {
    // println!("match call {:?} {:?}", pat_idx, str_idx);
    let mut next_frame = Some(StackFrame::Run {
        pat_idx: 0,
        str_idx,
    });
    'call_stack: while let Some(frame) = next_frame {
        // Only runs when submatch fails
        let (mut pat_idx, mut str_idx) = match frame {
            StackFrame::FinalizeCapture => {
                state.captures.pop();
                next_frame = stack.pop();
                continue;
            }
            StackFrame::ReopenCapture(cap) => {
                state.captures[cap].closed = false;
                next_frame = stack.pop();
                continue;
            }
            StackFrame::RepeatMin {
                pat_idx,
                str_idx,
                class,
                pat_start,
            } => {
                // just returned from call starting at str_idx + i - 1; try str_idx + i
                if str_idx < state.str.len() {
                    if match_class(state.str[str_idx], class, &state.pat[pat_start..pat_idx]) {
                        stack.push(StackFrame::RepeatMin {
                            pat_idx,
                            str_idx: str_idx + 1,
                            class,
                            pat_start,
                        });
                    }
                    (pat_idx, str_idx)
                } else if str_idx == state.str.len() {
                    (pat_idx, str_idx)
                } else {
                    next_frame = stack.pop();
                    continue;
                }
            }
            StackFrame::RepeatMax {
                pat_idx,
                str_idx,
                i,
            } => {
                // just returned from call starting at str_idx + i; try str_idx + i - 1
                if i > 0 {
                    stack.push(StackFrame::RepeatMax {
                        pat_idx,
                        str_idx,
                        i: i - 1,
                    });
                    (pat_idx, str_idx + i - 1)
                } else {
                    next_frame = stack.pop();
                    continue;
                }
            }
            StackFrame::Run { pat_idx, str_idx } => (pat_idx, str_idx),
        };

        'main_loop: loop {
            let Some(b) = state.pat.get(pat_idx).copied() else {
                // println!("p{} s{} end", pat_idx, str_idx);
                assert!(str_idx <= state.str.len());
                // Skip unwinding the stack, just return the match
                return Ok((
                    Some(MatchResult {
                        str_idx_end: str_idx,
                    }),
                    stack,
                ));
            };
            let next = state.pat.get(pat_idx + 1);
            match b {
                b'(' => {
                    // println!("p{} s{} start-capture", pat_idx, str_idx);
                    // start capture
                    // push capture onto capture stack
                    // try match rest of pattern
                    // on fail, pop capture?
                    // return match res

                    // if immediately followed by ')', is position capture
                    // (yield int instead of str)
                    if next == Some(&b')') {
                        state.captures.push(PartialCapture {
                            start: str_idx,
                            end: str_idx,
                            pos: true,
                            closed: false,
                        });
                    } else {
                        state.captures.push(PartialCapture {
                            start: str_idx,
                            end: str_idx,
                            pos: false,
                            closed: false,
                        });
                    }
                    stack.push(StackFrame::FinalizeCapture);
                    next_frame = Some(StackFrame::Run {
                        pat_idx: pat_idx + 1,
                        str_idx,
                    });
                    continue 'call_stack;
                }
                b')' => {
                    // println!("p{} s{} end-capture", pat_idx, str_idx);
                    // end capture
                    // find most recent unclosed capture
                    // set end to current str index
                    // try matching rest of pattern
                    // if failed, re-mark as unfinished?
                    let cap =
                        last_unclosed_capture_idx(&state).ok_or(MatchError::CaptureMismatch)?;
                    state.captures[cap].end = str_idx;
                    state.captures[cap].closed = true;
                    stack.push(StackFrame::ReopenCapture(cap));
                    pat_idx = pat_idx + 1;
                    continue 'main_loop;
                }
                b'$' if pat_idx == state.pat.len() - 1 => {
                    // println!("p{} s{} end-match", pat_idx, str_idx);
                    // end of str (but only if at the end of pat)
                    // if at end of str, return str; else fail
                    if str_idx == state.str.len() {
                        // Skip unwinding the stack, just return the match
                        return Ok((
                            Some(MatchResult {
                                str_idx_end: str_idx,
                            }),
                            stack,
                        ));
                    } else {
                        break 'main_loop;
                    }
                }
                ESCAPE if next == Some(&b'b') => {
                    // println!("p{} s{} balanced", pat_idx, str_idx);
                    // balanced str
                    // match balanced pairs of tokens
                    // if matched, continue
                    // else return fail
                    let (s, e) = (state.pat.get(pat_idx + 2), state.pat.get(pat_idx + 3));
                    let (Some(start), Some(end)) = (s, e) else {
                        return Err(MatchError::Balanced);
                    };

                    // TODO: not backtracking, but may be slow; interruptible?
                    match match_balanced(state, *start, *end, str_idx) {
                        Some(res) => {
                            str_idx = res.str_idx_end;
                            pat_idx = pat_idx + 4;
                        }
                        None => break 'main_loop,
                    }
                }
                ESCAPE if next == Some(&b'f') => {
                    // println!("p{} s{} frontier", pat_idx, str_idx);
                    // frontier
                    // parse set class
                    // if !classmatches(s - 1) and classmatches(s), advance pat and continue
                    // else return fail

                    if !matches!(state.pat.get(pat_idx + 2), Some(b'['))
                        || pat_idx + 3 >= state.pat.len()
                    {
                        return Err(MatchError::Frontier);
                    }
                    let (inv, len) =
                        parse_set(&state.pat[pat_idx + 3..]).ok_or(MatchError::Frontier)?;
                    let pat_end = pat_idx + 3 + len;
                    let class = Class::Set(inv);
                    let class_slice = &state.pat[pat_idx + 2..pat_end];

                    // "The beginning and the end of the subject are handled as if they were the character '\0'"
                    // let before = if str_idx == 0 || str_idx > state.str.len() {
                    //     b'\0'
                    // } else {
                    //     state.str[str_idx - 1]
                    // }
                    let before = str_idx
                        .checked_sub(1)
                        .and_then(|i| state.str.get(i))
                        .copied()
                        .unwrap_or(b'\0');
                    let after = state.str.get(str_idx).copied().unwrap_or(b'\0');
                    if !match_class(before, class, class_slice)
                        && match_class(after, class, class_slice)
                    {
                        pat_idx = pat_end;
                    } else {
                        break 'main_loop;
                    }
                }
                ESCAPE if matches!(next, Some(b'0'..=b'9')) => {
                    // println!("p{} s{} backref", pat_idx, str_idx);
                    let n = (next.unwrap() - b'0') as usize;
                    // match prev capture literally
                    // if matches, advance str and pat; continue
                    // else return fail

                    if n == 0 {
                        return Err(MatchError::Backref);
                    }
                    let capture = state.captures.get(n - 1).ok_or(MatchError::Backref)?;
                    if !capture.closed {
                        return Err(MatchError::Backref);
                    }
                    let slice = &state.str[capture.start..capture.end];

                    if state.str[str_idx..].starts_with(slice) {
                        str_idx += slice.len();
                        assert!(str_idx <= state.str.len());
                        pat_idx += 2;
                    } else {
                        break 'main_loop;
                    }
                }
                _ => {
                    let (class, suffix, pat_len) =
                        parse_class(&state.pat[pat_idx..]).ok_or(MatchError::Class)?;
                    let pat_end = pat_idx + pat_len;
                    let pat_slice = &state.pat[pat_idx..pat_end];

                    // println!("p{} s{} class {:?} {:?}", pat_idx, str_idx, class, suffix);
                    if str_idx < state.str.len()
                        && match_class(state.str[str_idx], class, pat_slice)
                    {
                        match suffix {
                            Some(Suffix::Optional) => {
                                // println!("p{} s{} optional found start", pat_idx, str_idx);
                                // if matching with this fails, try matching without it afterwards
                                stack.push(StackFrame::Run {
                                    pat_idx: pat_end,
                                    str_idx,
                                });
                                pat_idx = pat_end;
                                str_idx = str_idx + 1;
                                continue 'main_loop;
                            }
                            Some(Suffix::OneOrMoreMax) => {
                                // println!("p{} s{} one-more-max found start", pat_idx, str_idx);
                                str_idx += 1;
                                // repeat single match until fail, count matches
                                let mut i = 0;
                                while str_idx + i < state.str.len()
                                    && match_class(state.str[str_idx + i], class, pat_slice)
                                {
                                    i += 1;
                                }

                                stack.push(StackFrame::RepeatMax {
                                    pat_idx: pat_end,
                                    str_idx,
                                    i,
                                });
                                pat_idx = pat_end;
                                str_idx = str_idx + i;
                                continue 'main_loop;
                            }
                            Some(Suffix::AnyNumMax) => {
                                // println!("p{} s{} any-max found start", pat_idx, str_idx);
                                // repeat single match until fail, count matches
                                let mut i = 1;
                                while str_idx + i < state.str.len()
                                    && match_class(state.str[str_idx + i], class, pat_slice)
                                {
                                    i += 1;
                                }

                                stack.push(StackFrame::RepeatMax {
                                    pat_idx: pat_end,
                                    str_idx,
                                    i,
                                });
                                pat_idx = pat_end;
                                str_idx = str_idx + i;
                                continue 'main_loop;
                            }
                            Some(Suffix::AnyNumMin) => {
                                // println!("p{} s{} any-min found start", pat_idx, str_idx);
                                stack.push(StackFrame::RepeatMin {
                                    pat_idx: pat_end,
                                    str_idx: str_idx + 1,
                                    class,
                                    pat_start: pat_idx,
                                });
                                pat_idx = pat_end;
                                continue 'main_loop;
                            }
                            None => {
                                // println!("p{} s{} single", pat_idx, str_idx);
                                // no suffix
                                // advance str by 1
                                // advance pat to end of class
                                // loop
                                str_idx += 1;
                                pat_idx = pat_end;
                            }
                        }
                    } else {
                        // No match
                        if let Some(Suffix::Optional | Suffix::AnyNumMax | Suffix::AnyNumMin) =
                            suffix
                        {
                            // println!("p{} s{} no-match allowed", pat_idx, str_idx);
                            // class allows empty
                            // advance pattern
                            // loop
                            pat_idx = pat_end;
                        } else {
                            // println!("p{} s{} fail match", pat_idx, str_idx);
                            // fail match, return null
                            break 'main_loop;
                        }
                    }
                }
            }
        }
        next_frame = stack.pop();
    }
    Ok((None, stack))
}

#[derive(Copy, Clone, Debug)]
enum Class {
    Any,
    Lit(u8),
    Class(u8),
    Set(bool),
}
#[derive(Copy, Clone, Debug)]
enum Suffix {
    Optional,
    OneOrMoreMax,
    AnyNumMax,
    AnyNumMin,
}

fn parse_class(pat: &[u8]) -> Option<(Class, Option<Suffix>, usize)> {
    let (mut i, class) = match pat[0] {
        b'.' => (1, Class::Any),
        b'%' => (2, Class::Class(*pat.get(1)?)),
        b'[' => {
            let (inv, end) = parse_set(&pat[1..])?;
            (1 + end, Class::Set(inv))
        }
        c => (1, Class::Lit(c)),
    };
    let suffix = match pat.get(i) {
        Some(b'?') => Some(Suffix::Optional),
        Some(b'+') => Some(Suffix::OneOrMoreMax),
        Some(b'*') => Some(Suffix::AnyNumMax),
        Some(b'-') => Some(Suffix::AnyNumMin),
        _ => None,
    };
    i += suffix.is_some() as usize;
    Some((class, suffix, i))
}

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

fn match_class(ch: u8, class: Class, pat: &[u8]) -> bool {
    match class {
        Class::Any => true,
        Class::Lit(l) => ch == l,
        Class::Class(cl) => match_char_class(cl, ch),
        Class::Set(inv) => match_set(ch, inv, &pat[1 + inv as usize..pat.len() - 1]),
    }
}

fn match_char_class(class: u8, c: u8) -> bool {
    let invert = class.is_ascii_uppercase();
    invert
        ^ match class.to_ascii_lowercase() {
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
        }
}

fn match_set(char: u8, invert: bool, set: &[u8]) -> bool {
    // Get the optimizer to remove overflow checks on i + 2 < set.len()
    if set.len() > MAX_PAT_LENGTH {
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

fn match_balanced(
    state: &mut MatchState,
    start: u8,
    end: u8,
    str_idx: usize,
) -> Option<MatchResult> {
    if state.str.get(str_idx) != Some(&start) {
        return None;
    }
    let mut count = 1;
    for i in str_idx + 1..state.str.len() {
        let c = state.str[i];
        // Prioritize end, when start == end
        if c == end {
            count -= 1;
            if count == 0 {
                return Some(MatchResult { str_idx_end: i + 1 });
            }
        } else if c == start {
            count += 1;
        }
    }
    None
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
        let mut o = match output {
            None => replace[0..j].to_owned(),
            Some(mut o) => {
                o.extend_from_slice(&replace[i..j]);
                o
            }
        };
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
        output = Some(o);
    }
    Ok(match output {
        None => Cow::Borrowed(replace),
        Some(mut o) => {
            o.extend_from_slice(&replace[i..replace.len()]);
            Cow::Owned(o)
        }
    })
}
