// This is a port of part of PUC-Rio Lua 5.4.6's lstrlib.c to Rust.
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

use std::convert::Infallible;

use super::{match_balanced, match_char_class, match_set, parse_set};
use super::{Capture, Match, MatchError, ESCAPE};

struct MatchState<'a> {
    pat: &'a [u8],
    str: &'a [u8],
    captures: Vec<PartialCapture>,
}

#[derive(Clone, Copy)]
struct PartialCapture {
    start: usize,
    end: usize,
    pos: bool,
    closed: bool,
}

struct MatchEnd(usize);

fn convert_captures(captures: Vec<PartialCapture>) -> Vec<Capture> {
    captures
        .into_iter()
        .map(|c| Capture {
            start: c.start,
            end: c.end,
            pos: c.pos,
        })
        .collect()
}

pub fn str_find_impl(
    pat: &[u8],
    str: &[u8],
    start: usize,
    anchored: bool,
) -> Result<Option<Match>, MatchError> {
    assert!(start <= str.len());

    let mut state = MatchState {
        pat,
        str,
        captures: Vec::new(),
    };

    for base in start..=str.len() {
        if let Some(MatchEnd(end)) = do_match(&mut state, 0, base)? {
            debug_assert!(state.captures.iter().all(|cs| cs.closed));

            // if !state.captures.iter().all(|cs| cs.closed) {
            //     return Err(MatchError::UnclosedCapture);
            // }

            let captures = convert_captures(state.captures);
            return Ok(Some(Match {
                start: base,
                end,
                captures,
            }));
        } else {
            assert!(state.captures.is_empty());
        }
        if anchored {
            break;
        }
    }
    Ok(None)
}

fn check_capture_closed(state: &mut MatchState, cap_idx: usize) -> bool {
    state
        .captures
        .get(cap_idx)
        .map(|l| l.closed)
        .unwrap_or(false)
}

// Only returns at the end of a match
fn do_match(
    state: &mut MatchState,
    mut pat_idx: usize,
    mut str_idx: usize,
) -> Result<Option<MatchEnd>, MatchError> {
    while let Some(b) = state.pat.get(pat_idx).copied() {
        // This being a reference is aparently load-bearing for optimizations
        // (I guess some part of this is *really* fragile...)
        let next = state.pat.get(pat_idx + 1);

        // Every branch in this match must either return or continue
        let _: Infallible = match (b, next) {
            (b'(', _) => {
                // Push capture onto the capture stack
                // If immediately followed by ')', this is a position capture

                // This branching code is very fragile, and can deoptimize
                // the entire match loop (~50% slowdown).  Be careful here!
                let is_pos = matches!(next, Some(b')'));
                if is_pos {
                    state.captures.push(PartialCapture {
                        start: str_idx,
                        end: str_idx,
                        pos: is_pos,
                        closed: is_pos,
                    });
                } else {
                    state.captures.push(PartialCapture {
                        start: str_idx,
                        end: str_idx,
                        pos: false,
                        closed: false,
                    });
                }
                let cap_idx = state.captures.len() - 1;
                let next_pat_idx = pat_idx + 1 + is_pos as usize;
                if let Some(res) = do_match(state, next_pat_idx, str_idx)? {
                    if !check_capture_closed(state, cap_idx) {
                        return Err(MatchError::UnclosedCapture);
                    }
                    return Ok(Some(res));
                } else {
                    // Backtracking; pop the capture that this added
                    state.captures.pop();
                    return Ok(None);
                }
            }
            (b')', _) => {
                // Close the most recent unclosed capture
                let cap = last_unclosed_capture_idx(&state).ok_or(MatchError::CapMismatch)?;
                state.captures[cap].end = str_idx;
                state.captures[cap].closed = true;
                if let Some(res) = do_match(state, pat_idx + 1, str_idx)? {
                    return Ok(Some(res));
                } else {
                    // Backtracking; undo marking the capture as closed
                    state.captures[cap].closed = false;
                    return Ok(None);
                }
            }
            (b'$', None) => {
                // If at the end of the string, return match; otherwise, backtrack
                if str_idx == state.str.len() {
                    return Ok(Some(MatchEnd(str_idx)));
                } else {
                    return Ok(None);
                }
            }
            (ESCAPE, Some(b'b')) => {
                // `%b()`, or `%bAB` in the general case -- find balanced pairs with
                // the given starting and ending characters.
                if pat_idx + 3 >= state.pat.len() {
                    return Err(MatchError::Balanced);
                }
                let (start, end) = (state.pat[pat_idx + 2], state.pat[pat_idx + 3]);

                if let Some(str_idx_end) = match_balanced(state.str, start, end, str_idx) {
                    str_idx = str_idx_end;
                    pat_idx = pat_idx + 4;
                    assert!(str_idx <= state.str.len());
                    continue;
                } else {
                    return Ok(None);
                }
            }
            (ESCAPE, Some(b'f')) => {
                // Frontier pattern; match a boundary between a character
                // not in the given set and one in it.
                if !matches!(state.pat.get(pat_idx + 2), Some(b'['))
                    || pat_idx + 3 >= state.pat.len()
                {
                    return Err(MatchError::Frontier);
                }

                let slice = &state.pat[pat_idx + 3..];
                let (invert, len) = parse_set(&slice).ok_or(MatchError::Frontier)?;
                let slice = &slice[invert as usize..len - 1];

                // If beginning or end are out of bounds, treat as `\0`
                let before = if str_idx == 0 || str_idx > state.str.len() {
                    b'\0'
                } else {
                    state.str[str_idx - 1]
                };
                let after = state.str.get(str_idx).copied().unwrap_or(b'\0');

                if !match_set(before, invert, slice) && match_set(after, invert, slice) {
                    pat_idx = pat_idx + 3 + len;
                    continue;
                } else {
                    return Ok(None);
                }
            }
            (ESCAPE, Some(n @ b'0'..=b'9')) => {
                // Backreference; literally match the result of a previous capture
                let n = (*n - b'0') as usize;
                if n == 0 || n - 1 >= state.captures.len() {
                    return Err(MatchError::Backref);
                }
                let capture = &state.captures[n - 1];
                if !capture.closed {
                    return Err(MatchError::Backref);
                }
                let slice = &state.str[capture.start..capture.end];

                if state.str[str_idx..].starts_with(slice) {
                    str_idx += slice.len();
                    pat_idx += 2;
                    assert!(str_idx <= state.str.len());
                    continue;
                } else {
                    return Ok(None);
                }
            }
            _ => {
                // General case; attempt to match a character class/set/./literal
                let (class, pat_len) =
                    parse_class(&state.pat[pat_idx..]).ok_or(MatchError::Class)?;
                let pat_end = pat_idx + pat_len;

                if str_idx < state.str.len() && match_class(state.str[str_idx], class) {
                    match state.pat.get(pat_end) {
                        Some(b'?') => {
                            // Optional match
                            // Try matching the rest of the pattern if this char is included
                            if let Some(end) = do_match(state, pat_end + 1, str_idx + 1)? {
                                return Ok(Some(end));
                            } else {
                                // Match failed; try matching without this char
                                pat_idx = pat_end + 1;
                                continue;
                            }
                        }
                        Some(b'+') => {
                            // Match one or more, greedy
                            str_idx += 1;
                            return repeat_max(state, class, str_idx, pat_end + 1, 0);
                        }
                        Some(b'*') => {
                            // Match zero or more, greedy
                            return repeat_max(state, class, str_idx, pat_end + 1, 1);
                        }
                        Some(b'-') => {
                            // Match zero or more, non-greedy (minimize count)
                            return repeat_min(state, class, str_idx, pat_end + 1);
                        }
                        _ => {
                            // Normal, non-repeated match
                            str_idx += 1;
                            pat_idx = pat_end;
                            continue;
                        }
                    }
                } else {
                    // No match
                    if let Some(b'?' | b'*' | b'-') = state.pat.get(pat_end) {
                        // Suffix allows empty matches, advance pattern
                        pat_idx = pat_end + 1;
                        continue;
                    } else {
                        return Ok(None);
                    }
                }
            }
        };
    }
    assert!(str_idx <= state.str.len());
    Ok(Some(MatchEnd(str_idx)))
}

fn last_unclosed_capture_idx(state: &MatchState<'_>) -> Option<usize> {
    for i in (0..state.captures.len()).rev() {
        if !state.captures[i].closed {
            return Some(i);
        }
    }
    None
}

#[derive(Copy, Clone, Debug)]
enum Class<'a> {
    Any,
    Lit(u8),
    Class(u8),
    Set(bool, &'a [u8]),
}

/// Parses a character class from the pattern slice; returns the class
/// enum and the index of the end of this class.
fn parse_class(pat: &[u8]) -> Option<(Class<'_>, usize)> {
    if pat.len() == 0 {
        return None;
    }
    let (i, class) = match pat[0] {
        b'.' => (1, Class::Any),
        b'%' => (2, Class::Class(*pat.get(1)?)),
        b'[' => {
            let (inv, end) = parse_set(&pat[1..])?;
            // This can't panic (despite what the rust compiler sometimes thinks)
            // inv is only true if pat.get(2) is not none; thus pat.len > 2, and the start bound is valid
            // end is always i + 1 for some i < pat.len() - 1, so end <= pat.len()
            // parse_set can only return values of end > inv
            // This gives a ~30% overall time reduction on pm.lua (repeated 16x)
            (1 + end, Class::Set(inv, &pat[1 + inv as usize..end]))
        }
        c => (1, Class::Lit(c)),
    };
    Some((class, i))
}

/// Evaluates whether a character matches a character class.
fn match_class(ch: u8, class: Class<'_>) -> bool {
    match class {
        Class::Any => true,
        Class::Lit(l) => ch == l,
        Class::Class(cl) => match_char_class(cl, ch),
        Class::Set(inv, set) => match_set(ch, inv, set),
    }
}

/// Repeat matching the given class as many times as possible, then
/// match the rest of the pattern.  If that fails, repeat fewer times
/// until it works, or fail if matching zero times.
fn repeat_max(
    state: &mut MatchState,
    class: Class<'_>,
    str_idx: usize,
    next_pat_idx: usize,
    i: usize,
) -> Result<Option<MatchEnd>, MatchError> {
    let original_start = str_idx;
    let mut str_idx = str_idx + i;
    // Match as many chars as possible, and find the end
    while str_idx < state.str.len() && match_class(state.str[str_idx], class) {
        str_idx += 1;
    }
    // Try matching the pattern with each possible number of matches,
    // prioritizing longer runs of matches (greedy)
    for idx in (original_start..=str_idx).rev() {
        if let Some(m) = do_match(state, next_pat_idx, idx)? {
            return Ok(Some(m));
        }
    }
    Ok(None)
}

/// Matching the given class as few times as possible, then match the
/// rest of the pattern.  If matching the rest of the pattern fails,
/// try matching the given class another time, then repeat.
fn repeat_min(
    state: &mut MatchState,
    class: Class<'_>,
    mut str_idx: usize,
    next_pat_idx: usize,
) -> Result<Option<MatchEnd>, MatchError> {
    if let Some(m) = do_match(state, next_pat_idx, str_idx)? {
        return Ok(Some(m));
    }
    str_idx += 1;
    if str_idx > state.str.len() {
        return Ok(None);
    }
    loop {
        // if rest of pat matches, return that
        // else if class matches, advance str by one and try again
        // otherwise, return failure
        if let Some(m) = do_match(state, next_pat_idx, str_idx)? {
            return Ok(Some(m));
        }
        if str_idx < state.str.len() && match_class(state.str[str_idx], class) {
            str_idx += 1;
        } else {
            return Ok(None);
        }
    }
}
