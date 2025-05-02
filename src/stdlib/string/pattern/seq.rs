// This is a port of part of PUC-Rio Lua 5.4.6's lstrlib.c to Rust,
// modified to be non-recursive.
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

pub async fn str_find_async(
    seq: &mut crate::async_callback::AsyncSequence,
    pat: &crate::StashedString,
    str: &crate::StashedString,
    start: usize,
    plain: bool,
) -> Result<Option<Match>, MatchError> {
    enum Prepass {
        Found(Option<Match>),
        Main(bool, usize),
    }

    let res = seq.enter(|_, locals, _, _| {
        let str = locals.fetch(str);
        let pat = locals.fetch(pat);
        let str = str.as_bytes();
        let pat = pat.as_bytes();
        if start > str.len() {
            Prepass::Found(None)
        } else if plain || pat.is_empty() || !crate::stdlib::string::pattern::has_specials(pat) {
            // If the pattern is plain, fall back to a simple string search
            let found = crate::stdlib::string::pattern::plain_find(pat, &str[start..]);
            Prepass::Found(found.map(|idx| Match {
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
            let anchored = matches!(pat.first(), Some(b'^'));
            Prepass::Main(anchored, str.len())
        }
    });
    match res {
        Prepass::Found(res) => Ok(res),
        Prepass::Main(anchored, len) => {
            str_find_impl_async(seq, pat, str, start, anchored, len).await
        }
    }
}

pub async fn str_find_impl_async(
    seq: &mut crate::async_callback::AsyncSequence,
    pat: &crate::StashedString,
    str: &crate::StashedString,
    start: usize,
    anchored: bool,
    len: usize,
) -> Result<Option<Match>, MatchError> {
    let mut captures = Vec::new();
    let mut stack = Vec::with_capacity(4);

    for base in start..=len {
        // Try matching the pattern against the substring starting at
        // index `base`.

        // Initialize the stack at the start of the pattern and string
        stack.push(StackFrame::Run {
            pat_idx: anchored as usize,
            str_idx: base,
        });

        let result = loop {
            let result;
            (captures, result) = seq.enter(|_, locals, _, _| {
                let str = locals.fetch(str);
                let pat = locals.fetch(pat);
                let str = str.as_bytes();
                let pat = pat.as_bytes();
                let mut state = MatchState { pat, str, captures };
                let result = do_match(&mut state, stack);
                (state.captures, result)
            });
            match result? {
                MatchPoll::Poll(s) => {
                    stack = s;
                    seq.pending().await;
                }
                MatchPoll::Done(r, s) => {
                    stack = s;
                    break r;
                }
            }
        };

        if let Some(MatchEnd(end)) = result {
            if !captures.iter().all(|cs| cs.closed) {
                return Err(MatchError::UnclosedCapture);
            }

            let captures = convert_captures(captures);
            return Ok(Some(Match {
                start: base,
                end,
                captures,
            }));
        } else {
            assert!(captures.is_empty());
        }
        if anchored {
            break;
        }
    }
    Ok(None)
}

pub fn str_find_impl(
    pat: &[u8],
    str: &[u8],
    start: usize,
    anchored: bool,
) -> Result<Option<Match>, MatchError> {
    let mut state = MatchState {
        pat,
        str,
        captures: Vec::new(),
    };

    let mut stack = Vec::with_capacity(4);

    for base in start..=str.len() {
        // Try matching the pattern against the substring starting at
        // index `base`.

        // Initialize the stack at the start of the pattern and string
        stack.push(StackFrame::Run {
            pat_idx: 0,
            str_idx: base,
        });

        let result = loop {
            match do_match(&mut state, stack)? {
                MatchPoll::Poll(s) => stack = s,
                MatchPoll::Done(r, s) => {
                    stack = s;
                    break r;
                }
            }
        };

        if let Some(MatchEnd(end)) = result {
            if !state.captures.iter().all(|cs| cs.closed) {
                return Err(MatchError::UnclosedCapture);
            }

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

/// A stack frame for backtracking in the pattern matching state machine
enum StackFrame {
    /// When the submatch fails, resume matching at `pat_idx` and `str_idx`
    Run { pat_idx: usize, str_idx: usize },

    /// When the submatch fails, pop the top capture group, which is the
    /// one that was added for the `(` corresponding to this stack frame;
    /// then return up to the previous frame (fail).
    FinalizeCapture,

    /// When the submatch fails, re-open the capture group at the given
    /// index in the capture list, and then return to the previous
    /// frame (fail).
    ReopenCapture(usize),

    /// When the submatch fails, check if the associated class matches at str_idx.
    /// - If it matches, push a RepeatMin frame with str_idx = str_idx + 1,
    ///   and attempt to match the rest of the pattern with `pat_idx` and `str_idx`.
    /// - If the class does not match, do not push a new frame, and
    ///   attempt to match the rest of the pattern with `pat_idx` and `str_idx`.
    RepeatMin {
        pat_idx: usize,
        str_idx: usize,
        class: Class,
        pat_start: usize,
    },

    /// When the submatch fails:
    /// - If i == 0, return to the previous frame (fail).
    /// - Otherwise, push a RepeatMax frame with i = i - 1, and try matching
    ///   the rest of the pattern, starting at str_idx + i - 1.
    RepeatMax {
        pat_idx: usize,
        str_idx: usize,
        i: usize,
    },
}

const LOOPS_PER_YIELD: usize = 512;

enum MatchPoll {
    Poll(Vec<StackFrame>),
    Done(Option<MatchEnd>, Vec<StackFrame>),
}

/// Evaluate a single match, starting at `start`.
///
/// The `stack` argument must be empty; if pattern does not have errors,
/// it is returned to the caller for reuse, and will be empty.
#[inline]
fn do_match(state: &mut MatchState, mut stack: Vec<StackFrame>) -> Result<MatchPoll, MatchError> {
    assert!(stack.len() > 0);

    let mut poll = 0;

    // A `continue` of this loop is equivalent to a return in the
    // recursive implementation.
    // No breaks in this loop; only exit is return.
    let _: Infallible = loop {
        let Some(frame) = stack.pop() else {
            assert!(stack.is_empty());
            return Ok(MatchPoll::Done(None, stack));
        };
        let (mut pat_idx, mut str_idx) = match frame {
            StackFrame::FinalizeCapture => {
                state.captures.pop();
                continue;
            }
            StackFrame::ReopenCapture(cap) => {
                state.captures[cap].closed = false;
                continue;
            }
            StackFrame::RepeatMin {
                pat_idx,
                str_idx,
                class,
                pat_start,
            } => {
                // just failed to match pat at `str_idx - 1`; try `str_idx`
                if str_idx < state.str.len() {
                    // TODO: The bounds check on this slice access causes a perf hit.
                    // The recursive impl can avoid this bounds check by storing
                    // the slice, but `StackFrame`s cannot borrow from the pattern,
                    // as the pattern is only available during matching.

                    // TODO: this improves perf for async, but makes perf worse on plain seq
                    if unlikely(pat_start > pat_idx - 1 || pat_idx - 1 > state.pat.len()) {
                        // Unreachable
                        continue;
                    }

                    let slice = &state.pat[pat_start..pat_idx - 1];
                    if match_class(state.str[str_idx], class, slice) {
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
                    continue;
                }
            }
            StackFrame::RepeatMax {
                pat_idx,
                str_idx,
                i,
            } => {
                // just failed to match pat at `str_idx + 1`; try `str_idx + i - 1`.
                if i > 0 {
                    stack.push(StackFrame::RepeatMax {
                        pat_idx,
                        str_idx,
                        i: i - 1,
                    });
                    (pat_idx, str_idx + i - 1)
                } else {
                    continue;
                }
            }
            StackFrame::Run { pat_idx, str_idx } => (pat_idx, str_idx),
        };

        // This assertion may or may not help performance, but will always be true.
        // assert!(str_idx <= state.str.len());

        'main_loop: loop {
            let Some(&b) = state.pat.get(pat_idx) else {
                assert!(str_idx <= state.str.len());
                stack.clear();
                return Ok(MatchPoll::Done(Some(MatchEnd(str_idx)), stack));
            };

            // TODO: evaluate getting this optimization without unsafe
            // unsafe_assert_unchecked!(str_idx <= state.str.len());
            // if str_idx > state.str.len() {
            //     continue;
            // }

            // `next` being a reference is aparently load-bearing for optimizations
            // (I guess some part of this is *really* fragile...)
            let next = state.pat.get(pat_idx + 1);

            // Every branch in this match must either continue, break, or return
            let _: Infallible = match (b, next) {
                (b'(', _) => {
                    // Push capture onto the capture stack
                    // If immediately followed by ')', this is a position capture

                    // changing this deoptimizes pattern_stack... (code alignment?)
                    // no effect on the code here, but changing this to an explicit
                    // case (b'(', Some(b')')) makes pattern_stack run ~50% slower.
                    let is_pos = next == Some(&b')');
                    state.captures.push(PartialCapture {
                        start: str_idx,
                        end: str_idx,
                        pos: is_pos,
                        closed: is_pos,
                    });
                    stack.push(StackFrame::FinalizeCapture);
                    if is_pos {
                        pat_idx += 2; // Skip the closing ')'
                    } else {
                        pat_idx += 1;
                    }
                    continue 'main_loop;
                }
                (b')', _) => {
                    // Close the most recent unclosed capture
                    let cap = last_unclosed_capture_idx(&state).ok_or(MatchError::CapMismatch)?;
                    state.captures[cap].end = str_idx;
                    state.captures[cap].closed = true;
                    stack.push(StackFrame::ReopenCapture(cap));
                    pat_idx += 1;
                    continue 'main_loop;
                }
                (b'$', None) => {
                    // If at the end of the string, return match; otherwise, backtrack
                    if str_idx == state.str.len() {
                        stack.clear();
                        return Ok(MatchPoll::Done(Some(MatchEnd(str_idx)), stack));
                    } else {
                        break 'main_loop;
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
                        continue 'main_loop;
                    } else {
                        break 'main_loop;
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
                    let (invert, len) = parse_set(slice).ok_or(MatchError::Frontier)?;
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
                        continue 'main_loop;
                    } else {
                        break 'main_loop;
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
                    if capture.pos {
                        break 'main_loop;
                    }
                    let slice = &state.str[capture.start..capture.end];

                    if state.str[str_idx..].starts_with(slice) {
                        str_idx += slice.len();
                        pat_idx += 2;
                        assert!(str_idx <= state.str.len());
                        continue 'main_loop;
                    } else {
                        break 'main_loop;
                    }
                }
                _ => {
                    // General case; attempt to match a character class/set/./literal
                    let (class, pat_len, pat_slice) =
                        parse_class(&state.pat[pat_idx..]).ok_or(MatchError::Class)?;
                    let pat_end = pat_idx + pat_len;

                    if str_idx < state.str.len()
                        && match_class(state.str[str_idx], class, pat_slice)
                    {
                        match state.pat.get(pat_end) {
                            Some(b'?') => {
                                // Optional match
                                // if matching with this fails, try matching without it afterwards
                                stack.push(StackFrame::Run {
                                    pat_idx: pat_end + 1,
                                    str_idx,
                                });
                                pat_idx = pat_end + 1;
                                str_idx = str_idx + 1;
                                continue 'main_loop;
                            }
                            Some(b'+') => {
                                // Match one or more, greedy
                                str_idx += 1;
                                let original_start = str_idx;

                                // keep matching until it fails, count matches
                                while str_idx < state.str.len()
                                    && match_class(state.str[str_idx], class, pat_slice)
                                {
                                    str_idx += 1;
                                }

                                stack.push(StackFrame::RepeatMax {
                                    pat_idx: pat_end + 1,
                                    str_idx: original_start,
                                    i: str_idx - original_start,
                                });
                                pat_idx = pat_end + 1;
                                continue 'main_loop;
                            }
                            Some(b'*') => {
                                // Match zero or more, greedy
                                let original_start = str_idx;
                                str_idx += 1;

                                // keep matching until it fails, count matches
                                while str_idx < state.str.len()
                                    && match_class(state.str[str_idx], class, pat_slice)
                                {
                                    str_idx += 1;
                                }

                                stack.push(StackFrame::RepeatMax {
                                    pat_idx: pat_end + 1,
                                    str_idx: original_start,
                                    i: str_idx - original_start,
                                });
                                pat_idx = pat_end + 1;
                                continue 'main_loop;
                            }
                            Some(b'-') => {
                                // Match zero or more, non-greedy (minimize count)
                                let offset = subslice_offset(state.pat, pat_slice);
                                stack.push(StackFrame::RepeatMin {
                                    pat_idx: pat_end + 1,
                                    str_idx: str_idx + 1,
                                    class,
                                    pat_start: offset,
                                });
                                pat_idx = pat_end + 1;
                                continue 'main_loop;
                            }
                            _ => {
                                // Normal, non-repeated match
                                str_idx += 1;
                                pat_idx = pat_end;
                                continue 'main_loop;
                            }
                        }
                    } else {
                        // No match
                        if let Some(b'?' | b'*' | b'-') = state.pat.get(pat_end) {
                            // Suffix allows empty matches, advance pattern
                            pat_idx = pat_end + 1;
                            continue 'main_loop;
                        } else {
                            break 'main_loop;
                        }
                    }
                }
            };
        }

        poll += 1;
        // TODO: tons of branch prediction misses here
        if unlikely(poll > LOOPS_PER_YIELD && !stack.is_empty()) {
            return Ok(MatchPoll::Poll(stack));
        }
    };
}

#[inline]
#[cold]
fn cold() {}

#[inline]
fn unlikely(b: bool) -> bool {
    if b {
        cold()
    }
    b
}

/// Returns the offset of the second slice into the first.
///
/// If the second argument is not a subslice of the first,
/// the value of the returned offset is unspecified.
fn subslice_offset(slice: &[u8], subslice: &[u8]) -> usize {
    (*subslice).as_ptr() as usize - (*slice).as_ptr() as usize
}

/// Returns the index of the last unclosed capture group
fn last_unclosed_capture_idx(state: &MatchState<'_>) -> Option<usize> {
    for i in (0..state.captures.len()).rev() {
        if !state.captures[i].closed {
            return Some(i);
        }
    }
    None
}

#[derive(Copy, Clone, Debug)]
enum Class {
    Any,
    Lit(u8),
    Class(u8),
    Set(bool),
}

/// Parses a character class from the pattern slice; returns the class
/// enum, the index of the end of this class, and (for sets) a slice
/// containing the inner contents of the set.
fn parse_class(pat: &[u8]) -> Option<(Class, usize, &[u8])> {
    if pat.len() == 0 {
        return None;
    }
    let (i, start, class) = match pat[0] {
        b'.' => (1, &pat[..1], Class::Any),
        b'%' => (2, &pat[..1], Class::Class(*pat.get(1)?)),
        b'[' => {
            let (inv, end) = parse_set(&pat[1..])?;
            // This can't panic (despite what the rust compiler sometimes thinks)
            // inv is only true if pat.get(2) is not none; thus pat.len > 2, and the start bound is valid
            // end is always i + 1 for some i < pat.len() - 1, so end <= pat.len()
            // parse_set can only return values of end > inv
            // This gives a ~30% overall time reduction on pm.lua (repeated 16x)
            (1 + end, &pat[1 + inv as usize..end], Class::Set(inv))
        }
        c => (1, &pat[..1], Class::Lit(c)),
    };
    Some((class, i, start))
}

/// Evaluates whether a character matches a character class; the slice
/// should be the slice returned by the parse_class function.
///
/// Specifically, the slice is only used in the `Set` case, and must be
/// a slice containing the contents of the set, between the open bracket
/// (excluding the `^`), and the closing bracket.
#[inline]
fn match_class(ch: u8, class: Class, set_slice: &[u8]) -> bool {
    match class {
        Class::Any => true,
        Class::Lit(l) => ch == l,
        Class::Class(cl) => match_char_class(cl, ch),
        Class::Set(inv) => match_set(ch, inv, set_slice),
    }
}
