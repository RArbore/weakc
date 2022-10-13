/*
 * This file is part of weakc.
 * weakc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * weakc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License
 * along with weakc. If not, see <https://www.gnu.org/licenses/>.
 */

extern crate bump;

pub fn pred_whitespace(c: u8) -> bool {
    c == b' ' || c == b'\n' || c == b'\r' || c == b'\t'
}

pub fn pred_number(c: u8) -> bool {
    c >= 48 && c <= 57
}

pub fn pred_alpha(c: u8) -> bool {
    (c >= 65 && c <= 90) || (c >= 97 && c <= 122)
}

pub fn parse_char<'a, T: Copy + PartialEq>(
    chunk: &'a [T],
    pred: &dyn Fn(T) -> bool,
) -> Option<(T, &'a [T])> {
    let c = *chunk.get(0)?;
    if pred(c) {
        Some((c, &chunk[1..]))
    } else {
        None
    }
}

pub fn parse_seq_char<'a, T: Copy + PartialEq>(
    chunk: &'a [T],
    pred: &dyn Fn(T) -> bool,
) -> (&'a [T], &'a [T]) {
    let mut i = 0;
    while i < chunk.len() {
        let c = chunk[i];
        if !pred(c) {
            return (&chunk[0..i], &chunk[i..]);
        }
        i += 1;
    }
    (chunk, &[])
}

pub fn parse_string<'a, T: Copy + PartialEq>(
    chunk: &'a [T],
    string: &[T],
) -> Option<(&'a [T], &'a [T])> {
    if chunk.len() < string.len() {
        return None;
    }
    for i in 0..string.len() {
        if chunk[i] != string[i] {
            return None;
        }
    }
    Some((&chunk[0..string.len()], &chunk[string.len()..]))
}

pub fn parse_token<'a, 'b, T: Copy + PartialEq, R>(
    chunk: &'a [T],
    pred: &dyn Fn(T) -> Option<R>,
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b R, &'a [T])> {
    let c = *chunk.get(0)?;
    if let Some(result) = pred(c) {
        Some((bump.alloc(result), &chunk[1..]))
    } else {
        None
    }
}

pub fn parse_token_consume<'a, 'b, T: Copy + PartialEq>(
    chunk: &'a [T],
    pred: T,
) -> Option<&'a [T]> {
    let c = *chunk.get(0)?;
    if pred == c {
        Some(&chunk[1..])
    } else {
        None
    }
}

pub fn parse_seq<'a, 'b, T: Copy + PartialEq, R>(
    chunk: &'a [T],
    parse: &dyn Fn(&'a [T], &'b bump::BumpAllocator) -> Option<(&'b R, &'a [T])>,
    bump: &'b bump::BumpAllocator,
) -> (Vec<&'b R>, &'a [T]) {
    let mut xs = vec![];
    let mut current = chunk;

    loop {
        let result = parse(current, bump);
        if let Some((parsed, rest)) = result {
            xs.push(parsed);
            current = rest;
        } else {
            break;
        }
    }

    (xs, current)
}

pub fn parse_or<'a, 'b, T: Copy + PartialEq, R>(
    chunk: &'a [T],
    parse: &[&dyn Fn(&'a [T], &'b bump::BumpAllocator) -> Option<(&'b R, &'a [T])>],
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b R, &'a [T])> {
    for parse in parse {
        if let Some(success) = parse(chunk, bump) {
            return Some(success);
        }
    }
    None
}

pub fn parse_maybe<'a, 'b, T: Copy + PartialEq, R>(
    chunk: &'a [T],
    parse: &dyn Fn(&'a [T], &'b bump::BumpAllocator) -> Option<(&'b R, &'a [T])>,
    bump: &'b bump::BumpAllocator,
) -> (Option<&'b R>, &'a [T]) {
    if let Some((parsed, rest)) = parse(chunk, bump) {
        (Some(parsed), rest)
    } else {
        (None, chunk)
    }
}

pub fn parse_any_of<'a, T: Copy + PartialEq, R: Copy>(
    chunk: &'a [T],
    assoc_list: &[(&[T], R)],
) -> Option<(R, &'a [T])> {
    for (string, token) in assoc_list {
        if let Some((_, rest)) = parse_string(chunk, string) {
            return Some((*token, rest));
        }
    }
    None
}
