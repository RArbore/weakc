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

pub fn parse_char<'a>(chunk: &'a [u8], pred: &dyn Fn(u8) -> bool) -> Option<(u8, &'a [u8])> {
    let c = *chunk.get(0)?;
    if pred(c) {
        Some((c, &chunk[1..]))
    } else {
        None
    }
}

pub fn parse_seq_char<'a>(chunk: &'a [u8], pred: &dyn Fn(u8) -> bool) -> (&'a [u8], &'a [u8]) {
    let i = 0;
    while i < chunk.len() {
        let c = chunk[i];
        if !pred(c) {
            return (&chunk[0..i], &chunk[i..]);
        }
    }
    (chunk, &[])
}

pub fn parse_string<'a>(chunk: &'a [u8], string: &[u8]) -> Option<(&'a [u8], &'a [u8])> {
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

pub fn parse_seq<'a, 'b, T>(
    chunk: &'a [u8],
    parse: &dyn Fn(&'a [u8], &'b bump::BumpAllocator) -> Option<(&'b T, &'a [u8])>,
    bump: &'b bump::BumpAllocator,
) -> (Vec<&'b T>, &'a [u8]) {
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

pub fn parse_or<'a, 'b, T>(
    chunk: &'a [u8],
    parse1: &dyn Fn(&'a [u8], &'b bump::BumpAllocator) -> Option<(&'b T, &'a [u8])>,
    parse2: &dyn Fn(&'a [u8], &'b bump::BumpAllocator) -> Option<(&'b T, &'a [u8])>,
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b T, &'a [u8])> {
    if let Some(success) = parse1(chunk, bump) {
        Some(success)
    } else if let Some(success) = parse2(chunk, bump) {
        Some(success)
    } else {
        None
    }
}

pub fn parse_and<'a, 'b, T1, T2>(
    chunk: &'a [u8],
    parse1: &dyn Fn(&'a [u8], &'b bump::BumpAllocator) -> Option<(&'b T1, &'a [u8])>,
    parse2: &dyn Fn(&'a [u8], &'b bump::BumpAllocator) -> Option<(&'b T2, &'a [u8])>,
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b T1, &'b T2, &'a [u8])> {
    let (first, rest) = parse1(chunk, bump)?;
    let (second, rest) = parse2(rest, bump)?;
    Some((first, second, rest))
}

pub fn parse_any_of<'a, T: Copy>(
    chunk: &'a [u8],
    assoc_list: &[(&[u8], T)],
) -> Option<(T, &'a [u8])> {
    for (string, token) in assoc_list {
        if let Some((_, rest)) = parse_string(chunk, string) {
            return Some((*token, rest));
        }
    }
    None
}
