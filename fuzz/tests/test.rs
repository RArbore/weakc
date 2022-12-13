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
extern crate parse;
extern crate semant;

use rand::Rng;

use bump::BumpAllocator;

#[test]
fn fuzz_bump() {
    const NUM: usize = 1000000;
    let mut rng = rand::thread_rng();

    let bp = BumpAllocator::new();

    let mut correct = vec![0.0; NUM];
    rng.fill(correct.as_mut_slice());
    let list = bp.create_list();
    for i in 0..NUM {
        list.push(correct[i]);
    }

    for i in 0..NUM {
        assert_eq!(*list.at(i), correct[i]);
    }

    let mut vec0 = vec![];
    let mut vec1 = vec![];
    let mut vec2 = vec![];
    let mut vec3 = vec![];
    let mut veci = vec![];

    let mut vecg: Vec<u64> = vec![0; NUM];
    rng.fill(vecg.as_mut_slice());
    for i in 0..NUM {
        let ty = vecg[i] % 4;
        match ty {
            0 => {
                vec0.push(bp.alloc::<u8>(i as _));
            }
            1 => {
                vec1.push(bp.alloc::<i16>(i as _));
            }
            2 => {
                vec2.push(bp.alloc::<f32>(i as _));
            }
            _ => {
                vec3.push(bp.alloc::<usize>(i as _));
            }
        }
        veci.push(ty);
    }

    let mut i0 = 0;
    let mut i1 = 0;
    let mut i2 = 0;
    let mut i3 = 0;

    for i in 0..NUM {
        let ty = veci[i];
        match ty {
            0 => {
                assert_eq!(*vec0[i0], i as _);
                i0 += 1;
            }
            1 => {
                assert_eq!(*vec1[i1], i as _);
                i1 += 1;
            }
            2 => {
                assert_eq!(*vec2[i2], i as _);
                i2 += 1;
            }
            _ => {
                assert_eq!(*vec3[i3], i as _);
                i3 += 1;
            }
        }
    }
}

#[test]
fn fuzz_parse() {
    const NUM: usize = 20;

    let mut rng = rand::thread_rng();
    const TOKENS: &[&[u8]] = &[
        b"0.1273",
        b"123879.0123809",
        b"\"ajskldjasdkl\"",
        b"\"!!!!!!!\"",
        b"asdkjlkas",
        b"xyz",
        b"x",
        b"flkasfdjkasdfl31893kljadsklj",
        b"T",
        b"F",
        b"p",
        b"a",
        b"s",
        b"sa",
        b"i",
        b"w",
        b"f",
        b"o",
        b"r",
        b"n",
        b"v",
        b"+",
        b"-",
        b"*",
        b"/",
        b"^",
        b"@",
        b"!",
        b"=",
        b">",
        b"<",
        b"!=",
        b"==",
        b">=",
        b"<=",
        b"A",
        b"O",
        b"(",
        b")",
        b"{",
        b"}",
        b"[",
        b"]",
        b",",
        b";",
    ];

    for _ in 0..NUM {
        let mut text = vec![];
        let num_tokens = rng.gen::<usize>() % 100000;
        for _ in 0..num_tokens {
            let token = TOKENS[rng.gen::<usize>() % TOKENS.len()];
            for c in token {
                text.push(*c);
            }
            text.push(b' ');
        }

        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(text.as_slice(), &bump).unwrap();
        let _ = parse::parse_stmt(&tokens, &bump);
    }
}
