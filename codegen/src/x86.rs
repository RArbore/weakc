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

use crate::*;

pub type X86BlockID = u32;

#[derive(Debug, PartialEq)]
pub enum X86Operand {
    Register(X86Register),
    Memory(X86Register, usize),
    Immediate(u64),
    DataSegmentVariable(u32),
}

#[derive(Debug, PartialEq)]
pub enum X86Instruction<'a> {
    Inc(X86Operand),
    Dec(X86Operand),
    Neg(X86Operand),
    Not(X86Operand),
    Leaq(X86Operand, X86Operand),
    Add(X86Operand, X86Operand),
    Sub(X86Operand, X86Operand),
    Imul(X86Operand, X86Operand),
    Cmp(X86Operand, X86Operand),
    Test(X86Operand, X86Operand),
    Jmp(&'a [u8]),
    Call(&'a [u8]),
    Ret,
}

#[derive(Debug, PartialEq)]
pub struct X86Block<'a> {
    pub label: &'a [u8],
    pub insts: &'a mut bump::List<'a, X86Instruction<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct X86Module<'a> {
    pub blocks: &'a mut bump::List<'a, X86Block<'a>>,
    pub strings: &'a mut bump::List<'a, &'a [u8]>,
}

pub fn write_block_id(id: X86BlockID, buf: &mut [u8], offset: usize) {
    let conv = |x| {
        if x < 10 {
            x + b'0'
        } else {
            x - 10 + b'A'
        }
    };

    buf[offset] = b'0';
    buf[1 + offset] = b'x';
    buf[2 + offset] = conv((id >> 28) as u8);
    buf[3 + offset] = conv((id >> 24 & 15) as u8);
    buf[4 + offset] = conv((id >> 20 & 15) as u8);
    buf[5 + offset] = conv((id >> 16 & 15) as u8);
    buf[6 + offset] = conv((id >> 12 & 15) as u8);
    buf[7 + offset] = conv((id >> 8 & 15) as u8);
    buf[8 + offset] = conv((id >> 4 & 15) as u8);
    buf[9 + offset] = conv((id & 15) as u8);
}
