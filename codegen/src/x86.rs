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

use core::fmt;
use core::str;

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

impl fmt::Display for X86Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86Operand::Register(reg) => write!(f, "{}", reg),
            X86Operand::Memory(reg, size) => write!(f, "[{} + {}]", reg, size),
            X86Operand::Immediate(con) => write!(f, "{}", con),
            X86Operand::DataSegmentVariable(_) => todo!(),
        }?;
        Ok(())
    }
}

impl<'a> fmt::Display for X86Instruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86Instruction::Inc(op) => write!(f, "inc {}", op),
            X86Instruction::Dec(op) => write!(f, "dec {}", op),
            X86Instruction::Neg(op) => write!(f, "neg {}", op),
            X86Instruction::Not(op) => write!(f, "not {}", op),
            X86Instruction::Leaq(op1, op2) => write!(f, "leaq {}, {}", op1, op2),
            X86Instruction::Add(op1, op2) => write!(f, "add {}, {}", op1, op2),
            X86Instruction::Sub(op1, op2) => write!(f, "sub {}, {}", op1, op2),
            X86Instruction::Imul(op1, op2) => write!(f, "imul {}, {}", op1, op2),
            X86Instruction::Cmp(op1, op2) => write!(f, "cmp {}, {}", op1, op2),
            X86Instruction::Test(op1, op2) => write!(f, "test {}, {}", op1, op2),
            X86Instruction::Jmp(label) => write!(
                f,
                "jmp {}",
                str::from_utf8(label).expect("PANIC: Label name not convertable to Rust str.")
            ),
            X86Instruction::Call(label) => write!(
                f,
                "call {}",
                str::from_utf8(label).expect("PANIC: Label name not convertable to Rust str.")
            ),
            X86Instruction::Ret => write!(f, "ret"),
        }?;
        Ok(())
    }
}

impl<'a> fmt::Display for X86Block<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}\n",
            str::from_utf8(self.label).expect("PANIC: Label name not convertable to Rust str.")
        )?;
        for i in 0..self.insts.len() {
            write!(f, "    {}\n", self.insts.at(i))?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for X86Module<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".text")?;
        for i in 0..self.blocks.len() {
            write!(f, "{}\n", self.blocks.at(i))?;
        }
        write!(f, ".ident \"weakc: 0.0.1\"\n")?;
        Ok(())
    }
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
