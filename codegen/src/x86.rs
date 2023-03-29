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
pub enum X86Operand<'a> {
    Register(X86Register),
    MemoryOffsetConstant(X86Register, usize),
    MemoryOffsetLinear(usize, X86Register, X86Register),
    MemoryLabel(X86Register, &'a [u8]),
    Immediate(u64),
    DataSegmentVariable(u32),
}

#[derive(Debug, PartialEq)]
pub enum X86Instruction<'a> {
    Inc(X86Operand<'a>),
    Dec(X86Operand<'a>),
    Neg(X86Operand<'a>),
    Not(X86Operand<'a>),
    Lea(X86Operand<'a>, X86Operand<'a>),
    Add(X86Operand<'a>, X86Operand<'a>),
    Addsd(X86Operand<'a>, X86Operand<'a>),
    Sub(X86Operand<'a>, X86Operand<'a>),
    Subsd(X86Operand<'a>, X86Operand<'a>),
    Imul(X86Operand<'a>, X86Operand<'a>),
    Mulsd(X86Operand<'a>, X86Operand<'a>),
    Divsd(X86Operand<'a>, X86Operand<'a>),
    Xor(X86Operand<'a>, X86Operand<'a>),
    Xorps(X86Operand<'a>, X86Operand<'a>),
    Or(X86Operand<'a>, X86Operand<'a>),
    And(X86Operand<'a>, X86Operand<'a>),
    Mov(X86Operand<'a>, X86Operand<'a>),
    Movsd(X86Operand<'a>, X86Operand<'a>),
    Movsxd(X86Operand<'a>, X86Operand<'a>),
    Push(X86Operand<'a>),
    Pop(X86Operand<'a>),
    Cvttsd2si(X86Operand<'a>, X86Operand<'a>),
    Cmp(X86Operand<'a>, X86Operand<'a>),
    Comisd(X86Operand<'a>, X86Operand<'a>),
    Test(X86Operand<'a>, X86Operand<'a>),
    Seta(X86Operand<'a>),
    Setae(X86Operand<'a>),
    Sete(X86Operand<'a>),
    Setne(X86Operand<'a>),
    Jmp(&'a [u8]),
    Jnz(&'a [u8]),
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
    pub floats: &'a mut bump::List<'a, f64>,
}

impl<'a> fmt::Display for X86Operand<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86Operand::Register(reg) => write!(f, "{}", reg),
            X86Operand::MemoryOffsetConstant(reg, size) => write!(f, "[{} + {}]", reg, size),
            X86Operand::MemoryOffsetLinear(coeff, linear_reg, constant_reg) => {
                write!(f, "[{} * {} + {}]", coeff, linear_reg, constant_reg)
            }
            X86Operand::MemoryLabel(reg, label) => write!(
                f,
                "[{} + {}]",
                reg,
                str::from_utf8(label).expect("PANIC: Label name not convertable to Rust str.")
            ),
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
            X86Instruction::Lea(op1, op2) => write!(f, "lea {}, {}", op1, op2),
            X86Instruction::Add(op1, op2) => write!(f, "add {}, {}", op1, op2),
            X86Instruction::Addsd(op1, op2) => write!(f, "addsd {}, {}", op1, op2),
            X86Instruction::Sub(op1, op2) => write!(f, "sub {}, {}", op1, op2),
            X86Instruction::Subsd(op1, op2) => write!(f, "subsd {}, {}", op1, op2),
            X86Instruction::Imul(op1, op2) => write!(f, "imul {}, {}", op1, op2),
            X86Instruction::Mulsd(op1, op2) => write!(f, "mulsd {}, {}", op1, op2),
            X86Instruction::Divsd(op1, op2) => write!(f, "divsd {}, {}", op1, op2),
            X86Instruction::Xor(op1, op2) => write!(f, "xor {}, {}", op1, op2),
            X86Instruction::Xorps(op1, op2) => write!(f, "xorps {}, {}", op1, op2),
            X86Instruction::Or(op1, op2) => write!(f, "or {}, {}", op1, op2),
            X86Instruction::And(op1, op2) => write!(f, "and {}, {}", op1, op2),
            X86Instruction::Mov(op1, op2) => write!(f, "mov {}, {}", op1, op2),
            X86Instruction::Movsd(op1, op2) => write!(f, "movsd {}, {}", op1, op2),
            X86Instruction::Movsxd(op1, op2) => write!(f, "movsxd {}, {}", op1, op2),
            X86Instruction::Push(op) => write!(f, "push {}", op),
            X86Instruction::Pop(op) => write!(f, "pop {}", op),
            X86Instruction::Cvttsd2si(op1, op2) => write!(f, "cvttsd2si {}, {}", op1, op2),
            X86Instruction::Cmp(op1, op2) => write!(f, "cmp {}, {}", op1, op2),
            X86Instruction::Comisd(op1, op2) => write!(f, "comisd {}, {}", op1, op2),
            X86Instruction::Test(op1, op2) => write!(f, "test {}, {}", op1, op2),
            X86Instruction::Seta(op) => write!(f, "seta {}", op),
            X86Instruction::Setae(op) => write!(f, "seta {}", op),
            X86Instruction::Sete(op) => write!(f, "sete {}", op),
            X86Instruction::Setne(op) => write!(f, "setne {}", op),
            X86Instruction::Jmp(label) => write!(
                f,
                "jmp {}",
                str::from_utf8(label).expect("PANIC: Label name not convertable to Rust str.")
            ),
            X86Instruction::Jnz(label) => write!(
                f,
                "jnz {}",
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
            "{}:\n",
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
        write!(f, ".intel_syntax\n.text\n.globl main\n\n")?;
        for i in 0..self.blocks.len() {
            write!(f, "{}\n", self.blocks.at(i))?;
        }
        write!(f, ".rodata\n\n")?;
        for i in 0..self.floats.len() {
            let bits = self.floats.at(i).to_bits();
            write!(f, ".weak.float.{:#016x}:\n    .quad {:#016x}\n", bits, bits)?;
        }
        for i in 0..self.strings.len() {
            write!(
                f,
                ".weak.string.{:#010}:\n    .asciz \"{}\"\n",
                i,
                str::from_utf8(self.strings.at(i))
                    .expect("PANIC: Weak string not convertable to Rust str.")
            )?;
        }
        write!(f, "\n.ident \"weakc: 0.0.1\"\n")?;
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

pub fn write_bits_to_hex(id: u64, buf: &mut [u8], offset: usize) {
    let conv = |x| {
        if x < 10 {
            x + b'0'
        } else {
            x - 10 + b'a'
        }
    };

    buf[offset] = b'0';
    buf[1 + offset] = b'x';
    buf[2 + offset] = conv((id >> 60) as u8);
    buf[3 + offset] = conv((id >> 56 & 15) as u8);
    buf[4 + offset] = conv((id >> 52 & 15) as u8);
    buf[5 + offset] = conv((id >> 48 & 15) as u8);
    buf[6 + offset] = conv((id >> 44 & 15) as u8);
    buf[7 + offset] = conv((id >> 40 & 15) as u8);
    buf[8 + offset] = conv((id >> 36 & 15) as u8);
    buf[9 + offset] = conv((id >> 32 & 15) as u8);
    buf[10 + offset] = conv((id >> 28 & 15) as u8);
    buf[11 + offset] = conv((id >> 24 & 15) as u8);
    buf[12 + offset] = conv((id >> 20 & 15) as u8);
    buf[13 + offset] = conv((id >> 16 & 15) as u8);
    buf[14 + offset] = conv((id >> 12 & 15) as u8);
    buf[15 + offset] = conv((id >> 8 & 15) as u8);
    buf[16 + offset] = conv((id >> 4 & 15) as u8);
    buf[17 + offset] = conv((id & 15) as u8);
}

pub fn write_decimal(dec: usize, buf: &mut [u8], offset: usize) {
    let conv = |x| x as u8 + b'0';

    let mut pow10 = 1000000000;
    for i in 0..10 {
        buf[i + offset] = conv((dec / pow10) % 10);
        pow10 /= 10;
    }
}
