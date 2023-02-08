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

#[derive(Debug, PartialEq)]
pub enum X86Operand {
    Register(X86Register),
    Memory(X86Register, usize),
    Immediate(u64),
}

#[derive(Debug, PartialEq)]
pub enum X86Instruction {
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
}

#[derive(Debug, PartialEq)]
pub struct X86Block<'a> {
    pub label: &'a [u8],
    pub insts: &'a mut bump::List<'a, X86Instruction>,
}

#[derive(Debug, PartialEq)]
pub struct X86Module<'a> {
    pub funcs: &'a mut bump::List<'a, X86Block<'a>>,
    pub strings: &'a mut bump::List<'a, &'a [u8]>,
}
