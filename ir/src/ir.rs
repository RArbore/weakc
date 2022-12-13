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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IRType {
    Nil,
    Boolean,
    String,
    Number,
    Tensor,
}

type Register = (u32, IRType);
type BasicBlockRef = u32;

#[derive(Debug, PartialEq, Clone)]
pub enum IRInstruction<'a> {
    Immediate(Register, semant::Value<'a>),
    Copy(Register, Register),
    Unary(Register, parse::ASTUnaryOp, Register),
    Binary(Register, parse::ASTBinaryOp, Register, Register),
    Index(Register, Register, &'a bump::List<'a, Register>),
    BranchUncond(BasicBlockRef),
    BranchCond(Register, BasicBlockRef, BasicBlockRef),
    Call(Register, &'a [u8], &'a bump::List<'a, Register>),
    Print(Register),
    Verify(Register),
    Return(Register),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRBasicBlock<'a> {
    insts: &'a bump::List<'a, IRInstruction<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRFunction<'a> {
    name: &'a [u8],
    params: &'a bump::List<'a, Register>,
    ret_type: IRType,
    blocks: &'a bump::List<'a, IRBasicBlock<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRModule<'a> {
    globals: &'a bump::List<'a, Register>,
    funcs: &'a bump::List<'a, IRFunction<'a>>,
}
