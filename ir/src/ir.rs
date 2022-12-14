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

type IRRegister = (u32, IRType);
type IRBasicBlockRef = u32;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IRUnaryOp {
    Not,
    Negate,
    Shape,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IRBinaryOp {
    ShapedAs,
    AddNumbers,
    SubtractNumbers,
    MultiplyNumbers,
    DivideNumbers,
    PowerNumbers,
    AddTensors,
    SubtractTensors,
    MultiplyTensors,
    DivideTensors,
    PowerTensors,
    MatrixMultiply,
    Greater,
    Lesser,
    NotEqualsNils,
    EqualsEqualsNils,
    NotEqualsBooleans,
    EqualsEqualsBooleans,
    NotEqualsStrings,
    EqualsEqualsStrings,
    NotEqualsNumbers,
    EqualsEqualsNumbers,
    NotEqualsTensors,
    EqualsEqualsTensors,
    GreaterEquals,
    LesserEquals,
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IRInstruction<'a> {
    Immediate(IRRegister, semant::Value<'a>),
    Copy(IRRegister, IRRegister),
    Unary(IRRegister, IRUnaryOp, IRRegister),
    Binary(IRRegister, IRBinaryOp, IRRegister, IRRegister),
    Index(IRRegister, IRRegister, &'a bump::List<'a, IRRegister>),
    BranchUncond(IRBasicBlockRef),
    BranchCond(IRRegister, IRBasicBlockRef, IRBasicBlockRef),
    Call(IRRegister, &'a [u8], &'a bump::List<'a, IRRegister>),
    Print(IRRegister),
    Verify(IRRegister),
    Return(IRRegister),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRBasicBlock<'a> {
    insts: &'a bump::List<'a, IRInstruction<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRFunction<'a> {
    name: &'a [u8],
    params: &'a bump::List<'a, IRRegister>,
    ret_type: IRType,
    blocks: &'a bump::List<'a, IRBasicBlock<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRModule<'a> {
    globals: &'a bump::List<'a, IRRegister>,
    funcs: &'a bump::List<'a, IRFunction<'a>>,
}
