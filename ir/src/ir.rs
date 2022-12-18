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

#[derive(Debug, PartialEq, Clone)]
pub enum IRValue<'a> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(&'a [u8]),
    Tensor(&'a bump::List<'a, usize>, &'a bump::List<'a, f64>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IRType {
    Nil,
    Boolean,
    String,
    Number,
    Tensor,
}

type IRRegister = (u32, IRType);
type IRBasicBlockID = u32;

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
    Immediate(IRRegister, IRValue<'a>),
    Copy(IRRegister, IRRegister),
    Unary(IRRegister, IRUnaryOp, IRRegister),
    Binary(IRRegister, IRBinaryOp, IRRegister, IRRegister),
    Index(IRRegister, IRRegister, &'a bump::List<'a, IRRegister>),
    BranchUncond(IRBasicBlockID),
    BranchCond(IRRegister, IRBasicBlockID, IRBasicBlockID),
    Call(IRRegister, &'a [u8], &'a bump::List<'a, IRRegister>),
    Print(IRRegister),
    IR(IRRegister),
    Return(IRRegister),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRBasicBlock<'a> {
    pub(crate) insts: &'a bump::List<'a, IRInstruction<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRFunction<'a> {
    pub(crate) name: &'a [u8],
    pub(crate) params: &'a bump::List<'a, IRRegister>,
    pub(crate) ret_type: IRType,
    pub(crate) blocks: &'a bump::List<'a, IRBasicBlock<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRModule<'a> {
    pub(crate) funcs: &'a bump::List<'a, IRFunction<'a>>,
}

pub type IRResult<T> = Result<T, &'static str>;

impl<'a> IRModule<'a> {
    pub fn verify_module(&self) -> IRResult<()> {
        for i in 0..self.funcs.len() {
            self.verify_func(i)?;
        }
        Ok(())
    }

    fn verify_func(&self, func_idx: usize) -> IRResult<()> {
        let func = self.funcs.at(func_idx);
        Ok(())
    }

    fn query_func(&self, name: &'a [u8]) -> IRResult<(&'a bump::List<'a, IRRegister>, IRType)> {
        for i in 0..self.funcs.len() {
            let func = self.funcs.at(i);
            if func.name == name {
                return Ok((func.params, func.ret_type));
            }
        }
        Err("ERROR: Function not found in IR module.")
    }
}
