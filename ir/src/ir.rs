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

use core::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum IRConstant<'a> {
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

pub type IRRegisterID = u32;
pub type IRFunctionID = u32;
pub type IRBasicBlockID = u32;
pub type IRRegister = (IRRegisterID, IRType);

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
    Immediate(IRRegister, IRConstant<'a>),
    Copy(IRRegister, IRRegister),
    Unary(IRRegister, IRUnaryOp, IRRegister),
    Binary(IRRegister, IRBinaryOp, IRRegister, IRRegister),
    Index(IRRegister, IRRegister, &'a bump::List<'a, IRRegister>),
    Array(IRRegister, &'a bump::List<'a, IRRegister>),
    BranchUncond(IRBasicBlockID),
    BranchCond(IRRegister, IRBasicBlockID, IRBasicBlockID),
    Call(
        IRRegister,
        (IRFunctionID, &'a [u8]),
        &'a bump::List<'a, IRRegister>,
    ),
    Print(IRRegister),
    Line(IRRegister),
    Verify(IRRegister),
    Return(IRRegister),
}

#[derive(Debug, PartialEq)]
pub struct IRBasicBlock<'a> {
    pub insts: &'a mut bump::List<'a, IRInstruction<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct IRFunction<'a> {
    pub name: &'a [u8],
    pub params: &'a mut bump::List<'a, IRRegister>,
    pub ret_type: IRType,
    pub blocks: &'a mut bump::List<'a, IRBasicBlock<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct IRModule<'a> {
    pub funcs: &'a mut bump::List<'a, IRFunction<'a>>,
}

impl<'a> fmt::Display for IRConstant<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRConstant::Nil => {
                write!(f, "Nil")?;
            }
            IRConstant::Boolean(v) => {
                write!(f, "{}", v)?;
            }
            IRConstant::Number(v) => {
                write!(f, "{}", v)?;
            }
            IRConstant::String(v) => {
                write!(f, "{:?}", v)?;
            }
            IRConstant::Tensor(s, v) => {
                write!(f, "{:?} sa {:?}", v, s)?;
            }
        }
        Ok(())
    }
}

impl<'a> fmt::Display for IRInstruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRInstruction::Immediate(reg, cons) => {
                write!(f, "im %{}, {}", reg.0, cons)?;
            }
            IRInstruction::Copy(reg1, reg2) => {
                write!(f, "cp %{}, %{}", reg1.0, reg2.0)?;
            }
            IRInstruction::Unary(reg1, op, reg2) => {
                write!(f, "un %{}, {:?}, %{}", reg1.0, op, reg2.0)?;
            }
            IRInstruction::Binary(reg1, op, reg2, reg3) => {
                write!(f, "bi %{}, {:?}, %{}, %{}", reg1.0, op, reg2.0, reg3.0)?;
            }
            IRInstruction::Index(reg1, reg2, indices) => {
                match (reg1.1, reg2.1) {
                    (IRType::Number, IRType::Tensor) => {
                        write!(f, "in %{} <= %{}, [", reg1.0, reg2.0)?;
                    }
                    (IRType::Tensor, IRType::Number) => {
                        write!(f, "in %{} >= %{}, [", reg1.0, reg2.0)?;
                    }
                    _ => panic!("PANIC: Can't print invalid IR."),
                };
                for i in 0..indices.len() {
                    if i == 0 {
                        write!(f, "%{}", indices.at(i).0)?;
                    } else {
                        write!(f, ", %{}", indices.at(i).0)?;
                    }
                }
                write!(f, "]")?;
            }
            IRInstruction::Array(reg, elems) => {
                write!(f, "ar %{}, [", reg.0)?;
                for i in 0..elems.len() {
                    if i == 0 {
                        write!(f, "%{}", elems.at(i).0)?;
                    } else {
                        write!(f, ", %{}", elems.at(i).0)?;
                    }
                }
                write!(f, "]")?;
            }
            IRInstruction::BranchUncond(block) => {
                write!(f, "ju {}", block)?;
            }
            IRInstruction::BranchCond(reg, block1, block2) => {
                write!(f, "br %{}, {}, {}", reg.0, block1, block2)?;
            }
            IRInstruction::Call(reg, func, args) => {
                write!(
                    f,
                    "ca %{}, {}, (",
                    reg.0,
                    std::str::from_utf8(func.1)
                        .expect("PANIC: Function name not convertable to Rust str.")
                )?;
                for i in 0..args.len() {
                    if i == 0 {
                        write!(f, "%{}", args.at(i).0)?;
                    } else {
                        write!(f, ", %{}", args.at(i).0)?;
                    }
                }
                write!(f, ")")?;
            }
            IRInstruction::Print(reg) => {
                write!(f, "pr %{}", reg.0)?;
            }
            IRInstruction::Line(reg) => {
                write!(f, "li %{}", reg.0)?;
            }
            IRInstruction::Verify(reg) => {
                write!(f, "ve %{}", reg.0)?;
            }
            IRInstruction::Return(reg) => {
                write!(f, "re %{}", reg.0)?;
            }
        }
        Ok(())
    }
}

impl<'a> fmt::Display for IRBasicBlock<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in 0..self.insts.len() {
            write!(f, "    {}\n", self.insts.at(i))?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for IRFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn {}(",
            std::str::from_utf8(self.name)
                .expect("PANIC: Function name not convertable to Rust str.")
        )?;
        for i in 0..self.params.len() {
            let param = self.params.at(i);
            if i == 0 {
                write!(f, "%{}: {:?}", param.0, param.1)?;
            } else {
                write!(f, ", %{}: {:?}", param.0, param.1)?;
            }
        }
        write!(f, ") -> {:?} {{\n", self.ret_type)?;
        for i in 0..self.blocks.len() {
            write!(f, "{}:\n{}", i, self.blocks.at(i))?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl<'a> fmt::Display for IRModule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in 0..self.funcs.len() {
            if i == 0 {
                write!(f, "{}", self.funcs.at(i))?;
            } else {
                write!(f, "\n\n{}", self.funcs.at(i))?;
            }
        }
        Ok(())
    }
}
