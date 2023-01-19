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
pub enum MIRConstant {
    Boolean(bool),
    Real(f64),
    Index(usize),
    String(u32),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MIRType {
    Boolean,
    String,
    Real,
    Index,
    Pointer,
}

pub type MIRRegisterID = u32;
pub type MIRFunctionID = u32;
pub type MIRBasicBlockID = u32;
pub type MIRRegister = (MIRRegisterID, MIRType);

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MIRUnaryOp {
    Not,
    Negate,
    Shape,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MIRBinaryOp {
    AddReals,
    SubtractReals,
    MultiplyReals,
    DivideReals,
    PowerReals,
    AddIndices,
    SubtractIndices,
    MultiplyIndices,
    DivideIndices,
    PowerIndices,
    GreaterReals,
    LesserReals,
    GreaterIndices,
    LesserIndices,
    NotEqualsBooleans,
    EqualsEqualsBooleans,
    NotEqualsStrings,
    EqualsEqualsStrings,
    NotEqualsReals,
    EqualsEqualsReals,
    NotEqualsIndices,
    EqualsEqualsIndices,
    GreaterEqualsReals,
    LesserEqualsReals,
    GreaterEqualsIndices,
    LesserEqualsIndices,
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub enum MIRInstruction<'a> {
    Immediate(MIRRegister, MIRConstant),
    Copy(MIRRegister, MIRRegister),
    Unary(MIRRegister, MIRUnaryOp, MIRRegister),
    Binary(MIRRegister, MIRBinaryOp, MIRRegister, MIRRegister),
    Gep(MIRRegister, MIRRegister, MIRRegister, MIRType),
    Load(MIRRegister, MIRRegister, MIRType),
    Store(MIRRegister, MIRRegister, MIRType),
    Alloca(MIRRegister, usize),
    BranchUncond(MIRBasicBlockID),
    BranchCond(MIRRegister, MIRBasicBlockID, MIRBasicBlockID),
    Call(
        Option<MIRRegister>,
        (MIRFunctionID, &'a [u8]),
        &'a bump::List<'a, MIRRegister>,
    ),
    Return(Option<MIRRegister>),
}

#[derive(Debug, PartialEq)]
pub struct MIRBasicBlock<'a> {
    pub insts: &'a mut bump::List<'a, MIRInstruction<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct MIRFunction<'a> {
    pub name: &'a [u8],
    pub params: &'a mut bump::List<'a, MIRRegister>,
    pub ret_type: Option<MIRType>,
    pub blocks: &'a mut bump::List<'a, MIRBasicBlock<'a>>,
    pub num_regs_used: MIRRegisterID,
}

#[derive(Debug, PartialEq)]
pub struct MIRModule<'a> {
    pub funcs: &'a mut bump::List<'a, MIRFunction<'a>>,
    pub strings: &'a mut bump::List<'a, &'a [u8]>,
}
