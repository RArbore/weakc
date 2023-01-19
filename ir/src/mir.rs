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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MIRBasicBlockSuccessors {
    Returns,
    Jumps(MIRBasicBlockID),
    Branches(MIRBasicBlockID, MIRBasicBlockID),
}

impl<'a> MIRBasicBlock<'a> {
    pub fn successors(&self) -> MIRBasicBlockSuccessors {
        assert!(self.insts.len() > 0, "PANIC: Empty basic block.");
        let last_inst = self.insts.at(self.insts.len() - 1);
        match last_inst {
            MIRInstruction::Return(_) => MIRBasicBlockSuccessors::Returns,
            MIRInstruction::BranchUncond(b) => MIRBasicBlockSuccessors::Jumps(*b),
            MIRInstruction::BranchCond(_, b1, b2) => MIRBasicBlockSuccessors::Branches(*b1, *b2),
            _ => panic!("PANIC: Found invalid terminating instruction of basic block."),
        }
    }

    pub fn is_terminated(&self) -> bool {
        if self.insts.len() > 0 {
            match self.insts.at(self.insts.len() - 1) {
                MIRInstruction::Return(_) => true,
                MIRInstruction::BranchUncond(_) => true,
                MIRInstruction::BranchCond(_, _, _) => true,
                _ => false,
            }
        } else {
            false
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct MIRFunction<'a> {
    pub name: &'a [u8],
    pub params: &'a mut bump::List<'a, MIRRegister>,
    pub ret_type: MIRType,
    pub blocks: &'a mut bump::List<'a, MIRBasicBlock<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct MIRModule<'a> {
    pub funcs: &'a mut bump::List<'a, MIRFunction<'a>>,
    pub strings: &'a mut bump::List<'a, &'a [u8]>,
}
