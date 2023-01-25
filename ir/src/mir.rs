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
    Fixed(u32),
    Size(usize),
    String(u32),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MIRType {
    Boolean,
    String,
    Real,
    Fixed,
    Size,
    Pointer,
}

impl MIRType {
    pub fn get_size(&self) -> usize {
        match self {
            MIRType::Boolean => 1,
            MIRType::Fixed => 4,
            MIRType::Real | MIRType::Size | MIRType::String | MIRType::Pointer => 8,
        }
    }
}

pub type MIRRegisterID = u32;
pub type MIRFunctionID = u32;
pub type MIRBasicBlockID = u32;
pub type MIRRegister = (MIRRegisterID, MIRType);

pub const MIR_EXTERNAL_FUNCTION_ID: MIRFunctionID = !0;

pub type MIRExternalFunction = (
    (MIRFunctionID, &'static [u8]),
    (&'static [MIRType], Option<MIRType>),
);

pub const MIR_EXTERNAL_FUNCTION_MALLOC: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"malloc"),
    (&[MIRType::Size], Some(MIRType::Pointer)),
);

pub const MIR_EXTERNAL_FUNCTION_MEMCPY: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"memcpy"),
    (&[MIRType::Pointer, MIRType::Pointer, MIRType::Size], None),
);

pub const MIR_EXTERNAL_FUNCTION_ASSERT: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"assert"),
    (&[MIRType::Boolean], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_NIL: MIRExternalFunction =
    ((MIR_EXTERNAL_FUNCTION_ID, b"rt_print_nil"), (&[], None));

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_BOOLEAN: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_print_boolean"),
    (&[MIRType::Boolean], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_STRING: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_print_string"),
    (&[MIRType::String], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_NUMBER: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_print_number"),
    (&[MIRType::Real], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_TENSOR: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_print_tensor"),
    (&[MIRType::Pointer], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_LINE: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_line"),
    (&[], Some(MIRType::Pointer)),
);

pub const MIR_EXTERNAL_FUNCTION_RT_MATMUL: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_matmul"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_ADD_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_add_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_SUBTRACT_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_subtract_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_MULTIPLY_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_multiply_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_DIVIDE_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_divide_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_POWER_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_power_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_NOT_EQUALS_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_not_equals_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Boolean),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_EQUALS_EQUALS_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"rt_equals_equals_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Boolean),
    ),
);

pub const MIR_TENSOR_SIZE: u32 = 24;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MIRUnaryOp {
    Not,
    Negate,
    Shape,
    Round,
    Widen,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MIRBinaryOp {
    AddReals,
    SubtractReals,
    MultiplyReals,
    DivideReals,
    PowerReals,
    AddFixed,
    SubtractFixed,
    MultiplyFixed,
    DivideFixed,
    PowerFixed,
    AddSizes,
    SubtractSizes,
    MultiplySizes,
    DivideSizes,
    PowerSizes,
    GreaterReals,
    LesserReals,
    GreaterFixed,
    LesserFixed,
    GreaterSizes,
    LesserSizes,
    NotEqualsBooleans,
    EqualsEqualsBooleans,
    NotEqualsStrings,
    EqualsEqualsStrings,
    NotEqualsReals,
    EqualsEqualsReals,
    NotEqualsFixed,
    EqualsEqualsFixed,
    NotEqualsSizes,
    EqualsEqualsSizes,
    GreaterEqualsReals,
    LesserEqualsReals,
    GreaterEqualsFixed,
    LesserEqualsFixed,
    GreaterEqualsSizes,
    LesserEqualsSizes,
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
    Load(MIRRegister, MIRRegister),
    Store(MIRRegister, MIRRegister),
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
