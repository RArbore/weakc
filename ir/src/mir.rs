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
use core::str;
use std::io::Write;

#[derive(Debug, PartialEq, Clone, Copy)]
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

pub const MIR_EXTERNAL_FUNCTION_RT_MEMCPY: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_memcpy"),
    (&[MIRType::Pointer, MIRType::Pointer, MIRType::Size], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_ASSERT: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_assert"),
    (&[MIRType::Boolean], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_MALLOC: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_malloc"),
    (&[MIRType::Size], Some(MIRType::Pointer)),
);

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_NIL: MIRExternalFunction =
    ((MIR_EXTERNAL_FUNCTION_ID, b"@rt_print_nil"), (&[], None));

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_BOOLEAN: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_print_boolean"),
    (&[MIRType::Boolean], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_STRING: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_print_string"),
    (&[MIRType::String], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_NUMBER: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_print_number"),
    (&[MIRType::Real], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_PRINT_TENSOR: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_print_tensor"),
    (&[MIRType::Pointer], None),
);

pub const MIR_EXTERNAL_FUNCTION_RT_LINE: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_line"),
    (&[], Some(MIRType::Pointer)),
);

pub const MIR_EXTERNAL_FUNCTION_RT_MATMUL: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_matmul"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_ADD_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_add_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_SUBTRACT_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_subtract_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_MULTIPLY_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_multiply_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_DIVIDE_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_divide_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_POWER_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_power_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Pointer),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_NOT_EQUALS_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_not_equals_tensors"),
    (
        &[MIRType::Pointer, MIRType::Pointer],
        Some(MIRType::Boolean),
    ),
);

pub const MIR_EXTERNAL_FUNCTION_RT_EQUALS_EQUALS_TENSORS: MIRExternalFunction = (
    (MIR_EXTERNAL_FUNCTION_ID, b"@rt_equals_equals_tensors"),
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
    AndBooleans,
    OrBooleans,
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
    pub ret_type: Option<MIRType>,
    pub blocks: &'a mut bump::List<'a, MIRBasicBlock<'a>>,
    pub bloated_num_regs_used: MIRRegisterID,
}

impl<'a> MIRFunction<'a> {
    pub fn naive_stack_vars_size(&self) -> u32 {
        self.bloated_num_regs_used * 8
    }
}

#[derive(Debug, PartialEq)]
pub struct MIRModule<'a> {
    pub funcs: &'a mut bump::List<'a, MIRFunction<'a>>,
    pub strings: &'a mut bump::List<'a, &'a [u8]>,
}

impl fmt::Display for MIRConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MIRConstant::Boolean(v) => {
                write!(f, "{}", v)?;
            }
            MIRConstant::Real(v) => {
                write!(f, "{}", v)?;
            }
            MIRConstant::Fixed(v) => {
                write!(f, "{}", v)?;
            }
            MIRConstant::Size(v) => {
                write!(f, "{}", v)?;
            }
            MIRConstant::String(v) => {
                write!(f, "string {}", v)?;
            }
        }
        Ok(())
    }
}

impl<'a> fmt::Display for MIRInstruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MIRInstruction::Immediate(reg, cons) => {
                if f.alternate() {
                    write!(f, "im %{}, {:#}", reg.0, cons)?;
                } else {
                    write!(f, "im %{}, {}", reg.0, cons)?;
                }
            }
            MIRInstruction::Copy(reg1, reg2) => {
                write!(f, "cp %{}, %{}", reg1.0, reg2.0)?;
            }
            MIRInstruction::Unary(reg1, op, reg2) => {
                write!(f, "un %{}, {:?}, %{}", reg1.0, op, reg2.0)?;
            }
            MIRInstruction::Binary(reg1, op, reg2, reg3) => {
                write!(f, "bi %{}, {:?}, %{}, %{}", reg1.0, op, reg2.0, reg3.0)?;
            }
            MIRInstruction::Gep(reg1, reg2, reg3, ty) => {
                write!(f, "gp %{}, %{}, %{}, {:?}", reg1.0, reg2.0, reg3.0, ty)?;
            }
            MIRInstruction::Load(reg1, reg2) => {
                write!(f, "lo %{}, %{}", reg1.0, reg2.0)?;
            }
            MIRInstruction::Store(reg1, reg2) => {
                write!(f, "st %{}, %{}", reg1.0, reg2.0)?;
            }
            MIRInstruction::BranchUncond(block) => {
                write!(f, "ju {}", block)?;
            }
            MIRInstruction::BranchCond(reg, block1, block2) => {
                write!(f, "br %{}, {}, {}", reg.0, block1, block2)?;
            }
            MIRInstruction::Call(reg, func, args) => {
                if let Some(reg) = reg {
                    write!(
                        f,
                        "ca %{}, {}, (",
                        reg.0,
                        str::from_utf8(func.1)
                            .expect("PANIC: Function name not convertable to Rust str.")
                    )?;
                } else {
                    write!(
                        f,
                        "ca {}, (",
                        str::from_utf8(func.1)
                            .expect("PANIC: Function name not convertable to Rust str.")
                    )?;
                }
                for i in 0..args.len() {
                    if i == 0 {
                        write!(f, "%{}", args.at(i).0)?;
                    } else {
                        write!(f, ", %{}", args.at(i).0)?;
                    }
                }
                write!(f, ")")?;
            }
            MIRInstruction::Return(reg) => {
                if let Some(reg) = reg {
                    write!(f, "re %{}", reg.0)?;
                } else {
                    write!(f, "re")?;
                }
            }
        }
        Ok(())
    }
}

impl<'a> fmt::Display for MIRBasicBlock<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            for i in 0..self.insts.len() {
                write!(f, "{:#}\\l", self.insts.at(i))?;
            }
        } else {
            for i in 0..self.insts.len() {
                write!(f, "    {}\n", self.insts.at(i))?;
            }
        }
        Ok(())
    }
}

impl<'a> fmt::Display for MIRFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn {}(",
            str::from_utf8(self.name).expect("PANIC: Function name not convertable to Rust str.")
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

impl<'a> fmt::Display for MIRModule<'a> {
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

struct MIRDotContext<W: Write> {
    writer: W,
}

impl<W: Write> MIRDotContext<W> {
    fn new(w: W) -> Self {
        MIRDotContext { writer: w }
    }

    fn write_dot_function<'a>(&mut self, function: &'a MIRFunction<'a>) {
        self.writer.write(b"digraph \"CFG for \'").unwrap();
        self.writer.write(function.name).unwrap();
        self.writer
            .write(b"\' function\" {\nlabel=\"CFG for \'")
            .unwrap();
        self.writer.write(function.name).unwrap();
        self.writer.write(b"\' function\";\n").unwrap();
        for i in 0..function.blocks.len() {
            self.write_dot_basic_block(function.blocks.at(i), i as MIRBasicBlockID);
        }
        self.writer.write(b"}\n\n").unwrap();
    }

    fn write_dot_basic_block<'a>(
        &mut self,
        basic_block: &'a MIRBasicBlock<'a>,
        id: MIRBasicBlockID,
    ) {
        let mut name = [0; 14];
        write_basic_block_id(id, &mut name);
        self.writer.write(&name).unwrap();
        self.writer
            .write(
                format!(
                    " [shape=record, style=filled, label=\"{}:\\l{:#}\"];\n",
                    id, basic_block
                )
                .as_bytes(),
            )
            .unwrap();
        match basic_block.successors() {
            MIRBasicBlockSuccessors::Returns => {}
            MIRBasicBlockSuccessors::Jumps(id) => {
                self.writer.write(&name).unwrap();
                self.writer.write(b" -> ").unwrap();
                write_basic_block_id(id, &mut name);
                self.writer.write(&name).unwrap();
                self.writer.write(b";\n").unwrap();
            }
            MIRBasicBlockSuccessors::Branches(id1, id2) => {
                let mut dest = [0; 14];
                write_basic_block_id(id1, &mut dest);
                self.writer.write(&name).unwrap();
                self.writer.write(b" -> ").unwrap();
                self.writer.write(&dest).unwrap();
                self.writer.write(b";\n").unwrap();
                write_basic_block_id(id2, &mut dest);
                self.writer.write(&name).unwrap();
                self.writer.write(b" -> ").unwrap();
                self.writer.write(&dest).unwrap();
                self.writer.write(b";\n").unwrap();
            }
        }
    }
}

fn write_basic_block_id(id: MIRBasicBlockID, buf: &mut [u8]) {
    let conv = |x| {
        if x < 10 {
            x + b'0'
        } else {
            x - 10 + b'A'
        }
    };

    buf[0] = b'N';
    buf[1] = b'o';
    buf[2] = b'd';
    buf[3] = b'e';
    buf[4] = b'0';
    buf[5] = b'x';
    buf[6] = conv((id >> 28) as u8);
    buf[7] = conv((id >> 24 & 15) as u8);
    buf[8] = conv((id >> 20 & 15) as u8);
    buf[9] = conv((id >> 16 & 15) as u8);
    buf[10] = conv((id >> 12 & 15) as u8);
    buf[11] = conv((id >> 8 & 15) as u8);
    buf[12] = conv((id >> 4 & 15) as u8);
    buf[13] = conv((id & 15) as u8);
}

pub fn write_mir_dot_graph<'a, W: Write>(mir: &'a MIRModule<'a>, w: W, func: MIRFunctionID) {
    let mut context = MIRDotContext::new(w);
    context.write_dot_function(mir.funcs.at(func as usize));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_id() {
        let mut buf = [0; 14];
        write_basic_block_id(0xF, &mut buf);
        assert_eq!(&buf, b"Node0x0000000F");
        write_basic_block_id(0xA86BC, &mut buf);
        assert_eq!(&buf, b"Node0x000A86BC");
        write_basic_block_id(0x8756A7B, &mut buf);
        assert_eq!(&buf, b"Node0x08756A7B");
    }
}
