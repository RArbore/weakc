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

#[derive(Debug, PartialEq, Clone)]
pub enum HIRConstant<'a> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(&'a [u8]),
    Tensor(&'a bump::List<'a, u32>, &'a bump::List<'a, f64>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HIRType {
    Nil,
    Boolean,
    String,
    Number,
    Tensor,
}

pub type HIRRegisterID = u32;
pub type HIRFunctionID = u32;
pub type HIRBasicBlockID = u32;
pub type HIRRegister = (HIRRegisterID, HIRType);

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HIRUnaryOp {
    Not,
    Negate,
    Shape,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HIRBinaryOp {
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
pub enum HIRInstruction<'a> {
    Immediate(HIRRegister, HIRConstant<'a>),
    Copy(HIRRegister, HIRRegister),
    Unary(HIRRegister, HIRUnaryOp, HIRRegister),
    Binary(HIRRegister, HIRBinaryOp, HIRRegister, HIRRegister),
    Index(HIRRegister, HIRRegister, &'a bump::List<'a, HIRRegister>),
    Array(HIRRegister, &'a bump::List<'a, HIRRegister>),
    BranchUncond(HIRBasicBlockID),
    BranchCond(HIRRegister, HIRBasicBlockID, HIRBasicBlockID),
    Call(
        HIRRegister,
        (HIRFunctionID, &'a [u8]),
        &'a bump::List<'a, HIRRegister>,
    ),
    Print(HIRRegister),
    Line(HIRRegister),
    Verify(HIRRegister),
    Return(HIRRegister),
}

#[derive(Debug, PartialEq)]
pub struct HIRBasicBlock<'a> {
    pub insts: &'a mut bump::List<'a, HIRInstruction<'a>>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum HIRBasicBlockSuccessors {
    Returns,
    Jumps(HIRBasicBlockID),
    Branches(HIRBasicBlockID, HIRBasicBlockID),
}

impl<'a> HIRBasicBlock<'a> {
    pub fn successors(&self) -> HIRBasicBlockSuccessors {
        assert!(self.insts.len() > 0, "PANIC: Empty basic block.");
        let last_inst = self.insts.at(self.insts.len() - 1);
        match last_inst {
            HIRInstruction::Return(_) => HIRBasicBlockSuccessors::Returns,
            HIRInstruction::BranchUncond(b) => HIRBasicBlockSuccessors::Jumps(*b),
            HIRInstruction::BranchCond(_, b1, b2) => HIRBasicBlockSuccessors::Branches(*b1, *b2),
            _ => panic!("PANIC: Found invalid terminating instruction of basic block."),
        }
    }

    pub fn is_terminated(&self) -> bool {
        if self.insts.len() > 0 {
            match self.insts.at(self.insts.len() - 1) {
                HIRInstruction::Return(_) => true,
                HIRInstruction::BranchUncond(_) => true,
                HIRInstruction::BranchCond(_, _, _) => true,
                _ => false,
            }
        } else {
            false
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct HIRFunction<'a> {
    pub name: &'a [u8],
    pub params: &'a mut bump::List<'a, HIRRegister>,
    pub ret_type: HIRType,
    pub blocks: &'a mut bump::List<'a, HIRBasicBlock<'a>>,
    pub num_regs_used: HIRRegisterID,
}

#[derive(Debug, PartialEq)]
pub struct HIRModule<'a> {
    pub funcs: &'a mut bump::List<'a, HIRFunction<'a>>,
}

impl<'a> fmt::Display for HIRConstant<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HIRConstant::Nil => {
                write!(f, "Nil")?;
            }
            HIRConstant::Boolean(v) => {
                write!(f, "{}", v)?;
            }
            HIRConstant::Number(v) => {
                write!(f, "{}", v)?;
            }
            HIRConstant::String(v) => {
                if f.alternate() {
                    write!(
                        f,
                        "\\\"{}\\\"",
                        str::from_utf8(v).expect("PANIC: Couldn't convert string to utf8.")
                    )?;
                } else {
                    write!(
                        f,
                        "\"{}\"",
                        str::from_utf8(v).expect("PANIC: Couldn't convert string to utf8.")
                    )?;
                }
            }
            HIRConstant::Tensor(s, v) => {
                write!(f, "{:?} sa {:?}", v, s)?;
            }
        }
        Ok(())
    }
}

impl<'a> fmt::Display for HIRInstruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HIRInstruction::Immediate(reg, cons) => {
                if f.alternate() {
                    write!(f, "im %{}, {:#}", reg.0, cons)?;
                } else {
                    write!(f, "im %{}, {}", reg.0, cons)?;
                }
            }
            HIRInstruction::Copy(reg1, reg2) => {
                write!(f, "cp %{}, %{}", reg1.0, reg2.0)?;
            }
            HIRInstruction::Unary(reg1, op, reg2) => {
                write!(f, "un %{}, {:?}, %{}", reg1.0, op, reg2.0)?;
            }
            HIRInstruction::Binary(reg1, op, reg2, reg3) => {
                write!(f, "bi %{}, {:?}, %{}, %{}", reg1.0, op, reg2.0, reg3.0)?;
            }
            HIRInstruction::Index(reg1, reg2, indices) => {
                let op = if f.alternate() {
                    ("\\<=", "\\>=")
                } else {
                    ("<=", ">=")
                };
                match (reg1.1, reg2.1) {
                    (HIRType::Number, HIRType::Tensor) => {
                        write!(f, "in %{} {} %{}, [", reg1.0, op.0, reg2.0)?;
                    }
                    (HIRType::Tensor, HIRType::Number) => {
                        write!(f, "in %{} {} %{}, [", reg1.0, op.1, reg2.0)?;
                    }
                    _ => panic!("PANIC: Can't print invalid HIR."),
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
            HIRInstruction::Array(reg, elems) => {
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
            HIRInstruction::BranchUncond(block) => {
                write!(f, "ju {}", block)?;
            }
            HIRInstruction::BranchCond(reg, block1, block2) => {
                write!(f, "br %{}, {}, {}", reg.0, block1, block2)?;
            }
            HIRInstruction::Call(reg, func, args) => {
                write!(
                    f,
                    "ca %{}, {}, (",
                    reg.0,
                    str::from_utf8(func.1)
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
            HIRInstruction::Print(reg) => {
                write!(f, "pr %{}", reg.0)?;
            }
            HIRInstruction::Line(reg) => {
                write!(f, "li %{}", reg.0)?;
            }
            HIRInstruction::Verify(reg) => {
                write!(f, "ve %{}", reg.0)?;
            }
            HIRInstruction::Return(reg) => {
                write!(f, "re %{}", reg.0)?;
            }
        }
        Ok(())
    }
}

impl<'a> fmt::Display for HIRBasicBlock<'a> {
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

impl<'a> fmt::Display for HIRFunction<'a> {
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

impl<'a> fmt::Display for HIRModule<'a> {
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

struct HIRDotContext<W: Write> {
    writer: W,
}

impl<W: Write> HIRDotContext<W> {
    fn new(w: W) -> Self {
        HIRDotContext { writer: w }
    }

    fn write_dot_function<'a>(&mut self, function: &'a HIRFunction<'a>) {
        self.writer.write(b"digraph \"CFG for \'").unwrap();
        self.writer.write(function.name).unwrap();
        self.writer
            .write(b"\' function\" {\nlabel=\"CFG for \'")
            .unwrap();
        self.writer.write(function.name).unwrap();
        self.writer.write(b"\' function\";\n").unwrap();
        for i in 0..function.blocks.len() {
            self.write_dot_basic_block(function.blocks.at(i), i as HIRBasicBlockID);
        }
        self.writer.write(b"}\n\n").unwrap();
    }

    fn write_dot_basic_block<'a>(
        &mut self,
        basic_block: &'a HIRBasicBlock<'a>,
        id: HIRBasicBlockID,
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
            HIRBasicBlockSuccessors::Returns => {}
            HIRBasicBlockSuccessors::Jumps(id) => {
                self.writer.write(&name).unwrap();
                self.writer.write(b" -> ").unwrap();
                write_basic_block_id(id, &mut name);
                self.writer.write(&name).unwrap();
                self.writer.write(b";\n").unwrap();
            }
            HIRBasicBlockSuccessors::Branches(id1, id2) => {
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

fn write_basic_block_id(id: HIRBasicBlockID, buf: &mut [u8]) {
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

pub fn write_hir_dot_graph<'a, W: Write>(hir: &'a HIRModule<'a>, w: W, func: HIRFunctionID) {
    let mut context = HIRDotContext::new(w);
    context.write_dot_function(hir.funcs.at(func as usize));
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
