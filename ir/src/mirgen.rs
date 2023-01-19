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

use crate::*;

struct MIRGenContext<'a> {
    module: MIRModule<'a>,
    curr_func: MIRFunctionID,
    curr_block: MIRBasicBlockID,
    bump: &'a bump::BumpAllocator,
}

pub fn mirgen<'a>(program: HIRModule<'a>, bump: &'a bump::BumpAllocator) -> MIRModule<'a> {
    let mut context = MIRGenContext::new(bump);
    context.mirgen_program(program);
    context.module
}

fn convert_type(ty: HIRType) -> Option<MIRType> {
    match ty {
        HIRType::Nil => None,
        HIRType::Boolean => Some(MIRType::Boolean),
        HIRType::String => Some(MIRType::String),
        HIRType::Number => Some(MIRType::Real),
        HIRType::Tensor => Some(MIRType::Pointer),
    }
}

fn convert_register(reg: HIRRegister) -> Option<MIRRegister> {
    let (id, ty) = reg;
    if let Some(ty) = convert_type(ty) {
        Some((id, ty))
    } else {
        None
    }
}

fn convert_2_registers(regs: (HIRRegister, HIRRegister)) -> Option<(MIRRegister, MIRRegister)> {
    let reg1 = convert_register(regs.0);
    let reg2 = convert_register(regs.1);
    if let (Some(reg1), Some(reg2)) = (reg1, reg2) {
        Some((reg1, reg2))
    } else if let (None, None) = (reg1, reg2) {
        None
    } else {
        panic!("PANIC: One register in operation is Nil typed, and the other isn't.");
    }
}

fn convert_3_registers(
    regs: (HIRRegister, HIRRegister, HIRRegister),
) -> Option<(MIRRegister, MIRRegister, MIRRegister)> {
    let reg1 = convert_register(regs.0);
    let reg2 = convert_register(regs.1);
    let reg3 = convert_register(regs.2);
    if let (Some(reg1), Some(reg2), Some(reg3)) = (reg1, reg2, reg3) {
        Some((reg1, reg2, reg3))
    } else if let (None, None, None) = (reg1, reg2, reg3) {
        None
    } else {
        panic!("PANIC: Some register in operation is Nil typed, and some other isn't.");
    }
}

impl<'a> MIRGenContext<'a> {
    fn new(bump: &'a bump::BumpAllocator) -> Self {
        let context = MIRGenContext {
            module: MIRModule {
                funcs: bump.create_list(),
                strings: bump.create_list(),
            },
            curr_func: 0,
            curr_block: 0,
            bump,
        };
        context
    }

    fn convert_string(&mut self, string: &'a [u8]) -> u32 {
        let mut i = 0;
        while i < self.module.strings.len() as u32 {
            if *self.module.strings.at(i as usize) == string {
                return i;
            }
            i += 1;
        }
        self.module.strings.push(string);
        return i;
    }

    fn mirgen_program(&mut self, program: HIRModule<'a>) {
        for i in 0..program.funcs.len() {
            self.mirgen_func(program.funcs.at(i));
        }
    }

    fn mirgen_func(&mut self, func: &'a HIRFunction<'a>) {
        let mir_func = MIRFunction {
            name: func.name,
            params: self.bump.create_list(),
            ret_type: convert_type(func.ret_type),
            blocks: self.bump.create_list(),
        };

        for i in 0..func.params.len() {
            if let Some(reg) = convert_register(*func.params.at(i)) {
                mir_func.params.push(reg);
            }
        }

        self.curr_func = self.module.funcs.len() as MIRFunctionID;
        self.curr_block = 0;
        self.module.funcs.push(mir_func);

        for i in 0..func.blocks.len() {
            self.mirgen_block(func.blocks.at(i));
        }
    }

    fn add_inst(&mut self, inst: MIRInstruction<'a>) {
        self.module
            .funcs
            .at_mut(self.curr_func as usize)
            .blocks
            .at_mut(self.curr_block as usize)
            .insts
            .push(inst);
    }

    fn mirgen_block(&mut self, block: &'a HIRBasicBlock<'a>) {
        let mir_block = MIRBasicBlock {
            insts: self.bump.create_list(),
        };

        self.curr_block =
            self.module.funcs.at(self.curr_func as usize).blocks.len() as MIRBasicBlockID;
        self.module
            .funcs
            .at_mut(self.curr_func as usize)
            .blocks
            .push(mir_block);

        for i in 0..block.insts.len() {
            match block.insts.at(i) {
                HIRInstruction::Immediate(hir_reg, hir_const) => {
                    if let Some(mir_reg) = convert_register(*hir_reg) {
                        let mir_const = match hir_const {
                            HIRConstant::Boolean(v) => MIRConstant::Boolean(*v),
                            HIRConstant::Number(v) => MIRConstant::Real(*v),
                            HIRConstant::String(v) => MIRConstant::String(self.convert_string(*v)),
                            _ => panic!("PANIC: Unhandled HIR constant in immediate instruction."),
                        };
                        self.add_inst(MIRInstruction::Immediate(mir_reg, mir_const));
                    }
                }
                HIRInstruction::Copy(left_reg, right_reg) => {
                    if let Some((left_mir_reg, right_mir_reg)) =
                        convert_2_registers((*left_reg, *right_reg))
                    {
                        self.add_inst(MIRInstruction::Copy(left_mir_reg, right_mir_reg));
                    }
                }
                HIRInstruction::Unary(left_reg, op, right_reg) => {
                    let op = match op {
                        HIRUnaryOp::Not => MIRUnaryOp::Not,
                        HIRUnaryOp::Negate => MIRUnaryOp::Negate,
                        HIRUnaryOp::Shape => MIRUnaryOp::Shape,
                    };
                    if let Some((left_mir_reg, right_mir_reg)) =
                        convert_2_registers((*left_reg, *right_reg))
                    {
                        self.add_inst(MIRInstruction::Unary(left_mir_reg, op, right_mir_reg));
                    }
                }
                _ => todo!(),
            }
        }
    }
}
