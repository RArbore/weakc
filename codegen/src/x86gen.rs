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

use core::cmp::max;

extern crate bump;
extern crate ir;

use crate::*;

struct X86GenContext<'a> {
    module: X86Module<'a>,
    curr_block: X86BlockID,
    curr_func: Option<&'a ir::MIRFunction<'a>>,
    weak_float_labels: &'a mut bump::List<'a, &'a [u8]>,
    weak_string_labels: &'a mut bump::List<'a, &'a [u8]>,
    curr_func_block_labels: &'a mut bump::List<'a, &'a [u8]>,
    bump: &'a bump::BumpAllocator,
}

pub fn x86gen<'a>(program: &'a ir::MIRModule<'a>, bump: &'a bump::BumpAllocator) -> X86Module<'a> {
    let mut context = X86GenContext::new(bump);
    context.x86gen_program(program);
    context.module
}

impl<'a> X86GenContext<'a> {
    fn new(bump: &'a bump::BumpAllocator) -> Self {
        let context = X86GenContext {
            module: X86Module {
                func_entries: bump.create_list(),
                blocks: bump.create_list(),
                strings: bump.create_list(),
                floats: bump.create_list(),
                pre_norm_num_virtual_registers: 0,
            },
            curr_block: 0,
            curr_func: None,
            weak_float_labels: bump.create_list(),
            weak_string_labels: bump.create_list(),
            curr_func_block_labels: bump.create_list(),
            bump: bump,
        };
        context
    }

    fn record_float(&mut self, f: f64) -> usize {
        for i in 0..self.module.floats.len() {
            if *self.module.floats.at(i) == f {
                return i;
            }
        }
        self.module.floats.push(f);
        let weak_float_label = unsafe { self.bump.alloc_slice_raw(30) };
        for j in 0..12 {
            weak_float_label[j] = b".weak.float."[j];
        }
        write_bits_to_hex(f.to_bits(), weak_float_label, 12);
        self.weak_float_labels.push(weak_float_label);
        return self.module.floats.len() - 1;
    }

    fn rax_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::RAX))
    }

    fn rdi_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::RDI))
    }

    fn rsi_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::RSI))
    }

    fn rdx_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::RDX))
    }

    fn rcx_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::RCX))
    }

    fn r8_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::R8))
    }

    fn r9_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::R9))
    }

    fn rsp_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::RSP))
    }

    fn xmm0_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::XMM0))
    }

    fn xmm1_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::XMM1))
    }

    fn xmm2_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::XMM2))
    }

    fn xmm3_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::XMM3))
    }

    fn xmm4_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::XMM4))
    }

    fn xmm5_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::XMM5))
    }

    fn xmm6_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::XMM6))
    }

    fn xmm7_operand() -> X86Operand<'a> {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::XMM7))
    }

    fn mir_to_x86_virt_reg(&self, mir_reg: ir::MIRRegister) -> X86Register {
        X86Register::Virtual(
            mir_reg.0,
            match mir_reg.1 {
                ir::MIRType::Boolean => X86VirtualRegisterType::Fixed32,
                ir::MIRType::String => X86VirtualRegisterType::Fixed64,
                ir::MIRType::Real => X86VirtualRegisterType::Float64,
                ir::MIRType::Fixed => X86VirtualRegisterType::Fixed32,
                ir::MIRType::Size => X86VirtualRegisterType::Fixed64,
                ir::MIRType::Pointer => X86VirtualRegisterType::Fixed64,
            },
        )
    }

    fn get_curr_block_mut(&mut self) -> &mut X86Block<'a> {
        return self.module.blocks.at_mut(self.curr_block as usize);
    }

    fn x86gen_inst(&mut self, inst: X86Instruction<'a>) {
        match inst.get_virtual_register_pack() {
            X86VirtualRegisterPack::OneDef(id) | X86VirtualRegisterPack::One(id) => {
                self.module.pre_norm_num_virtual_registers =
                    max(self.module.pre_norm_num_virtual_registers, id + 1);
            }
            X86VirtualRegisterPack::TwoDef(id1, id2) | X86VirtualRegisterPack::Two(id1, id2) => {
                self.module.pre_norm_num_virtual_registers = max(
                    self.module.pre_norm_num_virtual_registers,
                    max(id1 + 1, id2 + 1),
                );
            }
            X86VirtualRegisterPack::ThreeDef(id1, id2, id3)
            | X86VirtualRegisterPack::Three(id1, id2, id3) => {
                self.module.pre_norm_num_virtual_registers = max(
                    self.module.pre_norm_num_virtual_registers,
                    max(id1 + 1, max(id2 + 1, id3 + 1)),
                );
            }
            X86VirtualRegisterPack::FourDef(id1, id2, id3, id4)
            | X86VirtualRegisterPack::Four(id1, id2, id3, id4) => {
                self.module.pre_norm_num_virtual_registers = max(
                    self.module.pre_norm_num_virtual_registers,
                    max(id1 + 1, max(id2 + 1, max(id3 + 1, id4 + 1))),
                );
            }
            _ => {}
        }
        self.get_curr_block_mut().insts.push(inst);
    }

    fn x86gen_block(
        &mut self,
        block: &'a ir::MIRBasicBlock<'a>,
        block_id: X86BlockID,
        func_block_id: X86BlockID,
        label: &'a [u8],
    ) {
        let mir_successors = block.successors();
        let x86_successors = match mir_successors {
            ir::MIRBasicBlockSuccessors::Returns => X86BlockSuccessors::Returns,
            ir::MIRBasicBlockSuccessors::Jumps(id) => {
                X86BlockSuccessors::Jumps(id + func_block_id + 1)
            }
            ir::MIRBasicBlockSuccessors::Branches(id1, id2) => {
                X86BlockSuccessors::Branches(id1 + func_block_id + 1, id2 + func_block_id + 1)
            }
        };
        self.curr_block = block_id;
        self.module.blocks.push(X86Block {
            label,
            insts: self.bump.create_list(),
            id: block_id,
            successors: x86_successors,
        });
        for i in 0..block.insts.len() {
            match block.insts.at(i) {
                ir::MIRInstruction::Immediate(reg, val) => match val {
                    ir::MIRConstant::Boolean(val) => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*reg)),
                            X86Operand::Immediate(if *val { 1 } else { 0 }),
                        ));
                    }
                    ir::MIRConstant::Real(val) => {
                        let virt_reg = self.mir_to_x86_virt_reg(*reg);
                        if *val != 0.0 {
                            let float_id = self.record_float(*val);
                            self.x86gen_inst(X86Instruction::Movsd(
                                X86Operand::Register(virt_reg),
                                X86Operand::MemoryLabel(
                                    X86Register::Physical(X86PhysicalRegisterID::RIP),
                                    self.weak_float_labels.at(float_id),
                                ),
                            ));
                        } else {
                            self.x86gen_inst(X86Instruction::Xorps(
                                X86Operand::Register(virt_reg),
                                X86Operand::Register(virt_reg),
                            ));
                        }
                    }
                    ir::MIRConstant::Fixed(val) => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*reg)),
                            X86Operand::Immediate(*val as u64),
                        ));
                    }
                    ir::MIRConstant::Size(val) => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*reg)),
                            X86Operand::Immediate(*val as u64),
                        ));
                    }
                    ir::MIRConstant::String(val) => {
                        self.x86gen_inst(X86Instruction::Lea(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*reg)),
                            X86Operand::MemoryLabel(
                                X86Register::Physical(X86PhysicalRegisterID::RIP),
                                self.weak_string_labels.at(*val as usize),
                            ),
                        ));
                    }
                },
                ir::MIRInstruction::Copy(dst_reg, src_reg) => {
                    self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                        X86Operand::Register(self.mir_to_x86_virt_reg(*src_reg)),
                    ));
                }
                ir::MIRInstruction::Unary(dst_reg, op, src_reg) => {
                    match op {
                        ir::MIRUnaryOp::Not => {
                            self.x86gen_inst(X86Instruction::Mov(
                                X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*src_reg)),
                            ));
                            self.x86gen_inst(X86Instruction::Not(X86Operand::Register(
                                self.mir_to_x86_virt_reg(*dst_reg),
                            )));
                        }
                        ir::MIRUnaryOp::Negate => {
                            self.x86gen_inst(X86Instruction::Mov(
                                X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*src_reg)),
                            ));
                            self.x86gen_inst(X86Instruction::Neg(X86Operand::Register(
                                self.mir_to_x86_virt_reg(*dst_reg),
                            )));
                        }
                        ir::MIRUnaryOp::Round => {
                            self.x86gen_inst(X86Instruction::Cvttsd2si(
                                X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*src_reg)),
                            ));
                        }
                        ir::MIRUnaryOp::Widen => {
                            self.x86gen_inst(X86Instruction::Movsxd(
                                X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*src_reg)),
                            ));
                        }
                    };
                }
                ir::MIRInstruction::Binary(dst_reg, op, left_reg, right_reg) => match op {
                    ir::MIRBinaryOp::AddReals => {
                        self.x86gen_inst(X86Instruction::Movsd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Addsd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }
                    ir::MIRBinaryOp::SubtractReals => {
                        self.x86gen_inst(X86Instruction::Movsd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Subsd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }
                    ir::MIRBinaryOp::MultiplyReals => {
                        self.x86gen_inst(X86Instruction::Movsd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Mulsd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }
                    ir::MIRBinaryOp::DivideReals => {
                        self.x86gen_inst(X86Instruction::Movsd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Divsd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }

                    ir::MIRBinaryOp::AddFixed | ir::MIRBinaryOp::AddSizes => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Add(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }
                    ir::MIRBinaryOp::SubtractFixed | ir::MIRBinaryOp::SubtractSizes => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Sub(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }
                    ir::MIRBinaryOp::MultiplyFixed | ir::MIRBinaryOp::MultiplySizes => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Imul(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }
                    ir::MIRBinaryOp::GreaterReals => {
                        self.x86gen_inst(X86Instruction::Comisd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Seta(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::LesserReals => {
                        self.x86gen_inst(X86Instruction::Comisd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Seta(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::LesserSizes => {
                        self.x86gen_inst(X86Instruction::Cmp(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Seta(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::NotEqualsBooleans => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Xor(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }
                    ir::MIRBinaryOp::EqualsEqualsBooleans => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Xor(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Not(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::NotEqualsStrings => {
                        self.x86gen_inst(X86Instruction::Cmp(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Setne(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::EqualsEqualsStrings
                    | ir::MIRBinaryOp::EqualsEqualsFixed
                    | ir::MIRBinaryOp::EqualsEqualsSizes => {
                        self.x86gen_inst(X86Instruction::Cmp(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Sete(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::NotEqualsReals => {
                        self.x86gen_inst(X86Instruction::Comisd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Setne(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::EqualsEqualsReals => {
                        self.x86gen_inst(X86Instruction::Comisd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Sete(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::GreaterEqualsReals => {
                        self.x86gen_inst(X86Instruction::Comisd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Setae(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::LesserEqualsReals => {
                        self.x86gen_inst(X86Instruction::Comisd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Setae(X86Operand::Register(
                            self.mir_to_x86_virt_reg(*dst_reg),
                        )));
                    }
                    ir::MIRBinaryOp::AndBooleans => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::And(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }
                    ir::MIRBinaryOp::OrBooleans => {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*left_reg)),
                        ));
                        self.x86gen_inst(X86Instruction::Or(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*right_reg)),
                        ));
                    }
                },
                ir::MIRInstruction::Gep(dst_reg, src_reg, offset_reg, offset_type) => {
                    self.x86gen_inst(X86Instruction::Lea(
                        X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                        X86Operand::MemoryOffsetLinear(
                            offset_type.get_size(),
                            self.mir_to_x86_virt_reg(*offset_reg),
                            self.mir_to_x86_virt_reg(*src_reg),
                        ),
                    ));
                }
                ir::MIRInstruction::Load(dst_reg, src_reg) => {
                    self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                        X86Operand::MemoryOffsetConstant(self.mir_to_x86_virt_reg(*src_reg), 0),
                    ));
                }
                ir::MIRInstruction::Store(src_reg, dst_reg) => {
                    self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::MemoryOffsetConstant(self.mir_to_x86_virt_reg(*dst_reg), 0),
                        X86Operand::Register(self.mir_to_x86_virt_reg(*src_reg)),
                    ));
                }
                ir::MIRInstruction::BranchUncond(mir_block_id) => {
                    self.x86gen_inst(X86Instruction::Jmp(
                        self.curr_func_block_labels.at(*mir_block_id as usize),
                    ));
                }
                ir::MIRInstruction::BranchCond(cond_reg, true_mir_block_id, false_mir_block_id) => {
                    self.x86gen_inst(X86Instruction::Test(
                        X86Operand::Register(self.mir_to_x86_virt_reg(*cond_reg)),
                        X86Operand::Register(self.mir_to_x86_virt_reg(*cond_reg)),
                    ));
                    self.x86gen_inst(X86Instruction::Jnz(
                        self.curr_func_block_labels.at(*true_mir_block_id as usize),
                    ));
                    self.x86gen_inst(X86Instruction::Jmp(
                        self.curr_func_block_labels.at(*false_mir_block_id as usize),
                    ));
                }
                ir::MIRInstruction::Call(ret, (_, label), args) => {
                    let mut num_pushed_args = 0;
                    let mut num_fixed_args = 0;
                    let mut num_floating_args = 0;
                    for i in 0..args.len() {
                        let mir_reg = *args.at(i);
                        let virt_reg = self.mir_to_x86_virt_reg(mir_reg);
                        if mir_reg.1 == ir::MIRType::Real {
                            match num_floating_args {
                                0 => self.x86gen_inst(X86Instruction::Movsd(
                                    Self::xmm0_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                1 => self.x86gen_inst(X86Instruction::Movsd(
                                    Self::xmm1_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                2 => self.x86gen_inst(X86Instruction::Movsd(
                                    Self::xmm2_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                3 => self.x86gen_inst(X86Instruction::Movsd(
                                    Self::xmm3_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                4 => self.x86gen_inst(X86Instruction::Movsd(
                                    Self::xmm4_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                5 => self.x86gen_inst(X86Instruction::Movsd(
                                    Self::xmm5_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                6 => self.x86gen_inst(X86Instruction::Movsd(
                                    Self::xmm6_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                7 => self.x86gen_inst(X86Instruction::Movsd(
                                    Self::xmm7_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                _ => {
                                    num_pushed_args += 1;
                                    self.x86gen_inst(X86Instruction::Sub(
                                        X86Operand::Register(X86Register::Physical(
                                            X86PhysicalRegisterID::RSP,
                                        )),
                                        X86Operand::Immediate(8),
                                    ));
                                    self.x86gen_inst(X86Instruction::Movsd(
                                        X86Operand::MemoryOffsetConstant(
                                            X86Register::Physical(X86PhysicalRegisterID::RSP),
                                            0,
                                        ),
                                        X86Operand::Register(virt_reg),
                                    ));
                                }
                            }
                            num_floating_args += 1;
                        } else {
                            match num_fixed_args {
                                0 => self.x86gen_inst(X86Instruction::Mov(
                                    Self::rdi_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                1 => self.x86gen_inst(X86Instruction::Mov(
                                    Self::rsi_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                2 => self.x86gen_inst(X86Instruction::Mov(
                                    Self::rdx_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                3 => self.x86gen_inst(X86Instruction::Mov(
                                    Self::rcx_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                4 => self.x86gen_inst(X86Instruction::Mov(
                                    Self::r8_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                5 => self.x86gen_inst(X86Instruction::Mov(
                                    Self::r9_operand(),
                                    X86Operand::Register(virt_reg),
                                )),
                                _ => {
                                    num_pushed_args += 1;
                                    self.x86gen_inst(X86Instruction::Push(X86Operand::Register(
                                        virt_reg,
                                    )));
                                }
                            }
                            num_fixed_args += 1;
                        }
                    }
                    self.x86gen_inst(X86Instruction::Call(&label[1..]));
                    if num_pushed_args > 0 {
                        self.x86gen_inst(X86Instruction::Add(
                            Self::rsp_operand(),
                            X86Operand::Immediate(num_pushed_args * 8),
                        ));
                    }
                    if let Some(ret) = ret {
                        if ret.1 == ir::MIRType::Real {
                            self.x86gen_inst(X86Instruction::Movsd(
                                X86Operand::Register(self.mir_to_x86_virt_reg(*ret)),
                                Self::xmm0_operand(),
                            ));
                        } else {
                            self.x86gen_inst(X86Instruction::Mov(
                                X86Operand::Register(self.mir_to_x86_virt_reg(*ret)),
                                Self::rax_operand(),
                            ));
                        }
                    }
                }
                ir::MIRInstruction::Return(ret_val) => {
                    if let Some(ret_val) = ret_val {
                        if ret_val.1 == ir::MIRType::Real {
                            self.x86gen_inst(X86Instruction::Movsd(
                                Self::xmm0_operand(),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*ret_val)),
                            ));
                        } else {
                            self.x86gen_inst(X86Instruction::Mov(
                                Self::rax_operand(),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*ret_val)),
                            ));
                        }
                    }
                    self.x86gen_function_epilogue(self.curr_func.unwrap());
                    self.x86gen_inst(X86Instruction::Ret);
                }
            };
        }
    }

    fn x86gen_program(&mut self, program: &'a ir::MIRModule<'a>) {
        for i in 0..program.strings.len() {
            self.module.strings.push(program.strings.at(i));
            let weak_string_label = unsafe { self.bump.alloc_slice_raw(23) };
            for j in 0..13 {
                weak_string_label[j] = b".weak.string."[j];
            }
            write_decimal(i, weak_string_label, 13);
            self.weak_string_labels.push(weak_string_label);
        }
        let mut max_len = 0;
        for i in 0..program.funcs.len() {
            if max_len < program.funcs.at(i).blocks.len() {
                max_len = program.funcs.at(i).blocks.len();
            }
        }
        for _ in 0..max_len {
            unsafe { self.curr_func_block_labels.push_empty() };
        }
        for i in 0..program.funcs.len() {
            self.x86gen_function(program.funcs.at(i));
        }
    }

    fn x86gen_function(&mut self, func: &'a ir::MIRFunction<'a>) {
        let func_block_id = self.module.blocks.len() as X86BlockID;
        self.curr_block = func_block_id;
        self.curr_func = Some(func);
        self.module.func_entries.push((func_block_id, 0));
        self.module.blocks.push(X86Block {
            label: &func.name[1..],
            insts: self.bump.create_list(),
            id: func_block_id,
            successors: X86BlockSuccessors::Jumps(func_block_id + 1),
        });
        for i in 0..func.blocks.len() {
            let label = unsafe { self.bump.alloc_slice_raw(func.name.len() + 10) };
            for j in 1..func.name.len() {
                label[j - 1] = func.name[j];
            }
            label[func.name.len() - 1] = b'.';
            let block_id = (i + 1) as X86BlockID + func_block_id;
            write_block_id(block_id, label, func.name.len());
            *self.curr_func_block_labels.at_mut(i) = label;
        }
        for i in 0..func.blocks.len() {
            let label = self.curr_func_block_labels.at(i);
            if i == 0 {
                self.x86gen_function_prologue(func, label);
            }
            let label = self.curr_func_block_labels.at(i);
            let block_id = (i + 1) as X86BlockID + func_block_id;
            self.x86gen_block(func.blocks.at(i), block_id, func_block_id, label);
        }
        self.curr_func = None;
    }

    fn x86gen_function_prologue(&mut self, func: &'a ir::MIRFunction<'a>, label: &'a [u8]) {
        self.x86gen_inst(X86Instruction::Sub(
            Self::rsp_operand(),
            X86Operand::Immediate(func.naive_stack_vars_size() as u64),
        ));
        let mut num_pushed_params = 0;
        let mut num_fixed_params = 0;
        let mut num_floating_params = 0;
        for i in 0..func.params.len() {
            let param = func.params.at(i);
            let virt_reg = self.mir_to_x86_virt_reg(*param);
            if param.1 == ir::MIRType::Real {
                match num_floating_params {
                    0 => self.x86gen_inst(X86Instruction::Movsd(
                        X86Operand::Register(virt_reg),
                        Self::xmm0_operand(),
                    )),
                    1 => self.x86gen_inst(X86Instruction::Movsd(
                        X86Operand::Register(virt_reg),
                        Self::xmm1_operand(),
                    )),
                    2 => self.x86gen_inst(X86Instruction::Movsd(
                        X86Operand::Register(virt_reg),
                        Self::xmm2_operand(),
                    )),
                    3 => self.x86gen_inst(X86Instruction::Movsd(
                        X86Operand::Register(virt_reg),
                        Self::xmm3_operand(),
                    )),
                    4 => self.x86gen_inst(X86Instruction::Movsd(
                        X86Operand::Register(virt_reg),
                        Self::xmm4_operand(),
                    )),
                    5 => self.x86gen_inst(X86Instruction::Movsd(
                        X86Operand::Register(virt_reg),
                        Self::xmm5_operand(),
                    )),
                    6 => self.x86gen_inst(X86Instruction::Movsd(
                        X86Operand::Register(virt_reg),
                        Self::xmm6_operand(),
                    )),
                    7 => self.x86gen_inst(X86Instruction::Movsd(
                        X86Operand::Register(virt_reg),
                        Self::xmm7_operand(),
                    )),
                    _ => {
                        num_pushed_params += 1;
                        self.x86gen_inst(X86Instruction::Movsd(
                            X86Operand::Register(virt_reg),
                            X86Operand::MemoryOffsetConstant(
                                X86Register::Physical(X86PhysicalRegisterID::RSP),
                                num_pushed_params * 8,
                            ),
                        ));
                    }
                }
                num_floating_params += 1;
            } else {
                match num_fixed_params {
                    0 => self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::Register(virt_reg),
                        Self::rdi_operand(),
                    )),
                    1 => self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::Register(virt_reg),
                        Self::rsi_operand(),
                    )),
                    2 => self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::Register(virt_reg),
                        Self::rdx_operand(),
                    )),
                    3 => self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::Register(virt_reg),
                        Self::rcx_operand(),
                    )),
                    4 => self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::Register(virt_reg),
                        Self::r8_operand(),
                    )),
                    5 => self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::Register(virt_reg),
                        Self::r9_operand(),
                    )),
                    _ => {
                        num_pushed_params += 1;
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(virt_reg),
                            X86Operand::MemoryOffsetConstant(
                                X86Register::Physical(X86PhysicalRegisterID::RSP),
                                num_pushed_params * 8,
                            ),
                        ));
                    }
                }
                num_fixed_params += 1;
            }
        }
        self.x86gen_inst(X86Instruction::Jmp(label));
    }

    fn x86gen_function_epilogue(&mut self, func: &'a ir::MIRFunction<'a>) {
        self.x86gen_inst(X86Instruction::Add(
            Self::rsp_operand(),
            X86Operand::Immediate(func.naive_stack_vars_size() as u64),
        ));
    }
}
