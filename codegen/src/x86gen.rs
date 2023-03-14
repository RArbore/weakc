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
extern crate ir;

use crate::*;

struct X86GenContext<'a> {
    module: X86Module<'a>,
    curr_block: X86BlockID,
    curr_func: Option<&'a ir::MIRFunction<'a>>,
    weak_float_labels: &'a mut bump::List<'a, &'a [u8]>,
    weak_string_labels: &'a mut bump::List<'a, &'a [u8]>,
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
                strings: bump.create_list(),
                blocks: bump.create_list(),
                floats: bump.create_list(),
            },
            curr_block: 0,
            curr_func: None,
            weak_float_labels: bump.create_list(),
            weak_string_labels: bump.create_list(),
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
        self.get_curr_block_mut().insts.push(inst);
    }

    fn x86gen_block(
        &mut self,
        block: &'a ir::MIRBasicBlock<'a>,
        block_id: X86BlockID,
        label: &'a [u8],
    ) {
        self.curr_block = block_id;
        self.module.blocks.push(X86Block {
            label,
            insts: self.bump.create_list(),
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
                        let float_id = self.record_float(*val);
                        self.x86gen_inst(X86Instruction::Movsd(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*reg)),
                            X86Operand::MemoryLabel(
                                X86Register::Physical(X86PhysicalRegisterID::RIP),
                                self.weak_float_labels.at(float_id),
                            ),
                        ));
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
                ir::MIRInstruction::Load(dst_reg, src_reg) => {
                    self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::Register(self.mir_to_x86_virt_reg(*dst_reg)),
                        X86Operand::MemoryOffset(self.mir_to_x86_virt_reg(*src_reg), 0),
                    ));
                }
                ir::MIRInstruction::Store(src_reg, dst_reg) => {
                    self.x86gen_inst(X86Instruction::Mov(
                        X86Operand::MemoryOffset(self.mir_to_x86_virt_reg(*dst_reg), 0),
                        X86Operand::Register(self.mir_to_x86_virt_reg(*src_reg)),
                    ));
                }
                ir::MIRInstruction::Return(ret_val) => {
                    if let Some(ret_val) = ret_val {
                        self.x86gen_inst(X86Instruction::Mov(
                            Self::rax_operand(),
                            X86Operand::Register(self.mir_to_x86_virt_reg(*ret_val)),
                        ));
                    } else {
                        self.x86gen_inst(X86Instruction::Xor(
                            Self::rax_operand(),
                            Self::rax_operand(),
                        ));
                    }
                    self.x86gen_function_epilogue(self.curr_func.unwrap());
                    self.x86gen_inst(X86Instruction::Ret);
                }
                ir::MIRInstruction::Call(ret, (_, label), args) => {
                    let mut pushed_args_size = 0;
                    for i in 0..args.len() {
                        match i {
                            0 => self.x86gen_inst(X86Instruction::Mov(
                                Self::rdi_operand(),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*args.at(i))),
                            )),
                            1 => self.x86gen_inst(X86Instruction::Mov(
                                Self::rsi_operand(),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*args.at(i))),
                            )),
                            2 => self.x86gen_inst(X86Instruction::Mov(
                                Self::rdx_operand(),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*args.at(i))),
                            )),
                            3 => self.x86gen_inst(X86Instruction::Mov(
                                Self::rcx_operand(),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*args.at(i))),
                            )),
                            4 => self.x86gen_inst(X86Instruction::Mov(
                                Self::r8_operand(),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*args.at(i))),
                            )),
                            5 => self.x86gen_inst(X86Instruction::Mov(
                                Self::r9_operand(),
                                X86Operand::Register(self.mir_to_x86_virt_reg(*args.at(i))),
                            )),
                            _ => {
                                let virt_reg = self.mir_to_x86_virt_reg(*args.at(i));
                                pushed_args_size += virt_reg.size();
                                self.x86gen_inst(X86Instruction::Push(X86Operand::Register(
                                    virt_reg,
                                )));
                            }
                        }
                    }
                    self.x86gen_inst(X86Instruction::Call(&label[1..]));
                    if pushed_args_size > 0 {
                        self.x86gen_inst(X86Instruction::Add(
                            Self::rsp_operand(),
                            X86Operand::Immediate(pushed_args_size),
                        ));
                    }
                    if let Some(ret) = ret {
                        self.x86gen_inst(X86Instruction::Mov(
                            X86Operand::Register(self.mir_to_x86_virt_reg(*ret)),
                            Self::rax_operand(),
                        ));
                    }
                }
                _ => panic!(),
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
        for i in 0..program.funcs.len() {
            self.x86gen_function(program.funcs.at(i));
        }
    }

    fn x86gen_function(&mut self, func: &'a ir::MIRFunction<'a>) {
        let func_block_id = self.module.blocks.len() as X86BlockID;
        self.curr_block = func_block_id;
        self.curr_func = Some(func);
        self.module.blocks.push(X86Block {
            label: &func.name[1..],
            insts: self.bump.create_list(),
        });
        self.x86gen_function_prologue(func);
        for i in 0..func.blocks.len() {
            let label = unsafe { self.bump.alloc_slice_raw(func.name.len() + 10) };
            for j in 1..func.name.len() {
                label[j - 1] = func.name[j];
            }
            label[func.name.len() - 1] = b'.';
            let block_id = (i + 1) as X86BlockID + func_block_id;
            write_block_id(block_id, label, func.name.len());
            self.x86gen_block(func.blocks.at(i), block_id, label);
        }
        self.curr_func = None;
    }

    fn x86gen_function_prologue(&mut self, func: &'a ir::MIRFunction<'a>) {
        self.x86gen_inst(X86Instruction::Sub(
            Self::rsp_operand(),
            X86Operand::Immediate(func.naive_stack_vars_size() as u64),
        ));
    }

    fn x86gen_function_epilogue(&mut self, func: &'a ir::MIRFunction<'a>) {
        self.x86gen_inst(X86Instruction::Add(
            Self::rsp_operand(),
            X86Operand::Immediate(func.naive_stack_vars_size() as u64),
        ));
    }
}
