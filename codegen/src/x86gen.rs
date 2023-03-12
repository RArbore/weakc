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
            },
            curr_block: 0,
            curr_func: None,
            bump: bump,
        };
        context
    }

    fn rsp_operand() -> X86Operand {
        X86Operand::Register(X86Register::Physical(X86PhysicalRegisterID::RSP))
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
            let x86inst = match block.insts.at(i) {
                ir::MIRInstruction::Return(None) => {
                    self.x86gen_function_epilogue(self.curr_func.unwrap());
                    X86Instruction::Ret
                }
                ir::MIRInstruction::Call(_, (_, label), _) => X86Instruction::Call(&label[1..]),
                _ => panic!(),
            };
            self.x86gen_inst(x86inst);
        }
    }

    fn x86gen_program(&mut self, program: &'a ir::MIRModule<'a>) {
        for i in 0..program.strings.len() {
            self.module.strings.push(program.strings.at(i));
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

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn x86gen_simple() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f abc() {} abc();", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = semant::typecheck_program(ast, &bump).unwrap();
        let hir_program = ir::hirgen(&typed_program, &bump);
        let mir_program = ir::mirgen(&hir_program, &bump);
        let x86_program = x86gen(&mir_program, &bump);
        assert_eq!(
            x86_program,
            X86Module {
                blocks: bump::bump_list!(
                    bump,
                    X86Block {
                        label: b"main",
                        insts: bump::bump_list!(
                            bump,
                            X86Instruction::Sub(
                                X86Operand::Register(X86Register::Physical(
                                    X86PhysicalRegisterID::RSP
                                )),
                                X86Operand::Immediate(16)
                            )
                        )
                    },
                    X86Block {
                        label: b"main.0x00000001",
                        insts: bump::bump_list!(
                            bump,
                            X86Instruction::Call(b"f_abc"),
                            X86Instruction::Add(
                                X86Operand::Register(X86Register::Physical(
                                    X86PhysicalRegisterID::RSP
                                )),
                                X86Operand::Immediate(16)
                            ),
                            X86Instruction::Ret
                        )
                    },
                    X86Block {
                        label: b"f_abc",
                        insts: bump::bump_list!(
                            bump,
                            X86Instruction::Sub(
                                X86Operand::Register(X86Register::Physical(
                                    X86PhysicalRegisterID::RSP
                                )),
                                X86Operand::Immediate(8)
                            )
                        )
                    },
                    X86Block {
                        label: b"f_abc.0x00000003",
                        insts: bump::bump_list!(
                            bump,
                            X86Instruction::Add(
                                X86Operand::Register(X86Register::Physical(
                                    X86PhysicalRegisterID::RSP
                                )),
                                X86Operand::Immediate(8)
                            ),
                            X86Instruction::Ret
                        )
                    }
                ),
                strings: bump.create_list(),
            },
        );
    }
}
