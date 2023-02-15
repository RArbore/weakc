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

    fn x86gen_program(&mut self, program: &'a ir::MIRModule<'a>) {
        for i in 0..program.strings.len() {
            self.module.strings.push(program.strings.at(i));
        }
        for i in 0..program.funcs.len() {
            self.x86gen_function(program.funcs.at(i));
        }
    }

    fn x86gen_function(&mut self, func: &'a ir::MIRFunction<'a>) {
        self.x86gen_function_prologue(func);

        self.x86gen_function_epilogue(func);
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
