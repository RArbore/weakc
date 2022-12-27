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

use std::collections::HashMap;

use crate::*;
use parse::ASTBinaryOp;
use parse::ASTUnaryOp;
use semant::Type;
use semant::TypedASTExpr;
use semant::TypedASTStmt;

pub fn irgen<'a>(
    program: &'a bump::List<'a, TypedASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> IRModule {
    let mut context = IRGenContext::new(bump);
    context.irgen_program(program);
    context.module
}

struct IRGenContext<'a> {
    module: IRModule<'a>,
    curr_func: IRFunctionID,
    curr_block: IRBasicBlockID,
    curr_vars: HashMap<&'a [u8], IRRegister>,
    curr_num_regs: IRRegisterID,
    funcs_to_gen: HashMap<
        &'a [u8],
        (
            &'a [u8],
            &'a bump::List<'a, &'a [u8]>,
            &'a bump::List<'a, Type>,
            &'a TypedASTStmt<'a>,
            Type,
        ),
    >,
    bump: &'a bump::BumpAllocator,
}

impl<'a> IRGenContext<'a> {
    fn new(bump: &'a bump::BumpAllocator) -> Self {
        let block = IRBasicBlock {
            insts: bump.create_list(),
        };
        let func = IRFunction {
            name: b"@main",
            params: bump.create_list(),
            ret_type: IRType::Nil,
            blocks: bump.create_list(),
        };
        func.blocks.push(block);
        let context = IRGenContext {
            module: IRModule {
                funcs: bump.create_list(),
            },
            curr_func: 0,
            curr_block: 0,
            curr_vars: HashMap::new(),
            curr_num_regs: 0,
            funcs_to_gen: HashMap::new(),
            bump,
        };
        context.module.funcs.push(func);
        context
    }

    fn get_curr_func(&self) -> &IRFunction<'a> {
        return self.module.funcs.at(self.curr_func as usize);
    }

    fn get_curr_func_mut(&mut self) -> &mut IRFunction<'a> {
        return self.module.funcs.at_mut(self.curr_func as usize);
    }

    fn get_curr_block(&self) -> &IRBasicBlock<'a> {
        return self
            .module
            .funcs
            .at(self.curr_func as usize)
            .blocks
            .at(self.curr_block as usize);
    }

    fn get_curr_block_mut(&mut self) -> &mut IRBasicBlock<'a> {
        return self
            .module
            .funcs
            .at_mut(self.curr_func as usize)
            .blocks
            .at_mut(self.curr_block as usize);
    }

    fn fresh_reg(&mut self, ty: IRType) -> IRRegister {
        let id = self.curr_num_regs;
        self.curr_num_regs += 1;
        (id, ty)
    }

    fn add_inst(&mut self, inst: IRInstruction<'a>) {
        self.get_curr_block_mut().insts.push(inst);
    }

    fn irgen_program(&mut self, program: &'a bump::List<'a, TypedASTStmt<'a>>) {
        for i in 0..program.len() {
            self.irgen_stmt(program.at(i));
        }
    }

    fn irgen_stmt(&mut self, stmt: &'a TypedASTStmt<'a>) {}

    fn irgen_expr(&mut self, expr: &'a TypedASTExpr<'a>) -> IRRegister {
        match expr {
            TypedASTExpr::Nil => {
                let reg = self.fresh_reg(IRType::Nil);
                self.add_inst(IRInstruction::Immediate(reg, IRValue::Nil));
                reg
            }
            _ => panic!(),
        }
    }
}
