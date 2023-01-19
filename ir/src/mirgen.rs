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
use bump::bump_list;

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
            let (id, ty) = func.params.at(i);
            if let Some(ty) = convert_type(*ty) {
                mir_func.params.push((*id, ty));
            }
        }

        self.curr_func = self.module.funcs.len() as MIRFunctionID;
        self.curr_block = 0;
        self.module.funcs.push(mir_func);

        for i in 0..func.blocks.len() {
            self.mirgen_block(func.blocks.at(i));
        }
    }

    fn mirgen_block(&mut self, block: &'a HIRBasicBlock<'a>) {}
}
