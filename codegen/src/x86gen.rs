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
                funcs: bump.create_list(),
            },
        };
        context
    }

    fn x86gen_program(&mut self, program: &'a ir::MIRModule<'a>) {}
}
