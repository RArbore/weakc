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

use crate::*;

pub fn x86regalloc<'a>(program: &'a X86Module<'a>, bump: &'a bump::BumpAllocator) -> X86Module<'a> {
    let num_blocks_in_func = |i| {
        if i < program.func_entries.len() - 1 {
            program.func_entries.at(i + 1) - program.func_entries.at(i)
        } else {
            program.blocks.len() as u32 - program.func_entries.at(i)
        }
    };
    for i in 0..program.func_entries.len() {
        let num_blocks = num_blocks_in_func(i);
        let func_blocks = bump.create_list();
        for j in 0..num_blocks {
            func_blocks.push(
                program
                    .blocks
                    .at(*program.func_entries.at(i) as usize + j as usize),
            );
        }
        build_interference_graph(func_blocks, bump);
    }

    todo!()
}

fn post_order_traversal<'a>(
    curr_block: &'a X86Block<'a>,
    post_order_function: &'a bump::List<'a, u32>,
) -> &'a bump::List<'a, u32> {
    post_order_function
}

fn build_interference_graph<'a>(
    function: &'a mut bump::List<'a, &'a X86Block<'a>>,
    bump: &'a bump::BumpAllocator,
) {
    println!("Here's a function: {:?}", function);
    println!("");
    let post_order_function = post_order_traversal(function.at(0), bump.create_list());
    println!(
        "Here's the function in post-order: {:?}",
        post_order_function
    );
    println!("");
}
