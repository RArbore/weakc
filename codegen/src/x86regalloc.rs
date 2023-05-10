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
        build_interference_graph(func_blocks, bump, program.num_virtual_registers);
    }

    todo!()
}

fn post_order_traversal<'a>(
    curr_block: u32,
    function_base_id: X86BlockID,
    found_bitset: &'a mut bump::Bitset<'a>,
    function: &'a bump::List<'a, &'a X86Block<'a>>,
    post_order_function: &'a mut bump::List<'a, u32>,
) -> (&'a mut bump::List<'a, u32>, &'a mut bump::Bitset<'a>) {
    if found_bitset.at(curr_block as usize) {
        return (post_order_function, found_bitset);
    }
    found_bitset.set(curr_block as usize);

    let (post_order_function, found_bitset) = match function.at(curr_block as usize).successors {
        X86BlockSuccessors::Returns => (post_order_function, found_bitset),
        X86BlockSuccessors::Jumps(id) => post_order_traversal(
            id - function_base_id,
            function_base_id,
            found_bitset,
            function,
            post_order_function,
        ),
        X86BlockSuccessors::Branches(id1, id2) => {
            let (left_post_order_function, left_found_bitset) = post_order_traversal(
                id1 - function_base_id,
                function_base_id,
                found_bitset,
                function,
                post_order_function,
            );
            post_order_traversal(
                id2 - function_base_id,
                function_base_id,
                left_found_bitset,
                function,
                left_post_order_function,
            )
        }
    };

    post_order_function.push(curr_block);
    (post_order_function, found_bitset)
}

struct X86ProgramPoint {
    block: X86BlockID,
    inst: usize,
}

fn build_interference_graph<'a>(
    function: &'a bump::List<'a, &'a X86Block<'a>>,
    bump: &'a bump::BumpAllocator,
    num_virtual_registers: u32,
) {
    println!("Here's a function: {:?}", function);
    println!("");
    let found_bitset = bump.create_bitset(function.len());
    let post_order_function = post_order_traversal(
        0,
        function.at(0).id,
        found_bitset,
        function,
        bump.create_list(),
    );
    println!(
        "Here's the function in post-order: {:?}",
        post_order_function
    );
    println!("");
    println!(
        "There are {} virtual registers present in the program.",
        num_virtual_registers
    );
}
