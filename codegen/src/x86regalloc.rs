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

pub fn x86regalloc<'a>(program: &'a X86Module<'a>, bump: &'a bump::BumpAllocator) {
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

fn generate_def_use_sets<'a>(
    block: &'a X86Block<'a>,
    bump: &'a bump::BumpAllocator,
    num_virtual_registers: u32,
) -> (&'a mut bump::Bitset<'a>, &'a mut bump::Bitset<'a>) {
    let def_bitset = bump.create_bitset(num_virtual_registers as usize);
    let use_bitset = bump.create_bitset(num_virtual_registers as usize);

    for i in 0..block.insts.len() {
        let inst = block.insts.at(i);
        let pack = inst.get_virtual_register_pack();
        match pack {
            X86VirtualRegisterPack::Zero => {}
            X86VirtualRegisterPack::OneDef(id) => {
                if !use_bitset.at(id as usize) {
                    def_bitset.set(id as usize);
                }
            }
            X86VirtualRegisterPack::One(id) => {
                if !def_bitset.at(id as usize) {
                    use_bitset.set(id as usize);
                }
            }
            X86VirtualRegisterPack::TwoDef(id1, id2) => {
                if !use_bitset.at(id1 as usize) {
                    def_bitset.set(id1 as usize);
                }
                if !def_bitset.at(id2 as usize) {
                    use_bitset.set(id2 as usize);
                }
            }
            X86VirtualRegisterPack::Two(id1, id2) => {
                if !def_bitset.at(id1 as usize) {
                    use_bitset.set(id1 as usize);
                }
                if !def_bitset.at(id2 as usize) {
                    use_bitset.set(id2 as usize);
                }
            }
            X86VirtualRegisterPack::ThreeDef(id1, id2, id3) => {
                if !use_bitset.at(id1 as usize) {
                    def_bitset.set(id1 as usize);
                }
                if !def_bitset.at(id2 as usize) {
                    use_bitset.set(id2 as usize);
                }
                if !def_bitset.at(id3 as usize) {
                    use_bitset.set(id3 as usize);
                }
            }
            X86VirtualRegisterPack::Three(id1, id2, id3) => {
                if !def_bitset.at(id1 as usize) {
                    use_bitset.set(id1 as usize);
                }
                if !def_bitset.at(id2 as usize) {
                    use_bitset.set(id2 as usize);
                }
                if !def_bitset.at(id3 as usize) {
                    use_bitset.set(id3 as usize);
                }
            }
            X86VirtualRegisterPack::FourDef(id1, id2, id3, id4) => {
                if !use_bitset.at(id1 as usize) {
                    def_bitset.set(id1 as usize);
                }
                if !def_bitset.at(id2 as usize) {
                    use_bitset.set(id2 as usize);
                }
                if !def_bitset.at(id3 as usize) {
                    use_bitset.set(id3 as usize);
                }
                if !def_bitset.at(id4 as usize) {
                    use_bitset.set(id4 as usize);
                }
            }
            X86VirtualRegisterPack::Four(id1, id2, id3, id4) => {
                if !def_bitset.at(id1 as usize) {
                    use_bitset.set(id1 as usize);
                }
                if !def_bitset.at(id2 as usize) {
                    use_bitset.set(id2 as usize);
                }
                if !def_bitset.at(id3 as usize) {
                    use_bitset.set(id3 as usize);
                }
                if !def_bitset.at(id4 as usize) {
                    use_bitset.set(id4 as usize);
                }
            }
        }
    }

    (def_bitset, use_bitset)
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
    let found_bitset = bump.create_bitset(function.len());
    let post_order_function = post_order_traversal(
        0,
        function.at(0).id,
        found_bitset,
        function,
        bump.create_list(),
    );
    let base_block_id = function.at(0).id;
    println!("{}", core::str::from_utf8(function.at(0).label).unwrap());
    println!(
        "Here's the function in post-order: {:?}",
        post_order_function
    );

    let def_sets = unsafe { bump.alloc_slice_raw(function.len()) };
    let use_sets = unsafe { bump.alloc_slice_raw(function.len()) };
    let in_sets = unsafe { bump.alloc_slice_raw(function.len()) };
    let out_sets = unsafe { bump.alloc_slice_raw(function.len()) };

    for i in 0..function.len() {
        let block = function.at(i);
        let def_use_sets = generate_def_use_sets(block, bump, num_virtual_registers);
        def_sets[i] = def_use_sets.0;
        use_sets[i] = def_use_sets.1;
        in_sets[i] = bump.create_bitset(num_virtual_registers as usize);
        out_sets[i] = bump.create_bitset(num_virtual_registers as usize);
        out_sets[i].copy(use_sets[i]);
    }

    let scratch_bitset = bump.create_bitset(num_virtual_registers as usize);
    let mut change = true;
    while change {
        change = false;
        for i in 0..function.len() {
            let i = *post_order_function.0.at(i) as usize;
            out_sets[i].clear();
            let successors = function.at(i).successors;
            match successors {
                X86BlockSuccessors::Returns => {}
                X86BlockSuccessors::Jumps(si) => {
                    out_sets[i].or(in_sets[(si - base_block_id) as usize]);
                }
                X86BlockSuccessors::Branches(si1, si2) => {
                    out_sets[i].or(in_sets[(si1 - base_block_id) as usize]);
                    out_sets[i].or(in_sets[(si2 - base_block_id) as usize]);
                }
            }
            scratch_bitset.copy(out_sets[i]);
            scratch_bitset.sub(def_sets[i]);
            scratch_bitset.or(use_sets[i]);
            if scratch_bitset != in_sets[i] {
                change = true;
            }
            in_sets[i].copy(scratch_bitset);
        }
    }

    for i in 0..function.len() {
        let block = function.at(i);
        println!("{}", core::str::from_utf8(block.label).unwrap());
        println!("In: {:?}", in_sets[i as usize]);
        println!("Out: {:?}", out_sets[i as usize]);
    }
}
