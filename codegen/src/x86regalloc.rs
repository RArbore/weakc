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

pub fn x86regnorm<'a>(program: X86Module<'a>, bump: &'a bump::BumpAllocator) -> X86Module<'a> {
    let scratch_bitset = bump.create_bitset(program.num_virtual_registers as usize);
    let scratch_scan = unsafe { bump.alloc_slice_raw(program.num_virtual_registers as usize) };
    for i in 0..program.func_entries.len() {
        scratch_bitset.clear();
        let num_blocks = if i < program.func_entries.len() - 1 {
            program.func_entries.at(i + 1).0 - program.func_entries.at(i).0
        } else {
            program.blocks.len() as u32 - program.func_entries.at(i).0
        };
        for b in 0..num_blocks {
            let block = program
                .blocks
                .at((program.func_entries.at(i).0 + b) as usize);
            for i in 0..block.insts.len() {
                let inst = block.insts.at(i);
                let virtual_pack = inst.get_virtual_register_pack();
                match virtual_pack {
                    X86VirtualRegisterPack::Zero => {}
                    X86VirtualRegisterPack::OneDef(id1) | X86VirtualRegisterPack::One(id1) => {
                        scratch_bitset.set(id1 as usize);
                    }
                    X86VirtualRegisterPack::TwoDef(id1, id2)
                    | X86VirtualRegisterPack::Two(id1, id2) => {
                        scratch_bitset.set(id1 as usize);
                        scratch_bitset.set(id2 as usize);
                    }
                    X86VirtualRegisterPack::ThreeDef(id1, id2, id3)
                    | X86VirtualRegisterPack::Three(id1, id2, id3) => {
                        scratch_bitset.set(id1 as usize);
                        scratch_bitset.set(id2 as usize);
                        scratch_bitset.set(id3 as usize);
                    }
                    X86VirtualRegisterPack::FourDef(id1, id2, id3, id4)
                    | X86VirtualRegisterPack::Four(id1, id2, id3, id4) => {
                        scratch_bitset.set(id1 as usize);
                        scratch_bitset.set(id2 as usize);
                        scratch_bitset.set(id3 as usize);
                        scratch_bitset.set(id4 as usize);
                    }
                }
            }
        }
        let mut cur_vid = 0;
        for i in 0..program.num_virtual_registers as usize {
            scratch_scan[i] = cur_vid;
            if scratch_bitset.at(i) {
                cur_vid += 1;
            }
        }
        for b in 0..num_blocks {
            let block = program
                .blocks
                .at_mut((program.func_entries.at(i).0 + b) as usize);
            for i in 0..block.insts.len() {
                let inst = block.insts.at_mut(i);
                inst.map_virtual_registers(scratch_scan);
            }
        }
        program.func_entries.at_mut(i).1 = cur_vid;
    }
    program
}

pub fn x86regalloc<'a>(program: &'a X86Module<'a>, bump: &'a bump::BumpAllocator) {
    for i in 0..program.func_entries.len() {
        let num_blocks = if i < program.func_entries.len() - 1 {
            program.func_entries.at(i + 1).0 - program.func_entries.at(i).0
        } else {
            program.blocks.len() as u32 - program.func_entries.at(i).0
        };
        let func_blocks = bump.create_list();
        for j in 0..num_blocks {
            func_blocks.push(
                program
                    .blocks
                    .at(program.func_entries.at(i).0 as usize + j as usize),
            );
        }
        let graph = build_interference_graph(func_blocks, bump, program.func_entries.at(i).1);
        _write_dot_interference_graph(
            program
                .blocks
                .at(program.func_entries.at(i).0 as usize)
                .label,
            graph,
            program.func_entries.at(i).1,
        );
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

fn build_interference_graph<'a>(
    function: &'a bump::List<'a, &'a X86Block<'a>>,
    bump: &'a bump::BumpAllocator,
    num_virtual_registers: u32,
) -> &'a mut bump::Bitset<'a> {
    let vid_types = get_vid_types(function, bump, num_virtual_registers);
    let same_register_space = |vid1: X86VirtualRegisterID, vid2: X86VirtualRegisterID| -> bool {
        match (vid_types.at(vid1 as usize), vid_types.at(vid2 as usize)) {
            (X86VirtualRegisterType::Fixed32, X86VirtualRegisterType::Fixed32)
            | (X86VirtualRegisterType::Fixed32, X86VirtualRegisterType::Fixed64)
            | (X86VirtualRegisterType::Fixed64, X86VirtualRegisterType::Fixed32)
            | (X86VirtualRegisterType::Fixed64, X86VirtualRegisterType::Fixed64)
            | (X86VirtualRegisterType::Float64, X86VirtualRegisterType::Float64) => true,
            _ => false,
        }
    };

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

    let graph_bitset =
        bump.create_bitset(num_virtual_registers as usize * num_virtual_registers as usize);
    for i in 0..function.len() {
        let block = function.at(i);
        scratch_bitset.copy(out_sets[i]);
        for j in (0..block.insts.len()).rev() {
            let inst = block.insts.at(j);
            let pack = inst.get_virtual_register_pack();
            let def = match pack {
                X86VirtualRegisterPack::Zero => None,
                X86VirtualRegisterPack::OneDef(id) => Some(id),
                X86VirtualRegisterPack::One(_) => None,
                X86VirtualRegisterPack::TwoDef(id, _) => Some(id),
                X86VirtualRegisterPack::Two(_, _) => None,
                X86VirtualRegisterPack::ThreeDef(id, _, _) => Some(id),
                X86VirtualRegisterPack::Three(_, _, _) => None,
                X86VirtualRegisterPack::FourDef(id, _, _, _) => Some(id),
                X86VirtualRegisterPack::Four(_, _, _, _) => None,
            };
            if let Some(def) = def {
                for vid in 0..num_virtual_registers as usize {
                    if scratch_bitset.at(vid)
                        && same_register_space(vid as X86VirtualRegisterID, def)
                    {
                        graph_bitset.set(vid * num_virtual_registers as usize + def as usize);
                        graph_bitset.set(def as usize * num_virtual_registers as usize + vid);
                    }
                }
                scratch_bitset.unset(def as usize);
            }
            match pack {
                X86VirtualRegisterPack::Zero => {}
                X86VirtualRegisterPack::OneDef(_) => {}
                X86VirtualRegisterPack::One(id1) => {
                    scratch_bitset.set(id1 as usize);
                }
                X86VirtualRegisterPack::TwoDef(_, id1) => {
                    scratch_bitset.set(id1 as usize);
                }
                X86VirtualRegisterPack::Two(id1, id2) => {
                    scratch_bitset.set(id1 as usize);
                    scratch_bitset.set(id2 as usize);
                }
                X86VirtualRegisterPack::ThreeDef(_, id1, id2) => {
                    scratch_bitset.set(id1 as usize);
                    scratch_bitset.set(id2 as usize);
                }
                X86VirtualRegisterPack::Three(id1, id2, id3) => {
                    scratch_bitset.set(id1 as usize);
                    scratch_bitset.set(id2 as usize);
                    scratch_bitset.set(id3 as usize);
                }
                X86VirtualRegisterPack::FourDef(_, id1, id2, id3) => {
                    scratch_bitset.set(id1 as usize);
                    scratch_bitset.set(id2 as usize);
                    scratch_bitset.set(id3 as usize);
                }
                X86VirtualRegisterPack::Four(id1, id2, id3, id4) => {
                    scratch_bitset.set(id1 as usize);
                    scratch_bitset.set(id2 as usize);
                    scratch_bitset.set(id3 as usize);
                    scratch_bitset.set(id4 as usize);
                }
            };
        }
    }
    graph_bitset
}

use std::fs::write;

fn _write_dot_interference_graph<'a>(
    label: &'a [u8],
    graph: &'a mut bump::Bitset<'a>,
    num_virtual_registers: u32,
) {
    let mut content = format!(
        "graph \"{}\" {{\nlabel=\"{}\";\n",
        core::str::from_utf8(label).unwrap(),
        core::str::from_utf8(label).unwrap()
    );
    for i in 0..num_virtual_registers {
        content = format!(
            "{}Node{} [shape=record, style=filled, label={}];\n",
            content, i, i
        );
    }
    for i in 0..num_virtual_registers as usize {
        for j in (i + 1)..num_virtual_registers as usize {
            if graph.at(i * num_virtual_registers as usize + j) {
                content = format!("{}Node{} -- Node{};\n", content, i, j);
            }
        }
    }
    content = format!("{}}}\n", content);
    write(
        format!(
            "debug_interference_graph_{}.dot",
            core::str::from_utf8(label).unwrap()
        ),
        content,
    )
    .unwrap();
}
