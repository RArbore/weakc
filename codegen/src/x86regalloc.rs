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

#[derive(Debug, Clone, Copy, PartialEq)]
enum X86Color {
    Register(X86PhysicalRegisterID),
    StackSlot(u64),
}

fn colors_interfere(color1: &X86Color, color2: &X86Color) -> bool {
    match (color1, color2) {
        (X86Color::Register(id1), X86Color::Register(id2)) => {
            x86_physical_registers_overlap(*id1, *id2)
        }
        (X86Color::StackSlot(slot1), X86Color::StackSlot(slot2)) => slot1 == slot2,
        _ => false,
    }
}

pub fn x86regnorm<'a>(program: X86Module<'a>, bump: &'a bump::BumpAllocator) -> X86Module<'a> {
    let scratch_bitset = bump.create_bitset(program.pre_norm_num_virtual_registers as usize);
    let scratch_scan =
        unsafe { bump.alloc_slice_raw(program.pre_norm_num_virtual_registers as usize) };
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
        for i in 0..program.pre_norm_num_virtual_registers as usize {
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

pub fn x86regalloc<'a>(program: &'a X86Module<'a>, bump: &'a bump::BumpAllocator) -> X86Module<'a> {
    let colorings = bump.create_list();
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
        let vid_types = get_vid_types(func_blocks, bump, program.func_entries.at(i).1);
        let graph =
            build_interference_graph(func_blocks, bump, program.func_entries.at(i).1, vid_types);
        _write_dot_interference_graph(
            program
                .blocks
                .at(program.func_entries.at(i).0 as usize)
                .label,
            graph,
            program.func_entries.at(i).1,
        );
        let coloring =
            color_interference_graph(graph, bump, program.func_entries.at(i).1, vid_types);
        println!("{:?}", coloring);
        colorings.push(coloring);
    }
    color_x86_module(program, colorings, bump)
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
    vid_types: &'a [X86VirtualRegisterType],
) -> &'a mut bump::Bitset<'a> {
    let same_register_space = |vid1: X86VirtualRegisterID, vid2: X86VirtualRegisterID| -> bool {
        match (vid_types[vid1 as usize], vid_types[vid2 as usize]) {
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

fn color_interference_graph<'a>(
    graph: &'a bump::Bitset<'a>,
    bump: &'a bump::BumpAllocator,
    num_virtual_registers: u32,
    vid_types: &'a [X86VirtualRegisterType],
) -> &'a [X86Color] {
    let colors: &mut [X86Color] = unsafe { bump.alloc_slice_raw(num_virtual_registers as usize) };
    let removed: &mut [bool] = bump.alloc_slice_filled(&false, num_virtual_registers as usize);

    let node_stack = unsafe { bump.alloc_slice_raw(num_virtual_registers as usize) };
    for head in 0..num_virtual_registers {
        let mut selection: i64 = -1;
        for vid in 0..num_virtual_registers {
            if !removed[vid as usize] {
                let mut degree = 0;
                for other_vid in 0..num_virtual_registers {
                    degree += (!removed[other_vid as usize]
                        && graph.at((vid * num_virtual_registers + other_vid) as usize))
                        as u32;
                }
                if degree < vid_types[vid as usize].num_units() {
                    selection = vid as i64;
                    break;
                } else if selection == -1 {
                    selection = vid as i64;
                }
            }
        }
        node_stack[head as usize] = selection as u32;
        removed[selection as usize] = true;
    }

    let mut stack_slot_counter = 0;
    'pop: for head in (0..num_virtual_registers).rev() {
        let vid = node_stack[head as usize];
        removed[vid as usize] = false;
        'color: for pid in reg_order_for_type(vid_types[vid as usize]) {
            let potential_color = X86Color::Register(*pid);
            for other_vid in 0..num_virtual_registers {
                if vid != other_vid
                    && !removed[other_vid as usize]
                    && graph.at((vid * num_virtual_registers + other_vid) as usize)
                    && colors_interfere(&potential_color, &colors[other_vid as usize])
                {
                    continue 'color;
                }
            }
            colors[vid as usize] = potential_color;
            continue 'pop;
        }
        colors[vid as usize] = X86Color::StackSlot(stack_slot_counter);
        stack_slot_counter += 1;
    }

    colors
}

const FIXED32_REG_ORDER: [X86PhysicalRegisterID; 16] = [
    X86PhysicalRegisterID::EAX,
    X86PhysicalRegisterID::EBX,
    X86PhysicalRegisterID::ECX,
    X86PhysicalRegisterID::EDX,
    X86PhysicalRegisterID::ESI,
    X86PhysicalRegisterID::EDI,
    X86PhysicalRegisterID::ESP,
    X86PhysicalRegisterID::EBP,
    X86PhysicalRegisterID::R8D,
    X86PhysicalRegisterID::R9D,
    X86PhysicalRegisterID::R10D,
    X86PhysicalRegisterID::R11D,
    X86PhysicalRegisterID::R12D,
    X86PhysicalRegisterID::R13D,
    X86PhysicalRegisterID::R14D,
    X86PhysicalRegisterID::R15D,
];

const FIXED64_REG_ORDER: [X86PhysicalRegisterID; 16] = [
    X86PhysicalRegisterID::RAX,
    X86PhysicalRegisterID::RBX,
    X86PhysicalRegisterID::RCX,
    X86PhysicalRegisterID::RDX,
    X86PhysicalRegisterID::RSI,
    X86PhysicalRegisterID::RDI,
    X86PhysicalRegisterID::RSP,
    X86PhysicalRegisterID::RBP,
    X86PhysicalRegisterID::R8,
    X86PhysicalRegisterID::R9,
    X86PhysicalRegisterID::R10,
    X86PhysicalRegisterID::R11,
    X86PhysicalRegisterID::R12,
    X86PhysicalRegisterID::R13,
    X86PhysicalRegisterID::R14,
    X86PhysicalRegisterID::R15,
];

const FLOAT64_REG_ORDER: [X86PhysicalRegisterID; 16] = [
    X86PhysicalRegisterID::XMM0,
    X86PhysicalRegisterID::XMM1,
    X86PhysicalRegisterID::XMM2,
    X86PhysicalRegisterID::XMM3,
    X86PhysicalRegisterID::XMM4,
    X86PhysicalRegisterID::XMM5,
    X86PhysicalRegisterID::XMM6,
    X86PhysicalRegisterID::XMM7,
    X86PhysicalRegisterID::XMM8,
    X86PhysicalRegisterID::XMM9,
    X86PhysicalRegisterID::XMM10,
    X86PhysicalRegisterID::XMM11,
    X86PhysicalRegisterID::XMM12,
    X86PhysicalRegisterID::XMM13,
    X86PhysicalRegisterID::XMM14,
    X86PhysicalRegisterID::XMM15,
];

fn reg_order_for_type(ty: X86VirtualRegisterType) -> &'static [X86PhysicalRegisterID] {
    match ty {
        X86VirtualRegisterType::Fixed32 => &FIXED32_REG_ORDER,
        X86VirtualRegisterType::Fixed64 => &FIXED64_REG_ORDER,
        X86VirtualRegisterType::Float64 => &FLOAT64_REG_ORDER,
    }
}

fn color_x86_module<'a>(
    program: &'a X86Module<'a>,
    colorings: &'a bump::List<'a, &'a [X86Color]>,
    bump: &'a bump::BumpAllocator,
) -> X86Module<'a> {
    assert_eq!(program.func_entries.len(), colorings.len());
    let mut colored_program = X86Module {
        func_entries: bump.create_list(),
        blocks: bump.create_list(),
        strings: bump.create_list(),
        floats: bump.create_list(),
        pre_norm_num_virtual_registers: program.pre_norm_num_virtual_registers,
    };
    for i in 0..program.func_entries.len() {
        colored_program
            .func_entries
            .push(*program.func_entries.at(i));
    }
    for i in 0..program.strings.len() {
        colored_program.strings.push(program.strings.at(i));
    }
    for i in 0..program.floats.len() {
        colored_program.floats.push(*program.floats.at(i));
    }
    for i in 0..program.func_entries.len() {
        let num_blocks = program.blocks.len() as u32;
        let entry_block = program.func_entries.at(i).0;
        let next_entry_block = if i == program.func_entries.len() - 1 {
            num_blocks
        } else {
            program.func_entries.at(i + 1).0
        };
        for j in entry_block..next_entry_block {
            colored_program = color_x86_block(program, colorings.at(i), j, colored_program, bump);
        }
    }
    colored_program
}

macro_rules! color_inst_arm {
    ($i:expr, $a:expr, $b:expr) => {{
        $b.blocks.at_mut($a as usize).insts.push($i);
    }};
    ($i:expr, $o1:expr, $a:expr, $b:expr, $c:expr) => {{
        let (new_op, moved_colored_program) = color_x86_operand($o1, $a, $b, $c, false);
        $c = moved_colored_program;
        $c.blocks.at_mut($b as usize).insts.push($i(new_op));
    }};
    ($i:expr, $o1:expr, $o2:expr, $a:expr, $b:expr, $c:expr) => {{
        let (new_op1, moved_colored_program) = color_x86_operand($o1, $a, $b, $c, false);
        let (new_op2, moved_colored_program) =
            color_x86_operand($o2, $a, $b, moved_colored_program, false);
        $c = moved_colored_program;
        $c.blocks
            .at_mut($b as usize)
            .insts
            .push($i(new_op1, new_op2));
    }};
}

macro_rules! color_inst_arm_byte_regs {
    ($i:expr, $o1:expr, $a:expr, $b:expr, $c:expr) => {{
        let (new_op, moved_colored_program) = color_x86_operand($o1, $a, $b, $c, true);
        $c = moved_colored_program;
        $c.blocks.at_mut($b as usize).insts.push($i(new_op));
    }};
    ($i:expr, $o1:expr, $o2:expr, $a:expr, $b:expr, $c:expr) => {{
        let (new_op1, moved_colored_program) = color_x86_operand($o1, $a, $b, $c, true);
        let (new_op2, moved_colored_program) =
            color_x86_operand($o2, $a, $b, moved_colored_program, true);
        $c = moved_colored_program;
        $c.blocks
            .at_mut($b as usize)
            .insts
            .push($i(new_op1, new_op2));
    }};
}

fn color_x86_block<'a>(
    program: &'a X86Module<'a>,
    coloring: &'a [X86Color],
    block_id: X86BlockID,
    mut colored_program: X86Module<'a>,
    bump: &'a bump::BumpAllocator,
) -> X86Module<'a> {
    assert_eq!(colored_program.blocks.len() as u32, block_id);
    let block = program.blocks.at(block_id as usize);
    colored_program.blocks.push(X86Block {
        label: block.label,
        insts: bump.create_list(),
        id: block.id,
        successors: block.successors,
    });
    for i in 0..block.insts.len() {
        match block.insts.at(i) {
            X86Instruction::Ret => colored_program
                .blocks
                .at_mut(block_id as usize)
                .insts
                .push(X86Instruction::Ret),
            X86Instruction::Nop => colored_program
                .blocks
                .at_mut(block_id as usize)
                .insts
                .push(X86Instruction::Nop),
            X86Instruction::Jmp(label) => colored_program
                .blocks
                .at_mut(block_id as usize)
                .insts
                .push(X86Instruction::Jmp(label)),
            X86Instruction::Jnz(label) => colored_program
                .blocks
                .at_mut(block_id as usize)
                .insts
                .push(X86Instruction::Jnz(label)),
            X86Instruction::Call(label) => colored_program
                .blocks
                .at_mut(block_id as usize)
                .insts
                .push(X86Instruction::Call(label)),
            X86Instruction::Inc(op) => {
                color_inst_arm!(X86Instruction::Inc, op, coloring, block_id, colored_program)
            }
            X86Instruction::Dec(op) => {
                color_inst_arm!(X86Instruction::Dec, op, coloring, block_id, colored_program)
            }
            X86Instruction::Neg(op) => {
                color_inst_arm!(X86Instruction::Neg, op, coloring, block_id, colored_program)
            }
            X86Instruction::Not(op) => {
                color_inst_arm!(X86Instruction::Not, op, coloring, block_id, colored_program)
            }
            X86Instruction::Push(op) => color_inst_arm!(
                X86Instruction::Push,
                op,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Pop(op) => {
                color_inst_arm!(X86Instruction::Pop, op, coloring, block_id, colored_program)
            }
            X86Instruction::Seta(op) => color_inst_arm_byte_regs!(
                X86Instruction::Seta,
                op,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Setae(op) => color_inst_arm_byte_regs!(
                X86Instruction::Setae,
                op,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Sete(op) => color_inst_arm_byte_regs!(
                X86Instruction::Sete,
                op,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Setne(op) => color_inst_arm_byte_regs!(
                X86Instruction::Setne,
                op,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Lea(op1, op2) => color_inst_arm!(
                X86Instruction::Lea,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Add(op1, op2) => color_inst_arm!(
                X86Instruction::Add,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Addsd(op1, op2) => color_inst_arm!(
                X86Instruction::Addsd,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Sub(op1, op2) => color_inst_arm!(
                X86Instruction::Sub,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Subsd(op1, op2) => color_inst_arm!(
                X86Instruction::Subsd,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Imul(op1, op2) => color_inst_arm!(
                X86Instruction::Imul,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Mulsd(op1, op2) => color_inst_arm!(
                X86Instruction::Mulsd,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Divsd(op1, op2) => color_inst_arm!(
                X86Instruction::Divsd,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Xor(op1, op2) => color_inst_arm!(
                X86Instruction::Xor,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Xorps(op1, op2) => color_inst_arm!(
                X86Instruction::Xorps,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Or(op1, op2) => color_inst_arm!(
                X86Instruction::Or,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::And(op1, op2) => color_inst_arm!(
                X86Instruction::And,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Mov(op1, op2) => color_inst_arm!(
                X86Instruction::Mov,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Movsd(op1, op2) => color_inst_arm!(
                X86Instruction::Movsd,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Movsxd(op1, op2) => color_inst_arm!(
                X86Instruction::Movsxd,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Cvttsd2si(op1, op2) => color_inst_arm!(
                X86Instruction::Cvttsd2si,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Cmp(op1, op2) => color_inst_arm!(
                X86Instruction::Cmp,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Comisd(op1, op2) => color_inst_arm!(
                X86Instruction::Comisd,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
            X86Instruction::Test(op1, op2) => color_inst_arm_byte_regs!(
                X86Instruction::Test,
                op1,
                op2,
                coloring,
                block_id,
                colored_program
            ),
        }
    }
    colored_program
}

fn color_x86_operand<'a>(
    op: &'a X86Operand<'a>,
    coloring: &'a [X86Color],
    block_id: X86BlockID,
    colored_program: X86Module<'a>,
    convert_to_byte_regs: bool,
) -> (X86Operand<'a>, X86Module<'a>) {
    let colored_block = colored_program.blocks.at_mut(block_id as usize);
    let conv = if convert_to_byte_regs {
        |reg| get_lowest_byte(reg)
    } else {
        |reg| reg
    };
    let new_op = match op {
        X86Operand::Register(X86Register::Virtual(vid, _)) => match coloring[*vid as usize] {
            X86Color::Register(pid) => X86Operand::Register(X86Register::Physical(conv(pid))),
            X86Color::StackSlot(slot) => X86Operand::MemoryOffsetConstant(
                X86Register::Physical(conv(X86PhysicalRegisterID::RSP)),
                (slot * 8) as usize,
            ),
        },
        X86Operand::Register(X86Register::Physical(pid)) => {
            X86Operand::Register(X86Register::Physical(conv(*pid)))
        }
        X86Operand::MemoryOffsetConstant(X86Register::Virtual(vid, _), constant) => {
            match coloring[*vid as usize] {
                X86Color::Register(pid) => {
                    X86Operand::MemoryOffsetConstant(X86Register::Physical(conv(pid)), *constant)
                }
                X86Color::StackSlot(slot) => {
                    colored_block.insts.push(X86Instruction::Mov(
                        X86Operand::Register(X86Register::Physical(conv(
                            X86PhysicalRegisterID::R15,
                        ))),
                        X86Operand::MemoryOffsetConstant(
                            X86Register::Physical(conv(X86PhysicalRegisterID::RSP)),
                            (slot * 8) as usize,
                        ),
                    ));
                    X86Operand::MemoryOffsetConstant(
                        X86Register::Physical(conv(X86PhysicalRegisterID::R15)),
                        *constant,
                    )
                }
            }
        }
        X86Operand::MemoryOffsetConstant(X86Register::Physical(pid), constant) => {
            X86Operand::MemoryOffsetConstant(X86Register::Physical(conv(*pid)), *constant)
        }
        X86Operand::MemoryLabel(X86Register::Virtual(vid, _), label) => {
            match coloring[*vid as usize] {
                X86Color::Register(pid) => {
                    X86Operand::MemoryLabel(X86Register::Physical(conv(pid)), label)
                }
                X86Color::StackSlot(slot) => {
                    colored_block.insts.push(X86Instruction::Mov(
                        X86Operand::Register(X86Register::Physical(conv(
                            X86PhysicalRegisterID::R15,
                        ))),
                        X86Operand::MemoryOffsetConstant(
                            X86Register::Physical(conv(X86PhysicalRegisterID::RSP)),
                            (slot * 8) as usize,
                        ),
                    ));
                    X86Operand::MemoryLabel(
                        X86Register::Physical(conv(X86PhysicalRegisterID::R15)),
                        label,
                    )
                }
            }
        }
        X86Operand::MemoryLabel(X86Register::Physical(pid), label) => {
            X86Operand::MemoryLabel(X86Register::Physical(conv(*pid)), label)
        }
        X86Operand::Immediate(imm) => X86Operand::Immediate(*imm),
    };
    (new_op, colored_program)
}

use std::fs::write;

fn _write_dot_interference_graph<'a>(
    label: &'a [u8],
    graph: &'a bump::Bitset<'a>,
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
