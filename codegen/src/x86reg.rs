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

pub type X86VirtualRegisterID = u32;

#[derive(Debug, PartialEq)]
pub enum X86PhysicalRegisterID {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RSP,
    RBP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,

    EAX,
    EBX,
    ECX,
    EDX,
    ESI,
    EDI,
    ESP,
    EBP,
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,
    R15D,

    AX,
    BX,
    CX,
    DX,
    SI,
    DI,
    SP,
    BP,
    R8W,
    R9W,
    R10W,
    R11W,
    R12W,
    R13W,
    R14W,
    R15W,

    AL,
    BL,
    CL,
    DL,
    SIL,
    DIL,
    SPL,
    BPL,
    R8B,
    R9B,
    R10B,
    R11B,
    R12B,
    R13B,
    R14B,
    R15B,
}

pub enum X86PhysicalRegisterUsageBit {
    GeneralPurposeCallerSaved = 1,
    GeneralPurposeCalleeSaved = 2,
    FunctionParameter1 = 4,
    FunctionParameter2 = 8,
    FunctionParameter3 = 16,
    FunctionParameter4 = 32,
    FunctionParameter5 = 64,
    FunctionParameter6 = 128,
    FunctionReturnValue = 256,
    StackPointer = 512,
}

pub type X86PhysicalRegisterUsage = u32;

impl X86PhysicalRegisterID {
    fn get_pack_and_pos(&self) -> (u32, u32) {
        match self {
            X86PhysicalRegisterID::RAX => (0, 0),
            X86PhysicalRegisterID::RBX => (1, 0),
            X86PhysicalRegisterID::RCX => (2, 0),
            X86PhysicalRegisterID::RDX => (3, 0),
            X86PhysicalRegisterID::RSI => (4, 0),
            X86PhysicalRegisterID::RDI => (5, 0),
            X86PhysicalRegisterID::RSP => (6, 0),
            X86PhysicalRegisterID::RBP => (7, 0),
            X86PhysicalRegisterID::R8 => (8, 0),
            X86PhysicalRegisterID::R9 => (9, 0),
            X86PhysicalRegisterID::R10 => (10, 0),
            X86PhysicalRegisterID::R11 => (11, 0),
            X86PhysicalRegisterID::R12 => (12, 0),
            X86PhysicalRegisterID::R13 => (13, 0),
            X86PhysicalRegisterID::R14 => (14, 0),
            X86PhysicalRegisterID::R15 => (15, 0),

            X86PhysicalRegisterID::EAX => (0, 1),
            X86PhysicalRegisterID::EBX => (1, 1),
            X86PhysicalRegisterID::ECX => (2, 1),
            X86PhysicalRegisterID::EDX => (3, 1),
            X86PhysicalRegisterID::ESI => (4, 1),
            X86PhysicalRegisterID::EDI => (5, 1),
            X86PhysicalRegisterID::ESP => (6, 1),
            X86PhysicalRegisterID::EBP => (7, 1),
            X86PhysicalRegisterID::R8D => (8, 1),
            X86PhysicalRegisterID::R9D => (9, 1),
            X86PhysicalRegisterID::R10D => (10, 1),
            X86PhysicalRegisterID::R11D => (11, 1),
            X86PhysicalRegisterID::R12D => (12, 1),
            X86PhysicalRegisterID::R13D => (13, 1),
            X86PhysicalRegisterID::R14D => (14, 1),
            X86PhysicalRegisterID::R15D => (15, 1),

            X86PhysicalRegisterID::AX => (0, 2),
            X86PhysicalRegisterID::BX => (1, 2),
            X86PhysicalRegisterID::CX => (2, 2),
            X86PhysicalRegisterID::DX => (3, 2),
            X86PhysicalRegisterID::SI => (4, 2),
            X86PhysicalRegisterID::DI => (5, 2),
            X86PhysicalRegisterID::SP => (6, 2),
            X86PhysicalRegisterID::BP => (7, 2),
            X86PhysicalRegisterID::R8W => (8, 2),
            X86PhysicalRegisterID::R9W => (9, 2),
            X86PhysicalRegisterID::R10W => (10, 2),
            X86PhysicalRegisterID::R11W => (11, 2),
            X86PhysicalRegisterID::R12W => (12, 2),
            X86PhysicalRegisterID::R13W => (13, 2),
            X86PhysicalRegisterID::R14W => (14, 2),
            X86PhysicalRegisterID::R15W => (15, 2),

            X86PhysicalRegisterID::AL => (0, 3),
            X86PhysicalRegisterID::BL => (1, 3),
            X86PhysicalRegisterID::CL => (2, 3),
            X86PhysicalRegisterID::DL => (3, 3),
            X86PhysicalRegisterID::SIL => (4, 3),
            X86PhysicalRegisterID::DIL => (5, 3),
            X86PhysicalRegisterID::SPL => (6, 3),
            X86PhysicalRegisterID::BPL => (7, 3),
            X86PhysicalRegisterID::R8B => (8, 3),
            X86PhysicalRegisterID::R9B => (9, 3),
            X86PhysicalRegisterID::R10B => (10, 3),
            X86PhysicalRegisterID::R11B => (11, 3),
            X86PhysicalRegisterID::R12B => (12, 3),
            X86PhysicalRegisterID::R13B => (13, 3),
            X86PhysicalRegisterID::R14B => (14, 3),
            X86PhysicalRegisterID::R15B => (15, 3),
        }
    }

    pub fn get_usage(&self) -> X86PhysicalRegisterUsage {
        let (pack, _) = self.get_pack_and_pos();
        let bits = match pack {
            0 => {
                X86PhysicalRegisterUsageBit::GeneralPurposeCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FunctionReturnValue as u32
            }
            1 => X86PhysicalRegisterUsageBit::GeneralPurposeCalleeSaved as u32,
            2 => {
                X86PhysicalRegisterUsageBit::GeneralPurposeCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FunctionParameter4 as u32
            }
            3 => {
                X86PhysicalRegisterUsageBit::GeneralPurposeCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FunctionParameter3 as u32
            }
            4 => {
                X86PhysicalRegisterUsageBit::GeneralPurposeCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FunctionParameter2 as u32
            }
            5 => {
                X86PhysicalRegisterUsageBit::GeneralPurposeCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FunctionParameter1 as u32
            }
            6 => X86PhysicalRegisterUsageBit::StackPointer as u32,
            7 => X86PhysicalRegisterUsageBit::GeneralPurposeCalleeSaved as u32,
            8 => {
                X86PhysicalRegisterUsageBit::GeneralPurposeCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FunctionParameter5 as u32
            }
            9 => {
                X86PhysicalRegisterUsageBit::GeneralPurposeCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FunctionParameter6 as u32
            }
            10 | 11 => X86PhysicalRegisterUsageBit::GeneralPurposeCallerSaved as u32,
            12 | 13 | 14 | 15 => X86PhysicalRegisterUsageBit::GeneralPurposeCalleeSaved as u32,
            _ => panic!("PANIC: Invalid register pack."),
        };
        bits
    }
}

pub fn x86_physical_registers_overlap(
    first: X86PhysicalRegisterID,
    second: X86PhysicalRegisterID,
) -> bool {
    let (first_pack, first_pos) = first.get_pack_and_pos();
    let (second_pack, second_pos) = second.get_pack_and_pos();
    first_pack == second_pack
        && (first_pos == 2 && second_pos == 3 || first_pos == 3 && second_pos == 2)
}

#[derive(Debug, PartialEq)]
pub enum X86Register {
    Virtual(X86VirtualRegisterID, u32),
    Physical(X86PhysicalRegisterID),
}
