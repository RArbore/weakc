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

use core::fmt;

pub type X86VirtualRegisterID = u32;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum X86VirtualRegisterType {
    Fixed32,
    Fixed64,
    Float64,
}

impl X86VirtualRegisterType {
    pub fn size(&self) -> u64 {
        match self {
            X86VirtualRegisterType::Fixed32 => 4,
            X86VirtualRegisterType::Fixed64 => 8,
            X86VirtualRegisterType::Float64 => 8,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,

    RIP,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum X86PhysicalRegisterUsageBit {
    FixedCallerSaved = 1 << 0,
    FixedCalleeSaved = 1 << 1,
    FloatingCallerSaved = 1 << 2,
    FixedFunctionParameter1 = 1 << 3,
    FixedFunctionParameter2 = 1 << 4,
    FixedFunctionParameter3 = 1 << 5,
    FixedFunctionParameter4 = 1 << 6,
    FixedFunctionParameter5 = 1 << 7,
    FixedFunctionParameter6 = 1 << 8,
    FloatingFunctionParameter1 = 1 << 9,
    FloatingFunctionParameter2 = 1 << 10,
    FloatingFunctionParameter3 = 1 << 11,
    FloatingFunctionParameter4 = 1 << 12,
    FloatingFunctionParameter5 = 1 << 13,
    FloatingFunctionParameter6 = 1 << 14,
    FloatingFunctionParameter7 = 1 << 15,
    FloatingFunctionParameter8 = 1 << 16,
    FixedFunctionReturnValue = 1 << 17,
    FloatingFunctionReturnValue = 1 << 18,
    StackPointer = 1 << 19,
    InstructionPointer = 1 << 20,
}

pub type X86PhysicalRegisterUsage = u32;

impl X86PhysicalRegisterID {
    fn get_pack_and_pos(&self) -> (i32, i32) {
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

            X86PhysicalRegisterID::XMM0 => (16, 0),
            X86PhysicalRegisterID::XMM1 => (17, 0),
            X86PhysicalRegisterID::XMM2 => (18, 0),
            X86PhysicalRegisterID::XMM3 => (19, 0),
            X86PhysicalRegisterID::XMM4 => (20, 0),
            X86PhysicalRegisterID::XMM5 => (21, 0),
            X86PhysicalRegisterID::XMM6 => (22, 0),
            X86PhysicalRegisterID::XMM7 => (23, 0),
            X86PhysicalRegisterID::XMM8 => (24, 0),
            X86PhysicalRegisterID::XMM9 => (25, 0),
            X86PhysicalRegisterID::XMM10 => (26, 0),
            X86PhysicalRegisterID::XMM11 => (27, 0),
            X86PhysicalRegisterID::XMM12 => (28, 0),
            X86PhysicalRegisterID::XMM13 => (29, 0),
            X86PhysicalRegisterID::XMM14 => (30, 0),
            X86PhysicalRegisterID::XMM15 => (31, 0),

            X86PhysicalRegisterID::RIP => (-1, -1),
        }
    }

    pub fn get_usage(&self) -> X86PhysicalRegisterUsage {
        let (pack, _) = self.get_pack_and_pos();
        let bits = match pack {
            -1 => X86PhysicalRegisterUsageBit::InstructionPointer as u32,
            0 => {
                X86PhysicalRegisterUsageBit::FixedCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FixedFunctionReturnValue as u32
            }
            1 => X86PhysicalRegisterUsageBit::FixedCalleeSaved as u32,
            2 => {
                X86PhysicalRegisterUsageBit::FixedCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FixedFunctionParameter4 as u32
            }
            3 => {
                X86PhysicalRegisterUsageBit::FixedCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FixedFunctionParameter3 as u32
            }
            4 => {
                X86PhysicalRegisterUsageBit::FixedCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FixedFunctionParameter2 as u32
            }
            5 => {
                X86PhysicalRegisterUsageBit::FixedCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FixedFunctionParameter1 as u32
            }
            6 => X86PhysicalRegisterUsageBit::StackPointer as u32,
            7 => X86PhysicalRegisterUsageBit::FixedCalleeSaved as u32,
            8 => {
                X86PhysicalRegisterUsageBit::FixedCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FixedFunctionParameter5 as u32
            }
            9 => {
                X86PhysicalRegisterUsageBit::FixedCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FixedFunctionParameter6 as u32
            }
            10 | 11 => X86PhysicalRegisterUsageBit::FixedCallerSaved as u32,
            12 | 13 | 14 | 15 => X86PhysicalRegisterUsageBit::FixedCalleeSaved as u32,
            16 => {
                X86PhysicalRegisterUsageBit::FloatingCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FloatingFunctionParameter1 as u32
                    | X86PhysicalRegisterUsageBit::FloatingFunctionReturnValue as u32
            }
            17 => {
                X86PhysicalRegisterUsageBit::FloatingCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FloatingFunctionParameter2 as u32
            }
            18 => {
                X86PhysicalRegisterUsageBit::FloatingCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FloatingFunctionParameter3 as u32
            }
            19 => {
                X86PhysicalRegisterUsageBit::FloatingCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FloatingFunctionParameter4 as u32
            }
            20 => {
                X86PhysicalRegisterUsageBit::FloatingCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FloatingFunctionParameter5 as u32
            }
            21 => {
                X86PhysicalRegisterUsageBit::FloatingCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FloatingFunctionParameter6 as u32
            }
            22 => {
                X86PhysicalRegisterUsageBit::FloatingCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FloatingFunctionParameter7 as u32
            }
            23 => {
                X86PhysicalRegisterUsageBit::FloatingCallerSaved as u32
                    | X86PhysicalRegisterUsageBit::FloatingFunctionParameter8 as u32
            }
            24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 => {
                X86PhysicalRegisterUsageBit::FloatingCallerSaved as u32
            }
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
        && !(first_pos == 2 && second_pos == 3 || first_pos == 3 && second_pos == 2)
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum X86Register {
    Virtual(X86VirtualRegisterID, X86VirtualRegisterType),
    Physical(X86PhysicalRegisterID),
}

impl X86Register {
    pub fn size(&self) -> u64 {
        match self {
            X86Register::Virtual(_, ty) => ty.size(),
            X86Register::Physical(id) => match id.get_pack_and_pos().1 {
                0 => 8,
                1 => 4,
                2 => 2,
                3 => 2,
                _ => panic!(),
            },
        }
    }
}

impl fmt::Display for X86VirtualRegisterType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86VirtualRegisterType::Fixed32 => write!(f, "fixed32"),
            X86VirtualRegisterType::Fixed64 => write!(f, "fixed64"),
            X86VirtualRegisterType::Float64 => write!(f, "float64"),
        }?;
        Ok(())
    }
}

impl fmt::Display for X86PhysicalRegisterID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86PhysicalRegisterID::RAX => write!(f, "rax"),
            X86PhysicalRegisterID::RBX => write!(f, "rbx"),
            X86PhysicalRegisterID::RCX => write!(f, "rcx"),
            X86PhysicalRegisterID::RDX => write!(f, "rdx"),
            X86PhysicalRegisterID::RSI => write!(f, "rsi"),
            X86PhysicalRegisterID::RDI => write!(f, "rdi"),
            X86PhysicalRegisterID::RSP => write!(f, "rsp"),
            X86PhysicalRegisterID::RBP => write!(f, "rbp"),
            X86PhysicalRegisterID::R8 => write!(f, "r8"),
            X86PhysicalRegisterID::R9 => write!(f, "r9"),
            X86PhysicalRegisterID::R10 => write!(f, "r10"),
            X86PhysicalRegisterID::R11 => write!(f, "r11"),
            X86PhysicalRegisterID::R12 => write!(f, "r12"),
            X86PhysicalRegisterID::R13 => write!(f, "r13"),
            X86PhysicalRegisterID::R14 => write!(f, "r14"),
            X86PhysicalRegisterID::R15 => write!(f, "r15"),

            X86PhysicalRegisterID::EAX => write!(f, "eax"),
            X86PhysicalRegisterID::EBX => write!(f, "ebx"),
            X86PhysicalRegisterID::ECX => write!(f, "ecx"),
            X86PhysicalRegisterID::EDX => write!(f, "edx"),
            X86PhysicalRegisterID::ESI => write!(f, "esi"),
            X86PhysicalRegisterID::EDI => write!(f, "edi"),
            X86PhysicalRegisterID::ESP => write!(f, "esp"),
            X86PhysicalRegisterID::EBP => write!(f, "ebp"),
            X86PhysicalRegisterID::R8D => write!(f, "r8d"),
            X86PhysicalRegisterID::R9D => write!(f, "r9d"),
            X86PhysicalRegisterID::R10D => write!(f, "r10d"),
            X86PhysicalRegisterID::R11D => write!(f, "r11d"),
            X86PhysicalRegisterID::R12D => write!(f, "r12d"),
            X86PhysicalRegisterID::R13D => write!(f, "r13d"),
            X86PhysicalRegisterID::R14D => write!(f, "r14d"),
            X86PhysicalRegisterID::R15D => write!(f, "r15d"),

            X86PhysicalRegisterID::AX => write!(f, "ax"),
            X86PhysicalRegisterID::BX => write!(f, "bx"),
            X86PhysicalRegisterID::CX => write!(f, "cx"),
            X86PhysicalRegisterID::DX => write!(f, "dx"),
            X86PhysicalRegisterID::SI => write!(f, "si"),
            X86PhysicalRegisterID::DI => write!(f, "di"),
            X86PhysicalRegisterID::SP => write!(f, "sp"),
            X86PhysicalRegisterID::BP => write!(f, "bp"),
            X86PhysicalRegisterID::R8W => write!(f, "r8w"),
            X86PhysicalRegisterID::R9W => write!(f, "r9w"),
            X86PhysicalRegisterID::R10W => write!(f, "r10w"),
            X86PhysicalRegisterID::R11W => write!(f, "r11w"),
            X86PhysicalRegisterID::R12W => write!(f, "r12w"),
            X86PhysicalRegisterID::R13W => write!(f, "r13w"),
            X86PhysicalRegisterID::R14W => write!(f, "r14w"),
            X86PhysicalRegisterID::R15W => write!(f, "r15w"),

            X86PhysicalRegisterID::AL => write!(f, "al"),
            X86PhysicalRegisterID::BL => write!(f, "bl"),
            X86PhysicalRegisterID::CL => write!(f, "cl"),
            X86PhysicalRegisterID::DL => write!(f, "dl"),
            X86PhysicalRegisterID::SIL => write!(f, "sil"),
            X86PhysicalRegisterID::DIL => write!(f, "dil"),
            X86PhysicalRegisterID::SPL => write!(f, "spl"),
            X86PhysicalRegisterID::BPL => write!(f, "bpl"),
            X86PhysicalRegisterID::R8B => write!(f, "r8b"),
            X86PhysicalRegisterID::R9B => write!(f, "r9b"),
            X86PhysicalRegisterID::R10B => write!(f, "r10b"),
            X86PhysicalRegisterID::R11B => write!(f, "r11b"),
            X86PhysicalRegisterID::R12B => write!(f, "r12b"),
            X86PhysicalRegisterID::R13B => write!(f, "r13b"),
            X86PhysicalRegisterID::R14B => write!(f, "r14b"),
            X86PhysicalRegisterID::R15B => write!(f, "r15b"),

            X86PhysicalRegisterID::XMM0 => write!(f, "xmm0"),
            X86PhysicalRegisterID::XMM1 => write!(f, "xmm1"),
            X86PhysicalRegisterID::XMM2 => write!(f, "xmm2"),
            X86PhysicalRegisterID::XMM3 => write!(f, "xmm3"),
            X86PhysicalRegisterID::XMM4 => write!(f, "xmm4"),
            X86PhysicalRegisterID::XMM5 => write!(f, "xmm5"),
            X86PhysicalRegisterID::XMM6 => write!(f, "xmm6"),
            X86PhysicalRegisterID::XMM7 => write!(f, "xmm7"),
            X86PhysicalRegisterID::XMM8 => write!(f, "xmm8"),
            X86PhysicalRegisterID::XMM9 => write!(f, "xmm9"),
            X86PhysicalRegisterID::XMM10 => write!(f, "xmm10"),
            X86PhysicalRegisterID::XMM11 => write!(f, "xmm11"),
            X86PhysicalRegisterID::XMM12 => write!(f, "xmm12"),
            X86PhysicalRegisterID::XMM13 => write!(f, "xmm13"),
            X86PhysicalRegisterID::XMM14 => write!(f, "xmm14"),
            X86PhysicalRegisterID::XMM15 => write!(f, "xmm15"),

            X86PhysicalRegisterID::RIP => write!(f, "rip"),
        }?;
        Ok(())
    }
}

impl fmt::Display for X86Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            X86Register::Virtual(reg, reg_type) => write!(f, "%{} {}", reg, reg_type),
            X86Register::Physical(reg) => write!(f, "{}", reg),
        }?;
        Ok(())
    }
}
