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

pub type X86VirtualRegisterID = u32;

#[derive(Debug, PartialEq)]
pub enum X86PhysicalRegisterID {}

#[derive(Debug, PartialEq)]
pub enum X86Register {
    Virtual(X86VirtualRegisterID, u32),
    Physical(X86PhysicalRegisterID),
}

#[derive(Debug, PartialEq)]
pub enum X86Instruction {}

#[derive(Debug, PartialEq)]
pub struct X86Block<'a> {
    pub label: &'a [u8],
    pub insts: &'a mut bump::List<'a, X86Instruction>,
}

#[derive(Debug, PartialEq)]
pub struct X86Module<'a> {
    pub funcs: &'a mut bump::List<'a, X86Block<'a>>,
    pub strings: &'a mut bump::List<'a, &'a [u8]>,
}
