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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IRType {
    Nil,
    Boolean,
    String,
    Number,
    Tensor,
}

type Register = (u32, IRType);

#[derive(Debug, PartialEq, Clone)]
pub struct IRInstruction {}

#[derive(Debug, PartialEq, Clone)]
pub struct IRBasicBlock<'a> {
    insts: &'a bump::List<'a, IRInstruction>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRFunction<'a> {
    name: &'a [u8],
    blocks: &'a bump::List<'a, IRFunction<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRModule<'a> {
    funcs: &'a bump::List<'a, IRFunction<'a>>,
}
