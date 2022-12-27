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

use crate::*;
use parse::ASTBinaryOp;
use parse::ASTUnaryOp;
use semant::TypedASTExpr;
use semant::TypedASTStmt;

pub fn irgen_program<'a>(
    program: &'a bump::List<'a, TypedASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> IRModule {
    IRModule {
        funcs: bump.create_list(),
    }
}
