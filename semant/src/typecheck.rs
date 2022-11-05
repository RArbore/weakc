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

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    Tensor,
    Boolean,
    String,
}

pub fn typecheck<'a>(ast: &'a bump::List<'a, parse::ASTStmt<'a>>, bump: &'a bump::BumpAllocator) {
    let constraints = generate_constraints(ast, bump);
}

fn generate_constraints<'a>(
    ast: &'a bump::List<'a, parse::ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> &'a bump::List<'a, (&'a [u8], Type)> {
    let constraints = bump.create_list();
    constraints
}
