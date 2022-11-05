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

use core::cell::RefCell;

use parse::ASTExpr;
use parse::ASTStmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    Tensor,
    Boolean,
    String,
}

pub fn typecheck<'a>(
    ast: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> Option<&'a bump::List<'a, Type>> {
    let types: RefCell<&'a bump::List<'a, Type>> = RefCell::new(bump.create_list());
    for i in 0..ast.len() {
        typecheck_stmt(ast.at(i), types.clone(), bump)?;
    }
    Some(types.into_inner())
}

pub fn typecheck_stmt<'a>(
    ast: &'a ASTStmt<'a>,
    types: RefCell<&'a bump::List<'a, Type>>,
    bump: &'a bump::BumpAllocator,
) -> Option<()> {
    Some(())
}

pub fn typecheck_expr<'a>(
    ast: &'a ASTExpr<'a>,
    types: RefCell<&'a bump::List<'a, Type>>,
    bump: &'a bump::BumpAllocator,
) -> Option<()> {
    Some(())
}

mod tests {
    use super::*;

    #[test]
    fn typecheck1() {}
}
