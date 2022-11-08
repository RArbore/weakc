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

use parse::ASTExpr;
use parse::ASTStmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    Tensor,
    Numeric,
    Boolean,
    String,
}

enum Symbol<'a> {
    Variable(&'a [u8], Type),
    Function(&'a [u8], Option<&'a bump::List<'a, Type>>, Option<Type>),
}

pub fn typecheck<'a>(
    ast: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> Option<&'a bump::List<'a, Type>> {
    let mut types = bump.create_list();
    let mut symbols = vec![];
    for i in 0..ast.len() {
        (types, symbols) = typecheck_stmt(ast.at(i), types, symbols, bump)?;
    }
    Some(types)
}

fn typecheck_stmt<'a>(
    ast: &'a ASTStmt<'a>,
    types: &'a mut bump::List<'a, Type>,
    symbols: Vec<Symbol<'a>>,
    bump: &'a bump::BumpAllocator,
) -> Option<(&'a mut bump::List<'a, Type>, Vec<Symbol<'a>>)> {
    Some((types, symbols))
}

mod tests {
    use super::*;

    #[test]
    fn typecheck1() {}
}
