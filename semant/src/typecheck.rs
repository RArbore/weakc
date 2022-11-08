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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Nil,
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
        (types, symbols) = typecheck_stmt(ast.at(i), types, symbols)?;
    }
    Some(types)
}

fn typecheck_stmt<'a>(
    ast: &'a ASTStmt<'a>,
    mut types: &'a mut bump::List<'a, Type>,
    mut symbols: Vec<Symbol<'a>>,
) -> Option<(&'a mut bump::List<'a, Type>, Vec<Symbol<'a>>)> {
    match ast {
        ASTStmt::Block(stmts) => {
            let before_symbols = symbols.len();
            for i in 0..stmts.len() {
                (types, symbols) = typecheck_stmt(stmts.at(i), types, symbols)?;
            }
            symbols.truncate(before_symbols);
        }
        ASTStmt::Variable(var, init) => {
            let (new_types, new_symbols, init_type) = typecheck_expr(init, types, symbols)?;
            types = new_types;
            symbols = new_symbols;
            symbols.push(Symbol::Variable(var, init_type));
        }
        _ => panic!(),
    }
    Some((types, symbols))
}

fn typecheck_expr<'a>(
    ast: &'a ASTExpr<'a>,
    types: &'a mut bump::List<'a, Type>,
    symbols: Vec<Symbol<'a>>,
) -> Option<(&'a mut bump::List<'a, Type>, Vec<Symbol<'a>>, Type)> {
    let my_type = match ast {
        ASTExpr::Nil => Type::Nil,
        ASTExpr::Identifier(var) => {
            let mut found_type = None;
            for symbol in symbols.iter() {
                if let Symbol::Variable(potential_var, potential_type) = symbol {
                    if var == potential_var {
                        found_type = Some(*potential_type);
                        break;
                    }
                }
            }
            found_type?
        }
        _ => panic!(),
    };
    types.push(my_type);
    Some((types, symbols, my_type))
}

mod tests {
    use super::*;

    #[test]
    fn typecheck1() {
        let bump = bump::BumpAllocator::new();
        let ast = bump.create_list();
        ast.push(ASTStmt::Variable(b"abc", bump.alloc(ASTExpr::Nil)));
        ast.push(ASTStmt::Variable(
            b"xyz",
            bump.alloc(ASTExpr::Identifier(b"abc")),
        ));
        let typecheck = typecheck(ast, &bump).unwrap();
        let correct_list = bump.create_list();
        correct_list.push(Type::Nil);
        correct_list.push(Type::Nil);
        assert_eq!(typecheck, correct_list);
    }
}
