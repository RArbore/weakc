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
    Boolean,
    String,
    Generic(u32),
    Numeric(u32),
}

enum Symbol<'a> {
    Variable(&'a [u8], Type),
    Function(&'a [u8], Option<&'a bump::List<'a, Type>>, Option<Type>),
}

struct TypeContext<'a> {
    types: &'a mut bump::List<'a, Type>,
    symbols: Vec<Symbol<'a>>,
    cur_type_var: u32,
}

pub fn typecheck<'a>(
    ast: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> Option<&'a bump::List<'a, Type>> {
    let mut context = TypeContext {
        types: bump.create_list(),
        symbols: vec![],
        cur_type_var: 0,
    };
    for i in 0..ast.len() {
        context = typecheck_stmt(ast.at(i), context)?;
    }
    Some(context.types)
}

fn typecheck_stmt<'a>(
    ast: &'a ASTStmt<'a>,
    mut context: TypeContext<'a>,
) -> Option<TypeContext<'a>> {
    match ast {
        ASTStmt::Block(stmts) => {
            let before_symbols = context.symbols.len();
            for i in 0..stmts.len() {
                context = typecheck_stmt(stmts.at(i), context)?;
            }
            context.symbols.truncate(before_symbols);
        }
        ASTStmt::Variable(var, init) => {
            let (new_context, init_type) = typecheck_expr(init, context)?;
            context = new_context;
            context.symbols.push(Symbol::Variable(var, init_type));
        }
        _ => panic!(),
    }
    Some(context)
}

fn typecheck_expr<'a>(
    ast: &'a ASTExpr<'a>,
    context: TypeContext<'a>,
) -> Option<(TypeContext<'a>, Type)> {
    let my_type = match ast {
        ASTExpr::Nil => Type::Nil,
        ASTExpr::Boolean(_) => Type::Boolean,
        ASTExpr::Number(_) => Type::Number,
        ASTExpr::String(_) => Type::String,
        ASTExpr::Identifier(var) => {
            let mut found_type = None;
            for symbol in context.symbols.iter() {
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
    context.types.push(my_type);
    Some((context, my_type))
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

    #[test]
    fn typecheck2() {
        let bump = bump::BumpAllocator::new();
        let ast = bump.create_list();
        ast.push(ASTStmt::Variable(
            b"var1",
            bump.alloc(ASTExpr::Boolean(false)),
        ));
        ast.push(ASTStmt::Variable(b"var2", bump.alloc(ASTExpr::Number(0.0))));
        ast.push(ASTStmt::Variable(
            b"var3",
            bump.alloc(ASTExpr::String(b"string")),
        ));
        ast.push(ASTStmt::Variable(
            b"var4",
            bump.alloc(ASTExpr::Identifier(b"var1")),
        ));
        ast.push(ASTStmt::Variable(
            b"var5",
            bump.alloc(ASTExpr::Identifier(b"var2")),
        ));
        ast.push(ASTStmt::Variable(
            b"var6",
            bump.alloc(ASTExpr::Identifier(b"var3")),
        ));
        let typecheck = typecheck(ast, &bump).unwrap();
        let correct_list = bump.create_list();
        correct_list.push(Type::Boolean);
        correct_list.push(Type::Number);
        correct_list.push(Type::String);
        correct_list.push(Type::Boolean);
        correct_list.push(Type::Number);
        correct_list.push(Type::String);
        assert_eq!(typecheck, correct_list);
    }
}
