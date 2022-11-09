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
    bounds_of_type_vars: Vec<(u32, u32)>,
}

impl<'a> TypeContext<'a> {
    fn replace_generic(&mut self, var: u32, new: Type) {
        let bounds = self.bounds_of_type_vars[var as usize];
        for i in bounds.0..=bounds.1 {
            let i = i as usize;
            match self.types.at(i) {
                Type::Generic(gen_var) => {
                    if var == *gen_var {
                        *self.types.at_mut(i) = new;
                    }
                }
                Type::Numeric(gen_var) => {
                    if var == *gen_var {
                        *self.types.at_mut(i) = new;
                    }
                }
                _ => {}
            }
        }
        let new_var = match new {
            Type::Generic(var) => Some(var),
            Type::Numeric(var) => Some(var),
            _ => None,
        };
        if let Some(new_var) = new_var {
            let new_var = new_var as usize;
            self.bounds_of_type_vars[new_var].0 =
                core::cmp::min(self.bounds_of_type_vars[new_var].0, bounds.0);
            self.bounds_of_type_vars[new_var].1 =
                core::cmp::max(self.bounds_of_type_vars[new_var].1, bounds.1);
        }
        self.bounds_of_type_vars[var as usize].0 = 1;
        self.bounds_of_type_vars[var as usize].1 = 0;
    }
}

fn constrain<'a>(dst: Type, src: Type, mut context: TypeContext<'a>) -> Option<TypeContext<'a>> {
    match dst {
        Type::Generic(var) => context.replace_generic(var, src),
        Type::Numeric(var) => context.replace_generic(var, src),
        ty => {
            if ty != src {
                return None;
            }
        }
    }
    Some(context)
}

pub fn typecheck<'a>(
    ast: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> Option<&'a bump::List<'a, Type>> {
    let mut context = TypeContext {
        types: bump.create_list(),
        symbols: vec![],
        cur_type_var: 0,
        bounds_of_type_vars: vec![],
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
            let (init_type, new_context) = typecheck_expr(init, context)?;
            context = new_context;
            context.symbols.push(Symbol::Variable(var, init_type));
        }
        ASTStmt::Expression(expr) => {
            (_, context) = typecheck_expr(expr, context)?;
        }
        _ => panic!(),
    }
    Some(context)
}

fn typecheck_expr<'a>(
    ast: &'a ASTExpr<'a>,
    mut context: TypeContext<'a>,
) -> Option<(Type, TypeContext<'a>)> {
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
        ASTExpr::Index(arr, indices) => {
            let (arr_type, new_context) = typecheck_expr(arr, context)?;
            context = constrain(arr_type, Type::Tensor, new_context)?;
            for i in 0..indices.len() {
                let (idx_type, idx_context) = typecheck_expr(indices.at(i), context)?;
                context = constrain(idx_type, Type::Number, idx_context)?;
            }
            Type::Number
        }
        ASTExpr::ArrayLiteral(contents) => {
            for i in 0..contents.len() {
                let (elem_type, elem_context) = typecheck_expr(contents.at(i), context)?;
                context = constrain(elem_type, Type::Number, elem_context)?;
            }
            Type::Tensor
        }
        _ => panic!(),
    };
    context.types.push(my_type);
    Some((my_type, context))
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
        let correct_list = bump.create_list_with(&[
            Type::Boolean,
            Type::Number,
            Type::String,
            Type::Boolean,
            Type::Number,
            Type::String,
        ]);
        assert_eq!(typecheck, correct_list);
    }

    #[test]
    fn typecheck3() {
        let bump = bump::BumpAllocator::new();
        let ast = bump.create_list();
        ast.push(ASTStmt::Expression(bump.alloc(ASTExpr::ArrayLiteral(
            bump.create_list_with(&[ASTExpr::Number(0.0)]),
        ))));
        let typecheck = typecheck(ast, &bump).unwrap();
        let correct_list = bump.create_list_with(&[Type::Number, Type::Tensor]);
        assert_eq!(typecheck, correct_list);
    }
}
