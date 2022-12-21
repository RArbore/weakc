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

use parse::ASTBinaryOp;
use parse::ASTExpr;
use parse::ASTStmt;
use parse::ASTUnaryOp;

type TypeResult<T> = Result<T, &'static str>;

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

#[derive(Debug, PartialEq, Clone)]
pub enum TypedASTStmt<'a> {
    Block(&'a bump::List<'a, TypedASTStmt<'a>>),
    Function(
        &'a [u8],
        &'a bump::List<'a, (&'a [u8], Type)>,
        &'a TypedASTStmt<'a>,
        Type,
    ),
    Operator(
        &'a [u8],
        &'a [u8],
        &'a [u8],
        Type,
        Type,
        &'a TypedASTStmt<'a>,
        Type,
    ),
    If(&'a TypedASTExpr<'a>, &'a TypedASTStmt<'a>),
    While(&'a TypedASTExpr<'a>, &'a TypedASTStmt<'a>),
    Print(&'a TypedASTExpr<'a>),
    Return(&'a TypedASTExpr<'a>),
    Verify(&'a TypedASTExpr<'a>),
    Variable(&'a [u8], &'a TypedASTExpr<'a>),
    Expression(&'a TypedASTExpr<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypedASTExpr<'a> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(&'a [u8]),
    Identifier(&'a [u8], Type),
    Call(&'a [u8], &'a bump::List<'a, TypedASTExpr<'a>>, Type),
    Index(&'a TypedASTExpr<'a>, &'a bump::List<'a, TypedASTExpr<'a>>),
    ArrayLiteral(&'a bump::List<'a, TypedASTExpr<'a>>),
    Assign(&'a TypedASTExpr<'a>, &'a TypedASTExpr<'a>, Type),
    Unary(ASTUnaryOp, &'a TypedASTExpr<'a>, Type),
    Binary(
        ASTBinaryOp,
        &'a TypedASTExpr<'a>,
        &'a TypedASTExpr<'a>,
        Type,
    ),
    CustomBinary(&'a [u8], &'a TypedASTExpr<'a>, &'a TypedASTExpr<'a>, Type),
}

struct TypeContext {
    num_generics: u32,
}

pub fn typecheck_program<'a>(
    program: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> TypeResult<&'a bump::List<'a, TypedASTStmt<'a>>> {
    let mut context = TypeContext::new();
    let unconstrained = context.generate_unconstrained_tree(program, bump);
    Err("Unimplemented!")
}

impl TypeContext {
    fn new() -> Self {
        TypeContext { num_generics: 0 }
    }

    fn generate_generic(&mut self) -> Type {
        let ty = Type::Generic(self.num_generics);
        self.num_generics += 1;
        ty
    }

    fn generate_unconstrained_tree<'a>(
        &mut self,
        program: &'a bump::List<'a, ASTStmt<'a>>,
        bump: &'a bump::BumpAllocator,
    ) -> &'a bump::List<'a, TypedASTStmt<'a>> {
        let unconstrained = bump.create_list();
        for i in 0..program.len() {
            unconstrained.push(self.generate_unconstrained_stmt(program.at(i), bump));
        }
        unconstrained
    }

    fn generate_unconstrained_stmt<'a>(
        &mut self,
        stmt: &'a ASTStmt<'a>,
        bump: &'a bump::BumpAllocator,
    ) -> TypedASTStmt<'a> {
        match stmt {
            ASTStmt::Block(stmts) => {
                let contents = bump.create_list();
                for i in 0..stmts.len() {
                    contents.push(self.generate_unconstrained_stmt(stmts.at(i), bump));
                }
                TypedASTStmt::Block(contents)
            }
            ASTStmt::Function(name, params, body) => {
                let new_params = bump.create_list();
                for i in 0..params.len() {
                    new_params.push((*params.at(i), self.generate_generic()));
                }
                let new_body = self.generate_unconstrained_stmt(body, bump);
                TypedASTStmt::Function(
                    name,
                    new_params,
                    bump.alloc(new_body),
                    self.generate_generic(),
                )
            }
            ASTStmt::Operator(name, left_param, right_param, body) => {
                let new_body = self.generate_unconstrained_stmt(body, bump);
                TypedASTStmt::Operator(
                    name,
                    left_param,
                    right_param,
                    self.generate_generic(),
                    self.generate_generic(),
                    bump.alloc(new_body),
                    self.generate_generic(),
                )
            }
            ASTStmt::If(cond, body) => {
                let new_cond = self.generate_unconstrained_expr(cond, bump);
                let new_body = self.generate_unconstrained_stmt(body, bump);
                TypedASTStmt::If(bump.alloc(new_cond), bump.alloc(new_body))
            }
            ASTStmt::While(cond, body) => {
                let new_cond = self.generate_unconstrained_expr(cond, bump);
                let new_body = self.generate_unconstrained_stmt(body, bump);
                TypedASTStmt::While(bump.alloc(new_cond), bump.alloc(new_body))
            }
            ASTStmt::Print(expr) => {
                let new_expr = self.generate_unconstrained_expr(expr, bump);
                TypedASTStmt::Print(bump.alloc(new_expr))
            }
            ASTStmt::Return(expr) => {
                let new_expr = self.generate_unconstrained_expr(expr, bump);
                TypedASTStmt::Return(bump.alloc(new_expr))
            }
            ASTStmt::Verify(expr) => {
                let new_expr = self.generate_unconstrained_expr(expr, bump);
                TypedASTStmt::Verify(bump.alloc(new_expr))
            }
            ASTStmt::Variable(name, init) => {
                let new_init = self.generate_unconstrained_expr(init, bump);
                TypedASTStmt::Variable(name, bump.alloc(new_init))
            }
            ASTStmt::Expression(expr) => {
                let new_expr = self.generate_unconstrained_expr(expr, bump);
                TypedASTStmt::Expression(bump.alloc(new_expr))
            }
        }
    }

    fn generate_unconstrained_expr<'a>(
        &mut self,
        stmt: &'a ASTExpr<'a>,
        bump: &'a bump::BumpAllocator,
    ) -> TypedASTExpr<'a> {
        match stmt {
            ASTExpr::Nil => TypedASTExpr::Nil,
            ASTExpr::Boolean(v) => TypedASTExpr::Boolean(*v),
            ASTExpr::Number(v) => TypedASTExpr::Number(*v),
            ASTExpr::String(v) => TypedASTExpr::String(v),
            _ => panic!(),
        }
    }
}
