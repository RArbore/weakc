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
    let unconstrained = context.generate_unconstrained_tree(program, bump)?;
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
    ) -> TypeResult<&'a bump::List<'a, TypedASTStmt<'a>>> {
        let unconstrained = bump.create_list();
        for i in 0..program.len() {
            unconstrained.push(self.generate_unconstrained_stmt(program.at(i), bump)?);
        }
        Ok(unconstrained)
    }

    fn generate_unconstrained_stmt<'a>(
        &mut self,
        stmt: &'a ASTStmt<'a>,
        bump: &'a bump::BumpAllocator,
    ) -> TypeResult<TypedASTStmt<'a>> {
        match stmt {
            ASTStmt::Block(stmts) => {
                let contents = bump.create_list();
                for i in 0..stmts.len() {
                    contents.push(self.generate_unconstrained_stmt(stmts.at(i), bump)?);
                }
                Ok(TypedASTStmt::Block(contents))
            }
            ASTStmt::Function(name, params, body) => {
                let new_params = bump.create_list();
                for i in 0..params.len() {
                    new_params.push((*params.at(i), self.generate_generic()));
                }
                let new_body = self.generate_unconstrained_stmt(body, bump)?;
                Ok(TypedASTStmt::Function(
                    name,
                    new_params,
                    bump.alloc(new_body),
                    self.generate_generic(),
                ))
            }
            ASTStmt::Operator(name, left_param, right_param, body) => {
                let new_body = self.generate_unconstrained_stmt(body, bump)?;
                Ok(TypedASTStmt::Operator(
                    name,
                    left_param,
                    right_param,
                    self.generate_generic(),
                    self.generate_generic(),
                    bump.alloc(new_body),
                    self.generate_generic(),
                ))
            }
            ASTStmt::If(cond, body) => {
                let new_cond = self.generate_unconstrained_expr(cond, bump)?;
                let new_body = self.generate_unconstrained_stmt(body, bump)?;
                Ok(TypedASTStmt::If(bump.alloc(new_cond), bump.alloc(new_body)))
            }
            ASTStmt::While(cond, body) => {
                let new_cond = self.generate_unconstrained_expr(cond, bump)?;
                let new_body = self.generate_unconstrained_stmt(body, bump)?;
                Ok(TypedASTStmt::While(
                    bump.alloc(new_cond),
                    bump.alloc(new_body),
                ))
            }
            ASTStmt::Print(expr) => {
                let new_expr = self.generate_unconstrained_expr(expr, bump)?;
                Ok(TypedASTStmt::Print(bump.alloc(new_expr)))
            }
            ASTStmt::Return(expr) => {
                let new_expr = self.generate_unconstrained_expr(expr, bump)?;
                Ok(TypedASTStmt::Return(bump.alloc(new_expr)))
            }
            ASTStmt::Verify(expr) => {
                let new_expr = self.generate_unconstrained_expr(expr, bump)?;
                Ok(TypedASTStmt::Verify(bump.alloc(new_expr)))
            }
            ASTStmt::Variable(name, init) => {
                let new_init = self.generate_unconstrained_expr(init, bump)?;
                Ok(TypedASTStmt::Variable(name, bump.alloc(new_init)))
            }
            ASTStmt::Expression(expr) => {
                let new_expr = self.generate_unconstrained_expr(expr, bump)?;
                Ok(TypedASTStmt::Expression(bump.alloc(new_expr)))
            }
        }
    }

    fn generate_unconstrained_expr<'a>(
        &mut self,
        expr: &'a ASTExpr<'a>,
        bump: &'a bump::BumpAllocator,
    ) -> TypeResult<TypedASTExpr<'a>> {
        match expr {
            ASTExpr::Nil => Ok(TypedASTExpr::Nil),
            ASTExpr::Boolean(v) => Ok(TypedASTExpr::Boolean(*v)),
            ASTExpr::Number(v) => Ok(TypedASTExpr::Number(*v)),
            ASTExpr::String(v) => Ok(TypedASTExpr::String(v)),
            ASTExpr::Identifier(name) => {
                Ok(TypedASTExpr::Identifier(name, self.generate_generic()))
            }
            ASTExpr::Call(func, args) => {
                let new_args = bump.create_list();
                for i in 0..args.len() {
                    new_args.push(self.generate_unconstrained_expr(args.at(i), bump)?);
                }
                Ok(TypedASTExpr::Call(func, new_args, self.generate_generic()))
            }
            ASTExpr::Index(tensor, indices) => {
                let new_tensor = self.generate_unconstrained_expr(tensor, bump)?;
                let new_indices = bump.create_list();
                for i in 0..indices.len() {
                    new_indices.push(self.generate_unconstrained_expr(indices.at(i), bump)?);
                }
                Ok(TypedASTExpr::Index(bump.alloc(new_tensor), new_indices))
            }
            ASTExpr::ArrayLiteral(elements) => {
                let new_elements = bump.create_list();
                for i in 0..elements.len() {
                    new_elements.push(self.generate_unconstrained_expr(elements.at(i), bump)?);
                }
                Ok(TypedASTExpr::ArrayLiteral(new_elements))
            }
            ASTExpr::Assign(left, right) => {
                match left {
                    ASTExpr::Identifier(_) => {}
                    ASTExpr::Index(ASTExpr::Identifier(_), _) => {}
                    _ => Err(
                        "ERROR: Can only assign to a variable or indexing into a tensor variable.",
                    )?,
                }
                let new_left = self.generate_unconstrained_expr(left, bump)?;
                let new_right = self.generate_unconstrained_expr(right, bump)?;
                Ok(TypedASTExpr::Assign(
                    bump.alloc(new_left),
                    bump.alloc(new_right),
                    self.generate_generic(),
                ))
            }
            ASTExpr::Unary(op, expr) => {
                let new_expr = self.generate_unconstrained_expr(expr, bump)?;
                Ok(TypedASTExpr::Unary(
                    *op,
                    bump.alloc(new_expr),
                    self.generate_generic(),
                ))
            }
            ASTExpr::Binary(op, left_expr, right_expr) => {
                let new_left_expr = self.generate_unconstrained_expr(left_expr, bump)?;
                let new_right_expr = self.generate_unconstrained_expr(right_expr, bump)?;
                Ok(TypedASTExpr::Binary(
                    *op,
                    bump.alloc(new_left_expr),
                    bump.alloc(new_right_expr),
                    self.generate_generic(),
                ))
            }
            ASTExpr::CustomBinary(op, left_expr, right_expr) => {
                let new_left_expr = self.generate_unconstrained_expr(left_expr, bump)?;
                let new_right_expr = self.generate_unconstrained_expr(right_expr, bump)?;
                Ok(TypedASTExpr::CustomBinary(
                    op,
                    bump.alloc(new_left_expr),
                    bump.alloc(new_right_expr),
                    self.generate_generic(),
                ))
            }
        }
    }
}
