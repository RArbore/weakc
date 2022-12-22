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

use core::mem::swap;
use std::collections::HashMap;

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
        &'a bump::List<'a, &'a [u8]>,
        &'a bump::List<'a, Type>,
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

#[derive(Debug, PartialEq, Clone, Copy)]
enum Constraint {
    Symmetric(Type, Type),
    Conformant(Type, Type),
}

#[derive(Debug, PartialEq)]
struct TypeContext<'a> {
    num_generics: u32,
    constraints: &'a mut bump::List<'a, Constraint>,
    funcs: HashMap<&'a [u8], (&'a bump::List<'a, Type>, Type)>,
    ops: HashMap<&'a [u8], (Type, Type, Type)>,
    vars: HashMap<&'a [u8], Type>,
    ret_ty: Type,
}

pub fn typecheck_program<'a>(
    program: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> TypeResult<&'a bump::List<'a, TypedASTStmt<'a>>> {
    let mut context = TypeContext::new(&bump);
    let unconstrained = context.generate_unconstrained_tree(program, bump)?;
    context.generate_constraints_tree(unconstrained);
    Err("Unimplemented!")
}

fn join_types(ty1: Type, ty2: Type) -> TypeResult<Type> {
    match (ty1, ty2) {
        (Type::Nil, Type::Nil) => Ok(Type::Nil),
        (Type::Number, Type::Number) => Ok(Type::Number),
        (Type::Tensor, Type::Tensor) => Ok(Type::Tensor),
        (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
        (Type::String, Type::String) => Ok(Type::String),
        (Type::Numeric(_), Type::Number) => Ok(Type::Number),
        (Type::Numeric(_), Type::Tensor) => Ok(Type::Tensor),
        (Type::Number, Type::Numeric(_)) => Ok(Type::Number),
        (Type::Tensor, Type::Numeric(_)) => Ok(Type::Tensor),
        (Type::Numeric(v), Type::Numeric(_)) => Ok(Type::Numeric(v)),
        (ty, Type::Generic(_)) => Ok(ty),
        (Type::Generic(_), ty) => Ok(ty),
        _ => Err("ERROR: Could not join incompatible types."),
    }
}

fn is_conforming(constraint: Type, ty: Type) -> TypeResult<Type> {
    match (constraint, ty) {
        (Type::Nil, Type::Nil) => Ok(Type::Nil),
        (Type::Number, Type::Number) => Ok(Type::Number),
        (Type::Tensor, Type::Tensor) => Ok(Type::Tensor),
        (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),
        (Type::String, Type::String) => Ok(Type::String),
        (Type::Numeric(_), Type::Number) => Ok(Type::Number),
        (Type::Numeric(_), Type::Tensor) => Ok(Type::Tensor),
        (Type::Number, Type::Numeric(_)) => Ok(Type::Number),
        (Type::Tensor, Type::Numeric(_)) => Ok(Type::Tensor),
        (Type::Numeric(_), Type::Numeric(v)) => Ok(Type::Numeric(v)),
        (Type::Generic(_), ty) => Ok(ty),
        _ => Err("ERROR: Type is not conformant to requested type."),
    }
}

impl<'a> TypedASTExpr<'a> {
    pub fn get_type(&self) -> Type {
        match self {
            TypedASTExpr::Nil => Type::Nil,
            TypedASTExpr::Boolean(_) => Type::Boolean,
            TypedASTExpr::Number(_) => Type::Number,
            TypedASTExpr::String(_) => Type::String,
            TypedASTExpr::Identifier(_, ty) => *ty,
            TypedASTExpr::Call(_, _, ty) => *ty,
            TypedASTExpr::Index(_, _) => Type::Number,
            TypedASTExpr::ArrayLiteral(_) => Type::Tensor,
            TypedASTExpr::Assign(_, _, ty) => *ty,
            TypedASTExpr::Unary(_, _, ty) => *ty,
            TypedASTExpr::Binary(_, _, _, ty) => *ty,
            TypedASTExpr::CustomBinary(_, _, _, ty) => *ty,
        }
    }
}

impl<'a> TypeContext<'a> {
    fn new(bump: &'a bump::BumpAllocator) -> Self {
        TypeContext {
            num_generics: 0,
            constraints: bump.create_list(),
            funcs: HashMap::new(),
            ops: HashMap::new(),
            vars: HashMap::new(),
            ret_ty: Type::Nil,
        }
    }

    fn generate_generic(&mut self) -> Type {
        let ty = Type::Generic(self.num_generics);
        self.num_generics += 1;
        ty
    }

    fn generate_unconstrained_tree(
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

    fn generate_unconstrained_stmt(
        &mut self,
        stmt: &'a ASTStmt<'a>,
        bump: &'a bump::BumpAllocator,
    ) -> TypeResult<TypedASTStmt<'a>> {
        match stmt {
            ASTStmt::Block(stmts) => {
                let contents = bump.create_list();
                for i in 0..stmts.len() {
                    match stmts.at(i) {
                        ASTStmt::Return(_) => {
                            if i + 1 < stmts.len() {
                                Err("ERROR: Return statement found before end of function.")?
                            }
                        }
                        _ => {}
                    }
                    contents.push(self.generate_unconstrained_stmt(stmts.at(i), bump)?);
                }
                Ok(TypedASTStmt::Block(contents))
            }
            ASTStmt::Function(name, params, body) => {
                let new_params = bump.create_list();
                let new_params_ty = bump.create_list();
                for i in 0..params.len() {
                    new_params.push(*params.at(i));
                    new_params_ty.push(self.generate_generic());
                }
                let new_body = self.generate_unconstrained_stmt(body, bump)?;
                Ok(TypedASTStmt::Function(
                    name,
                    new_params,
                    new_params_ty,
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

    fn generate_unconstrained_expr(
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

    fn generate_constraints_tree(&mut self, program: &'a bump::List<'a, TypedASTStmt<'a>>) {
        for i in 0..program.len() {
            self.generate_constraints_stmt(program.at(i));
        }
    }

    fn generate_constraints_stmt(&mut self, stmt: &'a TypedASTStmt<'a>) {
        match stmt {
            TypedASTStmt::Block(stmts) => {
                for i in 0..stmts.len() {
                    self.generate_constraints_stmt(stmts.at(i));
                }
            }
            TypedASTStmt::Function(op, params, params_ty, body, ret_ty) => {
                self.ret_ty = Type::Nil;
                self.funcs.insert(op, (params_ty, *ret_ty));
                let mut old_vars = HashMap::new();
                swap(&mut self.vars, &mut old_vars);
                for i in 0..params.len() {
                    self.vars.insert(params.at(i), *params_ty.at(i));
                }
                self.generate_constraints_stmt(body);
                self.constraints
                    .push(Constraint::Symmetric(self.ret_ty, *ret_ty));
                swap(&mut self.vars, &mut old_vars);
            }
            TypedASTStmt::Operator(op, left, right, left_ty, right_ty, body, ret_ty) => {
                self.ret_ty = Type::Nil;
                self.ops.insert(op, (*left_ty, *right_ty, *ret_ty));
                let mut old_vars = HashMap::new();
                swap(&mut self.vars, &mut old_vars);
                self.vars.insert(left, *left_ty);
                self.vars.insert(right, *right_ty);
                self.generate_constraints_stmt(body);
                self.constraints
                    .push(Constraint::Symmetric(self.ret_ty, *ret_ty));
                swap(&mut self.vars, &mut old_vars);
            }
            TypedASTStmt::If(cond, body) => {
                self.generate_constraints_expr(cond);
                self.constraints
                    .push(Constraint::Symmetric(Type::Boolean, cond.get_type()));
                self.generate_constraints_stmt(body);
            }
            TypedASTStmt::While(cond, body) => {
                self.generate_constraints_expr(cond);
                self.constraints
                    .push(Constraint::Symmetric(Type::Boolean, cond.get_type()));
                self.generate_constraints_stmt(body);
            }
            TypedASTStmt::Print(expr) => {
                self.generate_constraints_expr(expr);
            }
            TypedASTStmt::Return(expr) => {
                self.generate_constraints_expr(expr);
                self.constraints
                    .push(Constraint::Symmetric(Type::Boolean, expr.get_type()));
                self.ret_ty = expr.get_type();
            }
            TypedASTStmt::Verify(expr) => {
                self.generate_constraints_expr(expr);
                self.constraints
                    .push(Constraint::Symmetric(Type::Boolean, expr.get_type()));
            }
            TypedASTStmt::Expression(expr) => {
                self.generate_constraints_expr(expr);
            }
            _ => panic!(),
        }
    }

    fn generate_constraints_expr(&mut self, expr: &'a TypedASTExpr<'a>) {
        match expr {
            TypedASTExpr::Nil => {}
            TypedASTExpr::Boolean(_) => {}
            TypedASTExpr::Number(_) => {}
            TypedASTExpr::String(_) => {}
            TypedASTExpr::Index(tensor, indices) => {
                self.constraints
                    .push(Constraint::Symmetric(Type::Tensor, tensor.get_type()));
                for i in 0..indices.len() {
                    self.constraints.push(Constraint::Symmetric(
                        Type::Number,
                        indices.at(i).get_type(),
                    ));
                }
            }
            TypedASTExpr::ArrayLiteral(elements) => {
                for i in 0..elements.len() {
                    self.constraints.push(Constraint::Symmetric(
                        Type::Number,
                        elements.at(i).get_type(),
                    ));
                }
            }
            TypedASTExpr::Assign(left, right, ty) => {
                self.constraints
                    .push(Constraint::Symmetric(left.get_type(), right.get_type()));
                self.constraints
                    .push(Constraint::Symmetric(right.get_type(), *ty));
            }
            TypedASTExpr::Unary(op, expr, ty) => match op {
                ASTUnaryOp::Not => {
                    self.constraints
                        .push(Constraint::Symmetric(Type::Boolean, expr.get_type()));
                    self.constraints
                        .push(Constraint::Symmetric(Type::Boolean, *ty));
                }
                ASTUnaryOp::Negate => {
                    self.constraints
                        .push(Constraint::Symmetric(Type::Number, expr.get_type()));
                    self.constraints
                        .push(Constraint::Symmetric(Type::Number, *ty));
                }
                ASTUnaryOp::Shape => {
                    self.constraints
                        .push(Constraint::Symmetric(Type::Tensor, expr.get_type()));
                    self.constraints
                        .push(Constraint::Symmetric(Type::Tensor, *ty));
                }
            },
            _ => panic!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_unconstrained1() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f myop (x, y) { r x + y; }", &bump).unwrap();
        let (ast, rest) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();

        assert_eq!(
            unconstrained,
            bump.create_list_with(&[TypedASTStmt::Function(
                b"myop",
                bump.create_list_with(&[b"x" as &[_], b"y" as &[_]]),
                bump.create_list_with(&[Type::Generic(0), Type::Generic(1)]),
                bump.alloc(TypedASTStmt::Block(bump.create_list_with(&[
                    TypedASTStmt::Return(bump.alloc(TypedASTExpr::Binary(
                        ASTBinaryOp::Add,
                        bump.alloc(TypedASTExpr::Identifier(b"x", Type::Generic(2))),
                        bump.alloc(TypedASTExpr::Identifier(b"y", Type::Generic(3))),
                        Type::Generic(4),
                    )))
                ]))),
                Type::Generic(5),
            )])
        );

        assert_eq!(rest, &[]);
    }

    #[test]
    fn generate_unconstrained2() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f myop (x, y) { r x + y; p x; }", &bump).unwrap();
        let (ast, rest) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump);

        assert_eq!(
            unconstrained,
            Err("ERROR: Return statement found before end of function.")
        );
        assert_eq!(rest, &[]);
    }

    #[test]
    fn generate_unconstrained3() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f myop (x, y) { 3 = x; }", &bump).unwrap();
        let (ast, rest) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump);

        assert_eq!(
            unconstrained,
            Err("ERROR: Can only assign to a variable or indexing into a tensor variable.")
        );
        assert_eq!(rest, &[]);
    }
}
