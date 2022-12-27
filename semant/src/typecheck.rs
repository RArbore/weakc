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

impl Type {
    fn is_gen(&self) -> Option<u32> {
        match self {
            Type::Generic(var) | Type::Numeric(var) => Some(*var),
            _ => None,
        }
    }
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
    Conformant(Type, Type, u32),
}

#[derive(Debug, PartialEq)]
struct TypeContext<'a> {
    num_generics: u32,
    num_callsites: u32,
    constraints: &'a mut bump::List<'a, Constraint>,
    funcs: HashMap<&'a [u8], (&'a bump::List<'a, Type>, Type)>,
    ops: HashMap<&'a [u8], (Type, Type, Type)>,
    vars: HashMap<&'a [u8], Type>,
    ret_ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedProgram<'a>(pub &'a bump::List<'a, TypedASTStmt<'a>>, pub &'a [Type]);

pub fn typecheck_program<'a>(
    program: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> TypeResult<TypedProgram> {
    let mut context = TypeContext::new(&bump);
    let unconstrained = context.generate_unconstrained_tree(program, bump)?;
    let num_pure_generics = context.num_generics;
    context.generate_constraints_tree(unconstrained)?;
    let types = context.constrain_types(num_pure_generics, bump)?;
    Ok(TypedProgram(unconstrained, types))
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
        (Type::Numeric(_), Type::Numeric(v)) => Ok(Type::Numeric(v)),
        (Type::Generic(_), ty) => Ok(ty),
        (ty, Type::Generic(_)) => Ok(ty),
        _ => Err("ERROR: Could not join incompatible types."),
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
            num_callsites: 0,
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

    fn generate_numeric(&mut self) -> Type {
        let ty = Type::Numeric(self.num_generics);
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

    fn generate_constraints_tree(
        &mut self,
        program: &'a bump::List<'a, TypedASTStmt<'a>>,
    ) -> TypeResult<()> {
        for i in 0..program.len() {
            self.generate_constraints_stmt(program.at(i))?;
        }
        Ok(())
    }

    fn generate_constraints_stmt(&mut self, stmt: &'a TypedASTStmt<'a>) -> TypeResult<()> {
        match stmt {
            TypedASTStmt::Block(stmts) => {
                for i in 0..stmts.len() {
                    self.generate_constraints_stmt(stmts.at(i))?;
                }
                Ok(())
            }
            TypedASTStmt::Function(op, params, params_ty, body, ret_ty) => {
                self.ret_ty = Type::Nil;
                self.funcs.insert(op, (params_ty, *ret_ty));
                let mut old_vars = HashMap::new();
                swap(&mut self.vars, &mut old_vars);
                for i in 0..params.len() {
                    self.vars.insert(params.at(i), *params_ty.at(i));
                }
                self.generate_constraints_stmt(body)?;
                self.constraints
                    .push(Constraint::Symmetric(self.ret_ty, *ret_ty));
                swap(&mut self.vars, &mut old_vars);
                Ok(())
            }
            TypedASTStmt::Operator(op, left, right, left_ty, right_ty, body, ret_ty) => {
                self.ret_ty = Type::Nil;
                self.ops.insert(op, (*left_ty, *right_ty, *ret_ty));
                let mut old_vars = HashMap::new();
                swap(&mut self.vars, &mut old_vars);
                self.vars.insert(left, *left_ty);
                self.vars.insert(right, *right_ty);
                self.generate_constraints_stmt(body)?;
                self.constraints
                    .push(Constraint::Symmetric(self.ret_ty, *ret_ty));
                swap(&mut self.vars, &mut old_vars);
                Ok(())
            }
            TypedASTStmt::If(cond, body) => {
                self.generate_constraints_expr(cond)?;
                self.constraints
                    .push(Constraint::Symmetric(Type::Boolean, cond.get_type()));
                self.generate_constraints_stmt(body)?;
                Ok(())
            }
            TypedASTStmt::While(cond, body) => {
                self.generate_constraints_expr(cond)?;
                self.constraints
                    .push(Constraint::Symmetric(Type::Boolean, cond.get_type()));
                self.generate_constraints_stmt(body)?;
                Ok(())
            }
            TypedASTStmt::Print(expr) => {
                self.generate_constraints_expr(expr)?;
                Ok(())
            }
            TypedASTStmt::Return(expr) => {
                self.generate_constraints_expr(expr)?;
                self.ret_ty = expr.get_type();
                Ok(())
            }
            TypedASTStmt::Verify(expr) => {
                self.generate_constraints_expr(expr)?;
                self.constraints
                    .push(Constraint::Symmetric(Type::Boolean, expr.get_type()));
                Ok(())
            }
            TypedASTStmt::Variable(name, expr) => {
                self.generate_constraints_expr(expr)?;
                self.vars.insert(name, expr.get_type());
                Ok(())
            }
            TypedASTStmt::Expression(expr) => {
                self.generate_constraints_expr(expr)?;
                Ok(())
            }
        }
    }

    fn generate_constraints_expr(&mut self, expr: &'a TypedASTExpr<'a>) -> TypeResult<()> {
        match expr {
            TypedASTExpr::Nil => Ok(()),
            TypedASTExpr::Boolean(_) => Ok(()),
            TypedASTExpr::Number(_) => Ok(()),
            TypedASTExpr::String(_) => Ok(()),
            TypedASTExpr::Identifier(name, ty) => {
                if let Some(decl_ty) = self.vars.get(name) {
                    self.constraints.push(Constraint::Symmetric(*decl_ty, *ty));
                    Ok(())
                } else {
                    Err("ERROR: Attempted to use variable not currently in scope.")?
                }
            }
            TypedASTExpr::Call(op, args, ty) => {
                for i in 0..args.len() {
                    self.generate_constraints_expr(args.at(i))?;
                }
                if let Some((params_ty, ret_ty)) = self.funcs.get(op) {
                    if params_ty.len() == args.len() {
                        for i in 0..params_ty.len() {
                            self.constraints.push(Constraint::Conformant(
                                *params_ty.at(i),
                                args.at(i).get_type(),
                                self.num_callsites,
                            ));
                        }
                        self.constraints.push(Constraint::Conformant(
                            *ret_ty,
                            *ty,
                            self.num_callsites,
                        ));
                        self.num_callsites += 1;
                        Ok(())
                    } else {
                        Err("ERROR: Attempted to call function with wrong number of arguments.")?
                    }
                } else {
                    Err("ERROR: Attempted to use function not currently in scope.")?
                }
            }
            TypedASTExpr::Index(tensor, indices) => {
                self.generate_constraints_expr(tensor)?;
                for i in 0..indices.len() {
                    self.generate_constraints_expr(indices.at(i))?;
                }
                self.constraints
                    .push(Constraint::Symmetric(Type::Tensor, tensor.get_type()));
                for i in 0..indices.len() {
                    self.constraints.push(Constraint::Symmetric(
                        Type::Number,
                        indices.at(i).get_type(),
                    ));
                }
                Ok(())
            }
            TypedASTExpr::ArrayLiteral(elements) => {
                for i in 0..elements.len() {
                    self.generate_constraints_expr(elements.at(i))?;
                }
                for i in 0..elements.len() {
                    self.constraints.push(Constraint::Symmetric(
                        Type::Number,
                        elements.at(i).get_type(),
                    ));
                }
                Ok(())
            }
            TypedASTExpr::Assign(left, right, ty) => {
                self.generate_constraints_expr(left)?;
                self.generate_constraints_expr(right)?;
                self.constraints
                    .push(Constraint::Symmetric(left.get_type(), right.get_type()));
                self.constraints
                    .push(Constraint::Symmetric(right.get_type(), *ty));
                Ok(())
            }
            TypedASTExpr::Unary(op, expr, ty) => {
                self.generate_constraints_expr(expr)?;
                match op {
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
                }
                Ok(())
            }
            TypedASTExpr::Binary(op, left, right, ty) => {
                self.generate_constraints_expr(left)?;
                self.generate_constraints_expr(right)?;
                match op {
                    ASTBinaryOp::ShapedAs | ASTBinaryOp::MatrixMultiply => {
                        self.constraints
                            .push(Constraint::Symmetric(Type::Tensor, left.get_type()));
                        self.constraints
                            .push(Constraint::Symmetric(Type::Tensor, right.get_type()));
                        self.constraints
                            .push(Constraint::Symmetric(Type::Tensor, *ty));
                        Ok(())
                    }
                    ASTBinaryOp::Add
                    | ASTBinaryOp::Subtract
                    | ASTBinaryOp::Multiply
                    | ASTBinaryOp::Divide
                    | ASTBinaryOp::Power => {
                        let numeric_var = self.generate_numeric();
                        self.constraints
                            .push(Constraint::Symmetric(numeric_var, left.get_type()));
                        self.constraints
                            .push(Constraint::Symmetric(numeric_var, right.get_type()));
                        self.constraints
                            .push(Constraint::Symmetric(numeric_var, *ty));
                        Ok(())
                    }
                    ASTBinaryOp::Greater
                    | ASTBinaryOp::Lesser
                    | ASTBinaryOp::GreaterEquals
                    | ASTBinaryOp::LesserEquals => {
                        self.constraints
                            .push(Constraint::Symmetric(Type::Number, left.get_type()));
                        self.constraints
                            .push(Constraint::Symmetric(Type::Number, right.get_type()));
                        self.constraints
                            .push(Constraint::Symmetric(Type::Boolean, *ty));
                        Ok(())
                    }
                    ASTBinaryOp::EqualsEquals | ASTBinaryOp::NotEquals => {
                        self.constraints
                            .push(Constraint::Symmetric(left.get_type(), right.get_type()));
                        self.constraints
                            .push(Constraint::Symmetric(Type::Boolean, *ty));
                        Ok(())
                    }
                    ASTBinaryOp::And | ASTBinaryOp::Or => {
                        self.constraints
                            .push(Constraint::Symmetric(Type::Boolean, left.get_type()));
                        self.constraints
                            .push(Constraint::Symmetric(Type::Boolean, right.get_type()));
                        self.constraints
                            .push(Constraint::Symmetric(Type::Boolean, *ty));
                        Ok(())
                    }
                }
            }
            TypedASTExpr::CustomBinary(op, left, right, ty) => {
                self.generate_constraints_expr(left)?;
                self.generate_constraints_expr(right)?;
                if let Some((left_ty, right_ty, ret_ty)) = self.ops.get(op) {
                    self.constraints.push(Constraint::Conformant(
                        *left_ty,
                        left.get_type(),
                        self.num_callsites,
                    ));
                    self.constraints.push(Constraint::Conformant(
                        *right_ty,
                        right.get_type(),
                        self.num_callsites,
                    ));
                    self.constraints
                        .push(Constraint::Conformant(*ret_ty, *ty, self.num_callsites));
                    self.num_callsites += 1;
                    Ok(())
                } else {
                    Err("ERROR: Attempted to use operation not currently in scope.")?
                }
            }
        }
    }

    fn constrain_types(
        &mut self,
        num_pure_generics: u32,
        bump: &'a bump::BumpAllocator,
    ) -> TypeResult<&'a [Type]> {
        let types = unsafe { bump.alloc_slice_raw(self.num_generics as usize) };
        let types_clone = unsafe { bump.alloc_slice_raw(self.num_generics as usize) };

        for i in 0..self.num_generics {
            types[i as usize] = if i < num_pure_generics {
                Type::Generic(i)
            } else {
                Type::Numeric(i)
            };
        }

        let traverse = |mut idx: u32, types: &[Type]| {
            let mut num_iters = 0;
            while let Some(new_idx) = types[idx as usize].is_gen() {
                if num_iters > types.len() {
                    return idx as usize;
                }
                if new_idx == idx {
                    return idx as usize;
                } else {
                    idx = new_idx;
                }
                num_iters += 1;
            }
            return idx as usize;
        };

        let get_concrete_type = |ty: Type, types: &[Type]| {
            if let Some(idx) = ty.is_gen() {
                types[traverse(idx, types)]
            } else {
                ty
            }
        };

        let cleanup_types = |types: &mut [Type]| {
            for i in 0..types.len() {
                types[i] = types[traverse(i as u32, types)];
            }
        };

        let enforce_symmetric = |ty1: Type, ty2: Type, types: &mut [Type]| -> TypeResult<()> {
            let joined = join_types(get_concrete_type(ty1, types), get_concrete_type(ty2, types))?;
            if let Some(idx) = ty1.is_gen() {
                types[traverse(idx, types)] = joined;
            }
            if let Some(idx) = ty2.is_gen() {
                types[traverse(idx, types)] = joined;
            }
            Ok(())
        };

        let mut i = 0;

        while i < self.constraints.len() {
            match self.constraints.at(i) {
                Constraint::Symmetric(ty1, ty2) => {
                    enforce_symmetric(*ty1, *ty2, types)?;
                    i += 1;
                }
                Constraint::Conformant(_, _, callsite) => {
                    let mut j = i;
                    let mut constraints = vec![];
                    while j < self.constraints.len() {
                        match self.constraints.at(j) {
                            Constraint::Conformant(ty1, ty2, future_callsite) => {
                                if callsite != future_callsite {
                                    break;
                                } else {
                                    constraints.push((ty1, ty2));
                                }
                            }
                            _ => break,
                        }
                        j += 1;
                    }
                    for k in 0..types.len() {
                        types_clone[k] = types[k];
                    }

                    let mut first_gen = -1;
                    let mut latest_gen = -1;

                    for (ty1, ty2) in constraints {
                        enforce_symmetric(*ty1, *ty2, types)?;
                        if let Some(var) = ty1.is_gen() {
                            if first_gen == -1 {
                                first_gen = var as i64;
                            }
                            latest_gen = var as i64;
                        }
                    }

                    for k in std::cmp::max(0, first_gen)..=latest_gen {
                        types[traverse(k as u32, types)] =
                            types_clone[traverse(k as u32, types_clone)];
                    }
                    i = j;
                }
            }
        }

        cleanup_types(types);

        Ok(types)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_unconstrained1() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f myop (x, y) { r x + y; }", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

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
    }

    #[test]
    fn generate_unconstrained2() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f myop (x, y) { r x + y; p x; }", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump);

        assert_eq!(
            unconstrained,
            Err("ERROR: Return statement found before end of function.")
        );
    }

    #[test]
    fn generate_unconstrained3() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f myop (x, y) { 3 = x; }", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump);

        assert_eq!(
            unconstrained,
            Err("ERROR: Can only assign to a variable or indexing into a tensor variable.")
        );
    }

    #[test]
    fn generate_constraints1() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f myop (x, y) { r x; }", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        context.generate_constraints_tree(unconstrained).unwrap();
        assert_eq!(
            context.constraints,
            bump.create_list_with(&[
                Constraint::Symmetric(Type::Generic(0), Type::Generic(2)),
                Constraint::Symmetric(Type::Generic(2), Type::Generic(3))
            ])
        );
    }

    #[test]
    fn generate_constraints2() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f myop (x, y) { r x + y; }", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        context.generate_constraints_tree(unconstrained).unwrap();
        assert_eq!(
            context.constraints,
            bump.create_list_with(&[
                Constraint::Symmetric(Type::Generic(0), Type::Generic(2)),
                Constraint::Symmetric(Type::Generic(1), Type::Generic(3)),
                Constraint::Symmetric(Type::Numeric(6), Type::Generic(2)),
                Constraint::Symmetric(Type::Numeric(6), Type::Generic(3)),
                Constraint::Symmetric(Type::Numeric(6), Type::Generic(4)),
                Constraint::Symmetric(Type::Generic(4), Type::Generic(5))
            ])
        );
    }

    #[test]
    fn generate_constraints3() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f myop (x, y) { p y; r x + 3; } p myop(8, \"Hello\");",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        context.generate_constraints_tree(unconstrained).unwrap();
        assert_eq!(
            context.constraints,
            bump.create_list_with(&[
                Constraint::Symmetric(Type::Generic(1), Type::Generic(2)),
                Constraint::Symmetric(Type::Generic(0), Type::Generic(3)),
                Constraint::Symmetric(Type::Numeric(7), Type::Generic(3)),
                Constraint::Symmetric(Type::Numeric(7), Type::Number),
                Constraint::Symmetric(Type::Numeric(7), Type::Generic(4)),
                Constraint::Symmetric(Type::Generic(4), Type::Generic(5)),
                Constraint::Conformant(Type::Generic(0), Type::Number, 0),
                Constraint::Conformant(Type::Generic(1), Type::String, 0),
                Constraint::Conformant(Type::Generic(5), Type::Generic(6), 0)
            ])
        );
    }

    #[test]
    fn generate_constraints4() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"p [1, 2, 3, 4] sa [2, 2]; p 5 - 7; p \"hello\"; p ([1, 2, 3, 4] sa [1, 4]) @ ([5, 6, 7, 8] sa [4, 1]); p 9 ^ 2;",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        context.generate_constraints_tree(unconstrained).unwrap();
        assert_eq!(
            context.constraints,
            bump.create_list_with(&[
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Tensor, Type::Tensor),
                Constraint::Symmetric(Type::Tensor, Type::Tensor),
                Constraint::Symmetric(Type::Tensor, Type::Generic(0)),
                Constraint::Symmetric(Type::Numeric(6), Type::Number),
                Constraint::Symmetric(Type::Numeric(6), Type::Number),
                Constraint::Symmetric(Type::Numeric(6), Type::Generic(1)),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Tensor, Type::Tensor),
                Constraint::Symmetric(Type::Tensor, Type::Tensor),
                Constraint::Symmetric(Type::Tensor, Type::Generic(2)),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Tensor, Type::Tensor),
                Constraint::Symmetric(Type::Tensor, Type::Tensor),
                Constraint::Symmetric(Type::Tensor, Type::Generic(3)),
                Constraint::Symmetric(Type::Tensor, Type::Generic(2)),
                Constraint::Symmetric(Type::Tensor, Type::Generic(3)),
                Constraint::Symmetric(Type::Tensor, Type::Generic(4)),
                Constraint::Symmetric(Type::Numeric(7), Type::Number),
                Constraint::Symmetric(Type::Numeric(7), Type::Number),
                Constraint::Symmetric(Type::Numeric(7), Type::Generic(5))
            ])
        );
    }

    #[test]
    fn generate_constraints5() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f myop (x, y) { r x + y; } p myop(1, 2); p myop([1], [2]); p myop(myop(4, 5), myop(myop(1, 1), 3));",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        context.generate_constraints_tree(unconstrained).unwrap();
        assert_eq!(
            context.constraints,
            bump.create_list_with(&[
                Constraint::Symmetric(Type::Generic(0), Type::Generic(2)),
                Constraint::Symmetric(Type::Generic(1), Type::Generic(3)),
                Constraint::Symmetric(Type::Numeric(12), Type::Generic(2)),
                Constraint::Symmetric(Type::Numeric(12), Type::Generic(3)),
                Constraint::Symmetric(Type::Numeric(12), Type::Generic(4)),
                Constraint::Symmetric(Type::Generic(4), Type::Generic(5)),
                Constraint::Conformant(Type::Generic(0), Type::Number, 0),
                Constraint::Conformant(Type::Generic(1), Type::Number, 0),
                Constraint::Conformant(Type::Generic(5), Type::Generic(6), 0),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Symmetric(Type::Number, Type::Number),
                Constraint::Conformant(Type::Generic(0), Type::Tensor, 1),
                Constraint::Conformant(Type::Generic(1), Type::Tensor, 1),
                Constraint::Conformant(Type::Generic(5), Type::Generic(7), 1),
                Constraint::Conformant(Type::Generic(0), Type::Number, 2),
                Constraint::Conformant(Type::Generic(1), Type::Number, 2),
                Constraint::Conformant(Type::Generic(5), Type::Generic(8), 2),
                Constraint::Conformant(Type::Generic(0), Type::Number, 3),
                Constraint::Conformant(Type::Generic(1), Type::Number, 3),
                Constraint::Conformant(Type::Generic(5), Type::Generic(9), 3),
                Constraint::Conformant(Type::Generic(0), Type::Generic(9), 4),
                Constraint::Conformant(Type::Generic(1), Type::Number, 4),
                Constraint::Conformant(Type::Generic(5), Type::Generic(10), 4),
                Constraint::Conformant(Type::Generic(0), Type::Generic(8), 5),
                Constraint::Conformant(Type::Generic(1), Type::Generic(10), 5),
                Constraint::Conformant(Type::Generic(5), Type::Generic(11), 5)
            ])
        );
    }

    #[test]
    fn generate_types1() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"p N; p [10, -1.0, 3, 4] sa [2, 2]; p 10 - 3; p \"hello\";",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[Type::Number, Type::Tensor, Type::Number, Type::Number]
        );
    }

    #[test]
    fn generate_types2() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f myop1(x, y) { r x + y; } f myop2(x) { r x + 3; } p N;",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[
                Type::Numeric(10),
                Type::Numeric(10),
                Type::Numeric(10),
                Type::Numeric(10),
                Type::Numeric(10),
                Type::Numeric(10),
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Numeric(10),
                Type::Number
            ]
        );
    }

    #[test]
    fn generate_types3() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"[3, 5] ^ [1, 2]; p N; p [1, 2, 3] + [3 ^ 7 / 8, 2, 1 + 5]; p \"hello there\";",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Tensor,
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Tensor
            ]
        );
    }

    #[test]
    fn generate_types4() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"o op(x, y) { r x ^ (y + 3); }", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number
            ]
        );
    }

    #[test]
    fn generate_types5() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f myop(x, y) { r x + y; } p myop(3, 5);", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[
                Type::Numeric(7),
                Type::Numeric(7),
                Type::Numeric(7),
                Type::Numeric(7),
                Type::Numeric(7),
                Type::Numeric(7),
                Type::Number,
                Type::Numeric(7)
            ]
        );
    }

    #[test]
    fn generate_types6() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f myop(x, y) { r x + y; } p myop(3, 5); p myop([3], [5]);",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[
                Type::Numeric(8),
                Type::Numeric(8),
                Type::Numeric(8),
                Type::Numeric(8),
                Type::Numeric(8),
                Type::Numeric(8),
                Type::Number,
                Type::Tensor,
                Type::Numeric(8)
            ]
        );
    }

    #[test]
    fn generate_types7() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"o myop(x, y) { r x + y; } f xyz(x, y, z, ww) { p ww; r x + y + z; } p xyz(5, 2 myop 8, 3, N);",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[
                Type::Numeric(19),
                Type::Numeric(19),
                Type::Numeric(19),
                Type::Numeric(19),
                Type::Numeric(19),
                Type::Numeric(19),
                Type::Numeric(20),
                Type::Numeric(20),
                Type::Numeric(20),
                Type::Generic(10),
                Type::Generic(10),
                Type::Numeric(20),
                Type::Numeric(20),
                Type::Numeric(20),
                Type::Numeric(20),
                Type::Numeric(20),
                Type::Numeric(20),
                Type::Number,
                Type::Number,
                Type::Numeric(19),
                Type::Numeric(20),
                Type::Numeric(20)
            ]
        );
    }

    #[test]
    fn generate_types8() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f ab(x) { f cd(x) { r x; } r cd(x); } p ab(5);", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[
                Type::Generic(6),
                Type::Generic(6),
                Type::Generic(6),
                Type::Generic(6),
                Type::Generic(6),
                Type::Generic(6),
                Type::Generic(6),
                Type::Number
            ]
        );
    }

    #[test]
    fn generate_types9() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f ab(x) { f cd(x, y) { f ef(x) { r x; } r ef(x) + y; } r cd(3, x); } p ab(5);",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[
                Type::Number,
                Type::Numeric(15),
                Type::Numeric(15),
                Type::Numeric(15),
                Type::Numeric(15),
                Type::Numeric(15),
                Type::Numeric(15),
                Type::Numeric(15),
                Type::Numeric(15),
                Type::Numeric(15),
                Type::Numeric(15),
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Numeric(15),
            ]
        );
    }

    #[test]
    fn generate_types10() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f dim(mat) {    r (s (s mat))[0];}f len(list) {    v dim(list) == 1;    r (s list)[0];}f part_one(depths) {    v dim(depths) == 1;    a len = len(depths);    v len >= 1;    a j = 1;    a count = 0;    w (j < len) {        i (depths[j] > depths[j-1]) {            count = count + 1;        }        j = j + 1;    }    r count;}f part_two(depths) {    v dim(depths) == 1;    a len = len(depths);    v len >= 1;    a j = 0;    a new_depths = [0] sa [len];    a count = 0;    w (j < len - 2) {        new_depths[count] = depths[j] + depths[j+1] + depths[j+2];        count = count + 1;        j = j + 1;    }    r part_one(new_depths);}a d = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263];p part_one(d); p part_two(d);",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

        let mut context = TypeContext::new(&bump);
        let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
        let num_pure_generics = context.num_generics;
        context.generate_constraints_tree(unconstrained).unwrap();
        let types = context.constrain_types(num_pure_generics, &bump).unwrap();
        assert_eq!(
            types,
            &[
                Type::Tensor,
                Type::Tensor,
                Type::Tensor,
                Type::Tensor,
                Type::Number,
                Type::Tensor,
                Type::Tensor,
                Type::Number,
                Type::Boolean,
                Type::Tensor,
                Type::Tensor,
                Type::Number,
                Type::Tensor,
                Type::Tensor,
                Type::Number,
                Type::Boolean,
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Boolean,
                Type::Number,
                Type::Number,
                Type::Boolean,
                Type::Tensor,
                Type::Number,
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Boolean,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Tensor,
                Type::Tensor,
                Type::Number,
                Type::Boolean,
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Boolean,
                Type::Number,
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Boolean,
                Type::Tensor,
                Type::Number,
                Type::Tensor,
                Type::Number,
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Tensor,
                Type::Number,
                Type::Tensor,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number,
                Type::Number
            ]
        );
    }

    #[test]
    fn generate_types11() {
        let bad_programs = &[
            b"f ab(x) { f cd(x, y) { f ef(x) { r x; } r ef(x) + y; } r cd(3, x); } p ab(\"uh oh\");"
                as &[_],
            b"3 + N;",
            b"3 + 9; f xx(x) { r x + 3; } xx([3]);",
            b"f ab(x) { f cd(x, y) { f ef(x) { r x; } r ef(x) + y; } r cd([3], x); } p ab(3);",
            b"(3 > 5) + 1;",
            b"v 5 + 3;",
            b"5 @ 3;",
            b"\"hhh\" + \"aaa\";",
        ];
        for bad_program in bad_programs {
            let bump = bump::BumpAllocator::new();
            let tokens = parse::lex(bad_program, &bump).unwrap();
            let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();

            let mut context = TypeContext::new(&bump);
            let unconstrained = context.generate_unconstrained_tree(ast, &bump).unwrap();
            let num_pure_generics = context.num_generics;
            context.generate_constraints_tree(unconstrained).unwrap();
            let types = context.constrain_types(num_pure_generics, &bump);
            assert_eq!(types, Err("ERROR: Could not join incompatible types."));
        }
    }

    #[test]
    fn full_typecheck1() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f xyz(x, y) { r x + y; } p xyz(1, 2);", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = typecheck_program(ast, &bump).unwrap();
        assert_eq!(
            typed_program,
            TypedProgram(
                bump.create_list_with(&[
                    TypedASTStmt::Function(
                        b"xyz" as &[u8],
                        bump.create_list_with(&[b"x" as &[u8], b"y"]),
                        bump.create_list_with(&[Type::Generic(0), Type::Generic(1)]),
                        bump.alloc(TypedASTStmt::Block(bump.create_list_with(&[
                            TypedASTStmt::Return(bump.alloc(TypedASTExpr::Binary(
                                ASTBinaryOp::Add,
                                bump.alloc(TypedASTExpr::Identifier(
                                    b"x" as &[u8],
                                    Type::Generic(2)
                                )),
                                bump.alloc(TypedASTExpr::Identifier(
                                    b"y" as &[u8],
                                    Type::Generic(3)
                                )),
                                Type::Generic(4)
                            )))
                        ]))),
                        Type::Generic(5)
                    ),
                    TypedASTStmt::Print(bump.alloc(TypedASTExpr::Call(
                        b"xyz" as &[u8],
                        bump.create_list_with(&[
                            TypedASTExpr::Number(1.0),
                            TypedASTExpr::Number(2.0)
                        ]),
                        Type::Generic(6)
                    )))
                ]),
                &[
                    Type::Numeric(7),
                    Type::Numeric(7),
                    Type::Numeric(7),
                    Type::Numeric(7),
                    Type::Numeric(7),
                    Type::Numeric(7),
                    Type::Number,
                    Type::Numeric(7)
                ]
            ),
        );
    }
}
