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

pub fn typecheck_program<'a>(
    program: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> TypeResult<&'a bump::List<'a, TypedASTStmt<'a>>> {
    let unconstrained = generate_unconstrained_tree(program, bump);
    Err("Unimplemented!")
}

fn generate_unconstrained_tree<'a>(
    program: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> &'a bump::List<'a, TypedASTStmt<'a>> {
    let unconstrained = bump.create_list();
    for i in 0..program.len() {
        unconstrained.push(generate_unconstrained_stmt(program.at(i), bump));
    }
    unconstrained
}

fn generate_unconstrained_stmt<'a>(
    stmt: &'a ASTStmt<'a>,
    bump: &'a bump::BumpAllocator,
) -> TypedASTStmt<'a> {
    match stmt {
        ASTStmt::Block(stmts) => {
            let contents = bump.create_list();
            for i in 0..stmts.len() {
                contents.push(generate_unconstrained_stmt(stmts.at(i), bump));
            }
            TypedASTStmt::Block(contents)
        }
        _ => panic!(),
    }
}
