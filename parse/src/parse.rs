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

use super::combi;
use super::lex;

pub enum ASTStmt<'a> {
    Block(&'a [ASTStmt<'a>]),
    Function(&'a [u8], &'a [&'a [u8]], &'a ASTStmt<'a>),
    Operator(&'a [u8], [&'a [u8]; 2], &'a ASTStmt<'a>),
    If(ASTExpr<'a>, &'a ASTStmt<'a>),
    While(ASTExpr<'a>, &'a ASTStmt<'a>),
    Print(ASTExpr<'a>),
    Return(ASTExpr<'a>),
    Verify(ASTExpr<'a>),
    Variable(&'a [u8], ASTExpr<'a>),
}

pub enum ASTExpr<'a> {
    Nil,
    Boolean(bool),
    Number(&'a f64),
    String(&'a [u8]),
    Identifier(&'a [u8]),
    Assign(&'a ASTExpr<'a>, &'a ASTExpr<'a>),
    Call(&'a [u8], &'a [&'a ASTExpr<'a>]),
    Index(&'a [u8], &'a [&'a ASTExpr<'a>]),
    Unary(ASTUnaryOp, &'a ASTExpr<'a>),
    Binary(ASTBinaryOp, &'a ASTExpr<'a>, &'a ASTExpr<'a>),
}

pub enum ASTUnaryOp {
    Shape,
    Negate,
    Not,
}

pub enum ASTBinaryOp {
    ShapedAs,
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    MatrixMultiply,
    Greater,
    Lesser,
    NotEquals,
    EqualsEquals,
    GreaterEquals,
    LesserEquals,
    And,
    Or,
}

pub fn parse<'a>(tokens: &[lex::Token], bump: &'a bump::BumpAllocator) -> ASTStmt<'a> {
    ASTStmt::Block(&[])
}

pub fn parse_boolean<'a, 'b>(
    tokens: &'a [lex::Token<'a>],
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b ASTExpr<'b>, &'a [lex::Token<'a>])> {
    combi::parse_token(
        tokens,
        &|c| match c {
            lex::Token::True => Some(ASTExpr::Boolean(true)),
            lex::Token::False => Some(ASTExpr::Boolean(false)),
            _ => None,
        },
        bump,
    )
}

pub fn parse_nil<'a, 'b>(
    tokens: &'a [lex::Token<'a>],
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b ASTExpr<'b>, &'a [lex::Token<'a>])> {
    combi::parse_token(
        tokens,
        &|c| match c {
            lex::Token::Nil => Some(ASTExpr::Nil),
            _ => None,
        },
        bump,
    )
}
