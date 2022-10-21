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

#[derive(Debug, PartialEq, Clone)]
pub enum ASTStmt<'a> {
    Block(&'a [ASTStmt<'a>]),
    Function(&'a [u8], &'a bump::List<'a, &'a [u8]>, &'a ASTStmt<'a>),
    Operator(&'a [u8], [&'a [u8]; 2], &'a ASTStmt<'a>),
    If(ASTExpr<'a>, &'a ASTStmt<'a>),
    While(&'a ASTExpr<'a>, &'a ASTStmt<'a>),
    Print(&'a ASTExpr<'a>),
    Return(&'a ASTExpr<'a>),
    Verify(&'a ASTExpr<'a>),
    Variable(&'a [u8], ASTExpr<'a>),
    Expression(&'a ASTExpr<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTExpr<'a> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(&'a [u8]),
    Identifier(&'a [u8]),
    Call(&'a [u8], &'a bump::List<'a, &'a ASTExpr<'a>>),
    Index(&'a ASTExpr<'a>, &'a bump::List<'a, &'a ASTExpr<'a>>),
    ArrayLiteral(&'a bump::List<'a, &'a ASTExpr<'a>>),
    Unary(ASTUnaryOp, &'a ASTExpr<'a>),
    Binary(ASTBinaryOp, &'a ASTExpr<'a>, &'a ASTExpr<'a>),
    CustomBinary(&'a [u8], &'a ASTExpr<'a>, &'a ASTExpr<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTUnaryOp {
    Shape,
    Negate,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
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
    Assign,
}

/*const LEFT_ASSOC_BINARY_PRECEDENCE: &[&[ASTBinaryOp]] = &[
    &[ASTBinaryOp::Or],
    &[ASTBinaryOp::And],
    &[ASTBinaryOp::EqualsEquals, ASTBinaryOp::NotEquals],
    &[
        ASTBinaryOp::GreaterEquals,
        ASTBinaryOp::LesserEquals,
        ASTBinaryOp::Greater,
        ASTBinaryOp::Lesser,
    ],
    &[ASTBinaryOp::Add, ASTBinaryOp::Subtract],
    &[
        ASTBinaryOp::Multiply,
        ASTBinaryOp::Divide,
        ASTBinaryOp::Power,
        ASTBinaryOp::MatrixMultiply,
        ASTBinaryOp::ShapedAs,
    ],
];*/

pub fn parse_stmt<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b ASTStmt<'b>, &'a [lex::Token<'b>])> {
    None
}

fn parse_expr<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b ASTExpr<'b>, &'a [lex::Token<'b>])> {
    parse_primary(tokens, bump)
}

fn parse_primary<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b ASTExpr<'b>, &'a [lex::Token<'b>])> {
    combi::parse_or(
        tokens,
        &[
            &parse_identifier,
            &parse_string,
            &parse_number,
            &parse_boolean,
            &parse_nil,
            &parse_parentheses,
            &parse_array_literal,
        ],
        bump,
    )
}

fn parse_parentheses<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b ASTExpr<'b>, &'a [lex::Token<'b>])> {
    let mut cp = bump.create_checkpoint();
    let rest = combi::parse_token_consume(tokens, lex::Token::LeftParen)?;
    let (expr, rest) = parse_expr(rest, bump)?;
    let rest = combi::parse_token_consume(rest, lex::Token::RightParen)?;
    cp.commit();
    Some((expr, rest))
}

fn parse_array_literal<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(&'b ASTExpr<'b>, &'a [lex::Token<'b>])> {
    let mut cp = bump.create_checkpoint();
    let rest = combi::parse_token_consume(tokens, lex::Token::LeftBracket)?;
    let list = bump.create_list();
    let (expr, mut rest) = parse_expr(rest, bump)?;
    list.push(expr);
    while let Some(rest_comma) = combi::parse_token_consume(rest, lex::Token::Comma) {
        let (expr, rest_new) = parse_expr(rest_comma, bump)?;
        list.push(expr);
        rest = rest_new;
    }
    let rest = combi::parse_token_consume(rest, lex::Token::RightBracket)?;
    cp.commit();
    Some((bump.alloc(ASTExpr::ArrayLiteral(list)), rest))
}

macro_rules! define_simple_expr_parse {
    ($x:ident, $y: expr) => {
        fn $x<'a, 'b>(
            tokens: &'a [lex::Token<'b>],
            bump: &'b bump::BumpAllocator,
        ) -> Option<(&'b ASTExpr<'b>, &'a [lex::Token<'b>])> {
            combi::parse_token(tokens, $y, bump)
        }
    };
}

define_simple_expr_parse!(parse_identifier, &|c| match c {
    lex::Token::Identifier(i) => Some(ASTExpr::Identifier(i)),
    _ => None,
});

define_simple_expr_parse!(parse_string, &|c| match c {
    lex::Token::String(s) => Some(ASTExpr::String(s)),
    _ => None,
});

define_simple_expr_parse!(parse_number, &|c| match c {
    lex::Token::Number(n) => Some(ASTExpr::Number(n)),
    _ => None,
});

define_simple_expr_parse!(parse_boolean, &|c| match c {
    lex::Token::True => Some(ASTExpr::Boolean(true)),
    lex::Token::False => Some(ASTExpr::Boolean(false)),
    _ => None,
});

define_simple_expr_parse!(parse_nil, &|c| match c {
    lex::Token::Nil => Some(ASTExpr::Nil),
    _ => None,
});

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_primary1() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"1.22387", &bump).unwrap();
        let (ast, rest) = parse_primary(&tokens, &bump).unwrap();
        assert_eq!(ast, &ASTExpr::Number(1.22387));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_primary2() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"\"This is a string!\"", &bump).unwrap();
        let (ast, rest) = parse_primary(&tokens, &bump).unwrap();
        assert_eq!(ast, &ASTExpr::String(b"This is a string!"));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_primary3() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"((((my_identifier))))", &bump).unwrap();
        let (ast, rest) = parse_primary(&tokens, &bump).unwrap();
        assert_eq!(ast, &ASTExpr::Identifier(b"my_identifier"));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_primary4() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"[x, y, z]", &bump).unwrap();
        let (ast, rest) = parse_primary(&tokens, &bump).unwrap();
        let correct_list = bump.create_list();
        let x_expr = bump.alloc(ASTExpr::Identifier(b"x")) as &_;
        let y_expr = bump.alloc(ASTExpr::Identifier(b"y")) as &_;
        let z_expr = bump.alloc(ASTExpr::Identifier(b"z")) as &_;
        correct_list.push(x_expr);
        correct_list.push(y_expr);
        correct_list.push(z_expr);
        assert_eq!(ast, &ASTExpr::ArrayLiteral(correct_list));
        assert_eq!(rest, &[]);
    }
}
