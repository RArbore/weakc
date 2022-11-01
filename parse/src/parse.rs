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
    Call(&'a [u8], &'a bump::List<'a, ASTExpr<'a>>),
    Index(&'a ASTExpr<'a>, &'a bump::List<'a, ASTExpr<'a>>),
    ArrayLiteral(&'a bump::List<'a, ASTExpr<'a>>),
    Unary(ASTUnaryOp, &'a ASTExpr<'a>),
    Binary(ASTBinaryOp, &'a ASTExpr<'a>, &'a ASTExpr<'a>),
    CustomBinary(&'a [u8], &'a ASTExpr<'a>, &'a ASTExpr<'a>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ASTUnaryOp {
    Not,
    Negate,
    Shape,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

const OR_OPS: &[(&[lex::Token], ASTBinaryOp)] = &[(&[lex::Token::Or], ASTBinaryOp::Or)];

const AND_OPS: &[(&[lex::Token], ASTBinaryOp)] = &[(&[lex::Token::And], ASTBinaryOp::And)];

const EQUALITY_OPS: &[(&[lex::Token], ASTBinaryOp)] = &[
    (&[lex::Token::EqualsEquals], ASTBinaryOp::EqualsEquals),
    (&[lex::Token::ExclamationEquals], ASTBinaryOp::NotEquals),
];

const COMPARISON_OPS: &[(&[lex::Token], ASTBinaryOp)] = &[
    (&[lex::Token::GreaterEquals], ASTBinaryOp::GreaterEquals),
    (&[lex::Token::Greater], ASTBinaryOp::Greater),
    (&[lex::Token::LesserEquals], ASTBinaryOp::LesserEquals),
    (&[lex::Token::Lesser], ASTBinaryOp::Lesser),
];

const TERM_OPS: &[(&[lex::Token], ASTBinaryOp)] = &[
    (&[lex::Token::Plus], ASTBinaryOp::Add),
    (&[lex::Token::Minus], ASTBinaryOp::Subtract),
];

const FACTOR_OPS: &[(&[lex::Token], ASTBinaryOp)] = &[
    (&[lex::Token::Star], ASTBinaryOp::Multiply),
    (&[lex::Token::Slash], ASTBinaryOp::Divide),
    (&[lex::Token::Carrot], ASTBinaryOp::Power),
    (&[lex::Token::At], ASTBinaryOp::MatrixMultiply),
    (&[lex::Token::ShapedAs], ASTBinaryOp::ShapedAs),
];

pub fn parse_stmt<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(ASTStmt<'b>, &'a [lex::Token<'b>])> {
    None
}

fn parse_expr<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(ASTExpr<'b>, &'a [lex::Token<'b>])> {
    parse_or(tokens, bump)
}

macro_rules! define_binary_expr_parse {
    ($x: ident, $y: ident, $z: ident) => {
        fn $x<'a, 'b>(
            tokens: &'a [lex::Token<'b>],
            bump: &'b bump::BumpAllocator,
        ) -> Option<(ASTExpr<'b>, &'a [lex::Token<'b>])> {
            let mut cp = bump.create_checkpoint();
            let (mut expr, mut rest) = $z(tokens, bump)?;
            let mut maybe_op = combi::parse_any_of(rest, $y);
            while let Some((op, tmp_rest)) = maybe_op {
                let (new_expr, tmp_rest) = $z(tmp_rest, bump)?;
                expr = ASTExpr::Binary(op, bump.alloc(expr), bump.alloc(new_expr));
                rest = tmp_rest;
                maybe_op = combi::parse_any_of(rest, $y);
            }
            cp.commit();
            Some((expr, rest))
        }
    };
}

define_binary_expr_parse!(parse_or, OR_OPS, parse_and);

define_binary_expr_parse!(parse_and, AND_OPS, parse_equality);

define_binary_expr_parse!(parse_equality, EQUALITY_OPS, parse_comparison);

define_binary_expr_parse!(parse_comparison, COMPARISON_OPS, parse_term);

define_binary_expr_parse!(parse_term, TERM_OPS, parse_factor);

define_binary_expr_parse!(parse_factor, FACTOR_OPS, parse_unary);

fn parse_unary<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(ASTExpr<'b>, &'a [lex::Token<'b>])> {
    combi::parse_or(
        tokens,
        &[
            &|tokens, bump| {
                let rest = combi::parse_token_consume(tokens, lex::Token::Exclamation)?;
                let (expr, rest) = parse_unary(rest, bump)?;
                Some((ASTExpr::Unary(ASTUnaryOp::Not, bump.alloc(expr)), rest))
            },
            &|tokens, bump| {
                let rest = combi::parse_token_consume(tokens, lex::Token::Minus)?;
                let (expr, rest) = parse_unary(rest, bump)?;
                Some((ASTExpr::Unary(ASTUnaryOp::Negate, bump.alloc(expr)), rest))
            },
            &|tokens, bump| {
                let rest = combi::parse_token_consume(tokens, lex::Token::Shape)?;
                let (expr, rest) = parse_unary(rest, bump)?;
                Some((ASTExpr::Unary(ASTUnaryOp::Shape, bump.alloc(expr)), rest))
            },
            &parse_index,
        ],
        bump,
    )
}

fn parse_index<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(ASTExpr<'b>, &'a [lex::Token<'b>])> {
    combi::parse_or(
        tokens,
        &[
            &|tokens, bump| {
                let mut cp = bump.create_checkpoint();
                let (to_index, rest) = parse_call(tokens, bump)?;
                let rest = combi::parse_token_consume(rest, lex::Token::LeftBracket)?;
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
                Some((ASTExpr::Index(bump.alloc(to_index), list), rest))
            },
            &parse_call,
        ],
        bump,
    )
}

fn parse_call<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(ASTExpr<'b>, &'a [lex::Token<'b>])> {
    combi::parse_or(
        tokens,
        &[
            &|tokens, bump| {
                let mut cp = bump.create_checkpoint();
                let (name, rest) = parse_identifier(tokens, bump)?;
                let rest = combi::parse_token_consume(rest, lex::Token::LeftParen)?;
                let (list, rest) = combi::parse_or(
                    rest,
                    &[
                        &|tokens, bump| {
                            let list = bump.create_list();
                            let (expr, mut rest) = parse_expr(tokens, bump)?;
                            list.push(expr);
                            while let Some(rest_comma) =
                                combi::parse_token_consume(rest, lex::Token::Comma)
                            {
                                let (expr, rest_new) = parse_expr(rest_comma, bump)?;
                                list.push(expr);
                                rest = rest_new;
                            }
                            Some((list, rest))
                        },
                        &|tokens, bump| Some((bump.create_list(), tokens)),
                    ],
                    bump,
                )?;
                let rest = combi::parse_token_consume(rest, lex::Token::RightParen)?;
                cp.commit();
                Some((ASTExpr::Call(name, list), rest))
            },
            &parse_primary,
        ],
        bump,
    )
}

fn parse_primary<'a, 'b>(
    tokens: &'a [lex::Token<'b>],
    bump: &'b bump::BumpAllocator,
) -> Option<(ASTExpr<'b>, &'a [lex::Token<'b>])> {
    combi::parse_or(
        tokens,
        &[
            &|tokens, bump| {
                let (expr, rest) = parse_identifier(tokens, bump)?;
                Some((ASTExpr::Identifier(expr), rest))
            },
            &|tokens, bump| {
                let (expr, rest) = parse_string(tokens, bump)?;
                Some((ASTExpr::String(expr), rest))
            },
            &|tokens, bump| {
                let (expr, rest) = parse_number(tokens, bump)?;
                Some((ASTExpr::Number(expr), rest))
            },
            &|tokens, bump| {
                let (expr, rest) = parse_boolean(tokens, bump)?;
                Some((ASTExpr::Boolean(expr), rest))
            },
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
) -> Option<(ASTExpr<'b>, &'a [lex::Token<'b>])> {
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
) -> Option<(ASTExpr<'b>, &'a [lex::Token<'b>])> {
    let mut cp = bump.create_checkpoint();
    let rest = combi::parse_token_consume(tokens, lex::Token::LeftBracket)?;
    let (list, rest) = combi::parse_or(
        rest,
        &[
            &|tokens, bump| {
                let list = bump.create_list();
                let (expr, mut rest) = parse_expr(tokens, bump)?;
                list.push(expr);
                while let Some(rest_comma) = combi::parse_token_consume(rest, lex::Token::Comma) {
                    let (expr, rest_new) = parse_expr(rest_comma, bump)?;
                    list.push(expr);
                    rest = rest_new;
                }
                Some((list, rest))
            },
            &|tokens, bump| Some((bump.create_list(), tokens)),
        ],
        bump,
    )?;
    let rest = combi::parse_token_consume(rest, lex::Token::RightBracket)?;
    cp.commit();
    Some((ASTExpr::ArrayLiteral(list), rest))
}

macro_rules! define_simple_expr_parse {
    ($x:ident, $y: expr, $z: ty) => {
        fn $x<'a, 'b>(
            tokens: &'a [lex::Token<'b>],
            bump: &'b bump::BumpAllocator,
        ) -> Option<($z, &'a [lex::Token<'b>])> {
            combi::parse_token(tokens, $y, bump)
        }
    };
}

define_simple_expr_parse!(
    parse_identifier,
    &|c| match c {
        lex::Token::Identifier(i) => Some(i),
        _ => None,
    },
    &'b [u8]
);

define_simple_expr_parse!(
    parse_string,
    &|c| match c {
        lex::Token::String(s) => Some(s),
        _ => None,
    },
    &'b [u8]
);

define_simple_expr_parse!(
    parse_number,
    &|c| match c {
        lex::Token::Number(n) => Some(n),
        _ => None,
    },
    f64
);

define_simple_expr_parse!(
    parse_boolean,
    &|c| match c {
        lex::Token::True => Some(true),
        lex::Token::False => Some(false),
        _ => None,
    },
    bool
);

define_simple_expr_parse!(
    parse_nil,
    &|c| match c {
        lex::Token::Nil => Some(ASTExpr::Nil),
        _ => None,
    },
    ASTExpr<'b>
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_primary1() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"1.22387", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(ast, ASTExpr::Number(1.22387));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_primary2() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"\"This is a string!\"", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(ast, ASTExpr::String(b"This is a string!"));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_primary3() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"((((my_identifier))))", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(ast, ASTExpr::Identifier(b"my_identifier"));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_primary4() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"[x, y, z]", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        let correct_list = bump.create_list();
        correct_list.push(ASTExpr::Identifier(b"x"));
        correct_list.push(ASTExpr::Identifier(b"y"));
        correct_list.push(ASTExpr::Identifier(b"z"));
        assert_eq!(ast, ASTExpr::ArrayLiteral(correct_list));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_primary5() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"[]", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        let correct_list = bump.create_list();
        assert_eq!(ast, ASTExpr::ArrayLiteral(correct_list));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_call1() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"my_func()", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        let correct_list = bump.create_list();
        assert_eq!(ast, ASTExpr::Call(b"my_func", correct_list));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_call2() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"xyz(7.4, 3.1, \"a string\")", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        let correct_list = bump.create_list();
        correct_list.push(ASTExpr::Number(7.4));
        correct_list.push(ASTExpr::Number(3.1));
        correct_list.push(ASTExpr::String(b"a string"));
        assert_eq!(ast, ASTExpr::Call(b"xyz", correct_list));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_index1() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"xyz[]", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(ast, ASTExpr::Identifier(b"xyz"));
        assert_eq!(
            rest,
            vec![lex::Token::LeftBracket, lex::Token::RightBracket]
        );
    }

    #[test]
    fn parse_index2() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"xyz[1, 2, 3]", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        let correct_list = bump.create_list();
        correct_list.push(ASTExpr::Number(1.0));
        correct_list.push(ASTExpr::Number(2.0));
        correct_list.push(ASTExpr::Number(3.0));
        assert_eq!(
            ast,
            ASTExpr::Index(&ASTExpr::Identifier(b"xyz"), correct_list)
        );
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_unary1() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"-3.4", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(
            ast,
            ASTExpr::Unary(ASTUnaryOp::Negate, &ASTExpr::Number(3.4))
        );
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_unary2() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"!abc", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(
            ast,
            ASTExpr::Unary(ASTUnaryOp::Not, &ASTExpr::Identifier(b"abc"))
        );
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_unary3() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"s []", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(
            ast,
            ASTExpr::Unary(
                ASTUnaryOp::Shape,
                bump.alloc(ASTExpr::ArrayLiteral(bump.create_list()))
            )
        );
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_factor1() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"1.4 * 3.1", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(
            ast,
            ASTExpr::Binary(
                ASTBinaryOp::Multiply,
                bump.alloc(ASTExpr::Number(1.4)),
                bump.alloc(ASTExpr::Number(3.1)),
            )
        );
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_factor2() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"1.4 / 3.1 @ 8.1", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(
            ast,
            ASTExpr::Binary(
                ASTBinaryOp::MatrixMultiply,
                bump.alloc(ASTExpr::Binary(
                    ASTBinaryOp::Divide,
                    bump.alloc(ASTExpr::Number(1.4)),
                    bump.alloc(ASTExpr::Number(3.1)),
                )),
                bump.alloc(ASTExpr::Number(8.1))
            )
        );
        assert_eq!(rest, &[]);
    }

    #[test]
    fn parse_factor3() {
        let bump = bump::BumpAllocator::new();
        let tokens = lex(b"1.4 ^ (3.1 sa 8.1)", &bump).unwrap();
        let (ast, rest) = parse_expr(&tokens, &bump).unwrap();
        assert_eq!(
            ast,
            ASTExpr::Binary(
                ASTBinaryOp::Power,
                bump.alloc(ASTExpr::Number(1.4)),
                bump.alloc(ASTExpr::Binary(
                    ASTBinaryOp::ShapedAs,
                    bump.alloc(ASTExpr::Number(3.1)),
                    bump.alloc(ASTExpr::Number(8.1)),
                )),
            )
        );
        assert_eq!(rest, &[]);
    }
}