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

use super::combi;

#[derive(Clone, Copy, Debug)]
pub enum Token<'a> {
    Number(f64),
    String(&'a [u8]),
    Identifier(&'a [u8]),
    False,
    True,

    Print,
    Variable,
    Shape,
    ShapedAs,
    If,
    While,
    Function,
    Operator,
    Return,
    Nil,
    Assert,

    Plus,
    Minus,
    Star,
    Slash,
    Exp,
    At,

    Exclamation,
    Equals,
    Greater,
    Lesser,
    ExclamationEquals,
    EqualsEquals,
    GreaterEquals,
    LesserEquals,
    And,
    Or,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Semicolon,
}

const SIMPLE_TOKENS: &[(&[u8], Token)] = &[
    (b"F", Token::False),
    (b"T", Token::True),
    (b"p", Token::Print),
    (b"a", Token::Variable),
    (b"s", Token::Shape),
    (b"sa", Token::ShapedAs),
    (b"i", Token::If),
    (b"w", Token::While),
    (b"f", Token::Function),
    (b"o", Token::Operator),
    (b"r", Token::Return),
    (b"N", Token::Nil),
    (b"v", Token::Assert),
    (b"+", Token::Plus),
    (b"-", Token::Minus),
    (b"*", Token::Star),
    (b"/", Token::Slash),
    (b"^", Token::Exp),
    (b"@", Token::At),
    (b"!", Token::Exclamation),
    (b"=", Token::Equals),
    (b">", Token::Greater),
    (b"<", Token::Lesser),
    (b"!=", Token::ExclamationEquals),
    (b"==", Token::EqualsEquals),
    (b">=", Token::GreaterEquals),
    (b"<=", Token::LesserEquals),
    (b"A", Token::And),
    (b"O", Token::Or),
    (b"(", Token::LeftParen),
    (b")", Token::RightParen),
    (b"{", Token::LeftBrace),
    (b"}", Token::RightBrace),
    (b"[", Token::LeftBracket),
    (b"]", Token::RightBracket),
    (b",", Token::Comma),
    (b";", Token::Semicolon),
];

pub fn lex(chunk: &[u8]) -> Option<Vec<Token>> {
    let mut tokens = vec![];
    let mut current = chunk;

    loop {
        let attempt = combi::parse_any_of(current, SIMPLE_TOKENS);
        if let Some((token, rest)) = attempt {
            tokens.push(token);
            current = rest;
        } else {
            break;
        }
    }

    Some(tokens)
}
