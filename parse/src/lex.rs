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

const ALPHA_TOKENS: &[(&[u8], Token)] = &[
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
    (b"A", Token::And),
    (b"O", Token::Or),
];

const NON_ALPHA_TOKENS: &[(&[u8], Token)] = &[
    (b"!=", Token::ExclamationEquals),
    (b"==", Token::EqualsEquals),
    (b">=", Token::GreaterEquals),
    (b"<=", Token::LesserEquals),
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
    (b"(", Token::LeftParen),
    (b")", Token::RightParen),
    (b"{", Token::LeftBrace),
    (b"}", Token::RightBrace),
    (b"[", Token::LeftBracket),
    (b"]", Token::RightBracket),
    (b",", Token::Comma),
    (b";", Token::Semicolon),
];

fn parse_f64(integral: &[u8], decimal: &[u8]) -> f64 {
    let mut int = 0.0;

    for c in integral {
        int = int * 10.0 + (c - 48) as f64;
    }

    let mut dec = 0.0;
    let mut dig = 1.0 / 10.0;
    for c in decimal {
        dec += (c - 48) as f64 * dig;
        dig *= 1.0 / 10.0;
    }

    int + dec
}

pub fn lex(chunk: &[u8]) -> Option<Vec<Token>> {
    let clear_whitespace = |chunk| combi::parse_seq_char(chunk, &combi::pred_whitespace).1;
    let pred_identifier = |c| combi::pred_alpha(c) || combi::pred_number(c) || c == b'_';

    let mut tokens = vec![];
    let mut current = clear_whitespace(chunk);

    'consume: while current.len() > 0 {
        let non_alpha_numeric = combi::parse_any_of(current, NON_ALPHA_TOKENS);
        if let Some((token, rest)) = non_alpha_numeric {
            tokens.push(token);
            current = clear_whitespace(rest);
            continue;
        }

        let (number, rest) = combi::parse_seq_char(current, &combi::pred_number);
        if number.len() > 0 {
            if let Some((_, rest)) = combi::parse_char(rest, &|c| c == b'.') {
                let (decimal, rest) = combi::parse_seq_char(rest, &combi::pred_number);
                tokens.push(Token::Number(parse_f64(number, decimal)));
                current = clear_whitespace(rest);
                continue;
            } else if let Some(_) = combi::parse_char(rest, &combi::pred_alpha) {
                return None;
            } else {
                tokens.push(Token::Number(parse_f64(number, &[])));
                current = clear_whitespace(rest);
                continue;
            }
        }

        let (identifier, rest) = combi::parse_seq_char(current, &pred_identifier);
        if identifier.len() > 0 {
            for (repr, token) in ALPHA_TOKENS {
                if *repr == identifier {
                    tokens.push(*token);
                    current = clear_whitespace(rest);
                    continue 'consume;
                }
            }
            tokens.push(Token::Identifier(identifier));
            current = clear_whitespace(rest);
            continue;
        }

        return None;
    }

    Some(tokens)
}
