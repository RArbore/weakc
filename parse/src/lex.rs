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

use super::combi::*;

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

pub struct Lexer {}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {}
    }

    pub fn lex(chunk: &[u8]) {}
}
