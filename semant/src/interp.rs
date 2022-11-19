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

use std::collections::HashMap;
use std::rc::Rc;

use parse::ASTBinaryOp;
use parse::ASTExpr;
use parse::ASTStmt;
use parse::ASTUnaryOp;

#[derive(Debug, PartialEq, Clone)]
enum Value<'a> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(&'a [u8]),
    Tensor(Rc<[f64]>),
}

struct InterpContext<'a> {
    funcs: HashMap<&'a [u8], (&'a bump::List<'a, &'a [u8]>, &'a ASTStmt<'a>)>,
    ops: HashMap<&'a [u8], (&'a [u8], &'a [u8], &'a ASTStmt<'a>)>,
    vars: HashMap<&'a [u8], Value<'a>>,
}

pub fn eval<'a>(program: &'a ASTStmt<'a>) {
    let context = InterpContext {
        funcs: HashMap::new(),
        ops: HashMap::new(),
        vars: HashMap::new(),
    };
    eval_stmt(program, context);
}

fn eval_stmt<'a>(stmt: &'a ASTStmt<'a>, context: InterpContext<'a>) -> Option<InterpContext<'a>> {
    Some(context)
}

fn eval_expr<'a>(
    expr: &'a ASTExpr<'a>,
    context: InterpContext<'a>,
) -> Option<(Value<'a>, InterpContext<'a>)> {
    let val = match expr {
        ASTExpr::Nil => Value::Nil,
        ASTExpr::Boolean(val) => Value::Boolean(*val),
        ASTExpr::Number(val) => Value::Number(*val),
        ASTExpr::String(val) => Value::String(val),
        ASTExpr::Identifier(name) => context.vars.get(name)?.clone(),
        _ => panic!(),
    };
    Some((val, context))
}
