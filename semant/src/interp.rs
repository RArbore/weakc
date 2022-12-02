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

use crate::typecheck::*;

#[derive(Debug, PartialEq, Clone)]
enum Value<'a> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(&'a [u8]),
    Tensor(Box<[usize]>, Box<[f64]>),
}

#[derive(Debug, Default, PartialEq, Clone)]
struct InterpContext<'a> {
    funcs: HashMap<&'a [u8], (&'a bump::List<'a, &'a [u8]>, &'a ASTStmt<'a>)>,
    ops: HashMap<&'a [u8], (&'a [u8], &'a [u8], &'a ASTStmt<'a>)>,
    vars: HashMap<&'a [u8], Value<'a>>,
    ret_val: Option<Value<'a>>,
}

pub fn eval<'a>(program: &'a ASTStmt<'a>) {
    let context = InterpContext::default();
    eval_stmt(program, context);
}

fn eval_stmt<'a>(stmt: &'a ASTStmt<'a>, context: InterpContext<'a>) -> Option<InterpContext<'a>> {
    Some(context)
}

fn eval_expr<'a>(
    expr: &'a ASTExpr<'a>,
    mut context: InterpContext<'a>,
) -> Option<(Value<'a>, InterpContext<'a>)> {
    let val = match expr {
        ASTExpr::Nil => Value::Nil,
        ASTExpr::Boolean(val) => Value::Boolean(*val),
        ASTExpr::Number(val) => Value::Number(*val),
        ASTExpr::String(val) => Value::String(val),
        ASTExpr::Identifier(name) => context.vars.get(name)?.clone(),
        ASTExpr::Call(func, args) => {
            let (params, body) = context.funcs.get(func)?.clone();
            if args.len() != params.len() {
                return None;
            }
            let mut eval_args = vec![];
            for i in 0..args.len() {
                let (eval, new_context) = eval_expr(args.at(i), context)?;
                context = new_context;
                eval_args.push(eval);
            }
            let mut old_values = HashMap::new();
            for i in (0..args.len()).rev() {
                let old_value = context.vars.insert(params.at(i), eval_args.pop().unwrap());
                if let Some(old_value) = old_value {
                    old_values.insert(params.at(i), old_value);
                }
            }
            context = eval_stmt(body, context)?;
            for (old_k, old_v) in old_values {
                context.vars.insert(old_k, old_v);
            }
            if let Some(ret_val) = context.ret_val {
                context.ret_val = None;
                ret_val
            } else {
                Value::Nil
            }
        }
        ASTExpr::Index(tensor, indices) => {
            let (tensor_val, new_context) = eval_expr(tensor, context)?;
            context = new_context;
            if let Value::Tensor(size, contents) = tensor_val {
                let mut index_vals = vec![];
                for i in 0..indices.len() {
                    let (index, new_context) = eval_expr(indices.at(i), context)?;
                    context = new_context;
                    if let Value::Number(index_num) = index {
                        let index_num = index_num as isize;
                        if index_num >= 0 {
                            index_vals.push(index_num as usize);
                        } else {
                            None?
                        }
                    } else {
                        None?
                    }
                }
                if size.len() == index_vals.len() {
                    let mut flat_index = 0;
                    for i in 0..size.len() {
                        if index_vals[i] < size[i] {
                            flat_index = flat_index * size[i] + index_vals[i];
                        } else {
                            None?
                        }
                    }
                    Value::Number(contents[flat_index])
                } else {
                    None?
                }
            } else {
                None?
            }
        }
        _ => panic!(),
    };
    Some((val, context))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval1() {
        let bump = bump::BumpAllocator::new();
        let tests: &[(&[u8], Value)] = &[
            (b"1.22387", Value::Number(1.22387)),
            (b"T", Value::Boolean(true)),
            (b"\"hello\"", Value::String(b"hello")),
            (b"N", Value::Nil),
        ];
        for (input, output) in tests {
            let tokens = parse::lex(input, &bump).unwrap();
            let (ast, _) = parse::parse_expr(&tokens, &bump).unwrap();
            let context = InterpContext::default();
            let (val, _) = eval_expr(bump.alloc(ast), context).unwrap();
            assert_eq!(val, *output);
        }
    }
}
