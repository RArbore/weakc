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

use core::fmt;
use core::str;
use std::io::{stdout, Stdout, Write};

use parse::ASTBinaryOp;
use parse::ASTExpr;
use parse::ASTStmt;
use parse::ASTUnaryOp;

type InterpResult<T> = Result<T, &'static str>;

#[derive(Debug, PartialEq, Clone)]
enum Value<'a> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(&'a [u8]),
    Tensor(Box<[usize]>, Box<[f64]>),
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Boolean(true) => write!(f, "True"),
            Value::Boolean(false) => write!(f, "True"),
            Value::Number(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", str::from_utf8(v).unwrap()),
            Value::Tensor(d, v) => write!(f, "{:?} sa {:?}", v, d),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct SymbolTable<K: PartialEq, V>(Vec<(K, V)>);

impl<K: PartialEq, V> SymbolTable<K, V> {
    fn get(&self, k: &K) -> Option<&V> {
        Some(&self.0.iter().rev().find(|x| &x.0 == k)?.1)
    }

    fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        Some(&mut self.0.iter_mut().rev().find(|x| &x.0 == k)?.1)
    }

    fn contains(&self, k: &K, checkpoint: usize) -> bool {
        self.0[checkpoint..]
            .iter()
            .rev()
            .find(|x| &x.0 == k)
            .is_some()
    }

    fn insert(&mut self, k: K, v: V) {
        self.0.push((k, v));
    }

    fn get_checkpoint(&self) -> usize {
        self.0.len()
    }

    fn revert_checkpoint(&mut self, checkpoint: usize) {
        self.0.truncate(checkpoint);
    }
}

impl<K: PartialEq, V> Default for SymbolTable<K, V> {
    fn default() -> Self {
        SymbolTable(vec![])
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InterpContext<'a, W: Write> {
    funcs: SymbolTable<&'a [u8], (&'a bump::List<'a, &'a [u8]>, &'a ASTStmt<'a>)>,
    ops: SymbolTable<&'a [u8], (&'a [u8], &'a [u8], &'a ASTStmt<'a>)>,
    vars: SymbolTable<&'a [u8], Value<'a>>,
    ret_val: Option<Value<'a>>,
    current_checkpoints: (usize, usize, usize),
    writer: W,
}

impl<'a, W: Write> InterpContext<'a, W> {
    fn _new(w: W) -> Self {
        InterpContext {
            funcs: Default::default(),
            ops: Default::default(),
            vars: Default::default(),
            ret_val: Default::default(),
            current_checkpoints: (0, 0, 0),
            writer: w,
        }
    }
}

impl<'a> Default for InterpContext<'a, Stdout> {
    fn default() -> Self {
        InterpContext {
            funcs: Default::default(),
            ops: Default::default(),
            vars: Default::default(),
            ret_val: Default::default(),
            current_checkpoints: (0, 0, 0),
            writer: stdout(),
        }
    }
}

pub fn eval_program<'a, W: Write>(
    program: &'a bump::List<'a, ASTStmt<'a>>,
    mut context: InterpContext<'a, W>,
) -> InterpResult<InterpContext<'a, W>> {
    for i in 0..program.len() {
        context = eval_stmt(program.at(i), context)?;
    }
    Ok(context)
}

fn eval_stmt<'a, W: Write>(
    stmt: &'a ASTStmt<'a>,
    mut context: InterpContext<'a, W>,
) -> InterpResult<InterpContext<'a, W>> {
    match stmt {
        ASTStmt::Block(stmts) => {
            let funcs_checkpoint = context.funcs.get_checkpoint();
            let ops_checkpoint = context.ops.get_checkpoint();
            let vars_checkpoint = context.vars.get_checkpoint();
            for i in 0..stmts.len() {
                if context.ret_val.is_some() {
                    Err("ERROR: Attempted to evaluate statement after return in block.")?
                }
                context = eval_stmt(stmts.at(i), context)?;
            }
            context.funcs.revert_checkpoint(funcs_checkpoint);
            context.ops.revert_checkpoint(ops_checkpoint);
            context.vars.revert_checkpoint(vars_checkpoint);
            Ok(context)
        }
        ASTStmt::Function(name, params, body) => {
            if context.funcs.contains(name, context.current_checkpoints.0) {
                Err("ERROR: Attempted a redefinition of a function.")?
            }
            context.funcs.insert(name, (params, body));
            Ok(context)
        }
        ASTStmt::Operator(name, left_param, right_param, body) => {
            if context.ops.contains(name, context.current_checkpoints.1) {
                Err("ERROR: Attempted a redefinition of an operator.")?
            }
            context.ops.insert(name, (left_param, right_param, body));
            Ok(context)
        }
        ASTStmt::If(cond, body) => {
            let (cond_val, new_context) = eval_expr(cond, context)?;
            context = new_context;
            if let Value::Boolean(v) = cond_val {
                if v {
                    context = eval_stmt(body, context)?;
                }
                Ok(context)
            } else {
                Err("ERROR: Attempted to use a non-boolean in an if statement.")?
            }
        }
        ASTStmt::While(cond, body) => loop {
            let (cond_val, new_context) = eval_expr(cond, context)?;
            context = new_context;
            if let Value::Boolean(v) = cond_val {
                if v {
                    context = eval_stmt(body, context)?;
                } else {
                    break Ok(context);
                }
            } else {
                Err("ERROR: Attempted to use a non-boolean in a while statement.")?
            }
        },
        ASTStmt::Print(expr) => {
            let (value, mut context) = eval_expr(expr, context)?;
            context
                .writer
                .write(format!("{}\n", value).as_bytes())
                .unwrap();
            Ok(context)
        }
        ASTStmt::Return(expr) => {
            let (value, mut context) = eval_expr(expr, context)?;
            context.ret_val = Some(value);
            Ok(context)
        }
        ASTStmt::Verify(expr) => {
            let (value, context) = eval_expr(expr, context)?;
            if value == Value::Boolean(true) {
                Ok(context)
            } else {
                Err("ERROR: Failed a verify statement.")?
            }
        }
        ASTStmt::Variable(name, init) => {
            if context.vars.contains(name, context.current_checkpoints.2) {
                Err("ERROR: Attempted a redefinition of a variable.")?
            }
            let (value, mut context) = eval_expr(init, context)?;
            context.vars.insert(name, value);
            Ok(context)
        }
        ASTStmt::Expression(expr) => {
            let (_, context) = eval_expr(expr, context)?;
            Ok(context)
        }
    }
}

macro_rules! combine_elementwise {
    ($x: ident, $ld: expr, $lv: expr, $rd: expr, $rv: expr) => {
        if $ld == $rd {
            for i in 0..$lv.len() {
                $lv[i] = $lv[i].$x($rv[i]);
            }
            Value::Tensor($ld, $lv)
        } else {
            Err("ERROR: Attempted element-wise operation on two tensors of different sizes.")?
        }
    };
    ($x: tt, $ld: expr, $lv: expr, $rd: expr, $rv: expr) => {
        if $ld == $rd {
            for i in 0..$lv.len() {
                $lv[i] $x $rv[i];
            }
            Value::Tensor($ld, $lv)
        } else {
            Err("ERROR: Attempted element-wise operation on two tensors of different sizes.")?
        }
    };
}

fn eval_expr<'a, W: Write>(
    expr: &'a ASTExpr<'a>,
    mut context: InterpContext<'a, W>,
) -> InterpResult<(Value<'a>, InterpContext<'a, W>)> {
    let val = match expr {
        ASTExpr::Nil => Value::Nil,
        ASTExpr::Boolean(val) => Value::Boolean(*val),
        ASTExpr::Number(val) => Value::Number(*val),
        ASTExpr::String(val) => Value::String(val),
        ASTExpr::Identifier(name) => context
            .vars
            .get(name)
            .ok_or("ERROR: Attempted to reference an undefined variable.")?
            .clone(),
        ASTExpr::Call(func, args) => {
            let (params, body) = context
                .funcs
                .get(func)
                .ok_or("ERROR: Attempted to call an undefined function.")?
                .clone();
            if args.len() != params.len() {
                Err("ERROR: Attempted to call a function with the wrong number of arguments.")?
            }
            let mut eval_args = vec![];
            for i in 0..args.len() {
                let (eval, new_context) = eval_expr(args.at(i), context)?;
                context = new_context;
                eval_args.push(eval);
            }
            let funcs_checkpoint = context.funcs.get_checkpoint();
            let ops_checkpoint = context.ops.get_checkpoint();
            let vars_checkpoint = context.vars.get_checkpoint();
            for i in (0..args.len()).rev() {
                context.vars.insert(params.at(i), eval_args.pop().unwrap());
            }
            context.current_checkpoints = (funcs_checkpoint, ops_checkpoint, vars_checkpoint);
            context = eval_stmt(body, context)?;
            context.funcs.revert_checkpoint(funcs_checkpoint);
            context.ops.revert_checkpoint(ops_checkpoint);
            context.vars.revert_checkpoint(vars_checkpoint);
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
                            Err(
                                "ERROR: Attempted to use a negative number to index into a tensor.",
                            )?
                        }
                    } else {
                        Err("ERROR: Attempted to use a non-number as an index value.")?
                    }
                }
                if size.len() == index_vals.len() {
                    let mut flat_index = 0;
                    for i in 0..size.len() {
                        if index_vals[i] < size[i] {
                            flat_index = flat_index * size[i] + index_vals[i];
                        } else {
                            Err("ERROR: Index value doesn't fit in dimension of tensor.")?
                        }
                    }
                    Value::Number(contents[flat_index])
                } else {
                    Err("ERROR: Attempted to index into tensor using incorrect number of indices.")?
                }
            } else {
                Err("ERROR: Attempted to index into non-tensor.")?
            }
        }
        ASTExpr::ArrayLiteral(contents) => {
            let mut content_vals = vec![];
            for i in 0..contents.len() {
                let (val, new_context) = eval_expr(contents.at(i), context)?;
                context = new_context;
                if let Value::Number(val_num) = val {
                    content_vals.push(val_num);
                } else {
                    Err("ERROR: Attempted to place non-number inside tensor.")?
                }
            }
            Value::Tensor(
                Box::new([content_vals.len()]),
                content_vals.into_boxed_slice(),
            )
        }
        ASTExpr::Assign(left, right) => {
            let (right_val, new_context) = eval_expr(right, context)?;
            context = new_context;
            match left {
                ASTExpr::Identifier(name) => {
                    let cur_val = context
                        .vars
                        .get_mut(name)
                        .ok_or("ERROR: Attempted to reference an undefined variable.")?;
                    match (cur_val, right_val) {
                        (Value::Nil, Value::Nil) => Value::Nil,
                        (Value::Boolean(cur_bool), Value::Boolean(right_bool)) => {
                            *cur_bool = right_bool;
                            Value::Boolean(right_bool)
                        }
                        (Value::Number(cur_num), Value::Number(right_num)) => {
                            *cur_num = right_num;
                            Value::Number(right_num)
                        }
                        (Value::String(cur_str), Value::String(right_str)) => {
                            *cur_str = right_str;
                            Value::String(right_str)
                        }
                        (
                            Value::Tensor(cur_size, cur_data),
                            Value::Tensor(right_size, right_data),
                        ) => {
                            *cur_size = right_size.clone();
                            *cur_data = right_data.clone();
                            Value::Tensor(right_size, right_data)
                        }
                        _ => Err("ERROR: Attempted invalid assignment.")?,
                    }
                }
                ASTExpr::Index(&ASTExpr::Identifier(name), indices) => {
                    let mut index_vals = vec![];
                    for i in 0..indices.len() {
                        let (index, new_context) = eval_expr(indices.at(i), context)?;
                        context = new_context;
                        if let Value::Number(index_num) = index {
                            let index_num = index_num as isize;
                            if index_num >= 0 {
                                index_vals.push(index_num as usize);
                            } else {
                                Err("ERROR: Attempted to use a negative number to index into a tensor.")?
                            }
                        } else {
                            Err("ERROR: Attempted to use a non-number as an index value.")?
                        }
                    }
                    if let (Value::Tensor(size, contents), Value::Number(num)) = (
                        context
                            .vars
                            .get_mut(&name)
                            .ok_or("ERROR: Attempted to reference an undefined variable.")?,
                        right_val,
                    ) {
                        if size.len() == index_vals.len() {
                            let mut flat_index = 0;
                            for i in 0..size.len() {
                                if index_vals[i] < size[i] {
                                    flat_index = flat_index * size[i] + index_vals[i];
                                } else {
                                    Err("ERROR: Index value doesn't fit in dimension of tensor.")?
                                }
                            }
                            contents[flat_index] = num;
                            Value::Number(num)
                        } else {
                            Err("ERROR: Attempted to index into tensor using incorrect number of indices.")?
                        }
                    } else {
                        Err("ERROR: Attempted to index into non-tensor.")?
                    }
                }
                _ => Err("ERROR: Attempted to assign to a non-assignable thing.")?,
            }
        }
        ASTExpr::Unary(op, expr) => {
            let (val, new_context) = eval_expr(expr, context)?;
            context = new_context;
            match (op, val) {
                (ASTUnaryOp::Not, Value::Boolean(v)) => Value::Boolean(!v),
                (ASTUnaryOp::Negate, Value::Number(v)) => Value::Number(-v),
                (ASTUnaryOp::Negate, Value::Tensor(d, mut v)) => {
                    for i in 0..v.len() {
                        v[i] *= -1.0;
                    }
                    Value::Tensor(d, v)
                }
                (ASTUnaryOp::Shape, Value::Tensor(d, _)) => {
                    let mut v = vec![];
                    for i in 0..d.len() {
                        v.push(d[i] as f64);
                    }
                    Value::Tensor(Box::new([v.len()]), v.into_boxed_slice())
                }
                _ => Err("ERROR: Incorrect usage of a predefined unary operation.")?,
            }
        }
        ASTExpr::Binary(op, left, right) => {
            let (lval, new_context) = eval_expr(left, context)?;
            let (rval, new_context) = eval_expr(right, new_context)?;
            context = new_context;
            match (op, lval, rval) {
                (ASTBinaryOp::ShapedAs, Value::Tensor(ld, lv), Value::Tensor(rd, rv)) => {
                    if ld.len() == 1 && ld[0] == 1 {
                        let mut right_prod = 1;
                        let mut new_ld = vec![];
                        for i in 0..rv.len() {
                            let dim = rv[i] as usize;
                            if dim <= 0 {
                                Err("ERROR: Attempted to resize tensor using non-positive dimension.")?
                            }
                            new_ld.push(dim);
                            right_prod *= dim;
                        }
                        Value::Tensor(
                            new_ld.into_boxed_slice(),
                            vec![lv[0]; right_prod].into_boxed_slice(),
                        )
                    } else if rd.len() == 1 {
                        let mut right_prod = 1;
                        let mut new_ld = vec![];
                        for i in 0..rv.len() {
                            let dim = rv[i] as usize;
                            if dim <= 0 {
                                Err("ERROR: Attempted to resize tensor using non-positive dimension.")?
                            }
                            new_ld.push(dim);
                            right_prod *= dim;
                        }
                        if lv.len() == right_prod {
                            Value::Tensor(new_ld.into_boxed_slice(), lv)
                        } else {
                            Err("ERROR: Attempted to resize tensor, but new size has different number of elements than old size.")?
                        }
                    } else {
                        Err("ERROR: Attempted to resize with tensor which has more than one dimension.")?
                    }
                }
                (ASTBinaryOp::Add, Value::Number(lv), Value::Number(rv)) => Value::Number(lv + rv),
                (ASTBinaryOp::Subtract, Value::Number(lv), Value::Number(rv)) => {
                    Value::Number(lv - rv)
                }
                (ASTBinaryOp::Multiply, Value::Number(lv), Value::Number(rv)) => {
                    Value::Number(lv * rv)
                }
                (ASTBinaryOp::Divide, Value::Number(lv), Value::Number(rv)) => {
                    Value::Number(lv / rv)
                }
                (ASTBinaryOp::Power, Value::Number(lv), Value::Number(rv)) => {
                    Value::Number(lv.powf(rv))
                }
                (ASTBinaryOp::Add, Value::Tensor(ld, mut lv), Value::Tensor(rd, rv)) => {
                    combine_elementwise!(+=, ld, lv, rd, rv)
                }
                (ASTBinaryOp::Subtract, Value::Tensor(ld, mut lv), Value::Tensor(rd, rv)) => {
                    combine_elementwise!(-=, ld, lv, rd, rv)
                }
                (ASTBinaryOp::Multiply, Value::Tensor(ld, mut lv), Value::Tensor(rd, rv)) => {
                    combine_elementwise!(*=, ld, lv, rd, rv)
                }
                (ASTBinaryOp::Divide, Value::Tensor(ld, mut lv), Value::Tensor(rd, rv)) => {
                    combine_elementwise!(/=, ld, lv, rd, rv)
                }
                (ASTBinaryOp::Power, Value::Tensor(ld, mut lv), Value::Tensor(rd, rv)) => {
                    combine_elementwise!(powf, ld, lv, rd, rv)
                }
                (ASTBinaryOp::MatrixMultiply, Value::Tensor(ld, lv), Value::Tensor(rd, rv)) => {
                    if ld.len() == 2 && rd.len() == 2 && ld[1] == rd[0] {
                        let mut v = vec![0.0; ld[0] * rd[1]];
                        for i in 0..ld[0] {
                            for k in 0..ld[1] {
                                for j in 0..rd[1] {
                                    v[i * rd[1] + j] += lv[i * ld[1] + k] * rv[k * rd[1] + j];
                                }
                            }
                        }
                        Value::Tensor(Box::new([ld[0], rd[1]]), v.into_boxed_slice())
                    } else {
                        Err("ERROR: Attempted to matrix multiply incompatible tensors.")?
                    }
                }
                (ASTBinaryOp::Greater, Value::Number(lv), Value::Number(rv)) => {
                    Value::Boolean(lv > rv)
                }
                (ASTBinaryOp::Lesser, Value::Number(lv), Value::Number(rv)) => {
                    Value::Boolean(lv < rv)
                }
                (ASTBinaryOp::GreaterEquals, Value::Number(lv), Value::Number(rv)) => {
                    Value::Boolean(lv >= rv)
                }
                (ASTBinaryOp::LesserEquals, Value::Number(lv), Value::Number(rv)) => {
                    Value::Boolean(lv <= rv)
                }
                (ASTBinaryOp::NotEquals, lv, rv) => Value::Boolean(lv != rv),
                (ASTBinaryOp::EqualsEquals, lv, rv) => Value::Boolean(lv == rv),
                (ASTBinaryOp::And, Value::Boolean(lv), Value::Boolean(rv)) => {
                    Value::Boolean(lv && rv)
                }
                (ASTBinaryOp::Or, Value::Boolean(lv), Value::Boolean(rv)) => {
                    Value::Boolean(lv || rv)
                }
                _ => Err("ERROR: Incorrect usage of a predefined binary operation.")?,
            }
        }
        ASTExpr::CustomBinary(op, left, right) => {
            let (left_param, right_param, body) = context
                .ops
                .get(op)
                .ok_or("ERROR: Attempted to call an undefined operator.")?
                .clone();
            let (left_val, new_context) = eval_expr(left, context)?;
            let (right_val, new_context) = eval_expr(right, new_context)?;
            context = new_context;
            let funcs_checkpoint = context.funcs.get_checkpoint();
            let ops_checkpoint = context.ops.get_checkpoint();
            let vars_checkpoint = context.vars.get_checkpoint();
            context.vars.insert(left_param, left_val);
            context.vars.insert(right_param, right_val);
            context.current_checkpoints = (funcs_checkpoint, ops_checkpoint, vars_checkpoint);
            context = eval_stmt(body, context)?;
            context.funcs.revert_checkpoint(funcs_checkpoint);
            context.ops.revert_checkpoint(ops_checkpoint);
            context.vars.revert_checkpoint(vars_checkpoint);
            if let Some(ret_val) = context.ret_val {
                context.ret_val = None;
                ret_val
            } else {
                Value::Nil
            }
        }
    };
    Ok((val, context))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_eval1() {
        let bump = bump::BumpAllocator::new();
        let tests: &[(&[u8], Value)] = &[
            (b"1.22387", Value::Number(1.22387)),
            (b"T", Value::Boolean(true)),
            (b"\"hello\"", Value::String(b"hello")),
            (b"N", Value::Nil),
            (
                b"[1.2, 4]",
                Value::Tensor(Box::new([2]), Box::new([1.2, 4.0])),
            ),
            (b"[1.2, 4][1]", Value::Number(4.0)),
            (b"(-[1.2, 4])[1]", Value::Number(-4.0)),
            (b"(s [1.2, 4])[0]", Value::Number(2.0)),
            (b"!T", Value::Boolean(false)),
            (
                b"[1.2, 4] sa [1, 2]",
                Value::Tensor(Box::new([1, 2]), Box::new([1.2, 4.0])),
            ),
            (
                b"[1.2, 4, 1.0, 2.0, 3.0, 4.0] sa [3, 2]",
                Value::Tensor(Box::new([3, 2]), Box::new([1.2, 4.0, 1.0, 2.0, 3.0, 4.0])),
            ),
            (b"([1.2, 4]-[1.0, 2.0])[1]", Value::Number(2.0)),
            (b"([1.2, 4]^[1.0, 2.0])[1]", Value::Number(16.0)),
            (b"1.22387 + 1.0", Value::Number(2.22387)),
            (
                b"([1.0, 5.0, -2.0, 3.5, 7.1, 22.01, -110.0, 21.0, 2.13] sa [3, 3]) @ ([-1.9, 1.9, 22.0, 2.0, 3.0, -1.0] sa [3, 2])",
                Value::Tensor(Box::new([3, 2]), Box::new([102.1, 13.9, 215.57999999999998, -1.1600000000000037, 677.39, -169.13])),
            ),
            (b"T == F", Value::Boolean(false)),
            (b"T != F", Value::Boolean(true)),
            (b"T A F", Value::Boolean(false)),
            (b"1 < 2", Value::Boolean(true)),
        ];
        for (input, output) in tests {
            let tokens = parse::lex(input, &bump).unwrap();
            let (ast, _) = parse::parse_expr(&tokens, &bump).unwrap();
            let context = InterpContext::default();
            let (val, _) = eval_expr(bump.alloc(ast), context).unwrap();
            assert_eq!(val, *output);
        }
    }

    #[test]
    fn test_eval2() {
        let bump = bump::BumpAllocator::new();
        let tests: &[(&[u8], &[u8])] = &[
            (b"p 1.22387;", b"1.22387\n"),
            (b"{p T;p [1.0, 2.0];}", b"True\n[1.0, 2.0] sa [2]\n"),
            (
                b"{a x = 0; w (x < 10) { p x; x = x + 1; }}",
                b"0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n",
            ),
            (
                b"{a x = 0.5; w (x < 10) { p x; x = x + 1; }}",
                b"0.5\n1.5\n2.5\n3.5\n4.5\n5.5\n6.5\n7.5\n8.5\n9.5\n",
            ),
            (b"{f xyz(x) { r x + 7; } p xyz(1.5);}", b"8.5\n"),
            (b"{{a x = 0; p x;} a x = 1; p x;}", b"0\n1\n"),
        ];
        for (input, output) in tests {
            let tokens = parse::lex(input, &bump).unwrap();
            let (ast, _) = parse::parse_stmt(&tokens, &bump).unwrap();
            let mut context = InterpContext::_new(Cursor::new(vec![0; 64]));
            context = eval_stmt(bump.alloc(ast), context).unwrap();
            assert_eq!(
                str::from_utf8(*output).unwrap(),
                str::from_utf8(&context.writer.get_ref()[0..context.writer.position() as usize])
                    .unwrap(),
            );
        }
    }

    #[test]
    fn test_eval3() {
        let bump = bump::BumpAllocator::new();
        let tests: &[(&[u8], &[u8])] = &[
            (b"f dim(mat) {    r (s (s mat))[0];}f len(list) {    v dim(list) == 1;    r (s list)[0];}f part_one(depths) {    v dim(depths) == 1;    a len = len(depths);    v len >= 1;    a j = 1;    a count = 0;    w (j < len) {        i (depths[j] > depths[j-1]) {            count = count + 1;        }        j = j + 1;    }    r count;}f part_two(depths) {    v dim(depths) == 1;    a len = len(depths);    v len >= 1;    a j = 0;    a new_depths = [0] sa [len];    a count = 0;    w (j < len - 2) {        new_depths[count] = depths[j] + depths[j+1] + depths[j+2];        count = count + 1;        j = j + 1;    }    r part_one(new_depths);}a d = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263];p part_one(d); p part_two(d);", b"7\n5\n"),
        ];
        for (input, output) in tests {
            let tokens = parse::lex(input, &bump).unwrap();
            let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
            let mut context = InterpContext::_new(Cursor::new(vec![0; 64]));
            context = eval_program(bump.alloc(ast), context).unwrap();
            assert_eq!(
                str::from_utf8(*output).unwrap(),
                str::from_utf8(&context.writer.get_ref()[0..context.writer.position() as usize])
                    .unwrap(),
            );
        }
    }
}
