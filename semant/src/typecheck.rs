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

#[derive(Debug, PartialEq)]
pub enum Symbol<'a> {
    Variable(&'a [u8], Type),
    Function(&'a [u8], &'a mut bump::List<'a, Type>, Type),
    Operator(&'a [u8], Type, Type, Type),
}

struct TypeContext<'a> {
    bump: &'a bump::BumpAllocator,
    types: &'a mut bump::List<'a, Type>,
    symbols: Vec<Symbol<'a>>,
    ret_type: Vec<Type>,
    cur_type_var: u32,
}

fn join_types(recent_type: Type, older_type: Type) -> Option<Type> {
    match (recent_type, older_type) {
        (Type::Generic(_), _) => Some(older_type),
        (_, Type::Generic(_)) => Some(recent_type),
        (Type::Numeric(_), Type::Numeric(_)) => Some(older_type),
        (Type::Numeric(_), Type::Number) => Some(older_type),
        (Type::Numeric(_), Type::Tensor) => Some(older_type),
        (Type::Numeric(_), _) => None,
        (Type::Number, Type::Numeric(_)) => Some(recent_type),
        (Type::Tensor, Type::Numeric(_)) => Some(recent_type),
        (_, Type::Numeric(_)) => None,
        (recent_type, older_type) => {
            if recent_type == older_type {
                Some(recent_type)
            } else {
                None
            }
        }
    }
}

fn replace_single_generic(old: &mut Type, var: u32, new: Type) {
    match old {
        Type::Generic(gen_var) => {
            if var == *gen_var {
                *old = new;
            }
        }
        Type::Numeric(gen_var) => {
            if var == *gen_var {
                *old = new;
            }
        }
        _ => {}
    }
}

impl<'a> TypeContext<'a> {
    fn create_generic(&mut self) -> Type {
        let ty = Type::Generic(self.cur_type_var);
        self.cur_type_var += 1;
        ty
    }

    fn create_numeric(&mut self) -> Type {
        let ty = Type::Numeric(self.cur_type_var);
        self.cur_type_var += 1;
        ty
    }

    fn checkpoint_type_var(&self) -> u32 {
        self.cur_type_var
    }

    fn replay_generic(&self, idx: u32) -> Type {
        Type::Generic(idx)
    }

    fn replay_numeric(&self, idx: u32) -> Type {
        Type::Numeric(idx)
    }

    fn replace_generic(&mut self, var: u32, new: Type) {
        for i in 0..self.types.len() {
            let i = i as usize;
            replace_single_generic(self.types.at_mut(i), var, new);
        }

        for symbol in self.symbols.iter_mut() {
            match symbol {
                Symbol::Variable(_, ty) => {
                    replace_single_generic(ty, var, new);
                }
                Symbol::Function(_, args, ret) => {
                    for i in 0..args.len() {
                        replace_single_generic(args.at_mut(i), var, new);
                    }
                    replace_single_generic(ret, var, new);
                }
                Symbol::Operator(_, arg1, arg2, ret) => {
                    replace_single_generic(arg1, var, new);
                    replace_single_generic(arg2, var, new);
                    replace_single_generic(ret, var, new);
                }
            }
        }
    }

    fn fix_function_ret_ty(&mut self, ret_ty: Type) {
        let last = self.symbols.pop().unwrap();
        if let Symbol::Function(func, args, new_ret_ty) = last {
            if new_ret_ty == ret_ty {
                self.symbols.push(Symbol::Function(func, args, Type::Nil));
            } else {
                self.symbols.push(Symbol::Function(func, args, new_ret_ty));
            }
        } else if let Symbol::Operator(func, arg1, arg2, new_ret_ty) = last {
            if new_ret_ty == ret_ty {
                self.symbols
                    .push(Symbol::Operator(func, arg1, arg2, Type::Nil));
            } else {
                self.symbols
                    .push(Symbol::Operator(func, arg1, arg2, new_ret_ty));
            }
        } else {
            self.symbols.push(last);
        }
    }
}

fn constrain<'a>(dst: Type, src: Type, mut context: TypeContext<'a>) -> Option<TypeContext<'a>> {
    let joined_type = join_types(dst, src)?;
    let get_var = |ty| match ty {
        Type::Generic(var) => var,
        Type::Numeric(var) => var,
        _ => panic!("Unreachable: can't replace non-generic with generic"),
    };
    if joined_type != dst {
        let var = get_var(dst);
        context.replace_generic(var, joined_type);
    }
    if joined_type != src {
        let var = get_var(src);
        context.replace_generic(var, joined_type);
    }
    Some(context)
}

fn call_constrain<'a>(
    dst: Type,
    src: Type,
    mut context: TypeContext<'a>,
) -> Option<TypeContext<'a>> {
    if let (Type::Generic(_), Type::Numeric(_)) = (dst, src) {
        enforce_numeric(dst, context)
    } else {
        let joined_type = join_types(src, dst)?;
        let get_var = |ty| match ty {
            Type::Generic(var) => var,
            Type::Numeric(var) => var,
            _ => panic!("Unreachable: can't replace non-generic with generic"),
        };
        if joined_type != dst {
            let var = get_var(dst);
            context.replace_generic(var, joined_type);
        }
        Some(context)
    }
}

fn enforce_numeric(ty: Type, mut context: TypeContext) -> Option<TypeContext> {
    match ty {
        Type::Number => Some(context),
        Type::Tensor => Some(context),
        Type::Numeric(_) => Some(context),
        Type::Generic(var) => {
            let new_numeric = context.create_numeric();
            context.replace_generic(var, new_numeric);
            Some(context)
        }
        _ => None,
    }
}

pub fn typecheck<'a>(
    ast: &'a bump::List<'a, ASTStmt<'a>>,
    bump: &'a bump::BumpAllocator,
) -> Option<(&'a bump::List<'a, Type>, Vec<Symbol<'a>>)> {
    let mut context = TypeContext {
        bump,
        types: bump.create_list(),
        symbols: vec![],
        ret_type: vec![],
        cur_type_var: 0,
    };
    for i in 0..ast.len() {
        context = typecheck_stmt(ast.at(i), context)?;
    }
    Some((context.types, context.symbols))
}

fn typecheck_stmt<'a>(
    ast: &'a ASTStmt<'a>,
    mut context: TypeContext<'a>,
) -> Option<TypeContext<'a>> {
    match ast {
        ASTStmt::Block(stmts) => {
            let before_symbols = context.symbols.len();
            for i in 0..stmts.len() {
                context = typecheck_stmt(stmts.at(i), context)?;
            }
            context.symbols.truncate(before_symbols);
        }
        ASTStmt::Function(func, args, body) => {
            let arg_ty = context.bump.create_list();
            let ty_var_before = context.checkpoint_type_var();
            for _ in 0..args.len() {
                let ty = context.create_generic();
                arg_ty.push(ty);
            }
            let ret_ty = context.create_generic();
            context.symbols.push(Symbol::Function(func, arg_ty, ret_ty));
            context.ret_type.push(ret_ty);
            let before_len = context.symbols.len();
            for i in 0..args.len() {
                let arg = args.at(i);
                let ty = context.replay_generic(ty_var_before + i as u32);
                context.symbols.push(Symbol::Variable(arg, ty));
            }
            context = typecheck_stmt(body, context)?;
            context.symbols.truncate(before_len);
            context.ret_type.pop().unwrap();
            context.fix_function_ret_ty(ret_ty);
        }
        ASTStmt::Operator(op, arg1, arg2, body) => {
            let ty1 = context.create_generic();
            let ty2 = context.create_generic();
            let ret_ty = context.create_generic();
            context.symbols.push(Symbol::Operator(op, ty1, ty2, ret_ty));
            context.ret_type.push(ret_ty);
            let before_len = context.symbols.len();
            context.symbols.push(Symbol::Variable(arg1, ty1));
            context.symbols.push(Symbol::Variable(arg2, ty2));
            context = typecheck_stmt(body, context)?;
            context.symbols.truncate(before_len);
            context.ret_type.pop().unwrap();
            context.fix_function_ret_ty(ret_ty);
        }
        ASTStmt::If(cond, body) => {
            let (cond_type, new_context) = typecheck_expr(cond, context)?;
            context = constrain(cond_type, Type::Boolean, new_context)?;
            context = typecheck_stmt(body, context)?;
        }
        ASTStmt::While(cond, body) => {
            let (cond_type, new_context) = typecheck_expr(cond, context)?;
            context = constrain(cond_type, Type::Boolean, new_context)?;
            context = typecheck_stmt(body, context)?;
        }
        ASTStmt::Print(expr) => {
            (_, context) = typecheck_expr(expr, context)?;
        }
        ASTStmt::Return(expr) => {
            let (expr_type, new_context) = typecheck_expr(expr, context)?;
            context = constrain(*new_context.ret_type.last()?, expr_type, new_context)?;
        }
        ASTStmt::Verify(expr) => {
            let (expr_type, new_context) = typecheck_expr(expr, context)?;
            context = constrain(expr_type, Type::Boolean, new_context)?;
        }
        ASTStmt::Variable(var, init) => {
            let (init_type, new_context) = typecheck_expr(init, context)?;
            context = new_context;
            context.symbols.push(Symbol::Variable(var, init_type));
        }
        ASTStmt::Expression(expr) => {
            (_, context) = typecheck_expr(expr, context)?;
        }
    }
    Some(context)
}

fn typecheck_expr<'a>(
    ast: &'a ASTExpr<'a>,
    mut context: TypeContext<'a>,
) -> Option<(Type, TypeContext<'a>)> {
    let my_type = match ast {
        ASTExpr::Nil => Type::Nil,
        ASTExpr::Boolean(_) => Type::Boolean,
        ASTExpr::Number(_) => Type::Number,
        ASTExpr::String(_) => Type::String,
        ASTExpr::Identifier(var) => {
            let mut found_type = None;
            for symbol in context.symbols.iter() {
                if let Symbol::Variable(potential_var, potential_type) = symbol {
                    if var == potential_var {
                        found_type = Some(*potential_type);
                        break;
                    }
                }
            }
            found_type?
        }
        ASTExpr::Call(func, args) => {
            let mut real_arg_ty = vec![];
            for i in 0..args.len() {
                let (single_ty, single_context) = typecheck_expr(args.at(i), context)?;
                context = single_context;
                real_arg_ty.push(single_ty);
            }
            let mut found_symbol = None;
            for symbol in context.symbols.iter().rev() {
                match symbol {
                    Symbol::Function(query_func, arg_ty, ret_ty) => {
                        if query_func == func {
                            found_symbol = Some((arg_ty, ret_ty));
                            break;
                        }
                    }
                    _ => {}
                }
            }
            let (arg_ty, ret_ty) = found_symbol?;
            if arg_ty.len() == args.len() {
                let mut cp_arg_ty = vec![];
                for i in 0..arg_ty.len() {
                    cp_arg_ty.push(arg_ty.at(i).clone());
                }
                let mut ret_ty = *ret_ty;
                let mut call_inst_tys = vec![];
                for i in 0..arg_ty.len() {
                    for (single_arg_ty, single_inst_ty) in call_inst_tys.iter() {
                        if *single_arg_ty == cp_arg_ty[i] {
                            context = constrain(real_arg_ty[i], *single_inst_ty, context)?;
                            break;
                        }
                    }
                    let cur_var = context.cur_type_var;
                    context = call_constrain(real_arg_ty[i], cp_arg_ty[i], context)?;
                    if context.cur_type_var != cur_var {
                        call_inst_tys.push((cp_arg_ty[i], context.replay_numeric(cur_var)));
                    } else {
                        call_inst_tys.push((cp_arg_ty[i], real_arg_ty[i]));
                    }
                }
                for (single_arg_ty, single_inst_ty) in call_inst_tys.iter() {
                    if ret_ty == *single_arg_ty {
                        ret_ty = *single_inst_ty;
                        break;
                    }
                }
                ret_ty
            } else {
                None?
            }
        }
        ASTExpr::Index(arr, indices) => {
            let (arr_type, new_context) = typecheck_expr(arr, context)?;
            context = constrain(arr_type, Type::Tensor, new_context)?;
            for i in 0..indices.len() {
                let (idx_type, idx_context) = typecheck_expr(indices.at(i), context)?;
                context = constrain(idx_type, Type::Number, idx_context)?;
            }
            Type::Number
        }
        ASTExpr::ArrayLiteral(contents) => {
            for i in 0..contents.len() {
                let (elem_type, elem_context) = typecheck_expr(contents.at(i), context)?;
                context = constrain(elem_type, Type::Number, elem_context)?;
            }
            Type::Tensor
        }
        ASTExpr::Assign(to, from) => {
            let (to_type, to_context) = typecheck_expr(to, context)?;
            let (from_type, from_context) = typecheck_expr(from, to_context)?;
            context = constrain(to_type, from_type, from_context)?;
            to_type
        }
        ASTExpr::Unary(op, expr) => {
            let (in_type, out_type) = match op {
                ASTUnaryOp::Not => (Type::Boolean, Type::Boolean),
                ASTUnaryOp::Negate => (Type::Number, Type::Number),
                ASTUnaryOp::Shape => (Type::Tensor, Type::Tensor),
            };
            let (unary_type, unary_context) = typecheck_expr(expr, context)?;
            context = constrain(unary_type, in_type, unary_context)?;
            out_type
        }
        ASTExpr::Binary(op, left, right) => {
            let (left_type, left_context) = typecheck_expr(left, context)?;
            let (right_type, right_context) = typecheck_expr(right, left_context)?;
            if *op == ASTBinaryOp::Add
                || *op == ASTBinaryOp::Subtract
                || *op == ASTBinaryOp::Multiply
                || *op == ASTBinaryOp::Divide
                || *op == ASTBinaryOp::Power
            {
                context = constrain(left_type, right_type, right_context)?;
                context = enforce_numeric(left_type, context)?;
                context = enforce_numeric(right_type, context)?;
                *context.types.at(context.types.len() - 1)
            } else if *op == ASTBinaryOp::ShapedAs || *op == ASTBinaryOp::MatrixMultiply {
                context = constrain(left_type, Type::Tensor, right_context)?;
                context = constrain(right_type, Type::Tensor, context)?;
                Type::Tensor
            } else if *op == ASTBinaryOp::Greater
                || *op == ASTBinaryOp::Lesser
                || *op == ASTBinaryOp::GreaterEquals
                || *op == ASTBinaryOp::LesserEquals
            {
                context = constrain(left_type, Type::Number, right_context)?;
                context = constrain(right_type, Type::Number, context)?;
                Type::Boolean
            } else if *op == ASTBinaryOp::EqualsEquals || *op == ASTBinaryOp::NotEquals {
                context = constrain(left_type, right_type, right_context)?;
                Type::Boolean
            } else if *op == ASTBinaryOp::And || *op == ASTBinaryOp::Or {
                context = constrain(left_type, Type::Boolean, right_context)?;
                context = constrain(right_type, Type::Boolean, context)?;
                Type::Boolean
            } else {
                panic!("Unreachable: unimplemented ASTBinaryOp typecheck")
            }
        }
        ASTExpr::CustomBinary(op, left, right) => {
            let (left_ty, left_context) = typecheck_expr(left, context)?;
            let (right_ty, right_context) = typecheck_expr(right, left_context)?;
            let mut found_symbol = None;
            for symbol in right_context.symbols.iter().rev() {
                match symbol {
                    Symbol::Operator(query_op, arg1_ty, arg2_ty, ret_ty) => {
                        if query_op == op {
                            found_symbol = Some((arg1_ty, arg2_ty, ret_ty));
                            break;
                        }
                    }
                    _ => {}
                }
            }
            let (arg1_ty, arg2_ty, ret_ty) = found_symbol?;
            let arg1_ty = *arg1_ty;
            let arg2_ty = *arg2_ty;
            let ret_ty = *ret_ty;
            context = call_constrain(left_ty, arg1_ty, right_context)?;
            if arg1_ty == arg2_ty {
                context = constrain(right_ty, left_ty, context)?;
            }
            context = call_constrain(right_ty, arg2_ty, context)?;
            if ret_ty == arg1_ty {
                left_ty
            } else if ret_ty == arg2_ty {
                right_ty
            } else {
                ret_ty
            }
        }
    };
    context.types.push(my_type);
    Some((my_type, context))
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn typecheck1() {
        let bump = bump::BumpAllocator::new();
        let ast = bump.create_list();
        ast.push(ASTStmt::Variable(b"abc", bump.alloc(ASTExpr::Nil)));
        ast.push(ASTStmt::Variable(
            b"xyz",
            bump.alloc(ASTExpr::Identifier(b"abc")),
        ));
        let typecheck = typecheck(ast, &bump).unwrap().0;
        let correct_list = bump.create_list();
        correct_list.push(Type::Nil);
        correct_list.push(Type::Nil);
        assert_eq!(typecheck, correct_list);
    }

    #[test]
    fn typecheck2() {
        let bump = bump::BumpAllocator::new();
        let ast = bump.create_list();
        ast.push(ASTStmt::Variable(
            b"var1",
            bump.alloc(ASTExpr::Boolean(false)),
        ));
        ast.push(ASTStmt::Variable(b"var2", bump.alloc(ASTExpr::Number(0.0))));
        ast.push(ASTStmt::Variable(
            b"var3",
            bump.alloc(ASTExpr::String(b"string")),
        ));
        ast.push(ASTStmt::Variable(
            b"var4",
            bump.alloc(ASTExpr::Identifier(b"var1")),
        ));
        ast.push(ASTStmt::Variable(
            b"var5",
            bump.alloc(ASTExpr::Identifier(b"var2")),
        ));
        ast.push(ASTStmt::Variable(
            b"var6",
            bump.alloc(ASTExpr::Identifier(b"var3")),
        ));
        let typecheck = typecheck(ast, &bump).unwrap().0;
        let correct_list = bump.create_list_with(&[
            Type::Boolean,
            Type::Number,
            Type::String,
            Type::Boolean,
            Type::Number,
            Type::String,
        ]);
        assert_eq!(typecheck, correct_list);
    }

    #[test]
    fn typecheck3() {
        let bump = bump::BumpAllocator::new();
        let ast = bump.create_list();
        ast.push(ASTStmt::Expression(bump.alloc(ASTExpr::ArrayLiteral(
            bump.create_list_with(&[ASTExpr::Number(0.0)]),
        ))));
        let typecheck = typecheck(ast, &bump).unwrap().0;
        let correct_list = bump.create_list_with(&[Type::Number, Type::Tensor]);
        assert_eq!(typecheck, correct_list);
    }

    #[test]
    fn typecheck4() {
        let bump = bump::BumpAllocator::new();
        let ast = bump.create_list();
        ast.push(ASTStmt::While(
            bump.alloc(ASTExpr::Boolean(true)),
            bump.alloc(ASTStmt::Variable(b"xyz", bump.alloc(ASTExpr::Nil))),
        ));
        let typecheck = typecheck(ast, &bump).unwrap().0;
        let correct_list = bump.create_list_with(&[Type::Boolean, Type::Nil]);
        assert_eq!(typecheck, correct_list);
    }

    #[test]
    fn typecheck5() {
        let bump = bump::BumpAllocator::new();
        let lexed = &parse::lex(
            b"1 + 2; [1, 2, 3] sa [3, 1]; 7 ^ (4 * 5); 1 >= 2; 5 == 4;",
            &bump,
        )
        .unwrap();
        let (ast, rest) = parse::parse_program(lexed, &bump).unwrap();
        assert_eq!(rest, &[]);
        let typecheck = typecheck(ast, &bump).unwrap().0;
        let correct_list = bump.create_list_with(&[
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Tensor,
            Type::Number,
            Type::Number,
            Type::Tensor,
            Type::Tensor,
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Boolean,
            Type::Number,
            Type::Number,
            Type::Boolean,
        ]);
        assert_eq!(typecheck, correct_list);
    }

    #[test]
    fn typecheck6() {
        let bump = bump::BumpAllocator::new();
        let lexed = &parse::lex(b"f xyz(var) { var; }", &bump).unwrap();
        let (ast, rest) = parse::parse_program(lexed, &bump).unwrap();
        assert_eq!(rest, &[]);
        let (typecheck, symbols) = typecheck(ast, &bump).unwrap();
        let correct_list = bump.create_list_with(&[Type::Generic(0)]);
        assert_eq!(typecheck, correct_list);
        assert_eq!(
            symbols,
            vec![Symbol::Function(
                b"xyz",
                bump.create_list_with(&[Type::Generic(0)]),
                Type::Nil,
            )]
        );
    }

    #[test]
    fn typecheck7() {
        let bump = bump::BumpAllocator::new();
        let lexed = &parse::lex(b"f xyz(abc, def) { r abc + def; }", &bump).unwrap();
        let (ast, rest) = parse::parse_program(lexed, &bump).unwrap();
        assert_eq!(rest, &[]);
        let (typecheck, symbols) = typecheck(ast, &bump).unwrap();
        let correct_list =
            bump.create_list_with(&[Type::Numeric(4), Type::Numeric(4), Type::Numeric(4)]);
        assert_eq!(typecheck, correct_list);
        assert_eq!(
            symbols,
            vec![Symbol::Function(
                b"xyz",
                bump.create_list_with(&[Type::Numeric(4), Type::Numeric(4)]),
                Type::Numeric(4),
            )]
        );
    }

    #[test]
    fn typecheck8() {
        let bump = bump::BumpAllocator::new();
        let lexed = &parse::lex(b"o xyz(abc, def) { r abc + def; }", &bump).unwrap();
        let (ast, rest) = parse::parse_program(lexed, &bump).unwrap();
        assert_eq!(rest, &[]);
        let (typecheck, symbols) = typecheck(ast, &bump).unwrap();
        let correct_list =
            bump.create_list_with(&[Type::Numeric(4), Type::Numeric(4), Type::Numeric(4)]);
        assert_eq!(typecheck, correct_list);
        assert_eq!(
            symbols,
            vec![Symbol::Operator(
                b"xyz",
                Type::Numeric(4),
                Type::Numeric(4),
                Type::Numeric(4),
            )]
        );
    }

    #[test]
    fn typecheck9() {
        let bump = bump::BumpAllocator::new();
        let lexed = &parse::lex(b"f xyz(abc, def) { r abc + def; } xyz(1, 2);", &bump).unwrap();
        let (ast, rest) = parse::parse_program(lexed, &bump).unwrap();
        assert_eq!(rest, &[]);
        let (typecheck, symbols) = typecheck(ast, &bump).unwrap();
        let correct_list = bump.create_list_with(&[
            Type::Numeric(4),
            Type::Numeric(4),
            Type::Numeric(4),
            Type::Number,
            Type::Number,
            Type::Number,
        ]);
        assert_eq!(typecheck, correct_list);
        assert_eq!(
            symbols,
            vec![Symbol::Function(
                b"xyz",
                bump.create_list_with(&[Type::Numeric(4), Type::Numeric(4)]),
                Type::Numeric(4),
            )]
        );
    }

    #[test]
    fn typecheck10() {
        let bump = bump::BumpAllocator::new();
        let lexed = &parse::lex(b"f xyz(abc, def) { r abc + def; } xyz(1, 2); xyz([1], [2]); f pw(x) { p x; } pw(\"hello\"); f func(x, y, z, xx, yy, zz) { p x + y; p z + x; p xx == yy; p zz; }", &bump).unwrap();
        let (ast, rest) = parse::parse_program(lexed, &bump).unwrap();
        assert_eq!(rest, &[]);
        let (typecheck, symbols) = typecheck(ast, &bump).unwrap();
        let correct_list = bump.create_list_with(&[
            Type::Numeric(4),
            Type::Numeric(4),
            Type::Numeric(4),
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Number,
            Type::Tensor,
            Type::Number,
            Type::Tensor,
            Type::Tensor,
            Type::Generic(5),
            Type::String,
            Type::Nil,
            Type::Numeric(15),
            Type::Numeric(15),
            Type::Numeric(15),
            Type::Numeric(15),
            Type::Numeric(15),
            Type::Numeric(15),
            Type::Generic(11),
            Type::Generic(11),
            Type::Boolean,
            Type::Generic(12),
        ]);
        assert_eq!(typecheck, correct_list);
        assert_eq!(
            symbols,
            vec![
                Symbol::Function(
                    b"xyz",
                    bump.create_list_with(&[Type::Numeric(4), Type::Numeric(4)]),
                    Type::Numeric(4),
                ),
                Symbol::Function(b"pw", bump.create_list_with(&[Type::Generic(5)]), Type::Nil),
                Symbol::Function(
                    b"func",
                    bump.create_list_with(&[
                        Type::Numeric(15),
                        Type::Numeric(15),
                        Type::Numeric(15),
                        Type::Generic(11),
                        Type::Generic(11),
                        Type::Generic(12)
                    ]),
                    Type::Nil
                )
            ]
        );
    }

    #[test]
    fn typecheck11() {
        let bump = bump::BumpAllocator::new();
        let lexed =
            &parse::lex(b"f xyz(x) { r x; } f abc(x) { r xyz(x) + xyz(x); }", &bump).unwrap();
        let (ast, rest) = parse::parse_program(lexed, &bump).unwrap();
        assert_eq!(rest, &[]);
        let (typecheck, symbols) = typecheck(ast, &bump).unwrap();
        let correct_list = bump.create_list_with(&[
            Type::Generic(0),
            Type::Numeric(4),
            Type::Numeric(4),
            Type::Numeric(4),
            Type::Numeric(4),
            Type::Numeric(4),
        ]);
        assert_eq!(typecheck, correct_list);
        assert_eq!(
            symbols,
            vec![
                Symbol::Function(
                    b"xyz",
                    bump.create_list_with(&[Type::Generic(0)]),
                    Type::Generic(0),
                ),
                Symbol::Function(
                    b"abc",
                    bump.create_list_with(&[Type::Numeric(4)]),
                    Type::Numeric(4),
                ),
            ]
        );
    }

    #[test]
    fn typecheck12() {
        let bump = bump::BumpAllocator::new();
        let lexed = &parse::lex(b"f xyz(x) { r x + x; } o abc(x, y) { r xyz(x); }", &bump).unwrap();
        let (ast, rest) = parse::parse_program(lexed, &bump).unwrap();
        assert_eq!(rest, &[]);
        let (typecheck, symbols) = typecheck(ast, &bump).unwrap();
        let correct_list = bump.create_list_with(&[
            Type::Numeric(2),
            Type::Numeric(2),
            Type::Numeric(2),
            Type::Numeric(7),
            Type::Numeric(7),
        ]);
        assert_eq!(typecheck, correct_list);
        assert_eq!(
            symbols,
            vec![
                Symbol::Function(
                    b"xyz",
                    bump.create_list_with(&[Type::Numeric(2)]),
                    Type::Numeric(2),
                ),
                Symbol::Operator(b"abc", Type::Numeric(7), Type::Generic(5), Type::Numeric(7),),
            ]
        );
    }
}
