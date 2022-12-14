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
extern crate semant;

use std::collections::HashMap;

use crate::*;
use bump::bump_list;
use parse::ASTBinaryOp;
use parse::ASTUnaryOp;
use semant::Type;
use semant::TypedASTExpr;
use semant::TypedASTStmt;
use semant::TypedProgram;

pub fn irgen<'a>(program: TypedProgram<'a>, bump: &'a bump::BumpAllocator) -> IRModule<'a> {
    let mut context = IRGenContext::new(bump);
    context.irgen_program(program);
    context.module
}

fn convert_type(ty: Type) -> IRType {
    match ty {
        Type::Nil => IRType::Nil,
        Type::Number => IRType::Number,
        Type::Tensor => IRType::Tensor,
        Type::Boolean => IRType::Boolean,
        Type::String => IRType::String,
        _ => panic!("PANIC: Can't convert type variable to concrete IR type."),
    }
}

struct IRGenContext<'a> {
    module: IRModule<'a>,
    ast_types: &'a [Type],
    curr_func: IRFunctionID,
    curr_block: IRBasicBlockID,
    curr_vars: HashMap<&'a [u8], IRRegister>,
    curr_num_regs: IRRegisterID,
    func_defs: HashMap<
        &'a [u8],
        (
            &'a bump::List<'a, &'a [u8]>,
            &'a bump::List<'a, Type>,
            &'a TypedASTStmt<'a>,
            Type,
        ),
    >,
    op_defs: HashMap<&'a [u8], (&'a [u8], &'a [u8], Type, Type, &'a TypedASTStmt<'a>, Type)>,
    called_funcs: HashMap<&'a [u8], IRFunctionID>,
    found_return: bool,
    bump: &'a bump::BumpAllocator,
}

impl<'a> IRGenContext<'a> {
    fn new(bump: &'a bump::BumpAllocator) -> Self {
        let context = IRGenContext {
            module: IRModule {
                funcs: bump_list!(
                    bump,
                    IRFunction {
                        name: b"@main",
                        params: bump.create_list(),
                        ret_type: IRType::Nil,
                        blocks: bump_list!(
                            bump,
                            IRBasicBlock {
                                insts: bump.create_list(),
                            }
                        ),
                    }
                ),
            },
            ast_types: &[],
            curr_func: 0,
            curr_block: 0,
            curr_vars: HashMap::new(),
            curr_num_regs: 0,
            func_defs: HashMap::new(),
            op_defs: HashMap::new(),
            called_funcs: HashMap::new(),
            found_return: false,
            bump,
        };
        context
    }

    #[allow(dead_code)]
    fn get_curr_func(&self) -> &IRFunction<'a> {
        return self.module.funcs.at(self.curr_func as usize);
    }

    fn get_curr_func_mut(&mut self) -> &mut IRFunction<'a> {
        return self.module.funcs.at_mut(self.curr_func as usize);
    }

    #[allow(dead_code)]
    fn get_curr_block(&self) -> &IRBasicBlock<'a> {
        return self
            .module
            .funcs
            .at(self.curr_func as usize)
            .blocks
            .at(self.curr_block as usize);
    }

    fn get_curr_block_mut(&mut self) -> &mut IRBasicBlock<'a> {
        return self
            .module
            .funcs
            .at_mut(self.curr_func as usize)
            .blocks
            .at_mut(self.curr_block as usize);
    }

    fn fresh_reg(&mut self, ty: IRType) -> IRRegister {
        let id = self.curr_num_regs;
        self.curr_num_regs += 1;
        (id, ty)
    }

    fn fresh_block(&mut self) -> IRBasicBlockID {
        let block = IRBasicBlock {
            insts: self.bump.create_list(),
        };
        let func = self.get_curr_func_mut();
        let id = func.blocks.len() as IRBasicBlockID;
        func.blocks.push(block);
        id
    }

    fn fresh_func(
        &mut self,
        name: &'a [u8],
        params: &'a mut bump::List<'a, IRRegister>,
        ret_type: IRType,
    ) -> IRFunctionID {
        let func_id = self.module.funcs.len() as IRFunctionID;
        self.module.funcs.push(IRFunction {
            name,
            params,
            ret_type,
            blocks: bump_list!(
                self.bump,
                IRBasicBlock {
                    insts: self.bump.create_list()
                }
            ),
        });
        self.curr_func = func_id;
        self.curr_block = 0;
        func_id
    }

    fn add_inst(&mut self, inst: IRInstruction<'a>) {
        self.get_curr_block_mut().insts.push(inst);
    }

    fn get_type(&self, expr: &'a TypedASTExpr<'a>) -> Type {
        if let Type::Generic(var) = expr.get_type() {
            self.ast_types[var as usize]
        } else if let Type::Numeric(_) = expr.get_type() {
            panic!("PANIC: Numeric type variable in unconstrained typed AST.")
        } else {
            expr.get_type()
        }
    }

    fn get_irfunc_name(&self, func: &'a [u8], args: &'a bump::List<'a, IRRegister>) -> &'a [u8] {
        let size = 3 + func.len() + args.len();
        let name = unsafe { self.bump.alloc_slice_raw(size) };
        name[0] = b'@';
        name[1] = b'f';
        name[2] = b'_';
        for i in 0..func.len() {
            name[i + 3] = func[i];
        }
        for i in 0..args.len() {
            name[i + func.len() + 3] = match args.at(i).1 {
                IRType::Nil => b'0',
                IRType::Boolean => b'1',
                IRType::String => b'2',
                IRType::Number => b'3',
                IRType::Tensor => b'4',
            };
        }
        name
    }

    fn get_irop_name(&self, func: &'a [u8], left: IRRegister, right: IRRegister) -> &'a [u8] {
        let size = 3 + func.len() + 2;
        let name = unsafe { self.bump.alloc_slice_raw(size) };
        name[0] = b'@';
        name[1] = b'o';
        name[2] = b'_';
        for i in 0..func.len() {
            name[i + 3] = func[i];
        }
        name[func.len() + 3] = match left.1 {
            IRType::Nil => b'0',
            IRType::Boolean => b'1',
            IRType::String => b'2',
            IRType::Number => b'3',
            IRType::Tensor => b'4',
        };
        name[func.len() + 3] = match right.1 {
            IRType::Nil => b'0',
            IRType::Boolean => b'1',
            IRType::String => b'2',
            IRType::Number => b'3',
            IRType::Tensor => b'4',
        };
        name
    }

    fn irgen_program(&mut self, program: TypedProgram<'a>) {
        self.ast_types = program.1;
        for i in 0..program.0.len() {
            self.irgen_stmt(program.0.at(i));
        }
        if !self.found_return {
            let reg = self.fresh_reg(IRType::Nil);
            self.add_inst(IRInstruction::Immediate(reg, IRConstant::Nil));
            self.add_inst(IRInstruction::Return(reg));
        }
    }

    fn irgen_stmt(&mut self, stmt: &'a TypedASTStmt<'a>) {
        match stmt {
            TypedASTStmt::Block(stmts) => {
                for i in 0..stmts.len() {
                    self.irgen_stmt(stmts.at(i));
                }
            }
            TypedASTStmt::Function(name, params, params_ty, body, ret_ty) => {
                self.func_defs
                    .insert(name, (params, params_ty, body, *ret_ty));
            }
            TypedASTStmt::Operator(
                name,
                left_param,
                right_param,
                left_param_ty,
                right_param_ty,
                body,
                ret_ty,
            ) => {
                self.op_defs.insert(
                    name,
                    (
                        left_param,
                        right_param,
                        *left_param_ty,
                        *right_param_ty,
                        body,
                        *ret_ty,
                    ),
                );
            }
            TypedASTStmt::If(cond, body) => {
                let cond_reg = self.irgen_expr(cond);
                let body_block = self.fresh_block();
                let post_block = self.fresh_block();
                self.add_inst(IRInstruction::BranchCond(cond_reg, body_block, post_block));
                self.curr_block = body_block;
                self.irgen_stmt(body);
                if !self.get_curr_block().is_terminated() {
                    self.add_inst(IRInstruction::BranchUncond(post_block));
                }
                self.curr_block = post_block;
            }
            TypedASTStmt::While(cond, body) => {
                let cond_block = self.fresh_block();
                let body_block = self.fresh_block();
                let post_block = self.fresh_block();
                self.add_inst(IRInstruction::BranchUncond(cond_block));
                self.curr_block = cond_block;
                let cond_reg = self.irgen_expr(cond);
                self.add_inst(IRInstruction::BranchCond(cond_reg, body_block, post_block));
                self.curr_block = body_block;
                self.irgen_stmt(body);
                if !self.get_curr_block().is_terminated() {
                    self.add_inst(IRInstruction::BranchUncond(cond_block));
                }
                self.curr_block = post_block;
            }
            TypedASTStmt::Print(expr) => {
                let reg = self.irgen_expr(expr);
                self.add_inst(IRInstruction::Print(reg));
            }
            TypedASTStmt::Line(name) => {
                let reg = *self
                    .curr_vars
                    .get(name)
                    .expect("PANIC: Variable not in scope.");
                self.add_inst(IRInstruction::Line(reg));
            }
            TypedASTStmt::Return(expr) => {
                let reg = self.irgen_expr(expr);
                self.found_return = true;
                self.add_inst(IRInstruction::Return(reg));
            }
            TypedASTStmt::Verify(expr) => {
                let reg = self.irgen_expr(expr);
                self.add_inst(IRInstruction::Verify(reg));
            }
            TypedASTStmt::Variable(name, expr) => {
                let reg = self.irgen_expr(expr);
                let var_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                self.curr_vars.insert(name, var_reg);
                self.add_inst(IRInstruction::Copy(var_reg, reg));
            }
            TypedASTStmt::Expression(expr) => {
                self.irgen_expr(expr);
            }
        }
    }

    fn irgen_expr(&mut self, expr: &'a TypedASTExpr<'a>) -> IRRegister {
        match expr {
            TypedASTExpr::Nil => {
                let reg = self.fresh_reg(IRType::Nil);
                self.add_inst(IRInstruction::Immediate(reg, IRConstant::Nil));
                reg
            }
            TypedASTExpr::Boolean(v) => {
                let reg = self.fresh_reg(IRType::Boolean);
                self.add_inst(IRInstruction::Immediate(reg, IRConstant::Boolean(*v)));
                reg
            }
            TypedASTExpr::Number(v) => {
                let reg = self.fresh_reg(IRType::Number);
                self.add_inst(IRInstruction::Immediate(reg, IRConstant::Number(*v)));
                reg
            }
            TypedASTExpr::String(v) => {
                let reg = self.fresh_reg(IRType::String);
                self.add_inst(IRInstruction::Immediate(reg, IRConstant::String(*v)));
                reg
            }
            TypedASTExpr::Identifier(v, _) => *self
                .curr_vars
                .get(v)
                .expect("PANIC: Variable not in scope."),
            TypedASTExpr::Call(func, args, _) => {
                let arg_regs = self.bump.create_list();
                for i in 0..args.len() {
                    arg_regs.push(self.irgen_expr(args.at(i)));
                }
                let result_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                let func_name = self.get_irfunc_name(func, arg_regs);
                let func_id = self
                    .called_funcs
                    .get(func_name)
                    .map(|x| *x)
                    .unwrap_or_else(|| {
                        let new_ast_types = self.bump.alloc_slice(self.ast_types);
                        let func_def = self
                            .func_defs
                            .get(func)
                            .expect("PANIC: Function called before definition.");
                        for i in 0..args.len() {
                            if let Type::Generic(var) = func_def.1.at(i) {
                                let new_type = self.get_type(args.at(i));
                                assert!(
                                    new_type.is_gen().is_none(),
                                    "PANIC: New type is not concrete."
                                );
                                new_ast_types[semant::traverse(*var, new_ast_types)] = new_type;
                            }
                        }
                        semant::cleanup_types(new_ast_types);

                        let mut old_curr_vars = HashMap::new();
                        let old_found_return = self.found_return;
                        core::mem::swap(&mut self.curr_vars, &mut old_curr_vars);
                        let old_num_regs = self.curr_num_regs;
                        self.curr_num_regs = arg_regs.len() as IRRegisterID;

                        let params = self.bump.create_list();
                        for i in 0..arg_regs.len() {
                            let param_reg = (i as u32, arg_regs.at(i).1);
                            params.push(param_reg);
                            self.curr_vars.insert(func_def.0.at(i), param_reg);
                        }
                        let body = func_def.2;

                        let old_ast_types = self.ast_types;
                        self.ast_types = new_ast_types;
                        let old_func_id = self.curr_func;
                        let old_block_id = self.curr_block;
                        let func_id = self.fresh_func(func_name, params, result_reg.1);
                        self.irgen_stmt(body);
                        if !self.found_return {
                            let reg = self.fresh_reg(IRType::Nil);
                            self.add_inst(IRInstruction::Immediate(reg, IRConstant::Nil));
                            self.add_inst(IRInstruction::Return(reg));
                        }
                        self.found_return = old_found_return;
                        self.curr_num_regs = old_num_regs;
                        core::mem::swap(&mut self.curr_vars, &mut old_curr_vars);
                        self.curr_func = old_func_id;
                        self.curr_block = old_block_id;
                        self.ast_types = old_ast_types;

                        self.called_funcs.insert(func_name, func_id);
                        func_id
                    });
                self.add_inst(IRInstruction::Call(
                    result_reg,
                    (func_id, func_name),
                    arg_regs,
                ));
                result_reg
            }
            TypedASTExpr::Index(tensor, indices) => {
                let tensor_reg = self.irgen_expr(tensor);
                let index_regs = self.bump.create_list();
                for i in 0..indices.len() {
                    index_regs.push(self.irgen_expr(indices.at(i)));
                }
                let result_reg = self.fresh_reg(IRType::Number);
                self.add_inst(IRInstruction::Index(result_reg, tensor_reg, index_regs));
                result_reg
            }
            TypedASTExpr::ArrayLiteral(elements) => {
                let contents = self.bump.create_list();
                for i in 0..elements.len() {
                    contents.push(self.irgen_expr(elements.at(i)));
                }
                let result_reg = self.fresh_reg(IRType::Tensor);
                self.add_inst(IRInstruction::Array(result_reg, contents));
                result_reg
            }
            TypedASTExpr::Assign(left, right, _) => {
                let right_reg = self.irgen_expr(right);
                match left {
                    TypedASTExpr::Identifier(left_var, _) => {
                        let var_reg = *self.curr_vars.get(left_var).expect("PANIC: Variable not in scope.");
                        self.add_inst(IRInstruction::Copy(var_reg, right_reg));
                        right_reg
                    }
                    TypedASTExpr::Index(TypedASTExpr::Identifier(left_var, _), indices) => {
                        let var_reg = *self.curr_vars.get(left_var).expect("PANIC: Variable not in scope.");
                        let index_regs = self.bump.create_list();
                        for i in 0..indices.len() {
                            index_regs.push(self.irgen_expr(indices.at(i)));
                        }
                        self.add_inst(IRInstruction::Index(var_reg, right_reg, index_regs));
                        right_reg
                    }
                    _ => panic!("PANIC: Something other than identifier or indexing an identifier on left-hand side of assign expression.")
                }
            }
            TypedASTExpr::Unary(op, expr, _) => {
                let right_reg = self.irgen_expr(expr);
                let result_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                self.add_inst(IRInstruction::Unary(
                    result_reg,
                    match op {
                        ASTUnaryOp::Not => IRUnaryOp::Not,
                        ASTUnaryOp::Negate => IRUnaryOp::Negate,
                        ASTUnaryOp::Shape => IRUnaryOp::Shape,
                    },
                    right_reg,
                ));
                result_reg
            }
            TypedASTExpr::Binary(op, left, right, _) => {
                let left_reg = self.irgen_expr(left);
                let right_reg = self.irgen_expr(right);
                let result_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                let op = match (op, left_reg.1) {
                    (ASTBinaryOp::ShapedAs, _) => IRBinaryOp::ShapedAs,
                    (ASTBinaryOp::Add, IRType::Number) => IRBinaryOp::AddNumbers,
                    (ASTBinaryOp::Add, IRType::Tensor) => IRBinaryOp::AddTensors,
                    (ASTBinaryOp::Subtract, IRType::Number) => IRBinaryOp::SubtractNumbers,
                    (ASTBinaryOp::Subtract, IRType::Tensor) => IRBinaryOp::SubtractTensors,
                    (ASTBinaryOp::Multiply, IRType::Number) => IRBinaryOp::MultiplyNumbers,
                    (ASTBinaryOp::Multiply, IRType::Tensor) => IRBinaryOp::MultiplyTensors,
                    (ASTBinaryOp::Divide, IRType::Number) => IRBinaryOp::DivideNumbers,
                    (ASTBinaryOp::Divide, IRType::Tensor) => IRBinaryOp::DivideTensors,
                    (ASTBinaryOp::Power, IRType::Number) => IRBinaryOp::PowerNumbers,
                    (ASTBinaryOp::Power, IRType::Tensor) => IRBinaryOp::PowerTensors,
                    (ASTBinaryOp::MatrixMultiply, _) => IRBinaryOp::MatrixMultiply,
                    (ASTBinaryOp::Greater, _) => IRBinaryOp::Greater,
                    (ASTBinaryOp::Lesser, _) => IRBinaryOp::Lesser,
                    (ASTBinaryOp::NotEquals, IRType::Nil) => IRBinaryOp::NotEqualsNils,
                    (ASTBinaryOp::NotEquals, IRType::Boolean) => IRBinaryOp::NotEqualsBooleans,
                    (ASTBinaryOp::NotEquals, IRType::String) => IRBinaryOp::NotEqualsStrings,
                    (ASTBinaryOp::NotEquals, IRType::Number) => IRBinaryOp::NotEqualsNumbers,
                    (ASTBinaryOp::NotEquals, IRType::Tensor) => IRBinaryOp::NotEqualsTensors,
                    (ASTBinaryOp::EqualsEquals, IRType::Nil) => IRBinaryOp::EqualsEqualsNils,
                    (ASTBinaryOp::EqualsEquals, IRType::Boolean) => {
                        IRBinaryOp::EqualsEqualsBooleans
                    }
                    (ASTBinaryOp::EqualsEquals, IRType::String) => IRBinaryOp::EqualsEqualsStrings,
                    (ASTBinaryOp::EqualsEquals, IRType::Number) => IRBinaryOp::EqualsEqualsNumbers,
                    (ASTBinaryOp::EqualsEquals, IRType::Tensor) => IRBinaryOp::EqualsEqualsTensors,
                    (ASTBinaryOp::GreaterEquals, _) => IRBinaryOp::GreaterEquals,
                    (ASTBinaryOp::LesserEquals, _) => IRBinaryOp::LesserEquals,
                    (ASTBinaryOp::And, _) => IRBinaryOp::And,
                    (ASTBinaryOp::Or, _) => IRBinaryOp::Or,
                    _ => panic!("PANIC: Unsupported binary operation configuration."),
                };
                self.add_inst(IRInstruction::Binary(result_reg, op, left_reg, right_reg));
                result_reg
            }
            TypedASTExpr::CustomBinary(func, left, right, _) => {
                let left_reg = self.irgen_expr(left);
                let right_reg = self.irgen_expr(right);
                let arg_regs = bump_list!(self.bump, left_reg, right_reg);
                let result_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                let func_name = self.get_irop_name(func, left_reg, right_reg);
                let func_id = self
                    .called_funcs
                    .get(func_name)
                    .map(|x| *x)
                    .unwrap_or_else(|| {
                        let new_ast_types = self.bump.alloc_slice(self.ast_types);
                        let op_def = self
                            .op_defs
                            .get(func)
                            .expect("PANIC: Operator called before definition.");
                        if let Type::Generic(var) = op_def.2 {
                            let new_type = self.get_type(left);
                            assert!(
                                new_type.is_gen().is_none(),
                                "PANIC: New type is not concrete."
                            );
                            new_ast_types[semant::traverse(var, new_ast_types)] = new_type;
                        }
                        if let Type::Generic(var) = op_def.3 {
                            let new_type = self.get_type(right);
                            assert!(
                                new_type.is_gen().is_none(),
                                "PANIC: New type is not concrete."
                            );
                            new_ast_types[semant::traverse(var, new_ast_types)] = new_type;
                        }
                        semant::cleanup_types(new_ast_types);

                        let mut old_curr_vars = HashMap::new();
                        let old_found_return = self.found_return;
                        core::mem::swap(&mut self.curr_vars, &mut old_curr_vars);
                        let old_num_regs = self.curr_num_regs;
                        self.curr_num_regs = arg_regs.len() as IRRegisterID;

                        let params = self.bump.create_list();
                        for i in 0..arg_regs.len() {
                            let param_reg = (i as u32, arg_regs.at(i).1);
                            params.push(param_reg);
                            self.curr_vars.insert(&[op_def.0, op_def.1][i], param_reg);
                        }
                        let body = op_def.4;

                        let old_ast_types = self.ast_types;
                        self.ast_types = new_ast_types;
                        let old_func_id = self.curr_func;
                        let old_block_id = self.curr_block;
                        let func_id = self.fresh_func(func_name, params, result_reg.1);
                        self.irgen_stmt(body);
                        if !self.found_return {
                            let reg = self.fresh_reg(IRType::Nil);
                            self.add_inst(IRInstruction::Immediate(reg, IRConstant::Nil));
                            self.add_inst(IRInstruction::Return(reg));
                        }
                        self.found_return = old_found_return;
                        self.curr_num_regs = old_num_regs;
                        core::mem::swap(&mut self.curr_vars, &mut old_curr_vars);
                        self.curr_func = old_func_id;
                        self.curr_block = old_block_id;
                        self.ast_types = old_ast_types;

                        self.called_funcs.insert(func_name, func_id);
                        func_id
                    });
                self.add_inst(IRInstruction::Call(
                    result_reg,
                    (func_id, func_name),
                    arg_regs,
                ));
                result_reg
            }
        }
    }
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn irgen_simple() {
        let bump = bump::BumpAllocator::new();
        let pairs = &mut [
            (
                b"a x = 0; p x;" as &[_],
                bump_list!(
                    bump,
                    IRInstruction::Immediate((0, IRType::Number), IRConstant::Number(0.0)),
                    IRInstruction::Copy((1, IRType::Number), (0, IRType::Number)),
                    IRInstruction::Print((1, IRType::Number))
                ),
            ),
            (
                b"p 3 + 5;",
                bump_list!(
                    bump,
                    IRInstruction::Immediate((0, IRType::Number), IRConstant::Number(3.0)),
                    IRInstruction::Immediate((1, IRType::Number), IRConstant::Number(5.0)),
                    IRInstruction::Binary(
                        (2, IRType::Number),
                        IRBinaryOp::AddNumbers,
                        (0, IRType::Number),
                        (1, IRType::Number)
                    ),
                    IRInstruction::Print((2, IRType::Number))
                ),
            ),
            (
                b"a x = 5; p 3 + x; a y = x ^ 2; a z = [y];",
                bump_list!(
                    bump,
                    IRInstruction::Immediate((0, IRType::Number), IRConstant::Number(5.0)),
                    IRInstruction::Copy((1, IRType::Number), (0, IRType::Number)),
                    IRInstruction::Immediate((2, IRType::Number), IRConstant::Number(3.0)),
                    IRInstruction::Binary(
                        (3, IRType::Number),
                        IRBinaryOp::AddNumbers,
                        (2, IRType::Number),
                        (1, IRType::Number)
                    ),
                    IRInstruction::Print((3, IRType::Number)),
                    IRInstruction::Immediate((4, IRType::Number), IRConstant::Number(2.0)),
                    IRInstruction::Binary(
                        (5, IRType::Number),
                        IRBinaryOp::PowerNumbers,
                        (1, IRType::Number),
                        (4, IRType::Number)
                    ),
                    IRInstruction::Copy((6, IRType::Number), (5, IRType::Number)),
                    IRInstruction::Array(
                        (7, IRType::Tensor),
                        bump_list!(bump, (6, IRType::Number))
                    ),
                    IRInstruction::Copy((8, IRType::Tensor), (7, IRType::Tensor))
                ),
            ),
        ];
        for (input, output) in pairs.iter_mut() {
            let tokens = parse::lex(input, &bump).unwrap();
            let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
            let typed_program = semant::typecheck_program(ast, &bump).unwrap();
            let ir_program = irgen(typed_program, &bump);
            assert_eq!(
                ir_program,
                IRModule {
                    funcs: bump_list!(
                        bump,
                        IRFunction {
                            name: b"@main",
                            params: bump.create_list(),
                            ret_type: IRType::Nil,
                            blocks: bump_list!(bump, IRBasicBlock { insts: *output })
                        }
                    )
                }
            );
        }
    }

    #[test]
    fn irgen_complex1() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"i (T) { p N; } w (T) { p N; }", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = semant::typecheck_program(ast, &bump).unwrap();
        let ir_program = irgen(typed_program, &bump);
        assert_eq!(
            ir_program,
            IRModule {
                funcs: bump_list!(
                    bump,
                    IRFunction {
                        name: b"@main",
                        params: bump.create_list(),
                        ret_type: IRType::Nil,
                        blocks: bump_list!(
                            bump,
                            IRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    IRInstruction::Immediate(
                                        (0, IRType::Boolean),
                                        IRConstant::Boolean(true)
                                    ),
                                    IRInstruction::BranchCond((0, IRType::Boolean), 1, 2)
                                )
                            },
                            IRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    IRInstruction::Immediate((1, IRType::Nil), IRConstant::Nil),
                                    IRInstruction::Print((1, IRType::Nil)),
                                    IRInstruction::BranchUncond(2)
                                )
                            },
                            IRBasicBlock {
                                insts: bump_list!(bump, IRInstruction::BranchUncond(3))
                            },
                            IRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    IRInstruction::Immediate(
                                        (2, IRType::Boolean),
                                        IRConstant::Boolean(true)
                                    ),
                                    IRInstruction::BranchCond((2, IRType::Boolean), 4, 5)
                                )
                            },
                            IRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    IRInstruction::Immediate((3, IRType::Nil), IRConstant::Nil),
                                    IRInstruction::Print((3, IRType::Nil)),
                                    IRInstruction::BranchUncond(3)
                                )
                            },
                            IRBasicBlock {
                                insts: bump_list!(bump,)
                            }
                        )
                    }
                )
            }
        );
    }

    #[test]
    fn irgen_complex2() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f xyz() { p N; r 5; } p xyz();", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = semant::typecheck_program(ast, &bump).unwrap();
        let ir_program = irgen(typed_program, &bump);
        assert_eq!(
            ir_program,
            IRModule {
                funcs: bump_list!(
                    bump,
                    IRFunction {
                        name: b"@main",
                        params: bump.create_list(),
                        ret_type: IRType::Nil,
                        blocks: bump_list!(
                            bump,
                            IRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    IRInstruction::Call(
                                        (0, IRType::Number),
                                        (1, b"@f_xyz"),
                                        bump_list!(bump,)
                                    ),
                                    IRInstruction::Print((0, IRType::Number))
                                )
                            }
                        )
                    },
                    IRFunction {
                        name: b"@f_xyz",
                        params: bump.create_list(),
                        ret_type: IRType::Number,
                        blocks: bump_list!(
                            bump,
                            IRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    IRInstruction::Immediate((0, IRType::Nil), IRConstant::Nil),
                                    IRInstruction::Print((0, IRType::Nil)),
                                    IRInstruction::Immediate(
                                        (1, IRType::Number),
                                        IRConstant::Number(5.0)
                                    ),
                                    IRInstruction::Return((1, IRType::Number))
                                )
                            }
                        )
                    }
                )
            }
        );
    }

    #[test]
    fn irgen_complex3() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f xyz(x, y) { r x + y; } p xyz(5, 4); p xyz(1, 2); p xyz([2], [7]);",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = semant::typecheck_program(ast, &bump).unwrap();
        let ir_program = irgen(typed_program, &bump);
        assert_eq!(
            ir_program,
            IRModule {
                funcs: bump_list!(
                    bump,
                    IRFunction {
                        name: b"@main",
                        params: bump.create_list(),
                        ret_type: IRType::Nil,
                        blocks: bump_list!(
                            bump,
                            IRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    IRInstruction::Immediate(
                                        (0, IRType::Number),
                                        IRConstant::Number(5.0)
                                    ),
                                    IRInstruction::Immediate(
                                        (1, IRType::Number),
                                        IRConstant::Number(4.0)
                                    ),
                                    IRInstruction::Call(
                                        (2, IRType::Number),
                                        (1, b"@f_xyz33"),
                                        bump_list!(bump, (0, IRType::Number), (1, IRType::Number))
                                    ),
                                    IRInstruction::Print((2, IRType::Number)),
                                    IRInstruction::Immediate(
                                        (3, IRType::Number),
                                        IRConstant::Number(1.0)
                                    ),
                                    IRInstruction::Immediate(
                                        (4, IRType::Number),
                                        IRConstant::Number(2.0)
                                    ),
                                    IRInstruction::Call(
                                        (5, IRType::Number),
                                        (1, b"@f_xyz33"),
                                        bump_list!(bump, (3, IRType::Number), (4, IRType::Number))
                                    ),
                                    IRInstruction::Print((5, IRType::Number)),
                                    IRInstruction::Immediate(
                                        (6, IRType::Number),
                                        IRConstant::Number(2.0)
                                    ),
                                    IRInstruction::Array(
                                        (7, IRType::Tensor),
                                        bump_list!(bump, (6, IRType::Number))
                                    ),
                                    IRInstruction::Immediate(
                                        (8, IRType::Number),
                                        IRConstant::Number(7.0)
                                    ),
                                    IRInstruction::Array(
                                        (9, IRType::Tensor),
                                        bump_list!(bump, (8, IRType::Number))
                                    ),
                                    IRInstruction::Call(
                                        (10, IRType::Tensor),
                                        (2, b"@f_xyz44"),
                                        bump_list!(bump, (7, IRType::Tensor), (9, IRType::Tensor))
                                    ),
                                    IRInstruction::Print((10, IRType::Tensor))
                                )
                            }
                        )
                    },
                    IRFunction {
                        name: b"@f_xyz33",
                        params: bump_list!(bump, (0, IRType::Number), (1, IRType::Number)),
                        ret_type: IRType::Number,
                        blocks: bump_list!(
                            bump,
                            IRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    IRInstruction::Binary(
                                        (2, IRType::Number),
                                        IRBinaryOp::AddNumbers,
                                        (0, IRType::Number),
                                        (1, IRType::Number)
                                    ),
                                    IRInstruction::Return((2, IRType::Number))
                                )
                            }
                        )
                    },
                    IRFunction {
                        name: b"@f_xyz44",
                        params: bump_list!(bump, (0, IRType::Tensor), (1, IRType::Tensor)),
                        ret_type: IRType::Tensor,
                        blocks: bump_list!(
                            bump,
                            IRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    IRInstruction::Binary(
                                        (2, IRType::Tensor),
                                        IRBinaryOp::AddTensors,
                                        (0, IRType::Tensor),
                                        (1, IRType::Tensor)
                                    ),
                                    IRInstruction::Return((2, IRType::Tensor))
                                )
                            }
                        )
                    }
                )
            }
        );
    }

    #[test]
    fn irgen_complex4() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f dim(mat) {    r (s (s mat))[0];}f len(list) {    v dim(list) == 1;    r (s list)[0];}f part_one(depths) {    v dim(depths) == 1;    a len = len(depths);    v len >= 1;    a j = 1;    a count = 0;    w (j < len) {        i (depths[j] > depths[j-1]) {            count = count + 1;        }        j = j + 1;    }    r count;}f part_two(depths) {    v dim(depths) == 1;    a len = len(depths);    v len >= 1;    a j = 0;    a new_depths = [0] sa [len];    a count = 0;    w (j < len - 2) {        new_depths[count] = depths[j] + depths[j+1] + depths[j+2];        count = count + 1;        j = j + 1;    }    r part_one(new_depths);}a d = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263];p part_one(d); p part_two(d);",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = semant::typecheck_program(ast, &bump).unwrap();
        let ir_program = irgen(typed_program, &bump);
        let correct_program = "fn @main() -> Nil {\n0:\n    im %0, 199\n    im %1, 200\n    im %2, 208\n    im %3, 210\n    im %4, 200\n    im %5, 207\n    im %6, 240\n    im %7, 269\n    im %8, 260\n    im %9, 263\n    ar %10, [%0, %1, %2, %3, %4, %5, %6, %7, %8, %9]\n    cp %11, %10\n    ca %12, @f_part_one4, (%11)\n    pr %12\n    ca %13, @f_part_two4, (%11)\n    pr %13\n}\n\nfn @f_part_one4(%0: Tensor) -> Number {\n0:\n    ca %1, @f_dim4, (%0)\n    im %2, 1\n    bi %3, EqualsEqualsNumbers, %1, %2\n    ve %3\n    ca %4, @f_len4, (%0)\n    cp %5, %4\n    im %6, 1\n    bi %7, GreaterEquals, %5, %6\n    ve %7\n    im %8, 1\n    cp %9, %8\n    im %10, 0\n    cp %11, %10\n    ju 1\n1:\n    bi %12, Lesser, %9, %5\n    br %12, 2, 3\n2:\n    in %13 <= %0, [%9]\n    im %14, 1\n    bi %15, SubtractNumbers, %9, %14\n    in %16 <= %0, [%15]\n    bi %17, Greater, %13, %16\n    br %17, 4, 5\n3:\n    re %11\n4:\n    im %18, 1\n    bi %19, AddNumbers, %11, %18\n    cp %11, %19\n    ju 5\n5:\n    im %20, 1\n    bi %21, AddNumbers, %9, %20\n    cp %9, %21\n    ju 1\n}\n\nfn @f_dim4(%0: Tensor) -> Number {\n0:\n    un %1, Shape, %0\n    un %2, Shape, %1\n    im %3, 0\n    in %4 <= %2, [%3]\n    re %4\n}\n\nfn @f_len4(%0: Tensor) -> Number {\n0:\n    ca %1, @f_dim4, (%0)\n    im %2, 1\n    bi %3, EqualsEqualsNumbers, %1, %2\n    ve %3\n    un %4, Shape, %0\n    im %5, 0\n    in %6 <= %4, [%5]\n    re %6\n}\n\nfn @f_part_two4(%0: Tensor) -> Number {\n0:\n    ca %1, @f_dim4, (%0)\n    im %2, 1\n    bi %3, EqualsEqualsNumbers, %1, %2\n    ve %3\n    ca %4, @f_len4, (%0)\n    cp %5, %4\n    im %6, 1\n    bi %7, GreaterEquals, %5, %6\n    ve %7\n    im %8, 0\n    cp %9, %8\n    im %10, 0\n    ar %11, [%10]\n    ar %12, [%5]\n    bi %13, ShapedAs, %11, %12\n    cp %14, %13\n    im %15, 0\n    cp %16, %15\n    ju 1\n1:\n    im %17, 2\n    bi %18, SubtractNumbers, %5, %17\n    bi %19, Lesser, %9, %18\n    br %19, 2, 3\n2:\n    in %20 <= %0, [%9]\n    im %21, 1\n    bi %22, AddNumbers, %9, %21\n    in %23 <= %0, [%22]\n    bi %24, AddNumbers, %20, %23\n    im %25, 2\n    bi %26, AddNumbers, %9, %25\n    in %27 <= %0, [%26]\n    bi %28, AddNumbers, %24, %27\n    in %14 >= %28, [%16]\n    im %29, 1\n    bi %30, AddNumbers, %16, %29\n    cp %16, %30\n    im %31, 1\n    bi %32, AddNumbers, %9, %31\n    cp %9, %32\n    ju 1\n3:\n    ca %33, @f_part_one4, (%14)\n    re %33\n}";
        assert_eq!(ir_program.to_string(), correct_program);
    }
}
