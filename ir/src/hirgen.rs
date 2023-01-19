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

pub fn hirgen<'a>(program: TypedProgram<'a>, bump: &'a bump::BumpAllocator) -> HIRModule<'a> {
    let mut context = HIRGenContext::new(bump);
    context.hirgen_program(program);
    context.module
}

fn convert_type(ty: Type) -> HIRType {
    match ty {
        Type::Nil => HIRType::Nil,
        Type::Number => HIRType::Number,
        Type::Tensor => HIRType::Tensor,
        Type::Boolean => HIRType::Boolean,
        Type::String => HIRType::String,
        _ => panic!("PANIC: Can't convert type variable to concrete HIR type."),
    }
}

struct HIRGenContext<'a> {
    module: HIRModule<'a>,
    ast_types: &'a [Type],
    curr_func: HIRFunctionID,
    curr_block: HIRBasicBlockID,
    curr_vars: HashMap<&'a [u8], HIRRegister>,
    curr_num_regs: HIRRegisterID,
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
    called_funcs: HashMap<&'a [u8], HIRFunctionID>,
    found_return: bool,
    bump: &'a bump::BumpAllocator,
}

impl<'a> HIRGenContext<'a> {
    fn new(bump: &'a bump::BumpAllocator) -> Self {
        let context = HIRGenContext {
            module: HIRModule {
                funcs: bump_list!(
                    bump,
                    HIRFunction {
                        name: b"@main",
                        params: bump.create_list(),
                        ret_type: HIRType::Nil,
                        blocks: bump_list!(
                            bump,
                            HIRBasicBlock {
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
    fn get_curr_func(&self) -> &HIRFunction<'a> {
        return self.module.funcs.at(self.curr_func as usize);
    }

    fn get_curr_func_mut(&mut self) -> &mut HIRFunction<'a> {
        return self.module.funcs.at_mut(self.curr_func as usize);
    }

    #[allow(dead_code)]
    fn get_curr_block(&self) -> &HIRBasicBlock<'a> {
        return self
            .module
            .funcs
            .at(self.curr_func as usize)
            .blocks
            .at(self.curr_block as usize);
    }

    fn get_curr_block_mut(&mut self) -> &mut HIRBasicBlock<'a> {
        return self
            .module
            .funcs
            .at_mut(self.curr_func as usize)
            .blocks
            .at_mut(self.curr_block as usize);
    }

    fn fresh_reg(&mut self, ty: HIRType) -> HIRRegister {
        let id = self.curr_num_regs;
        self.curr_num_regs += 1;
        (id, ty)
    }

    fn fresh_block(&mut self) -> HIRBasicBlockID {
        let block = HIRBasicBlock {
            insts: self.bump.create_list(),
        };
        let func = self.get_curr_func_mut();
        let id = func.blocks.len() as HIRBasicBlockID;
        func.blocks.push(block);
        id
    }

    fn fresh_func(
        &mut self,
        name: &'a [u8],
        params: &'a mut bump::List<'a, HIRRegister>,
        ret_type: HIRType,
    ) -> HIRFunctionID {
        let func_id = self.module.funcs.len() as HIRFunctionID;
        self.module.funcs.push(HIRFunction {
            name,
            params,
            ret_type,
            blocks: bump_list!(
                self.bump,
                HIRBasicBlock {
                    insts: self.bump.create_list()
                }
            ),
        });
        self.curr_func = func_id;
        self.curr_block = 0;
        func_id
    }

    fn add_inst(&mut self, inst: HIRInstruction<'a>) {
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

    fn get_hirfunc_name(&self, func: &'a [u8], args: &'a bump::List<'a, HIRRegister>) -> &'a [u8] {
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
                HIRType::Nil => b'0',
                HIRType::Boolean => b'1',
                HIRType::String => b'2',
                HIRType::Number => b'3',
                HIRType::Tensor => b'4',
            };
        }
        name
    }

    fn get_hirop_name(&self, func: &'a [u8], left: HIRRegister, right: HIRRegister) -> &'a [u8] {
        let size = 3 + func.len() + 2;
        let name = unsafe { self.bump.alloc_slice_raw(size) };
        name[0] = b'@';
        name[1] = b'o';
        name[2] = b'_';
        for i in 0..func.len() {
            name[i + 3] = func[i];
        }
        name[func.len() + 3] = match left.1 {
            HIRType::Nil => b'0',
            HIRType::Boolean => b'1',
            HIRType::String => b'2',
            HIRType::Number => b'3',
            HIRType::Tensor => b'4',
        };
        name[func.len() + 3] = match right.1 {
            HIRType::Nil => b'0',
            HIRType::Boolean => b'1',
            HIRType::String => b'2',
            HIRType::Number => b'3',
            HIRType::Tensor => b'4',
        };
        name
    }

    fn hirgen_program(&mut self, program: TypedProgram<'a>) {
        self.ast_types = program.1;
        for i in 0..program.0.len() {
            self.hirgen_stmt(program.0.at(i));
        }
        if !self.found_return {
            let reg = self.fresh_reg(HIRType::Nil);
            self.add_inst(HIRInstruction::Immediate(reg, HIRConstant::Nil));
            self.add_inst(HIRInstruction::Return(reg));
        }
    }

    fn hirgen_stmt(&mut self, stmt: &'a TypedASTStmt<'a>) {
        match stmt {
            TypedASTStmt::Block(stmts) => {
                for i in 0..stmts.len() {
                    self.hirgen_stmt(stmts.at(i));
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
                let cond_reg = self.hirgen_expr(cond);
                let body_block = self.fresh_block();
                let post_block = self.fresh_block();
                self.add_inst(HIRInstruction::BranchCond(cond_reg, body_block, post_block));
                self.curr_block = body_block;
                self.hirgen_stmt(body);
                if !self.get_curr_block().is_terminated() {
                    self.add_inst(HIRInstruction::BranchUncond(post_block));
                }
                self.curr_block = post_block;
            }
            TypedASTStmt::While(cond, body) => {
                let cond_block = self.fresh_block();
                let body_block = self.fresh_block();
                let post_block = self.fresh_block();
                self.add_inst(HIRInstruction::BranchUncond(cond_block));
                self.curr_block = cond_block;
                let cond_reg = self.hirgen_expr(cond);
                self.add_inst(HIRInstruction::BranchCond(cond_reg, body_block, post_block));
                self.curr_block = body_block;
                self.hirgen_stmt(body);
                if !self.get_curr_block().is_terminated() {
                    self.add_inst(HIRInstruction::BranchUncond(cond_block));
                }
                self.curr_block = post_block;
            }
            TypedASTStmt::Print(expr) => {
                let reg = self.hirgen_expr(expr);
                self.add_inst(HIRInstruction::Print(reg));
            }
            TypedASTStmt::Line(name) => {
                let reg = *self
                    .curr_vars
                    .get(name)
                    .expect("PANIC: Variable not in scope.");
                self.add_inst(HIRInstruction::Line(reg));
            }
            TypedASTStmt::Return(expr) => {
                let reg = self.hirgen_expr(expr);
                self.found_return = true;
                self.add_inst(HIRInstruction::Return(reg));
            }
            TypedASTStmt::Verify(expr) => {
                let reg = self.hirgen_expr(expr);
                self.add_inst(HIRInstruction::Verify(reg));
            }
            TypedASTStmt::Variable(name, expr) => {
                let reg = self.hirgen_expr(expr);
                let var_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                self.curr_vars.insert(name, var_reg);
                self.add_inst(HIRInstruction::Copy(var_reg, reg));
            }
            TypedASTStmt::Expression(expr) => {
                self.hirgen_expr(expr);
            }
        }
    }

    fn hirgen_expr(&mut self, expr: &'a TypedASTExpr<'a>) -> HIRRegister {
        match expr {
            TypedASTExpr::Nil => {
                let reg = self.fresh_reg(HIRType::Nil);
                self.add_inst(HIRInstruction::Immediate(reg, HIRConstant::Nil));
                reg
            }
            TypedASTExpr::Boolean(v) => {
                let reg = self.fresh_reg(HIRType::Boolean);
                self.add_inst(HIRInstruction::Immediate(reg, HIRConstant::Boolean(*v)));
                reg
            }
            TypedASTExpr::Number(v) => {
                let reg = self.fresh_reg(HIRType::Number);
                self.add_inst(HIRInstruction::Immediate(reg, HIRConstant::Number(*v)));
                reg
            }
            TypedASTExpr::String(v) => {
                let reg = self.fresh_reg(HIRType::String);
                self.add_inst(HIRInstruction::Immediate(reg, HIRConstant::String(*v)));
                reg
            }
            TypedASTExpr::Identifier(v, _) => *self
                .curr_vars
                .get(v)
                .expect("PANIC: Variable not in scope."),
            TypedASTExpr::Call(func, args, _) => {
                let arg_regs = self.bump.create_list();
                for i in 0..args.len() {
                    arg_regs.push(self.hirgen_expr(args.at(i)));
                }
                let result_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                let func_name = self.get_hirfunc_name(func, arg_regs);
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
                        self.curr_num_regs = arg_regs.len() as HIRRegisterID;

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
                        self.hirgen_stmt(body);
                        if !self.found_return {
                            let reg = self.fresh_reg(HIRType::Nil);
                            self.add_inst(HIRInstruction::Immediate(reg, HIRConstant::Nil));
                            self.add_inst(HIRInstruction::Return(reg));
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
                self.add_inst(HIRInstruction::Call(
                    result_reg,
                    (func_id, func_name),
                    arg_regs,
                ));
                result_reg
            }
            TypedASTExpr::Index(tensor, indices) => {
                let tensor_reg = self.hirgen_expr(tensor);
                let index_regs = self.bump.create_list();
                for i in 0..indices.len() {
                    index_regs.push(self.hirgen_expr(indices.at(i)));
                }
                let result_reg = self.fresh_reg(HIRType::Number);
                self.add_inst(HIRInstruction::Index(result_reg, tensor_reg, index_regs));
                result_reg
            }
            TypedASTExpr::ArrayLiteral(elements) => {
                let contents = self.bump.create_list();
                for i in 0..elements.len() {
                    contents.push(self.hirgen_expr(elements.at(i)));
                }
                let result_reg = self.fresh_reg(HIRType::Tensor);
                self.add_inst(HIRInstruction::Array(result_reg, contents));
                result_reg
            }
            TypedASTExpr::Assign(left, right, _) => {
                let right_reg = self.hirgen_expr(right);
                match left {
                    TypedASTExpr::Identifier(left_var, _) => {
                        let var_reg = *self.curr_vars.get(left_var).expect("PANIC: Variable not in scope.");
                        self.add_inst(HIRInstruction::Copy(var_reg, right_reg));
                        right_reg
                    }
                    TypedASTExpr::Index(TypedASTExpr::Identifier(left_var, _), indices) => {
                        let var_reg = *self.curr_vars.get(left_var).expect("PANIC: Variable not in scope.");
                        let index_regs = self.bump.create_list();
                        for i in 0..indices.len() {
                            index_regs.push(self.hirgen_expr(indices.at(i)));
                        }
                        self.add_inst(HIRInstruction::Index(var_reg, right_reg, index_regs));
                        right_reg
                    }
                    _ => panic!("PANIC: Something other than identifier or indexing an identifier on left-hand side of assign expression.")
                }
            }
            TypedASTExpr::Unary(op, expr, _) => {
                let right_reg = self.hirgen_expr(expr);
                let result_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                self.add_inst(HIRInstruction::Unary(
                    result_reg,
                    match op {
                        ASTUnaryOp::Not => HIRUnaryOp::Not,
                        ASTUnaryOp::Negate => HIRUnaryOp::Negate,
                        ASTUnaryOp::Shape => HIRUnaryOp::Shape,
                    },
                    right_reg,
                ));
                result_reg
            }
            TypedASTExpr::Binary(op, left, right, _) => {
                let left_reg = self.hirgen_expr(left);
                let right_reg = self.hirgen_expr(right);
                let result_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                let op = match (op, left_reg.1) {
                    (ASTBinaryOp::ShapedAs, _) => HIRBinaryOp::ShapedAs,
                    (ASTBinaryOp::Add, HIRType::Number) => HIRBinaryOp::AddNumbers,
                    (ASTBinaryOp::Add, HIRType::Tensor) => HIRBinaryOp::AddTensors,
                    (ASTBinaryOp::Subtract, HIRType::Number) => HIRBinaryOp::SubtractNumbers,
                    (ASTBinaryOp::Subtract, HIRType::Tensor) => HIRBinaryOp::SubtractTensors,
                    (ASTBinaryOp::Multiply, HIRType::Number) => HIRBinaryOp::MultiplyNumbers,
                    (ASTBinaryOp::Multiply, HIRType::Tensor) => HIRBinaryOp::MultiplyTensors,
                    (ASTBinaryOp::Divide, HIRType::Number) => HIRBinaryOp::DivideNumbers,
                    (ASTBinaryOp::Divide, HIRType::Tensor) => HIRBinaryOp::DivideTensors,
                    (ASTBinaryOp::Power, HIRType::Number) => HIRBinaryOp::PowerNumbers,
                    (ASTBinaryOp::Power, HIRType::Tensor) => HIRBinaryOp::PowerTensors,
                    (ASTBinaryOp::MatrixMultiply, _) => HIRBinaryOp::MatrixMultiply,
                    (ASTBinaryOp::Greater, _) => HIRBinaryOp::Greater,
                    (ASTBinaryOp::Lesser, _) => HIRBinaryOp::Lesser,
                    (ASTBinaryOp::NotEquals, HIRType::Nil) => HIRBinaryOp::NotEqualsNils,
                    (ASTBinaryOp::NotEquals, HIRType::Boolean) => HIRBinaryOp::NotEqualsBooleans,
                    (ASTBinaryOp::NotEquals, HIRType::String) => HIRBinaryOp::NotEqualsStrings,
                    (ASTBinaryOp::NotEquals, HIRType::Number) => HIRBinaryOp::NotEqualsNumbers,
                    (ASTBinaryOp::NotEquals, HIRType::Tensor) => HIRBinaryOp::NotEqualsTensors,
                    (ASTBinaryOp::EqualsEquals, HIRType::Nil) => HIRBinaryOp::EqualsEqualsNils,
                    (ASTBinaryOp::EqualsEquals, HIRType::Boolean) => {
                        HIRBinaryOp::EqualsEqualsBooleans
                    }
                    (ASTBinaryOp::EqualsEquals, HIRType::String) => {
                        HIRBinaryOp::EqualsEqualsStrings
                    }
                    (ASTBinaryOp::EqualsEquals, HIRType::Number) => {
                        HIRBinaryOp::EqualsEqualsNumbers
                    }
                    (ASTBinaryOp::EqualsEquals, HIRType::Tensor) => {
                        HIRBinaryOp::EqualsEqualsTensors
                    }
                    (ASTBinaryOp::GreaterEquals, _) => HIRBinaryOp::GreaterEquals,
                    (ASTBinaryOp::LesserEquals, _) => HIRBinaryOp::LesserEquals,
                    (ASTBinaryOp::And, _) => HIRBinaryOp::And,
                    (ASTBinaryOp::Or, _) => HIRBinaryOp::Or,
                    _ => panic!("PANIC: Unsupported binary operation configuration."),
                };
                self.add_inst(HIRInstruction::Binary(result_reg, op, left_reg, right_reg));
                result_reg
            }
            TypedASTExpr::CustomBinary(func, left, right, _) => {
                let left_reg = self.hirgen_expr(left);
                let right_reg = self.hirgen_expr(right);
                let arg_regs = bump_list!(self.bump, left_reg, right_reg);
                let result_reg = self.fresh_reg(convert_type(self.get_type(expr)));
                let func_name = self.get_hirop_name(func, left_reg, right_reg);
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
                        self.curr_num_regs = arg_regs.len() as HIRRegisterID;

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
                        self.hirgen_stmt(body);
                        if !self.found_return {
                            let reg = self.fresh_reg(HIRType::Nil);
                            self.add_inst(HIRInstruction::Immediate(reg, HIRConstant::Nil));
                            self.add_inst(HIRInstruction::Return(reg));
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
                self.add_inst(HIRInstruction::Call(
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
    fn hirgen_simple() {
        let bump = bump::BumpAllocator::new();
        let pahirs = &mut [
            (
                b"a x = 0; p x;" as &[_],
                bump_list!(
                    bump,
                    HIRInstruction::Immediate((0, HIRType::Number), HIRConstant::Number(0.0)),
                    HIRInstruction::Copy((1, HIRType::Number), (0, HIRType::Number)),
                    HIRInstruction::Print((1, HIRType::Number)),
                    HIRInstruction::Immediate((2, HIRType::Nil), HIRConstant::Nil),
                    HIRInstruction::Return((2, HIRType::Nil))
                ),
            ),
            (
                b"p 3 + 5;",
                bump_list!(
                    bump,
                    HIRInstruction::Immediate((0, HIRType::Number), HIRConstant::Number(3.0)),
                    HIRInstruction::Immediate((1, HIRType::Number), HIRConstant::Number(5.0)),
                    HIRInstruction::Binary(
                        (2, HIRType::Number),
                        HIRBinaryOp::AddNumbers,
                        (0, HIRType::Number),
                        (1, HIRType::Number)
                    ),
                    HIRInstruction::Print((2, HIRType::Number)),
                    HIRInstruction::Immediate((3, HIRType::Nil), HIRConstant::Nil),
                    HIRInstruction::Return((3, HIRType::Nil))
                ),
            ),
            (
                b"a x = 5; p 3 + x; a y = x ^ 2; a z = [y];",
                bump_list!(
                    bump,
                    HIRInstruction::Immediate((0, HIRType::Number), HIRConstant::Number(5.0)),
                    HIRInstruction::Copy((1, HIRType::Number), (0, HIRType::Number)),
                    HIRInstruction::Immediate((2, HIRType::Number), HIRConstant::Number(3.0)),
                    HIRInstruction::Binary(
                        (3, HIRType::Number),
                        HIRBinaryOp::AddNumbers,
                        (2, HIRType::Number),
                        (1, HIRType::Number)
                    ),
                    HIRInstruction::Print((3, HIRType::Number)),
                    HIRInstruction::Immediate((4, HIRType::Number), HIRConstant::Number(2.0)),
                    HIRInstruction::Binary(
                        (5, HIRType::Number),
                        HIRBinaryOp::PowerNumbers,
                        (1, HIRType::Number),
                        (4, HIRType::Number)
                    ),
                    HIRInstruction::Copy((6, HIRType::Number), (5, HIRType::Number)),
                    HIRInstruction::Array(
                        (7, HIRType::Tensor),
                        bump_list!(bump, (6, HIRType::Number))
                    ),
                    HIRInstruction::Copy((8, HIRType::Tensor), (7, HIRType::Tensor)),
                    HIRInstruction::Immediate((9, HIRType::Nil), HIRConstant::Nil),
                    HIRInstruction::Return((9, HIRType::Nil))
                ),
            ),
        ];
        for (input, output) in pahirs.iter_mut() {
            let tokens = parse::lex(input, &bump).unwrap();
            let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
            let typed_program = semant::typecheck_program(ast, &bump).unwrap();
            let hir_program = hirgen(typed_program, &bump);
            assert_eq!(
                hir_program,
                HIRModule {
                    funcs: bump_list!(
                        bump,
                        HIRFunction {
                            name: b"@main",
                            params: bump.create_list(),
                            ret_type: HIRType::Nil,
                            blocks: bump_list!(bump, HIRBasicBlock { insts: *output })
                        }
                    )
                }
            );
        }
    }

    #[test]
    fn hirgen_complex1() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"i (T) { p N; } w (T) { p N; }", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = semant::typecheck_program(ast, &bump).unwrap();
        let hir_program = hirgen(typed_program, &bump);
        assert_eq!(
            hir_program,
            HIRModule {
                funcs: bump_list!(
                    bump,
                    HIRFunction {
                        name: b"@main",
                        params: bump.create_list(),
                        ret_type: HIRType::Nil,
                        blocks: bump_list!(
                            bump,
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Immediate(
                                        (0, HIRType::Boolean),
                                        HIRConstant::Boolean(true)
                                    ),
                                    HIRInstruction::BranchCond((0, HIRType::Boolean), 1, 2)
                                )
                            },
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Immediate((1, HIRType::Nil), HIRConstant::Nil),
                                    HIRInstruction::Print((1, HIRType::Nil)),
                                    HIRInstruction::BranchUncond(2)
                                )
                            },
                            HIRBasicBlock {
                                insts: bump_list!(bump, HIRInstruction::BranchUncond(3))
                            },
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Immediate(
                                        (2, HIRType::Boolean),
                                        HIRConstant::Boolean(true)
                                    ),
                                    HIRInstruction::BranchCond((2, HIRType::Boolean), 4, 5)
                                )
                            },
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Immediate((3, HIRType::Nil), HIRConstant::Nil),
                                    HIRInstruction::Print((3, HIRType::Nil)),
                                    HIRInstruction::BranchUncond(3)
                                )
                            },
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Immediate((4, HIRType::Nil), HIRConstant::Nil),
                                    HIRInstruction::Return((4, HIRType::Nil))
                                )
                            }
                        )
                    }
                )
            }
        );
    }

    #[test]
    fn hirgen_complex2() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"f xyz() { p N; r 5; } p xyz();", &bump).unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = semant::typecheck_program(ast, &bump).unwrap();
        let hir_program = hirgen(typed_program, &bump);
        assert_eq!(
            hir_program,
            HIRModule {
                funcs: bump_list!(
                    bump,
                    HIRFunction {
                        name: b"@main",
                        params: bump.create_list(),
                        ret_type: HIRType::Nil,
                        blocks: bump_list!(
                            bump,
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Call(
                                        (0, HIRType::Number),
                                        (1, b"@f_xyz"),
                                        bump_list!(bump,)
                                    ),
                                    HIRInstruction::Print((0, HIRType::Number)),
                                    HIRInstruction::Immediate((1, HIRType::Nil), HIRConstant::Nil),
                                    HIRInstruction::Return((1, HIRType::Nil))
                                )
                            }
                        )
                    },
                    HIRFunction {
                        name: b"@f_xyz",
                        params: bump.create_list(),
                        ret_type: HIRType::Number,
                        blocks: bump_list!(
                            bump,
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Immediate((0, HIRType::Nil), HIRConstant::Nil),
                                    HIRInstruction::Print((0, HIRType::Nil)),
                                    HIRInstruction::Immediate(
                                        (1, HIRType::Number),
                                        HIRConstant::Number(5.0)
                                    ),
                                    HIRInstruction::Return((1, HIRType::Number))
                                )
                            }
                        )
                    }
                )
            }
        );
    }

    #[test]
    fn hirgen_complex3() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f xyz(x, y) { r x + y; } p xyz(5, 4); p xyz(1, 2); p xyz([2], [7]);",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = semant::typecheck_program(ast, &bump).unwrap();
        let hir_program = hirgen(typed_program, &bump);
        assert_eq!(
            hir_program,
            HIRModule {
                funcs: bump_list!(
                    bump,
                    HIRFunction {
                        name: b"@main",
                        params: bump.create_list(),
                        ret_type: HIRType::Nil,
                        blocks: bump_list!(
                            bump,
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Immediate(
                                        (0, HIRType::Number),
                                        HIRConstant::Number(5.0)
                                    ),
                                    HIRInstruction::Immediate(
                                        (1, HIRType::Number),
                                        HIRConstant::Number(4.0)
                                    ),
                                    HIRInstruction::Call(
                                        (2, HIRType::Number),
                                        (1, b"@f_xyz33"),
                                        bump_list!(
                                            bump,
                                            (0, HIRType::Number),
                                            (1, HIRType::Number)
                                        )
                                    ),
                                    HIRInstruction::Print((2, HIRType::Number)),
                                    HIRInstruction::Immediate(
                                        (3, HIRType::Number),
                                        HIRConstant::Number(1.0)
                                    ),
                                    HIRInstruction::Immediate(
                                        (4, HIRType::Number),
                                        HIRConstant::Number(2.0)
                                    ),
                                    HIRInstruction::Call(
                                        (5, HIRType::Number),
                                        (1, b"@f_xyz33"),
                                        bump_list!(
                                            bump,
                                            (3, HIRType::Number),
                                            (4, HIRType::Number)
                                        )
                                    ),
                                    HIRInstruction::Print((5, HIRType::Number)),
                                    HIRInstruction::Immediate(
                                        (6, HIRType::Number),
                                        HIRConstant::Number(2.0)
                                    ),
                                    HIRInstruction::Array(
                                        (7, HIRType::Tensor),
                                        bump_list!(bump, (6, HIRType::Number))
                                    ),
                                    HIRInstruction::Immediate(
                                        (8, HIRType::Number),
                                        HIRConstant::Number(7.0)
                                    ),
                                    HIRInstruction::Array(
                                        (9, HIRType::Tensor),
                                        bump_list!(bump, (8, HIRType::Number))
                                    ),
                                    HIRInstruction::Call(
                                        (10, HIRType::Tensor),
                                        (2, b"@f_xyz44"),
                                        bump_list!(
                                            bump,
                                            (7, HIRType::Tensor),
                                            (9, HIRType::Tensor)
                                        )
                                    ),
                                    HIRInstruction::Print((10, HIRType::Tensor)),
                                    HIRInstruction::Immediate((11, HIRType::Nil), HIRConstant::Nil),
                                    HIRInstruction::Return((11, HIRType::Nil))
                                )
                            }
                        )
                    },
                    HIRFunction {
                        name: b"@f_xyz33",
                        params: bump_list!(bump, (0, HIRType::Number), (1, HIRType::Number)),
                        ret_type: HIRType::Number,
                        blocks: bump_list!(
                            bump,
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Binary(
                                        (2, HIRType::Number),
                                        HIRBinaryOp::AddNumbers,
                                        (0, HIRType::Number),
                                        (1, HIRType::Number)
                                    ),
                                    HIRInstruction::Return((2, HIRType::Number))
                                )
                            }
                        )
                    },
                    HIRFunction {
                        name: b"@f_xyz44",
                        params: bump_list!(bump, (0, HIRType::Tensor), (1, HIRType::Tensor)),
                        ret_type: HIRType::Tensor,
                        blocks: bump_list!(
                            bump,
                            HIRBasicBlock {
                                insts: bump_list!(
                                    bump,
                                    HIRInstruction::Binary(
                                        (2, HIRType::Tensor),
                                        HIRBinaryOp::AddTensors,
                                        (0, HIRType::Tensor),
                                        (1, HIRType::Tensor)
                                    ),
                                    HIRInstruction::Return((2, HIRType::Tensor))
                                )
                            }
                        )
                    }
                )
            }
        );
    }

    #[test]
    fn hirgen_complex4() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(
            b"f dim(mat) {    r (s (s mat))[0];}f len(list) {    v dim(list) == 1;    r (s list)[0];}f part_one(depths) {    v dim(depths) == 1;    a len = len(depths);    v len >= 1;    a j = 1;    a count = 0;    w (j < len) {        i (depths[j] > depths[j-1]) {            count = count + 1;        }        j = j + 1;    }    r count;}f part_two(depths) {    v dim(depths) == 1;    a len = len(depths);    v len >= 1;    a j = 0;    a new_depths = [0] sa [len];    a count = 0;    w (j < len - 2) {        new_depths[count] = depths[j] + depths[j+1] + depths[j+2];        count = count + 1;        j = j + 1;    }    r part_one(new_depths);}a d = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263];p part_one(d); p part_two(d);",
            &bump,
        )
        .unwrap();
        let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
        let typed_program = semant::typecheck_program(ast, &bump).unwrap();
        let hir_program = hirgen(typed_program, &bump);
        let correct_program = "fn @main() -> Nil {\n0:\n    im %0, 199\n    im %1, 200\n    im %2, 208\n    im %3, 210\n    im %4, 200\n    im %5, 207\n    im %6, 240\n    im %7, 269\n    im %8, 260\n    im %9, 263\n    ar %10, [%0, %1, %2, %3, %4, %5, %6, %7, %8, %9]\n    cp %11, %10\n    ca %12, @f_part_one4, (%11)\n    pr %12\n    ca %13, @f_part_two4, (%11)\n    pr %13\n    im %14, Nil\n    re %14\n}\n\nfn @f_part_one4(%0: Tensor) -> Number {\n0:\n    ca %1, @f_dim4, (%0)\n    im %2, 1\n    bi %3, EqualsEqualsNumbers, %1, %2\n    ve %3\n    ca %4, @f_len4, (%0)\n    cp %5, %4\n    im %6, 1\n    bi %7, GreaterEquals, %5, %6\n    ve %7\n    im %8, 1\n    cp %9, %8\n    im %10, 0\n    cp %11, %10\n    ju 1\n1:\n    bi %12, Lesser, %9, %5\n    br %12, 2, 3\n2:\n    in %13 <= %0, [%9]\n    im %14, 1\n    bi %15, SubtractNumbers, %9, %14\n    in %16 <= %0, [%15]\n    bi %17, Greater, %13, %16\n    br %17, 4, 5\n3:\n    re %11\n4:\n    im %18, 1\n    bi %19, AddNumbers, %11, %18\n    cp %11, %19\n    ju 5\n5:\n    im %20, 1\n    bi %21, AddNumbers, %9, %20\n    cp %9, %21\n    ju 1\n}\n\nfn @f_dim4(%0: Tensor) -> Number {\n0:\n    un %1, Shape, %0\n    un %2, Shape, %1\n    im %3, 0\n    in %4 <= %2, [%3]\n    re %4\n}\n\nfn @f_len4(%0: Tensor) -> Number {\n0:\n    ca %1, @f_dim4, (%0)\n    im %2, 1\n    bi %3, EqualsEqualsNumbers, %1, %2\n    ve %3\n    un %4, Shape, %0\n    im %5, 0\n    in %6 <= %4, [%5]\n    re %6\n}\n\nfn @f_part_two4(%0: Tensor) -> Number {\n0:\n    ca %1, @f_dim4, (%0)\n    im %2, 1\n    bi %3, EqualsEqualsNumbers, %1, %2\n    ve %3\n    ca %4, @f_len4, (%0)\n    cp %5, %4\n    im %6, 1\n    bi %7, GreaterEquals, %5, %6\n    ve %7\n    im %8, 0\n    cp %9, %8\n    im %10, 0\n    ar %11, [%10]\n    ar %12, [%5]\n    bi %13, ShapedAs, %11, %12\n    cp %14, %13\n    im %15, 0\n    cp %16, %15\n    ju 1\n1:\n    im %17, 2\n    bi %18, SubtractNumbers, %5, %17\n    bi %19, Lesser, %9, %18\n    br %19, 2, 3\n2:\n    in %20 <= %0, [%9]\n    im %21, 1\n    bi %22, AddNumbers, %9, %21\n    in %23 <= %0, [%22]\n    bi %24, AddNumbers, %20, %23\n    im %25, 2\n    bi %26, AddNumbers, %9, %25\n    in %27 <= %0, [%26]\n    bi %28, AddNumbers, %24, %27\n    in %14 >= %28, [%16]\n    im %29, 1\n    bi %30, AddNumbers, %16, %29\n    cp %16, %30\n    im %31, 1\n    bi %32, AddNumbers, %9, %31\n    cp %9, %32\n    ju 1\n3:\n    ca %33, @f_part_one4, (%14)\n    re %33\n}";
        assert_eq!(hir_program.to_string(), correct_program);
    }
}
