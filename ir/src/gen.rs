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

const TEMP_BUF_SIZE: usize = 64;

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
    bump: &'a bump::BumpAllocator,
}

impl<'a> IRGenContext<'a> {
    fn new(bump: &'a bump::BumpAllocator) -> Self {
        let block = IRBasicBlock {
            insts: bump.create_list(),
        };
        let func = IRFunction {
            name: b"@main",
            params: bump.create_list(),
            ret_type: IRType::Nil,
            blocks: bump.create_list(),
        };
        func.blocks.push(block);
        let context = IRGenContext {
            module: IRModule {
                funcs: bump.create_list(),
            },
            ast_types: &[],
            curr_func: 0,
            curr_block: 0,
            curr_vars: HashMap::new(),
            curr_num_regs: 0,
            func_defs: HashMap::new(),
            op_defs: HashMap::new(),
            called_funcs: HashMap::new(),
            bump,
        };
        context.module.funcs.push(func);
        context
    }

    fn get_curr_func(&self) -> &IRFunction<'a> {
        return self.module.funcs.at(self.curr_func as usize);
    }

    fn get_curr_func_mut(&mut self) -> &mut IRFunction<'a> {
        return self.module.funcs.at_mut(self.curr_func as usize);
    }

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
            name[i + 1] = func[i];
        }
        for i in 0..args.len() {
            name[i + func.len() + 1] = match args.at(i).1 {
                IRType::Nil => 0,
                IRType::Boolean => 1,
                IRType::String => 2,
                IRType::Number => 3,
                IRType::Tensor => 4,
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
            name[i + 1] = func[i];
        }
        name[func.len() + 1] = match left.1 {
            IRType::Nil => 0,
            IRType::Boolean => 1,
            IRType::String => 2,
            IRType::Number => 3,
            IRType::Tensor => 4,
        };
        name[func.len() + 2] = match right.1 {
            IRType::Nil => 0,
            IRType::Boolean => 1,
            IRType::String => 2,
            IRType::Number => 3,
            IRType::Tensor => 4,
        };
        name
    }

    fn irgen_program(&mut self, program: TypedProgram<'a>) {
        self.ast_types = program.1;
        for i in 0..program.0.len() {
            self.irgen_stmt(program.0.at(i));
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
                self.add_inst(IRInstruction::BranchUncond(post_block));
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
                self.add_inst(IRInstruction::BranchUncond(cond_block));
                self.curr_block = post_block;
            }
            TypedASTStmt::Print(expr) => {
                let reg = self.irgen_expr(expr);
                self.add_inst(IRInstruction::Print(reg));
            }
            TypedASTStmt::Return(expr) => {
                let reg = self.irgen_expr(expr);
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
            TypedASTExpr::Call(func, args, ty) => {
                let arg_regs = self.bump.create_list();
                for i in 0..args.len() {
                    arg_regs.push(self.irgen_expr(args.at(i)));
                }
                let result_reg = self.fresh_reg(convert_type(*ty));
                let mut cp = self.bump.create_checkpoint();
                let func_name = self.get_irfunc_name(func, arg_regs);
                let func_id = self
                    .called_funcs
                    .get(func_name)
                    .map(|x| *x)
                    .unwrap_or_else(|| {
                        let func_id = self.called_funcs.len() as u32;
                        self.called_funcs.insert(func_name, func_id);
                        cp.commit();
                        func_id
                    });
                core::mem::drop(cp);
                self.add_inst(IRInstruction::Call(result_reg, func_id, arg_regs));
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
            TypedASTExpr::Unary(op, expr, ty) => {
                let right_reg = self.irgen_expr(expr);
                let result_reg = self.fresh_reg(convert_type(*ty));
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
            TypedASTExpr::Binary(op, left, right, ty) => {
                let left_reg = self.irgen_expr(left);
                let right_reg = self.irgen_expr(right);
                let result_reg = self.fresh_reg(convert_type(*ty));
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
            TypedASTExpr::CustomBinary(func, left, right, ty) => {
                let left_reg = self.irgen_expr(left);
                let right_reg = self.irgen_expr(right);
                let arg_regs = bump_list!(self.bump, left_reg, right_reg);
                let result_reg = self.fresh_reg(convert_type(*ty));
                let mut cp = self.bump.create_checkpoint();
                let func_name = self.get_irop_name(func, left_reg, right_reg);
                let func_id = self
                    .called_funcs
                    .get(func_name)
                    .map(|x| *x)
                    .unwrap_or_else(|| {
                        let func_id = self.called_funcs.len() as u32;
                        self.called_funcs.insert(func_name, func_id);
                        cp.commit();
                        func_id
                    });
                core::mem::drop(cp);
                self.add_inst(IRInstruction::Call(result_reg, func_id, arg_regs));
                result_reg
            }
        }
    }
}

mod tests {
    use super::*;

    #[test]
    fn irgen1() {
        let bump = bump::BumpAllocator::new();
        let tokens = parse::lex(b"a x = 0; p x;", &bump).unwrap();
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
                                        IRConstant::Number(0.0)
                                    ),
                                    IRInstruction::Copy((1, IRType::Number), (0, IRType::Number)),
                                    IRInstruction::Print((1, IRType::Number))
                                )
                            }
                        )
                    }
                )
            }
        );
    }
}
