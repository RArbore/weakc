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

use crate::*;
use bump::bump_list;

struct MIRGenContext<'a> {
    module: MIRModule<'a>,
    curr_func: MIRFunctionID,
    curr_block: MIRBasicBlockID,
    bump: &'a bump::BumpAllocator,
}

pub fn mirgen<'a>(program: HIRModule<'a>, bump: &'a bump::BumpAllocator) -> MIRModule<'a> {
    let mut context = MIRGenContext::new(bump);
    context.mirgen_program(program);
    context.module
}

fn convert_type(ty: HIRType) -> Option<MIRType> {
    match ty {
        HIRType::Nil => None,
        HIRType::Boolean => Some(MIRType::Boolean),
        HIRType::String => Some(MIRType::String),
        HIRType::Number => Some(MIRType::Real),
        HIRType::Tensor => Some(MIRType::Pointer),
    }
}

fn convert_register(reg: HIRRegister) -> Option<MIRRegister> {
    let (id, ty) = reg;
    if let Some(ty) = convert_type(ty) {
        Some((id, ty))
    } else {
        None
    }
}

fn convert_2_registers(regs: (HIRRegister, HIRRegister)) -> Option<(MIRRegister, MIRRegister)> {
    let reg1 = convert_register(regs.0);
    let reg2 = convert_register(regs.1);
    if let (Some(reg1), Some(reg2)) = (reg1, reg2) {
        Some((reg1, reg2))
    } else if let (None, None) = (reg1, reg2) {
        None
    } else {
        panic!("PANIC: One register in operation is Nil typed, and the other isn't.");
    }
}

fn convert_3_registers(
    regs: (HIRRegister, HIRRegister, HIRRegister),
) -> Option<(MIRRegister, MIRRegister, MIRRegister)> {
    let reg1 = convert_register(regs.0);
    let reg2 = convert_register(regs.1);
    let reg3 = convert_register(regs.2);
    if let (Some(reg1), Some(reg2), Some(reg3)) = (reg1, reg2, reg3) {
        Some((reg1, reg2, reg3))
    } else if let (None, None, None) = (reg1, reg2, reg3) {
        None
    } else {
        panic!("PANIC: Some register in operation is Nil typed, and some other isn't.");
    }
}

impl<'a> MIRGenContext<'a> {
    fn new(bump: &'a bump::BumpAllocator) -> Self {
        let context = MIRGenContext {
            module: MIRModule {
                funcs: bump.create_list(),
                strings: bump.create_list(),
            },
            curr_func: 0,
            curr_block: 0,
            bump,
        };
        context
    }

    fn convert_string(&mut self, string: &'a [u8]) -> u32 {
        let mut i = 0;
        while i < self.module.strings.len() as u32 {
            if *self.module.strings.at(i as usize) == string {
                return i;
            }
            i += 1;
        }
        self.module.strings.push(string);
        return i;
    }

    fn get_curr_func_mut(&mut self) -> &mut MIRFunction<'a> {
        return self.module.funcs.at_mut(self.curr_func as usize);
    }

    fn get_curr_block_mut(&mut self) -> &mut MIRBasicBlock<'a> {
        return self
            .module
            .funcs
            .at_mut(self.curr_func as usize)
            .blocks
            .at_mut(self.curr_block as usize);
    }

    fn fresh_reg(&mut self, ty: MIRType) -> MIRRegister {
        let id = self.get_curr_func_mut().num_regs_used;
        self.get_curr_func_mut().num_regs_used += 1;
        (id, ty)
    }

    fn fresh_block(&mut self) -> MIRBasicBlockID {
        let block = MIRBasicBlock {
            insts: self.bump.create_list(),
        };
        let func = self.get_curr_func_mut();
        let id = func.blocks.len() as MIRBasicBlockID;
        func.blocks.push(block);
        id
    }

    fn mirgen_program(&mut self, program: HIRModule<'a>) {
        for i in 0..program.funcs.len() {
            self.mirgen_func(program.funcs.at(i));
        }
    }

    fn mirgen_func(&mut self, func: &'a HIRFunction<'a>) {
        let mir_func = MIRFunction {
            name: func.name,
            params: self.bump.create_list(),
            ret_type: convert_type(func.ret_type),
            blocks: self.bump.create_list(),
            num_regs_used: func.num_regs_used,
        };

        for i in 0..func.params.len() {
            if let Some(reg) = convert_register(*func.params.at(i)) {
                mir_func.params.push(reg);
            }
        }

        self.curr_func = self.module.funcs.len() as MIRFunctionID;
        self.curr_block = 0;
        self.module.funcs.push(mir_func);

        for _ in 0..func.blocks.len() {
            let empty_mir_block = MIRBasicBlock {
                insts: self.bump.create_list(),
            };
            self.get_curr_func_mut().blocks.push(empty_mir_block);
        }

        for i in 0..func.blocks.len() {
            self.mirgen_block(func.blocks.at(i), i as MIRBasicBlockID);
        }
    }

    fn add_inst(&mut self, inst: MIRInstruction<'a>) {
        self.get_curr_block_mut().insts.push(inst);
    }

    fn mirgen_block(&mut self, block: &'a HIRBasicBlock<'a>, id: MIRBasicBlockID) {
        self.curr_block = id;

        for i in 0..block.insts.len() {
            match block.insts.at(i) {
                HIRInstruction::Immediate(hir_reg, hir_const) => {
                    if let Some(mir_reg) = convert_register(*hir_reg) {
                        let mir_const = match hir_const {
                            HIRConstant::Boolean(v) => MIRConstant::Boolean(*v),
                            HIRConstant::Number(v) => MIRConstant::Real(*v),
                            HIRConstant::String(v) => MIRConstant::String(self.convert_string(*v)),
                            _ => panic!("PANIC: Unhandled HIR constant in immediate instruction."),
                        };
                        self.add_inst(MIRInstruction::Immediate(mir_reg, mir_const));
                    }
                }
                HIRInstruction::Copy(left_reg, right_reg) => {
                    if let Some((left_mir_reg, right_mir_reg)) =
                        convert_2_registers((*left_reg, *right_reg))
                    {
                        self.add_inst(MIRInstruction::Copy(left_mir_reg, right_mir_reg));
                    }
                }
                HIRInstruction::Unary(left_reg, op, right_reg) => {
                    let op = match op {
                        HIRUnaryOp::Not => MIRUnaryOp::Not,
                        HIRUnaryOp::Negate => MIRUnaryOp::Negate,
                        HIRUnaryOp::Shape => MIRUnaryOp::Shape,
                    };
                    if let Some((left_mir_reg, right_mir_reg)) =
                        convert_2_registers((*left_reg, *right_reg))
                    {
                        self.add_inst(MIRInstruction::Unary(left_mir_reg, op, right_mir_reg));
                    }
                }
                HIRInstruction::Binary(result_reg, op, left_reg, right_reg) => {
                    if let HIRBinaryOp::NotEqualsNils = op {
                        self.add_inst(MIRInstruction::Immediate(
                            convert_register(*result_reg).unwrap(),
                            MIRConstant::Boolean(false),
                        ));
                    } else if let HIRBinaryOp::EqualsEqualsNils = op {
                        self.add_inst(MIRInstruction::Immediate(
                            convert_register(*result_reg).unwrap(),
                            MIRConstant::Boolean(true),
                        ));
                    } else if let Some((result_mir_reg, left_mir_reg, right_mir_reg)) =
                        convert_3_registers((*result_reg, *left_reg, *right_reg))
                    {
                        match op {
                            HIRBinaryOp::ShapedAs => {
                                self.mirgen_shaped_as(result_mir_reg, left_mir_reg, right_mir_reg);
                            }
                            HIRBinaryOp::MatrixMultiply => {}
                            HIRBinaryOp::AddTensors => {}
                            HIRBinaryOp::SubtractTensors => {}
                            HIRBinaryOp::MultiplyTensors => {}
                            HIRBinaryOp::DivideTensors => {}
                            HIRBinaryOp::PowerTensors => {}
                            HIRBinaryOp::NotEqualsTensors => {}
                            HIRBinaryOp::EqualsEqualsTensors => {}
                            _ => {
                                let mir_op = match op {
                                    HIRBinaryOp::AddNumbers => MIRBinaryOp::AddReals,
                                    HIRBinaryOp::SubtractNumbers => MIRBinaryOp::SubtractReals,
                                    HIRBinaryOp::MultiplyNumbers => MIRBinaryOp::MultiplyReals,
                                    HIRBinaryOp::DivideNumbers => MIRBinaryOp::DivideReals,
                                    HIRBinaryOp::PowerNumbers => MIRBinaryOp::PowerReals,
                                    HIRBinaryOp::Greater => MIRBinaryOp::GreaterReals,
                                    HIRBinaryOp::Lesser => MIRBinaryOp::LesserReals,
                                    HIRBinaryOp::NotEqualsBooleans => {
                                        MIRBinaryOp::NotEqualsBooleans
                                    }
                                    HIRBinaryOp::EqualsEqualsBooleans => {
                                        MIRBinaryOp::EqualsEqualsBooleans
                                    }
                                    HIRBinaryOp::NotEqualsStrings => MIRBinaryOp::NotEqualsStrings,
                                    HIRBinaryOp::EqualsEqualsStrings => {
                                        MIRBinaryOp::EqualsEqualsStrings
                                    }
                                    HIRBinaryOp::NotEqualsNumbers => MIRBinaryOp::NotEqualsReals,
                                    HIRBinaryOp::EqualsEqualsNumbers => {
                                        MIRBinaryOp::EqualsEqualsReals
                                    }
                                    HIRBinaryOp::GreaterEquals => MIRBinaryOp::GreaterEqualsReals,
                                    HIRBinaryOp::LesserEquals => MIRBinaryOp::LesserEqualsReals,
                                    HIRBinaryOp::And => MIRBinaryOp::And,
                                    HIRBinaryOp::Or => MIRBinaryOp::Or,
                                    _ => panic!("PANIC: Unimplemented simple HIR->MIR binary op."),
                                };
                                self.add_inst(MIRInstruction::Binary(
                                    result_mir_reg,
                                    mir_op,
                                    left_mir_reg,
                                    right_mir_reg,
                                ));
                            }
                        }
                    }
                }
                _ => todo!(),
            }
        }
    }

    fn mirgen_allocate_empty_tensor(&mut self, result: MIRRegister) {
        let tensor_size_const = self.fresh_reg(MIRType::Fixed);
        self.add_inst(MIRInstruction::Immediate(
            tensor_size_const,
            MIRConstant::Fixed(MIR_TENSOR_SIZE),
        ));
        self.add_inst(MIRInstruction::Call(
            Some(result),
            MIR_EXTERNAL_FUNCTION_MALLOC.0,
            bump_list!(self.bump, tensor_size_const),
        ));
    }

    fn mirgen_shaped_as(&mut self, result: MIRRegister, tensor: MIRRegister, shape: MIRRegister) {
        self.mirgen_allocate_empty_tensor(result);
        let shape_num_dims = self.fresh_reg(MIRType::Fixed);
        self.add_inst(MIRInstruction::Load(shape_num_dims, shape));
        let one_register = self.fresh_reg(MIRType::Fixed);
        self.add_inst(MIRInstruction::Immediate(
            one_register,
            MIRConstant::Fixed(1),
        ));
        let assert_reg = self.fresh_reg(MIRType::Boolean);
        self.add_inst(MIRInstruction::Binary(
            assert_reg,
            MIRBinaryOp::EqualsEqualsFixed,
            shape_num_dims,
            one_register,
        ));
        self.add_inst(MIRInstruction::Call(
            None,
            MIR_EXTERNAL_FUNCTION_ASSERT.0,
            bump_list!(self.bump, assert_reg),
        ));

        let new_dimensionality_ptr = self.fresh_reg(MIRType::Pointer);
        let new_dimensionality = self.fresh_reg(MIRType::Fixed);
        let one_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            one_register,
            MIRConstant::Size(1),
        ));
        self.add_inst(MIRInstruction::Gep(
            new_dimensionality_ptr,
            shape,
            one_register,
            MIRType::Pointer,
        ));
        self.add_inst(MIRInstruction::Load(
            new_dimensionality,
            new_dimensionality_ptr,
        ));

        let new_dimensionality_size = self.fresh_reg(MIRType::Size);
        let shape_elem_size = self.fresh_reg(MIRType::Size);
        let new_shape_malloc_size = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Unary(
            new_dimensionality_size,
            MIRUnaryOp::Widen,
            new_dimensionality,
        ));
        self.add_inst(MIRInstruction::Immediate(
            shape_elem_size,
            MIRConstant::Size(MIRType::Fixed.get_size()),
        ));
        self.add_inst(MIRInstruction::Binary(
            new_shape_malloc_size,
            MIRBinaryOp::MultiplySizes,
            new_dimensionality_size,
            shape_elem_size,
        ));
        let result_shape_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Call(
            Some(result_shape_ptr),
            MIR_EXTERNAL_FUNCTION_MALLOC.0,
            bump_list!(self.bump, new_shape_malloc_size),
        ));

        let one_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            one_register,
            MIRConstant::Size(1),
        ));
        let result_shape_ptr_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Gep(
            result_shape_ptr_ptr,
            result,
            one_register,
            MIRType::Pointer,
        ));
        self.add_inst(MIRInstruction::Store(
            result_shape_ptr,
            result_shape_ptr_ptr,
        ));

        let two_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            two_register,
            MIRConstant::Size(2),
        ));
        let shape_elements_ptr_ptr = self.fresh_reg(MIRType::Pointer);
        let shape_elements_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Gep(
            shape_elements_ptr_ptr,
            shape,
            two_register,
            MIRType::Pointer,
        ));
        self.add_inst(MIRInstruction::Load(
            shape_elements_ptr,
            shape_elements_ptr_ptr,
        ));

        let copy_index = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(copy_index, MIRConstant::Size(0)));
        let result_num_elements = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            result_num_elements,
            MIRConstant::Size(1),
        ));
        let cond_block = self.fresh_block();
        let body_block = self.fresh_block();
        let post_block = self.fresh_block();
        self.add_inst(MIRInstruction::BranchUncond(cond_block));

        self.curr_block = cond_block;
        let cond_reg = self.fresh_reg(MIRType::Boolean);
        self.add_inst(MIRInstruction::Binary(
            cond_reg,
            MIRBinaryOp::LesserSizes,
            copy_index,
            new_dimensionality_size,
        ));
        self.add_inst(MIRInstruction::BranchCond(cond_reg, body_block, post_block));

        self.curr_block = body_block;
        let shape_element_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Gep(
            shape_element_ptr,
            shape_elements_ptr,
            copy_index,
            MIRType::Real,
        ));
        let shape_element = self.fresh_reg(MIRType::Real);
        self.add_inst(MIRInstruction::Load(shape_element, shape_element_ptr));
        let rounded_shape_element = self.fresh_reg(MIRType::Fixed);
        self.add_inst(MIRInstruction::Unary(
            rounded_shape_element,
            MIRUnaryOp::Round,
            shape_element,
        ));
        let widened_shape_element = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Unary(
            widened_shape_element,
            MIRUnaryOp::Widen,
            rounded_shape_element,
        ));
        self.add_inst(MIRInstruction::Binary(
            result_num_elements,
            MIRBinaryOp::MultiplySizes,
            result_num_elements,
            widened_shape_element,
        ));

        let result_shape_element_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Gep(
            result_shape_element_ptr,
            result_shape_ptr,
            copy_index,
            MIRType::Fixed,
        ));
        self.add_inst(MIRInstruction::Store(
            rounded_shape_element,
            result_shape_element_ptr,
        ));

        let one_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            one_register,
            MIRConstant::Size(1),
        ));
        self.add_inst(MIRInstruction::Binary(
            copy_index,
            MIRBinaryOp::AddSizes,
            copy_index,
            one_register,
        ));
        self.add_inst(MIRInstruction::BranchUncond(cond_block));

        self.curr_block = post_block;

        let tensor_elem_size = self.fresh_reg(MIRType::Size);
        let result_elements_malloc_size = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            tensor_elem_size,
            MIRConstant::Size(MIRType::Real.get_size()),
        ));
        self.add_inst(MIRInstruction::Binary(
            result_elements_malloc_size,
            MIRBinaryOp::MultiplySizes,
            result_num_elements,
            tensor_elem_size,
        ));
        let result_elements_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Call(
            Some(result_elements_ptr),
            MIR_EXTERNAL_FUNCTION_MALLOC.0,
            bump_list!(self.bump, result_elements_malloc_size),
        ));

        let two_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            two_register,
            MIRConstant::Size(2),
        ));
        let result_elements_ptr_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Gep(
            result_elements_ptr_ptr,
            result,
            two_register,
            MIRType::Pointer,
        ));
        self.add_inst(MIRInstruction::Store(
            result_elements_ptr,
            result_elements_ptr_ptr,
        ));
        let tensor_elements_ptr_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Gep(
            tensor_elements_ptr_ptr,
            tensor,
            two_register,
            MIRType::Pointer,
        ));
        let tensor_elements_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Load(
            tensor_elements_ptr,
            tensor_elements_ptr_ptr,
        ));

        let result_elements_bytes_size = self.fresh_reg(MIRType::Size);
        let real_size = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            real_size,
            MIRConstant::Size(MIRType::Real.get_size()),
        ));
        self.add_inst(MIRInstruction::Binary(
            result_elements_bytes_size,
            MIRBinaryOp::MultiplySizes,
            real_size,
            result_num_elements,
        ));
        self.add_inst(MIRInstruction::Call(
            None,
            MIR_EXTERNAL_FUNCTION_MEMCPY.0,
            bump_list!(
                self.bump,
                result_elements_ptr,
                tensor_elements_ptr,
                result_elements_bytes_size
            ),
        ));
    }
}
