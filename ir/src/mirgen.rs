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

pub fn mirgen<'a>(program: &'a HIRModule<'a>, bump: &'a bump::BumpAllocator) -> MIRModule<'a> {
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
        let id = self.get_curr_func_mut().bloated_num_regs_used;
        self.get_curr_func_mut().bloated_num_regs_used += 1;
        (id, ty)
    }

    fn mirgen_program(&mut self, program: &'a HIRModule<'a>) {
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
            bloated_num_regs_used: func.num_regs_used,
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
                        if let HIRType::Tensor = left_reg.1 {
                            self.add_inst(MIRInstruction::Call(
                                Some(left_mir_reg),
                                MIR_EXTERNAL_FUNCTION_RT_COPY_TENSOR.0,
                                bump_list!(self.bump, right_mir_reg),
                            ));
                        } else {
                            self.add_inst(MIRInstruction::Copy(left_mir_reg, right_mir_reg));
                        }
                    }
                }
                HIRInstruction::Unary(left_reg, op, right_reg) => {
                    if let HIRUnaryOp::Shape = op {
                        if let Some((left_mir_reg, right_mir_reg)) =
                            convert_2_registers((*left_reg, *right_reg))
                        {
                            self.add_inst(MIRInstruction::Call(
                                Some(left_mir_reg),
                                MIR_EXTERNAL_FUNCTION_RT_SHAPE_OF_TENSOR.0,
                                bump_list!(self.bump, right_mir_reg),
                            ));
                        }
                    } else {
                        let op = match op {
                            HIRUnaryOp::Not => MIRUnaryOp::Not,
                            HIRUnaryOp::Negate => MIRUnaryOp::Negate,
                            _ => {
                                panic!("PANIC: HIRUnaryOp should've been handled by outer branch.")
                            }
                        };
                        if let Some((left_mir_reg, right_mir_reg)) =
                            convert_2_registers((*left_reg, *right_reg))
                        {
                            self.add_inst(MIRInstruction::Unary(left_mir_reg, op, right_mir_reg));
                        }
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
                                self.add_inst(MIRInstruction::Call(
                                    Some(result_mir_reg),
                                    MIR_EXTERNAL_FUNCTION_RT_SHAPED_AS.0,
                                    bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                ));
                            }
                            HIRBinaryOp::MatrixMultiply => {
                                self.add_inst(MIRInstruction::Call(
                                    Some(result_mir_reg),
                                    MIR_EXTERNAL_FUNCTION_RT_MATMUL.0,
                                    bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                ));
                            }
                            HIRBinaryOp::AddTensors => {
                                self.add_inst(MIRInstruction::Call(
                                    Some(result_mir_reg),
                                    MIR_EXTERNAL_FUNCTION_RT_ADD_TENSORS.0,
                                    bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                ));
                            }
                            HIRBinaryOp::SubtractTensors => {
                                self.add_inst(MIRInstruction::Call(
                                    Some(result_mir_reg),
                                    MIR_EXTERNAL_FUNCTION_RT_SUBTRACT_TENSORS.0,
                                    bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                ));
                            }
                            HIRBinaryOp::MultiplyTensors => {
                                self.add_inst(MIRInstruction::Call(
                                    Some(result_mir_reg),
                                    MIR_EXTERNAL_FUNCTION_RT_MULTIPLY_TENSORS.0,
                                    bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                ));
                            }
                            HIRBinaryOp::DivideTensors => {
                                self.add_inst(MIRInstruction::Call(
                                    Some(result_mir_reg),
                                    MIR_EXTERNAL_FUNCTION_RT_DIVIDE_TENSORS.0,
                                    bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                ));
                            }
                            HIRBinaryOp::PowerTensors => {
                                self.add_inst(MIRInstruction::Call(
                                    Some(result_mir_reg),
                                    MIR_EXTERNAL_FUNCTION_RT_POWER_TENSORS.0,
                                    bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                ));
                            }
                            HIRBinaryOp::NotEqualsTensors => {
                                self.add_inst(MIRInstruction::Call(
                                    Some(result_mir_reg),
                                    MIR_EXTERNAL_FUNCTION_RT_NOT_EQUALS_TENSORS.0,
                                    bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                ));
                            }
                            HIRBinaryOp::EqualsEqualsTensors => {
                                self.add_inst(MIRInstruction::Call(
                                    Some(result_mir_reg),
                                    MIR_EXTERNAL_FUNCTION_RT_EQUALS_EQUALS_TENSORS.0,
                                    bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                ));
                            }
                            _ => {
                                if let HIRBinaryOp::PowerNumbers = op {
                                    self.add_inst(MIRInstruction::Call(
                                        Some(result_mir_reg),
                                        MIR_EXTERNAL_FUNCTION_RT_POWER_REALS.0,
                                        bump_list!(self.bump, left_mir_reg, right_mir_reg),
                                    ));
                                } else {
                                    let mir_op = match op {
                                        HIRBinaryOp::AddNumbers => MIRBinaryOp::AddReals,
                                        HIRBinaryOp::SubtractNumbers => MIRBinaryOp::SubtractReals,
                                        HIRBinaryOp::MultiplyNumbers => MIRBinaryOp::MultiplyReals,
                                        HIRBinaryOp::DivideNumbers => MIRBinaryOp::DivideReals,
                                        HIRBinaryOp::Greater => MIRBinaryOp::GreaterReals,
                                        HIRBinaryOp::Lesser => MIRBinaryOp::LesserReals,
                                        HIRBinaryOp::NotEqualsBooleans => {
                                            MIRBinaryOp::NotEqualsBooleans
                                        }
                                        HIRBinaryOp::EqualsEqualsBooleans => {
                                            MIRBinaryOp::EqualsEqualsBooleans
                                        }
                                        HIRBinaryOp::NotEqualsStrings => {
                                            MIRBinaryOp::NotEqualsStrings
                                        }
                                        HIRBinaryOp::EqualsEqualsStrings => {
                                            MIRBinaryOp::EqualsEqualsStrings
                                        }
                                        HIRBinaryOp::NotEqualsNumbers => {
                                            MIRBinaryOp::NotEqualsReals
                                        }
                                        HIRBinaryOp::EqualsEqualsNumbers => {
                                            MIRBinaryOp::EqualsEqualsReals
                                        }
                                        HIRBinaryOp::GreaterEquals => {
                                            MIRBinaryOp::GreaterEqualsReals
                                        }
                                        HIRBinaryOp::LesserEquals => MIRBinaryOp::LesserEqualsReals,
                                        HIRBinaryOp::And => MIRBinaryOp::AndBooleans,
                                        HIRBinaryOp::Or => MIRBinaryOp::OrBooleans,
                                        _ => panic!(
                                            "PANIC: Unimplemented simple HIR->MIR binary op."
                                        ),
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
                }
                HIRInstruction::Index(dst_reg, src_reg, indices) => {
                    let (dst_mir_reg, src_mir_reg) =
                        convert_2_registers((*dst_reg, *src_reg)).unwrap();
                    match (dst_reg.1, src_reg.1) {
                        (HIRType::Number, HIRType::Tensor) => {
                            let ptr = self.mirgen_index_tensor(src_mir_reg, indices);
                            self.add_inst(MIRInstruction::Load(dst_mir_reg, ptr));
                        }
                        (HIRType::Tensor, HIRType::Number) => {
                            let ptr = self.mirgen_index_tensor(dst_mir_reg, indices);
                            self.add_inst(MIRInstruction::Store(src_mir_reg, ptr));
                        }
                        _ => panic!("PANIC: Types for index op."),
                    };
                }
                HIRInstruction::Array(result_reg, contents) => {
                    let result_reg = convert_register(*result_reg).unwrap();
                    self.mirgen_allocate_empty_tensor(result_reg);
                    self.mirgen_fill_empty_tensor(result_reg, contents);
                }
                HIRInstruction::BranchUncond(bb_id) => {
                    self.add_inst(MIRInstruction::BranchUncond(*bb_id));
                }
                HIRInstruction::BranchCond(cond, true_bb_id, false_bb_id) => {
                    let mir_cond = convert_register(*cond).unwrap();
                    self.add_inst(MIRInstruction::BranchCond(
                        mir_cond,
                        *true_bb_id,
                        *false_bb_id,
                    ));
                }
                HIRInstruction::Call(result_reg, function, arguments) => {
                    let maybe_mir_result_reg = convert_register(*result_reg);
                    let arguments_mir = self.bump.create_list();
                    for i in 0..arguments.len() {
                        if let Some(mir_reg) = convert_register(*arguments.at(i)) {
                            arguments_mir.push(mir_reg);
                        }
                    }
                    self.add_inst(MIRInstruction::Call(
                        maybe_mir_result_reg,
                        *function,
                        arguments_mir,
                    ));
                }
                HIRInstruction::Print(print_reg) => match print_reg.1 {
                    HIRType::Nil => {
                        self.add_inst(MIRInstruction::Call(
                            None,
                            MIR_EXTERNAL_FUNCTION_RT_PRINT_NIL.0,
                            self.bump.create_list(),
                        ));
                    }
                    HIRType::Boolean => {
                        self.add_inst(MIRInstruction::Call(
                            None,
                            MIR_EXTERNAL_FUNCTION_RT_PRINT_BOOLEAN.0,
                            bump_list!(self.bump, convert_register(*print_reg).unwrap()),
                        ));
                    }
                    HIRType::String => {
                        self.add_inst(MIRInstruction::Call(
                            None,
                            MIR_EXTERNAL_FUNCTION_RT_PRINT_STRING.0,
                            bump_list!(self.bump, convert_register(*print_reg).unwrap()),
                        ));
                    }
                    HIRType::Number => {
                        self.add_inst(MIRInstruction::Call(
                            None,
                            MIR_EXTERNAL_FUNCTION_RT_PRINT_NUMBER.0,
                            bump_list!(self.bump, convert_register(*print_reg).unwrap()),
                        ));
                    }
                    HIRType::Tensor => {
                        self.add_inst(MIRInstruction::Call(
                            None,
                            MIR_EXTERNAL_FUNCTION_RT_PRINT_TENSOR.0,
                            bump_list!(self.bump, convert_register(*print_reg).unwrap()),
                        ));
                    }
                },
                HIRInstruction::Line(line_reg) => {
                    self.add_inst(MIRInstruction::Call(
                        Some(convert_register(*line_reg).unwrap()),
                        MIR_EXTERNAL_FUNCTION_RT_LINE.0,
                        self.bump.create_list(),
                    ));
                }
                HIRInstruction::Verify(verify_reg) => {
                    self.add_inst(MIRInstruction::Call(
                        None,
                        MIR_EXTERNAL_FUNCTION_RT_ASSERT.0,
                        bump_list!(self.bump, convert_register(*verify_reg).unwrap()),
                    ));
                }
                HIRInstruction::Return(return_reg) => {
                    self.add_inst(MIRInstruction::Return(convert_register(*return_reg)));
                }
            }
        }
    }

    fn mirgen_allocate_empty_tensor(&mut self, result: MIRRegister) {
        let tensor_size_const = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            tensor_size_const,
            MIRConstant::Size(MIR_TENSOR_SIZE as usize),
        ));
        self.add_inst(MIRInstruction::Call(
            Some(result),
            MIR_EXTERNAL_FUNCTION_RT_MALLOC.0,
            bump_list!(self.bump, tensor_size_const),
        ));
    }

    fn mirgen_index_tensor(
        &mut self,
        tensor: MIRRegister,
        indices: &'a bump::List<'a, HIRRegister>,
    ) -> MIRRegister {
        let tensor_dimensionality = self.fresh_reg(MIRType::Fixed);
        self.add_inst(MIRInstruction::Load(tensor_dimensionality, tensor));
        let static_indices_len = self.fresh_reg(MIRType::Fixed);
        self.add_inst(MIRInstruction::Immediate(
            static_indices_len,
            MIRConstant::Fixed(indices.len() as u32),
        ));
        let assert_reg = self.fresh_reg(MIRType::Boolean);
        self.add_inst(MIRInstruction::Binary(
            assert_reg,
            MIRBinaryOp::EqualsEqualsFixed,
            static_indices_len,
            tensor_dimensionality,
        ));
        self.add_inst(MIRInstruction::Call(
            None,
            MIR_EXTERNAL_FUNCTION_RT_ASSERT.0,
            bump_list!(self.bump, assert_reg),
        ));

        let tensor_shape_ptr_ptr = self.fresh_reg(MIRType::Pointer);
        let one_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            one_register,
            MIRConstant::Size(1),
        ));
        self.add_inst(MIRInstruction::Gep(
            tensor_shape_ptr_ptr,
            tensor,
            one_register,
            MIRType::Pointer,
        ));
        let tensor_shape_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Load(tensor_shape_ptr, tensor_shape_ptr_ptr));
        let first_index_hir_reg = *indices.at(0);
        let first_index_reg = convert_register(first_index_hir_reg).unwrap();
        let first_index_fixed_reg = self.fresh_reg(MIRType::Fixed);
        self.add_inst(MIRInstruction::Unary(
            first_index_fixed_reg,
            MIRUnaryOp::Round,
            first_index_reg,
        ));
        let flat_index = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Unary(
            flat_index,
            MIRUnaryOp::Widen,
            first_index_fixed_reg,
        ));
        for i in 1..indices.len() {
            let index_reg = self.fresh_reg(MIRType::Size);
            self.add_inst(MIRInstruction::Immediate(index_reg, MIRConstant::Size(i)));
            let tensor_shape_element_ptr = self.fresh_reg(MIRType::Pointer);
            self.add_inst(MIRInstruction::Gep(
                tensor_shape_element_ptr,
                tensor_shape_ptr,
                index_reg,
                MIRType::Fixed,
            ));
            let tensor_shape_element = self.fresh_reg(MIRType::Fixed);
            self.add_inst(MIRInstruction::Load(
                tensor_shape_element,
                tensor_shape_element_ptr,
            ));
            let wide_tensor_shape_element = self.fresh_reg(MIRType::Size);
            self.add_inst(MIRInstruction::Unary(
                wide_tensor_shape_element,
                MIRUnaryOp::Widen,
                tensor_shape_element,
            ));
            self.add_inst(MIRInstruction::Binary(
                flat_index,
                MIRBinaryOp::MultiplySizes,
                flat_index,
                wide_tensor_shape_element,
            ));

            let index_hir_reg = *indices.at(i);
            let index_reg = convert_register(index_hir_reg).unwrap();
            let index_fixed_reg = self.fresh_reg(MIRType::Fixed);
            self.add_inst(MIRInstruction::Unary(
                index_fixed_reg,
                MIRUnaryOp::Round,
                index_reg,
            ));
            let index_wide_reg = self.fresh_reg(MIRType::Size);
            self.add_inst(MIRInstruction::Unary(
                index_wide_reg,
                MIRUnaryOp::Widen,
                index_fixed_reg,
            ));
            self.add_inst(MIRInstruction::Binary(
                flat_index,
                MIRBinaryOp::AddSizes,
                flat_index,
                index_wide_reg,
            ));
        }

        let tensor_elements_ptr_ptr = self.fresh_reg(MIRType::Pointer);
        let two_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            two_register,
            MIRConstant::Size(2),
        ));
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
        let tensor_element_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Gep(
            tensor_element_ptr,
            tensor_elements_ptr,
            flat_index,
            MIRType::Real,
        ));

        tensor_element_ptr
    }

    fn mirgen_fill_empty_tensor(
        &mut self,
        result: MIRRegister,
        contents: &'a bump::List<'a, HIRRegister>,
    ) {
        let one_register = self.fresh_reg(MIRType::Fixed);
        self.add_inst(MIRInstruction::Immediate(
            one_register,
            MIRConstant::Fixed(1),
        ));
        self.add_inst(MIRInstruction::Store(one_register, result));
        let result_shape_ptr_ptr = self.fresh_reg(MIRType::Pointer);
        let one_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            one_register,
            MIRConstant::Size(1),
        ));
        self.add_inst(MIRInstruction::Gep(
            result_shape_ptr_ptr,
            result,
            one_register,
            MIRType::Pointer,
        ));
        let result_shape_ptr = self.fresh_reg(MIRType::Pointer);
        let one_fixed_size_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            one_fixed_size_register,
            MIRConstant::Size(MIRType::Fixed.get_size()),
        ));
        self.add_inst(MIRInstruction::Call(
            Some(result_shape_ptr),
            MIR_EXTERNAL_FUNCTION_RT_MALLOC.0,
            bump_list!(self.bump, one_fixed_size_register),
        ));
        self.add_inst(MIRInstruction::Store(
            result_shape_ptr,
            result_shape_ptr_ptr,
        ));
        let num_elements_register = self.fresh_reg(MIRType::Fixed);
        self.add_inst(MIRInstruction::Immediate(
            num_elements_register,
            MIRConstant::Fixed(contents.len() as u32),
        ));
        self.add_inst(MIRInstruction::Store(
            num_elements_register,
            result_shape_ptr,
        ));
        let one_real_size_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            one_real_size_register,
            MIRConstant::Size(MIRType::Real.get_size()),
        ));
        let num_elements_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            num_elements_register,
            MIRConstant::Size(contents.len()),
        ));
        let elements_malloc_size = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Binary(
            elements_malloc_size,
            MIRBinaryOp::MultiplySizes,
            one_real_size_register,
            num_elements_register,
        ));
        let result_elements_ptr = self.fresh_reg(MIRType::Pointer);
        self.add_inst(MIRInstruction::Call(
            Some(result_elements_ptr),
            MIR_EXTERNAL_FUNCTION_RT_MALLOC.0,
            bump_list!(self.bump, elements_malloc_size),
        ));
        let result_elements_ptr_ptr = self.fresh_reg(MIRType::Pointer);
        let two_register = self.fresh_reg(MIRType::Size);
        self.add_inst(MIRInstruction::Immediate(
            two_register,
            MIRConstant::Size(2),
        ));
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
        for i in 0..contents.len() {
            let element = convert_register(*contents.at(i)).unwrap();
            let index_reg = self.fresh_reg(MIRType::Size);
            self.add_inst(MIRInstruction::Immediate(index_reg, MIRConstant::Size(i)));
            let result_element_ptr = self.fresh_reg(MIRType::Pointer);
            self.add_inst(MIRInstruction::Gep(
                result_element_ptr,
                result_elements_ptr,
                index_reg,
                MIRType::Real,
            ));
            self.add_inst(MIRInstruction::Store(element, result_element_ptr));
        }
    }
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn mirgen_simple() {
        let cases = &[
            (b"a x = 1 + 2;" as &'static [u8], "fn @main() -> None {\n0:\n    im %0, 1\n    im %1, 2\n    bi %2, AddReals, %0, %1\n    cp %3, %2\n    re\n}"),
            (b"p [2, 3, 5];", "fn @main() -> None {\n0:\n    im %0, 2\n    im %1, 3\n    im %2, 5\n    im %5, 24\n    ca %3, @rt_malloc, (%5)\n    im %6, 1\n    st %6, %3\n    im %8, 1\n    gp %7, %3, %8, Pointer\n    im %10, 4\n    ca %9, @rt_malloc, (%10)\n    st %9, %7\n    im %11, 3\n    st %11, %9\n    im %12, 8\n    im %13, 3\n    bi %14, MultiplySizes, %12, %13\n    ca %15, @rt_malloc, (%14)\n    im %17, 2\n    gp %16, %3, %17, Pointer\n    st %15, %16\n    im %18, 0\n    gp %19, %15, %18, Real\n    st %0, %19\n    im %20, 1\n    gp %21, %15, %20, Real\n    st %1, %21\n    im %22, 2\n    gp %23, %15, %22, Real\n    st %2, %23\n    ca @rt_print_tensor, (%3)\n    re\n}"),
            (b"", "fn @main() -> None {\n0:\n    re\n}")
        ];

        for (input, output) in cases {
            let bump = bump::BumpAllocator::new();
            let tokens = parse::lex(*input, &bump).unwrap();
            let (ast, _) = parse::parse_program(&tokens, &bump).unwrap();
            let typed_program = semant::typecheck_program(ast, &bump).unwrap();
            let hir_program = hirgen(&typed_program, &bump);
            let mir_program = mirgen(&hir_program, &bump);
            let correct_program = *output;
            assert_eq!(mir_program.to_string(), correct_program);
        }
    }
}
