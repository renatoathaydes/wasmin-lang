use std::io::Result;

use wasm_encoder::{BlockType, Instruction, ValType};

use crate::sink::sanitize_number;
use crate::sink::wasm::Context;
use crate::types::{FunType, Type, types_to_string};

pub fn to_val_types(types: &[Type]) -> Vec<ValType> {
    types.iter().map(|t| to_val_type(t)).collect()
}

pub fn to_val_type(typ: &Type) -> ValType {
    match typ {
        Type::I64 => ValType::I64,
        Type::I32 => ValType::I32,
        Type::F64 => ValType::F64,
        Type::F32 => ValType::F32,
        _ => panic!("cannot convert to value type: {}", typ)
    }
}

pub fn to_multi_val_block_type(typ: &[Type], ctx: &mut Context) -> BlockType {
    let types: Vec<_> = typ.to_vec();
    let fun_type = FunType { ins: vec![], outs: types };
    let fun_idx = ctx.index_fun_type(&fun_type);
    BlockType::FunctionType(fun_idx)
}

pub fn to_const(typ: ValType, text: &str) -> Instruction {
    let n = sanitize_number(text);
    match typ {
        ValType::I32 => Instruction::I32Const(n.parse::<i32>().unwrap()),
        ValType::I64 => Instruction::I64Const(n.parse::<i64>().unwrap()),
        ValType::F32 => Instruction::F32Const(n.parse::<f32>().unwrap()),
        ValType::F64 => Instruction::F64Const(n.parse::<f64>().unwrap()),
        _ => panic!("cannot convert to const: {:?}", typ)
    }
}

pub fn map_to_wasm_fun<'a>(name: &'a str, fun_type: &'a FunType) -> Result<Instruction<'a>> {
    let ins = &fun_type.ins;
    let outs = &fun_type.outs;
    if outs.len() != 1 {
        cannot_find_fun(name, fun_type);
    }
    let is_op1 = ins.len() == 1 && ins.get(0) == outs.get(0);
    let is_op2 = ins.len() == 2;
    let is_conv = ins.len() == 1 && !is_op1;

    let instr = if is_op1 {
        match *ins.get(0).unwrap() {
            Type::I32 if name == "eqz" => Instruction::I32Eqz,
            Type::I64 if name == "eqz" => Instruction::I64Eqz,
            Type::I32 if name == "clz" => Instruction::I32Clz,
            Type::I64 if name == "clz" => Instruction::I64Clz,
            Type::I32 if name == "ctz" => Instruction::I32Ctz,
            Type::I64 if name == "ctz" => Instruction::I64Ctz,
            Type::I32 if name == "popcnt" => Instruction::I32Popcnt,
            Type::I64 if name == "popcnt" => Instruction::I64Popcnt,
            Type::I32 if name == "extend_8s" => Instruction::I32Extend8S,
            Type::I64 if name == "extend_8s" => Instruction::I64Extend8S,
            Type::I32 if name == "extend_16s" => Instruction::I32Extend16S,
            Type::I64 if name == "extend_16s" => Instruction::I64Extend16S,

            Type::F32 if name == "abs" => Instruction::F32Abs,
            Type::F64 if name == "abs" => Instruction::F64Abs,
            Type::F32 if name == "neg" => Instruction::F32Neg,
            Type::F64 if name == "neg" => Instruction::F64Neg,
            Type::F32 if name == "sqrt" => Instruction::F32Sqrt,
            Type::F64 if name == "sqrt" => Instruction::F64Sqrt,
            Type::F32 if name == "ceil" => Instruction::F32Ceil,
            Type::F64 if name == "ceil" => Instruction::F64Ceil,
            Type::F32 if name == "floor" => Instruction::F32Floor,
            Type::F64 if name == "floor" => Instruction::F64Floor,
            Type::F32 if name == "trunc" => Instruction::F32Trunc,
            Type::F64 if name == "trunc" => Instruction::F64Trunc,
            Type::F32 if name == "nearest" => Instruction::F32Nearest,
            Type::F64 if name == "nearest" => Instruction::F64Nearest,
            _ => cannot_find_fun(name, fun_type)
        }
    } else if is_op2 {
        match *ins.get(0).unwrap() {
            Type::I64 if name == "add" => Instruction::I64Add,
            Type::I32 if name == "add" => Instruction::I32Add,
            Type::F64 if name == "add" => Instruction::F64Add,
            Type::F32 if name == "add" => Instruction::F32Sub,

            Type::I64 if name == "sub" => Instruction::I64Sub,
            Type::I32 if name == "sub" => Instruction::I32Sub,
            Type::F64 if name == "sub" => Instruction::F64Sub,
            Type::F32 if name == "sub" => Instruction::F32Sub,

            Type::I64 if name == "mul" => Instruction::I64Mul,
            Type::I32 if name == "mul" => Instruction::I32Mul,
            Type::F64 if name == "mul" => Instruction::F64Mul,
            Type::F32 if name == "mul" => Instruction::F32Mul,

            Type::I64 if name == "eq" => Instruction::I64Eq,
            Type::I32 if name == "eq" => Instruction::I32Eq,
            Type::F64 if name == "eq" => Instruction::F64Eq,
            Type::F32 if name == "eq" => Instruction::F32Eq,

            Type::I64 if name == "ne" => Instruction::I64Neq,
            Type::I32 if name == "ne" => Instruction::I32Neq,
            Type::F64 if name == "ne" => Instruction::F64Neq,
            Type::F32 if name == "ne" => Instruction::F32Neq,

            Type::I32 if name == "rem_s" => Instruction::I32RemS,
            Type::I64 if name == "rem_s" => Instruction::I64RemS,
            Type::I32 if name == "rem_u" => Instruction::I32RemU,
            Type::I64 if name == "rem_u" => Instruction::I64RemU,

            Type::I32 if name == "div_s" => Instruction::I32DivS,
            Type::I64 if name == "div_s" => Instruction::I64DivS,
            Type::I32 if name == "div_u" => Instruction::I32DivU,
            Type::I64 if name == "div_u" => Instruction::I64DivU,

            Type::I64 if name == "and" => Instruction::I64And,
            Type::I32 if name == "and" => Instruction::I32And,

            Type::I64 if name == "or" => Instruction::I64Or,
            Type::I32 if name == "or" => Instruction::I32Or,

            Type::I64 if name == "xor" => Instruction::I64Xor,
            Type::I32 if name == "xor" => Instruction::I32Xor,

            Type::I64 if name == "shl" => Instruction::I64Shl,
            Type::I32 if name == "shl" => Instruction::I32Shl,
            Type::I64 if name == "shr_u" => Instruction::I64ShrU,
            Type::I32 if name == "shr_u" => Instruction::I32ShrU,
            Type::I64 if name == "shr_s" => Instruction::I64ShrS,
            Type::I32 if name == "shr_s" => Instruction::I32ShrS,

            Type::I64 if name == "rotl" => Instruction::I64Rotl,
            Type::I32 if name == "rotl" => Instruction::I32Rotl,
            Type::I64 if name == "rotr" => Instruction::I64Rotr,
            Type::I32 if name == "rotr" => Instruction::I32Rotr,

            Type::I64 if name == "gt_s" => Instruction::I64GtS,
            Type::I32 if name == "gt_s" => Instruction::I32GtS,
            Type::I64 if name == "gt_u" => Instruction::I64GtU,
            Type::I32 if name == "gt_u" => Instruction::I32GtU,
            Type::F64 if name == "gt" => Instruction::F64Gt,
            Type::F32 if name == "gt" => Instruction::F32Gt,

            Type::I64 if name == "ge_s" => Instruction::I64GeS,
            Type::I32 if name == "ge_s" => Instruction::I32GeS,
            Type::I64 if name == "ge_u" => Instruction::I64GeU,
            Type::I32 if name == "ge_u" => Instruction::I32GeU,
            Type::F64 if name == "ge" => Instruction::F64Ge,
            Type::F32 if name == "ge" => Instruction::F32Ge,

            Type::I64 if name == "lt_s" => Instruction::I64LtS,
            Type::I32 if name == "lt_s" => Instruction::I32LtS,
            Type::I64 if name == "lt_u" => Instruction::I64LtU,
            Type::I32 if name == "lt_u" => Instruction::I32LtU,
            Type::F64 if name == "lt" => Instruction::F64Lt,
            Type::F32 if name == "lt" => Instruction::F32Lt,

            Type::I64 if name == "le_s" => Instruction::I64LeS,
            Type::I32 if name == "le_s" => Instruction::I32LeS,
            Type::I64 if name == "le_u" => Instruction::I64LeU,
            Type::I32 if name == "le_u" => Instruction::I32LeU,
            Type::F64 if name == "le" => Instruction::F64Le,
            Type::F32 if name == "le" => Instruction::F32Le,

            Type::F64 if name == "div" => Instruction::F64Div,
            Type::F32 if name == "div" => Instruction::F32Div,

            Type::F64 if name == "min" => Instruction::F64Min,
            Type::F32 if name == "min" => Instruction::F32Min,

            Type::F64 if name == "max" => Instruction::F64Max,
            Type::F32 if name == "max" => Instruction::F32Max,

            Type::F64 if name == "copysign" => Instruction::F64Copysign,
            Type::F32 if name == "copysign" => Instruction::F32Copysign,

            _ => cannot_find_fun(name, fun_type)
        }
    } else if is_conv {
        match (ins.get(0).unwrap(), outs.get(0).unwrap()) {
            (&Type::I32, &Type::F64) if name == "convert_i32_u" => Instruction::F64ConvertI32U,
            (&Type::I32, &Type::F32) if name == "convert_i32_u" => Instruction::F32ConvertI32U,
            (&Type::I32, &Type::F64) if name == "convert_i32_s" => Instruction::F64ConvertI32S,
            (&Type::I32, &Type::F32) if name == "convert_i32_s" => Instruction::F32ConvertI32S,
            (&Type::I64, &Type::F64) if name == "convert_i64_u" => Instruction::F64ConvertI64U,
            (&Type::I64, &Type::F32) if name == "convert_i64_u" => Instruction::F32ConvertI64U,
            (&Type::I64, &Type::F64) if name == "convert_i64_s" => Instruction::F64ConvertI64S,
            (&Type::I64, &Type::F32) if name == "convert_i64_s" => Instruction::F32ConvertI64S,

            (&Type::F32, &Type::I64) if name == "trunc_f32_u" => Instruction::I64TruncF32U,
            (&Type::F32, &Type::I32) if name == "trunc_f32_u" => Instruction::I32TruncF32U,
            (&Type::F32, &Type::I64) if name == "trunc_f32_s" => Instruction::I64TruncF32S,
            (&Type::F32, &Type::I32) if name == "trunc_f32_s" => Instruction::I32TruncF32S,
            (&Type::F64, &Type::I64) if name == "trunc_f64_u" => Instruction::I64TruncF64U,
            (&Type::F64, &Type::I32) if name == "trunc_f64_u" => Instruction::I32TruncF64U,
            (&Type::F64, &Type::I64) if name == "trunc_f64_s" => Instruction::I64TruncF64S,
            (&Type::F64, &Type::I32) if name == "trunc_f64_s" => Instruction::I32TruncF64S,

            (&Type::I64, &Type::I32) if name == "wrap_i64" => Instruction::I32WrapI64,
            (&Type::I32, &Type::I64) if name == "extend_i32s" => Instruction::I64Extend32S,
            (&Type::I32, &Type::I64) if name == "extend_i32u" => Instruction::I64ExtendI32U,
            (&Type::F64, &Type::F32) if name == "demote_f64" => Instruction::F32DemoteF64,
            (&Type::F32, &Type::F64) if name == "promote_f32" => Instruction::F64PromoteF32,
            (&Type::I32, &Type::F32) if name == "reinterpret_i32" => Instruction::F32ReinterpretI32,
            (&Type::F32, &Type::I32) if name == "reinterpret_f32" => Instruction::I32ReinterpretF32,
            (&Type::I64, &Type::F64) if name == "reinterpret_i64" => Instruction::F64ReinterpretI64,
            (&Type::F64, &Type::I64) if name == "reinterpret_f64" => Instruction::I64ReinterpretF64,

            _ => cannot_find_fun(name, fun_type)
        }
    } else {
        cannot_find_fun(name, fun_type)
    };
    Ok(instr)
}

fn cannot_find_fun(name: &str, fun_type: &FunType) -> ! {
    panic!("Cannot find WASM fun '{}' taking arguments {:?}",
           name, types_to_string(&fun_type.ins));
}

pub(crate) fn block_type(typ: &[Type], ctx: &mut Context) -> BlockType {
    if typ.is_empty() || typ.first().unwrap().is_empty() {
        BlockType::Empty
    } else if typ.len() == 1 {
        BlockType::Result(to_val_type(typ.first().unwrap()))
    } else {
        to_multi_val_block_type(typ, ctx)
    }
}