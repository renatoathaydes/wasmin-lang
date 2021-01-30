use std::io::Result;

use wasm_encoder::{BlockType, Instruction, ValType};

use crate::sink::sanitize_number;
use crate::sink::wasm::Context;
use crate::types::{FunType, Type};

pub fn to_val_types(types: &Vec<Type>) -> Vec<ValType> {
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

pub fn to_multi_val_block_type(typ: &Vec<Type>, ctx: &mut Context) -> BlockType {
    let fun_type = FunType { ins: vec![], outs: typ.clone() };
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
    let types = &fun_type.ins;
    if types.len() == 2 && types.get(0).unwrap() == types.get(1).unwrap() {
        let instr = match types.get(0).unwrap() {
            &Type::I64 if name == "add" => Instruction::I64Add,
            &Type::I32 if name == "add" => Instruction::I32Add,
            &Type::F64 if name == "add" => Instruction::F64Add,
            &Type::F32 if name == "add" => Instruction::F32Sub,

            &Type::I64 if name == "sub" => Instruction::I64Sub,
            &Type::I32 if name == "sub" => Instruction::I32Sub,
            &Type::F64 if name == "sub" => Instruction::F64Sub,
            &Type::F32 if name == "sub" => Instruction::F32Sub,

            &Type::I64 if name == "mul" => Instruction::I64Mul,
            &Type::I32 if name == "mul" => Instruction::I32Mul,
            &Type::F64 if name == "mul" => Instruction::F64Mul,
            &Type::F32 if name == "mul" => Instruction::F32Mul,

            &Type::I64 if name == "gt_s" => Instruction::I64GtS,
            &Type::I32 if name == "gt_s" => Instruction::I32GtS,
            &Type::I64 if name == "gt_u" => Instruction::I64GtU,
            &Type::I32 if name == "gt_u" => Instruction::I32GtU,
            &Type::F64 if name == "gt" => Instruction::F64Gt,
            &Type::F32 if name == "gt" => Instruction::F32Gt,

            &Type::I64 if name == "ge_s" => Instruction::I64GeS,
            &Type::I32 if name == "ge_s" => Instruction::I32GeS,
            &Type::I64 if name == "ge_u" => Instruction::I64GeU,
            &Type::I32 if name == "ge_u" => Instruction::I32GeU,
            &Type::F64 if name == "ge" => Instruction::F64Ge,
            &Type::F32 if name == "ge" => Instruction::F32Ge,

            &Type::I64 if name == "lt_s" => Instruction::I64LtS,
            &Type::I32 if name == "lt_s" => Instruction::I32LtS,
            &Type::I64 if name == "lt_u" => Instruction::I64LtU,
            &Type::I32 if name == "lt_u" => Instruction::I32LtU,
            &Type::F64 if name == "lt" => Instruction::F64Lt,
            &Type::F32 if name == "lt" => Instruction::F32Lt,

            &Type::I64 if name == "le_s" => Instruction::I64LeS,
            &Type::I32 if name == "le_s" => Instruction::I32LeS,
            &Type::I64 if name == "le_u" => Instruction::I64LeU,
            &Type::I32 if name == "le_u" => Instruction::I32LeU,
            &Type::F64 if name == "le" => Instruction::F64Le,
            &Type::F32 if name == "le" => Instruction::F32Le,

            _ => panic!("Cannot find WASM fun '{}' with type: {:?}", name, fun_type)
        };
        Ok(instr)
    } else {
        panic!("Cannot find WASM fun '{}' with type: {:?}", name, fun_type)
    }
}
