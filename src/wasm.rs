use crate::ast::{ExprType, Type};
use crate::ast::Type::*;

pub struct WASM {
    binops: Vec<ExprType>,
    ibinops: Vec<ExprType>,
    fbinops: Vec<ExprType>,
}

impl WASM {
    pub fn new() -> WASM {
        WASM {
            binops: vec![
                ExprType::new(vec![I32, I32], vec![I32]),
                ExprType::new(vec![I64, I64], vec![I64]),
                ExprType::new(vec![F32, F32], vec![F32]),
                ExprType::new(vec![F64, F64], vec![F64]),
            ],
            ibinops: vec![
                ExprType::new(vec![I32, I32], vec![I32]),
                ExprType::new(vec![I64, I64], vec![I64]),
            ],
            fbinops: vec![
                ExprType::new(vec![F32, F32], vec![F32]),
                ExprType::new(vec![F64, F64], vec![F64]),
            ],
        }
    }

    pub fn lookup_wasm_fun_type(&self, name: &str) -> Option<&Vec<ExprType>> {
        // TODO add more functions, only binary operators added so far
        match name {
            "add" | "sub" | "mul" => Some(&self.binops),
            "div_u" | "div_s" | "rem_u" | "rem_s" |
            "and" | "or" | "xor" | "shl" | "shr_s" | "shr_u" | "rotl" | "rotr" => Some(&self.ibinops),
            "div" | "min" | "max" | "copysign" => Some(&self.fbinops),
            _ => None,
        }
    }
}
