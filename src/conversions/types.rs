use std::convert::TryInto;

use wasm_encoder::ValType;

use crate::ast::Type;

impl TryInto<ValType> for Type {
    type Error = String;

    fn try_into(self) -> Result<ValType, <Type as TryInto<ValType>>::Error> {
        Ok(match self {
            Type::I64 => ValType::I64,
            Type::I32 => ValType::I32,
            Type::F64 => ValType::F64,
            Type::F32 => ValType::F32,
            Type::String => return Err("String cannot be converted to ValType yet".to_owned()),
            Type::Empty => return Err("Empty cannot be converted to ValType".to_owned()),
            Type::FunType(_) => return Err("FunType cannot be converted to ValType yet".to_owned()),
            Type::Custom(_) => return Err("Custom cannot be converted to ValType yet".to_owned()),
            Type::Error(err) => return Err(err.cause().to_owned()),
        })
    }
}

pub fn val_types(types: &Vec<Type>) -> Result<Vec<ValType>, String> {
    types.iter().map(|t| t.clone().try_into()).collect()
}