use wasm_encoder::ConstExpr;

use crate::ast::{Constant, Expression};
use crate::parse::model::Numeric;

impl TryInto<ConstExpr> for Expression {
    type Error = String;

    fn try_into(self) -> Result<ConstExpr, Self::Error> {
        Ok(match self {
            Expression::Empty(_) => ConstExpr::empty(),
            Expression::Const(c, _, _, _) => match c {
                Constant::String(s) => return Err(format!("cannot convert InternedStr to bytes yet")),
                Constant::Number(Numeric::F64(float)) => ConstExpr::f64_const(float),
                Constant::Number(Numeric::F32(float)) => ConstExpr::f32_const(float),
                Constant::Number(Numeric::I64(int)) => ConstExpr::i64_const(int),
                Constant::Number(Numeric::I32(int)) => ConstExpr::i32_const(int),
            }
            _ => return Err(format!("not a constant: {:?}", self)),
        })
    }
}