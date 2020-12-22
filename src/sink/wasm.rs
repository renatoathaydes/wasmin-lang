use crate::ast::TopLevelExpression;
use crate::sink::WasminSink;

pub struct Wasm;

impl WasminSink for Wasm {
    fn receive(&self, _expr: TopLevelExpression) -> Result<Vec<u8>, i32> {
        unimplemented!()
    }
}
