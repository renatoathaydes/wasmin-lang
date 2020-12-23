use crate::ast::TopLevelExpression;
use crate::sink::WasminSink;

pub struct Wasm;

impl WasminSink for Wasm {
    fn start(&mut self, _module_name: String) -> Vec<u8> {
        unimplemented!()
    }

    fn receive(&mut self, _expr: TopLevelExpression) -> Result<Vec<u8>, i32> {
        unimplemented!()
    }
}
