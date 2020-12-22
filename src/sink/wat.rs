use crate::ast::TopLevelExpression;
use crate::sink::WasminSink;

pub struct Wat;

impl WasminSink for Wat {
    fn start(&self, module_name: String) -> Vec<u8> {
        format!("({}\n", module_name).into_bytes()
    }

    fn receive(&self, expr: TopLevelExpression) -> Result<Vec<u8>, i32> {
        Ok(format!("WAT: {:?}\n", expr).into_bytes())
    }

    fn flush(&self) -> Result<Vec<u8>, i32> {
        Ok(format!(")\n").into_bytes())
    }
}
