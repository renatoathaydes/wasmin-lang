use crate::ast::TopLevelExpression;
use crate::sink::WasminSink;

pub struct DebugSink;

impl WasminSink for DebugSink {
    fn start(&mut self, module_name: String) -> Vec<u8> {
        format!("({}\n", module_name).into_bytes()
    }

    fn receive(&mut self, expr: TopLevelExpression) -> Result<Vec<u8>, i32> {
        Ok(format!("{:?}\n", expr).into_bytes())
    }

    fn flush(&mut self) -> Result<Vec<u8>, i32> {
        Ok(format!(")\n").into_bytes())
    }
}
