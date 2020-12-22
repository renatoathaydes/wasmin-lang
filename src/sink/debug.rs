use crate::ast::TopLevelExpression;
use crate::sink::WasminSink;

pub struct DebugSink;

impl WasminSink for DebugSink {
    fn receive(&self, expr: TopLevelExpression) -> Result<Vec<u8>, i32> {
        println!("{:?}", expr);
        Ok(Vec::new())
    }
}
