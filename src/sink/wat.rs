use crate::ast::TopLevelExpression;
use crate::sink::WasminSink;

pub struct Wat;

impl WasminSink for Wat {
    fn receive(&self, expr: TopLevelExpression) -> Result<Vec<u8>, i32> {
        println!("WAT: {:?}", expr);
        Ok(Vec::new())
    }
}
