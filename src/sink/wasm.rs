use std::io::{Result, Write};

use crate::ast::TopLevelExpression;
use crate::sink::WasminSink;

#[derive(Default)]
pub struct Wasm;

impl WasminSink for Wasm {
    fn start(&mut self, _module_name: String, mut _w: &mut Box<dyn Write>) -> Result<()> {
        unimplemented!()
    }

    fn receive(&mut self, _expr: TopLevelExpression, mut _w: &mut Box<dyn Write>) -> Result<()> {
        unimplemented!()
    }
}
