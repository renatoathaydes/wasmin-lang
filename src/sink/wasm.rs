use std::io::{Result, Write};

use crate::ast::TopLevelElement;
use crate::sink::WasminSink;

#[derive(Default)]
pub struct Wasm;

impl WasminSink for Wasm {
    fn start(&mut self, _module_name: String, mut _w: &mut Box<dyn Write>) -> Result<()> {
        unimplemented!()
    }

    fn receive(&mut self, _expr: TopLevelElement, mut _w: &mut Box<dyn Write>) -> Result<()> {
        unimplemented!()
    }
}
