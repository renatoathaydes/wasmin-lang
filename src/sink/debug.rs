use std::io::{Result, Write};

use crate::ast::TopLevelElement;
use crate::sink::WasminSink;

#[derive(Default)]
pub struct DebugSink;

impl WasminSink for DebugSink {
    fn start(&mut self, module_name: String, w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(b"(")?;
        w.write_all(module_name.as_bytes())?;
        w.write_all(b"\n")
    }

    fn receive(&mut self, expr: TopLevelElement, w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(format!("  {:?}\n", expr).as_bytes())
    }

    fn flush(&mut self, w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(b")")
    }
}
