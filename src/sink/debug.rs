use std::io::{Write};

use crate::ast::TopLevelElement;
use crate::sink::WasminSink;
use crate::errors::Result;

#[derive(Default)]
pub struct DebugSink;

impl WasminSink<()> for DebugSink {
    fn start(&mut self, module_name: String, w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(b"(")?;
        w.write_all(module_name.as_bytes())?;
        w.write_all(b"\n")?;
        Ok(())
    }

    fn receive(&mut self, elem: TopLevelElement, w: &mut Box<dyn Write>, _: &mut ()) -> Result<()> {
        w.write_all(format!("  {:?}\n", elem).as_bytes())?;
        Ok(())
    }

    fn flush(&mut self, w: &mut Box<dyn Write>, _: ()) -> Result<()> {
        w.write_all(b")")?;
        Ok(())
    }
}
