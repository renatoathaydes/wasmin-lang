use std::io::{ErrorKind, Result, Write};

use parity_wasm::builder::ModuleBuilder;
use parity_wasm::serialize;

use crate::ast::TopLevelElement;
use crate::sink::WasminSink;

#[derive(Default)]
pub struct Wasm {}

impl WasminSink<ModuleBuilder> for Wasm {
    fn start(&mut self, _: String, _: &mut Box<dyn Write>) -> Result<ModuleBuilder> {
        Ok(ModuleBuilder::new())
    }

    fn receive(&mut self,
               _expr: TopLevelElement,
               mut _w: &mut Box<dyn Write>,
               _ctx: &mut ModuleBuilder,
    ) -> Result<()> {
        Ok(())
    }

    fn flush(&mut self, w: &mut Box<dyn Write>, ctx: ModuleBuilder) -> Result<()> {
        let module = ctx.build();
        let bytes = serialize(module)
            .map_err(|e| std::io::Error::new(ErrorKind::Other, e.to_string()))?;
        w.write_all(&bytes)
    }
}
