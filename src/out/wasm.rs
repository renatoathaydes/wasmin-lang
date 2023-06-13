use std::io::Write;
use std::ops::Deref;
use std::sync::mpsc::Receiver;

use wasm_encoder::{CodeSection, ConstExpr, ExportKind, ExportSection, FunctionSection, GlobalSection, GlobalType, ImportSection, Module, TypeSection, ValType};

use crate::ast::{TopLevelElement, Visibility};
use crate::conversions::expression::{*};
use crate::conversions::types::{*};
use crate::errors::Error;

pub struct WasmContext {
    imports: ImportSection,
    exports: ExportSection,
    types: TypeSection,
    globals: GlobalSection,
    funs: FunctionSection,
    code: CodeSection,

}

impl WasmContext {
    pub fn new() -> Self {
        WasmContext {
            imports: ImportSection::new(),
            exports: ExportSection::new(),
            types: TypeSection::new(),
            globals: GlobalSection::new(),
            funs: FunctionSection::new(),
            code: CodeSection::new(),
        }
    }

    pub fn write(&mut self, write: &mut Box<dyn Write>, receiver: Receiver<TopLevelElement>) -> Result<(), Error> {
        loop {
            match receiver.recv() {
                Ok(element) => self.add(element)?,
                Err(_) => break,
            }
        }
        self.finish(write)
    }

    fn add(&mut self, element: TopLevelElement) -> Result<(), Error> {
        match element {
            TopLevelElement::Let(assign, visibility, _, _) => {
                let typ = assign.expr.get_type();
                for (_, typ) in assign.vars.iter().zip(&typ.outs) {
                    self.globals.global(GlobalType { mutable: false, val_type: typ.clone().try_into()? },
                                        &assign.expr.deref().clone().try_into()?);
                    // if visibility == Visibility::Public {
                    //     let name = self.interned_str(&var.name);
                    //     self.exports.export(name, ExportKind::Global, index);
                    // }
                }
            }
            TopLevelElement::Mut(_, _, _, _) => {}
            TopLevelElement::Ext(_, _, _, _, _) => {}
            TopLevelElement::Fun(_, _, _, _) => {}
            TopLevelElement::Error(_) => {}
        }
        Ok(())
    }

    fn finish(&mut self, write: &mut Box<dyn Write>) -> Result<(), Error> {
        let mut module = Module::new();
        module.section(&self.types)
            .section(&self.imports)
            .section(&self.funs)
            .section(&self.globals)
            .section(&self.exports)
            .section(&self.code);

        let wasm = module.finish();

        match wasmparser::validate(&wasm) {
            Ok(_) => {}
            Err(e) => {
                return Err(Error::Validation(format!("{}", e)));
            }
        }
        write.write_all(&wasm)?;
        Ok(())
    }
}
