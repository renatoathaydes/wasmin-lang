use std::io::Write;
use std::ops::Deref;
use std::sync::{Arc, Mutex};
use std::sync::mpsc::Receiver;

use wasm_encoder::{CodeSection, ExportKind, ExportSection, Function, FunctionSection, GlobalSection, GlobalType, ImportSection, Instruction, Module, TypeSection};

use crate::ast::{Constant, Expression, TopLevelElement};
use crate::ast::Visibility::Public;
use crate::conversions::expression::{*};
use crate::conversions::types::{*};
use crate::errors::Error;
use crate::interner::{InternedStr, Interner};
use crate::parse::model::Numeric;

pub struct WasmContext {
    imports: ImportSection,
    exports: ExportSection,
    types: TypeSection,
    globals: GlobalSection,
    funs: FunctionSection,
    code: CodeSection,
    interned_strings: Arc<Mutex<Interner>>,
    var_index: u32,
}

impl WasmContext {
    pub fn new(interned_strings: Arc<Mutex<Interner>>) -> Self {
        WasmContext {
            imports: ImportSection::new(),
            exports: ExportSection::new(),
            types: TypeSection::new(),
            globals: GlobalSection::new(),
            funs: FunctionSection::new(),
            code: CodeSection::new(),
            interned_strings,
            var_index: 0,
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

    fn get_interned(&self, interned: &InternedStr) -> String {
        self.interned_strings.lock().unwrap().get(interned).to_owned()
    }

    fn add(&mut self, element: TopLevelElement) -> Result<(), Error> {
        match element {
            TopLevelElement::Let(assign, visibility, _, _) => {
                let typ = assign.expr.get_type();
                for (var, typ) in assign.vars.iter().zip(&typ.outs) {
                    self.globals.global(GlobalType { mutable: false, val_type: typ.clone().try_into()? },
                                        &assign.expr.deref().clone().try_into()?);
                    if visibility == Public {
                        let index = self.var_index;
                        self.var_index += 1;
                        let name = self.get_interned(&var.name);
                        self.exports.export(&name, ExportKind::Global, index);
                    }
                }
            }
            TopLevelElement::Mut(_, _, _, _) => {}
            TopLevelElement::Ext(_, _, _, _, _) => {}
            TopLevelElement::Fun(fun, vis, _, _) => {
                self.types.function(val_types(&fun.typ.ins)?, val_types(&fun.typ.outs)?);
                self.funs.function(fun.fun_index as u32);
                self.code.function(&self.new_function(&fun.body)?);
                if vis == Public {
                    let name = self.get_interned(&fun.name);
                    self.exports.export(&name, ExportKind::Func, fun.fun_index as u32);
                }
            }
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

    fn new_function(&self, body: &Expression) -> Result<Function, String> {
        let locals = vec![];
        let mut fun = Function::new(locals);
        match body {
            Expression::Const(Constant::Number(Numeric::I32(n)), ..) => {
                fun.instruction(&Instruction::I32Const(*n))
            }
            _ => {
                return Err(format!("cannot generate instruction for {:?} yet", body));
            }
        };
        fun.instruction(&Instruction::End);
        Ok(fun)
    }
}
