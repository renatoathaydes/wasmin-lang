use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::io::{ErrorKind, Result, Write};

use wasm_encoder::{CodeSection, EntityType, Export, ExportSection, Function, FunctionSection, GlobalSection, GlobalType, ImportSection, Instruction, Module, TypeSection, ValType};

use crate::ast::{Expression, ExtDef, ReAssignment, TopLevelElement, Visibility};
use crate::sink::{expr_to_vec, WasminSink};
use crate::sink::wasm_utils::{*};
use crate::types::{FunType, Type, types_to_string};
use crate::vec_utils::remove_last;

#[derive(Default)]
pub struct Wasm {
    mod_name: String,
}

pub struct Context {
    imports: ImportSection,
    exports: ExportSection,
    types: TypeSection,
    globals: GlobalSection,
    funs: FunctionSection,
    code: CodeSection,
    block_levels: Vec<bool>,
    fun_idx_by_name: HashMap<String, u32>,
    global_idx_by_name: HashMap<String, u32>,
    type_idx_by_type_str: HashMap<String, u32>,
}

impl Context {
    fn start_block(&mut self, is_breakable: bool) {
        self.block_levels.push(is_breakable);
    }

    fn end_block(&mut self) {
        remove_last(&mut self.block_levels);
    }

    fn index_of_breakable(&self) -> u32 {
        let mut current: u32 = 0;
        for is_breakable in self.block_levels.iter().rev() {
            if *is_breakable { return current; }
            current += 1;
        }
        panic!("No breakable level found");
    }

    pub(crate) fn index_fun_type(&mut self, typ: &FunType) -> u32 {
        let types = vec![Type::Fn(vec![typ.clone()])];
        let key = types_to_string(&types);
        let len = self.type_idx_by_type_str.len() as u32;
        match self.type_idx_by_type_str.entry(key) {
            Entry::Occupied(e) => e.get().clone(),
            Entry::Vacant(v) => {
                v.insert(len);
                self.types.function(to_val_types(&typ.ins), to_val_types(&typ.outs));
                len
            }
        }
    }

    fn index_fun(&mut self, fun_name: &str, type_index: u32, is_import: bool) -> u32 {
        let fun_idx = self.fun_idx_by_name.len() as u32;
        if let Some(_) = self.fun_idx_by_name.insert(fun_name.to_owned(), fun_idx) {
            // FIXME overloads should be supported
            panic!("function '{}' duplicated, overload is not implemented yet", fun_name);
        }
        if !is_import {
            self.funs.function(type_index);
        }
        fun_idx
    }
}

impl Wasm {
    fn receive_fun(
        &self,
        name: String,
        args: Vec<String>,
        body: Expression,
        typ: FunType,
        vis: Visibility,
        ctx: &mut Context,
    ) -> Result<()> {
        let type_idx = ctx.index_fun_type(&typ);
        let fun_idx = ctx.index_fun(&name, type_idx, false);
        if vis == Visibility::Public {
            ctx.exports.export(name.as_ref(), Export::Function(fun_idx));
        }
        let f = self.create_fun(ctx, &typ.ins, &args, &body)?;
        ctx.code.function(&f);
        Ok(())
    }

    fn create_fun(
        &self,
        ctx: &mut Context,
        arg_types: &Vec<Type>,
        arg_names: &Vec<String>,
        body: &Expression,
    ) -> Result<Function> {
        let mut local_map = HashMap::with_capacity(arg_names.len() + 2);
        for (i, (typ, name)) in arg_types.iter().zip(arg_names.iter()).enumerate() {
            local_map.insert(name.clone(), (i as u32, to_val_type(typ)));
        }
        self.collect_locals(body, &mut local_map);
        let locals: Vec<_> = local_map.values().cloned().collect();
        let mut f = Function::new(locals);
        self.add_instructions(&mut f, ctx, &local_map, body)?;
        f.instruction(Instruction::End);
        Ok(f)
    }

    fn receive_assignment(&mut self,
                          ctx: &mut Context,
                          names: Vec<String>,
                          value: Expression,
                          vis: Visibility,
                          is_mut: bool,
    ) -> Result<()> {
        let values = expr_to_vec(value);

        // if all values have value types, write globals directly
        if values.iter().all(|v| v.get_value_type().is_some()) {
            for (name, expr) in names.iter().zip(values.iter()) {
                let (instr, typ) = self.expr_to_value(expr, ctx)?;
                let global_idx = ctx.global_idx_by_name.len() as u32;
                ctx.global_idx_by_name.insert(name.to_string(), global_idx);
                if vis == Visibility::Public {
                    ctx.exports.export(name.as_str(), Export::Global(global_idx));
                }
                ctx.globals.global(GlobalType {
                    val_type: typ,
                    mutable: is_mut,
                }, instr);
            }
            Ok(())
        } else {
            self.error("Non-constant global variables are not supported yet.", (0, 0))
        }
    }

    fn receive_ext(&mut self,
                   ctx: &mut Context,
                   mod_name: &str,
                   defs: &Vec<ExtDef>,
    ) -> Result<()> {
        for def in defs {
            let typ = &def.typ;
            match typ {
                // TODO implement imports
                Type::I64 => {}
                Type::I32 => {}
                Type::F64 => {}
                Type::F32 => {}
                Type::Empty => {}
                Type::Fn(types) => {
                    for typ in types {
                        let type_idx = ctx.index_fun_type(&typ);
                        let fun_name = format!("{}.{}", mod_name, def.id);
                        ctx.index_fun(&fun_name, type_idx, true);
                        ctx.imports.import(mod_name, Some(&def.id), EntityType::Function(type_idx));
                    }
                }
                Type::WasmFn(_) => {}
                Type::Error(_) => {}
            };
        }
        Ok(())
    }

    fn add_instructions(
        &self,
        f: &mut Function,
        ctx: &mut Context,
        local_map: &HashMap<String, (u32, ValType)>,
        expr: &Expression,
    ) -> Result<()> {
        match expr {
            Expression::Empty => {}
            Expression::Const(value, typ) => {
                f.instruction(to_const(to_val_type(typ), value.as_ref()));
            }
            Expression::Local(name, ..) => {
                f.instruction(Instruction::LocalGet(local_map.get(name)
                    .expect("local name exists").0.clone()));
            }
            Expression::Global(name, ..) => {
                f.instruction(Instruction::GlobalGet(ctx.global_idx_by_name.get(name)
                    .expect("global name exists").clone()));
            }
            Expression::Let((names, values, ..)) |
            Expression::Mut((names, values, ..)) => {
                self.add_instructions(f, ctx, local_map, values)?;
                names.iter().rev().for_each(|name| {
                    f.instruction(Instruction::LocalSet(local_map.get(name)
                        .expect("local name exists").0.clone()));
                });
            }
            Expression::Set(ReAssignment {
                                assignment: (names, values, ..),
                                globals,
                            }) => {
                self.add_instructions(f, ctx, local_map, values)?;
                names.iter().rev().zip(globals.iter().rev()).for_each(|(name, is_global)| {
                    if *is_global {
                        f.instruction(Instruction::GlobalSet(ctx.global_idx_by_name.get(name)
                            .expect("global name exists").clone()));
                    } else {
                        f.instruction(Instruction::LocalSet(local_map.get(name)
                            .expect("local name exists").0.clone()));
                    }
                });
            }
            Expression::If(cond, then, els) => {
                self.add_instructions(f, ctx, local_map, cond)?;
                let typ = then.get_type();
                f.instruction(Instruction::If(block_type(&typ, ctx)));
                ctx.start_block(false);
                self.add_instructions(f, ctx, local_map, then)?;
                f.instruction(Instruction::Else);
                self.add_instructions(f, ctx, local_map, els)?;
                f.instruction(Instruction::End);
                ctx.end_block();
            }
            Expression::Loop { expr, error } => {
                if let Some(e) = error {
                    return self.error(&e.reason, e.pos);
                }
                let typ = expr.get_type();
                f.instruction(Instruction::Block(block_type(&typ, ctx)));
                ctx.start_block(true);
                f.instruction(Instruction::Loop(block_type(&typ, ctx)));
                ctx.start_block(false);
                self.add_instructions(f, ctx, local_map, expr)?;
                f.instruction(Instruction::Br(0));
                f.instruction(Instruction::End);
                ctx.end_block();
                f.instruction(Instruction::End);
                ctx.end_block();
            }
            Expression::Br(_) => {
                f.instruction(Instruction::Br(ctx.index_of_breakable()));
            }
            Expression::Group(exprs) => {
                for expr in exprs {
                    self.add_instructions(f, ctx, local_map, expr)?;
                }
            }
            Expression::FunCall { name, fun_index, is_wasm_fun, typ: Ok(typ) } => {
                if *is_wasm_fun {
                    f.instruction(map_to_wasm_fun(name.as_ref(), typ)?);
                } else {
                    let idx = ctx.fun_idx_by_name.get(name)
                        .expect("called function exists").clone();
                    f.instruction(Instruction::Call(idx));
                }
            }
            Expression::FunCall { typ: Err(e), .. } => {
                return self.error(e.reason.as_str(), e.pos);
            }
            Expression::ExprError(e) => {
                return self.error(e.reason.as_str(), e.pos);
            }
        };
        Ok(())
    }

    fn error<T>(&self, msg: &str, pos: (usize, usize)) -> Result<T> {
        let (row, col) = pos;
        Err(std::io::Error::new(
            ErrorKind::Other,
            format!("{}[{},{}]: {}\n", self.mod_name, row, col, msg)))
    }

    fn collect_locals(&self, body: &Expression, res: &mut HashMap<String, (u32, ValType)>) {
        match body {
            Expression::Let((names, values, ..)) |
            Expression::Mut((names, values, ..)) => {
                let types = values.get_type();
                names.iter().zip(types.iter()).for_each(|(name, typ)| {
                    res.insert(name.clone(), (res.len() as u32, to_val_type(typ)));
                });
            }
            Expression::If(cond, then, els) => {
                self.collect_locals(cond, res);
                self.collect_locals(then, res);
                self.collect_locals(els, res);
            }
            Expression::Group(exprs) => {
                for expr in exprs {
                    self.collect_locals(expr, res)
                }
            }
            Expression::Loop { expr, .. } => {
                self.collect_locals(expr, res)
            }
            _ => {}
        };
    }

    fn expr_to_value<'a>(&self, expr: &'a Expression, ctx: &Context) -> Result<(Instruction<'a>, ValType)> {
        match expr {
            Expression::Global(name, typ) => {
                let idx = ctx.global_idx_by_name.get(name).unwrap();
                Ok((Instruction::GlobalGet(*idx), to_val_type(typ)))
            }
            Expression::Const(value, typ) => {
                let t = to_val_type(typ);
                Ok((to_const(t, value), t))
            }
            Expression::ExprError(e) =>
                self.error(e.reason.as_str(), e.pos),
            _ => self.error("only constants are currently supported to initialize globals", (0, 0))
        }
    }
}

impl WasminSink<Context> for Wasm {
    fn start(&mut self, mod_name: String, _: &mut Box<dyn Write>) -> Result<Context> {
        self.mod_name = mod_name;
        Ok(Context {
            imports: ImportSection::new(),
            exports: ExportSection::new(),
            globals: GlobalSection::new(),
            types: TypeSection::new(),
            funs: FunctionSection::new(),
            code: CodeSection::new(),
            block_levels: Vec::new(),
            fun_idx_by_name: HashMap::default(),
            global_idx_by_name: HashMap::default(),
            type_idx_by_type_str: HashMap::default(),
        })
    }

    fn receive(&mut self,
               elem: TopLevelElement,
               mut _w: &mut Box<dyn Write>,
               ctx: &mut Context,
    ) -> Result<()> {
        match elem {
            TopLevelElement::Let((names, values, ..), vis, ..) => {
                self.receive_assignment(ctx, names, *values, vis, false)?;
            }
            TopLevelElement::Mut((names, values, ..), vis, ..) => {
                self.receive_assignment(ctx, names, *values, vis, true)?;
            }
            TopLevelElement::Ext(name, defs, ..) => {
                self.receive_ext(ctx, &name, &defs)?;
            }
            TopLevelElement::Fun((name, args, body, typ), vis, _) => {
                self.receive_fun(name, args, body, typ, vis, ctx)?;
            }
            TopLevelElement::Error(reason, pos) => {
                return self.error(reason.as_str(), pos);
            }
        };
        Ok(())
    }

    fn flush(&mut self, w: &mut Box<dyn Write>, ctx: Context) -> Result<()> {
        let mut module = Module::new();
        module.section(&ctx.types);
        module.section(&ctx.imports);
        module.section(&ctx.funs);
        module.section(&ctx.globals);
        module.section(&ctx.exports);
        module.section(&ctx.code);
        let wasm = module.finish();
        match wasmparser::validate(&wasm) {
            Ok(_) => {}
            Err(e) => {
                return Err(std::io::Error::new(ErrorKind::Other,
                                               format!("(WASM Validation) {}\n", e)));
            }
        }
        w.write_all(&wasm)
            .map_err(|e| std::io::Error::new(ErrorKind::Other, e.to_string()))
    }
}
