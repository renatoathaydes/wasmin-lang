use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::io::{ErrorKind, Result, Write};

use wasm_encoder::{BlockType, CodeSection, Export, ExportSection, Function, FunctionSection,
                   GlobalSection, GlobalType, Instruction, Module, TypeSection, ValType};

use crate::ast::{Expression, ReAssignment, TopLevelElement, Visibility};
use crate::sink::WasminSink;
use crate::sink::wasm_utils::{*};
use crate::types::{FnType, Type, types_to_string};

#[derive(Default)]
pub struct Wasm {
    mod_name: String,
}

pub struct Context {
    exports: ExportSection,
    types: TypeSection,
    globals: GlobalSection,
    funs: FunctionSection,
    code: CodeSection,
    fun_idx_by_name: HashMap<String, u32>,
    global_idx_by_name: HashMap<String, u32>,
    type_idx_by_type_str: HashMap<String, u32>,
}

impl Context {
    fn index_fun_type(&mut self, typ: &FnType) -> u32 {
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
}

impl Wasm {
    fn receive_fun(
        &self,
        name: String,
        args: Vec<String>,
        body: Expression,
        typ: FnType,
        vis: Visibility,
        ctx: &mut Context,
    ) -> Result<()> {
        let type_idx = ctx.index_fun_type(&typ);
        let fun_idx = ctx.fun_idx_by_name.len() as u32;
        // FIXME allow overloads
        if let Some(_) = ctx.fun_idx_by_name.insert(name.clone(), fun_idx) {
            panic!("function '{}' duplicated, overload is not implemented yet", name);
        }
        ctx.funs.function(type_idx);
        if vis == Visibility::Public {
            ctx.exports.export(name.as_ref(), Export::Function(fun_idx));
        }
        let f = self.create_fun(ctx, &typ.ins, &args, &body)?;
        ctx.code.function(&f);
        Ok(())
    }

    fn create_fun(
        &self,
        ctx: &Context,
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
                          values: Vec<Expression>,
                          vis: Visibility,
                          is_mut: bool,
    ) -> Result<()> {
        for (name, value) in names.iter().zip(values.iter()) {
            let global_idx = ctx.global_idx_by_name.len() as u32;
            ctx.global_idx_by_name.insert(name.to_string(), global_idx);
            let types = value.get_type();
            if types.len() == 1 {
                let (instr, typ) = self.expr_to_const(value)?;
                if vis == Visibility::Public {
                    ctx.exports.export(name.as_str(), Export::Global(global_idx));
                }
                ctx.globals.global(GlobalType {
                    val_type: typ,
                    mutable: is_mut,
                }, instr);
            } else {
                panic!("cannot generate WASM for global '{}' with multi-value", name);
            }
        }
        Ok(())
    }

    fn add_instructions(
        &self,
        f: &mut Function,
        ctx: &Context,
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
                for (name, value) in names.iter().zip(values.iter()) {
                    self.add_instructions(f, ctx, local_map, value)?;
                    f.instruction(Instruction::LocalSet(local_map.get(name)
                        .expect("local name exists").0.clone()));
                }
            }
            Expression::Set(ReAssignment {
                                assignment: (names, values, ..),
                                globals,
                            }) => {
                for (name, (value, is_global)) in
                names.iter().zip(values.iter().zip(globals.iter())) {
                    self.add_instructions(f, ctx, local_map, value)?;
                    if *is_global {
                        f.instruction(Instruction::GlobalSet(ctx.global_idx_by_name.get(name)
                            .expect("global name exists").clone()));
                    } else {
                        f.instruction(Instruction::LocalSet(local_map.get(name)
                            .expect("local name exists").0.clone()));
                    }
                }
            }
            Expression::If(cond, then, els) => {
                self.add_instructions(f, ctx, local_map, cond)?;
                let typ = then.get_type();
                if typ.is_empty() || typ.first().unwrap().is_empty() {
                    f.instruction(Instruction::If(BlockType::Empty));
                } else {
                    // TODO what if it's a multi-value if block?
                    f.instruction(Instruction::If(BlockType::Result(
                        to_val_type(typ.first().unwrap()))));
                }
                self.add_instructions(f, ctx, local_map, then)?;
                f.instruction(Instruction::Else);
                self.add_instructions(f, ctx, local_map, els)?;
                f.instruction(Instruction::End);
            }
            Expression::Group(exprs) |
            Expression::Multi(exprs) => {
                for expr in exprs {
                    self.add_instructions(f, ctx, local_map, expr)?;
                }
            }
            Expression::FunCall { name, args, fun_index, is_wasm_fun, .. } => {
                for expr in args {
                    self.add_instructions(f, ctx, local_map, expr)?;
                }
                if *is_wasm_fun {
                    f.instruction(map_to_wasm_fun(name.as_ref(), args)?);
                } else {
                    let idx = ctx.fun_idx_by_name.get(name)
                        .expect("called function exists").clone();
                    f.instruction(Instruction::Call(idx));
                }
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
            Expression::Let((names, types, ..)) |
            Expression::Mut((names, types, ..)) => {
                for (name, expr) in names.iter().zip(types.iter()) {
                    // FIXME multi-values
                    let typ = expr.get_type().first().unwrap().clone();
                    res.insert(name.clone(), (res.len() as u32, to_val_type(&typ)));
                }
            }
            Expression::If(cond, then, els) => {
                self.collect_locals(cond, res);
                self.collect_locals(then, res);
                self.collect_locals(els, res);
            }
            Expression::Group(exprs) |
            Expression::Multi(exprs) => {
                for expr in exprs {
                    self.collect_locals(expr, res)
                }
            }
            _ => {}
        };
    }

    fn expr_to_const<'a>(&self, expr: &'a Expression) -> Result<(Instruction<'a>, ValType)> {
        match expr {
            Expression::Const(value, typ) => {
                let t = to_val_type(typ);
                Ok((to_const(t, value), t))
            }
            Expression::ExprError(e) =>
                self.error(e.reason.as_str(), e.pos),
            _ =>
                self.error("cannot initialize constant", (0, 0))
        }
    }
}

impl WasminSink<Context> for Wasm {
    fn start(&mut self, mod_name: String, _: &mut Box<dyn Write>) -> Result<Context> {
        self.mod_name = mod_name;
        Ok(Context {
            exports: ExportSection::new(),
            globals: GlobalSection::new(),
            types: TypeSection::new(),
            funs: FunctionSection::new(),
            code: CodeSection::new(),
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
                self.receive_assignment(ctx, names, values, vis, false)?;
            }
            TopLevelElement::Mut((names, values, ..), vis, ..) => {
                self.receive_assignment(ctx, names, values, vis, true)?;
            }
            TopLevelElement::Ext(_, _, _, _) => {
                unimplemented!();
            }
            TopLevelElement::Fn((name, args, body, typ), vis, _) => {
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
        module.section(&ctx.funs);
        module.section(&ctx.globals);
        module.section(&ctx.exports);
        module.section(&ctx.code);
        let wasm = module.finish();
        match wasmparser::validate(&wasm) {
            Ok(_) => {}
            Err(e) => {
                return Err(std::io::Error::new(ErrorKind::Other, format!("{}\n", e)));
            }
        }
        w.write_all(&wasm)
            .map_err(|e| std::io::Error::new(ErrorKind::Other, e.to_string()))
    }
}
