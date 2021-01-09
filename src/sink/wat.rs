use std::io::{ErrorKind, Result, Write};

use crate::ast::{Expression, ExtDef, ReAssignment, TopLevelElement, Visibility};
use crate::sink::{for_each_assignment, sanitize_number, WasminSink};
use crate::types::{FnType, Type};

const ONE_IDENT: &str = "  ";

pub struct Wat {
    mod_name: String,
    ident: String,
}

impl Default for Wat {
    fn default() -> Self {
        Wat { mod_name: String::from(""), ident: ONE_IDENT.to_owned() }
    }
}

impl Wat {
    fn increase_ident(&mut self) {
        self.ident.push_str(ONE_IDENT);
    }

    fn decrease_ident(&mut self) {
        (0..ONE_IDENT.len()).for_each(|_| { self.ident.remove(0); });
    }

    fn write_global_assignment(
        &mut self,
        mut w: &mut Box<dyn Write>,
        id: &String,
        expr: &Expression,
        vis: &Visibility,
        is_mut: bool,
    ) -> Result<()> {
        let typ = if is_mut {
            format!("(mut {})", expr.get_type().get(0).unwrap().to_string())
        } else {
            expr.get_type().get(0).unwrap().to_string()
        };
        w.write_all(b"(global $")?;
        w.write_all(id.as_bytes())?;
        w.write_all(b" ")?;
        w.write_all(typ.as_bytes())?;
        w.write_all(b" ")?;
        if vis == &Visibility::Public {
            w.write_all(b"(export \"")?;
            w.write_all(id.as_bytes())?;
            w.write_all(b"\") ")?;
        }
        self.increase_ident();
        self.start_expr(w)?;
        self.write_expr(&mut w, &expr)?;
        self.decrease_ident();
        w.write_all(b")")
    }

    fn write_local_assignment(
        &mut self,
        mut w: &mut Box<dyn Write>,
        id: &String,
        expr: &Expression,
        is_global: bool,
    ) -> Result<()> {
        if is_global {
            w.write_all(b"(global.set $")?;
        } else {
            w.write_all(b"(local.set $")?;
        };
        w.write_all(id.as_bytes())?;
        self.increase_ident();
        self.start_expr(w)?;
        self.write_expr(&mut w, &expr)?;
        self.decrease_ident();
        w.write_all(b")")
    }

    fn write_variables(
        &mut self,
        mut w: &mut Box<dyn Write>,
        expr: &Expression,
    ) -> Result<()> {
        match expr {
            Expression::Let(assign) | Expression::Mut(assign) => {
                for_each_assignment(&mut w, assign, |mut w2, id, expr, _| {
                    self.start_expr(&mut w2)?;
                    w2.write_all(b"(local $")?;
                    w2.write_all(id.as_bytes())?;
                    w2.write_all(b" ")?;
                    w2.write_all(expr.get_type()[0].to_string().as_bytes())?;
                    w2.write_all(b")")
                })?;
            }
            Expression::Multi(exprs) | Expression::Group(exprs) => {
                for expr in exprs {
                    self.write_variables(&mut w, expr)?;
                }
            }
            _ => {}
        };
        Ok(())
    }

    fn write_expr(
        &mut self,
        mut w: &mut Box<dyn Write>,
        expr: &Expression,
    ) -> Result<()> {
        match expr {
            Expression::Empty => Ok(()),
            Expression::Const(id, typ) => {
                w.write_all(b"(")?;
                w.write_all(typ.to_string().as_bytes())?;
                w.write_all(b".const ")?;
                w.write_all(sanitize_number(id).as_bytes())?;
                w.write_all(b")")
            }
            Expression::Global(id, _) => {
                w.write_all(b"(global.get $")?;
                w.write_all(id.as_bytes())?;
                w.write_all(b")")
            }
            Expression::Local(id, _) => {
                w.write_all(b"(local.get $")?;
                w.write_all(id.as_bytes())?;
                w.write_all(b")")
            }
            Expression::Let(assign) | Expression::Mut(assign) => {
                let use_new_lines = !assign.0.is_empty();
                for_each_assignment(&mut w, assign, |mut w2, id, expr, is_first| {
                    if !is_first && use_new_lines { self.start_expr(w2)?; }
                    self.write_local_assignment(&mut w2, &id, &expr, false)
                })
            }
            Expression::Set(ReAssignment { assignment, globals }) => {
                let use_new_lines = !assignment.0.is_empty();
                let mut is_global_iter = globals.iter();
                for_each_assignment(&mut w, assignment, |mut w2, id, expr, is_first| {
                    if !is_first && use_new_lines { self.start_expr(w2)?; }
                    let is_global = is_global_iter.next().unwrap_or(&false);
                    self.write_local_assignment(&mut w2, &id, &expr, *is_global)
                })
            }
            Expression::Multi(exprs) | Expression::Group(exprs) => {
                let mut is_first = true;
                for expr in exprs {
                    if !is_first { self.start_expr(w)?; }
                    self.write_expr(w, expr)?;
                    is_first = false;
                }
                Ok(())
            }
            Expression::FunCall { name, args, is_wasm_fun, typ, fun_index } => {
                if let Err(e) = typ {
                    return self.error(e.reason.as_str(), e.pos);
                }
                let mut is_first = true;
                for arg in args {
                    if !is_first { self.start_expr(w)?; }
                    self.write_expr(w, arg)?;
                    is_first = false;
                }
                if !args.is_empty() {
                    self.start_expr(w)?;
                }
                if *is_wasm_fun {
                    // this is safe because we checked for errors above and
                    // WASM functions always have at least one argument.
                    let t = typ.as_ref().unwrap().ins.get(0).unwrap();
                    w.write_all(format!("({}.{})", t, name).as_bytes())
                } else {
                    w.write_all(b"(call $")?;
                    w.write_all(name.as_bytes())?;
                    if fun_index != &0 { w.write_all(format!("${}", fun_index).as_bytes())?; }
                    w.write_all(b")")
                }
            }
            Expression::ExprError(e) =>
                self.error(e.reason.as_str(), e.pos),
        }
    }

    fn write_fun(&mut self,
                 w: &mut Box<dyn Write>,
                 id: &str,
                 args: Option<Vec<String>>,
                 body: Option<Expression>,
                 typ: FnType,
                 vis: Visibility,
    ) -> Result<()> {
        w.write_all(b"(func $")?;
        w.write_all(id.as_bytes())?;
        if vis == Visibility::Public {
            w.write_all(b" (export \"")?;
            w.write_all(id.as_bytes())?;
            w.write_all(b"\")")?;
        }
        let mut err: Vec<_> = if let Some(a) = args {
            a.iter().zip(typ.ins).map(|(name, t)| {
                w.write_all(b" (param $")?;
                w.write_all(name.as_bytes())?;
                w.write_all(b" ")?;
                w.write_all(t.to_string().as_bytes())?;
                w.write_all(b")")
            }).filter(|r| r.is_err())
                .take(1).collect()
        } else {
            w.write_all(b" (param ")?;
            let types = typ.ins.iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            w.write_all(types.as_bytes())?;
            w.write_all(b")")?;
            vec![]
        };
        if !err.is_empty() {
            return err.remove(0);
        }
        if !typ.outs.is_empty() {
            w.write_all(b" (result")?;
            for out in typ.outs {
                w.write_all(b" ")?;
                w.write_all(out.to_string().as_bytes())?;
            }
            w.write_all(b")")?;
        }
        if let Some(b) = body {
            self.increase_ident();
            self.write_variables(w, &b)?;
            self.start_expr(w)?;
            self.write_expr(w, &b)?;
            self.decrease_ident();
            w.write_all(b"\n")?;
            w.write_all(self.ident.as_bytes())?;
        }
        w.write_all(b")")
    }

    fn write_ext(&mut self,
                 w: &mut Box<dyn Write>,
                 mod_name: &str,
                 defs: Vec<ExtDef>,
    ) -> Result<()> {
        let mut is_first = true;
        for def in defs {
            if !is_first {
                self.start_expr(w)?;
            }
            is_first = false;
            let type_to_write = match def.typ {
                Type::Empty | Type::WasmFn(_) => None,
                Type::Fn(fn_types) => {
                    let mut i = 0;
                    for fn_type in fn_types {
                        let name = if i == 0 {
                            def.id.clone()
                        } else {
                            self.start_expr(w)?;
                            format!("{}${}", def.id, i)
                        };
                        w.write_all(format!("(import \"{}\" \"{}\" ", mod_name, def.id).as_bytes())?;
                        self.write_fun(w, &format!("{}.{}", mod_name, name),
                                       None, None, fn_type, Visibility::Private)?;
                        w.write_all(b")")?;
                        i += 1;
                    }
                    None
                }
                Type::I64 => Some(b"i64"),
                Type::I32 => Some(b"i32"),
                Type::F64 => Some(b"f64"),
                Type::F32 => Some(b"f32"),
                Type::Error(e) => { return self.error(e.reason.as_str(), e.pos); }
            };
            if let Some(t) = type_to_write {
                w.write_all(format!("(global ${}.{} (import \"{}\" \"{}\") ",
                                    mod_name, &def.id, mod_name, &def.id).as_bytes())?;
                w.write_all(t)?;
                w.write_all(b")")?;
            }
        }
        Ok(())
    }

    fn start_expr(&self, w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(b"\n")?;
        w.write_all(self.ident.as_bytes())
    }

    fn start_texpr(w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(b"\n")?;
        w.write_all(ONE_IDENT.as_bytes())
    }

    fn error(&self, msg: &str, pos: (usize, usize)) -> Result<()> {
        let (row, col) = pos;
        Err(std::io::Error::new(
            ErrorKind::Other,
            format!("{}[{},{}]: {}\n", self.mod_name, row, col, msg)))
    }
}

impl WasminSink for Wat {
    fn start(&mut self, module_name: String, w: &mut Box<dyn Write>) -> Result<()> {
        self.mod_name = module_name;
        w.write_all(b"(module\n")
    }

    fn receive(&mut self, element: TopLevelElement, mut w: &mut Box<dyn Write>) -> Result<()> {
        Wat::start_texpr(w)?;
        match element {
            TopLevelElement::Let(assign, vis) => {
                for_each_assignment(&mut w, &assign, |mut w2, id, expr, _| {
                    self.write_global_assignment(&mut w2, &id, &expr, &vis, false)?;
                    w2.write_all(b"\n")
                })
            }
            TopLevelElement::Mut(assign, vis) => {
                for_each_assignment(&mut w, &assign, |mut w2, id, expr, _| {
                    self.write_global_assignment(&mut w2, &id, &expr, &vis, true)?;
                    w2.write_all(b"\n")
                })
            }
            TopLevelElement::Fn((id, args, body, typ), vis) => {
                self.write_fun(w, id.as_str(), Some(args), Some(body), typ, vis)
            }
            TopLevelElement::Ext(mod_name, defs, ..) => {
                self.write_ext(w, mod_name.as_str(), defs)
            }
            TopLevelElement::Error(e, pos) =>
                self.error(e.as_str(), pos)
        }
    }

    fn flush(&mut self, w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(b"\n)\n")
    }
}
