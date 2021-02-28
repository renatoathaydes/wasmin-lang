use std::io::Write;
use std::iter::repeat;

use crate::ast::{Assignment, Expression, ExtDef, ReAssignment, TopLevelElement, Visibility};
use crate::errors::Result;
use crate::sink::{expr_to_vec, sanitize_number, WasminSink};
use crate::types::{FunType, Type};

const ONE_IDENT: &str = "  ";

pub struct Wat {
    mod_name: String,
    ident: String,
    block_level: usize,
}

impl Default for Wat {
    fn default() -> Self {
        Wat {
            mod_name: String::from(""),
            ident: ONE_IDENT.to_owned(),
            block_level: 0,
        }
    }
}

impl Wat {
    fn increase_ident(&mut self) {
        self.ident.push_str(ONE_IDENT);
    }

    fn decrease_ident(&mut self) {
        (0..ONE_IDENT.len()).for_each(|_| { self.ident.remove(0); });
    }

    fn start_block(&mut self) {
        self.block_level += 1;
    }

    fn end_block(&mut self) {
        self.block_level -= 1;
    }

    fn write_global_assignment(
        &mut self,
        mut w: &mut Box<dyn Write>,
        assignment: &Assignment,
        vis: &Visibility,
        is_mut: bool,
    ) -> Result<()> {
        let (names, value, type_conversion) = assignment;
        let values = expr_to_vec(*value.clone());

        // if all values have value types, write globals directly
        if values.iter().all(|v| v.get_value_type().is_some()) {
            for (name, expr) in names.iter().zip(values.iter()) {
                let typ = expr.get_value_type().unwrap();
                let type_str = if is_mut {
                    format!("(mut {})", typ.to_string())
                } else {
                    typ.to_string()
                };

                self.start_expr(w)?;
                w.write_all(b"(global $")?;
                w.write_all(name.as_bytes())?;
                w.write_all(b" ")?;
                w.write_all(type_str.as_bytes())?;
                w.write_all(b" ")?;
                if vis == &Visibility::Public {
                    w.write_all(b"(export \"")?;
                    w.write_all(name.as_bytes())?;
                    w.write_all(b"\") ")?;
                }
                self.write_expr(&mut w, expr)?;
                w.write_all(b")")?
            }
            Ok(())
        } else {
            err_wasmin!(werr_unsupported_feature!(
                "Non-constant global variables are not supported yet.", (0, 0)))
        }
    }

    fn write_local_assignment(
        &mut self,
        w: &mut Box<dyn Write>,
        assignment: &Assignment,
        is_global: &[bool],
    ) -> Result<()> {
        let (names, value, type_conversion) = assignment;
        self.write_expr(w, value.as_ref())?;
        for (name, global) in names.iter().rev().zip(is_global.iter().rev()) {
            self.start_expr(w)?;
            if *global {
                w.write_all(b"global.set $")?;
            } else {
                w.write_all(b"local.set $")?;
            };
            w.write_all(name.as_bytes())?;
        }
        Ok(())
    }

    fn write_variables(
        &mut self,
        mut w: &mut Box<dyn Write>,
        expr: &Expression,
    ) -> Result<()> {
        match expr {
            Expression::Let(assignment) | Expression::Mut(assignment) => {
                let (names, value, type_conversion) = assignment;
                for (name, (typ, new_type)) in names.iter()
                    .zip(value.get_type().iter().zip(type_conversion.iter())) {
                    let t = if let Some(t) = new_type { t } else { typ };
                    self.start_expr(&mut w)?;
                    w.write_all(b"(local $")?;
                    w.write_all(name.as_bytes())?;
                    w.write_all(b" ")?;
                    w.write_all(t.to_string().as_bytes())?;
                    w.write_all(b")")?;
                }
            }
            Expression::Group(exprs) => {
                for expr in exprs {
                    self.write_variables(&mut w, expr)?;
                }
            }
            Expression::If(cond, then, els) => {
                self.write_variables(&mut w, cond)?;
                self.write_variables(&mut w, then)?;
                self.write_variables(&mut w, els)?;
            }
            Expression::Loop { expr, .. } => {
                self.write_variables(&mut w, expr)?;
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
                w.write_all(typ.to_string().as_bytes())?;
                w.write_all(b".const ")?;
                w.write_all(sanitize_number(id.as_str()).as_bytes())?;
                Ok(())
            }
            Expression::Global(id, _) => {
                w.write_all(b"global.get $")?;
                w.write_all(id.as_bytes())?;
                Ok(())
            }
            Expression::If(cond, then, els) => {
                self.write_expr(w, cond)?;
                self.start_expr(w)?;
                w.write_all(b"(if ")?;
                if !then.get_type().is_empty() {
                    w.write_all(b"(result ")?;
                    w.write_all(then.get_type().iter().map(|t| t.to_string())
                        .collect::<Vec<_>>().join(" ").as_bytes())?;
                    w.write_all(b") ")?;
                }
                w.write_all(b"(then")?;
                self.increase_ident();
                self.start_expr(w)?;
                self.write_expr(w, then)?;
                self.decrease_ident();
                self.start_expr(w)?;
                w.write_all(b")")?;
                if els.is_empty() {
                    w.write_all(b" (else)")?;
                } else {
                    w.write_all(b" (else")?;
                    self.increase_ident();
                    self.start_expr(w)?;
                    self.write_expr(w, els)?;
                    self.decrease_ident();
                    self.start_expr(w)?;
                    w.write_all(b")")?;
                }
                w.write_all(b")")?;
                Ok(())
            }
            Expression::Loop { expr, .. } => {
                self.start_expr(w)?;
                w.write_all(b"(block $block")?;
                self.start_block();
                self.increase_ident();
                w.write_fmt(format_args!("{}", self.block_level))?;
                self.start_expr(w)?;
                w.write_all(b"(loop")?;
                self.increase_ident();
                self.start_expr(w)?;
                self.write_expr(w, expr)?;
                self.start_expr(w)?;
                w.write_all(b"br 0")?;
                self.decrease_ident();
                self.start_expr(w)?;
                w.write_all(b")")?;
                self.decrease_ident();
                self.end_block();
                self.start_expr(w)?;
                w.write_all(b")")?;
                Ok(())
            }
            Expression::Br(_) => {
                w.write_all(b"br $block")?;
                w.write_fmt(format_args!("{}", self.block_level))?;
                Ok(())
            }
            Expression::Local(id, _) => {
                w.write_all(b"local.get $")?;
                w.write_all(id.as_bytes())?;
                Ok(())
            }
            Expression::Let(assign) | Expression::Mut(assign) => {
                let globals: Vec<_> = repeat(false).take(assign.0.len()).collect();
                self.write_local_assignment(&mut w, &assign, &globals)
            }
            Expression::Set(ReAssignment { assignment, globals }) => {
                self.write_local_assignment(&mut w, &assignment, globals)
            }
            Expression::Group(exprs) => {
                let mut is_first = true;
                for expr in exprs {
                    if !is_first { self.start_expr(w)?; }
                    self.write_expr(w, expr)?;
                    is_first = false;
                }
                Ok(())
            }
            Expression::FunCall { name, is_wasm_fun, typ, fun_index } => {
                if let Err(e) = typ {
                    return err_wasmin!(werr_type!(e.reason, e.pos));
                }
                if *is_wasm_fun {
                    // this is safe because we checked for errors above and
                    // WASM functions always have at least one argument.
                    let t = typ.as_ref().unwrap().ins.get(0).unwrap();
                    w.write_all(format!("{}.{}", t, name).as_bytes())?;
                } else {
                    w.write_all(b"call $")?;
                    w.write_all(name.as_bytes())?;
                    if fun_index != &0 { w.write_all(format!("${}", fun_index).as_bytes())?; }
                }
                Ok(())
            }
            Expression::ExprError(e) =>
                err_wasmin!(werr_type!(e.reason, e.pos))
        }
    }

    fn write_fun(&mut self,
                 w: &mut Box<dyn Write>,
                 id: &str,
                 args: Option<Vec<String>>,
                 body: Option<Expression>,
                 typ: FunType,
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
            err.remove(0)?;
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
        w.write_all(b")")?;
        Ok(())
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
                    for (i, fn_type) in fn_types.into_iter().enumerate() {
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
                    }
                    None
                }
                Type::I64 => Some(b"i64"),
                Type::I32 => Some(b"i32"),
                Type::F64 => Some(b"f64"),
                Type::F32 => Some(b"f32"),
                Type::Error(e) => { return err_wasmin!(werr_type!(e.reason, e.pos)); }
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
        w.write_all(self.ident.as_bytes())?;
        Ok(())
    }

    fn start_texpr(w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(b"\n")?;
        w.write_all(ONE_IDENT.as_bytes())?;
        Ok(())
    }
}

impl WasminSink<()> for Wat {
    fn start(&mut self, module_name: String, w: &mut Box<dyn Write>) -> Result<()> {
        self.mod_name = module_name;
        w.write_all(b"(module\n")?;
        Ok(())
    }

    fn receive(&mut self, element: TopLevelElement, mut w: &mut Box<dyn Write>, _: &mut ()) -> Result<()> {
        Wat::start_texpr(w)?;
        match element {
            TopLevelElement::Let(assign, vis, _) => {
                self.write_global_assignment(&mut w, &assign, &vis, false)?;
            }
            TopLevelElement::Mut(assign, vis, _) => {
                self.write_global_assignment(&mut w, &assign, &vis, true)?;
            }
            TopLevelElement::Fun((id, args, body, typ), vis, _) => {
                self.write_fun(w, id.as_str(), Some(args), Some(body), typ, vis)?;
            }
            TopLevelElement::Ext(mod_name, defs, ..) => {
                self.write_ext(w, mod_name.as_str(), defs)?;
            }
            TopLevelElement::Error(e) => return err_wasmin!(e)
        };
        Ok(())
    }

    fn flush(&mut self, w: &mut Box<dyn Write>, _: ()) -> Result<()> {
        w.write_all(b"\n)\n")?;
        Ok(())
    }
}
