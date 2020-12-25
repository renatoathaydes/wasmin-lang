use std::io::{ErrorKind, Result, Write};

use crate::ast::{Expression, TopLevelExpression, Visibility};
use crate::sink::{for_each_assignment, WasminSink};

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

    fn write_assignment(
        &mut self,
        mut w: &mut Box<dyn Write>,
        id: &String,
        expr: &Expression,
        vis: &Visibility,
        is_mut: bool,
        is_global: bool,
    ) -> Result<()> {
        w.write_all(if is_global { b"(global.set $" } else { b"(local.set $" })?;
        w.write_all(id.as_bytes())?;
        if vis == &Visibility::Public {
            w.write_all(b" (export \"")?;
            w.write_all(id.as_bytes())?;
            w.write_all(b"\")")?;
        }
        for typ in expr.get_type() {
            w.write_all(format!(" ({}{}) ", if is_mut { "mut " } else { "" }, typ).as_bytes())?;
        }
        self.write_expr(&mut w, &expr)?;
        w.write_all(b")")
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
                w.write_all(id.as_bytes())?;
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
            Expression::Let(assign) => {
                let use_new_lines = !assign.0.is_empty();
                for_each_assignment(&mut w, assign, |mut w2, id, expr, is_first| {
                    if !is_first && use_new_lines { self.start_expr(w2)?; }
                    self.write_assignment(&mut w2, &id, &expr, &Visibility::Private, false, false)
                })
            }
            Expression::Mut(assign) => {
                let use_new_lines = !assign.0.is_empty();
                for_each_assignment(&mut w, assign, |mut w2, id, expr, is_first| {
                    if !is_first && use_new_lines { self.start_expr(w2)?; }
                    self.write_assignment(&mut w2, &id, &expr, &Visibility::Private, true, false)
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
            Expression::FunCall { name, args, .. } => {
                let mut is_first = true;
                for arg in args {
                    if !is_first { self.start_expr(w)?; }
                    self.write_expr(w, arg)?;
                    is_first = false;
                }
                if !args.is_empty() {
                    self.start_expr(w)?;
                }
                w.write_all(b"(call $")?;
                w.write_all(name.as_bytes())?;
                w.write_all(b")")
            }
            Expression::ExprError(_) => Ok(()),
        }
    }

    fn start_expr(&self, w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(b"\n")?;
        w.write_all(self.ident.as_bytes())
    }

    fn start_texpr(w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(b"\n")?;
        w.write_all(ONE_IDENT.as_bytes())
    }
}

impl WasminSink for Wat {
    fn start(&mut self, module_name: String, w: &mut Box<dyn Write>) -> Result<()> {
        self.mod_name = module_name;
        w.write_all(b"(module\n")
    }

    fn receive(&mut self, expr: TopLevelExpression, mut w: &mut Box<dyn Write>) -> Result<()> {
        Wat::start_texpr(w)?;
        match expr {
            TopLevelExpression::Let(assign, vis) => {
                for_each_assignment(&mut w, &assign, |mut w2, id, expr, _| {
                    self.write_assignment(&mut w2, &id, &expr, &vis, true, true)?;
                    w2.write_all(b"\n")
                })
            }
            TopLevelExpression::Mut(assign, vis) => {
                for_each_assignment(&mut w, &assign, |mut w2, id, expr, _| {
                    self.write_assignment(&mut w2, &id, &expr, &vis, true, true)?;
                    w2.write_all(b"\n")
                })
            }
            TopLevelExpression::Fn((id, args, body, typ), vis) => {
                w.write_all(b"(func $")?;
                w.write_all(id.as_bytes())?;
                if vis == Visibility::Public {
                    w.write_all(b" (export \"")?;
                    w.write_all(id.as_bytes())?;
                    w.write_all(b"\")")?;
                }
                let mut err: Vec<_> = args.iter().zip(typ.ins).map(|(name, t)| {
                    w.write_all(b" (param $")?;
                    w.write_all(name.as_bytes())?;
                    w.write_all(b" ")?;
                    w.write_all(t.to_string().as_bytes())?;
                    w.write_all(b")")
                }).filter(|r| r.is_err())
                    .take(1).collect();
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
                self.increase_ident();
                self.start_expr(w)?;
                self.write_expr(w, &body)?;
                self.decrease_ident();
                w.write_all(b"\n")?;
                w.write_all(self.ident.as_bytes())?;
                w.write_all(b")\n")
            }
            TopLevelExpression::Error(e, (row, col)) =>
                Err(std::io::Error::new(
                    ErrorKind::Other,
                    format!("ERROR: {}[{},{}]: {}\n", self.mod_name, row, col, e)))
        }
    }

    fn flush(&mut self, w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(format!(")\n").as_bytes())
    }
}
