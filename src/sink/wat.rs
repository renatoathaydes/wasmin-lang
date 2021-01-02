use std::io::{ErrorKind, Result, Write};

use crate::ast::{Expression, TopLevelExpression, Visibility};
use crate::sink::{for_each_assignment, sanitize_number, WasminSink};

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
        is_global: bool,
    ) -> Result<()> {
        let def = if is_global {
            format!("(global ${} {} ", id, expr.get_type().get(0).unwrap().to_string())
        } else {
            format!("(local.set ${} ", id)
        };
        w.write_all(def.as_bytes())?;
        if vis == &Visibility::Public {
            w.write_all(b"(export \"")?;
            w.write_all(id.as_bytes())?;
            w.write_all(b"\") ")?;
        }
        self.write_expr(&mut w, &expr)?;
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
            Expression::Let(assign) => {
                let use_new_lines = !assign.0.is_empty();
                for_each_assignment(&mut w, assign, |mut w2, id, expr, is_first| {
                    if !is_first && use_new_lines { self.start_expr(w2)?; }
                    self.write_assignment(&mut w2, &id, &expr, &Visibility::Private, false)
                })
            }
            Expression::Mut(assign) => {
                let use_new_lines = !assign.0.is_empty();
                for_each_assignment(&mut w, assign, |mut w2, id, expr, is_first| {
                    if !is_first && use_new_lines { self.start_expr(w2)?; }
                    self.write_assignment(&mut w2, &id, &expr, &Visibility::Private, false)
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
            Expression::FunCall { name, args, is_wasm_fun, typ } => {
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
                    w.write_all(b")")
                }
            }
            Expression::ExprError(e) =>
                self.error(e.reason.as_str(), e.pos),
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

    fn receive(&mut self, expr: TopLevelExpression, mut w: &mut Box<dyn Write>) -> Result<()> {
        Wat::start_texpr(w)?;
        match expr {
            TopLevelExpression::Let(assign, vis) => {
                for_each_assignment(&mut w, &assign, |mut w2, id, expr, _| {
                    self.write_assignment(&mut w2, &id, &expr, &vis, true)?;
                    w2.write_all(b"\n")
                })
            }
            TopLevelExpression::Mut(assign, vis) => {
                for_each_assignment(&mut w, &assign, |mut w2, id, expr, _| {
                    self.write_assignment(&mut w2, &id, &expr, &vis, true)?;
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
                self.write_variables(w, &body)?;
                self.start_expr(w)?;
                self.write_expr(w, &body)?;
                self.decrease_ident();
                w.write_all(b"\n")?;
                w.write_all(self.ident.as_bytes())?;
                w.write_all(b")\n")
            }
            TopLevelExpression::Error(e, pos) =>
                self.error(e.as_str(), pos)
        }
    }

    fn flush(&mut self, w: &mut Box<dyn Write>) -> Result<()> {
        w.write_all(format!(")\n").as_bytes())
    }
}
