use crate::ast::{Expression, TopLevelExpression, Visibility};
use crate::sink::{for_each_assignment, WasminSink};
use crate::types::Type;

const ONE_IDENT: &str = "  ";

pub struct Wat {
    ident: String
}

impl Default for Wat {
    fn default() -> Self {
        Wat { ident: ONE_IDENT.to_owned() }
    }
}

impl Wat {
    fn increase_ident(&mut self) {
        self.ident.push_str(ONE_IDENT);
    }

    fn decrease_ident(&mut self) {
        (0..ONE_IDENT.len()).for_each(|_| { self.ident.remove(0); });
    }

    fn create_assignment(
        &mut self,
        id: &String,
        expr: &Expression,
        vis: &Visibility,
        is_mut: bool,
        is_global: bool,
    ) -> Result<String, i32> {
        let ident = self.ident.clone();
        Ok(format!("{}({} ${}{}{} {})", ident,
                   if is_global { "global" } else { "local" },
                   id,
                   if vis == &Visibility::Public { format!(" (export {})", id) } else { "".to_owned() },
                   format!("{}{}",
                           if is_mut { "" } else { "" },
                           expr.get_type().first().unwrap_or(&Type::Empty)),
                   self.create_expr(expr)?))
    }

    fn create_expr(
        &mut self,
        expr: &Expression,
    ) -> Result<String, i32> {
        Ok("expr".to_owned())
    }
}

impl WasminSink for Wat {
    fn start(&mut self, module_name: String) -> Vec<u8> {
        format!("(module {}\n", module_name).into_bytes()
    }

    fn receive(&mut self, expr: TopLevelExpression) -> Result<Vec<u8>, i32> {
        let expr_str = match expr {
            TopLevelExpression::Let(assign, vis) => {
                let assigns = for_each_assignment::<_, String, i32>(assign, |id, expr|
                    self.create_assignment(id, expr, &vis, false, true),
                )?;
                let mut text = String::new();
                for a in assigns {
                    text.push_str(&a);
                }
                text
            }
            TopLevelExpression::Mut(_, _) => "".to_owned(),
            TopLevelExpression::Fn(_, _) => "".to_owned(),
            TopLevelExpression::Error(_, _) => "".to_owned(),
        };
        Ok(expr_str.into_bytes())
    }

    fn flush(&mut self) -> Result<Vec<u8>, i32> {
        Ok(format!(")\n").into_bytes())
    }
}
