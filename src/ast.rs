use Expression::{*};

use crate::types::{FnType, Type, TypeError};

pub type Assignment = (Vec<String>, Vec<Expression>);

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    Empty,
    Const(String, Type),
    Let(Assignment),
    Mut(Assignment),
    Group(Vec<Expression>),
    Multi(Vec<Expression>),
    FnCall { name: String, args: Vec<Expression>, typ: Result<FnType, TypeError> },
    ExprError(TypeError),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Visibility {
    Public,
    Private,
    Internal,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum TopLevelExpression {
    Const(String, Type, Visibility),
    Let(Assignment, Visibility),
    Mut(Assignment, Visibility),
    Fn(String, Expression, Visibility),
}

impl Expression {
    pub fn fn_call(name: &str, args: Vec<Expression>, typ: Result<FnType, TypeError>) -> Expression {
        FnCall { name: name.to_string(), args, typ }
    }

    pub fn get_type(&self) -> Vec<Type> {
        match self {
            Expression::Empty | Let(..) | Mut(..) => Vec::new(),
            Const(.., typ) => vec![typ.clone()],
            Group(es) => es.last()
                .map_or(Vec::new(), |e| e.get_type()),
            Multi(es) => es.iter()
                .flat_map(|e| e.get_type()).collect(),
            FnCall { typ, .. } => match typ {
                Ok(t) => t.get_type(),
                Err(e) => e.get_type(),
            },
            ExprError(t) => t.get_type(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Expression::Empty => true,
            Const(..) | Let(..) | Mut(..) | Multi(_) | FnCall { .. } | ExprError(..) => false,
            Group(es) => es.last()
                .map_or(true, |e| e.is_empty()),
        }
    }

    pub fn into_multi(self) -> Vec<Expression> {
        match self {
            Expression::Empty => vec![],
            Let(..) | Mut(..) | Const(..) | FnCall { .. } | ExprError(..) | Group(..) => vec![self],
            Multi(mut es) => es.drain(..).flat_map(|e| e.into_multi()).collect(),
        }
    }
}
