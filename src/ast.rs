use Expression::{*};

use crate::types::Type;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    Empty,
    Const(String, Type),
    Let(String, Type),
    Mut(String, Type),
    Group(Vec<Expression>),
    Multi(Vec<Expression>),
    FnCall { name: String, args: Vec<Expression>, typ: Vec<Type> },
    ExprError(Type),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Visibility {
    Public,
    Private,
    Internal,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum TopLevelExpression {
    Def(String, Type, Visibility),
    Const(String, Type),
    Let(String, Type),
    Mut(String, Type),
    Fn(String, Type),
}

impl Expression {
    pub fn fn_call(name: &str, args: Vec<Expression>, typ: Vec<Type>) -> Expression {
        FnCall { name: name.to_string(), args, typ }
    }

    pub fn get_type_owned(&self) -> Vec<Type> {
        self.get_type().drain(..).map(|t| t.clone()).collect()
    }

    pub fn get_type(&self) -> Vec<&Type> {
        match self {
            Expression::Empty | Let(..) | Mut(..) => Vec::new(),
            Const(.., typ) => vec![typ],
            Group(es) => es.last()
                .map_or(Vec::new(), |e| e.get_type()),
            Multi(es) => es.iter()
                .flat_map(|e| e.get_type()).collect(),
            FnCall { typ, .. } => typ.iter().collect(),
            ExprError(typ) => vec![typ],
        }
    }
}
