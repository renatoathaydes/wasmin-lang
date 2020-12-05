use crate::types::Type;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    Empty,
    Const(String, Type),
    Let(String, Type),
    Mut(String, Type),
    FnCall { name: String, args: Vec<Expression>, typ: Type },
    Err(Type),
}

impl Expression {
    pub fn fnCall(name: &str, args: Vec<Expression>, typ: Type) -> Expression {
        Expression::FnCall { name: name.to_string(), args, typ }
    }
}
