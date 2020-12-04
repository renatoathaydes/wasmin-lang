use crate::types::Type;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct FnInvocation {
    pub name: String,
    pub args: Vec<Expression>,
    pub typ: Type,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    Empty,
    Const(String, Type),
    Let(String, Type),
    Mut(String, Type),
    FnCall(Box<FnInvocation>),
}
