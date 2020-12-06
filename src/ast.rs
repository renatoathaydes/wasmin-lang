use crate::types::Type;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    Empty,
    Const(String, Type),
    Let(String, Type),
    Mut(String, Type),
    Group(Vec<Expression>),
    FnCall { name: String, args: Vec<Expression>, typ: Vec<Type> },
    Err(Type),
}

impl Expression {
    pub fn

    fn_call(name: &str, args: Vec<Expression>, typ: Vec<Type>) -> Expression {
        Expression::FnCall { name: name.to_string(), args, typ }
    }

    pub fn get_type(&self) -> Vec<&Type> {
        match self {
            Expression::Empty => vec![&Type::Empty],
            Expression::Const(_, typ) => vec![typ],
            Expression::Let(_, _) => Vec::new(),
            Expression::Mut(_, _) => Vec::new(),
            Expression::Group(es) => es.last()
                .map_or(Vec::new(), |e| e.get_type()),
            Expression::FnCall { typ, .. } => typ.iter().collect(),
            Expression::Err(typ) => vec![typ],
        }
    }
}
