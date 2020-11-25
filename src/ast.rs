#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type {
    I64, I32, F64, F32
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    Empty,
    Const(String, Type)
}
