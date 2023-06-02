pub type Position = usize;

#[derive(Debug, PartialEq, Clone)]
pub enum Numeric {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Let(Position),
    Set(Position),
    Mut(Position),
    Pub(Position),
    Ext(Position),
    Colon(Position),
    SemiColon(Position),
    Comma(Position),
    OpenParens(Position),
    CloseParens(Position),
    OpenBracket(Position),
    CloseBracket(Position),
    OpenCurly(Position),
    CloseCurly(Position),
    Eq(Position),
    Comment(Position, String),
    Str(Position, String),
    Id(Position, String),
    Number(Position, Numeric),
    Error(Position, String),
}
