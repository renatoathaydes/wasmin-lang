pub type Position = usize;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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
    Error(Position, String)
}
