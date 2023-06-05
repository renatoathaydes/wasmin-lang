use std::fmt::{Display, Formatter};

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

impl Token {
    pub fn pos(&self) -> Position {
        match self {
            Token::Let(pos, ..) |
            Token::Set(pos, ..) |
            Token::Mut(pos, ..) |
            Token::Pub(pos, ..) |
            Token::Ext(pos, ..) |
            Token::Colon(pos, ..) |
            Token::SemiColon(pos, ..) |
            Token::Comma(pos, ..) |
            Token::OpenParens(pos, ..) |
            Token::CloseParens(pos, ..) |
            Token::OpenBracket(pos, ..) |
            Token::CloseBracket(pos, ..) |
            Token::OpenCurly(pos, ..) |
            Token::CloseCurly(pos, ..) |
            Token::Eq(pos, ..) |
            Token::Comment(pos, ..) |
            Token::Str(pos, ..) |
            Token::Id(pos, ..) |
            Token::Number(pos, ..) |
            Token::Error(pos, ..) => *pos,
        }
    }

    /// Whether this token represents one of the special characters and is equal to the given c.
    pub fn is_char(&self, c: char) -> bool {
        match self {
            Token::Colon(_) => c == ':',
            Token::SemiColon(_) => c == ';',
            Token::Comma(_) => c == ',',
            Token::OpenParens(_) => c == '(',
            Token::CloseParens(_) => c == ')',
            Token::OpenBracket(_) => c == '[',
            Token::CloseBracket(_) => c == ']',
            Token::OpenCurly(_) => c == '{',
            Token::CloseCurly(_) => c == '}',
            Token::Eq(_) => c == '=',
            _ => false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Let(_, ..) => write!(f, "let"),
            Token::Set(_, ..) => write!(f, "set"),
            Token::Mut(_, ..) => write!(f, "mut"),
            Token::Pub(_, ..) => write!(f, "pub"),
            Token::Ext(_, ..) => write!(f, "ext"),
            Token::Colon(_, ..) => write!(f, "':'"),
            Token::SemiColon(_, ..) => write!(f, "';'"),
            Token::Comma(_, ..) => write!(f, "','"),
            Token::OpenParens(_, ..) => write!(f, "'('"),
            Token::CloseParens(_, ..) => write!(f, "')'"),
            Token::OpenBracket(_, ..) => write!(f, "'['"),
            Token::CloseBracket(_, ..) => write!(f, "']'"),
            Token::OpenCurly(_, ..) => write!(f, "'{{'"),
            Token::CloseCurly(_, ..) => write!(f, "'}}'"),
            Token::Eq(_, ..) => write!(f, "'='"),
            Token::Comment(_, ..) => write!(f, "## comment ##"),
            Token::Str(_, s) => write!(f, "\"{}\"", s),
            Token::Id(_, id) => write!(f, "{}", id),
            Token::Number(_, n) => write!(f, "{}", n),
            Token::Error(_, err) => write!(f, "ERROR:{}", err),
        }
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Numeric::I32(n) => write!(f, "{}i32", n),
            Numeric::I64(n) => write!(f, "{}i64", n),
            Numeric::F32(n) => write!(f, "{}f32", n),
            Numeric::F64(n) => write!(f, "{}f64", n),
        }
    }
}