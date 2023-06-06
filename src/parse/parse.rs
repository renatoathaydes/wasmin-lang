use crate::ast::{Assignment, AST, Comment, Constant, Expression, TopLevelElement, Type, Visibility};
use crate::errors::WasminError;
use crate::interner::{InternedStr, Interner};
use crate::parse::lex::Lexer;
use crate::parse::model::{Position, Token};

pub struct Parser<'s> {
    pub(crate) lexer: Lexer<'s>,
    pub(crate) ast: AST,
    comments: Vec<Comment>,
    stack: Vec<Type>,
}

impl<'s> Parser<'s> {
    pub fn new(text: &str) -> Parser {
        Parser { lexer: Lexer::new(text), ast: AST::new(), comments: vec![], stack: vec![] }
    }

    pub fn new_with_ast(text: &str, ast: AST) -> Parser {
        Parser { lexer: Lexer::new(text), ast, comments: vec![], stack: vec![] }
    }

    pub fn parse_next(&mut self) -> Option<TopLevelElement> {
        if let Some(token) = self.lexer.next() {
            let expr = match token {
                Token::Let(pos) => self.parse_let(pos),
                Token::Fun(pos) => self.parse_fun(pos),
                _ => {
                    TopLevelElement::Error(WasminError::UnsupportedFeatureError {
                        cause: "Only let and fun are supported for now".into(),
                        pos: self.lexer.pos(),
                    })
                }
            };
            Some(expr)
        } else {
            None
        }
    }
}

