use std::collections::HashMap;

use crate::ast::{Assignment, AST, Comment, Constant, Expression, ExprType, TopLevelElement, Type, Visibility};
use crate::errors::WasminError;
use crate::interner::{InternedStr, Interner};
use crate::parse::lex::Lexer;
use crate::parse::model::{Position, Token};

pub struct Parser<'s> {
    pub(crate) lexer: Lexer<'s>,
    pub(crate) ast: AST,
    pub(crate) comments: Vec<Comment>,
    pub(crate) stack: Vec<Type>,
    pub(crate) scope: Vec<HashMap<InternedStr, Type>>,
}

impl<'s> Parser<'s> {
    pub fn new(text: &str) -> Parser {
        Parser::new_with_ast(text, AST::new())
    }

    pub fn new_with_ast(text: &str, ast: AST) -> Parser {
        Parser {
            lexer: Lexer::new(text),
            ast,
            comments: vec![],
            stack: vec![],
            scope: vec![HashMap::with_capacity(4)],
        }
    }

    pub fn parse_next(&mut self) -> Option<TopLevelElement> {
        if let Some(token) = self.lexer.next() {
            let (token, visibility) = if let Token::Pub(..) = token {
                let token = if let Some(token) = self.lexer.next() {
                    token
                } else {
                    return Some(TopLevelElement::Error(WasminError::SyntaxError {
                        pos: token.pos(),
                        cause: "expected declaration after 'pub' but got nothing".into(),
                    }));
                };
                (token, Visibility::Public)
            } else {
                (token, Visibility::Private)
            };
            let expr = match token {
                Token::Let(pos) => self.parse_let(pos, visibility),
                Token::Fun(pos) => self.parse_fun(pos, visibility),
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

    pub(crate) fn lookup_type(&self, name: &InternedStr, pos: Position) -> Type {
        let typ = self.scope.last().expect("scope stack must not be empty").get(name);
        let name_string = self.ast.interned_str(name);
        match typ {
            None => Type::Error(WasminError::TypeError {
                cause: format!("no identifier '{}' could be find in this scope", name_string),
                pos,
            }),
            Some(t) => t.clone()
        }
    }
}

