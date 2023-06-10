use std::collections::HashMap;

use crate::ast::{Assignment, AST, Comment, Constant, Expression, ExprType, FunKind, IdKind, TopLevelElement, Type, Visibility};
use crate::errors::WasminError;
use crate::interner::{InternedStr, Interner};
use crate::parse::lex::Lexer;
use crate::parse::model::{Position, Token};
use crate::wasm::WASM;

pub struct Parser<'s> {
    pub(crate) wasm: WASM,
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
            wasm: WASM::new(),
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

    pub(crate) fn lookup_fun_types(&self, name: &str, interned_name: &InternedStr) -> Option<Vec<(&ExprType, FunKind)>> {
        match self.wasm.lookup_wasm_fun_type(name) {
            Some(types) => Some(types.iter().map(|t| (t, FunKind::Wasm)).collect()),
            None => {
                let typ = self.scope.last().expect("scope stack must not be empty").get(interned_name);
                match typ {
                    None => None,
                    Some(Type::Fn(t)) => {
                        let args_count = t.ins.len();
                        Some(vec![(t, FunKind::Local { args_count })])
                    }
                    Some(_) => None,
                }
            }
        }
    }

    pub(crate) fn lookup_id_kind(&self, name: &str, interned_name: &InternedStr, pos: Position)
                                 -> Result<IdKind, WasminError> {
        match self.wasm.lookup_wasm_fun_type(name) {
            Some(_) => Ok(IdKind::Fun),
            None => {
                let typ = self.scope.last().expect("scope stack must not be empty").get(interned_name);
                match typ {
                    None => Err(WasminError::TypeError {
                        cause: format!("no identifier '{}' could be find in this scope", name),
                        pos,
                    }),
                    Some(Type::Fn(..)) => Ok(IdKind::Fun),
                    Some(t) => Ok(IdKind::Var(t.clone())),
                }
            }
        }
    }
}

