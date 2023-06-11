use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::iter::{empty, once};

use crate::ast::{Assignment, AST, Comment, Constant, Expression, ExprType, FunKind, IdKind, TopLevelElement, Type, Visibility};
use crate::errors::WasminError;
use crate::interner::{InternedStr, Interner};
use crate::parse::lex::Lexer;
use crate::parse::model::{Position, Token};
use crate::wasm::WASM;

pub type Scope = HashMap<InternedStr, HashSet<Type>>;

pub struct Parser<'s> {
    pub(crate) wasm: WASM,
    pub(crate) lexer: Lexer<'s>,
    pub(crate) ast: AST,
    pub(crate) comments: Vec<Comment>,
    pub(crate) stack: Vec<Type>,
    scope: Vec<Scope>,
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

    pub(crate) fn current_scope(&self) -> &Scope {
        self.scope.last().expect("there must be a root scope")
    }

    pub(crate) fn current_scope_mut(&mut self) -> &mut Scope {
        self.scope.last_mut().expect("there must be a root scope")
    }

    pub(crate) fn insert_type_in_scope(&mut self, name: &InternedStr, typ: Type) {
        if let Some(mut types) = self.current_scope_mut().get_mut(name) {
            types.insert(typ);
        } else {
            let mut types = HashSet::with_capacity(2);
            types.insert(typ);
            self.current_scope_mut().insert(name.clone(), types);
        }
    }

    pub(crate) fn enter_scope(&mut self) {
        self.scope.push(HashMap::with_capacity(4));
    }

    pub(crate) fn exit_scope(&mut self) -> Scope {
        if self.scope.len() < 2 {
            panic!("Attempt to exit root scope")
        }
        self.scope.pop().unwrap()
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
                let types = self.current_scope().get(interned_name);
                match types {
                    None => None,
                    Some(typ) => {
                        let fun_types: Vec<_> = typ.iter().filter(|t| match t {
                            Type::FunType(_) => true,
                            _ => false,
                        }).map(|t| match t {
                            Type::FunType(expr) => (expr, FunKind::Custom),
                            _ => panic!("")
                        }).collect();
                        Some(fun_types)
                    }
                }
            }
        }
    }

    pub(crate) fn lookup_id_kind(&self, name: &str, interned_name: &InternedStr, pos: Position)
                                 -> Result<IdKind, WasminError> {
        match self.wasm.lookup_wasm_fun_type(name) {
            Some(_) => Ok(IdKind::Fun),
            None => {
                let typ = self.current_scope().get(interned_name);
                match typ {
                    None => Err(WasminError::TypeError {
                        cause: format!("no identifier '{}' could be find in this scope", name),
                        pos,
                    }),
                    Some(types) => {
                        let t = types.iter().next().expect("type Set must not be empty");
                        match t {
                            Type::FunType(_) => Ok(IdKind::Fun),
                            _ => Ok(IdKind::Var(t.clone()))
                        }
                    }
                }
            }
        }
    }
}

