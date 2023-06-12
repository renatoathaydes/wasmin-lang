use std::collections::HashMap;

use crate::ast::{Expression, ExprType, Function, Type};
use crate::errors::WasminError;
use crate::interner::InternedStr;
use crate::parse::model::Position;
use crate::parse::parser::Parser;

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ScopeFun {
    pub typ: ExprType,
    pub fun_index: usize,
    pub pos: Position,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum ScopeItem {
    Variable(Type),
    Fun(Vec<ScopeFun>),
}

pub type Scope = HashMap<InternedStr, ScopeItem>;

impl<'s> Parser<'s> {
    pub(crate) fn current_scope(&self) -> &Scope {
        self.scope.last().expect("there must be a root scope")
    }

    pub(crate) fn current_scope_mut(&mut self) -> &mut Scope {
        self.scope.last_mut().expect("there must be a root scope")
    }

    pub(crate) fn lookup_in_scope(&self, name: &InternedStr) -> Option<&ScopeItem> {
        for scope in self.scope.iter().rev() {
            let item = scope.get(name);
            if item.is_some() { return item; }
        }
        None
    }

    pub(crate) fn insert_type_in_scope(&mut self, name: InternedStr, typ: Type) {
        self.current_scope_mut().insert(name, ScopeItem::Variable(typ));
    }

    pub(crate) fn add_fun_type_to_scope(&mut self, name: &InternedStr, args: Vec<String>,
                                        body: Expression, pos: Position, typ: ExprType)
                                        -> Result<Function, WasminError> {
        let (other_pos, is_new_name) = match self.current_scope_mut().get_mut(name) {
            None | Some(ScopeItem::Variable(_)) => (None, true),
            Some(&mut ScopeItem::Fun(ref mut items)) => {
                if let Some(other) = items.iter().find(|s| s.typ == typ) {
                    (Some(other.pos), false)
                } else {
                    (None, false)
                }
            }
        };
        if let Some(other) = other_pos {
            Err(WasminError::DuplicateDeclaration {
                cause: format!("function with type {} has already been declared",
                               self.ast.interned_str(name)),
                pos,
                other,
            })
        } else {
            let fun = self.ast.new_fun_interned(name.clone(), args, body, typ.clone());
            if is_new_name {
                self.current_scope_mut().insert(name.clone(), ScopeItem::Fun(vec![ScopeFun {
                    typ,
                    fun_index: fun.fun_index,
                    pos,
                }]));
            } else {
                // if we get here, we know we need to add to the existing ScopeItem::Fun
                match self.current_scope_mut().get_mut(name) {
                    Some(&mut ScopeItem::Fun(ref mut items)) => {
                        items.push(ScopeFun { typ, fun_index: fun.fun_index, pos });
                    }
                    _ => {}
                }
            }
            Ok(fun)
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
}