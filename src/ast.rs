use std::convert::TryInto;

use Expression::{*};

use crate::types::{FnType, Type, TypeError};

pub type Assignment = (Vec<String>, Vec<Expression>, Vec<Option<Type>>);

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ReAssignment {
    pub assignment: Assignment,
    pub globals: Vec<bool>,
}

pub type Fun = (String, Vec<String>, Expression, FnType);

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ExtDef {
    pub def_name: String,
    pub typ: Type,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    Empty,
    Const(String, Type),
    Local(String, Type),
    Global(String, Type),
    Let(Assignment),
    Mut(Assignment),
    Set(ReAssignment),
    Group(Vec<Expression>),
    Multi(Vec<Expression>),
    FunCall {
        name: String,
        args: Vec<Expression>,
        typ: Result<FnType, TypeError>,
        is_wasm_fun: bool,
    },
    ExprError(TypeError),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Visibility {
    Public,
    Private,
    Internal,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum TopLevelExpression {
    Let(Assignment, Visibility),
    Mut(Assignment, Visibility),
    Ext(String, Vec<ExtDef>, Visibility),
    Fn(Fun, Visibility),
    Error(String, (usize, usize)),
}

impl Expression {
    pub fn get_type(&self) -> Vec<Type> {
        match self {
            Expression::Empty | Let(..) | Mut(..) | Set(..) => Vec::new(),
            Const(.., typ) | Local(.., typ) | Global(.., typ) => vec![typ.clone()],
            Group(es) => es.last()
                .map_or(Vec::new(), |e| e.get_type()),
            Multi(es) => es.iter()
                .flat_map(|e| e.get_type()).collect(),
            FunCall { typ, .. } => match typ {
                Ok(t) => t.get_type(),
                Err(e) => e.get_type(),
            },
            ExprError(t) => t.get_type(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Expression::Empty => true,
            Const(..) | Local(..) | Global(..) | Let(..) | Mut(..) | Set(..)
            | Multi(_) | FunCall { .. } | ExprError(..) => false,
            Group(es) => es.last()
                .map_or(true, |e| e.is_empty()),
        }
    }

    pub fn into_multi(self) -> Vec<Expression> {
        match self {
            Expression::Empty => vec![],
            Let(..) | Mut(..) | Set(..) | Const(..) | Local(..) | Global(..)
            | FunCall { .. } | ExprError(..) | Group(..) => vec![self],
            Multi(mut es) => es.drain(..).flat_map(|e| e.into_multi()).collect(),
        }
    }
}

impl TryInto<TopLevelExpression> for Expression {
    type Error = String;

    fn try_into(self) -> Result<TopLevelExpression, Self::Error> {
        match self {
            Empty => Err("empty expression cannot appear at top-level".to_owned()),
            Let(l) => Ok(TopLevelExpression::Let(l, Visibility::Private)),
            Mut(m) => Ok(TopLevelExpression::Mut(m, Visibility::Private)),
            Set(..) | Const(..) | Local(..) | Global(..) | Group(..) | Multi(..) | FunCall { .. } =>
                Err("free expression appear at top-level".to_owned()),
            ExprError(e) => Err(e.reason)
        }
    }
}

impl From<TypeError> for TopLevelExpression {
    fn from(e: TypeError) -> Self {
        TopLevelExpression::Error(e.reason, e.pos)
    }
}

#[macro_export]
macro_rules! expr_empty {
    () => { Expression::Empty }
}

#[macro_export]
macro_rules! expr_const {
    ($id:literal $typ:expr) => { Expression::Const($id.to_string(), $typ) }
}

#[macro_export]
macro_rules! expr_local {
    ($id:literal $typ:expr) => { Expression::Local($id.to_string(), $typ) }
}

#[macro_export]
macro_rules! expr_global {
    ($id:literal $typ:expr) => { Expression::Global($id.to_string(), $typ) }
}

#[macro_export]
macro_rules! expr_group {
    ($($e:expr)*) => {{
        let mut exprs = Vec::new();
        $(exprs.push($e);)*
        Expression::Group(exprs)
    }}
}

#[macro_export]
macro_rules! expr_multi {
    ($($e:expr),*) => {{
        let mut exprs = Vec::new();
        $(exprs.push($e);)*
        Expression::Multi(exprs)
    }}
}

#[macro_export]
macro_rules! expr_let {
    ($($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::Expression;
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        Expression::Let((ids, exprs, replacements))
    }};
    ($($id:literal),+ = $($e:expr),+ ; $($rep:expr),+) => {{
        use crate::ast::Expression;
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string());)*
        $(exprs.push($e);)*
        $(replacements.push($rep);)*
        Expression::Let((ids, exprs, replacements))
    }};
}

#[macro_export]
macro_rules! expr_mut {
    ($($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::Expression;
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        Expression::Mut((ids, exprs, replacements))
    }};
    ($($id:literal),+ = $($e:expr),+ ; $($rep:expr),+) => {{
        use crate::ast::Expression;
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string());)*
        $(exprs.push($e);)*
        $(replacements.push($rep);)*
        Expression::Mut((ids, exprs, replacements))
    }};
}

#[macro_export]
macro_rules! expr_set {
    ($($id:literal),+ = $($e:expr),+ ; $($global:literal)*) => {{
        use crate::ast::{Expression, ReAssignment};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut globals = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        $(globals.push($global);)*
        Expression::Set(ReAssignment{assignment: (ids, exprs, replacements), globals})
    }};
    ($($id:literal),+ = $($e:expr),+ ; $($rep:expr),+ ; $($global:literal)*) => {{
        use crate::ast::{Expression, ReAssignment};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut globals = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string());)*
        $(exprs.push($e);)*
        $(replacements.push($rep);)*
        $(globals.push($global);)*
        Expression::Set(ReAssignment{assignment: (ids, exprs, replacements), globals})
    }};
}

#[macro_export]
macro_rules! expr_fun_call {
    ($id:literal $($arg:expr)* ; $typ:expr) => {{
        use crate::ast::Expression;
        let name = $id.to_owned();
        let mut args = Vec::new();
        $(args.push($arg);)*
        Expression::FunCall {name, args, typ: Ok($typ), is_wasm_fun: false}
    }};
    (wasm $id:literal $($arg:expr)* ; $typ:expr) => {{
        use crate::ast::Expression;
        let name = $id.to_owned();
        let mut args = Vec::new();
        $(args.push($arg);)*
        Expression::FunCall {name, args, typ: Ok($typ), is_wasm_fun: true}
    }};
}

#[macro_export]
macro_rules! fun_type {
    ([$($in:expr)*]($($out:expr)*)) => {{
        use crate::types::FnType;
        let mut _ins = Vec::new();
        let mut _outs = Vec::new();
        $(_ins.push($in);)*
        $(_outs.push($out);)*
        FnType { ins: _ins, outs: _outs }
    }}
}

#[macro_export]
macro_rules! texpr_let {
    ($($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{TopLevelExpression, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        TopLevelExpression::Let((ids, exprs, replacements), Visibility::Private)
    }};
    (p $($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{TopLevelExpression, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        TopLevelExpression::Let((ids, exprs, replacements), Visibility::Public)
    }};
}

#[macro_export]
macro_rules! texpr_mut {
    ($($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{TopLevelExpression, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        TopLevelExpression::Mut((ids, exprs, replacements), Visibility::Private)
    }};
    (p $($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{TopLevelExpression, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        TopLevelExpression::Mut((ids, exprs, replacements), Visibility::Public)
    }};
}

#[macro_export]
macro_rules! texpr_fun {
    (def $t:expr; fun $id:literal $($arg:literal)* = $e:expr) => {{
        use crate::ast::{TopLevelExpression, Visibility};
        let mut args = Vec::new();
        $(args.push($arg.to_owned());)*
        TopLevelExpression::Fn(($id.to_owned(), args, $e, $t), Visibility::Private)
    }};
    (def $t:expr; p fun $id:literal $($arg:literal)* = $e:expr) => {{
        use crate::ast::{TopLevelExpression, Visibility};
        let mut args = Vec::new();
        $(args.push($arg.to_owned());)*
        TopLevelExpression::Fn(($id.to_owned(), args, $e, $t), Visibility::Public)
    }}
}

#[macro_export]
macro_rules! texpr_ext {
    ($name: literal => $($id:literal $typ:expr);*) => {{
        use crate::ast::{TopLevelExpression, Visibility, ExtDef};
        let mut defs = Vec::new();
        $(
            defs.push(ExtDef{ def_name: $id.to_owned(), typ: $typ });
        )*
        TopLevelExpression::Ext($name.to_owned(), defs, Visibility::Private)
    }};
    (p $name: literal => $($id:literal $typ:expr);*) => {{
        use crate::ast::{TopLevelExpression, Visibility, ExtDef};
        let mut defs = Vec::new();
        $(
            defs.push(ExtDef{ def_name: $id.to_owned(), typ: $typ });
        )*
        TopLevelExpression::Ext($name.to_owned(), defs, Visibility::Public)
    }}
}