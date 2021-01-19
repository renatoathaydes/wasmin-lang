use std::convert::TryInto;

use Expression::{*};

use crate::types::{FunType, Type, TypeError};

/// Assignment defines one or more Wasmin assignments.
///
/// It is represented as a tuple with the following contents:
/// * variable names
/// * variable expressions
/// * optional type replacements (for implicit type conversions)
pub type Assignment = (Vec<String>, Vec<Expression>, Vec<Option<Type>>);

/// Reassignment is an [`Assignment`] of one or more mutable variables.
/// The variables may be local or global. The [`globals`] field determines which is the case
/// for each variable.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ReAssignment {
    pub assignment: Assignment,
    pub globals: Vec<bool>,
}

/// Function defines a function implementation as a tuple with the following contents:
/// * function name
/// * arg names
/// * body
/// * function type
pub type Function = (String, Vec<String>, Expression, FunType);

/// ExtDef is an external definition that a Wasmin program requires.
///
/// It translates to a WASM import.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ExtDef {
    /// external identifier used to refer to this definition.
    pub id: String,
    /// type of this definition.
    pub typ: Type,
}

/// Expression is the basic unit of Wasmin code.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression {
    Empty,
    Const(String, Type),
    Local(String, Type),
    Global(String, Type),
    Let(Assignment),
    Mut(Assignment),
    Set(ReAssignment),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Group(Vec<Expression>),
    Multi(Vec<Expression>),
    FunCall {
        name: String,
        args: Vec<Expression>,
        typ: Result<FunType, TypeError>,
        fun_index: usize,
        is_wasm_fun: bool,
    },
    ExprError(TypeError),
}

/// Visibility determines the level of visibility of a Wasmin program element.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Visibility {
    Public,
    Private,
}

/// Comment is a source code comment.
/// Comments may be used for documenting Wasmin code by placing them immediately before
/// source code top-level elements.
pub type Comment = String;

/// TopLevelElement represents elements that may appear at the top-level of a Wasmin program.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum TopLevelElement {
    Let(Assignment, Visibility, Option<Comment>),
    Mut(Assignment, Visibility, Option<Comment>),
    Ext(String, Vec<ExtDef>, Visibility, Option<Comment>),
    Fun(Function, Visibility, Option<Comment>),
    Error(String, (usize, usize)),
}

impl Expression {
    pub fn flatten_types_of(exprs: &Vec<Expression>) -> Vec<Type> {
        exprs.iter().flat_map(|e| e.get_type().into_iter()).collect()
    }

    pub fn get_type(&self) -> Vec<Type> {
        match self {
            Expression::Empty | Let(..) | Mut(..) | Set(..) => Vec::new(),
            Const(.., typ) | Local(.., typ) | Global(.., typ) => vec![typ.clone()],
            Group(es) => es.last()
                .map_or(Vec::new(), |e| e.get_type()),
            Multi(es) => Expression::flatten_types_of(es),
            FunCall { typ, .. } => match typ {
                Ok(t) => t.get_type(),
                Err(e) => e.get_type(),
            },
            If(_, then, ..) => then.get_type(),
            ExprError(t) => t.get_type(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Expression::Empty => true,
            Const(..) | Local(..) | Global(..) | Let(..) | Mut(..) | Set(..)
            | Multi(_) | FunCall { .. } | If(..) | ExprError(..) => false,
            Group(es) => es.is_empty() || es.iter().all(|e| e.is_empty()),
        }
    }

    pub fn into_multi(self) -> Vec<Expression> {
        match self {
            Expression::Empty => vec![],
            Let(..) | Mut(..) | Set(..) | Const(..) | Local(..) | Global(..)
            | FunCall { .. } | ExprError(..) | Group(..) | If(..) => vec![self],
            Multi(mut es) => es.drain(..).flat_map(|e| e.into_multi()).collect(),
        }
    }
}

impl TryInto<TopLevelElement> for Expression {
    type Error = String;

    fn try_into(self) -> Result<TopLevelElement, Self::Error> {
        match self {
            Empty => Err("empty expression cannot appear at top-level".to_owned()),
            Let(l) => Ok(TopLevelElement::Let(l, Visibility::Private, None)),
            Mut(m) => Ok(TopLevelElement::Mut(m, Visibility::Private, None)),
            Set(..) | Const(..) | Local(..) | Global(..) | Group(..) | Multi(..) |
            FunCall { .. } | If(..) =>
                Err("free expression appear at top-level".to_owned()),
            ExprError(e) => Err(e.reason)
        }
    }
}

impl From<TypeError> for TopLevelElement {
    fn from(e: TypeError) -> Self {
        TopLevelElement::Error(e.reason, e.pos)
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
    ($id:literal $($args:expr)* ; [$($ins:expr)*]($($outs:expr)*) $(;$idx:literal)? ) => {{
        use crate::ast::Expression;
        use crate::types::FunType;
        #[allow(unused_mut)]
        let (mut args, mut ins, mut outs, mut idx) = (Vec::new(), Vec::new(), Vec::new(), 0);
        $(args.push($args);)*
        $(ins.push($ins);)*
        $(outs.push($outs);)*
        let typ = FunType{ins , outs};
        $(idx = $idx;)?
        Expression::FunCall { name: $id.to_string(), args, typ: Ok(typ), fun_index: idx, is_wasm_fun: false }
    }};
    ($id:literal $($args:expr)* ; $err:expr ) => {{
        use crate::ast::Expression;
        #[allow(unused_mut)]
        let mut args = Vec::new();
        $(args.push($args);)*
        Expression::FunCall { name: $id.to_string(), args, typ: Err($err), fun_index: 0, is_wasm_fun: false }
    }};
    (wasm $id:literal $($arg:expr)* ; [$($ins:expr)*]($($outs:expr)*) $(;$idx:literal)? ) => {{
        use crate::ast::Expression;
        use crate::types::FunType;
        let name = $id.to_owned();
        #[allow(unused_mut)]
        let (mut args, mut ins, mut outs, mut idx) = (Vec::new(), Vec::new(), Vec::new(), 0);
        $(args.push($arg);)*
        $(ins.push($ins);)*
        $(outs.push($outs);)*
        let typ = FunType{ins , outs};
        $(idx = $idx;)?
        Expression::FunCall { name, args, typ: Ok(typ), fun_index: idx, is_wasm_fun: true}
    }};
}

#[macro_export]
macro_rules! expr_if {
    ($cond:expr; $then:expr; $els:expr) => {{
        use crate::ast::Expression;
        Expression::If(Box::new($cond), Box::new($then), Box::new($els))
    }};
}

#[macro_export]
macro_rules! fun_type {
    ([$($in:expr)*]($($out:expr)*)) => {{
        use crate::types::FunType;
        let mut _ins = Vec::new();
        let mut _outs = Vec::new();
        $(_ins.push($in);)*
        $(_outs.push($out);)*
        FunType { ins: _ins, outs: _outs }
    }}
}

#[macro_export]
macro_rules! top_let {
    ($($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{TopLevelElement, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        TopLevelElement::Let((ids, exprs, replacements), Visibility::Private, None)
    }};
    (p $($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{TopLevelElement, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        TopLevelElement::Let((ids, exprs, replacements), Visibility::Public, None)
    }};
}

#[macro_export]
macro_rules! top_mut {
    ($($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{TopLevelElement, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        TopLevelElement::Mut((ids, exprs, replacements), Visibility::Private, None)
    }};
    (p $($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{TopLevelElement, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        TopLevelElement::Mut((ids, exprs, replacements), Visibility::Public, None)
    }};
}

#[macro_export]
macro_rules! top_fun {
    (def $t:expr; fun $id:literal $($arg:literal)* = $e:expr) => {{
        use crate::ast::{TopLevelElement, Visibility};
        let mut args = Vec::new();
        $(args.push($arg.to_owned());)*
        TopLevelElement::Fun(($id.to_owned(), args, $e, $t), Visibility::Private, None)
    }};
    (def $t:expr; p fun $id:literal $($arg:literal)* = $e:expr) => {{
        use crate::ast::{TopLevelElement, Visibility};
        let mut args = Vec::new();
        $(args.push($arg.to_owned());)*
        TopLevelElement::Fun(($id.to_owned(), args, $e, $t), Visibility::Public, None)
    }}
}

#[macro_export]
macro_rules! top_ext {
    ($name: literal => $($id:literal $typ:expr);*) => {{
        use crate::ast::{TopLevelElement, Visibility, ExtDef};
        let mut defs = Vec::new();
        $(
            defs.push(ExtDef{ def_name: $id.to_owned(), typ: $typ });
        )*
        TopLevelElement::Ext($name.to_owned(), defs, Visibility::Private, None)
    }};
    (p $name: literal => $($id:literal $typ:expr);*) => {{
        use crate::ast::{TopLevelElement, Visibility, ExtDef};
        let mut defs = Vec::new();
        $(
            defs.push(ExtDef{ def_name: $id.to_owned(), typ: $typ });
        )*
        TopLevelElement::Ext($name.to_owned(), defs, Visibility::Public, None)
    }}
}
