use Expression::{*};

use crate::types::{FunType, Type, TypeError};
use crate::vec_utils::{get_last, push_all, remove_last, remove_last_n};
use crate::errors::WasminError;

/// Assignment defines one or more Wasmin assignments.
///
/// It is represented as a tuple with the following contents:
/// * variable names
/// * variable expression (if more than one value, will be a [Group] of [Expression]s).
/// * optional type replacements (for implicit type conversions)
pub type Assignment = (Vec<String>, Box<Expression>, Vec<Option<Type>>);

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

/// Break instruction that exits a loop with a certain set of types on the stack.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Break { pub types: Vec<Type> }

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
    Loop { expr: Box<Expression>, error: Option<TypeError> },
    Br(Break),
    Group(Vec<Expression>),
    FunCall {
        name: String,
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
    Error(WasminError),
}

impl Expression {
    pub fn get_type(&self) -> Vec<Type> {
        let mut types = Vec::new();
        self.get_type_internal(&mut types);
        types
    }

    fn get_type_internal(&self, result: &mut Vec<Type>) {
        match self {
            Expression::Empty | Let(..) | Mut(..) | Set(..) | Br(..) => {}
            Const(.., typ) | Local(.., typ) | Global(.., typ) => {
                result.push(typ.clone())
            }
            Group(es) => {
                let ignore_stack = if !es.is_empty() {
                    matches!(get_last(es), Br(..))
                } else { false };
                if !ignore_stack {
                    for expr in es {
                        expr.get_type_internal(result);
                    }
                }
            }
            FunCall { typ, .. } => match typ {
                Ok(t) => {
                    remove_last_n(result, t.ins.len());
                    push_all(&t.outs, result);
                }
                Err(e) => {
                    push_all(&e.get_type(), result)
                }
            },
            If(_, then, ..) => {
                then.get_type_internal(result)
            }
            Loop { expr, .. } => {
                if let Some(Break { types }) = expr.get_nested_break() {
                    push_all(types, result)
                }
            }
            ExprError(e) => {
                push_all(&e.get_type(), result)
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Expression::Empty => true,
            Group(es) => es.is_empty() || es.iter().all(|e| e.is_empty()),
            Loop { expr, .. } => expr.is_empty(),
            _ => false
        }
    }

    pub fn get_value_type(&self) -> Option<&Type> {
        match self {
            Const(_, typ) |
            Local(_, typ) |
            Global(_, typ) => Some(typ),
            _ => None
        }
    }

    fn get_nested_break(&self) -> Option<&Break> {
        let mut result = vec![];
        self.get_nested_breaks(1, &mut result);
        if result.is_empty() { None } else { Some(remove_last(&mut result)) }
    }

    pub fn get_nested_breaks<'s, 't>(&'s self, limit: usize, result: &'t mut Vec<&'s Break>)
        where 's: 't {
        match self {
            Br(br) => {
                result.push(br);
            }
            Let((_, e, ..)) | Mut((_, e, ..)) |
            Set(ReAssignment { assignment: (_, e, ..), .. }) => {
                e.get_nested_breaks(limit, result)
            }
            If(_, then, els) => {
                then.get_nested_breaks(limit, result);
                if result.len() < limit {
                    els.get_nested_breaks(limit, result);
                }
            }
            Group(exprs) => {
                for e in exprs {
                    e.get_nested_breaks(limit, result);
                    if result.len() >= limit { break; }
                }
            }
            _ => {}
        };
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
macro_rules! expr_let {
    ($($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::Expression;
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        Expression::Let((ids, Box::new(e), replacements))
    }};
    ($($id:literal),+ = $($e:expr),+ ; $($rep:expr),+) => {{
        use crate::ast::Expression;
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string());)*
        $(exprs.push($e);)*
        $(replacements.push($rep);)*
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        Expression::Let((ids, Box::new(e), replacements))
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
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        Expression::Mut((ids, Box::new(e), replacements))
    }};
    ($($id:literal),+ = $($e:expr),+ ; $($rep:expr),+) => {{
        use crate::ast::Expression;
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string());)*
        $(exprs.push($e);)*
        $(replacements.push($rep);)*
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        Expression::Mut((ids, Box::new(e), replacements))
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
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        Expression::Set(ReAssignment{assignment: (ids, Box::new(e), replacements), globals})
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
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        Expression::Set(ReAssignment{assignment: (ids, Box::new(e), replacements), globals})
    }};
}

#[macro_export]
macro_rules! expr_fun_call {
    ($id:literal [$($ins:expr)*]($($outs:expr)*) $(;$idx:literal)? ) => {{
        use crate::ast::Expression;
        use crate::types::FunType;
        #[allow(unused_mut)]
        let (mut ins, mut outs, mut idx) = (Vec::new(), Vec::new(), 0);
        $(ins.push($ins);)*
        $(outs.push($outs);)*
        let typ = FunType{ins , outs};
        $(idx = $idx;)?
        Expression::FunCall { name: $id.to_string(), typ: Ok(typ), fun_index: idx, is_wasm_fun: false }
    }};
    ($id:literal $err:expr ) => {{
        use crate::ast::Expression;
        #[allow(unused_mut)]
        Expression::FunCall { name: $id.to_string(), typ: Err($err), fun_index: 0, is_wasm_fun: false }
    }};
    (wasm $id:literal [$($ins:expr)*]($($outs:expr)*) $(;$idx:literal)? ) => {{
        use crate::ast::Expression;
        use crate::types::FunType;
        let name = $id.to_owned();
        #[allow(unused_mut)]
        let (mut ins, mut outs, mut idx) = (Vec::new(), Vec::new(), 0);
        $(ins.push($ins);)*
        $(outs.push($outs);)*
        let typ = FunType{ins , outs};
        $(idx = $idx;)?
        Expression::FunCall { name, typ: Ok(typ), fun_index: idx, is_wasm_fun: true}
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
macro_rules! expr_loop {
    ($e:expr) => {{
        use crate::ast::Expression;
        Expression::Loop { expr: Box::new($e), error: None }
    }};
    ($e:expr, $err:expr) => {{
        use crate::ast::Expression;
        Expression::Loop { expr: Box::new($e), error: Some($err) }
    }};
}

#[macro_export]
macro_rules! expr_break {
    ($($t:expr)*) => {{
        use crate::ast::{Expression, Break};
        let mut _types = Vec::new();
        $(_types.push($t);)*
        Expression::Br(Break { types: _types })
    }}
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
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        TopLevelElement::Let((ids, Box::new(e), replacements), Visibility::Private, None)
    }};
    (p $($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{TopLevelElement, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        TopLevelElement::Let((ids, Box::new(e), replacements), Visibility::Public, None)
    }};
}

#[macro_export]
macro_rules! top_mut {
    ($($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{Expression, TopLevelElement, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        TopLevelElement::Mut((ids, Box::new(e), replacements), Visibility::Private, None)
    }};
    (p $($id:literal),+ = $($e:expr),+) => {{
        use crate::ast::{Expression, TopLevelElement, Visibility};
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        let e = if exprs.len() == 1 { exprs.remove(0) } else { Expression::Group(exprs) };
        TopLevelElement::Mut((ids, Box::new(e), replacements), Visibility::Public, None)
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
