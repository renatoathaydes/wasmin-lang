use std::error::Error;
use std::fmt;

use crate::errors::{TypeError, WasminError};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type<'s> {
    I64,
    I32,
    F64,
    F32,
    Empty,
    Fn(ExprType<'s>),
    Custom(&'s str),
    Error(WasminError),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ExprType<'s> {
    ins: Vec<Type<'s>>,
    outs: Vec<Type<'s>>,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Def<'s> {
    name: &'s str,
    target_type: Option<Type<'s>>,
}

/// Assignment defines one or more Wasmin assignments.
///
/// It is represented as a tuple with the following contents:
/// * variable names
/// * variable expression (if more than one value, will be a [Group] of [Expression]s).
/// * optional type replacements (for implicit type conversions)
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Assignment<'s> {
    vars: Vec<Def<'s>>,
    expr: Box<Expression<'s>>,
}

/// Reassignment is an [`Assignment`] of one or more mutable variables.
/// The variables may be local or global. The [`globals`] field determines which is the case
/// for each variable.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ReAssignment<'s> {
    pub assignment: Assignment<'s>,
    pub globals: Vec<bool>,
}

/// Break instruction that exits a loop with a certain set of types on the stack.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Break<'s> {
    pub types: Vec<Type<'s>>,
}

/// Comment is a source code comment.
/// Comments may be used for documenting Wasmin code by placing them immediately before
/// source code top-level elements.
pub type Comment<'s> = &'s str;

/// Warning emitted by the Wasmin compiler.
pub type Warning = String;

/// Expression is the basic unit of Wasmin code.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression<'s> {
    Empty(Vec<Warning>),
    Const(&'s str, Type<'s>, ExprType<'s>, Vec<Warning>),
    Local(&'s str, Type<'s>, ExprType<'s>, Vec<Warning>),
    Global(&'s str, Type<'s>, ExprType<'s>, Vec<Warning>),
    Let(Assignment<'s>, Vec<Warning>),
    Mut(Assignment<'s>, Vec<Warning>),
    Set(ReAssignment<'s>, Vec<Warning>),
    If {
        cond: Box<Expression<'s>>,
        yes: Box<Expression<'s>>,
        no: Box<Expression<'s>>,
        typ: ExprType<'s>,
        warnings: Vec<Warning>,
    },
    Loop {
        expr: Box<Expression<'s>>,
        error: Option<WasminError>,
        typ: ExprType<'s>,
        warnings: Vec<Warning>,
    },
    Br(Break<'s>, ExprType<'s>, Vec<Warning>),
    Group(Vec<Expression<'s>>, ExprType<'s>, Vec<Warning>),
    FunCall {
        name: &'s str,
        typ: ExprType<'s>,
        fun_index: usize,
        is_wasm_fun: bool,
        warnings: Vec<Warning>,
    },
    ExprError(WasminError, Vec<Warning>),
}

/// Function defines a function implementation as a tuple with the following contents:
/// * function name
/// * arg names
/// * body
/// * function type
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Function<'s> {
    name: &'s str,
    arg_names: Vec<&'s str>,
    body: Expression<'s>,
    target_type: ExprType<'s>,
}

/// Visibility determines the level of visibility of a Wasmin program element.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Visibility {
    Public,
    Private,
}

/// TopLevelElement represents elements that may appear at the top-level of a Wasmin program.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum TopLevelElement<'s> {
    Let(Assignment<'s>, Visibility, Option<Comment<'s>>, Vec<Warning>),
    Mut(Assignment<'s>, Visibility, Option<Comment<'s>>, Vec<Warning>),
    Ext(&'s str, Vec<Def<'s>>, Visibility, Option<Comment<'s>>, Vec<Warning>),
    Fun(Function<'s>, Visibility, Option<Comment<'s>>, Vec<Warning>),
    Error(WasminError),
}

impl<'s> ExprType<'s> {
    fn outs(outs: Vec<Type<'s>>) -> ExprType<'s> {
        ExprType { ins: vec![], outs }
    }
}

impl<'s> fmt::Display for ExprType<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[")?;
        f.write_str(&type_to_string(&self.ins))?;
        f.write_str("](")?;
        f.write_str(&type_to_string(&self.outs))?;
        f.write_str(")")
    }
}

impl<'s> fmt::Display for Type<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Custom(s) => write!(f, ":{}", s)?,
            Type::I64 => write!(f, "i64")?,
            Type::I32 => write!(f, "i32")?,
            Type::F64 => write!(f, "f64")?,
            Type::F32 => write!(f, "f32")?,
            Type::Empty => write!(f, "()")?,
            Type::Fn(types) => {
                write!(f, "({})", types)?;
            }
            Type::Error(err) => write!(f, "ERROR({:?})", err.source())?,
        };
        Ok(())
    }
}

const EMPTY_EXPR_TYPE: &'static ExprType = &ExprType { ins: vec![], outs: vec![] };

impl<'s> Expression<'s> {
    pub fn get_type(&self) -> Result<&'s ExprType<'_>, TypeError> {
        match self {
            Expression::Empty(_) |
            Expression::Mut(_, _) |
            Expression::Set(_, _) |
            Expression::ExprError(_, _) |
            Expression::Let(_, _) => Ok(EMPTY_EXPR_TYPE),
            Expression::Const(_, _, t, _) |
            Expression::Local(_, _, t, _) |
            Expression::Global(_, _, t, _) => Ok(t),
            Expression::If { yes, .. } => yes.get_type(),
            Expression::Loop { typ, .. } => Ok(typ),
            Expression::Br(_, t, _) => Ok(t),
            Expression::FunCall { typ, .. } => Ok(typ),
            Expression::Group(_, typ, _) => Ok(typ),
        }
    }
    pub fn empty() -> Expression<'s> { Expression::Empty(vec![]) }

    pub fn new_mut(a: Assignment<'s>, w: Vec<Warning>) -> Expression<'s> {
        Expression::Mut(a, w)
    }

    pub fn new_set(re: ReAssignment<'s>, w: Vec<Warning>) -> Expression<'s> {
        Expression::Set(re, w)
    }

    pub fn new_error(err: WasminError, w: Vec<Warning>) -> Expression<'s> {
        Expression::ExprError(err, w)
    }

    pub fn new_let(a: Assignment<'s>, w: Vec<Warning>) -> Expression<'s> {
        Expression::Let(a, w)
    }

    pub fn new_const(name: &'s str, typ: Type<'s>, w: Vec<Warning>) -> Expression<'s> {
        let expr_type = ExprType::outs(vec![typ.clone()]);
        Expression::Const(name, typ, expr_type, w)
    }
    pub fn new_local(name: &'s str, typ: Type<'s>, w: Vec<Warning>) -> Expression<'s> {
        let expr_type = ExprType::outs(vec![typ.clone()]);
        Expression::Local(name, typ, expr_type, w)
    }
    pub fn new_global(name: &'s str, typ: Type<'s>, w: Vec<Warning>) -> Expression<'s> {
        let expr_type = ExprType::outs(vec![typ.clone()]);
        Expression::Global(name, typ, expr_type, w)
    }
}

impl<'s> Def<'s> {
    fn new(name: &'s str, target_type: Option<Type<'s>>) -> Def<'s> {
        Def { name, target_type }
    }

    fn new_many(defs: &mut Vec<(&'s str, Option<Type<'s>>)>) -> Vec<Def<'s>> {
        defs.drain(..)
            .map(|(name, target_type)| Def { name, target_type })
            .collect()
    }
}

impl<'s> Assignment<'s> {
    pub fn new(name: &'s str, expr: Expression<'s>) -> Assignment<'s> {
        Assignment { vars: vec![Def { name, target_type: None }], expr: Box::new(expr) }
    }

    pub fn new_vars(vars: Vec<Def<'s>>,
                    expr: Box<Expression<'s>>) -> Assignment<'s> {
        Assignment { vars, expr }
    }
}

pub(crate) fn type_to_string(types: &[Type]) -> String {
    if types.is_empty() {
        return "()".to_owned();
    }
    let mut res = String::new();
    let max = types.len() - 1;
    for (i, t) in types.iter().enumerate() {
        res.push_str(&format!("{}", t));
        if i != max {
            res.push(' ');
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Type::*;

    #[test]
    fn test_get_type() {
        assert_eq!(Expression::empty().get_type(), Ok(EMPTY_EXPR_TYPE));

        assert_eq!(Expression::new_const("i32", I32, vec![]).get_type(),
                   Ok(&ExprType::outs(vec![I32])));

        assert_eq!(Expression::new_local("foo", I64, vec![]).get_type(),
                   Ok(&ExprType::outs(vec![I64])));

        assert_eq!(Expression::new_global("foo", F64, vec![]).get_type(),
                   Ok(&ExprType::outs(vec![F64])));

        assert_eq!(Expression::new_let(
            Assignment::new(
                "", Expression::new_const("", Type::F32, vec![])),
            vec![]).get_type(), Ok(EMPTY_EXPR_TYPE));

        assert_eq!(Expression::new_mut(Assignment::new(
            "", Expression::new_const("", Type::F32, vec![])),
                                       vec![]).get_type(), Ok(EMPTY_EXPR_TYPE));

        assert_eq!(Expression::new_set(ReAssignment {
            assignment: Assignment::new(
                "", Expression::new_const("", Type::F32, vec![])),
            globals: vec![],
        }, vec![]).get_type(), Ok(EMPTY_EXPR_TYPE));
    }
}
