use std::fmt;

use crate::errors::{ErrorPosition, TypeError, WasminError};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct FunType {
    pub ins: Vec<Type>,
    pub outs: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type {
    I64,
    I32,
    F64,
    F32,
    Empty,
    Fn(Vec<FunType>),
    WasmFn(Vec<FunType>),
    Error(WasminError),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ExprType {
    ins: Vec<Type>,
    outs: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Def<'s> {
    name: &'s str,
    target_type: Option<Type>,
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
    target_types: Vec<Option<Type>>,
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
pub struct Break {
    pub types: Vec<Type>,
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
    Const(&'s str, Type, Vec<Warning>),
    Local(&'s str, Type, Vec<Warning>),
    Global(&'s str, Type, Vec<Warning>),
    Let(Assignment<'s>, Vec<Warning>),
    Mut(Assignment<'s>, Vec<Warning>),
    Set(ReAssignment<'s>, Vec<Warning>),
    If {
        cond: Box<Expression<'s>>,
        yes: Box<Expression<'s>>,
        no: Box<Expression<'s>>,
        warnings: Vec<Warning>,
    },
    Loop {
        expr: Box<Expression<'s>>,
        error: Option<WasminError>,
        warnings: Vec<Warning>,
    },
    Br(Break, Vec<Warning>),
    Group(Vec<Expression<'s>>, Vec<Warning>),
    FunCall {
        name: String,
        typ: FunType,
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
    target_type: FunType,
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

impl ExprType {
    fn empty() -> Self {
        ExprType { ins: vec![], outs: vec![] }
    }

    fn of(t: &Type) -> Self {
        ExprType { ins: vec![], outs: vec![t.clone()] }
    }
}

impl fmt::Display for FunType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[")?;
        f.write_str(&types_to_string(&self.ins))?;
        f.write_str("](")?;
        f.write_str(&types_to_string(&self.outs))?;
        f.write_str(")")
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I64 => write!(f, "i64")?,
            Type::I32 => write!(f, "i32")?,
            Type::F64 => write!(f, "f64")?,
            Type::F32 => write!(f, "f32")?,
            Type::Empty => write!(f, "()")?,
            Type::Fn(types) | Type::WasmFn(types) => {
                write!(f, "(")?;
                let text = types
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(" | ");
                write!(f, "{})", text)?;
            }
            Type::Error(err) => write!(f, "ERROR({})", err.cause())?,
        };
        Ok(())
    }
}

impl<'s> Expression<'s> {
    pub fn get_type(&self) -> Result<ExprType, TypeError> {
        match self {
            Expression::Empty(_) |
            Expression::Mut(_, _) |
            Expression::Set(_, _) |
            Expression::ExprError(_, _) |
            Expression::Let(_, _) => Ok(ExprType::empty()),
            Expression::Const(_, t, _) |
            Expression::Local(_, t, _) |
            Expression::Global(_, t, _) => Ok(ExprType::of(t)),
            Expression::If { yes, .. } => yes.get_type(),
            Expression::Loop { expr, .. } => expr.get_type(),
            Expression::Br(b, _) => Ok(ExprType { ins: b.types.clone(), outs: vec![] }),
            Expression::FunCall { typ, .. } =>
                Ok(ExprType { ins: typ.ins.clone(), outs: typ.outs.clone() }),
            Expression::Group(exprs, _) => type_of_exprs(exprs),
        }
    }
}

fn type_of_exprs(exprs: &Vec<Expression>) -> Result<ExprType, TypeError> {
    let mut ins: Vec<Type> = vec![];
    let mut outs: Vec<Type> = vec![];
    for expr in exprs {
        let mut typ = expr.get_type()?;
        for i in typ.ins {
            if let Some(e) = outs.pop() {
                if e != i {
                    return Err(werr_t!(format!("expected input of type '{}', \
                        but got '{}'", i, e), (0, 0)));
                }
            } else {
                ins.push(i);
            }
        }
        outs.append(&mut typ.outs);
    }
    Ok(ExprType { ins, outs })
}

pub(crate) fn types_to_string(types: &[Type]) -> String {
    let v: Vec<_> = types.iter().collect();
    type_refs_to_string(&v)
}

pub(crate) fn type_refs_to_string(types: &[&Type]) -> String {
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
        assert_eq!(Expression::Empty(vec![]).get_type(), Ok(ExprType::empty()));

        assert_eq!(Expression::Const("i32", I32, vec![]).get_type(),
                   Ok(ExprType { ins: vec![], outs: vec![I32] }));

        assert_eq!(Expression::Local("foo", I64, vec![]).get_type(),
                   Ok(ExprType { ins: vec![], outs: vec![I64] }));

        assert_eq!(Expression::Global("foo", F64, vec![]).get_type(),
                   Ok(ExprType { ins: vec![], outs: vec![F64] }));

        assert_eq!(Expression::Let(Assignment {
            vars: vec![],
            expr: Box::new(Expression::Empty(vec![])),
            target_types: vec![Some(I32)],
        }, vec![]).get_type(), Ok(ExprType::empty()));

        assert_eq!(Expression::Mut(Assignment {
            vars: vec![],
            expr: Box::new(Expression::Empty(vec![])),
            target_types: vec![Some(I32)],
        }, vec![]).get_type(), Ok(ExprType::empty()));

        assert_eq!(Expression::Set(ReAssignment {
            assignment: Assignment {
                vars: vec![],
                expr: Box::new(Expression::Empty(vec![])),
                target_types: vec![Some(I32)],
            },
            globals: vec![],
        }, vec![]).get_type(), Ok(ExprType::empty()));
    }
}
