use std::cmp::min;
use std::collections::VecDeque;
use std::ops::Range;

use crate::errors::WasminError;
use crate::interner::{*};
use crate::parse::model::Numeric;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type {
    I64,
    I32,
    F64,
    F32,
    String,
    Empty,
    Fn(ExprType),
    Custom(InternedStr),
    Error(WasminError),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ExprType {
    ins: Vec<Type>,
    outs: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Def {
    name: InternedStr,
    target_type: Option<Type>,
}

/// Assignment defines one or more Wasmin assignments.
///
/// It is represented as a tuple with the following contents:
/// * variable names
/// * variable expression (if more than one value, will be a [Group] of [Expression]s).
/// * optional type replacements (for implicit type conversions)
#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    vars: Vec<Def>,
    expr: Box<Expression>,
}

/// Reassignment is an [`Assignment`] of one or more mutable variables.
/// The variables may be local or global. The [`globals`] field determines which is the case
/// for each variable.
#[derive(Debug, PartialEq, Clone)]
pub struct ReAssignment {
    assignment: Assignment,
    globals: Vec<bool>,
}

/// Break instruction that exits a loop with a certain set of types on the stack.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Break {
    pub types: Vec<Type>,
}

/// Comment is a source code comment.
/// Comments may be used for documenting Wasmin code by placing them immediately before
/// source code top-level elements.
pub type Comment = InternedStr;

/// Warning emitted by the Wasmin compiler.
pub type Warning = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    String(InternedStr),
    Number(Numeric),
}

/// Expression is the basic unit of Wasmin code.
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Empty(Vec<Warning>),
    Const(Constant, Type, ExprType, Vec<Warning>),
    Local(InternedStr, Type, ExprType, Vec<Warning>),
    Global(InternedStr, Type, ExprType, Vec<Warning>),
    Let(Assignment, Vec<Warning>),
    Mut(Assignment, Vec<Warning>),
    Set(ReAssignment, Vec<Warning>),
    If {
        cond: Box<Expression>,
        yes: Box<Expression>,
        no: Box<Expression>,
        typ: ExprType,
        warnings: Vec<Warning>,
    },
    Loop {
        expr: Box<Expression>,
        error: Option<WasminError>,
        typ: ExprType,
        warnings: Vec<Warning>,
    },
    Br(Break, ExprType, Vec<Warning>),
    Group(Group),
    FunCall {
        name: InternedStr,
        typ: ExprType,
        fun_index: usize,
        is_wasm_fun: bool,
        warnings: Vec<Warning>,
    },
    ExprError(WasminError, Vec<Warning>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Group {
    exprs: Vec<Expression>,
    typ: ExprType,
    warnings: Vec<Warning>,
}

/// Function defines a function implementation as a tuple with the following contents:
/// * function name
/// * arg names
/// * body
/// * function type
#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    name: InternedStr,
    arg_names: Vec<InternedStr>,
    body: Expression,
    target_type: ExprType,
}

/// Visibility determines the level of visibility of a Wasmin program element.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Visibility {
    Public,
    Private,
}

/// TopLevelElement represents elements that may appear at the top-level of a Wasmin program.
#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelElement {
    Let(Assignment, Visibility, Option<Comment>, Vec<Warning>),
    Mut(Assignment, Visibility, Option<Comment>, Vec<Warning>),
    Ext(InternedStr, Vec<Def>, Visibility, Option<Comment>, Vec<Warning>),
    Fun(Function, Visibility, Option<Comment>, Vec<Warning>),
    Error(WasminError),
}

impl ExprType {
    pub fn new(ins: Vec<Type>, outs: Vec<Type>) -> ExprType {
        ExprType { ins, outs }
    }

    pub fn outs(outs: Vec<Type>) -> ExprType {
        ExprType { ins: vec![], outs }
    }

    pub fn ins(ins: Vec<Type>) -> ExprType {
        ExprType { ins, outs: vec![] }
    }
}

// impl fmt::Display for ExprType {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         f.write_str("[")?;
//         f.write_str(&type_to_string(&self.ins))?;
//         f.write_str("](")?;
//         f.write_str(&type_to_string(&self.outs))?;
//         f.write_str(")")
//     }
// }
//
// impl fmt::Display for Type {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Type::Custom(s) => write!(f, ":{}", s)?,
//             Type::I64 => write!(f, "i64")?,
//             Type::I32 => write!(f, "i32")?,
//             Type::F64 => write!(f, "f64")?,
//             Type::F32 => write!(f, "f32")?,
//             Type::Empty => write!(f, "()")?,
//             Type::Fn(typ) => {
//                 write!(f, "({})", typ)?;
//             }
//             Type::Error(err) => write!(f, "ERROR({:?})", err.source())?,
//         };
//         Ok(())
//     }
// }

const EMPTY_EXPR_TYPE: &'static ExprType = &ExprType { ins: vec![], outs: vec![] };

fn end_range_of_len(range_len: usize, vec_len: usize) -> Range<usize> {
    (vec_len - range_len)..vec_len
}

fn merge_types(mut types: Vec<ExprType>) -> (ExprType, Vec<Warning>) {
    let mut ins: Vec<Type> = Vec::with_capacity(types.len());
    let mut outs: Vec<Type> = Vec::with_capacity(types.len());
    let mut errs: Vec<Warning> = Vec::new();
    for typ in types.iter_mut() {
        let mut annihilate_len = min(typ.ins.len(), outs.len());
        let pre_outs = outs.drain(end_range_of_len(annihilate_len, outs.len()));
        let cur_ins = typ.ins.drain(0..annihilate_len);

        for (o, i) in pre_outs.zip(cur_ins) {
            if i != o {
                errs.push(format!("expected {:?} but found {:?}", i, o));
            }
        }

        // transfer inputs
        typ.ins.drain(..).for_each(|t| ins.push(t));

        // add outputs
        typ.outs.drain(..).for_each(|t| outs.push(t));
    }
    (ExprType { ins, outs }, errs)
}

impl Expression {
    pub fn get_type(&self) -> &ExprType {
        match self {
            Expression::Empty(_) |
            Expression::Mut(_, _) |
            Expression::Set(_, _) |
            Expression::ExprError(_, _) |
            Expression::Let(_, _) => EMPTY_EXPR_TYPE,
            Expression::Const(_, _, typ, _) |
            Expression::Local(_, _, typ, _) |
            Expression::Global(_, _, typ, _) |
            Expression::Loop { typ, .. } |
            Expression::Br(_, typ, _) |
            Expression::FunCall { typ, .. } |
            Expression::Group(Group { typ, .. }) => typ,
            Expression::If { yes, .. } => yes.get_type(),
        }
    }
}


#[derive(Debug, Default)]
pub struct AST {
    interner: Interner,
}

impl AST {
    fn intern(&mut self, s: &str) -> InternedStr {
        self.interner.intern(s)
    }

    pub fn new() -> Self { Default::default() }

    pub fn empty() -> Expression { Expression::Empty(vec![]) }

    pub fn new_mut(a: Assignment, w: Vec<Warning>) -> Expression {
        Expression::Mut(a, w)
    }

    pub fn new_set(a: Assignment, globals: Vec<bool>, w: Vec<Warning>) -> Expression {
        Expression::Set(ReAssignment { assignment: a, globals }, w)
    }

    pub fn new_error(err: WasminError, w: Vec<Warning>) -> Expression {
        Expression::ExprError(err, w)
    }

    pub fn new_let(a: Assignment, w: Vec<Warning>) -> Expression {
        Expression::Let(a, w)
    }

    pub fn new_string(&mut self, value: &str, w: Vec<Warning>) -> Expression {
        let typ = Type::String;
        let expr_type = ExprType::outs(vec![typ.clone()]);
        Expression::Const(Constant::String(self.intern(value)), typ, expr_type, w)
    }

    pub fn new_number(&mut self, value: Numeric, w: Vec<Warning>) -> Expression {
        let typ = match value {
            Numeric::I32(_) => Type::I32,
            Numeric::I64(_) => Type::I64,
            Numeric::F32(_) => Type::F32,
            Numeric::F64(_) => Type::F64,
        };
        let expr_type = ExprType::outs(vec![typ.clone()]);
        Expression::Const(Constant::Number(value), typ, expr_type, w)
    }

    pub fn new_local(&mut self, name: &str, typ: Type, w: Vec<Warning>) -> Expression {
        let expr_type = ExprType::outs(vec![typ.clone()]);
        Expression::Local(self.intern(name), typ, expr_type, w)
    }

    pub fn new_global(&mut self, name: &str, typ: Type, w: Vec<Warning>) -> Expression {
        let expr_type = ExprType::outs(vec![typ.clone()]);
        Expression::Global(self.intern(name), typ, expr_type, w)
    }

    pub fn new_group(exprs: Vec<Expression>,
                     mut warnings: Vec<Warning>) -> Expression {
        let (typ, mut err) = {
            let mut types = Vec::with_capacity(exprs.len());
            for expr in exprs.iter() {
                let typ = expr.get_type().clone();
                types.push(typ);
            }
            merge_types(types)
        };
        err.drain(..).for_each(|e| warnings.push(e));
        Expression::Group(Group { exprs, typ, warnings })
    }

    fn new_def(&mut self, name: &str, target_type: Option<Type>) -> Def {
        Def { name: self.intern(name), target_type }
    }

    fn new_many(&mut self, defs: &mut Vec<(&str, Option<Type>)>) -> Vec<Def> {
        defs.drain(..)
            .map(|(name, target_type)| self.new_def(name, target_type))
            .collect()
    }

    pub fn new_assignment(&mut self, name: &str, typ: Option<Type>, expr: Expression) -> Assignment {
        Assignment {
            vars: vec![Def { name: self.intern(name), target_type: typ }],
            expr: Box::new(expr),
        }
    }

    pub fn new_assignments(&mut self, mut vars: Vec<(String, Option<Type>)>, expr: Expression) -> Assignment {
        let defs: Vec<_> = vars.into_iter()
            .map(|(n, t)| Def { name: self.intern(&n), target_type: t })
            .collect();
        Assignment {
            vars: defs,
            expr: Box::new(expr),
        }
    }

    pub fn new_vars(vars: Vec<Def>,
                    expr: Expression) -> Assignment {
        Assignment { vars, expr: Box::new(expr) }
    }
}

// pub(crate) fn type_to_string(types: &[Type]) -> String {
//     if types.is_empty() {
//         return "()".to_owned();
//     }
//     let mut res = String::new();
//     let max = types.len() - 1;
//     for (i, t) in types.iter().enumerate() {
//         res.push_str(&format!("{}", t));
//         if i != max {
//             res.push(' ');
//         }
//     }
//     res
// }

#[cfg(test)]
mod tests {
    use crate::ast::Constant::Number;

    use super::*;
    use super::Type::*;

    #[test]
    fn test_merge_types() {
        let (typ, w) = merge_types(vec![EMPTY_EXPR_TYPE.clone()]);
        assert_eq!(&typ, EMPTY_EXPR_TYPE);
        assert_eq!(w, Vec::<Warning>::new());

        let (typ, w) = merge_types(vec![ExprType::outs(vec![I32])]);
        assert_eq!(typ, ExprType::outs(vec![I32]));
        assert_eq!(w, Vec::<Warning>::new());

        let (typ, w) = merge_types(vec![ExprType::outs(vec![I32]), ExprType::outs(vec![F32])]);
        assert_eq!(typ, ExprType::outs(vec![I32, F32]));
        assert_eq!(w, Vec::<Warning>::new());

        let (typ, w) = merge_types(vec![
            ExprType::new(vec![I32, I32], vec![F32])]);
        assert_eq!(typ, ExprType::new(vec![I32, I32], vec![F32]));
        assert_eq!(w, Vec::<Warning>::new());

        let (typ, w) = merge_types(vec![
            ExprType::new(vec![I32, I32], vec![F32]),
            ExprType::new(vec![F32], vec![F64])]);
        assert_eq!(typ, ExprType::new(vec![I32, I32], vec![F64]));
        assert_eq!(w, Vec::<Warning>::new());

        let (typ, w) = merge_types(vec![
            ExprType::new(vec![I32, I32], vec![F32]),
            ExprType::outs(vec![F64])]);
        assert_eq!(typ, ExprType::new(vec![I32, I32], vec![F32, F64]));
        assert_eq!(w, Vec::<Warning>::new());

        let (typ, w) = merge_types(vec![
            ExprType::new(vec![I32, I32], vec![F32]),
            ExprType::new(vec![F32, F64], vec![I64])]);
        assert_eq!(typ, ExprType::new(vec![I32, I32, F64], vec![I64]));
        assert_eq!(w, Vec::<Warning>::new());

        let (typ, w) = merge_types(vec![
            ExprType::new(vec![], vec![F32, F64]),
            ExprType::new(vec![F32, F64], vec![I64])]);
        assert_eq!(typ, ExprType::new(vec![], vec![I64]));
        assert_eq!(w, Vec::<Warning>::new());

        let (typ, w) = merge_types(vec![
            ExprType::new(vec![], vec![F32, F64, I64]),
            ExprType::new(vec![F64, I64], vec![I64])]);
        assert_eq!(typ, ExprType::new(vec![], vec![F32, I64]));
        assert_eq!(w, Vec::<Warning>::new());

        let (typ, w) = merge_types(vec![
            ExprType::new(vec![I32], vec![F32]),
            ExprType::new(vec![F32], vec![I64]),
            ExprType::new(vec![I64, F64], vec![I32])]);
        assert_eq!(typ, ExprType::new(vec![I32, F64], vec![I32]));
        assert_eq!(w, Vec::<Warning>::new());
    }

    #[test]
    fn test_get_type() {
        let mut ast = AST::default();

        assert_eq!(AST::empty().get_type(), EMPTY_EXPR_TYPE);

        assert_eq!(ast.new_number(Numeric::I32(0), vec![]).get_type(),
                   &ExprType::outs(vec![I32]));

        assert_eq!(ast.new_local("foo", I64, vec![]).get_type(),
                   &ExprType::outs(vec![I64]));

        assert_eq!(ast.new_global("foo", F64, vec![]).get_type(),
                   &ExprType::outs(vec![F64]));

        let e1 = ast.new_number(Numeric::F32(0.0), vec![]);
        assert_eq!(AST::new_let(ast.new_assignment("", None, e1.clone()), vec![])
                       .get_type(), EMPTY_EXPR_TYPE);

        assert_eq!(AST::new_mut(ast.new_assignment("", None, e1.clone()), vec![])
                       .get_type(), EMPTY_EXPR_TYPE);

        assert_eq!(AST::new_set(ast.new_assignment("", None, e1.clone()), vec![false], vec![])
                       .get_type(), EMPTY_EXPR_TYPE);

        let const_a = ast.new_string("a", vec![]);
        let const_b = ast.new_number(Numeric::I64(0), vec![]); // allows coercion to I64
        assert_eq!(AST::new_group(vec![const_a, const_b], vec![]).get_type(),
                   &ExprType::outs(vec![String, I64]));

        let const_a = ast.new_number(Numeric::I32(0), vec![]);
        let const_b = ast.new_number(Numeric::I64(0), vec![]); // allows coercion to I64
        assert_eq!(AST::new_group(
            vec![const_a.clone(),
                 AST::new_group(vec![const_b, const_a], vec![])], vec![]).get_type(),
                   &ExprType::outs(vec![I32, I64, I32]));
    }
}
