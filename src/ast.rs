use std::cmp::min;
use std::ops::Range;
use std::sync::{Arc, Mutex};

use crate::errors::WasminError;
use crate::interner::{*};
use crate::parse::model::{Numeric, Position};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type {
    I64,
    I32,
    F64,
    F32,
    String,
    Empty,
    FunType(ExprType),
    Custom(InternedStr),
    Error(WasminError),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ExprType {
    pub ins: Vec<Type>,
    pub outs: Vec<Type>,
}

pub enum IdKind {
    Var(Type),
    Fun,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Def {
    pub name: InternedStr,
    pub target_type: Option<Type>,
}

/// Assignment defines one or more Wasmin assignments.
///
/// It is represented as a tuple with the following contents:
/// * variable names
/// * variable expression (if more than one value, will be a [Group] of [Expression]s).
/// * optional type replacements (for implicit type conversions)
#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    pub vars: Vec<Def>,
    pub expr: Box<Expression>,
}

impl Assignment {
    pub fn get_types(&self) -> Vec<(Def, Type)> {
        let mut result = Vec::with_capacity(self.vars.len());
        let expr_types = &self.expr.get_type().outs;
        for (var, expr_t) in self.vars.iter().zip(expr_types) {
            if let Some(t) = &var.target_type {
                result.push((var.clone(), t.clone()));
            } else {
                result.push((var.clone(), expr_t.clone()));
            }
        }
        result
    }
}

/// Reassignment is an [`Assignment`] of one or more mutable variables.
/// The variables may be local or global. The [`globals`] field determines which is the case
/// for each variable.
#[derive(Debug, PartialEq, Clone)]
pub struct ReAssignment {
    pub assignment: Assignment,
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
pub type Comment = String;

/// Warning emitted by the Wasmin compiler.
pub type Warning = String;

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    String(InternedStr),
    Number(Numeric),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunKind {
    Wasm,
    Custom { fun_index: usize },
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
    Group {
        exprs: Vec<Expression>,
        typ: ExprType,
        warnings: Vec<Warning>,
    },
    FunCall {
        name: InternedStr,
        typ: ExprType,
        kind: FunKind,
        warnings: Vec<Warning>,
    },
    ExprError(WasminError, Vec<Warning>),
}

/// Function defines a function implementation.
#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: InternedStr,
    pub arg_names: Vec<InternedStr>,
    pub body: Expression,
    pub typ: ExprType,
    pub fun_index: usize,
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

const EMPTY_EXPR_TYPE: &'static ExprType = &ExprType { ins: vec![], outs: vec![] };

fn end_range_of_len(range_len: usize, vec_len: usize) -> Range<usize> {
    (vec_len - range_len)..vec_len
}

fn merge_types(mut types: Vec<ExprType>) -> (ExprType, Vec<Warning>) {
    let mut ins: Vec<Type> = Vec::with_capacity(types.len());
    let mut outs: Vec<Type> = Vec::with_capacity(types.len());
    let mut errs: Vec<Warning> = Vec::new();
    for typ in types.iter_mut() {
        let annihilate_len = min(typ.ins.len(), outs.len());
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
            Expression::Group { typ, .. } => typ,
            Expression::If { yes, .. } => yes.get_type(),
        }
    }
}

impl From<WasminError> for Expression {
    fn from(value: WasminError) -> Self {
        Expression::ExprError(value, vec![])
    }
}

#[derive(Debug)]
pub struct AST {
    interner: Arc<Mutex<Interner>>,
    fun_index: usize,
}

impl AST {
    pub fn intern(&mut self, s: &str) -> InternedStr {
        self.interner.lock().unwrap().intern(s)
    }

    pub fn new() -> Self { AST { interner: Arc::new(Mutex::new(Interner::new())), fun_index: 0 } }

    pub fn interned_strings(&self) -> Arc<Mutex<Interner>> {
        self.interner.clone()
    }

    fn build_type_string(&self, types: &Vec<Type>, result: &mut String) {
        let len = types.len();
        for arg in types.iter().take(len - 1) {
            result.push_str(&self.type_to_string(arg));
            result.push(' ');
        }
        if len > 0 {
            result.push_str(&self.type_to_string(types.last().unwrap()));
        }
    }

    pub fn type_to_string(&self, typ: &Type) -> String {
        match typ {
            Type::I64 => "i64".to_owned(),
            Type::I32 => "i32".to_owned(),
            Type::F64 => "f64".to_owned(),
            Type::F32 => "f32".to_owned(),
            Type::String => "string".to_owned(),
            Type::Empty => "()".to_owned(),
            Type::FunType(e, ..) => {
                let mut result = String::with_capacity(32);
                result.push('[');
                self.build_type_string(&e.ins, &mut result);
                result.push(']');
                result.push('(');
                self.build_type_string(&e.outs, &mut result);
                result.push(')');
                result
            }
            Type::Custom(name) => self.interned_str(name),
            Type::Error(err) => err.cause().to_owned(),
        }
    }

    pub fn interned_str(&self, interned: &InternedStr) -> String {
        self.interner.lock().unwrap().get(interned).to_owned()
    }

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

    pub fn new_if(cond: Expression, yes: Expression, no: Expression, warnings: Vec<Warning>) -> Expression {
        let cond = Box::new(cond);
        let yes = Box::new(yes);
        let no = Box::new(no);
        let typ = yes.get_type().clone();
        Expression::If { cond, yes, no, typ, warnings }
    }

    pub fn new_fun_call(&mut self, name: InternedStr, typ: ExprType, kind: FunKind, warnings: Vec<Warning>) -> Expression {
        Expression::FunCall { name, typ, kind, warnings }
    }

    pub fn new_string(&mut self, value: &str, w: Vec<Warning>) -> Expression {
        let typ = Type::String;
        let expr_type = ExprType::outs(vec![typ.clone()]);
        Expression::Const(Constant::String(self.intern(value)), typ, expr_type, w)
    }

    pub fn new_number(value: Numeric, w: Vec<Warning>) -> Expression {
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
        Expression::Group { exprs, typ, warnings }
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

    pub fn new_assignments(&mut self, vars: Vec<(String, Option<Type>)>, expr: Expression) -> Assignment {
        // TODO typecheck the assignments
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

    pub fn new_type(&mut self, name: &str) -> Type {
        match name {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => Type::Custom(self.intern(name))
        }
    }

    pub fn new_fun(&mut self, name: &str, args: Vec<String>,
                   body: Expression, typ: ExprType) -> Function {
        let interned_name = self.intern(name);
        self.new_fun_interned(interned_name, args, body, typ)
    }

    pub fn new_fun_interned(&mut self, name: InternedStr, args: Vec<String>,
                            body: Expression, typ: ExprType) -> Function {
        let index = self.fun_index;
        self.fun_index = index + 1;
        self.new_fun_interned_with_index(name, args, body, typ, index)
    }

    pub fn new_fun_interned_with_index(
        &mut self, name: InternedStr, mut args: Vec<String>,
        body: Expression, typ: ExprType, fun_index: usize) -> Function {
        let arg_names: Vec<InternedStr> = args.drain(..)
            .map(|a| self.intern(&a))
            .collect();
        Function {
            name,
            arg_names,
            body,
            typ,
            fun_index,
        }
    }
}

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
        let mut ast = AST::new();

        assert_eq!(AST::empty().get_type(), EMPTY_EXPR_TYPE);

        assert_eq!(AST::new_number(Numeric::I32(0), vec![]).get_type(),
                   &ExprType::outs(vec![I32]));

        assert_eq!(ast.new_local("foo", I64, vec![]).get_type(),
                   &ExprType::outs(vec![I64]));

        assert_eq!(ast.new_global("foo", F64, vec![]).get_type(),
                   &ExprType::outs(vec![F64]));

        let e1 = AST::new_number(Numeric::F32(0.0), vec![]);
        assert_eq!(AST::new_let(ast.new_assignment("", None, e1.clone()), vec![])
                       .get_type(), EMPTY_EXPR_TYPE);

        assert_eq!(AST::new_mut(ast.new_assignment("", None, e1.clone()), vec![])
                       .get_type(), EMPTY_EXPR_TYPE);

        assert_eq!(AST::new_set(ast.new_assignment("", None, e1.clone()), vec![false], vec![])
                       .get_type(), EMPTY_EXPR_TYPE);

        let const_a = ast.new_string("a", vec![]);
        let const_b = AST::new_number(Numeric::I64(0), vec![]); // allows coercion to I64
        assert_eq!(AST::new_group(vec![const_a, const_b], vec![]).get_type(),
                   &ExprType::outs(vec![String, I64]));

        let const_a = AST::new_number(Numeric::I32(0), vec![]);
        let const_b = AST::new_number(Numeric::I64(0), vec![]); // allows coercion to I64
        assert_eq!(AST::new_group(
            vec![const_a.clone(),
                 AST::new_group(vec![const_b, const_a], vec![])], vec![]).get_type(),
                   &ExprType::outs(vec![I32, I64, I32]));
    }
}
