use crate::ast::{Expression, ReAssignment};
use crate::ast::Expression::If;
use crate::types::{TypeError};
use crate::types::Type::*;

#[test]
fn test_empty_get_type() {
    assert_eq!(Expression::Empty.get_type(), vec![]);
}

#[test]
fn test_const_get_type() {
    assert_eq!(Expression::Const("1".to_owned(), I32).get_type(), vec![I32]);
    assert_eq!(Expression::Const("1".to_owned(), I64).get_type(), vec![I64]);
}

#[test]
fn test_global_get_type() {
    assert_eq!(Expression::Global("1".to_owned(), I32).get_type(), vec![I32]);
    assert_eq!(Expression::Global("1".to_owned(), I64).get_type(), vec![I64]);
}

#[test]
fn test_local_get_type() {
    assert_eq!(Expression::Local("1".to_owned(), I32).get_type(), vec![I32]);
    assert_eq!(Expression::Local("1".to_owned(), I64).get_type(), vec![I64]);
}

#[test]
fn test_let_get_type() {
    assert_eq!(Expression::Let((vec!["a".to_owned()], Box::new(expr_group!(expr_const!("1" I32))), vec![None])).get_type(), vec![]);
    assert_eq!(Expression::Let((vec!["b".to_owned()], Box::new(expr_group!(expr_const!("1" I32))), vec![Some(I64)])).get_type(), vec![]);
    assert_eq!(Expression::Let((vec!["b".to_owned(), "c".to_owned()],
                                Box::new(expr_group!(expr_const!("1" I32) expr_const!("1" I32))), vec![Some(I64), None])).get_type(), vec![]);
}

#[test]
fn test_mut_get_type() {
    assert_eq!(Expression::Mut((vec!["a".to_owned()], Box::new(expr_group!(expr_const!("1" I32))), vec![None])).get_type(), vec![]);
    assert_eq!(Expression::Mut((vec!["b".to_owned()], Box::new(expr_group!(expr_const!("1" I32))), vec![Some(I64)])).get_type(), vec![]);
    assert_eq!(Expression::Mut((vec!["b".to_owned(), "c".to_owned()],
                                Box::new(expr_group!(expr_const!("1" I32) expr_const!("1" I32))), vec![Some(I64), None])).get_type(), vec![]);
}

#[test]
fn test_set_get_type() {
    assert_eq!(Expression::Set(ReAssignment {
        assignment: (vec!["a".to_owned()], Box::new(expr_group!(expr_const!("1" I32))), vec![None]),
        globals: vec![false],
    }).get_type(), vec![]);
    assert_eq!(Expression::Set(ReAssignment {
        assignment: (vec!["a".to_owned(), "b".to_owned()],
                     Box::new(expr_group!(expr_const!("1" I32) expr_const!("1" I32))), vec![None, Some(I64)]),
        globals: vec![true, true],
    }).get_type(), vec![]);
}

#[test]
fn test_if_simple_get_type() {
    assert_eq!(If(Box::new(expr_const!("1" I32)),
                  Box::new(expr_const!("2" I32)),
                  Box::new(expr_const!("3" I32))).get_type(), vec![I32]);
}

#[test]
fn test_if_multi_value_get_type() {
    assert_eq!(If(Box::new(expr_const!("1" I32)),
                  Box::new(expr_group!(expr_const!("2" I32) expr_const!("4" I64))),
                  Box::new(expr_group!(expr_const!("2" I32) expr_const!("4" I64)))).get_type(),
               vec![I32, I64]);
}

#[test]
fn test_group_get_type() {
    assert_eq!(Expression::Group(vec![]).get_type(), vec![]);
    assert_eq!(Expression::Group(vec![expr_const!("1" I32)]).get_type(), vec![I32]);
    assert_eq!(Expression::Group(vec![expr_const!("1" F64)]).get_type(), vec![F64]);
    assert_eq!(Expression::Group(vec![expr_const!("1" I32), expr_const!("1" F64)]).get_type(), vec![I32, F64]);
}

#[test]
fn test_group_multi_get_type() {
    assert_eq!(Expression::Group(vec![expr_group!(expr_const!("1" I32) expr_const!("1" F64))]).get_type(),
               vec![I32, F64]);
    assert_eq!(Expression::Group(vec![
        expr_const!("1" I64),
        expr_group!(expr_const!("1" I64) expr_const!("1" F32))]).get_type(),
               vec![I64, I64, F32]);
}

#[test]
fn test_multi_get_type() {
    assert_eq!(Expression::Group(vec![]).get_type(), vec![]);
    assert_eq!(Expression::Group(vec![expr_const!("1" I32)]).get_type(), vec![I32]);
    assert_eq!(Expression::Group(vec![expr_const!("1" I32), expr_const!("1" F64)]).get_type(), vec![I32, F64]);
}

#[test]
fn test_multi_nested_get_type() {
    assert_eq!(Expression::Group(vec![
        expr_group!(expr_const!("1" I32) expr_const!("1" F64)),
        expr_group!(expr_const!("1" I64) expr_const!("1" F32))
    ]).get_type(), vec![I32, F64, I64, F32]);
}

#[test]
fn test_fun_call_get_type() {
    assert_eq!(expr_fun_call!("a" []()).get_type(), vec![]);
    assert_eq!(expr_fun_call!("a" [I32]()).get_type(), vec![]);
    assert_eq!(expr_fun_call!("a" [](I32)).get_type(), vec![I32]);
    assert_eq!(expr_fun_call!("a" [](I32 F32)).get_type(), vec![I32, F32]);
}

#[test]
fn test_error_get_type() {
    assert_eq!(Expression::ExprError(TypeError {
        reason: "wrong".to_owned(),
        pos: (10, 20),
    }).get_type(), vec![Error(TypeError {
        reason: "wrong".to_owned(),
        pos: (10, 20),
    })])
}
