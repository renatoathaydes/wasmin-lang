use std::convert::TryInto;
use std::sync::mpsc::channel;

use wasmin::{*};
use wasmin::ast::{*};
use wasmin::parse::new_parser;
use wasmin::types::Type::{*};

#[test]
fn test_let() {
    let mut chars = "let x = 1; let y = (let z = 2; z) pub let PI = 3.14".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(), texpr_let!("x" = expr_const!("1" I32)));
        assert_eq!(rcv.iter().next().unwrap(), texpr_let!("y" =
        expr_group!(expr_let!("z" = expr_const!("2" I32)) expr_var!("z" I32))));
        assert_eq!(rcv.iter().next().unwrap(), texpr_let!(p "PI" = expr_const!("3.14" F32)));
    }

    assert_eq!(rcv.iter().next(), None);
}

#[test]
fn test_multi_let() {
    let mut chars = "let x, y, z = 1, 0.1, 10i64; pub let PI, E = 3.14, 2.16f64".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(),
                   texpr_let!("x", "y", "z" = expr_const!("1" I32), expr_const!("0.1" F32), expr_const!("10i64" I64)));
        assert_eq!(rcv.iter().next().unwrap(),
                   texpr_let!(p "PI", "E" = expr_const!("3.14" F32), expr_const!("2.16f64" F64)));
    }

    assert_eq!(rcv.iter().next(), None);
}

#[test]
fn test_def_then_let() {
    let mut chars = "def foo i32; let foo = 0; def bar i64; let bar = 3".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(),
                   texpr_let!("foo" = expr_const!("0" I32)));
        assert_eq!(rcv.iter().next().unwrap(),
                   expr_let!("bar" = expr_const!("3" I32); Some(I64)).try_into().unwrap());
    }

    assert_eq!(rcv.iter().next(), None);
}

#[test]
fn test_fun_0_args() {
    let mut chars = "def nothing []; fun nothing = ()".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(),
                   texpr_fun!(def fn_type!([]());
                              fun "nothing" = expr_empty!()));
    }

    assert_eq!(rcv.iter().next(), None);
}

#[test]
fn test_fun_1_arg() {
    let mut chars = "def f [i32]i32; fun f x = x".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(),
                   texpr_fun!(def fn_type!([I32](I32));
                              fun "f" "x" = expr_var!("x" I32)));
    }

    assert_eq!(rcv.iter().next(), None);
}

#[test]
fn test_fun_2_arg_multi_value() {
    let mut chars = "def swap [i32 i64] i64 i32; pub fun swap x y = y, x".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(),
                   texpr_fun!(def fn_type!([I32 I64](I64 I32));
                              p fun "swap" "x" "y" = expr_multi!(expr_var!("y" I64), expr_var!("x" I32))));
    }

    assert_eq!(rcv.iter().next(), None);
}