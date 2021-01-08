use std::collections::HashSet;
use std::convert::TryInto;
use std::sync::mpsc::channel;

use wasmin::{*};
use wasmin::ast::{*};
use wasmin::parse::new_parser;
use wasmin::types::{Type::*};

#[test]
fn test_let() {
    let mut chars = "let x = 1; let y = (let z = 2; z) pub let PI = 3.14".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(), top_let!("x" = expr_const!("1" I32)));
        assert_eq!(rcv.iter().next().unwrap(), top_let!("y" =
        expr_group!(expr_let!("z" = expr_const!("2" I32)) expr_local!("z" I32))));
        assert_eq!(rcv.iter().next().unwrap(), top_let!(p "PI" = expr_const!("3.14" F32)));
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
                   top_let!("x", "y", "z" = expr_const!("1" I32), expr_const!("0.1" F32), expr_const!("10i64" I64)));
        assert_eq!(rcv.iter().next().unwrap(),
                   top_let!(p "PI", "E" = expr_const!("3.14" F32), expr_const!("2.16f64" F64)));
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
                   top_let!("foo" = expr_const!("0" I32)));
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
                   top_fun!(def fun_type!([]());
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
                   top_fun!(def fun_type!([I32](I32));
                              fun "f" "x" = expr_local!("x" I32)));
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
                   top_fun!(def fun_type!([I32 I64](I64 I32));
                              p fun "swap" "x" "y" = expr_multi!(expr_local!("y" I64), expr_local!("x" I32))));
    }

    assert_eq!(rcv.iter().next(), None);
}

#[test]
fn test_def_multi_fun() {
    let mut chars = "def foo []i32; fun foo = 0; def foo [i32]i32; fun foo n=n;".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(),
                   top_fun!(def fun_type!([](I32));
                              fun "foo" = expr_const!("0" I32)));

        assert_eq!(rcv.iter().next().unwrap(),
                   top_fun!(def fun_type!([I32](I32));
                              fun "foo" "n" = expr_local!("n" I32)));
    }

    assert_eq!(rcv.iter().next(), None);
}

#[test]
fn test_use_std_wasm_arithmetics() {
    let mut chars = "let x = add 2 2; let y = mul 3i64 4i64; let z = sub 1.0 0.1;".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(),
                   top_let!("x" = expr_fun_call!(wasm "add" expr_const!("2" I32) expr_const!("2" I32);
                        fun_type!([I32 I32](I32)))));

        assert_eq!(rcv.iter().next().unwrap(),
                   top_let!("y" = expr_fun_call!(wasm "mul" expr_const!("3i64" I64) expr_const!("4i64" I64);
                        fun_type!([I64 I64](I64)))));

        assert_eq!(rcv.iter().next().unwrap(),
                   top_let!("z" = expr_fun_call!(wasm "sub" expr_const!("1.0" F32) expr_const!("0.1" F32);
                        fun_type!([F32 F32](F32)))));
    }

    assert_eq!(rcv.iter().next(), None);
}

#[test]
fn test_global_mut_set() {
    let mut chars = "mut i = 0; def f []i32; fun f = (set i = add i 1; i)".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        assert_eq!(rcv.iter().next().unwrap(),
                   top_mut!("i" = expr_const!("0" I32)));

        assert_eq!(rcv.iter().next().unwrap(),
                   top_fun!(def fun_type!([](I32)); fun "f" = expr_group!(
                       expr_set!("i" = expr_fun_call!(wasm "add"
                            expr_global!("i" I32) expr_const!("1" I32); fun_type!([I32 I32](I32))); true)
                       expr_global!("i" I32)
                   )));
    }

    assert_eq!(rcv.iter().next(), None);
}

#[test]
fn test_ext_module() {
    let mut chars = "\
        ext console {
            log [i32];
            log [f32];
            warn [i32 i32];
            a_number i32;
        }
    ".chars();
    let (sender, rcv) = channel();

    // let the sender "drop" so the channel is closed
    {
        let mut parser = new_parser(&mut chars, sender);
        parser.parse();

        match rcv.iter().next().unwrap() {
            TopLevelElement::Ext(mod_name, mut defs, _) => {
                assert_eq!(mod_name, "console");
                assert_eq!(defs.len(), 3);
                let actual_defs: HashSet<_> = defs.drain(..).collect();

                let mut expected_defs = HashSet::new();

                expected_defs.insert(ExtDef {
                    id: "log".to_owned(),
                    typ: Fn(vec![fun_type!([I32]()), fun_type!([F32]())]),
                });
                expected_defs.insert(ExtDef {
                    id: "warn".to_owned(),
                    typ: Fn(vec![fun_type!([I32 I32]())]),
                });
                expected_defs.insert(ExtDef {
                    id: "a_number".to_owned(),
                    typ: I32,
                });

                assert_eq!(actual_defs, expected_defs);
            }
            e => {
                panic!(format!("Did not get Ext, got {:?}", e));
            }
        }
    }

    assert_eq!(rcv.iter().next(), None);
}
