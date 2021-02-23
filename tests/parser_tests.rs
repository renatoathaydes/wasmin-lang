use std::collections::HashSet;
use std::sync::mpsc::channel;

use wasmin::{*};
use wasmin::ast::{*};
use wasmin::errors::WasminError;
use wasmin::parse::new_parser;
use wasmin::types::{Type::*};

macro_rules! test_parser {
    ($program:literal ; $($ns:literal: $( $id:literal => $ns_expr:expr ),+ )* ; $($e:expr),+) => {{
        let mut chars = $program.chars();
        let (sender, rcv) = channel();

        // let the sender "drop" so the channel is closed
        {
            let mut parser = new_parser(&mut chars, sender);
            $(
                let namespace = $ns.to_owned();
                let mut ns_defs = Vec::new();
                $(
                    ns_defs.push(($id.to_owned(), $ns_expr));
                )*
                parser.stack_mut().push_namespace(namespace, ns_defs).unwrap();
            )*
            parser.parse();
            $(
                assert_eq!(rcv.iter().next().unwrap(), $e);
            )+
        }
        assert_eq!(rcv.iter().next(), None);
    }};
}

#[test]
fn test_let() {
    test_parser!("let x = 1; let y = (let z = 2; z) pub let PI = 3.14";;
        top_let!("x" = expr_const!("1" I32)),
        top_let!("y" = expr_group!(expr_let!("z" = expr_const!("2" I32)) expr_local!("z" I32))),
        top_let!(p "PI" = expr_const!("3.14" F32))
    );
}

#[test]
fn test_multi_let() {
    test_parser!("let x, y, z = 1, 0.1, 10i64; pub let PI, E = 3.14, 2.16f64";;
        top_let!("x", "y", "z" =
            expr_const!("1" I32), expr_const!("0.1" F32), expr_const!("10i64" I64)),
            top_let!(p "PI", "E" = expr_const!("3.14" F32), expr_const!("2.16f64" F64))
        );
}

#[test]
fn test_def_then_let() {
    let bar = TopLevelElement::Let((vec!["bar".to_owned()],
                                    Box::new(expr_const!("3" I32)), vec![Some(I64)]),
                                   Visibility::Private, None);

    test_parser!("def foo i32; let foo = 0; def bar i64; let bar = 3";;
        top_let!("foo" = expr_const!("0" I32)),
        bar
    );
}

#[test]
fn test_fun_0_args() {
    test_parser!("def nothing []; fun nothing = ()";;
       top_fun!(def fun_type!([]()); fun "nothing" = expr_empty!()));
}

#[test]
fn test_fun_1_arg() {
    test_parser!("def f [i32]i32; fun f x = x";;
       top_fun!(def fun_type!([I32](I32)); fun "f" "x" = expr_local!("x" I32)));
}

#[test]
fn test_fun_2_arg_multi_value() {
    test_parser!("def swap [i32 i64] i64 i32; pub fun swap x y = y, x";;
       top_fun!(def fun_type!([I32 I64](I64 I32));
          p fun "swap" "x" "y" = expr_group!(expr_local!("y" I64) expr_local!("x" I32))));
}

#[test]
fn test_def_multi_fun() {
    test_parser!("def foo []i32; fun foo = 0; def foo [i32]i32; fun foo n=n;";;
        top_fun!(def fun_type!([](I32));
                  fun "foo" = expr_const!("0" I32)),
        top_fun!(def fun_type!([I32](I32));
                  fun "foo" "n" = expr_local!("n" I32))
    );
}

#[test]
fn test_fun_without_def_calling_namespace_fun() {
    test_parser!("fun _start = console.log 42;";
        "console": "log" => Fn(vec![fun_type!([I32]())]);
        top_fun!(def fun_type!([]());
            fun "_start" = expr_group!(
                expr_const!(42 I32)
                expr_fun_call!("console.log" [I32]())))
    );
}

#[test]
fn test_fun_without_def_must_not_return_value() {
    test_parser!("fun _start = add 1 2;";;
        TopLevelElement::Error(werr_syntax!(
        "fun '_start' missing def (body returns a value of type 'i32', hence the return type is mandatory)",
        (0, 4)))
    );
}

#[test]
fn test_use_std_wasm_arithmetics() {
    test_parser!("let x = add 2 2; let y = mul 3i64 4i64; let z = sub 1.0 0.1;";;
       top_let!("x" = expr_group!(expr_const!("2" I32)
                                  expr_const!("2" I32)
                                  expr_fun_call!(wasm "add" [I32 I32](I32)))),
       top_let!("y" = expr_group!(expr_const!("3i64" I64)
                                  expr_const!("4i64" I64)
                                  expr_fun_call!(wasm "mul" [I64 I64](I64) ; 1))),
       top_let!("z" = expr_group!(expr_const!("1.0" F32)
                                  expr_const!("0.1" F32)
                                  expr_fun_call!(wasm "sub" [F32 F32](F32) ; 2)))
    );
}

#[test]
fn test_global_mut_set() {
    test_parser!("mut i = 0; def f []i32; fun f = (set i = add i 1; i)";;
        top_mut!("i" = expr_const!("0" I32)),
        top_fun!(def fun_type!([](I32)); fun "f" = expr_group!(
            expr_set!("i" = expr_group!(
                           expr_global!("i" I32)
                           expr_const!("1" I32)
                           expr_fun_call!(wasm "add" [I32 I32](I32))); true)
            expr_global!("i" I32)
       ))
    );
}

#[test]
fn test_fun_with_conditionals() {
    test_parser!("\
        def factorial [i32]i32;
        pub fun factorial n = if n, le_s 2;
          then n;
          else n, mul (n, sub 1, factorial);
        ";;
        top_fun!(def fun_type!([I32](I32));
            p fun "factorial" "n" =
                expr_if!(expr_group!(
                    expr_local!("n" I32)
                    expr_const!("2" I32)
                    expr_fun_call!(wasm "le_s" [I32 I32](I32))
                );
                expr_local!("n" I32);
                expr_group!(
                    expr_local!("n" I32)
                    expr_local!("n" I32)
                    expr_const!("1" I32)
                    expr_fun_call!(wasm "sub" [I32 I32](I32))
                    expr_fun_call!("factorial" [I32](I32))
                    expr_fun_call!(wasm "mul" [I32 I32](I32))
                ))
    ));
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
            TopLevelElement::Ext(mod_name, mut defs, _, _) => {
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

#[test]
fn test_let_and_fun_with_multiline_comment() {
    test_parser!( "\n#this is 10\nlet n = 10;\n\
       #{   \nthis is a noop fun\n  }\nfun start = () fun no-comments = ()";;
       TopLevelElement::Let(
            (vec!["n".to_owned()], Box::new(expr_const!("10" I32)), vec![None]),
            Visibility::Private, Some("this is 10".to_owned())),
       TopLevelElement::Fun(
            ("start".to_owned(), vec![], Expression::Empty, fun_type!([]())),
            Visibility::Private, Some("   \nthis is a noop fun\n  ".to_owned())),
       TopLevelElement::Fun(
            ("no-comments".to_owned(), vec![], Expression::Empty, fun_type!([]())),
            Visibility::Private, None)
    );
}
