use crate::ast::{*, Expression::*};
use crate::parse::{expr_parser, new_parser_with_stack, new_parser_without_sink};
use crate::parse::parser::{*};
use crate::types::{*, Type::*};

macro_rules! parse_expr {
    ($e:expr) => {{
        let mut chars = $e.chars();
        let mut parser = new_parser_without_sink(&mut chars);
        parser.parse_expr()
    }};
    ($e:expr, $($id:expr => $typ:expr),+) => {{
        let mut chars = $e.chars();
        let mut stack = Stack::new();
        $(stack.push_item($id.to_string(), $typ);)*
        let mut parser = new_parser_with_stack(&mut chars, stack);
        parser.parse_expr()
    }};
}

macro_rules! type_of {
    ($e:expr) => {{
        let stack = Stack::new();
        expr_parser::type_of($e, &stack)
    }};
    ($e:expr, $($id:expr => $typ:expr),+) => {{
        let mut stack = Stack::new();
        $(stack.push_item($id.to_string(), $typ);)*
        expr_parser::type_of($e, &stack)
    }};
}

#[test]
fn test_type_of_empty() {
    assert_eq!(type_of!(""), Ok(Type::Empty));
}

#[test]
fn test_type_of_var() {
    assert_eq!(type_of!("foo", "foo" => F32), Ok(F32));
    assert_eq!(type_of!("bar", "foo" => F32),
               Err("variable \'bar\' does not exist".to_string()));
    assert_eq!(type_of!("bar", "foo" => F32, "bar" => I64), Ok(I64));

    // funny variable names
    assert_eq!(type_of!("--", "++" => F32, "--" => I64), Ok(I64));
    assert_eq!(type_of!("++", "++" => F32, "--" => I64), Ok(F32));
}

#[test]
fn test_type_of_int() {
    // max value of i32 is "2147483647", anything with more digits is classified as i64
    for i in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "10", "11", "20", "100",
        "1_000", "111_222_333", "1_2_3_4_0", "2147483647",
        "-1", "-0", "-1_2", "-98_76_43"].iter() {
        assert_eq!(type_of!(i), Ok(I32), "Example: {}", i);
    }
    for i in ["21474836478", "12345678900", "123_456_789_012_345_678_900"].iter() {
        assert_eq!(type_of!(i), Ok(I64), "Example: {}", i);
    }

    assert_eq!(type_of!("123z"), Err("number contains invalid character: 'z'".to_string()));
    assert_eq!(type_of!("123!0123"), Err("number contains invalid character: '!'".to_string()));
    assert_eq!(type_of!("0123"), Err("non-zero integer cannot start with zero".to_string()));
    assert_eq!(type_of!("000"), Err("number cannot start with more than one zero".to_string()));
    assert_eq!(type_of!("0_0"), Err("number cannot start with more than one zero".to_string()));
}

#[test]
fn test_type_of_float() {
    for f in ["0.1", "2.0", "1.3", "3.14151695", "2345.67890",
        "1_000.0", "0.1_111_222_3", "9_1_2_.3_4", "1.0000000",
        "-0.1", "-1_.000", "-3.141516956"].iter() {
        assert_eq!(type_of!(f), Ok(F32), "Example: {}", f);
    }
    for f in ["0.1111111111", "2.000000000000", "3.141_592_653_589_793"].iter() {
        assert_eq!(type_of!(f), Ok(F64), "Example: {}", f);
    }

    assert_eq!(type_of!("0.0.0"), Err("number contains more than one dot".to_string()));
    assert_eq!(type_of!("0.0."), Err("number contains more than one dot".to_string()));
    assert_eq!(type_of!("0."), Err("number cannot end with dot".to_string()));
    assert_eq!(type_of!("1z"), Err("number contains invalid character: 'z'".to_string()));
    assert_eq!(type_of!("00.1"), Err("number cannot start with more than one zero".to_string()));
    assert_eq!(type_of!("0_012.3"), Err("number cannot start with more than one zero".to_string()));
}

#[test]
fn test_type_of_num_explicit() {
    // max value is "2147483647", anything with less digits is classified as i32
    assert_eq!(type_of!("2147483647i32"), Ok(I32));
    assert_eq!(type_of!("-2147483646i32"), Ok(I32));
    assert_eq!(type_of!("0i32"), Ok(I32));
    assert_eq!(type_of!("-0i32"), Ok(I32));
    assert_eq!(type_of!("0i64"), Ok(I64));
    assert_eq!(type_of!("-0i64"), Ok(I64));
    assert_eq!(type_of!("512i64"), Ok(I64));
    assert_eq!(type_of!("-512i64"), Ok(I64));

    assert_eq!(type_of!("0f32"), Ok(F32));
    assert_eq!(type_of!("-0f32"), Ok(F32));
    assert_eq!(type_of!("0f64"), Ok(F64));
    assert_eq!(type_of!("-0f64"), Ok(F64));
    assert_eq!(type_of!("10f32"), Ok(F32));
    assert_eq!(type_of!("-10f32"), Ok(F32));
    assert_eq!(type_of!("10_000_f64"), Ok(F64));
    assert_eq!(type_of!("-10_000_f64"), Ok(F64));
    assert_eq!(type_of!("0.5_f64"), Ok(F64));
    assert_eq!(type_of!("-0.5_f64"), Ok(F64));
    assert_eq!(type_of!("12.__5678__f64"), Ok(F64));
    assert_eq!(type_of!("-12.__5678__f64"), Ok(F64));
}

#[test]
fn test_empty() {
    assert_eq!(parse_expr!("()"), Expression::Empty);
    assert_eq!(parse_expr!("(())"), Expression::Empty);
    assert_eq!(parse_expr!("(( (  ) ) )"), Expression::Empty);
}

#[test]
fn test_expr_i32() {
    assert_eq!(parse_expr!("(0)"), Const(String::from("0"), I32));
    assert_eq!(parse_expr!("( 1 )"), Const(String::from("1"), I32));
    assert_eq!(parse_expr!("( 100)"), Const(String::from("100"), I32));
    assert_eq!(parse_expr!("( 100_000 )"), Const(String::from("100_000"), I32));
}

#[test]
fn test_expr_f32() {
    assert_eq!(parse_expr!("(0.0)"), Const(String::from("0.0"), F32));
    assert_eq!(parse_expr!("( 1.0 )"), Const(String::from("1.0"), F32));
    assert_eq!(parse_expr!("( 1.00)"), Const(String::from("1.00"), F32));
    assert_eq!(parse_expr!("( 1_0.0_0)"), Const(String::from("1_0.0_0"), F32));
}

#[test]
fn test_expr_non_parens() {
    assert_eq!(parse_expr!("0.0;"), Const(String::from("0.0"), F32));
    assert_eq!(parse_expr!("  1_000; "), Const(String::from("1_000"), I32));
    assert_eq!(parse_expr!(" 0  ; "), Const(String::from("0"), I32));
    assert_eq!(parse_expr!("  1_000; "), Const(String::from("1_000"), I32));
}

#[test]
fn test_expr_single_item_nested() {
    assert_eq!(parse_expr!("((0.0))"), Const(String::from("0.0"), F32));
    assert_eq!(parse_expr!("(;;1)"), Const(String::from("1"), I32));
    assert_eq!(parse_expr!("(((((  42 )))))"), Const(String::from("42"), I32));
    assert_eq!(parse_expr!("( ( ; ; ) (4) )"), Const(String::from("4"), I32));
}

#[test]
fn test_fn_call_basic() {
    assert_eq!(parse_expr!("do-it 2;", "do-it" => Fn(FnType { ins: vec![I32], outs: vec![] } )),
               Expression::fn_call("do-it", vec![
                   Const(String::from("2"), I32)], vec![]));

    assert_eq!(parse_expr!("add 2 2;", "add" => Fn(FnType { ins: vec![I32, I32], outs: vec![I64] } )),
               Expression::fn_call("add", vec![
                   Const(String::from("2"), I32), Const(String::from("2"), I32)
               ], vec![I64]));

    assert_eq!(parse_expr!("(div-rem 4 2)", "div-rem" => Fn(FnType { ins: vec![I32, I32], outs: vec![I32, I32] } )),
               Expression::fn_call("div-rem", vec![
                   Const(String::from("4"), I32), Const(String::from("2"), I32)
               ], vec![I32, I32]));

    assert_eq!(parse_expr!("(print 0.0)"),
               Expression::fn_call("print",
                                   vec![Const(String::from("0.0"), F32)],
                                   vec![TypeError { reason: "Unknown function: 'print'".to_string(), pos: (0, 11) }]));

    // function without args: only invoke the function if it's alone within parens
    assert_eq!(parse_expr!("no-args;", "no-args" => Fn(FnType { ins: vec![], outs: vec![I32] } )),
               Const(String::from("no-args"), Fn(FnType { ins: vec![], outs: vec![I32] })));

    assert_eq!(parse_expr!("(no-args);", "no-args" => Fn(FnType { ins: vec![], outs: vec![I32] } )),
               Expression::fn_call("no-args", vec![], vec![I32]));
}

#[test]
fn test_expr_multi_value() {
    assert_eq!(parse_expr!("0,1"), Multi(vec![Const(
        String::from("0"), I32), Const(String::from("1"), I32)]));
    assert_eq!(parse_expr!("(1, 2, 3.0)"), Multi(vec![
        Const(String::from("1"), I32), Const(String::from("2"), I32), Const(String::from("3.0"), F32)]));
    assert_eq!(parse_expr!("1, (2, 3)"), Multi(vec![
        Const(String::from("1"), I32), Multi(vec![
            Const(String::from("2"), I32), Const(String::from("3"), I32)])]));
    assert_eq!(parse_expr!("((1, 2), 3)"), Multi(vec![Multi(vec![
        Const(String::from("1"), I32), Const(String::from("2"), I32)]),
                                                      Const(String::from("3"), I32)]));
    assert_eq!(parse_expr!("((1, 2) 3)"), Group(vec![Multi(vec![
        Const(String::from("1"), I32), Const(String::from("2"), I32)]),
                                                     Const(String::from("3"), I32)]));
}

#[test]
fn test_word() {
    let mut chars = "abc".chars();
    let mut parser = new_parser_without_sink(&mut chars);
    assert_eq!(parser.parse_word(), Some("abc".to_string()));
}

#[test]
fn test_words() {
    let mut chars = "a b  cde    fgh\nij\n  klmnop  \r\n  rs  ".chars();
    let mut parser = new_parser_without_sink(&mut chars);
    assert_eq!(parser.parse_word(), Some("a".to_string()));
    assert_eq!(parser.parse_word(), Some("b".to_string()));
    assert_eq!(parser.parse_word(), Some("cde".to_string()));
    assert_eq!(parser.parse_word(), Some("fgh".to_string()));
    assert_eq!(parser.parse_word(), Some("ij".to_string()));
    assert_eq!(parser.parse_word(), Some("klmnop".to_string()));
    assert_eq!(parser.parse_word(), Some("rs".to_string()));
    assert_eq!(parser.parse_word(), None);
    assert_eq!(parser.parse_word(), None);
}

#[test]
fn test_pos() {
    let mut chars = "a b  cde    fgh\nij\n  klmnop  \r\n  rs  ".chars();
    let mut parser = new_parser_without_sink(&mut chars);
    assert_eq!(parser.pos(), (0, 0));
    parser.parse_word();
    assert_eq!(parser.pos(), (0, 2));
    parser.parse_word();
    assert_eq!(parser.pos(), (0, 4));
    parser.parse_word();
    assert_eq!(parser.pos(), (0, 9));
    parser.parse_word();
    assert_eq!(parser.pos(), (1, 0));
    parser.parse_word();
    assert_eq!(parser.pos(), (2, 0));
    parser.parse_word();
    assert_eq!(parser.pos(), (2, 9));
    parser.parse_word();
    assert_eq!(parser.pos(), (3, 5));
}

#[test]
fn test_separators() {
    let mut chars = "a{b}  (cde)   [ fgh]\n\"foo\"  last'''".chars();
    let mut parser = new_parser_without_sink(&mut chars);
    assert_eq!(parser.parse_word(), Some("a".to_string()));
    assert_eq!(parser.curr_char(), Some('{'));
    parser.next();
    assert_eq!(parser.parse_word(), Some("b".to_string()));
    assert_eq!(parser.curr_char(), Some('}'));
    parser.next();
    // next char after spaces is a '(', so there's no word
    assert_eq!(parser.parse_word(), None);
    assert_eq!(parser.curr_char(), Some('('));
    parser.next();
    assert_eq!(parser.parse_word(), Some("cde".to_string()));
    assert_eq!(parser.curr_char(), Some(')'));
    parser.next();
    parser.skip_spaces();
    assert_eq!(parser.curr_char(), Some('['));
    parser.next();
    assert_eq!(parser.parse_word(), Some("fgh".to_string()));
    assert_eq!(parser.curr_char(), Some(']'));
    parser.next();
    parser.skip_spaces();
    // next char is a '"', so there's no word
    assert_eq!(parser.parse_word(), None);
    assert_eq!(parser.curr_char(), Some('"'));
    parser.next();
    assert_eq!(parser.parse_word(), Some("foo".to_string()));
    assert_eq!(parser.curr_char(), Some('"'));
    parser.next();
    assert_eq!(parser.parse_word(), Some("last'''".to_string()));
    assert_eq!(parser.parse_word(), None);
    assert_eq!(parser.parse_word(), None);
}

#[test]
fn test_type_values() {
    let mut chars = "i32 i64 f32 f64 err".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_type(), I32);
    assert_eq!(parser.parse_type(), I64);
    assert_eq!(parser.parse_type(), F32);
    assert_eq!(parser.parse_type(), F64);
    assert_eq!(parser.parse_type(), TypeError { reason: "type does not exist: err".to_string(), pos: (0, 19) });
    assert_eq!(parser.parse_type(), TypeError { reason: "EOF reached (type was expected)".to_string(), pos: (0, 19) });
}

#[test]
fn test_type_functions() {
    let mut chars = "[]; []() []i32; [ ] (i64 ) ; [f32]f32 ; [i32 i64 ] f64 i32;
        [i32] ([i64] f32) [i64] ([[i32]](f32) [i64] i32 ) err".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![] }));

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![] }));

    assert_eq!(parser.curr_char(), Some('['));
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![I32] }));

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![I64] }));

    assert_eq!(parser.curr_char(), Some(' '));
    parser.next();
    assert_eq!(parser.curr_char(), Some('['));

    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![F32], outs: vec![F32] }));

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();
    assert_eq!(parser.parse_type(),
               Fn(FnType { ins: vec![I32, I64], outs: vec![F64, I32] }));

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();

    assert_eq!(parser.parse_type(),
               Fn(FnType {
                   ins: vec![I32],
                   outs: vec![
                       Fn(FnType { ins: vec![I64], outs: vec![F32] })
                   ],
               }));

    assert_eq!(parser.parse_type(),
               Fn(FnType {
                   ins: vec![I64],
                   outs: vec![
                       Fn(FnType {
                           ins: vec![Fn(FnType { ins: vec![I32], outs: vec![] })],
                           outs: vec![F32],
                       }),
                       Fn(FnType {
                           ins: vec![I64],
                           outs: vec![I32],
                       })
                   ],
               }));

    assert_eq!(parser.parse_type(), TypeError { reason: "type does not exist: err".to_string(), pos: (1, 61) });
    assert_eq!(parser.parse_type(), TypeError { reason: "EOF reached (type was expected)".to_string(), pos: (1, 61) });
}

#[test]
fn test_type_function_optional_semi_colon() {
    let mut chars = "[](i32);[]()".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    // first type parsing should consume the optional semi-colon
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![I32] }));
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![] }));
}

macro_rules! assert_symbols_contains {
    ($parser:ident, $key:expr => $value:expr) => {{
        if let Some(v) = $parser.stack().get($key) {
            assert_eq!(v, &$value);
        } else {
            panic!("Stack does not contain key {}", &$key);
        }
    }};
}

#[test]
fn test_def() -> Result<(), ParserError> {
    let mut chars = "foo i32 blah f64 wrong: ending".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_def()?, ());
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_def()?, ());
    assert_symbols_contains!(parser, "blah" => F64);
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_def()?, ());

    // error on ':'
    assert_symbols_contains!(parser,
        "wrong" => TypeError {reason: "unexpected character: ':'".to_string(), pos: (0, 23)});
    assert_eq!(parser.curr_char(), Some(':'));
    parser.next();

    assert_eq!(parser.parse_def()?, ());
    assert_symbols_contains!(parser,
        "ending" => TypeError {reason: "EOF reached (type was expected)".to_string(), pos: (0, 30)});
    assert_eq!(parser.curr_char(), None);

    assert_eq!(parser.parse_def(),
               Err(ParserError { pos: (0, 30), msg: "Expected identifier after def, but got EOF".to_string() }));

    let mut chars = "[".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_def(),
               Err(ParserError { pos: (0, 1), msg: "Expected identifier after def, but got '['".to_string() }));

    Result::Ok(())
}

#[test]
fn test_let() -> Result<(), ParserError> {
    let mut chars = "foo = 1; blah=2.0   ; z=(2) wrong:".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_let()?, vec![("foo".to_string(), I32)]);
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_let()?, vec![("blah".to_string(), F32)]);
    assert_symbols_contains!(parser, "blah" => F32);
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_let()?, vec![("z".to_string(), I32)]);
    assert_symbols_contains!(parser, "z" => I32);
    assert_symbols_contains!(parser, "blah" => F32);
    assert_symbols_contains!(parser, "foo" => I32);
    // assert_eq!(parser.stack().len(), 3);
    assert_eq!(parser.parse_let(), Err(ParserError {
        pos: (0, 34),
        msg: "Expected '=' in let expression, but got ':'".to_string(),
    }));
    assert_eq!(parser.curr_char(), Some(':'));

    Result::Ok(())
}

#[test]
fn test_let_multi_value() {
    let mut stack = Stack::new();
    stack.push_item(
        "func".to_string(),
        Fn(FnType { ins: vec![], outs: vec![I64, F32, F64] }));

    let mut chars = "foo, bar = 1, 2; b,c=(2.0,4i64)  e ,f,g=(func); end".chars();
    let mut parser = new_parser_with_stack(&mut chars, stack);

    assert_eq!(parser.parse_let(), Ok(vec![("foo".to_string(), I32), ("bar".to_string(), I32)]));
    assert_symbols_contains!(parser, "foo" => I32);
    assert_symbols_contains!(parser, "bar" => I32);
    assert_eq!(parser.parse_let(), Ok(vec![("b".to_string(), F32), ("c".to_string(), I64)]));
    assert_symbols_contains!(parser, "b" => F32);
    assert_symbols_contains!(parser, "c" => I64);
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_let(), Ok(vec![("e".to_string(), I64), ("f".to_string(), F32), ("g".to_string(), F64)]));
    assert_symbols_contains!(parser, "e" => I64);
    assert_symbols_contains!(parser, "f" => F32);
    assert_symbols_contains!(parser, "g" => F64);
    assert_symbols_contains!(parser, "b" => F32);
    assert_symbols_contains!(parser, "c" => I64);
    assert_symbols_contains!(parser, "foo" => I32);
    // assert_eq!(parser.stack().len(), 3);
    assert_eq!(parser.parse_let(), Err(ParserError {
        pos: (0, 51),
        msg: "Expected '=' in let expression, but got EOF".to_string(),
    }));
    assert_eq!(parser.curr_char(), None);
}
