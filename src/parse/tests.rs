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
        $(stack.push($id.to_string(), $typ, false).unwrap();)*
        let mut parser = new_parser_with_stack(&mut chars, stack);
        parser.parse_expr()
    }};
}

macro_rules! type_of {
    ($e:expr) => {{
        let stack = Stack::new();
        expr_parser::type_of($e, &stack).map(|t| (t.typ, t.kind))
    }};
    ($e:expr, $($id:expr => $typ:expr),+) => {{
        let mut stack = Stack::new();
        $(stack.push($id.to_string(), $typ, false).unwrap();)*
        expr_parser::type_of($e, &stack).map(|t| (t.typ, t.kind))
    }};
}

macro_rules! assign {
    ($($id:literal),+ = $($e:expr),+) => {{
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string()); replacements.push(None);)*
        $(exprs.push($e);)*
        (ids, exprs, replacements)
    }};
    ($($id:literal),+ = $($e:expr),+ ; $($rep:expr),+) => {{
        let mut ids = Vec::new();
        let mut exprs = Vec::new();
        let mut replacements = Vec::new();
        $(ids.push($id.to_string());)*
        $(exprs.push($e);)*
        $(replacements.push($rep);)*
        (ids, exprs, replacements)
    }};
}

macro_rules! fn_call {
    ($id:literal $($args:expr)* => [$($ins:expr)*] $($outs:expr)* ) => {{
        #[allow(unused_mut)]
        let (mut args, mut ins, mut outs) = (Vec::new(), Vec::new(), Vec::new());
        $(args.push($args);)*
        $(ins.push($ins);)*
        $(outs.push($outs);)*
        let typ = FnType{ins , outs};
        FnCall { name: $id.to_string(), args, typ: Ok(typ) }
    }};
    ($id:literal $($args:expr)* => $err:expr ) => {{
        #[allow(unused_mut)]
        let mut args = Vec::new();
        $(args.push($args);)*
        FnCall { name: $id.to_string(), args, typ: Err($err) }
    }};
}

#[test]
fn test_type_of_empty() {
    assert_eq!(type_of!(""), Err("Unexpected EOF".into()));
}

#[test]
fn test_type_of_var() {
    assert_eq!(type_of!("foo", "foo" => F32), Ok((F32, Kind::Var)));
    assert_eq!(type_of!("bar", "foo" => F32),
               Err("variable \'bar\' does not exist".to_string()));
    assert_eq!(type_of!("bar", "foo" => F32, "bar" => I64), Ok((I64, Kind::Var)));

    // funny variable names
    assert_eq!(type_of!("--", "++" => F32, "--" => I64), Ok((I64, Kind::Var)));
    assert_eq!(type_of!("++", "++" => F32, "--" => I64), Ok((F32, Kind::Var)));
}

#[test]
fn test_type_of_int() {
    // max value of i32 is "2147483647", anything with more digits is classified as i64
    for i in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "10", "11", "20", "100",
        "1_000", "111_222_333", "1_2_3_4_0", "2147483647",
        "-1", "-0", "-1_2", "-98_76_43"].iter() {
        assert_eq!(type_of!(i), Ok((I32, Kind::Const)), "Example: {}", i);
    }
    for i in ["21474836478", "12345678900", "123_456_789_012_345_678_900"].iter() {
        assert_eq!(type_of!(i), Ok((I64, Kind::Const)), "Example: {}", i);
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
        assert_eq!(type_of!(f), Ok((F32, Kind::Const)), "Example: {}", f);
    }
    for f in ["0.1111111111", "2.000000000000", "3.141_592_653_589_793"].iter() {
        assert_eq!(type_of!(f), Ok((F64, Kind::Const)), "Example: {}", f);
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
    assert_eq!(type_of!("2147483647i32"), Ok((I32, Kind::Const)));
    assert_eq!(type_of!("-2147483646i32"), Ok((I32, Kind::Const)));
    assert_eq!(type_of!("0i32"), Ok((I32, Kind::Const)));
    assert_eq!(type_of!("-0i32"), Ok((I32, Kind::Const)));
    assert_eq!(type_of!("0i64"), Ok((I64, Kind::Const)));
    assert_eq!(type_of!("-0i64"), Ok((I64, Kind::Const)));
    assert_eq!(type_of!("512i64"), Ok((I64, Kind::Const)));
    assert_eq!(type_of!("-512i64"), Ok((I64, Kind::Const)));

    assert_eq!(type_of!("0f32"), Ok((F32, Kind::Const)));
    assert_eq!(type_of!("-0f32"), Ok((F32, Kind::Const)));
    assert_eq!(type_of!("0f64"), Ok((F64, Kind::Const)));
    assert_eq!(type_of!("-0f64"), Ok((F64, Kind::Const)));
    assert_eq!(type_of!("10f32"), Ok((F32, Kind::Const)));
    assert_eq!(type_of!("-10f32"), Ok((F32, Kind::Const)));
    assert_eq!(type_of!("10_000_f64"), Ok((F64, Kind::Const)));
    assert_eq!(type_of!("-10_000_f64"), Ok((F64, Kind::Const)));
    assert_eq!(type_of!("0.5_f64"), Ok((F64, Kind::Const)));
    assert_eq!(type_of!("-0.5_f64"), Ok((F64, Kind::Const)));
    assert_eq!(type_of!("12.__5678__f64"), Ok((F64, Kind::Const)));
    assert_eq!(type_of!("-12.__5678__f64"), Ok((F64, Kind::Const)));
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
               fn_call!("do-it" expr_const!("2" I32) => [I32] ));

    assert_eq!(parse_expr!("add 2 2;", "add" => Fn(FnType { ins: vec![I32, I32], outs: vec![I64] } )),
               fn_call!("add" expr_const!("2" I32) expr_const!("2" I32) => [I32 I32] I64));

    assert_eq!(parse_expr!("(div-rem 4 2)", "div-rem" => Fn(FnType { ins: vec![I32, I32], outs: vec![I32, I32] } )),
               fn_call!("div-rem" expr_const!("4" I32) expr_const!("2" I32) => [I32 I32] I32 I32));

    assert_eq!(parse_expr!("(print 0.0)"),
               fn_call!("print" expr_const!("0.0" F32) =>
               TypeError { reason: "Unknown function: 'print'".to_string(), pos: (0, 11) }));

    // function without args: only invoke the function if it's alone within parens
    assert_eq!(parse_expr!("no-args;", "no-args" => Fn(FnType { ins: vec![], outs: vec![I32] } )),
               Var(String::from("no-args"), Fn(FnType { ins: vec![], outs: vec![I32] })));

    assert_eq!(parse_expr!("(no-args);", "no-args" => Fn(FnType { ins: vec![], outs: vec![I32] } )),
               fn_call!("no-args" => [] I32));
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
    let mut chars = "i32; i64 ; f32 ; f64; err".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_type(), I32);
    assert_eq!(parser.parse_type(), I64);
    assert_eq!(parser.parse_type(), F32);
    assert_eq!(parser.parse_type(), F64);
    assert_eq!(parser.parse_type(), Type::Error(TypeError { reason: "type does not exist: err".to_string(), pos: (0, 25) }));
    assert_eq!(parser.parse_type(), Type::Error(TypeError { reason: "EOF reached (type was expected)".to_string(), pos: (0, 25) }));
}

#[test]
fn test_type_functions() {
    let mut chars = "[]; []() []i32; [ ] (i64 ) ; [f32]f32 ; [i32 i64 ] f64 i32;
        [i32] ([i64] f32) [i64] ([[i32]](f32) [i64] i32 ) err".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![] }));
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![] }));
    assert_eq!(parser.curr_char(), Some('['));
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![I32] }));
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![], outs: vec![I64] }));
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![F32], outs: vec![F32] }));
    assert_eq!(parser.parse_type(), Fn(FnType { ins: vec![I32, I64], outs: vec![F64, I32] }));

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

    assert_eq!(parser.parse_type(), Type::Error(TypeError { reason: "type does not exist: err".to_string(), pos: (1, 61) }));
    assert_eq!(parser.parse_type(), Type::Error(TypeError { reason: "EOF reached (type was expected)".to_string(), pos: (1, 61) }));
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
fn test_def() {
    let mut chars = "foo i32; blah f64 ; wrong: ending".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_def(), Ok(()));
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_def(), Ok(()));
    assert_symbols_contains!(parser, "blah" => F64);
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_def(), Ok(()));

    // error on ':'
    assert_symbols_contains!(parser,
        "wrong" => Type::Error( TypeError {reason: "unexpected character: ':'".to_string(), pos: (0, 26)}));
    assert_eq!(parser.curr_char(), Some(':'));
    parser.next();

    assert_eq!(parser.parse_def(), Ok(()));
    assert_symbols_contains!(parser,
        "ending" => Type::Error(TypeError {reason: "EOF reached (type was expected)".to_string(), pos: (0, 33)}));
    assert_eq!(parser.curr_char(), None);

    assert_eq!(parser.parse_def(),
               Err(ParserError { pos: (0, 33), msg: "Expected identifier after def, but got EOF".to_string() }));

    let mut chars = "[".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_def(),
               Err(ParserError { pos: (0, 1), msg: "Expected identifier after def, but got '['".to_string() }));
}

#[test]
fn test_let() {
    let mut chars = "foo = 1; blah=2.0   ; z=(2) wrong:".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_assignment(), Ok(assign!("foo" = expr_const!("1" I32))));
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_assignment(), Ok(assign!("blah" = expr_const!("2.0" F32))));
    assert_symbols_contains!(parser, "blah" => F32);
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_assignment(), Ok(assign!("z" = expr_const!("2" I32))));
    assert_symbols_contains!(parser, "z" => I32);
    assert_symbols_contains!(parser, "blah" => F32);
    assert_symbols_contains!(parser, "foo" => I32);
    // assert_eq!(parser.stack().len(), 3);
    assert_eq!(parser.parse_assignment(), Err(ParserError {
        pos: (0, 34),
        msg: "Expected '=' in let expression, but got ':'".to_string(),
    }));
    assert_eq!(parser.curr_char(), Some(':'));
}

#[test]
fn test_def_then_assign() {
    let mut chars = "foo i64; foo = 1; bar f32; bar = 1; zed i32; zed = 0.1;".chars();
    let mut parser = new_parser_without_sink(&mut chars);

    assert_eq!(parser.parse_def(), Ok(()));
    assert_symbols_contains!(parser, "foo" => I64);

    assert_eq!(parser.parse_assignment(), Ok(assign!("foo" = expr_const!("1" I32); Some(I64))));
}

#[test]
fn test_let_multi_value() {
    let mut stack = Stack::new();
    stack.push(
        "func".to_string(),
        Fn(FnType { ins: vec![], outs: vec![I64, F32, F64] }), false).unwrap();

    let stack2 = stack.clone();

    let mut chars = "foo, bar = 1, 2; b,c=(2.0,4i64)  e ,f,g=(func); end".chars();
    let mut parser = new_parser_with_stack(&mut chars, stack);

    assert_eq!(parser.parse_assignment(), Ok(assign!("foo", "bar" = expr_const!("1" I32), expr_const!("2" I32))));
    assert_symbols_contains!(parser, "foo" => I32);
    assert_symbols_contains!(parser, "bar" => I32);
    assert_eq!(parser.parse_assignment(), Ok(assign!("b", "c" = expr_const!("2.0" F32), expr_const!("4i64" I64))));
    assert_symbols_contains!(parser, "b" => F32);
    assert_symbols_contains!(parser, "c" => I64);
    assert_symbols_contains!(parser, "foo" => I32);
    assert_eq!(parser.parse_assignment(), Ok(assign!("e", "f", "g" = fn_call!("func" => [] I64 F32 F64))));
    assert_symbols_contains!(parser, "e" => I64);
    assert_symbols_contains!(parser, "f" => F32);
    assert_symbols_contains!(parser, "g" => F64);
    assert_symbols_contains!(parser, "b" => F32);
    assert_symbols_contains!(parser, "c" => I64);
    assert_symbols_contains!(parser, "foo" => I32);
    // assert_eq!(parser.stack().len(), 3);
    assert_eq!(parser.parse_assignment(), Err(ParserError {
        pos: (0, 51),
        msg: "Expected '=' in let expression, but got EOF".to_string(),
    }));
    assert_eq!(parser.curr_char(), None);

    let mut chars = " ee, ff = (func) ".chars();
    let mut parser = new_parser_with_stack(&mut chars, stack2);

    assert_eq!(parser.parse_assignment(), Err(ParserError {
        msg: "multi-value assignment mismatch: 2 identifiers but 3 expressions of types \
        'i64 f32 f64' found".to_string(),
        pos: (0, 17),
    }));
}
