use crate::ast::Expression;
use crate::types::{*, Type::*};
use crate::parse::parser::{*};
use crate::parse::{expr_parser, new_parser};

macro_rules! parse_expr {
    ($e:expr) => {{
        let mut chars = $e.chars();
        let mut parser = new_parser(&mut chars);
        parser.parse_expr()
    }};
}

macro_rules! type_of {
    ($e:expr) => {{
        let stack = Stack::new();
        expr_parser::type_of($e, &stack)
    }};
}

#[test]
fn test_type_of_empty() {
    assert_eq!(type_of!(""), Ok(Type::Empty));
}

#[test]
fn test_type_of_int() {
    // max value of i32 is "2147483647", anything with more digits is classified as i64
    for i in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "10", "11", "20", "100",
        "1_000", "111_222_333", "1_2_3_4_0", "2147483647",
        "-1", "-0", "-1_2", "-98_76_43"].iter() {
        assert_eq!(type_of!(i), Ok(Type::I32), "Example: {}", i);
    }
    for i in ["21474836478", "12345678900", "123_456_789_012_345_678_900"].iter() {
        assert_eq!(type_of!(i), Ok(Type::I64), "Example: {}", i);
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
        assert_eq!(type_of!(f), Ok(Type::F32), "Example: {}", f);
    }
    for f in ["0.1111111111", "2.000000000000", "3.141_592_653_589_793"].iter() {
        assert_eq!(type_of!(f), Ok(Type::F64), "Example: {}", f);
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
    assert_eq!(type_of!("2147483647i32"), Ok(Type::I32));
    assert_eq!(type_of!("-2147483646i32"), Ok(Type::I32));
    assert_eq!(type_of!("0i32"), Ok(Type::I32));
    assert_eq!(type_of!("-0i32"), Ok(Type::I32));
    assert_eq!(type_of!("0i64"), Ok(Type::I64));
    assert_eq!(type_of!("-0i64"), Ok(Type::I64));
    assert_eq!(type_of!("512i64"), Ok(Type::I64));
    assert_eq!(type_of!("-512i64"), Ok(Type::I64));

    assert_eq!(type_of!("0f32"), Ok(Type::F32));
    assert_eq!(type_of!("-0f32"), Ok(Type::F32));
    assert_eq!(type_of!("0f64"), Ok(Type::F64));
    assert_eq!(type_of!("-0f64"), Ok(Type::F64));
    assert_eq!(type_of!("10f32"), Ok(Type::F32));
    assert_eq!(type_of!("-10f32"), Ok(Type::F32));
    assert_eq!(type_of!("10_000_f64"), Ok(Type::F64));
    assert_eq!(type_of!("-10_000_f64"), Ok(Type::F64));
    assert_eq!(type_of!("0.5_f64"), Ok(Type::F64));
    assert_eq!(type_of!("-0.5_f64"), Ok(Type::F64));
    assert_eq!(type_of!("12.__5678__f64"), Ok(Type::F64));
    assert_eq!(type_of!("-12.__5678__f64"), Ok(Type::F64));
}

#[test]
fn test_empty() {
    assert_eq!(parse_expr!("()"), Expression::Empty);
}

#[test]
fn test_i32() {
    assert_eq!(parse_expr!("(0)"), Expression::Const(String::from("0"), I32));
    assert_eq!(parse_expr!("( 1 )"), Expression::Const(String::from("1"), I32));
    assert_eq!(parse_expr!("( 100)"), Expression::Const(String::from("100"), I32));
    assert_eq!(parse_expr!("( 100_000 )"), Expression::Const(String::from("100_000"), I32));
}

#[test]
fn test_f32() {
    assert_eq!(parse_expr!("(0.0)"), Expression::Const(String::from("0.0"), F32));
    assert_eq!(parse_expr!("( 1.0 )"), Expression::Const(String::from("1.0"), F32));
    assert_eq!(parse_expr!("( 1.00)"), Expression::Const(String::from("1.00"), F32));
    assert_eq!(parse_expr!("( 1_0.0_0)"), Expression::Const(String::from("1_0.0_0"), F32));
}

#[test]
fn test_fn_basic() {
    assert_eq!(parse_expr!("(print 0.0)"),
               Expression::fn_call("print",
                                   vec![Expression::Const(String::from("0.0"), F32)],
                                   Error { reason: "Unknown function: 'print'".to_string(), pos: (0, 11) }));
}

#[test]
fn test_word() {
    let mut chars = "abc".chars();
    let mut parser = new_parser(&mut chars);
    assert_eq!(parser.parse_word(), Some("abc".to_string()));
}

#[test]
fn test_words() {
    let mut chars = "a b  cde    fgh\nij\n  klmnop  \r\n  rs  ".chars();
    let mut parser = new_parser(&mut chars);
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
    let mut parser = new_parser(&mut chars);
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
    let mut parser = new_parser(&mut chars);
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
    let mut parser = new_parser(&mut chars);

    assert_eq!(parser.parse_type(), Type::I32);
    assert_eq!(parser.parse_type(), Type::I64);
    assert_eq!(parser.parse_type(), Type::F32);
    assert_eq!(parser.parse_type(), Type::F64);
    assert_eq!(parser.parse_type(), Type::Error { reason: "type does not exist: err".to_string(), pos: (0, 19) });
    assert_eq!(parser.parse_type(), Type::Error { reason: "EOF reached (type was expected)".to_string(), pos: (0, 19) });
}

#[test]
fn test_type_functions() {
    let mut chars = "[]; []() []i32; [ ] (i64 ) ; [f32]f32 ; [i32 i64 ] f64 i32;
        [i32] ([i64] f32) [i64] ([[i32]](f32) [i64] i32 ) err".chars();
    let mut parser = new_parser(&mut chars);

    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![] });

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();
    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![] });

    assert_eq!(parser.curr_char(), Some('['));
    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![Type::I32] });

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();
    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![Type::I64] });

    assert_eq!(parser.curr_char(), Some(' '));
    parser.next();
    assert_eq!(parser.curr_char(), Some('['));

    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![Type::F32], outs: vec![Type::F32] });

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();
    assert_eq!(parser.parse_type(),
               Type::Fn { ins: vec![Type::I32, Type::I64], outs: vec![Type::F64, Type::I32] });

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();

    assert_eq!(parser.parse_type(),
               Type::Fn {
                   ins: vec![Type::I32],
                   outs: vec![
                       Type::Fn { ins: vec![Type::I64], outs: vec![Type::F32] }
                   ],
               });

    assert_eq!(parser.parse_type(),
               Type::Fn {
                   ins: vec![Type::I64],
                   outs: vec![
                       Type::Fn {
                           ins: vec![Type::Fn { ins: vec![Type::I32], outs: vec![] }],
                           outs: vec![Type::F32],
                       },
                       Type::Fn {
                           ins: vec![Type::I64],
                           outs: vec![Type::I32],
                       }
                   ],
               });

    assert_eq!(parser.parse_type(), Type::Error { reason: "type does not exist: err".to_string(), pos: (1, 61) });
    assert_eq!(parser.parse_type(), Type::Error { reason: "EOF reached (type was expected)".to_string(), pos: (1, 61) });
}

#[test]
fn test_type_function_optional_semi_colon() {
    let mut chars = "[](i32);[]()".chars();
    let mut parser = new_parser(&mut chars);

    // first type parsing should consume the optional semi-colon
    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![Type::I32] });
    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![] });
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
    let mut parser = new_parser(&mut chars);

    assert_eq!(parser.parse_def()?, ());
    assert_symbols_contains!(parser, "foo" => Type::I32);
    assert_eq!(parser.parse_def()?, ());
    assert_symbols_contains!(parser, "blah" => Type::F64);
    assert_symbols_contains!(parser, "foo" => Type::I32);
    assert_eq!(parser.parse_def()?, ());

    // error on ':'
    assert_symbols_contains!(parser,
        "wrong" => Type::Error {reason: "unexpected character: ':'".to_string(), pos: (0, 23)});
    assert_eq!(parser.curr_char(), Some(':'));
    parser.next();

    assert_eq!(parser.parse_def()?, ());
    assert_symbols_contains!(parser,
        "ending" => Type::Error {reason: "EOF reached (type was expected)".to_string(), pos: (0, 30)});
    assert_eq!(parser.curr_char(), None);

    assert_eq!(parser.parse_def(),
               Err(ParserError { pos: (0, 30), msg: "Expected identifier after def, but got EOF".to_string() }));

    let mut chars = "[".chars();
    let mut parser = new_parser(&mut chars);

    assert_eq!(parser.parse_def(),
               Err(ParserError { pos: (0, 1), msg: "Expected identifier after def, but got '['".to_string() }));

    Result::Ok(())
}
