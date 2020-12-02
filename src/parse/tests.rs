use super::*;

#[test]
fn test_empty() {
    let mut chars = "()".chars();
    assert_eq!(parse_expr(&mut chars), Expression::Empty);
}

#[test]
fn test_i32() {
    let mut chars = "(0)".chars();
    assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("0"), I32));

    let mut chars = "( 1 )".chars();
    assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("1"), I32));

    let mut chars = "( 100)".chars();
    assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("100"), I32));
}

#[test]
fn test_f32() {
    let mut chars = "(0.0)".chars();
    assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("0.0"), F32));

    let mut chars = "( 1.0 )".chars();
    assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("1.0"), F32));

    let mut chars = "( 1.00)".chars();
    assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("1.00"), F32));
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
    assert_eq!(parser.parse_type(), Type::error("err", "type does not exist"));
    assert_eq!(parser.parse_type(), Type::error("", "EOF reached"));
}

#[test]
fn test_type_functions() {
    let mut chars = "[]; [](); []i32; [ ] (i64 ) ; [f32]f32 ; [i32 i64 ] f64 i32;
        [i32] ([i64] f32) [i64] ([[i32]](f32) [i64] i32 ) err".chars();
    let mut parser = new_parser(&mut chars);

    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![] });

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();
    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![] });

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();
    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![Type::I32] });

    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();
    assert_eq!(parser.parse_type(), Type::Fn { ins: vec![], outs: vec![Type::I64] });

    assert_eq!(parser.curr_char(), Some(' '));
    parser.next();
    assert_eq!(parser.curr_char(), Some(';'));
    parser.next();

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

    assert_eq!(parser.parse_type(), Type::error("err", "type does not exist"));
    assert_eq!(parser.parse_type(), Type::error("", "EOF reached"));
}

macro_rules! assert_symbols_contains {
    ($parser:ident, $key:expr => $value:expr) => {{
        if let Some(v) = $parser.stack().get($key.to_string()) {
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
    assert_eq!(parser.parse_def(), Result::Err(ParserError {
        pos: (0, 23),
        msg: "Bad type in 'wrong' def: ':' --> unexpected character".to_string(),
    }));
    parser.next();
    assert_eq!(parser.parse_def(), Result::Err(ParserError {
        pos: (0, 30),
        msg: "Bad type in 'ending' def: '' --> EOF reached".to_string(),
    }));
    assert_symbols_contains!(parser, "blah" => Type::F64);
    assert_symbols_contains!(parser, "foo" => Type::I32);
    Result::Ok(())
}
