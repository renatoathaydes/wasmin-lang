use std::sync::mpsc::{channel};

use wasmin::ast::{*};
use wasmin::{expr_const};
use wasmin::ast::Visibility::Public;
use wasmin::parse::new_parser;
use wasmin::types::Type::I32;

#[test]
fn test_let() {
    let mut chars = "let x = 1".chars();
    let (sender, rcv) = channel();
    let mut parser = new_parser(&mut chars, sender);
    parser.parse();

    assert_eq!(rcv.iter().next().unwrap(), TopLevelExpression::Let(
        (vec!["x".to_string()], vec![expr_const!("1" I32)]), Public));
}
