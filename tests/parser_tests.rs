use std::sync::mpsc::{channel, Sender};

use wasmin::ast::{Expression, TopLevelExpression};
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
        (vec!["x".to_string()], vec![Expression::Const("1".to_string(), I32)]), Public));
}
