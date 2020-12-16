use std::sync::mpsc::channel;

use wasmin::{*};
use wasmin::ast::{*};
use wasmin::ast::Visibility::Public;
use wasmin::parse::new_parser;
use wasmin::types::Type::I32;

#[test]
fn test_let() {
    let mut chars = "let x = 1; let y = (let z = 2; z)".chars();
    let (sender, rcv) = channel();
    let mut parser = new_parser(&mut chars, sender);
    parser.parse();

    assert_eq!(rcv.iter().next().unwrap(), texpr_let!("x" = expr_const!("1" I32)));
    assert_eq!(rcv.iter().next().unwrap(), texpr_let!("y" =
        expr_group!(expr_let!("z" = expr_const!("2" I32)) expr_var!("z" I32))));
}
