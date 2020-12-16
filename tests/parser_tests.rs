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
