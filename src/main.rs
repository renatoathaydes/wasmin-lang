use std::sync::mpsc;
use std::thread;

use crate::parse::new_parser;

pub mod ast;
pub mod types;
pub mod parse;

// test program
const PROGRAM: &str = "let constant-ten = 10;

let one, two = 1, 2;

let three, four = (
    let t = 3;
    let f = 4;
    t, f
)";

fn main() {
    println!("Wasmin compiler version 0.0");

    let (sender, rcvr) = mpsc::channel();

    let parser_handle = thread::spawn(move || {
        let mut chars = PROGRAM.chars();
        let mut parser = new_parser(&mut chars, sender);
        parser.parse()
    });

    for expr in rcvr {
        println!("xxx- {:?}", expr);
    }

    parser_handle.join().expect("Parser failed")
}
