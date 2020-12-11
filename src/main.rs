use std::sync::mpsc;
use std::thread;

use crate::parse::new_parser;

pub mod ast;
pub mod types;
pub mod parse;

// test program
const PROGRAM: &str = "let a, b = 2, 4;
    let x = mul a b";

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
