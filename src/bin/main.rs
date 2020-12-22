use std::sync::mpsc;
use std::thread;

use wasmin::parse::new_parser;

// test program
const PROGRAM: &str = "let constant-ten = 10;

def add-10 [i32] i32;
fun add-10 n = add n constant-ten;

def add-10 [i64] i64;
fun add-10 n = add n constant-ten;

pub let one, two = 1, 2;

def three i64;
let three, four = (
    let t = 3;
    let f = 4;
    t, add-10 f
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
        println!("TOP: {:?}", expr);
    }

    parser_handle.join().expect("Parser failed")
}
