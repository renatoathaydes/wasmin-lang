use crate::parse::new_parser;

pub mod ast;
pub mod types;
pub mod parse;

fn main() {
    let mut program = "(print 256)".chars();
    let mut parser = new_parser(&mut program);
    let program = parser.parse_expr();

    println!("Wasmin compiler version 0.0");
    println!("{:?}", program);
}

