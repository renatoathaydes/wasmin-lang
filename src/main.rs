use parse::parse_expr;

pub mod ast;
pub mod types;
pub mod parse;

fn main() {
    let program = "(256)";

    println!("Wasmin compiler version 0.0");

    println!("{:?}", parse_expr(&mut program.chars()));
}

