use parse::parse;

mod ast;
mod types;
mod parse;

fn main() {
    let program = "(256)";

    println!("Wasmin compiler version 0.0");

    println!("{:?}", parse(&mut program.chars()));
}

