use std::str::Chars;

use crate::ast::{Expression, Type};

mod ast;

fn main() {
    let program = "(factorial)";

    println!("Wasmin compiler version 0.0");

    println!("{:?}", parse(&mut program.chars()));
}

fn parse(chars: &mut Chars) -> Expression {
    loop {
        if let Some(c) = chars.next() {
            let x = match c {
                '(' => parse_parens(chars),
                _ => Expression::Empty,
            };
            return x;
        } else {
            break;
        }
    }

    Expression::Empty
}

fn parse_parens(chars: &mut Chars) -> Expression {
    let mut value = String::new();
    loop {
        if let Some(c) = chars.next() {
            match c {
                ')' => return Expression::Const(value, Type::I32),
                _ => value.push(c),
            };
        } else {
            panic!("Non-closed expression");
        }
    }
}