use std::str::Chars;

use crate::ast::Expression;
use crate::parse::parser::{Parser, Stack};
use crate::types::Type;

pub fn parse_expr(parser: &mut Parser) -> Expression {
    println!("Parsing expression");
    parser.skip_spaces();
    if let Some(c) = parser.curr_char() {
        match c {
            '(' => {
                parser.next();
                parse_parens(parser)
            }
            _ => Expression::Empty,
        }
    } else {
        Expression::Empty
    }
}

fn parse_parens(parser: &mut Parser) -> Expression {
    println!("Parsing parens");
    let mut words = Vec::<String>::with_capacity(2);
    loop {
        println!("Parser: {:?}, words: {:?}", parser, &words);
        parser.skip_spaces();
        if let Some(')') = parser.curr_char() { break; }
        if let None = parser.curr_char() {
            return Expression::Err(parser.error("Unclosed parens"));
        }
        if let Some(word) = parser.parse_word() {
            println!("Word: {}", &word);
            words.push(word);
        } else { break; }
    }
    to_expr(parser, &mut words)
}

fn to_expr(parser: &mut Parser, words: &mut Vec<String>) -> Expression {
    println!("To expr: {:?}", words);
    if words.is_empty() {
        Expression::Empty
    } else if words.len() == 1 {
        let w = words.remove(0);
        let typ = type_of(&w, parser.stack());
        Expression::Const(w, typ)
    } else {
        let name = words.remove(0);
        let args = words.drain(0..).map(|arg| {
            println!("Arg: {}", &arg);
            let typ = type_of(&arg, parser.stack());
            Expression::Const(arg, typ)
        }).collect();
        println!("Ags are : {:?}", &args);
        let typ = parser.stack().get(&name).map(|t| t.clone())
            .unwrap_or_else(|| parser.error(&format!("Unknown function: '{}'", &name)));

        println!("Done here");
        Expression::FnCall { name, args, typ }
    }
}


pub fn type_of(str: &str, stack: &Stack) -> Type {
    let mut chars = str.chars();
    let c = chars.next();
    return match c {
        Some('0'..='9') => { type_of_num(&mut chars) }
        None => { Type::Empty }
        _ => {
            stack.get(str).map(|t| t.to_owned()).unwrap_or_else(||
                Type::Error { pos: (0, 0), reason: "not a number".to_string() })
        }
    };
}

fn type_of_num(chars: &mut Chars) -> Type {
    let mut dots = 0;
    let mut digits = 0;
    let mut error = false;

    loop {
        if let Some(c) = chars.next() {
            match c {
                '0'..='9' => { digits += 1 }
                '_' => {}
                '.' => { dots += 1; }
                _ => { error = true; }
            }
        } else {
            break;
        }
    }
    if error || dots > 1 {
        Type::Error { pos: (0, 0), reason: "number contains invalid digits".to_string() }
    } else if dots == 1 {
        Type::F32
    } else {
        Type::I32
    }
}
