use crate::ast::Expression;
use crate::parse::parser::Parser;
use crate::types::type_of;

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
        let typ = type_of(&w);
        Expression::Const(w, typ)
    } else {
        let name = words.remove(0);
        let mut args = words.drain(0..).map(|arg| {
            println!("Arg: {}", &arg);
            let typ = type_of(&arg);
            Expression::Const(arg, typ)
        }).collect();
        println!("Ags are : {:?}", &args);
        let typ = parser.stack().get(&name).map(|t| t.clone())
            .unwrap_or_else(|| parser.error(&format!("Unknown function: '{}'", &name)));

        println!("Done here");
        Expression::FnCall { name, args, typ }
    }
}
