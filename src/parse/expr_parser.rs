use crate::ast::Expression;
use crate::ast::FnInvocation;
use crate::parse::parser::Parser;
use crate::types::type_of;

pub fn parse_expr(parser: &mut Parser) -> Expression {
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
    let mut words = Vec::<String>::with_capacity(2);
    loop {
        if let Some(word) = parser.parse_word() {
            words.push(word);
        } else { break; }
    }
    if words.is_empty() {
        Expression::Empty
    } else if words.len() == 1 {
        let w = words.remove(0);
        let typ = type_of(&w);
        Expression::Const(w, typ)
    } else {
        let name = words.remove(0);
        let mut args = Vec::<Expression>::with_capacity(words.len());
        for arg in words {
            let typ = type_of(&arg);
            args.push(Expression::Const(arg, typ));
        }
        let typ = parser.stack().get(&name).map(|t| t.clone())
            .unwrap_or_else(|| parser.error(&format!("Unknown function: '{}'", &name)));

        Expression::FnCall(Box::new(FnInvocation { name, args, typ }))
    }
}
