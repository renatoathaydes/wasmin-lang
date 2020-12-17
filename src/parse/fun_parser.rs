use crate::ast::Fun;
use crate::parse::Parser;
use crate::parse::parser::ParserError;
use crate::types::{Type::Fn, Types};

pub fn parse_fun(parser: &mut Parser) -> Result<Fun, ParserError> {
    let mut left = Vec::new();
    let pos = parser.pos();
    while let Some(item) = parser.parse_word() {
        left.push(item);
    }
    parser.skip_spaces();
    match parser.curr_char() {
        Some('=') => parser.next(),
        Some(c) => return Err(parser.error_unexpected_char(
            c, "expected function assignment '=' character").into()),
        None => return Err(ParserError { msg: "unexpected EOF".to_owned(), pos })
    };
    if left.is_empty() {
        let err = if let Some(c) = parser.curr_char() {
            parser.error_unexpected_char(c, "expected function name").into()
        } else {
            ParserError { msg: "unexpected EOF".to_owned(), pos }
        };
        Err(err)
    } else {
        let name = left.remove(0);
        let t = parser.stack().get(&name).map(|t| t.clone());
        match t {
            Some(Fn(typ)) => {
                let expr = parser.parse_expr();
                if typ.ins.len() == left.len() {
                    let actual_type = expr.get_type();
                    if typ.outs.len() == actual_type.len() {
                        Ok((name, left, expr, typ.clone()))
                    } else {
                        Err(ParserError {
                            msg: format!("fun '{}' should have return type(s) {} \
                                but its body has type {}", name, typ, Types::T(&actual_type)),
                            pos,
                        })
                    }
                } else {
                    let len = typ.ins.len();
                    let s = if len == 1 { "" } else { "s" };
                    Err(ParserError {
                        msg: format!("fun '{}' should have {} arguments{} \
                                but it has {}", name, len, s, left.len()),
                        pos,
                    })
                }
            }
            Some(typ) =>
                Err(ParserError { msg: format!("fun '{}' cannot implement type {}", name, typ), pos }),
            None =>
                Err(ParserError { msg: "cannot implement fun without def".to_owned(), pos })
        }
    }
}