use crate::ast::Fun;
use crate::parse::Parser;
use crate::parse::parser::ParserError;
use crate::types::{FnType, Type, Type::Fn, types_to_string};

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
        return Err(err);
    }
    let name = left.remove(0);
    match parser.stack().get_def(&name).cloned() {
        Some(Fn(types)) => {
            parser.stack_mut().new_level();
            let bind_result = bind_args(parser, &types, &left);
            let expr = parser.parse_expr();
            parser.stack_mut().drop_level();
            match bind_result {
                Ok(typ) => {
                    parser.stack_mut().push(name.clone(), Type::Fn(vec![typ.clone()]), false).unwrap();
                    if typ.ins.len() == left.len() {
                        let actual_type = expr.get_type();
                        if typ.outs.len() == actual_type.len() {
                            Ok((name, left, expr, typ.clone()))
                        } else {
                            Err(ParserError {
                                msg: format!("fun '{}' should have type(s) {} \
                                but its body has type {}", name, typ, types_to_string(&actual_type)),
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
                Err(e) => Err(ParserError { msg: e, pos }),
            }
        }
        Some(typ) =>
            Err(ParserError { msg: format!("fun '{}' cannot implement type '{}'", name, typ), pos }),
        None =>
            Err(ParserError { msg: format!("fun '{}' missing def (arg types cannot be inferred)", name), pos })
    }
}

fn bind_args<'s>(parser: &mut Parser, types: &'s Vec<FnType>, names: &Vec<String>) -> Result<&'s FnType, String> {
    println!("Binding fun args {:?} to {:?}", names, types);
    let typ = types.get(types.len() - 1).expect("types must not be empty");
    // do not check lengths match here... bind all args we can, an error will happen later if necessary
    for (t, name) in typ.ins.iter().zip(names.iter()) {
        parser.stack_mut().push(name.clone(), t.clone(), false)?;
    }
    Ok(typ)
}
