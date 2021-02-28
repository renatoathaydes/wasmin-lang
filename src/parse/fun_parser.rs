use crate::ast::Function;
use crate::parse::Parser;
use crate::parse::parser::ParserError;
use crate::types::{FunType, Type, Type::Fn, types_to_string, NO_ARGS_OR_RETURNS_FUN_TYPE};

pub fn parse_fun(parser: &mut Parser) -> Result<Function, ParserError> {
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
            let body = parser.parse_expr();
            parser.stack_mut().drop_level();
            match bind_result {
                Ok(typ) => {
                    parser.stack_mut().push(name.clone(), Type::Fn(vec![typ.clone()]), false, false)
                        .map_err(|msg| ParserError { pos: parser.pos(), msg })?;
                    if typ.ins.len() == left.len() {
                        let actual_type = body.get_type();
                        if typ.outs.len() == actual_type.len() {
                            Ok((name, left, body, typ.clone()))
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
        Some(typ) => {
            parser.stack_mut().new_level();
            let _ = parser.parse_expr();
            parser.stack_mut().drop_level();
            Err(ParserError { msg: format!("fun '{}' cannot implement type '{}'", name, typ), pos })
        }
        None if left.is_empty() => {
            parser.stack_mut().new_level();
            let body = parser.parse_expr();
            parser.stack_mut().drop_level();
            let typ = body.get_type();
            if typ.is_empty() {
                Ok((name, left, body, NO_ARGS_OR_RETURNS_FUN_TYPE))
            } else {
                Err(ParserError {
                    msg: format!("fun '{}' missing def (body returns a value of type '{}', \
                    hence the return type is mandatory)", name, types_to_string(&typ)),
                    pos,
                })
            }
        }
        None => {
            parser.stack_mut().new_level();
            let _ = parser.parse_expr();
            parser.stack_mut().drop_level();
            Err(ParserError { msg: format!("fun '{}' missing def (arg types cannot be inferred)", name), pos })
        }
    }
}

fn bind_args<'s>(parser: &mut Parser, types: &'s [FunType], names: &[String]) -> Result<&'s FunType, String> {
    let typ = types.get(types.len() - 1).expect("types must not be empty");
    // do not check lengths match here... bind all args we can, an error will happen later if necessary
    for (t, name) in typ.ins.iter().zip(names.iter()) {
        parser.stack_mut().push(name.clone(), t.clone(), false, false)?;
    }
    Ok(typ)
}
