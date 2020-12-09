use std::str::Chars;

use crate::ast::Expression;
use crate::parse::parser::{GroupingState, GroupingSymbol, Parser, Stack};
use crate::parse::parser::GroupingSymbol::Parens;
use crate::types::{FnType, Type};

pub fn parse_expr(parser: &mut Parser) -> Expression {
    let mut state = GroupingState::new();
    let expr = parse_expr_with_state(parser, &mut state);
    if state.is_empty() {
        expr
    } else {
        Expression::ExprError(parser.error("Unclosed grouping in expression"))
    }
}

fn parse_expr_with_state(parser: &mut Parser, state: &mut GroupingState) -> Expression {
    println!("Parsing expression");
    let mut words = Vec::<String>::with_capacity(2);
    let mut multi = Vec::<Expression>::with_capacity(2);
    let mut exprs = Vec::<Expression>::with_capacity(2);
    loop {
        parser.skip_spaces();
        match parser.curr_char() {
            Some('(') => {
                parser.next();
                state.enter(GroupingSymbol::Parens);
                let expr = parse_expr_with_state(parser, state);
                if !expr.get_type().is_empty() {
                    parser.skip_spaces();
                    let is_multi = parser.curr_char() == Some(',') || !multi.is_empty();
                    if is_multi {
                        multi.push(expr);
                    } else {
                        exprs.push(expr);
                    }
                    // allow redundant trailing ';' after (..)
                    if let Some(';') = parser.curr_char() {
                        parser.next();
                    }
                }
                if state.is_empty() { break; }
            }
            Some(')') => {
                parser.next();
                if state.is_inside(Parens) {
                    state.exit_symbol();
                    if multi.is_empty() && words.len() == 1 {
                        if let Some(Type::Fn(t)) = parser.stack().get(words.last().unwrap()) {
                            println!("Special case, single function call");
                            let typ = t.outs.clone();
                            exprs.push(Expression::FnCall { name: words.remove(0), args: vec![], typ });
                        }
                    }
                    break;
                } else {
                    return Expression::ExprError(parser.error_unexpected_char(
                        ')', "Closing parens does not match any opening parens"));
                }
            }
            Some(';') => {
                parser.next();
                if multi.is_empty() {
                    consume_expr(parser, &mut words, &mut exprs);
                } else {
                    consume_expr_with_multi(parser, &mut words, &mut exprs,
                                            std::mem::replace(&mut multi, Vec::with_capacity(2)));
                }
                if state.is_empty() { break; }
            }
            Some(',') => {
                parser.next();
                consume_expr(parser, &mut words, &mut multi);
            }
            None => { break; }
            _ => {
                if let Some(word) = parser.parse_word() {
                    println!("Word: {}", &word);
                    words.push(word);
                } else { break; }
            }
        }
    }

    if !multi.is_empty() {
        consume_expr_with_multi(parser, &mut words, &mut exprs,
                                std::mem::replace(&mut multi, Vec::with_capacity(2)));
    }

    if exprs.is_empty() {
        println!("exprs is empty");
        to_expr(parser, &mut words)
    } else if exprs.len() == 1 && words.is_empty() {
        println!("exprs has 1 expr, words is empty");
        exprs.remove(0)
    } else {
        if !words.is_empty() {
            println!("words is not empty");
            exprs.push(to_expr(parser, &mut words));
        }
        println!("Group of expressions: {:?}", &exprs);
        Expression::Group(exprs)
    }
}

fn consume_expr_with_multi(parser: &mut Parser,
                           words: &mut Vec<String>,
                           exprs: &mut Vec<Expression>,
                           mut multi: Vec<Expression>) {
    println!("Consuming multi: {:?}", &multi);
    if words.is_empty() && multi.len() == 1 {
        exprs.push(multi.remove(0));
        return;
    }
    consume_expr(parser, words, &mut multi);
    exprs.push(Expression::Multi(multi));
}

fn consume_expr(parser: &mut Parser,
                words: &mut Vec<String>,
                exprs: &mut Vec<Expression>) {
    if !words.is_empty() {
        let expr = to_expr(parser, words);
        exprs.push(expr);
        words.clear();
    }
}

fn to_expr(parser: &mut Parser, words: &mut Vec<String>) -> Expression {
    println!("To expr: {:?}", words);
    if words.is_empty() {
        Expression::Empty
    } else if words.len() == 1 {
        let w = words.remove(0);
        let typ = type_of(&w, parser.stack()).unwrap_or_else(|reason|
            Type::TypeError { reason, pos: parser.pos() });
        Expression::Const(w, typ)
    } else {
        let name = words.remove(0);
        let args = words.drain(0..).map(|arg| {
            println!("Arg: {}", &arg);
            let typ = type_of(&arg, parser.stack()).unwrap_or_else(|reason|
                Type::TypeError { reason, pos: parser.pos() });
            Expression::Const(arg, typ)
        }).collect();
        let t = parser.stack().get(&name).cloned()
            .unwrap_or_else(|| parser.error(&format!("Unknown function: '{}'", &name)));
        let typ = match t {
            Type::TypeError { .. } => vec![t],
            Type::Fn(FnType { outs, .. }) => outs,
            _ => vec![parser.error(&format!("Cannot use '{}' (which has type {}) as a function", &name, t))]
        };
        Expression::FnCall { name, args, typ }
    }
}

pub fn type_of(str: &str, stack: &Stack) -> Result<Type, String> {
    let mut chars = str.chars();
    type_of_with_sign(str, &mut chars, stack, false)
}

pub fn type_of_with_sign(str: &str, chars: &mut Chars, stack: &Stack, has_sign: bool) -> Result<Type, String> {
    if let Some(c) = chars.next() {
        match c {
            '0'..='9' => type_of_num(c, chars),
            '-' | '+' if !has_sign => type_of_with_sign(str, chars, stack, true),
            _ => type_of_var(str, stack)
        }
    } else {
        Ok(Type::Empty)
    }
}

fn type_of_var(str: &str, stack: &Stack) -> Result<Type, String> {
    stack.get(str).map(|t| Ok(t.to_owned())).unwrap_or_else(||
        Err(format!("variable '{}' does not exist", str)))
}

fn type_of_num(first_digit: char, chars: &mut Chars) -> Result<Type, String> {
    let mut has_dot = false;
    let mut whole_digits = 1;
    let mut decimal_digits = 0;
    let mut explicit_type: Option<Type> = None;
    let mut is_second_digit = true;

    while let Some(c) = chars.next() {
        match c {
            '0'..='9' => {
                if is_second_digit {
                    if first_digit == '0' && c == '0' {
                        return Err("number cannot start with more than one zero".to_string());
                    }
                    is_second_digit = false;
                }
                if has_dot {
                    decimal_digits += 1;
                } else {
                    whole_digits += 1;
                }
            }
            '_' => {}
            '.' => {
                if has_dot {
                    return Err("number contains more than one dot".to_string());
                }
                has_dot = true;
                is_second_digit = false;
            }
            'i' | 'f' => {
                match read_num_type(chars, c == 'i') {
                    Ok(t) => {
                        explicit_type = Some(t);
                        break;
                    }
                    Err(reason) => return Err(reason)
                }
            }
            _ => {
                return Err(format!("number contains invalid character: '{}'", c));
            }
        }
    }

    if has_dot {
        if decimal_digits == 0 {
            return Err("number cannot end with dot".to_string());
        }
        let fits_in_32_bits = decimal_digits + whole_digits < 11;
        if let Some(t) = explicit_type {
            return explicit_float_type(t, fits_in_32_bits);
        }
        Ok(if fits_in_32_bits { Type::F32 } else { Type::F64 })
    } else {
        if first_digit == '0' && whole_digits > 1 {
            return Err("non-zero integer cannot start with zero".to_string());
        }
        let fits_in_32_bits = whole_digits < 11;
        if let Some(t) = explicit_type {
            return explicit_int_type(t, fits_in_32_bits);
        }
        Ok(if fits_in_32_bits { Type::I32 } else { Type::I64 })
    }
}

fn read_num_type(chars: &mut Chars, is_int: bool) -> Result<Type, String> {
    let text: String = chars.collect();
    match text.as_ref() {
        "32" => return Ok(if is_int { Type::I32 } else { Type::F32 }),
        "64" => return Ok(if is_int { Type::I64 } else { Type::F64 }),
        _ => {}
    }
    Err(format!("Unexpected number suffix: '{}'. Valid suffixes are 'i32', 'i64', 'f32' or 'f64'.", text))
}

fn explicit_float_type(typ: Type, fits_in_32_bits: bool) -> Result<Type, String> {
    match typ {
        Type::I64 | Type::I32 => Err("number with decimal part cannot have integer suffix".to_string()),
        Type::F64 => Ok(Type::F64),
        Type::F32 => {
            if fits_in_32_bits {
                Ok(Type::F32)
            } else {
                Err("number is too big to fit in f32 (max total digits for f32 literals is 10)".to_string())
            }
        }
        _ => unreachable!()
    }
}

fn explicit_int_type(typ: Type, fits_in_32_bits: bool) -> Result<Type, String> {
    match typ {
        Type::I64 | Type::F64 => Ok(typ),
        Type::I32 | Type::F32 => {
            if fits_in_32_bits {
                Ok(typ)
            } else {
                Err("number is too big to fit in i32 (max total digits for i32 literals is 10)".to_string())
            }
        }
        _ => unreachable!()
    }
}
