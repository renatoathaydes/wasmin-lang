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
        let typ = type_of(&w, parser.stack()).unwrap_or_else(|reason|
            Type::Error { reason, pos: parser.pos() });
        Expression::Const(w, typ)
    } else {
        let name = words.remove(0);
        let args = words.drain(0..).map(|arg| {
            println!("Arg: {}", &arg);
            let typ = type_of(&arg, parser.stack()).unwrap_or_else(|reason|
                Type::Error { reason, pos: parser.pos() });
            Expression::Const(arg, typ)
        }).collect();
        println!("Ags are : {:?}", &args);
        let typ = parser.stack().get(&name).map(|t| t.clone())
            .unwrap_or_else(|| parser.error(&format!("Unknown function: '{}'", &name)));

        println!("Done here");
        Expression::FnCall { name, args, typ }
    }
}

pub fn type_of(str: &str, stack: &Stack) -> Result<Type, String> {
    let mut chars = str.chars();
    type_of_with_sign(str, &mut chars, stack, None)
}

pub fn type_of_with_sign(str: &str, chars: &mut Chars, stack: &Stack, sign: Option<bool>) -> Result<Type, String> {
    if let Some(c) = chars.next() {
        match c {
            '0'..='9' => type_of_num(c, chars),
            '-' | '+' =>
                if let Some(_) = sign {
                    // already consumed sign, so this is not a number
                    type_of_var(str, stack)
                } else {
                    type_of_with_sign(str, chars, stack, Some(c == '+'))
                },
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

    loop {
        if let Some(c) = chars.next() {
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
        } else {
            break;
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
