use std::str::Chars;

use crate::ast::Expression;
use crate::types::{Type::*};
use crate::types::type_of;

pub fn parse(chars: &mut Chars) -> Expression {
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
                ')' => {
                    let t = type_of(&value);
                    return if t == Empty { Expression::Empty } else { Expression::Const(value, t) };
                }
                ' ' => {}
                _ => value.push(c),
            };
        } else {
            let t = type_of(&value);
            return if t == Empty { Expression::Empty } else { Expression::Const(value, t) };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        let mut chars = "()".chars();
        assert_eq!(parse(&mut chars), Expression::Empty);
    }

    #[test]
    fn test_i32() {
        let mut chars = "(0)".chars();
        assert_eq!(parse(&mut chars), Expression::Const(String::from("0"), I32));

        let mut chars = "( 1 )".chars();
        assert_eq!(parse(&mut chars), Expression::Const(String::from("1"), I32));

        let mut chars = "( 100)".chars();
        assert_eq!(parse(&mut chars), Expression::Const(String::from("100"), I32));
    }
    #[test]
    fn test_f32() {
        let mut chars = "(0.0)".chars();
        assert_eq!(parse(&mut chars), Expression::Const(String::from("0.0"), F32));

        let mut chars = "( 1.0 )".chars();
        assert_eq!(parse(&mut chars), Expression::Const(String::from("1.0"), F32));

        let mut chars = "( 1.00)".chars();
        assert_eq!(parse(&mut chars), Expression::Const(String::from("1.00"), F32));
    }
}