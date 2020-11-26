use std::collections::HashMap;
use std::str::Chars;

use crate::ast::Expression;
use crate::types::{*, Type::*};
use crate::types::type_of;

pub struct Parser<'s> {
    line: u32,
    col: u32,
    symbols: HashMap<String, Type>,
    chars: &'s mut Chars<'s>,
    curr_char: Option<char>,
}

pub fn new_parser<'s>(chars: &'s mut Chars<'s>) -> Parser<'s> {
    Parser { line: 0, col: 0, symbols: HashMap::new(), curr_char: Option::None, chars }
}

impl Parser<'_> {
    fn next(&mut self) -> Option<char> {
        let next = self.chars.next();
        if let Some(c) = next {
            if c == '\n' { self.line += 1; } else { self.col += 1; }
        }
        self.curr_char = next;
        next
    }

    // fn parse_expr(&mut self) -> Expression {
    //     if let Some(c) = self.next() {
    //         match c {
    //             '(' => parse_parens(chars),
    //             _ => {
    //                 self.parse_word()
    //             }
    //         }
    //     } else {
    //         Expression::Empty
    //     }
    // }

    fn parse_word(&mut self) -> String {
        self.skip_spaces();
        let c = self.curr_char;
        if c.is_none() { return "".to_string(); }
        let mut word = String::with_capacity(8);
        word.push(c.unwrap());
        loop {
            if let Some(c) = self.next() {
                match c {
                    ' ' | '\n' | '\t' => { return word; }
                    _ => { word.push(c) }
                }
            } else { break; }
        }
        word
    }

    fn skip_spaces(&mut self) {
        if let Some(c) = self.curr_char {
            match c {
                ' ' | '\n' | '\t' => {}
                _ => { return; }
            }
        }
        loop {
            if let Some(c) = self.next() {
                match c {
                    ' ' | '\n' | '\t' => { /* continue */ }
                    _ => break
                }
            } else { break; }
        }
    }
}

pub fn parse_expr(chars: &mut Chars) -> Expression {
    if let Some(c) = chars.next() {
        match c {
            '(' => parse_parens(chars),
            _ => Expression::Empty,
        }
    } else {
        Expression::Empty
    }
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
        assert_eq!(parse_expr(&mut chars), Expression::Empty);
    }

    #[test]
    fn test_i32() {
        let mut chars = "(0)".chars();
        assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("0"), I32));

        let mut chars = "( 1 )".chars();
        assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("1"), I32));

        let mut chars = "( 100)".chars();
        assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("100"), I32));
    }

    #[test]
    fn test_f32() {
        let mut chars = "(0.0)".chars();
        assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("0.0"), F32));

        let mut chars = "( 1.0 )".chars();
        assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("1.0"), F32));

        let mut chars = "( 1.00)".chars();
        assert_eq!(parse_expr(&mut chars), Expression::Const(String::from("1.00"), F32));
    }

    #[test]
    fn test_word() {
        let mut chars = "abc".chars();
        let mut parser = new_parser(&mut chars);
        assert_eq!(parser.parse_word(), "abc");
    }
}