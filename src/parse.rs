use std::collections::HashMap;
use std::str::Chars;

use crate::ast::Expression;
use crate::types::{*, Type::*};
use crate::types::type_of;

/// space_or takes a char and two expressions.
/// The first expression is evaluated if the char is whitespace.
/// The second expression is evaluated otherwise.
macro_rules! space_or {
    ( $c:expr, $space:expr, $other:expr ) => {
        match $c {
            ' ' | '\n' | '\t' | '\r' => $space,
            _ => $other
        }
    };
}

pub struct Parser<'s> {
    line: usize,
    col: usize,
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
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
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

    fn pos(&mut self) -> (usize, usize) {
        (self.line, self.col)
    }

    fn parse_word(&mut self) -> Option<String> {
        self.skip_spaces();
        let c = self.curr_char;
        if c.is_none() { return None; }
        let mut word = String::with_capacity(8);
        word.push(c.unwrap());
        loop {
            if let Some(c) = self.next() {
                space_or!(c, { break; }, { word.push(c) });
            } else { break; }
        }
        Some(word)
    }

    fn skip_spaces(&mut self) {
        if let Some(c) = self.curr_char {
            space_or!(c, {}, { return; });
        }
        loop {
            if let Some(c) = self.next() {
                space_or!(c, {}, { break; });
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
        assert_eq!(parser.parse_word(), Some("abc".to_string()));
    }

    #[test]
    fn test_words() {
        let mut chars = "a b  cde    fgh\nij\n  klmnop  \r\n  rs  ".chars();
        let mut parser = new_parser(&mut chars);
        assert_eq!(parser.parse_word(), Some("a".to_string()));
        assert_eq!(parser.parse_word(), Some("b".to_string()));
        assert_eq!(parser.parse_word(), Some("cde".to_string()));
        assert_eq!(parser.parse_word(), Some("fgh".to_string()));
        assert_eq!(parser.parse_word(), Some("ij".to_string()));
        assert_eq!(parser.parse_word(), Some("klmnop".to_string()));
        assert_eq!(parser.parse_word(), Some("rs".to_string()));
        assert_eq!(parser.parse_word(), None);
        assert_eq!(parser.parse_word(), None);
    }

    #[test]
    fn test_pos() {
        let mut chars = "a b  cde    fgh\nij\n  klmnop  \r\n  rs  ".chars();
        let mut parser = new_parser(&mut chars);
        assert_eq!(parser.pos(), (0, 0));
        parser.parse_word();
        assert_eq!(parser.pos(), (0, 2));
        parser.parse_word();
        assert_eq!(parser.pos(), (0, 4));
        parser.parse_word();
        assert_eq!(parser.pos(), (0, 9));
        parser.parse_word();
        assert_eq!(parser.pos(), (1, 0));
        parser.parse_word();
        assert_eq!(parser.pos(), (2, 0));
        parser.parse_word();
        assert_eq!(parser.pos(), (2, 9));
        parser.parse_word();
        assert_eq!(parser.pos(), (3, 5));
    }
}
