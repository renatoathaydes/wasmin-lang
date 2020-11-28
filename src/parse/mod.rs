use std::collections::HashMap;
use std::str::Chars;

use crate::ast::Expression;
use crate::types::{*, Type::*};
use crate::types::type_of;

#[macro_use]
mod macros;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ParserError {
    pos: (usize, usize),
    msg: String,
}

/// Parser of Wasmin programs.
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

    fn pos(&mut self) -> (usize, usize) {
        (self.line, self.col)
    }

    fn parser_err<T>(&mut self, msg: String) -> Result<T, ParserError> {
        let pos = self.pos();
        Result::Err(ParserError { pos, msg })
    }

    fn parse_word(&mut self) -> Option<String> {
        self.skip_spaces();
        let c_opt = self.curr_char;
        if c_opt.is_none() { return None; }
        let c = c_opt.unwrap();
        space_sep_or!(c, { return None }, {});
        let mut word = String::with_capacity(8);
        word.push(c);
        loop {
            if let Some(c) = self.next() {
                space_sep_or!(c, { break; }, { word.push(c) });
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

    fn parse_type(&mut self) -> Type {
        if let Some(word) = self.parse_word() {
            match word.as_ref() {
                "i32" => Type::I32,
                "f32" => Type::F32,
                "i64" => Type::I64,
                "f64" => Type::F64,
                _ => Type::error(word.as_ref(), "type does not exist")
            }
        } else if let Some(c) = self.curr_char {
            Type::error(c.to_string().as_ref(), "unexpected character")
        } else {
            Type::error("", "EOF reached")
        }
    }

    fn parse_def(&mut self) -> Result<Expression, ParserError> {
        if let Some(id) = self.parse_word() {
            let typ = self.parse_type();
            match typ {
                Type::Error { text, reason } => {
                    self.parser_err(format!("Bad type in '{}' def: '{}' --> {}", id, text, reason))
                }
                _ => Result::Ok(Expression::Const(id, typ))
            }
        } else {
            let curr = self.curr_char.map_or("EOF".to_string(), |c| { format!("{}", c) });
            self.parser_err(format!("Expected identifier after def, but got {}", curr))
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

