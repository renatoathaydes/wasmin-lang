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

struct Stack {
    items: Vec<HashMap<String, Type>>
}

impl Stack {
    fn new() -> Stack {
        let mut s = Stack { items: Vec::with_capacity(4) };
        s.new_level();
        s
    }

    /// push_item onto stack, returning None if the item is accepted, or its ID otherwise.
    fn push_item(&mut self, id: String, typ: Type) -> Option<String> {
        let mut symbols = self.items.remove(self.items.len() - 1);
        if !symbols.contains_key(&id) {
            symbols.insert(id, typ);
            self.items.push(symbols);
            None
        } else {
            self.items.push(symbols);
            Some(id)
        }
    }

    fn get(&self, id: String) -> Option<&Type> {
        let i = self.items.len() - 1;
        while i >= 0 as usize {
            let symbols = self.items.get(i).unwrap();
            if let Some(val) = symbols.get(&id) {
                return Some(val);
            }
        }
        None
    }

    fn new_level(&mut self) {
        self.items.push(HashMap::new());
    }

    fn drop_level(&mut self) {
        let len = self.items.len();
        if len > 1 {
            self.items.remove(len - 1);
        } else {
            panic!("attempt to drop single stack level");
        }
    }
}

/// Parser of Wasmin programs.
pub struct Parser<'s> {
    line: usize,
    col: usize,
    stack: Stack,
    chars: &'s mut Chars<'s>,
    curr_char: Option<char>,
}

pub fn new_parser<'s>(chars: &'s mut Chars<'s>) -> Parser<'s> {
    let stack = Stack::new();
    Parser { line: 0, col: 0, stack, curr_char: Option::None, chars }
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

    fn parse_def(&mut self) -> Result<(), ParserError> {
        if let Some(id) = self.parse_word() {
            let typ = self.parse_type();
            match typ {
                Type::Error { text, reason } => {
                    self.parser_err(format!("Bad type in '{}' def: '{}' --> {}", id, text, reason))
                }
                _ => {
                    if let Some(id_back) = self.stack.push_item(id, typ) {
                        self.parser_err(format!("Attempting to re-define {}, which \
                            is not allowed", &id_back))
                    } else {
                        Result::Ok(())
                    }
                }
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

