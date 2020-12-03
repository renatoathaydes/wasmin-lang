use std::collections::HashMap;
use std::str::Chars;

use crate::parse::types::parse_type_internal;
use crate::types::{*};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ParserError {
    pub pos: (usize, usize),
    pub msg: String,
}

pub struct Stack {
    items: Vec<HashMap<String, Type>>
}

pub struct Parser<'s> {
    line: usize,
    col: usize,
    stack: Stack,
    chars: &'s mut Chars<'s>,
    curr_char: Option<char>,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum GroupingSymbol {
    Parens,
    SquareBracket,
}

#[derive(Debug)]
pub struct GroupingState { items: Vec<GroupingSymbol> }

impl Stack {
    pub fn new() -> Stack {
        let mut s = Stack { items: Vec::with_capacity(4) };
        s.new_level();
        s
    }

    /// push_item onto stack, returning None if the item is accepted, or its ID otherwise.
    pub fn push_item(&mut self, id: String, typ: Type) -> Option<String> {
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

    pub fn get(&self, id: String) -> Option<&Type> {
        let i = self.items.len() - 1;
        while i >= 0 as usize {
            let symbols = self.items.get(i).unwrap();
            if let Some(val) = symbols.get(&id) {
                return Some(val);
            }
        }
        None
    }

    pub fn new_level(&mut self) {
        self.items.push(HashMap::new());
    }

    pub fn drop_level(&mut self) {
        let len = self.items.len();
        if len > 1 {
            self.items.remove(len - 1);
        } else {
            panic!("attempt to drop single stack level");
        }
    }
}

impl GroupingState {
    pub fn new() -> GroupingState {
        GroupingState { items: Vec::with_capacity(2) }
    }

    pub fn is_inside(&self, symbol: GroupingSymbol) -> bool {
        self.items.last().map(|g| g == &symbol).unwrap_or(false)
    }

    pub fn enter(&mut self, symbol: GroupingSymbol) {
        self.items.push(symbol);
    }

    pub fn exit_symbol(&mut self) {
        let idx = self.items.len() - 1;
        self.items.remove(idx);
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl Parser<'_> {
    pub fn new<'s>(chars: &'s mut Chars<'s>) -> Parser<'s> {
        let stack = Stack::new();
        Parser { line: 0, col: 0, stack, curr_char: Option::None, chars }
    }

    pub fn curr_char(&self) -> Option<char> {
        self.curr_char
    }

    pub fn stack(&self) -> &Stack {
        &self.stack
    }

    pub fn error(&self, reason: &str) -> Type {
        Type::Error { reason: reason.to_string(), pos: self.pos() }
    }

    pub fn next(&mut self) -> Option<char> {
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

    pub fn pos(&self) -> (usize, usize) {
        (self.line, self.col)
    }

    pub fn parser_err<T>(&mut self, msg: String) -> Result<T, ParserError> {
        let pos = self.pos();
        Result::Err(ParserError { pos, msg })
    }

    pub fn parse_word(&mut self) -> Option<String> {
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

    pub fn skip_spaces(&mut self) {
        if let Some(c) = self.curr_char {
            space_or!(c, {}, { return; });
        }
        loop {
            if let Some(c) = self.next() {
                space_or!(c, {}, { break; });
            } else { break; }
        }
    }

    pub fn parse_type(&mut self) -> Type {
        parse_type_internal(self)
    }

    pub fn parse_def(&mut self) -> Result<(), ParserError> {
        if let Some(id) = self.parse_word() {
            let typ = self.parse_type();
            if let Some(id) = self.stack.push_item(id, typ) {
                self.parser_err(format!("Attempting to re-define {}, which \
                            is not allowed", &id))
            } else {
                Result::Ok(())
            }
        } else {
            let curr = self.curr_char.map_or("EOF".to_string(), |c| { format!("'{}'", c) });
            self.parser_err(format!("Expected identifier after def, but got {}", curr))
        }
    }
}
