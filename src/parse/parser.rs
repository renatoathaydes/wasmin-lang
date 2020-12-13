use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::fmt::Result as FmtResult;
use std::str::Chars;
use std::sync::mpsc::Sender;

use crate::ast::{Assignment, Expression, TopLevelExpression};
use crate::parse::{expr_parser, top_level_parser};
use crate::parse::type_parser;
use crate::types::{*};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ParserError {
    pub pos: (usize, usize),
    pub msg: String,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "[{}, {}]: {}", self.pos.0, self.pos.1, self.msg)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Stack {
    items: Vec<HashMap<String, Type>>
}

#[derive(Debug)]
pub struct Parser<'s> {
    line: usize,
    col: usize,
    stack: Stack,
    chars: &'s mut Chars<'s>,
    curr_char: Option<char>,
    sink: Sender<TopLevelExpression>,
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

    pub fn get(&self, id: &str) -> Option<&Type> {
        (0..self.items.len()).rev().find_map(|i| {
            let symbols = self.items.get(i).unwrap();
            if let Some(val) = symbols.get(id) {
                Some(val)
            } else { None }
        })
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

    /// Returns the length of the top-level stack container.
    pub fn len(&self) -> usize {
        self.items.len()
    }
}

impl Default for Stack {
    fn default() -> Self {
        Stack::new()
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
    pub fn new<'s>(
        chars: &'s mut Chars<'s>,
        stack: Stack,
        sink: Sender<TopLevelExpression>,
    ) -> Parser<'s> {
        Parser { line: 0, col: 0, curr_char: Option::None, chars, stack, sink }
    }

    pub fn curr_char(&self) -> Option<char> {
        self.curr_char
    }

    pub fn stack(&self) -> &Stack {
        &self.stack
    }

    pub fn error(&self, reason: &str) -> TypeError {
        TypeError { reason: reason.to_string(), pos: self.pos() }
    }

    pub fn error_unexpected_char(&self, c: char, reason: &str) -> TypeError {
        self.error(&format!("unexpected char: '{}' ({})", c, reason))
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

    pub fn sink(&self) -> &Sender<TopLevelExpression> {
        &self.sink
    }

    pub fn parser_err<T>(&mut self, msg: String) -> Result<T, ParserError> {
        let pos = self.pos();
        Result::Err(ParserError { pos, msg })
    }

    pub fn parse_word(&mut self) -> Option<String> {
        self.skip_spaces();
        let c = self.curr_char?;
        space_sep_or!(c, { return None }, {});
        let mut word = String::with_capacity(8);
        word.push(c);
        while let Some(c) = self.next() {
            space_sep_or!(c, { break; }, { word.push(c) });
        }
        Some(word)
    }

    pub fn skip_spaces(&mut self) {
        if let Some(c) = self.curr_char {
            space_or!(c, {}, { return; });
        }
        while let Some(c) = self.next() {
            space_or!(c, {}, { break; });
        }
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

    pub fn parse_let(&mut self) -> Result<Assignment, ParserError> {
        let mut ids = Vec::new();
        while let Some(id) = self.parse_word() {
            ids.push(id);
            self.skip_spaces();
            if let Some(',') = self.curr_char() { self.next(); } else { break; }
        }
        self.skip_spaces();
        if let Some('=') = self.curr_char() {
            self.next();
            self.stack.new_level();
            let expr = self.parse_expr();
            self.stack.drop_level();
            let mut typ = expr.get_type();
            if ids.len() == typ.len() {
                ids.iter().zip(typ.drain(..)).for_each(move |(id, t)| {
                    self.stack.push_item(id.clone(), t);
                });
                Ok((ids, expr.into_multi()))
            } else {
                let e = format!("multi-value assignment mismatch: \
                {} identifier{} but {} expression{} found",
                                ids.len(), if ids.len() == 1 { "" } else { "s" },
                                typ.len(), if typ.len() == 1 { "" } else { "s" });
                self.parser_err(e)
            }
        } else {
            self.parser_err(format!("Expected '=' in let expression, but got {}",
                                    self.curr_char().map(|c| format!("'{}'", c))
                                        .unwrap_or_else(|| "EOF".to_string())))
        }
    }

    pub fn parse_type(&mut self) -> Type {
        type_parser::parse_type(self)
    }

    pub fn parse_expr(&mut self) -> Expression {
        println!("Calling parse_expr");
        expr_parser::parse_expr(self)
    }

    pub fn parse(&mut self) {
        top_level_parser::parse(self)
    }
}

#[cfg(test)]
mod stack_tests {
    use super::*;

    #[test]
    fn stack_can_have_bindings() {
        let mut stack = Stack::new();
        assert_eq!(stack.get(&"foo"), None);
        stack.push_item("foo".to_string(), Type::Empty);
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.push_item("bar".to_string(), Type::I64);
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        assert_eq!(stack.get(&"z"), None);
    }

    #[test]
    fn stack_can_have_multi_level_bindings() {
        let mut stack = Stack::new();
        stack.push_item("foo".to_string(), Type::Empty);
        stack.new_level();
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.new_level();
        stack.push_item("bar".to_string(), Type::I64);
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.new_level();
        stack.push_item("z".to_string(), Type::F32);
        assert_eq!(stack.get(&"z"), Some(&Type::F32));
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.new_level();
        stack.push_item("foo".to_string(), Type::F64);
        assert_eq!(stack.get(&"z"), Some(&Type::F32));
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::F64));

        stack.drop_level();
        assert_eq!(stack.get(&"z"), Some(&Type::F32));
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.drop_level();
        assert_eq!(stack.get(&"z"), None);
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.drop_level();
        assert_eq!(stack.get(&"z"), None);
        assert_eq!(stack.get(&"bar"), None);
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
    }
}