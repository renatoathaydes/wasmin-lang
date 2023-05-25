use std::fmt::{Display, Formatter};
use std::fmt::Result as FmtResult;
use std::str::Chars;
use std::sync::mpsc::Sender;

use crate::ast::{Assignment, Comment, Expression, ExtDef, Function, TopLevelElement};
use crate::ast::Expression::ExprError;
use crate::errors::WasminError;
use crate::parse::{expr_parser, ext_parser, fun_parser, top_level_parser, type_parser};
pub use crate::parse::stack::*;
use crate::types::*;

#[derive(Debug)]
pub struct Parser<'s> {
    line: usize,
    col: usize,
    stack: Stack,
    chars: &'s mut Chars<'s>,
    curr_char: Option<char>,
    sink: Sender<TopLevelElement>,
    remember_comments: bool,
    comment: Option<Comment>,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum GroupingSymbol {
    Parens,
    SquareBracket,
}

impl Display for GroupingSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GroupingSymbol::Parens => {
                f.write_str("parenthesis")?;
            }
            GroupingSymbol::SquareBracket => {
                f.write_str("square-bracket")?;
            }
        };
        Ok(())
    }
}

#[derive(Debug)]
pub struct GroupingState {
    items: Vec<GroupingSymbol>,
}

impl GroupingState {
    pub fn new() -> GroupingState {
        GroupingState {
            items: Vec::with_capacity(2),
        }
    }

    pub fn is_inside(&self, symbol: &GroupingSymbol) -> bool {
        self.items.last().map(|g| g == symbol).unwrap_or(false)
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

impl Display for GroupingState {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        for item in &self.items {
            let str = match *item {
                GroupingSymbol::Parens => "(",
                GroupingSymbol::SquareBracket => "[",
            };
            f.write_str(str)?;
        }
        Ok(())
    }
}

impl Parser<'_> {
    pub fn new<'s>(
        chars: &'s mut Chars<'s>,
        stack: Stack,
        sink: Sender<TopLevelElement>,
    ) -> Parser<'s> {
        Parser {
            line: 0,
            col: 0,
            curr_char: Option::None,
            remember_comments: false,
            comment: None,
            chars,
            stack,
            sink,
        }
    }

    pub fn store_comments(&mut self, value: bool) {
        self.remember_comments = value;
    }

    pub fn take_comment(&mut self) -> Option<Comment> {
        self.store_comments(false);
        std::mem::replace(&mut self.comment, None)
    }

    pub fn curr_char(&self) -> Option<char> {
        self.curr_char
    }

    pub fn stack(&self) -> &Stack {
        &self.stack
    }

    pub fn stack_mut(&mut self) -> &mut Stack {
        &mut self.stack
    }

    fn next_after_comment(&mut self) -> Option<char> {
        let next = self.chars.next();
        let end_char = if let Some(c) = next {
            self.update_pos(c);
            if c == '{' {
                '}'
            } else {
                '\n'
            }
        } else {
            return None;
        };
        if self.remember_comments {
            let mut comment = String::with_capacity(32);
            if end_char == '\n' {
                // skip the '{' if it was given
                comment.push(next.unwrap());
            }
            while let Some(c) = self.chars.next() {
                self.update_pos(c);
                if c == end_char {
                    self.comment = Some(comment);
                    return self.chars.next();
                }
                comment.push(c);
            }
            self.comment = Some(comment);
        } else {
            while let Some(c) = self.chars.next() {
                self.update_pos(c);
                if c == end_char {
                    return self.chars.next();
                }
            }
        }
        None
    }

    fn update_pos(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let mut next = self.chars.next();
        if let Some(c) = next {
            self.update_pos(c);
            if c == '#' {
                next = self.next_after_comment();
            }
        }
        self.curr_char = next;
        next
    }

    pub fn pos(&self) -> (usize, usize) {
        (self.line, self.col)
    }

    pub fn sink(&self) -> &Sender<TopLevelElement> {
        &self.sink
    }

    pub fn parse_word(&mut self) -> Option<String> {
        self.skip_spaces();
        let c = self.curr_char?;
        space_sep_or!(c, { return None }, {});
        let mut word = String::with_capacity(8);
        word.push(c);
        while let Some(c) = self.next() {
            space_sep_or!(
                c,
                {
                    break;
                },
                { word.push(c) }
            );
        }
        Some(word)
    }

    pub fn skip_spaces(&mut self) {
        if let Some(c) = self.curr_char {
            space_or!(c, {}, {
                return;
            });
        }
        while let Some(c) = self.next() {
            space_or!(c, {}, {
                break;
            });
        }
    }

    pub fn parse_def(&mut self) -> Result<(), WasminError> {
        let start_pos = self.pos();
        if let Some(id) = self.parse_word() {
            let typ = self.parse_type();
            if let Err(msg) = self.stack.push(id, typ, true, false) {
                Err(werr_syntax!(msg, start_pos, self.pos()))
            } else {
                Ok(())
            }
        } else {
            let curr = self
                .curr_char
                .map_or("EOF".to_string(), |c| format!("'{}'", c));
            Err(werr_syntax!(
                format!("Expected identifier after def, but got {}", curr),
                self.pos()
            ))
        }
    }

    pub fn parse_assignment(&mut self, is_mut: bool) -> Result<Assignment, WasminError> {
        expr_parser::parse_assignment(self, is_mut)
    }

    pub fn parse_type(&mut self) -> Type {
        type_parser::parse_type(self)
    }

    pub fn parse_fun(&mut self) -> Result<Function, WasminError> {
        fun_parser::parse_fun(self)
    }

    pub fn parse_expr(&mut self) -> Expression {
        expr_parser::parse_expr(self).unwrap_or_else(|e| ExprError(e))
    }

    pub fn parse_ext(&mut self) -> Result<(String, Vec<ExtDef>), WasminError> {
        ext_parser::parse_ext(self)
    }

    pub fn parse(&mut self) {
        top_level_parser::parse(self)
    }
}
