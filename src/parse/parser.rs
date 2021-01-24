use std::fmt::{Display, Formatter};
use std::fmt::Result as FmtResult;
use std::str::Chars;
use std::sync::mpsc::Sender;

use crate::ast::{Assignment, Comment, Expression, ExtDef, Function, TopLevelElement};
use crate::ast::Expression::ExprError;
use crate::parse::{expr_parser, ext_parser, fun_parser, top_level_parser, type_parser};
pub use crate::parse::stack::{*};
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

impl Into<TypeError> for ParserError {
    fn into(self) -> TypeError {
        TypeError { reason: self.msg, pos: self.pos }
    }
}

impl From<TypeError> for ParserError {
    fn from(e: TypeError) -> Self {
        ParserError { msg: format!("(type error) {}", e.reason), pos: e.pos }
    }
}

impl Into<TopLevelElement> for ParserError {
    fn into(self) -> TopLevelElement {
        TopLevelElement::Error(self.msg, self.pos)
    }
}

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

#[derive(Debug)]
pub struct GroupingState { items: Vec<GroupingSymbol> }

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

    pub fn len(&self) -> usize { self.items.len() }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl Display for GroupingState {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        for item in &self.items {
            let str = match item {
                &GroupingSymbol::Parens => "(",
                &GroupingSymbol::SquareBracket => "[",
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

    pub fn get_comment(&mut self, remember_comments: bool) -> Option<Comment> {
        self.store_comments(remember_comments);
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

    pub fn error(&self, reason: &str) -> TypeError {
        TypeError { reason: reason.to_string(), pos: self.pos() }
    }

    pub fn error_unexpected_char(&self, c: char, reason: &str) -> TypeError {
        self.error(&format!("unexpected char: '{}' ({})", c, reason))
    }

    fn next_after_comment(&mut self) -> Option<char> {
        let next = self.chars.next();
        let end_char = if let Some(c) = next {
            self.update_pos(c);
            if c == '{' { '}' } else { '\n' }
        } else {
            return None;
        };
        if self.remember_comments {
            let mut comment = String::with_capacity(32);
            if end_char == '\n' { // skip the '{' if it was given
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
            if let Err(msg) = self.stack.push(id, typ, true, false) {
                self.parser_err(msg)
            } else {
                Result::Ok(())
            }
        } else {
            let curr = self.curr_char.map_or("EOF".to_string(), |c| { format!("'{}'", c) });
            self.parser_err(format!("Expected identifier after def, but got {}", curr))
        }
    }

    pub fn parse_assignment(&mut self, is_mut: bool) -> Result<Assignment, ParserError> {
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
            let pos = self.pos();
            let expr = self.parse_expr();
            self.stack.drop_level();
            let mut typ = expr.get_type();
            if ids.len() == typ.len() {
                let (mut results, mut errors): (Vec<_>, Vec<_>) = ids.iter().zip(typ.drain(..))
                    .map(move |(id, t)| {
                        self.stack.push(id.clone(), t, false, is_mut)
                    }).partition(|r| r.is_ok());
                if errors.is_empty() {
                    let type_replacements = results.drain(..)
                        .map(|r| r.unwrap()).collect();
                    Ok((ids, Box::new(expr), type_replacements))
                } else {
                    let msg = errors.drain(..).map(|e| e.unwrap_err())
                        .collect::<Vec<_>>().join(", ");
                    Err(ParserError { pos, msg })
                }
            } else {
                let e = format!("multi-value assignment mismatch: \
                {} identifier{} but {} expression{} of type{3} '{}' found",
                                ids.len(), if ids.len() == 1 { "" } else { "s" },
                                typ.len(), if typ.len() == 1 { "" } else { "s" },
                                typ.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join(" "));
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

    pub fn parse_fun(&mut self) -> Result<Function, ParserError> {
        fun_parser::parse_fun(self)
    }

    pub fn parse_expr(&mut self) -> Expression {
        // FIXME error handling
        expr_parser::parse_expr(self).unwrap_or_else(|e|
            ExprError(TypeError { reason: e, pos: (0, 0) }))
    }

    pub fn parse_ext(&mut self) -> Result<(String, Vec<ExtDef>), ParserError> {
        ext_parser::parse_ext(self)
    }

    pub fn parse(&mut self) {
        top_level_parser::parse(self)
    }
}
