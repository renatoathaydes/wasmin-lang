use std::str::Chars;
use std::sync::mpsc::Sender;

use crate::ast::TopLevelExpression;
use crate::parse::parser::Stack;

#[macro_use]
mod macros;

#[cfg(test)]
mod tests;
mod type_parser;
mod parser;
mod expr_parser;
mod fun_parser;
mod top_level_parser;
pub(crate) mod stack;

/// Parser of Wasmin programs.
pub type Parser<'s> = parser::Parser<'s>;

#[cfg(test)]
fn no_op_sink() -> Sender<TopLevelExpression> {
    use std::sync::mpsc::channel;
    let (sink, _) = channel();
    sink
}

#[cfg(test)]
pub(crate) fn new_parser_without_sink<'s>(chars: &'s mut Chars<'s>) -> Parser<'s> {
    let sink = no_op_sink();
    Parser::new(chars, Stack::default(), sink)
}

#[cfg(test)]
pub(crate) fn new_parser_with_stack<'s>(chars: &'s mut Chars<'s>, stack: Stack) -> Parser<'s> {
    let sink = no_op_sink();
    Parser::new(chars, stack, sink)
}

pub fn new_parser<'s>(chars: &'s mut Chars<'s>, sender: Sender<TopLevelExpression>) -> Parser<'s> {
    Parser::new(chars, Stack::default(), sender)
}
