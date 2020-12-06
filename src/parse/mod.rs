use std::str::Chars;
use crate::parse::parser::Stack;

#[macro_use]
mod macros;

#[cfg(test)]
mod tests;
mod type_parser;
mod parser;
mod expr_parser;


/// Parser of Wasmin programs.
pub type Parser<'s> = parser::Parser<'s>;

pub fn new_parser<'s>(chars: &'s mut Chars<'s>) -> Parser<'s> {
    Parser::new(chars)
}
pub fn new_parser_with_stack<'s>(chars: &'s mut Chars<'s>, stack: Stack) -> Parser<'s> {
    Parser::with_stack(chars, stack)
}


