use std::str::Chars;

use parser::{*};

use crate::ast::Expression;
use crate::types::{*, Type::*};

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


