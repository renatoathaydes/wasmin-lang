use std::fmt::format;
use std::ops::Index;

use crate::ast::{AST, Expression, Function, TopLevelElement, Type, Visibility, Warning};
use crate::errors::WasminError;
use crate::parse::model::{Position, Token};
use crate::parse::parse::Parser;

impl<'s> Parser<'s> {
    pub(crate) fn parse_if(&mut self, pos: Position) -> Expression {
        todo!()
    }
}