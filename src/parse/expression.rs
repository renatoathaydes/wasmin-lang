use crate::ast::{AST, Expression, Type};
use crate::errors::WasminError;
use crate::parse::model::Token;
use crate::parse::parse::Parser;

impl<'s> Parser<'s> {
    pub(crate) fn parse_expr(&mut self) -> Expression {
        if let Some(token) = self.lexer.next() {
            match token {
                Token::Str(.., string) => self.ast.new_string(&string, vec![]),
                // TODO support local of any type
                Token::Id(.., name) => self.ast.new_local(&name, Type::I32, vec![]),
                Token::Number(.., value) => self.ast.new_number(value, vec![]),
                Token::Error(pos, err) => AST::new_error(WasminError::SyntaxError {
                    cause: err,
                    pos,
                }, vec![]),
                _ => AST::new_error(WasminError::UnsupportedFeatureError {
                    cause: "expression not supported yet".into(),
                    pos: token.pos(),
                }, vec![]),
            }
        } else {
            AST::empty()
        }
    }
}