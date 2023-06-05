use crate::ast::{Assignment, AST, Comment, Constant, Expression, TopLevelElement, Type, Visibility};
use crate::errors::WasminError;
use crate::interner::{InternedStr, Interner};
use crate::parse::lex::Lexer;
use crate::parse::model::{Position, Token};
use crate::parse::types::parse_type;

pub struct Parser<'s> {
    pub(crate) lexer: Lexer<'s>,
    pub(crate) ast: AST,
    comments: Vec<Comment>,
    stack: Vec<Type>,
}

impl<'s> Parser<'s> {
    pub fn new(text: &str) -> Parser {
        Parser { lexer: Lexer::new(text), ast: AST::new(), comments: vec![], stack: vec![] }
    }

    pub fn new_with_ast(text: &str, ast: AST) -> Parser {
        Parser { lexer: Lexer::new(text), ast, comments: vec![], stack: vec![] }
    }

    pub fn parse_next(&mut self) -> Option<TopLevelElement> {
        if let Some(token) = self.lexer.next() {
            let expr = match token {
                Token::Let(pos) => {
                    self.parse_let(pos)
                }
                _ => {
                    TopLevelElement::Error(WasminError::UnsupportedFeatureError {
                        cause: "Only let is supported for now".into(),
                        pos: self.lexer.pos(),
                    })
                }
            };
            Some(expr)
        } else {
            None
        }
    }

    fn parse_defs(&mut self, pos: Position) -> Result<(Vec<(String, Option<Type>)>, Visibility), WasminError> {
        let mut vars: Vec<(String, Option<Type>)> = Vec::with_capacity(4);
        let mut after_id = false;
        let mut visibility = Visibility::Private;
        while let Some(token) = self.lexer.next() {
            if after_id {
                match token {
                    Token::Comma(_) => { todo!() }
                    Token::Colon(pos) => {
                        if vars.is_empty() {
                            return Err(WasminError::SyntaxError {
                                pos: token.pos(),
                                cause: "type declaration starting without any \
                                    variable names being defined".into(),
                            });
                        }
                        let typ = parse_type(self, pos)?;
                        let (name, _) = vars.remove(vars.len() - 1);
                        vars.push((name, Some(typ)));
                    }
                    Token::Eq(_) => {
                        return Ok((vars, visibility));
                    }
                    _ => {
                        // TODO skip to the next closing block
                        return Err(WasminError::SyntaxError {
                            pos: token.pos(),
                            cause: "expected only '=' or ',' at this position".into(),
                        });
                    }
                }
            } else {
                match token {
                    Token::Id(_, name) => {
                        after_id = true;
                        vars.push((name, None));
                    }
                    Token::Pub(pos) if vars.is_empty() => {
                        if visibility == Visibility::Public {
                            // TODO skip to the next closing block
                            return Err(WasminError::SyntaxError {
                                pos,
                                cause: "'pub' can only appear once per definition".into(),
                            });
                        } else {
                            visibility = Visibility::Public;
                        }
                    }
                    _ => {
                        return Err(WasminError::SyntaxError {
                            pos: token.pos(),
                            cause: "expected only 'pub' or identifier at this position".into(),
                        });
                    }
                }
            }
            // return TopLevelElement::Error(WasminError::SyntaxError { pos, cause: "".into() });
        }
        // else {
        Err(WasminError::SyntaxError {
            pos,
            cause: "incomplete let expression: missing name".into(),
        })
        // }
    }

    fn parse_let(&mut self, pos: Position) -> TopLevelElement {
        match self.parse_defs(pos) {
            Ok((vars, visibility)) => {
                let expr = self.parse_expr();
                let assignment = self.ast.new_assignments(vars, expr);
                let comment = None;
                let w = vec![];
                return TopLevelElement::Let(assignment, visibility, comment, w);
            }
            Err(err) => {
                TopLevelElement::Error(err)
            }
        }
    }

    fn parse_expr(&mut self) -> Expression {
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


#[cfg(test)]
mod tests {
    use crate::parse::model::Numeric;

    use super::*;
    use super::Type::*;

    #[test]
    fn test_parse_let() {
        let mut ast = AST::new();
        let expr = ast.new_number(Numeric::I32(1), vec![]);
        let assignment = ast.new_assignment("x", None, expr);
        let mut parser = Parser::new_with_ast("let x = 1", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Let(
            assignment,
            Visibility::Private, None, vec![])))
    }

    #[test]
    fn test_parse_let_typed() {
        let mut ast = AST::new();
        let expr = ast.new_number(Numeric::I32(1), vec![]);
        let assignment = ast.new_assignment("x", Some(I64), expr);
        let mut parser = Parser::new_with_ast("let x: i64 = 1", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Let(
            assignment,
            Visibility::Private, None, vec![])))
    }
}