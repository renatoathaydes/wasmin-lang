use crate::ast::{AST, Expression, TopLevelElement, Type, Visibility};
use crate::errors::WasminError;
use crate::parse::model::{Position, Token};
use crate::parse::parse::Parser;

impl<'s> Parser<'s> {
    pub(crate) fn parse_let(&mut self, pos: Position) -> TopLevelElement {
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

    fn parse_defs(&mut self, pos: Position)
                  -> Result<(Vec<(String, Option<Type>)>, Visibility), WasminError> {
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
                        let typ = self.parse_type(pos)?;
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
