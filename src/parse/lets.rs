use crate::ast::{AST, Expression, TopLevelElement, Type, Visibility};
use crate::errors::WasminError;
use crate::parse::model::{Position, Token};
use crate::parse::parse::Parser;

impl<'s> Parser<'s> {
    pub(crate) fn parse_let(&mut self, pos: Position, visibility: Visibility) -> TopLevelElement {
        match self.parse_defs(pos) {
            Ok(vars) => {
                let expr = self.parse_expr();
                let assignment = self.ast.new_assignments(vars, expr);
                let scope = self.scope.last_mut().expect("there must be a root scope");
                let mut var_types = assignment.get_types();
                for (var, typ) in var_types.drain(..) {
                    scope.insert(var.name, typ);
                }
                // TODO parse comments
                let comment = None;
                let w = vec![];
                return TopLevelElement::Let(assignment, visibility, comment, w);
            }
            Err(err) => {
                TopLevelElement::Error(err)
            }
        }
    }

    pub(crate) fn parse_defs(&mut self, pos: Position)
                             -> Result<Vec<(String, Option<Type>)>, WasminError> {
        let mut vars: Vec<(String, Option<Type>)> = Vec::with_capacity(4);
        let mut after_id = false;
        while let Some(token) = self.lexer.next() {
            if after_id {
                match token {
                    Token::Comma(_) => after_id = false,
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
                        return Ok(vars);
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
                    _ => {
                        return Err(WasminError::SyntaxError {
                            pos: token.pos(),
                            cause: "expected an identifier at this position".into(),
                        });
                    }
                }
            }
        }
        Err(WasminError::SyntaxError {
            pos,
            cause: "incomplete let expression: missing name".into(),
        })
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

    #[test]
    fn test_parse_let_typed_pub() {
        let mut ast = AST::new();
        let expr = ast.new_number(Numeric::I32(2), vec![]);
        let assignment = ast.new_assignment("abc", Some(I32), expr);
        let mut parser = Parser::new_with_ast("  pub let abc : i32 = 2", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Let(
            assignment,
            Visibility::Public, None, vec![])))
    }

    #[test]
    fn test_parse_let_multiple() {
        let mut ast = AST::new();
        let expr = AST::new_group(vec![
            ast.new_number(Numeric::I32(1), vec![]),
            ast.new_number(Numeric::I32(2), vec![]),
            ast.new_number(Numeric::I32(3), vec![]),
        ], vec![]);
        let assignment = ast.new_assignments(vec![
            ("x".to_owned(), None),
            ("y".to_owned(), None),
            ("z".to_owned(), None),
        ], expr);
        let mut parser = Parser::new_with_ast("let x, y, z = 1, 2 , 3", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Let(
            assignment,
            Visibility::Private, None, vec![])))
    }
}
