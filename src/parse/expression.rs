use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

use crate::ast::{AST, Expression, Type};
use crate::errors::WasminError;
use crate::parse::model::{Position, Token};
use crate::parse::parse::Parser;

#[derive(Copy, Clone, PartialEq)]
enum Nesting {
    Parens,
    Curly,
}

impl Nesting {
    fn close_symbol(&self) -> &'static str {
        match self {
            Nesting::Parens => ")",
            Nesting::Curly => "}",
        }
    }
}

impl<'s> Parser<'s> {
    pub(crate) fn parse_expr(&mut self) -> Expression {
        let mut nesting = Vec::new();
        self.parse_expr_nesting(&mut nesting, true)
    }

    fn enter_scope(&mut self) {
        self.scope.push(HashMap::with_capacity(4));
    }

    fn exit_scope(&mut self) {
        self.scope.pop();
    }

    fn parse_expr_nesting(&mut self, nesting: &mut Vec<Nesting>, multiple: bool) -> Expression {
        let mut expressions = Vec::new();
        let mut done = false;
        while let Some(token) = self.lexer.next() {
            if done { break; }
            let expr = match token {
                Token::Str(.., string) => self.ast.new_string(&string, vec![]),
                Token::Id(.., name) => self.ast.new_local(&name, Type::I32, vec![]),
                Token::Number(.., value) => self.ast.new_number(value, vec![]),
                Token::Let(pos) => self.parse_let_expr(pos),
                Token::Error(pos, err) => AST::new_error(WasminError::SyntaxError {
                    cause: err,
                    pos,
                }, vec![]),
                Token::OpenParens(_) => {
                    nesting.push(Nesting::Parens);
                    self.enter_scope();
                    self.parse_expr_nesting(nesting, true)
                }
                Token::CloseParens(pos) => {
                    self.exit_scope();
                    close_expr_with(Nesting::Parens, nesting.pop(), pos, &mut expressions);
                    break;
                }
                Token::OpenCurly(_) => {
                    nesting.push(Nesting::Curly);
                    // curly braces end an expression eagerly
                    done = true;
                    self.enter_scope();
                    self.parse_expr_nesting(nesting, true)
                }
                Token::CloseCurly(pos) => {
                    self.exit_scope();
                    close_expr_with(Nesting::Curly, nesting.pop(), pos, &mut expressions);
                    break;
                }
                Token::Comma(..) => continue,
                Token::SemiColon(..) => break,
                _ => AST::new_error(WasminError::UnsupportedFeatureError {
                    cause: "expression not supported yet".into(),
                    pos: token.pos(),
                }, vec![]),
            };
            push_flattened_into(&mut expressions, expr);
            if !multiple { break; }
        }
        collapse_expressions(expressions)
    }

    fn parse_let_expr(&mut self, pos: Position) -> Expression {
        match self.parse_defs(pos) {
            Ok(vars) => {
                let mut nesting = Vec::new();
                let expressions: Vec<_> = (0..vars.len()).map(|_| {
                    self.parse_expr_nesting(&mut nesting, false)
                }).collect();
                let assignment = self.ast.new_assignments(vars, collapse_expressions(expressions));
                let scope = self.scope.last_mut().expect("there must be a scope");
                let mut var_types = assignment.get_types();
                for (var, typ) in var_types.drain(..) {
                    scope.insert(var.name, typ);
                }
                Expression::Let(assignment, vec![])
            }
            Err(err) => {
                Expression::ExprError(err, vec![])
            }
        }
    }
}

fn collapse_expressions(mut expressions: Vec<Expression>) -> Expression {
    if expressions.is_empty() {
        AST::empty()
    } else if expressions.len() == 1 {
        expressions.remove(0)
    } else {
        AST::new_group(expressions, vec![])
    }
}

fn close_expr_with(
    closer: Nesting,
    opener: Option<Nesting>,
    pos: Position,
    expressions: &mut Vec<Expression>,
) {
    if let Some(op) = opener {
        if op != closer {
            expressions.push(Expression::ExprError(WasminError::SyntaxError {
                pos,
                cause: format!("unexpected '{}', did you mean to close a block with '{}'?",
                               closer.close_symbol(), op.close_symbol()),
            }, vec![]));
        }
    } else {
        expressions.push(Expression::ExprError(WasminError::SyntaxError {
            pos,
            cause: format!("unexpected '{}', no scope to be closed.", closer.close_symbol()),
        }, vec![]));
    }
}

fn push_flattened_into(expressions: &mut Vec<Expression>, expr: Expression) {
    match expr {
        Expression::Group { mut exprs, .. } => {
            expressions.append(&mut exprs);
        }
        _ => expressions.push(expr),
    };
}

#[cfg(test)]
mod tests {
    use crate::ast::{Constant, ExprType};
    use crate::ast::Expression::Const;
    use crate::ast::Visibility::{Private, Public};
    use crate::parse::model::Numeric;

    use super::*;
    use super::Type::*;

    #[test]
    fn test_parse_numbers() {
        let mut ast = AST::new();
        let group = AST::new_group(vec![
            ast.new_number(Numeric::I32(1), vec![]),
            ast.new_number(Numeric::I64(2), vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast("1, 2i64;", ast);
        assert_eq!(parser.parse_expr(), group);
    }

    #[test]
    fn test_parse_numbers_in_parens() {
        let mut ast = AST::new();
        let group = AST::new_group(vec![
            ast.new_number(Numeric::I32(1), vec![]),
            ast.new_number(Numeric::I64(2), vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast("(1) (2i64);", ast);
        assert_eq!(parser.parse_expr(), group);
    }

    #[test]
    fn test_parse_number_groups_in_parens() {
        let mut ast = AST::new();
        let group = AST::new_group(vec![
            ast.new_number(Numeric::I32(1), vec![]),
            ast.new_number(Numeric::I32(2), vec![]),
            ast.new_number(Numeric::I32(3), vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast("(1)( 2, 3 )  ;", ast);
        assert_eq!(parser.parse_expr(), group);
    }

    #[test]
    fn test_parse_number_groups_in_nested_blocks() {
        let mut ast = AST::new();
        let group = AST::new_group(vec![
            ast.new_number(Numeric::I32(1), vec![]),
            ast.new_number(Numeric::I32(2), vec![]),
            ast.new_number(Numeric::I32(3), vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast("{ (1 (2, (3))) }", ast);
        assert_eq!(parser.parse_expr(), group);
    }

    #[test]
    fn test_parse_let_expr_single() {
        let mut ast = AST::new();
        let one = ast.new_number(Numeric::I32(23), vec![]);
        let let_expr = AST::new_let(
            ast.new_assignment("x", None, one), vec![]);
        let group = AST::new_group(vec![
            let_expr,
            ast.new_local("x", I32, vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast("let x = 23, x", ast);
        assert_eq!(parser.parse_expr(), group);
    }

    #[test]
    fn test_parse_let_expr_curly_delimited() {
        let mut ast = AST::new();
        let one = ast.new_number(Numeric::I32(23), vec![]);
        let let_expr = AST::new_let(
            ast.new_assignment("x", None, one), vec![]);
        let group = AST::new_group(vec![
            let_expr,
            ast.new_local("x", I32, vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast(" { let x =  23, x } ", ast);
        assert_eq!(parser.parse_expr(), group);
    }

    #[test]
    fn test_parse_let_expr_multi_values() {
        let mut ast = AST::new();
        let expr = AST::new_group(vec![
            ast.new_number(Numeric::I32(2), vec![]),
            ast.new_number(Numeric::I32(3), vec![]),
        ], vec![]);
        let let_expr = AST::new_let(
            ast.new_assignments(vec![
                ("x".to_owned(), None),
                ("y".to_owned(), None),
            ], expr), vec![]);
        let mut parser = Parser::new_with_ast("let x, y = 2, 3;", ast);
        assert_eq!(parser.parse_expr(), let_expr);

        let scope = parser.scope.pop();
        assert!(scope.is_some());
        let scope = scope.unwrap();
        assert_eq!(scope.get(&parser.ast.intern("x")), Some(&I32));
        assert_eq!(scope.get(&parser.ast.intern("y")), Some(&I32));
        assert_eq!(scope.get(&parser.ast.intern("z")), None);
    }
}
