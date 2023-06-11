use crate::ast::{AST, Expression, IdKind};
use crate::errors::WasminError;
use crate::interner::InternedStr;
use crate::parse::model::{Position, Token};
use crate::parse::parse::Parser;

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum Nesting {
    Parens,
    Curly,
    If,
    Then,
}

impl Nesting {
    fn close_symbol(&self) -> &'static str {
        match self {
            Nesting::Parens => ")",
            Nesting::Curly => "}",
            Nesting::If => "then",
            Nesting::Then => "else",
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum State {
    /// no special case.
    Any,
    /// parsing a single-group expression
    Single,
    /// parsing a function call
    FunCall(Position, InternedStr),
}

impl<'s> Parser<'s> {
    pub(crate) fn parse_expr(&mut self) -> Expression {
        let mut nesting = Vec::new();
        self.parse_expr_nesting(&mut nesting, State::Any)
    }

    pub(crate) fn parse_expr_nesting(&mut self, nesting: &mut Vec<Nesting>, state: State) -> Expression {
        let mut expressions = Vec::new();
        let mut terminate = false;
        while let Some(token) = self.lexer.next() {
            if terminate { break; }
            let expr = match token {
                Token::Str(.., string) => self.ast.new_string(&string, vec![]),
                Token::Id(pos, name) => {
                    let interned_name = self.ast.intern(&name);
                    match self.lookup_id_kind(&name, &interned_name, pos) {
                        Ok(IdKind::Fun) => self.parse_expr_nesting(nesting, State::FunCall(pos, interned_name)),
                        Ok(IdKind::Var(typ)) => {
                            self.stack.push(typ.clone());
                            self.ast.new_local(&name, typ, vec![])
                        }
                        Err(err) => err.into(),
                    }
                }
                Token::Number(.., value) => {
                    self.stack.push(value.get_type());
                    self.ast.new_number(value, vec![])
                }
                Token::Let(pos) => self.parse_let_expr(pos),
                Token::Error(pos, err) => AST::new_error(WasminError::SyntaxError {
                    cause: err,
                    pos,
                }, vec![]),
                Token::OpenParens(_) => {
                    nesting.push(Nesting::Parens);
                    self.enter_scope();
                    self.parse_expr_nesting(nesting, State::Any)
                }
                Token::CloseParens(pos) => {
                    self.exit_scope();
                    close_expr_with(Nesting::Parens, nesting.pop(), pos, &mut expressions);
                    break;
                }
                Token::OpenCurly(_) => {
                    nesting.push(Nesting::Curly);
                    // curly braces end an expression eagerly
                    terminate = true;
                    self.enter_scope();
                    self.parse_expr_nesting(nesting, State::Any)
                }
                Token::CloseCurly(pos) => {
                    self.exit_scope();
                    close_expr_with(Nesting::Curly, nesting.pop(), pos, &mut expressions);
                    break;
                }
                Token::If(..) => self.parse_if(),
                Token::Then(pos) | Token::Else(pos) => {
                    AST::new_error(WasminError::UnsupportedFeatureError {
                        cause: format!("'{}' unexpected at this position", token),
                        pos,
                    }, vec![])
                }
                Token::Comma(..) => if let State::FunCall(..) = state { break; } else { continue; },
                Token::SemiColon(..) => break,
                _ => AST::new_error(WasminError::UnsupportedFeatureError {
                    cause: format!("token not supported in expressions yet: {}", token),
                    pos: token.pos(),
                }, vec![]),
            };
            push_flattened_into(&mut expressions, expr);
            if state == State::Single { break; }
        }
        let result = collapse_expressions(expressions);
        if let State::FunCall(pos, name) = state {
            self.create_fun_call(name, pos, result)
        } else {
            result
        }
    }

    fn parse_let_expr(&mut self, pos: Position) -> Expression {
        match self.parse_defs(pos) {
            Ok(vars) => {
                let mut nesting = Vec::new();
                let expressions: Vec<_> = (0..vars.len()).map(|_| {
                    self.parse_expr_nesting(&mut nesting, State::Single)
                }).collect();
                let assignment = self.ast.new_assignments(vars, collapse_expressions(expressions));
                let mut var_types = assignment.get_types();
                for (var, typ) in var_types.drain(..) {
                    let _ = self.stack.pop();
                    self.insert_type_in_scope(var.name.clone(), typ);
                }
                Expression::Let(assignment, vec![])
            }
            Err(err) => {
                Expression::ExprError(err, vec![])
            }
        }
    }

    fn parse_if(&mut self) -> Expression {
        match self.parse_if_expr() {
            Ok(if_expr) => if_expr,
            Err(err) => err.into(),
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expression, WasminError> {
        // TODO typecheck
        let mut nesting = Vec::new();
        let cond = self.parse_expr_nesting(&mut nesting, State::Single);
        let _ = self.stack.pop();
        self.next_token_must_be(is_then, "then")?;
        self.enter_scope();
        let yes = self.parse_expr_nesting(&mut nesting, State::Single);
        self.exit_scope();
        self.next_token_must_be(is_else, "else")?;
        self.enter_scope();
        let no = self.parse_expr_nesting(&mut nesting, State::Single);
        let _ = self.stack.pop();
        self.enter_scope();
        Ok(AST::new_if(cond, yes, no, vec![]))
    }

    fn next_token_must_be<F>(&mut self, expected: F, expected_name: &str) -> Result<(), WasminError>
        where F: FnOnce(&Token) -> bool {
        if let Some(token) = self.lexer.next() {
            let ok = expected(&token);
            if ok {
                return Ok(());
            }
            Err(WasminError::UnsupportedFeatureError {
                cause: format!("expected '{}' at this position but got {}", expected_name, token),
                pos: token.pos(),
            })
        } else {
            Err(WasminError::UnsupportedFeatureError {
                cause: format!("expected '{}' at this position but got nothing", expected_name),
                pos: self.lexer.pos(),
            })
        }
    }

    fn create_fun_call(&mut self, interned_name: InternedStr, pos: Position, args: Expression) -> Expression {
        let name = self.ast.interned_str(&interned_name);
        if let Some(mut types) = self.lookup_fun_types(name, &interned_name) {
            // the type with the longest list of arguments should win
            types.sort_by(|(a, _), (b, _)| b.ins.len().cmp(&a.ins.len()));
            if let Some((best_type, kind)) = self.find_closest_type_match(&types, &args) {
                let call = self.ast.new_fun_call(interned_name, best_type, kind, vec![]);
                AST::new_group(vec![args, call], vec![])
            } else {
                Expression::ExprError(WasminError::TypeError {
                    cause: format!("function {} cannot be called with the arguments provided",
                                   name),
                    pos,
                }, vec![])
            }
        } else {
            Expression::ExprError(WasminError::TypeError {
                cause: format!("function {} does not exist", name),
                pos,
            }, vec![])
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

fn is_then(token: &Token) -> bool {
    match token {
        Token::Then(..) => true,
        _ => false,
    }
}

fn is_else(token: &Token) -> bool {
    match token {
        Token::Else(..) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use std::hash::Hash;

    use crate::ast::{Constant, ExprType, FunKind};
    use crate::ast::Expression::Const;
    use crate::ast::Type::*;
    use crate::ast::Visibility::{Private, Public};
    use crate::interner::InternedStr;
    use crate::parse::model::Numeric;
    use crate::parse::scope::ScopeItem;

    use super::*;

    #[test]
    fn test_parse_numbers() {
        let mut ast = AST::new();
        let group = AST::new_group(vec![
            ast.new_number(Numeric::I32(1), vec![]),
            ast.new_number(Numeric::I64(2), vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast("1, 2i64;", ast);
        assert_eq!(parser.parse_expr(), group);
        assert_eq!(parser.stack, vec![I32, I64]);
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
        assert_eq!(parser.stack, vec![I32, I64]);
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
        assert_eq!(parser.stack, vec![I32, I32, I32]);
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
        assert_eq!(parser.stack, vec![I32, I32, I32]);
    }

    #[test]
    fn test_parse_let_expr_single() {
        let mut ast = AST::new();
        let num = ast.new_number(Numeric::I32(23), vec![]);
        let let_expr = AST::new_let(
            ast.new_assignment("x", None, num), vec![]);
        let group = AST::new_group(vec![
            let_expr,
            ast.new_local("x", I32, vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast("let x = 23, x", ast);
        assert_eq!(parser.parse_expr(), group);
        assert_eq!(parser.stack, vec![I32]);
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
        assert_eq!(parser.stack, vec![I32]);
    }

    #[test]
    fn test_parse_let_expr_multi_values() {
        let mut ast = AST::new();
        let expr = AST::new_group(vec![
            ast.new_number(Numeric::I32(2), vec![]),
            ast.new_number(Numeric::F32(3.14), vec![]),
        ], vec![]);
        let let_expr = AST::new_let(
            ast.new_assignments(vec![
                ("x".to_owned(), None),
                ("y".to_owned(), None),
            ], expr), vec![]);
        let mut parser = Parser::new_with_ast("let x, y = 2, 3.14;", ast);
        assert_eq!(parser.parse_expr(), let_expr);
        assert_eq!(parser.stack, vec![]);

        let scope = parser.current_scope().clone();
        assert_eq!(scope.get(&parser.ast.intern("x")), Some(&ScopeItem::Variable(I32)));
        assert_eq!(scope.get(&parser.ast.intern("y")), Some(&ScopeItem::Variable(F32)));
        assert_eq!(scope.get(&parser.ast.intern("z")), None);
    }

    #[test]
    fn test_parse_basic_if_expr() {
        let mut ast = AST::new();
        let if_expr = AST::new_if(ast.new_number(Numeric::I32(1), vec![]),
                                  ast.new_number(Numeric::I32(2), vec![]),
                                  ast.new_number(Numeric::I32(3), vec![]),
                                  vec![]);
        let mut parser = Parser::new_with_ast("if 1 then 2 else 3;", ast);
        assert_eq!(parser.parse_expr(), if_expr);
        assert_eq!(parser.stack, vec![I32]);
    }

    #[test]
    fn test_parse_basic_fun_call() -> Result<(), WasminError> {
        let mut ast = AST::new();
        let my_fun = ast.intern("my-fun");
        let fun_call = ast.new_fun_call(my_fun,
                                        ExprType::new(vec![I32], vec![I32]),
                                        FunKind::Custom { fun_index: 0 },
                                        vec![]);
        let fun_call_group = AST::new_group(vec![
            ast.new_number(Numeric::I32(1), vec![]),
            fun_call,
        ], vec![]);
        let mut parser = Parser::new_with_ast("my-fun 1;", ast);
        let _ = parser.add_fun_type_to_scope(&my_fun, vec!["a".to_owned()], AST::empty(), 1,
                                             ExprType::new(vec![I32], vec![I32]))?;
        assert_eq!(parser.parse_expr(), fun_call_group);
        assert_eq!(parser.stack, vec![I32]);
        // nothing is left
        assert_eq!(parser.parse_expr(), AST::empty());
        assert_eq!(parser.parse_expr(), AST::empty());
        Ok(())
    }
}
