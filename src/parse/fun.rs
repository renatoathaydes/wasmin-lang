use std::fmt::format;
use std::ops::Index;

use crate::ast::{AST, Expression, Function, TopLevelElement, Type, Visibility, Warning};
use crate::errors::WasminError;
use crate::parse::model::{Position, Token};
use crate::parse::parse::Parser;

impl<'s> Parser<'s> {
    pub(crate) fn parse_fun(&mut self, pos: Position, visibility: Visibility) -> TopLevelElement {
        let mut name_and_args: Vec<String> = Vec::with_capacity(4);
        while let Some(token) = self.lexer.next() {
            match token {
                Token::Id(_, id) => name_and_args.push(id),
                Token::Colon(pos) => {
                    return name_with_args(name_and_args, pos)
                        .map(|(name, args)| self.fun_with_type(pos, name, args, visibility))
                        .into();
                }
                Token::Eq(pos) => {
                    return name_with_args(name_and_args, pos).map(|(name, args)| {
                        let expr = self.parse_expr();
                        self.new_fun(name, args, visibility, None, expr)
                    }).into();
                }
                Token::Error(pos, err) => {
                    return TopLevelElement::Error(WasminError::SyntaxError {
                        pos,
                        cause: err,
                    });
                }
                _ => {
                    return TopLevelElement::Error(WasminError::SyntaxError {
                        pos: token.pos(),
                        cause: format!("malformed function declaration: expected name and arguments \
                            but got {}", token),
                    });
                }
            }
        }
        TopLevelElement::Error(WasminError::SyntaxError {
            pos,
            cause: format!("incomplete function declaration: missing {}",
                           if name_and_args.is_empty() { "name" } else { "function definition" }),
        })
    }

    fn fun_with_type(&mut self, pos: Position, name: String, args: Vec<String>,
                     visibility: Visibility) -> TopLevelElement {
        let typ = match self.parse_type(pos) {
            Ok(fun_type) => Some(fun_type),
            Err(err) => return TopLevelElement::Error(err),
        };
        if let Some(token) = self.lexer.next() {
            match token {
                Token::Eq(_) => {
                    let expr = self.parse_expr();
                    self.new_fun(name, args, visibility, typ, expr)
                }
                _ => {
                    TopLevelElement::Error(WasminError::SyntaxError {
                        pos,
                        cause: format!("malformed function declaration: expected '=' \
                            following function type but got {}", token),
                    })
                }
            }
        } else {
            TopLevelElement::Error(WasminError::SyntaxError {
                pos,
                cause: "incomplete function declaration: missing '=' after function type".into(),
            })
        }
    }

    fn new_fun(&mut self, name: String, args: Vec<String>,
               visibility: Visibility,
               typ: Option<Type>, body: Expression)
               -> TopLevelElement {
        let mut warnings: Vec<Warning> = Vec::new();
        let target_type = if let Some(target) = typ {
            match target {
                Type::Fn(e) => Some(e),
                Type::Error(err) => {
                    warnings.push(format!("error in function type declaration: {}", err.cause()));
                    None
                }
                _ => {
                    warnings.push(
                        format!("attempt to assign non-functional type '{}' to function",
                                self.ast.type_to_string(&target)));
                    None
                }
            }
        } else { None };
        let target_type = target_type.unwrap_or_else(|| body.get_type().clone());
        let fun = self.ast.new_fun(&name, args, body, target_type);
        TopLevelElement::Fun(fun, visibility, None, warnings)
    }
}

fn name_with_args(mut name_and_args: Vec<String>, pos: Position)
                  -> Result<(String, Vec<String>), WasminError> {
    if name_and_args.is_empty() {
        Err(WasminError::SyntaxError {
            pos,
            cause: "missing function name".into(),
        })
    } else {
        let name = name_and_args.remove(0);
        Ok((name, name_and_args))
    }
}

impl Into<TopLevelElement> for Result<TopLevelElement, WasminError> {
    fn into(self) -> TopLevelElement {
        match self {
            Ok(element) => element,
            Err(err) => TopLevelElement::Error(err),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Constant, ExprType};
    use crate::ast::Visibility::{Private, Public};
    use crate::parse::model::Numeric;

    use super::*;
    use super::Type::*;

    #[test]
    fn test_parse_fun() {
        let mut ast = AST::new();
        let body = Expression::Const(Constant::Number(Numeric::I32(1)), I32, ExprType::outs(vec![I32]), vec![]);
        let fun = ast.new_fun("x", vec![], body, ExprType::outs(vec![I32]));
        let mut parser = Parser::new_with_ast("fun x = 1", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Fun(fun, Private, None, vec![])));
    }

    #[test]
    fn test_parse_pub_fun() {
        let mut ast = AST::new();
        let body = Expression::Const(Constant::Number(Numeric::I32(42)), I32, ExprType::outs(vec![I32]), vec![]);
        let fun = ast.new_fun("abc", vec![], body, ExprType::outs(vec![I32]));
        let mut parser = Parser::new_with_ast("  pub fun abc = 42", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Fun(fun, Public, None, vec![])));
    }

    #[test]
    fn test_parse_fun_1_1() {
        let mut ast = AST::new();
        let expr = ast.new_number(Numeric::I32(1), vec![]);
        let body = Expression::Const(Constant::Number(Numeric::I64(10)), I64, ExprType::outs(vec![I64]), vec![]);
        // FIXME the function takes the type of its implementation expression regardless of arguments
        let fun = ast.new_fun("example", vec!["a".into()], body, ExprType::new(vec![], vec![I64]));
        let mut parser = Parser::new_with_ast("fun example a\n   =  10i64", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Fun(fun, Private, None, vec![])));
    }

    #[test]
    fn test_parse_fun_typed_0_1() {
        let mut ast = AST::new();
        let expr = ast.new_number(Numeric::I32(1), vec![]);
        let body = Expression::Const(Constant::Number(Numeric::I32(1)), I32, ExprType::outs(vec![I32]), vec![]);
        let fun = ast.new_fun("x", vec![], body, ExprType::outs(vec![I32]));
        let mut parser = Parser::new_with_ast("fun x: [](i32) = 1", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Fun(fun, Private, None, vec![])));
    }

    #[test]
    fn test_parse_fun_typed_1_1() {
        let mut ast = AST::new();
        let expr = ast.new_number(Numeric::I32(1), vec![]);
        let body = Expression::Const(Constant::Number(Numeric::I64(10)), I64, ExprType::outs(vec![I64]), vec![]);
        let fun = ast.new_fun("factorial", vec!["n".into()], body, ExprType::new(vec![I32], vec![I64]));
        let mut parser = Parser::new_with_ast("fun factorial n: [i32](i64) = 10i64", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Fun(fun, Private, None, vec![])));
    }

    #[test]
    fn test_parse_fun_typed_2_3() {
        let mut ast = AST::new();
        let expr = ast.new_number(Numeric::I32(1), vec![]);
        let body = Expression::Const(Constant::Number(Numeric::I64(10)), I64, ExprType::outs(vec![I64]), vec![]);
        let fun = ast.new_fun("foo", vec!["abc".into(), "def".into()], body,
                              ExprType::new(vec![I32, I64], vec![F32, F64, F32]));
        // FIXME no type checking is done yet
        let mut parser = Parser::new_with_ast("fun foo abc def: [ i32 i64 ] (f32 f64 f32) = 10i64", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Fun(fun, Private, None, vec![])));
    }

    #[test]
    fn test_parse_fun_typed_2_2_curly_delimited() {
        let mut ast = AST::new();
        let body = AST::new_group(vec![
            ast.new_number(Numeric::F32(1.0), vec![]),
            ast.new_number(Numeric::F64(2.0), vec![]),
        ], vec![]);
        let fun = ast.new_fun("foo", vec!["abc".into(), "def".into()], body,
                              ExprType::new(vec![I32, I64], vec![F32, F64]));
        let mut parser = Parser::new_with_ast(
            "fun foo abc def: [ i32 i64 ] (f32 f64) = {1.0, 2.0f64} , ignored", ast);
        assert_eq!(parser.parse_next(), Some(TopLevelElement::Fun(fun, Private, None, vec![])));
    }
}
