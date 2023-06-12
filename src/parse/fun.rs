use crate::ast::{Expression, TopLevelElement, Type, Visibility, Warning};
use crate::ast::Type::FunType;
use crate::errors::WasminError;
use crate::parse::model::{Position, Token};
use crate::parse::parse::Parser;
use crate::parse::scope::ScopeItem;

impl<'s> Parser<'s> {
    pub(crate) fn parse_fun(&mut self, pos: Position, visibility: Visibility) -> TopLevelElement {
        let mut name_and_args: Vec<String> = Vec::with_capacity(4);
        while let Some(token) = self.lexer.next() {
            match token {
                Token::Id(_, id) => name_and_args.push(id),
                Token::Colon(_) => {
                    return name_with_args(name_and_args, pos)
                        .map(|(name, args)| self.fun_with_type(pos, name, args, visibility))
                        .into();
                }
                Token::Eq(_) => {
                    return name_with_args(name_and_args, pos).map(|(name, args)| {
                        self.enter_scope();
                        let expr = self.parse_expr();
                        self.exit_scope();
                        self.new_fun(name, args, pos, visibility, None, expr)
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
            Ok(FunType(expr_type)) => expr_type,
            Ok(typ) => return TopLevelElement::Error(WasminError::TypeError {
                pos,
                cause: format!("function '{}' declared with non-function type: {:?}", name, typ),
            }),
            Err(err) => return TopLevelElement::Error(err),
        };
        if let Some(token) = self.lexer.next() {
            match token {
                Token::Eq(_) => {
                    self.enter_scope();
                    for (t, name) in typ.ins.iter().zip(args.iter()) {
                        let interned_name = self.ast.intern(name);
                        self.current_scope_mut().insert(interned_name, ScopeItem::Variable(t.clone()));
                    }
                    let expr = self.parse_expr();
                    self.exit_scope();
                    self.new_fun(name, args, pos, visibility, Some(FunType(typ)), expr)
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
               pos: Position, visibility: Visibility,
               typ: Option<Type>, body: Expression)
               -> TopLevelElement {
        let mut warnings: Vec<Warning> = Vec::new();
        let target_type = if let Some(target) = typ {
            match target {
                FunType(e) => Some(e),
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
        let typ = target_type.unwrap_or_else(|| body.get_type().clone());
        let interned_name = self.ast.intern(&name);
        match self.add_fun_type_to_scope(&interned_name, args, body, pos, typ) {
            Ok(fun) => TopLevelElement::Fun(fun, pos, visibility, None, warnings),
            Err(err) => TopLevelElement::Error(err)
        }
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
    use crate::ast::{AST, Constant, ExprType, Function};
    use crate::ast::Visibility::{Private, Public};
    use crate::parse::model::Numeric;
    use crate::parse::scope::ScopeItem;

    use super::*;
    use super::Type::{F32, F64, I32, I64};

    #[test]
    fn test_parse_fun() {
        let ast = AST::new();
        let body = Expression::Const(Constant::Number(Numeric::I32(1)), I32, ExprType::outs(vec![I32]), vec![]);
        let mut parser = Parser::new_with_ast("fun x = 1", ast);
        let result = parser.parse_next();
        assert_function(parser, "x", vec![], ExprType::outs(vec![I32]), body, 1, Private, result);
    }

    #[test]
    fn test_parse_pub_fun() {
        let ast = AST::new();
        let body = Expression::Const(Constant::Number(Numeric::I32(42)), I32, ExprType::outs(vec![I32]), vec![]);
        let mut parser = Parser::new_with_ast("  pub fun abc = 42", ast);
        let result = parser.parse_next();
        assert_function(parser, "abc", vec![], ExprType::outs(vec![I32]), body, 7, Public, result);
    }

    #[test]
    fn test_parse_fun_1_1() {
        let ast = AST::new();
        let body = Expression::Const(Constant::Number(Numeric::I64(10)), I64, ExprType::outs(vec![I64]), vec![]);
        // FIXME the function takes the type of its implementation expression regardless of arguments
        let mut parser = Parser::new_with_ast("fun example a\n   =  10i64", ast);
        let result = parser.parse_next();
        assert_function(parser, "example", vec!["a".to_owned()], ExprType::outs(vec![I64]), body, 1, Private, result);
    }

    #[test]
    fn test_parse_fun_typed_0_1() {
        let ast = AST::new();
        let body = Expression::Const(Constant::Number(Numeric::I32(1)), I32, ExprType::outs(vec![I32]), vec![]);
        let mut parser = Parser::new_with_ast("fun x: [](i32) = 1", ast);
        let result = parser.parse_next();
        assert_function(parser, "x", vec![], ExprType::outs(vec![I32]), body, 1, Private, result);
    }

    #[test]
    fn test_parse_fun_typed_1_1() {
        let ast = AST::new();
        let body = Expression::Const(Constant::Number(Numeric::I64(10)), I64, ExprType::outs(vec![I64]), vec![]);
        let mut parser = Parser::new_with_ast("fun factorial n: [i32](i64) = 10i64", ast);
        let result = parser.parse_next();
        assert_function(parser, "factorial", vec!["n".to_owned()],
                        ExprType::new(vec![I32], vec![I64]), body, 1, Private, result);
    }

    #[test]
    fn test_parse_fun_typed_2_3() {
        let ast = AST::new();
        let body = Expression::Const(Constant::Number(Numeric::I64(10)), I64, ExprType::outs(vec![I64]), vec![]);
        // FIXME no type checking is done yet
        let mut parser = Parser::new_with_ast("pub fun foo abc def: [ i32 i64 ] (f32 f64 f32) = 10i64", ast);
        let result = parser.parse_next();
        assert_function(parser, "foo", vec!["abc".to_owned(), "def".to_owned()],
                        ExprType::new(vec![I32, I64], vec![F32, F64, F32]), body, 5, Public, result);
    }

    #[test]
    fn test_parse_fun_typed_2_2_curly_delimited() {
        let mut ast = AST::new();
        let body = AST::new_group(vec![
            ast.new_number(Numeric::F32(1.0), vec![]),
            ast.new_number(Numeric::F64(2.0), vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast(
            "fun foo abc def: [ i32 i64 ] (f32 f64) = {1.0, 2.0f64} , ignored", ast);
        let result = parser.parse_next();
        assert_function(parser, "foo", vec!["abc".to_owned(), "def".to_owned()],
                        ExprType::new(vec![I32, I64], vec![F32, F64]), body, 1, Private, result);
    }

    #[test]
    fn test_parse_fun_typed_2_2_using_args() {
        let mut ast = AST::new();
        let body = AST::new_group(vec![
            ast.new_local("b", I64, vec![]),
            ast.new_local("a", I32, vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast(
            "fun swap a b: [ i32 i64 ] (i64 i32) = b, a;", ast);
        let result = parser.parse_next();
        assert_function(parser, "swap", vec!["a".to_owned(), "b".to_owned()],
                        ExprType::new(vec![I32, I64], vec![I64, I32]), body, 1, Private, result);
    }

    fn assert_function(mut parser: Parser<'_>, name: &str, arg_names: Vec<String>,
                       typ: ExprType, body: Expression, pos: Position, visibility: Visibility,
                       result: Option<TopLevelElement>) {
        let interned_name = parser.ast.intern(name);
        let scope_item = match parser.lookup_in_scope(&interned_name) {
            None => panic!("did not find function in scope"),
            Some(ScopeItem::Fun(items)) => {
                if items.len() == 1 {
                    items.first().unwrap()
                } else {
                    panic!("expected one function but found: {:?}", items);
                }
            }
            Some(ScopeItem::Variable(t)) => panic!("expected a fun, got a var of type {:?}", t)
        };
        assert_eq!(scope_item.pos, pos);
        assert_eq!(&scope_item.typ, &typ);
        let fun_index = scope_item.fun_index;
        assert_eq!(result, Some(TopLevelElement::Fun(
            Function {
                name: interned_name.clone(),
                arg_names: arg_names.iter().map(|a| parser.ast.intern(a)).collect(),
                body,
                typ,
                fun_index,
            },
            pos, visibility, None, vec![])));
    }
}
