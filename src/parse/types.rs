use crate::ast::{ExprType, Type};
use crate::errors::WasminError;
use crate::parse::model::{Position, Token, Token::*};
use crate::parse::parse::Parser;

pub(crate) fn parse_type(parser: &mut Parser<'_>, pos: Position) -> Result<Type, WasminError> {
    if let Some(token) = parser.lexer.next() {
        match token {
            Id(_, id) => Ok(parser.ast.new_type(&id)),
            OpenBracket(pos) => {
                let (ins, pos) = parse_types_until(']', parser, pos)?;
                parse_return_types(parser, ins, pos)
            }
            _ => Err(WasminError::SyntaxError {
                pos,
                cause: format!("expected a type definition, found {}", token),
            }),
        }
    } else {
        Err(WasminError::SyntaxError {
            pos,
            cause: "expected a type definition but found nothing".into(),
        })
    }
}

fn parse_return_types(parser: &mut Parser, ins: Vec<Type>, pos: Position)
                      -> Result<Type, WasminError> {
    if let Some(token) = parser.lexer.next() {
        match token {
            OpenParens(pos) => {
                let (outs, pos) = parse_types_until(')', parser, pos)?;
                Ok(Type::Fn(ExprType::new(ins, outs)))
            }
            _ => Err(WasminError::SyntaxError {
                pos,
                cause: format!("expected function return types declaration \
                    starting with '(', found {}", token),
            }),
        }
    } else {
        Err(WasminError::SyntaxError {
            pos,
            cause: "expected return types of function type but found nothing".into(),
        })
    }
}

fn parse_types_until(end: char, parser: &mut Parser, pos: Position)
                     -> Result<(Vec<Type>, Position), WasminError> {
    let mut types = Vec::with_capacity(2);
    loop {
        if let Some(token) = parser.lexer.next() {
            if token.is_char(end) {
                return Ok((types, token.pos()));
            }
            match token {
                Id(_, id) => types.push(parser.ast.new_type(&id)),
                _ => return Err(WasminError::SyntaxError {
                    pos,
                    cause: format!("expected a type definition, found {}", token),
                }),
            }
        }
    }
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::ast::{AST, ExprType};
    use crate::interner::InternedStr;
    use crate::parse::model::Numeric;

    use super::*;
    use super::Type::*;

    #[test]
    fn test_parse_i32() -> Result<(), WasminError> {
        let mut ast = AST::new();
        let mut parser = Parser::new_with_ast("i32", ast);

        assert_eq!(parse_type(&mut parser, 0)?, I32);
        Ok(())
    }

    fn new_interned_type(ast: &mut AST, name: &str) -> InternedStr {
        let typ = ast.new_type(name);
        let typ_str = match typ {
            Custom(name) => Some(name),
            _ => None,
        };
        typ_str.unwrap()
    }

    #[test]
    fn test_parse_custom_type() -> Result<(), WasminError> {
        let mut ast = AST::new();
        let hello_str = new_interned_type(&mut ast, "Hello");
        let mut parser = Parser::new_with_ast("Hello", ast);

        assert_eq!(parse_type(&mut parser, 0)?, Custom(hello_str));
        Ok(())
    }

    #[test]
    fn test_parse_fn_1_0() -> Result<(), WasminError> {
        let mut ast = AST::new();
        let mut parser = Parser::new_with_ast("[i32]()", ast);

        assert_eq!(parse_type(&mut parser, 0)?, Fn(ExprType::new(vec![I32], vec![])));
        Ok(())
    }

    #[test]
    fn test_parse_fn_1_1() -> Result<(), WasminError> {
        let mut ast = AST::new();
        let mut parser = Parser::new_with_ast("[i64](f32)", ast);

        assert_eq!(parse_type(&mut parser, 0)?, Fn(ExprType::new(vec![I64], vec![F32])));
        Ok(())
    }

    #[test]
    fn test_parse_fn_0_1() -> Result<(), WasminError> {
        let mut ast = AST::new();
        let mut parser = Parser::new_with_ast("[](f64)", ast);

        assert_eq!(parse_type(&mut parser, 0)?, Fn(ExprType::outs(vec![F64])));
        Ok(())
    }

    #[test]
    fn test_parse_fn_2_3() -> Result<(), WasminError> {
        let mut ast = AST::new();
        let mut parser = Parser::new_with_ast("[i32 i64](f32 f64 f32)", ast);

        assert_eq!(parse_type(&mut parser, 0)?,
                   Fn(ExprType::new(vec![I32, I64], vec![F32, F64, F32])));
        Ok(())
    }

    #[test]
    fn test_parse_fn_2_1_custom_types() -> Result<(), WasminError> {
        let mut ast = AST::new();
        let int_str = new_interned_type(&mut ast, "Int");
        let float_str = new_interned_type(&mut ast, "Float");
        let double_str = new_interned_type(&mut ast, "Double");

        let mut parser = Parser::new_with_ast("  [ Int Float ] \n( Double ) ", ast);

        assert_eq!(parse_type(&mut parser, 0)?,
                   Fn(ExprType::new(vec![Custom(int_str), Custom(float_str)], vec![Custom(double_str)])));
        Ok(())
    }
}
