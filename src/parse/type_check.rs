use std::collections::{HashMap, HashSet};
use std::fmt::{Display, format, Formatter};
use std::ops::Deref;

use crate::ast::{AST, Expression, ExprType, FunKind, IdKind, Type};
use crate::ast::Expression::ExprError;
use crate::errors::WasminError;
use crate::interner::InternedStr;
use crate::parse::model::{Position, Token};
use crate::parse::parse::Parser;

impl<'s> Parser<'s> {
    pub(crate) fn find_closest_type_match(
        &self, types: &Vec<(&ExprType, FunKind)>, args: &Expression,
    ) -> Option<(ExprType, FunKind)> {
        let provided_type = args.get_type();
        let provided_args_count = provided_type.outs.len();
        let max_args = provided_args_count + self.stack.len();
        for (typ, kind) in types {
            if typ.ins.len() == 0 {
                return if provided_args_count == 0 {
                    Some((typ.deref().clone(), kind.clone()))
                } else {
                    None
                };
            }
            if typ.ins.len() > max_args { continue; } // cannot match
            if typ.ins.len() < provided_type.outs.len() { continue; } // cannot match
            // check types from the last to the first
            for (arg, param) in provided_type.ins.iter().rev().zip(typ.ins.iter().rev()) {
                if *arg != *param { continue; } // no match
            }
            // if the function needs more args, check them on the stack
            for (param, arg) in typ.ins.iter().rev().skip(provided_args_count)
                .zip(self.stack.iter().rev()) {
                if *arg != *param { continue; } // no match
            }
            // SUCCESS!!
            return Some((typ.deref().clone(), kind.clone()));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Constant, ExprType};
    use crate::ast::Expression::Const;
    use crate::ast::Visibility::{Private, Public};
    use crate::interner::InternedStr;
    use crate::parse::model::Numeric;

    use super::*;
    use super::Type::*;

    #[test]
    fn test_fun_no_args() {
        let mut ast = AST::new();
        let typ = ExprType::ins(vec![]);
        let fun_types = vec![
            (&typ, FunKind::Custom { fun_index: 0 })
        ];
        let args = Expression::Empty(vec![]);
        let mut parser = Parser::new_with_ast("", ast);
        assert_eq!(parser.find_closest_type_match(&fun_types, &args),
                   Some((typ.clone(), FunKind::Custom { fun_index: 0 })));
    }

    #[test]
    fn test_fun_1_explicit_arg() {
        let mut ast = AST::new();
        let typ = ExprType::ins(vec![I32]);
        let fun_types = vec![
            (&typ, FunKind::Wasm)
        ];
        let args = ast.new_number(Numeric::I32(1), vec![]);
        let mut parser = Parser::new_with_ast("", ast);
        assert_eq!(parser.find_closest_type_match(&fun_types, &args),
                   Some((typ.clone(), FunKind::Wasm)));
    }

    #[test]
    fn test_fun_2_explicit_args() {
        let mut ast = AST::new();
        let typ = ExprType::ins(vec![I32, F64]);
        let fun_types = vec![
            (&typ, FunKind::Wasm),
        ];
        let args = AST::new_group(vec![
            ast.new_number(Numeric::I32(1), vec![]),
            ast.new_number(Numeric::F64(1.0), vec![]),
        ], vec![]);
        let mut parser = Parser::new_with_ast("", ast);
        assert_eq!(parser.find_closest_type_match(&fun_types, &args),
                   Some((typ.clone(), FunKind::Wasm)));
    }

    #[test]
    fn test_fun_1_explicit_arg_1_from_stack() {
        let mut ast = AST::new();
        let typ = ExprType::ins(vec![I32, F64]);
        let fun_types = vec![
            (&typ, FunKind::Custom { fun_index: 1 }),
        ];
        let args = ast.new_number(Numeric::F64(1.0), vec![]);
        let mut parser = Parser::new_with_ast("", ast);
        parser.stack.push(I32);
        assert_eq!(parser.find_closest_type_match(&fun_types, &args),
                   Some((typ.clone(), FunKind::Custom { fun_index: 1 })));
    }

    #[test]
    fn test_fun_2_args_from_stack() {
        let mut ast = AST::new();
        let typ = ExprType::ins(vec![I32, F64]);
        let fun_types = vec![
            (&typ, FunKind::Custom { fun_index: 2 }),
        ];
        let mut parser = Parser::new_with_ast("", ast);
        parser.stack.push(I32);
        parser.stack.push(F64);
        assert_eq!(parser.find_closest_type_match(&fun_types, &AST::empty()),
                   Some((typ.clone(), FunKind::Custom { fun_index: 2 })));
    }
}
