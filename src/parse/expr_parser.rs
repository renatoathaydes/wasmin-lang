use std::str::Chars;

use crate::ast::{Assignment, Expression, ReAssignment};
use crate::ast::Expression::{ExprError, Let, Mut, Set};
use crate::parse::parser::{GroupingState, GroupingSymbol, Parser, Stack};
use crate::parse::parser::GroupingSymbol::Parens;
use crate::types::{*};

#[derive(Debug)]
enum FirstItemInExpr {
    Expr(Expression),
    Str(String),
}

pub fn parse_expr(parser: &mut Parser) -> Expression {
    parse_expr_start(parser, false)
}

fn parse_expr_start(parser: &mut Parser, has_parent: bool) -> Expression {
    let mut state = GroupingState::new();
    let expr = parse_expr_with_state(parser, &mut state, has_parent);
    if state.is_empty() {
        expr
    } else {
        Expression::ExprError(parser.error("Unclosed grouping in expression"))
    }
}

fn parse_expr_with_state(
    parser: &mut Parser,
    state: &mut GroupingState,
    has_parent: bool,
) -> Expression {
    let mut first_item: Option<FirstItemInExpr> = None;
    let mut args = Vec::<Expression>::with_capacity(2);
    let mut exprs = Vec::<Expression>::with_capacity(2);
    let mut multi = Vec::new();
    loop {
        parser.skip_spaces();
        match parser.curr_char() {
            Some('(') => {
                parser.next();
                state.enter(GroupingSymbol::Parens);
                consume_expr(&mut first_item, &mut args,
                             parse_expr_with_state(parser, state, has_parent));
                if state.is_empty() { break; }
            }
            Some(')') => {
                if has_parent && !state.is_inside(Parens) {
                    // we're within a parent expr which is getting closed, do not consume ')'
                    break;
                }
                parser.next();
                consume_optional_semi_colon(parser);
                if state.is_inside(Parens) {
                    state.exit_symbol();
                    // open parens always cause recursion, so we need to always break on closing parens
                    break;
                } else {
                    return Expression::ExprError(parser.error_unexpected_char(
                        ')', "Closing parens does not match any opening parens"));
                }
            }
            Some(';') => {
                parser.next();
                if state.is_empty() { break; }
                push_if_non_empty(&mut exprs, create_expr(
                    parser, std::mem::replace(&mut first_item, None), &mut args));
            }
            Some(',') => {
                parser.next();
                push_if_non_empty(&mut exprs, create_expr(
                    parser, std::mem::replace(&mut first_item, None), &mut args));
                if let Some(expr) = consume_all(&mut exprs) {
                    if let Expression::Multi(mut m) = expr {
                        multi.append(&mut m);
                    } else {
                        push_if_non_empty(&mut multi, expr);
                    }
                }
            }
            Some(c) => {
                if let Some(word) = parser.parse_word() {
                    if first_item.is_some() {
                        push_if_non_empty(&mut args, expr(parser, word));
                    } else {
                        let word_str = word.as_str();
                        match word_str {
                            "let" | "mut" | "set" => {
                                let is_mut = !word_str.starts_with('l');
                                let expr = match parser.parse_assignment(is_mut) {
                                    Ok(e) => assignment_expr(parser, word_str, e),
                                    Err(e) => ExprError(e.into())
                                };
                                exprs.push(expr);
                            }
                            "if" => {
                                match parse_if(parser, &mut exprs, state) {
                                    Ok(is_done) => if is_done { break; }
                                    Err(e) => return ExprError(e)
                                }
                            }
                            _ => {
                                first_item = Some(FirstItemInExpr::Str(word));
                            }
                        }
                    }
                } else {
                    parser.next();
                    return ExprError(
                        parser.error_unexpected_char(c, "expected expression"));
                }
            }
            None => break
        }
    }

    push_if_non_empty(&mut exprs, create_expr(
        parser, std::mem::replace(&mut first_item, None), &mut args));

    if !multi.is_empty() {
        if let Some(expr) = consume_all(&mut exprs) {
            if let Expression::Multi(mut m) = expr {
                multi.append(&mut m);
            } else {
                push_if_non_empty(&mut multi, expr);
            }
        }
        return Expression::Multi(multi);
    }

    consume_all(&mut exprs).unwrap_or(Expression::Empty)
}

fn parse_if(parser: &mut Parser,
            exprs: &mut Vec<Expression>,
            state: &GroupingState, ) -> Result<bool, TypeError> {
    let state_len = state.len();
    let cond = {
        let expr = parse_expr_start(parser, true);
        let typ = expr.get_type();
        if typ == vec![Type::I32] {
            expr
        } else {
            ExprError(parser.error(
                &format!("condition in if expression must have type i32, but \
              found type {}", types_to_string(&typ))))
        }
    };
    if state.len() < state_len { // closed parens, terminating expr
        return Err(parser.error(
            "incomplete if expressions, missing then expression"));
    }
    let then = parse_expr_start(parser, true);
    let mut els = if state.len() < state_len { // closed parens, terminating expr
        Expression::Empty
    } else {
        parse_expr_start(parser, true)
    };

    let then_type = then.get_type();
    let else_type = els.get_type();
    if then_type != else_type {
        els = ExprError(parser.error(
            &format!("if expression has different types in each branch:\n  \
            - then: {}\n  \
            - else: {}\n\
          To be valid, an if expression must have the same type on both branches.",
                     types_to_string(&then_type),
                     types_to_string(&else_type))));
    }

    exprs.push(Expression::If(
        Box::new(cond), Box::new(then), Box::new(els)));
    Ok(state.len() < state_len || state.is_empty())
}

fn assignment_expr(parser: &Parser, word: &str, assignment: Assignment) -> Expression {
    if word.starts_with('l') {
        Let(assignment)
    } else if word.starts_with('m') {
        Mut(assignment)
    } else {
        let (ids, ..) = &assignment;
        let globals = ids.iter().map(|id|
            parser.stack().get_is_global(id.as_str())
                .map(|(_, global)| global))
            .map(|global| global.unwrap_or(false)).collect();
        Set(ReAssignment { assignment, globals })
    }
}

fn consume_optional_semi_colon(parser: &mut Parser) {
    parser.skip_spaces();
    if parser.curr_char() == Some(';') {
        parser.next();
    }
}

fn consume_all(exprs: &mut Vec<Expression>) -> Option<Expression> {
    let empty_count = exprs.iter().take_while(|e| e == &&Expression::Empty).count();
    exprs.drain(0..empty_count);
    if !exprs.is_empty() {
        if exprs.len() == 1 {
            Some(exprs.remove(0))
        } else {
            Some(Expression::Group(exprs.drain(..).collect()))
        }
    } else {
        None
    }
}

fn push_if_non_empty(exprs: &mut Vec<Expression>, expr: Expression) {
    if expr != Expression::Empty {
        exprs.push(expr);
    }
}

fn consume_expr(
    first_item: &mut Option<FirstItemInExpr>,
    args: &mut Vec<Expression>,
    expr: Expression,
) {
    if expr == Expression::Empty {
        return;
    }
    match (first_item.is_some(), args.is_empty()) {
        (false, false) => {
            panic!("unexpected state: expression has no first item, but has args")
        }
        (false, true) => { // first item expression
            *first_item = Some(FirstItemInExpr::Expr(expr))
        }
        (true, _) => { // there is a first item already
            args.push(expr);
        }
    }
}

fn create_expr(
    parser: &mut Parser,
    first_item: Option<FirstItemInExpr>,
    args: &mut Vec<Expression>,
) -> Expression {
    match (first_item.is_some(), args.is_empty()) {
        (false, true) => Expression::Empty,
        (false, false) => { // expression at first position
            Expression::ExprError(parser.error("Cannot use expression as a function"))
        }
        (true, true) => { // single item
            match first_item.unwrap() {
                FirstItemInExpr::Expr(e) => e,
                FirstItemInExpr::Str(name) => {
                    let args = args.drain(..).collect();
                    let t = parser.stack().get(&name).cloned()
                        .unwrap_or_else(|| Type::Error(parser.error(&format!("Unknown element: '{}'", &name))));
                    match t {
                        Type::Fn(_) | Type::WasmFn(_) => to_fn_call(parser, name, args, t),
                        _ => expr(parser, name)
                    }
                }
            }
        }
        (true, false) => { // item + args
            match first_item.unwrap() {
                FirstItemInExpr::Expr(_) => {
                    args.clear();
                    ExprError(parser.error("fun call via expression not supported yet"))
                }
                FirstItemInExpr::Str(name) => {
                    let args = args.drain(..).collect();
                    let t = parser.stack().get(&name).cloned()
                        .unwrap_or_else(|| Type::Error(parser.error(&format!("Unknown function: '{}'", &name))));
                    to_fn_call(parser, name, args, t)
                }
            }
        }
    }
}

fn to_fn_call(parser: &Parser, name: String, args: Vec<Expression>, t: Type) -> Expression {
    let (typ_idx, is_wasm_fun): (Result<(FunType, usize), TypeError>, bool) = match t {
        Type::Error(e) => (Err(e), false),
        Type::Fn(types) =>
            (type_of_fn_call(&name, &types, &args)
                 .map_err(|reason| TypeError { pos: parser.pos(), reason }), false),
        Type::WasmFn(types) =>
            (type_of_fn_call(&name, &types, &args)
                 .map_err(|reason| TypeError { pos: parser.pos(), reason }), true),
        _ => (Err(parser.error(&format!("Cannot use '{}' (which has type {}) as a function", &name, t))), false)
    };
    let fun_index = typ_idx.as_ref()
        .map(|(_, i)| i.clone())
        .unwrap_or(0 as usize);
    let typ = typ_idx.map(|(t, _)| t);
    Expression::FunCall { name, args, typ, fun_index, is_wasm_fun }
}

fn type_of_fn_call(name: &String, fn_types: &Vec<FunType>, args: &Vec<Expression>)
                   -> Result<(FunType, usize), String> {
    let arg_types = {
        let mut t = Vec::new();
        for arg in args {
            let mut types = arg.get_type();
            t.append(&mut types);
        }
        t
    };

    let mut fun_idx = 0;
    for typ in fn_types {
        if typ.ins.is_empty() && arg_types.is_empty() {
            return Ok((typ.clone(), fun_idx));
        }
        if typ.ins.len() == arg_types.len() {
            let matching_fun = typ.ins.iter().zip(arg_types.iter())
                .find(|(t, arg)| arg.is_assignable_to(t))
                .map(|(t, _)| t);
            if let Some(_) = matching_fun {
                return Ok((typ.clone(), fun_idx));
            }
        }
        fun_idx += 1;
    }

    let types = types_to_string(&arg_types);
    let valid_types = fn_types.iter()
        .map(|t| types_to_string(&t.ins))
        .map(|t| format!("  * {}", t))
        .collect::<Vec<_>>()
        .join("\n");

    Err(format!("fun '{}' cannot be called with arguments of type '{}'.
        Valid argument types are:
        {}", name, types, valid_types))
}

fn expr(parser: &mut Parser, word: String) -> Expression {
    let typ = type_of(word.as_str(), parser.stack());
    match typ {
        Ok(t) => match t.kind {
            Kind::Const => Expression::Const(word, t.typ),
            Kind::Global => Expression::Global(word, t.typ),
            Kind::Local => Expression::Local(word, t.typ),
        }
        Err(e) => ExprError(TypeError { reason: e, pos: parser.pos() })
    }
}

pub fn type_of(str: &str, stack: &Stack) -> Result<TypedElement, String> {
    let mut chars = str.chars();
    type_of_with_sign(str, &mut chars, stack, false)
}

fn type_of_with_sign(
    str: &str,
    chars: &mut Chars,
    stack: &Stack,
    has_sign: bool,
) -> Result<TypedElement, String> {
    if let Some(c) = chars.next() {
        match c {
            '0'..='9' => type_of_num(c, chars).map(|typ| TypedElement { typ, kind: Kind::Const }),
            '-' | '+' if !has_sign => type_of_with_sign(str, chars, stack, true),
            _ => type_of_var(str, stack)
        }
    } else {
        Err("Unexpected EOF".to_string())
    }
}

fn type_of_var(str: &str, stack: &Stack) -> Result<TypedElement, String> {
    stack.get_is_global(str).map(|(t, is_global)|
        Ok(TypedElement { typ: t.to_owned(), kind: if is_global { Kind::Global } else { Kind::Local } })
    ).unwrap_or_else(|| Err(format!("'{}' does not exist in this scope", str)))
}

fn type_of_num(first_digit: char, chars: &mut Chars) -> Result<Type, String> {
    let mut has_dot = false;
    let mut whole_digits = 1;
    let mut decimal_digits = 0;
    let mut explicit_type: Option<Type> = None;
    let mut is_second_digit = true;

    while let Some(c) = chars.next() {
        match c {
            '0'..='9' => {
                if is_second_digit {
                    if first_digit == '0' && c == '0' {
                        return Err("number cannot start with more than one zero".to_string());
                    }
                    is_second_digit = false;
                }
                if has_dot {
                    decimal_digits += 1;
                } else {
                    whole_digits += 1;
                }
            }
            '_' => {}
            '.' => {
                if has_dot {
                    return Err("number contains more than one dot".to_string());
                }
                has_dot = true;
                is_second_digit = false;
            }
            'i' | 'f' => {
                match read_num_type(chars, c == 'i') {
                    Ok(t) => {
                        explicit_type = Some(t);
                        break;
                    }
                    Err(reason) => return Err(reason)
                }
            }
            _ => {
                return Err(format!("number contains invalid character: '{}'", c));
            }
        }
    }

    if has_dot {
        if decimal_digits == 0 {
            return Err("number cannot end with dot".to_string());
        }
        let fits_in_32_bits = decimal_digits + whole_digits < 11;
        if let Some(t) = explicit_type {
            return explicit_float_type(t, fits_in_32_bits);
        }
        Ok(if fits_in_32_bits { Type::F32 } else { Type::F64 })
    } else {
        if first_digit == '0' && whole_digits > 1 {
            return Err("non-zero integer cannot start with zero".to_string());
        }
        let fits_in_32_bits = whole_digits < 11;
        if let Some(t) = explicit_type {
            return explicit_int_type(t, fits_in_32_bits);
        }
        Ok(if fits_in_32_bits { Type::I32 } else { Type::I64 })
    }
}

fn read_num_type(chars: &mut Chars, is_int: bool) -> Result<Type, String> {
    let text: String = chars.collect();
    match text.as_ref() {
        "32" => return Ok(if is_int { Type::I32 } else { Type::F32 }),
        "64" => return Ok(if is_int { Type::I64 } else { Type::F64 }),
        _ => {}
    }
    Err(format!("Unexpected number suffix: '{}'. Valid suffixes are 'i32', 'i64', 'f32' or 'f64'.", text))
}

fn explicit_float_type(typ: Type, fits_in_32_bits: bool) -> Result<Type, String> {
    match typ {
        Type::I64 | Type::I32 => Err("number with decimal part cannot have integer suffix".to_string()),
        Type::F64 => Ok(Type::F64),
        Type::F32 => {
            if fits_in_32_bits {
                Ok(Type::F32)
            } else {
                Err("number is too big to fit in f32 (max total digits for f32 literals is 10)".to_string())
            }
        }
        _ => unreachable!()
    }
}

fn explicit_int_type(typ: Type, fits_in_32_bits: bool) -> Result<Type, String> {
    match typ {
        Type::I64 | Type::F64 => Ok(typ),
        Type::I32 | Type::F32 => {
            if fits_in_32_bits {
                Ok(typ)
            } else {
                Err("number is too big to fit in i32 (max total digits for i32 literals is 10)".to_string())
            }
        }
        _ => unreachable!()
    }
}
