use std::str::Chars;

use crate::ast::{Assignment, Break, Expression, ReAssignment};
use crate::ast::Expression::{ExprError, Loop};
use crate::parse::Parser;
use crate::parse::parser::{GroupingSymbol, ParserError};
use crate::parse::stack::Stack;
use crate::parse::state::{*};
use crate::types::{FunType, has_error, Kind, Type, TypedElement, TypeError, types_to_string};
use crate::vec_utils::{push_all, remove_last_n};

pub fn parse_expr(parser: &mut Parser) -> Result<Expression, ParserError> {
    let mut stack = Vec::<Type>::new();
    let mut state = ParsingState::new(&mut stack);
    parse_expr_with_state(parser, &mut state, "")
}

fn parse_expr_with_state(
    parser: &mut Parser,
    state: &mut ParsingState,
    skip_word: &'static str,
) -> Result<Expression, ParserError> {
    let result = parse_expr_internal(parser, state, skip_word)?;
    state.verify_end_state();
    Ok(result)
}

fn parse_expr_internal(
    parser: &mut Parser,
    state: &mut ParsingState,
    skip_word: &'static str,
) -> Result<Expression, ParserError> {
    let done = skip_word_or_push(parser, state, skip_word);
    if done {
        consume_expr_parts(parser, state);
        return Ok(state.finish_off());
    }
    loop {
        parser.skip_spaces();
        match parser.curr_char() {
            Some('(') => {
                parser.next();
                state.enter_level(GroupingSymbol::Parens);
                parser.stack_mut().new_level();
            }
            Some(')') => {
                consume_expr_parts(parser, state);
                let is_dropped = state.exit_level(GroupingSymbol::Parens)
                    .map_err(|e| parser.error(e.as_str()))?;
                if is_dropped {
                    parser.stack_mut().drop_level();
                    parser.next();
                    consume_optional_semi_colon(parser);
                    if state.symbols().is_empty() {
                        break;
                    }
                } else { break; }
            }
            Some(';') => {
                parser.next();
                consume_expr_parts(parser, state);
                if state.symbols().is_empty() { break; }
            }
            Some(',') => {
                parser.next();
                consume_expr_parts(parser, state);
            }
            Some(c) => {
                if let Some(word) = parser.parse_word() {
                    let done = consume_expr_part(parser, state, word);
                    if done { break; }
                } else {
                    parser.next();
                    return Err(parser.error_unexpected_char(c, "expected expression").into());
                }
            }
            None => { break; }
        }
    };
    consume_expr_parts(parser, state);
    Ok(state.finish_off())
}

fn consume_expr_part(
    parser: &mut Parser,
    state: &mut ParsingState,
    word: String,
) -> bool {
    let part = create_expr_part(parser, state, word);
    let is_expr = part.is_expression();
    state.push_expr_part(part);
    // if we get an expression we need to stop collecting parts as we're done.
    is_expr && state.symbols().is_empty()
}

fn skip_word_or_push(
    parser: &mut Parser,
    state: &mut ParsingState,
    skip_word: &'static str,
) -> bool {
    if skip_word != "" {
        if let Some(word) = parser.parse_word() {
            if word == skip_word { return false; }
            return consume_expr_part(parser, state, word);
        }
    }
    return false;
}

fn consume_expr_parts(parser: &mut Parser, state: &mut ParsingState) {
    let parts = state.end_expr();
    let exprs = create_expr(parts, state, parser);
    state.push_exprs(exprs);
}

fn create_expr_part(parser: &mut Parser, state: &mut ParsingState, part: String) -> ExprPart {
    let is_first_part = !state.has_non_expr_part();
    let expr = match part.as_str() {
        "let" | "mut" | "set" => {
            let is_mut = !part.starts_with('l');
            match parse_assignment_internal(parser, is_mut, state) {
                Ok(e) => Some(assignment_expr(parser, part.as_str(), e)),
                Err(e) => Some(ExprError(e.into()))
            }
        }
        "if" => {
            match parse_if(parser, state) {
                Ok(e) => Some(e),
                Err(e) => Some(ExprError(e))
            }
        }
        "loop" => Some(parse_loop(parser, state)),
        _ => None
    };
    if let Some(e) = expr {
        return ExprPart::Expr(e);
    }
    if is_first_part { ExprPart::Fun(part) } else { ExprPart::Arg(part) }
}

pub(crate) fn parse_assignment(
    parser: &mut Parser,
    is_mut: bool,
) -> Result<Assignment, ParserError> {
    let mut stack = Vec::<Type>::new();
    let mut state = ParsingState::new(&mut stack);
    parse_assignment_internal(parser, is_mut, &mut state)
}

fn parse_assignment_internal(
    parser: &mut Parser,
    is_mut: bool,
    state: &mut ParsingState,
) -> Result<Assignment, ParserError> {
    let mut ids = Vec::new();
    while let Some(id) = parser.parse_word() {
        ids.push(id);
        parser.skip_spaces();
        if let Some(',') = parser.curr_char() { parser.next(); } else { break; }
    }
    parser.skip_spaces();
    if let Some('=') = parser.curr_char() {
        parser.next();
        let pos = parser.pos();
        let expr = {
            let mut state = ParsingState::new_nested(state);
            parse_expr_with_state(parser, &mut state, "")?
        };
        if ids.len() == expr.get_type().len() {
            let (mut results, mut errors): (Vec<_>, Vec<_>) = ids.iter().zip(state.stack.drain(0..ids.len()))
                .map(move |(id, t)| {
                    parser.stack_mut().push(id.clone(), t, false, is_mut)
                }).partition(|r| r.is_ok());
            if errors.is_empty() {
                let type_replacements = results.drain(..)
                    .map(|r| r.unwrap()).collect();
                Ok((ids, Box::new(expr), type_replacements))
            } else {
                let msg = errors.drain(..).map(|e| e.unwrap_err())
                    .collect::<Vec<_>>().join(", ");
                Err(ParserError { pos, msg })
            }
        } else {
            let typ = expr.get_type();
            let e = format!("multi-value assignment mismatch: \
                {} identifier{} but expression results in type{} '{}'",
                            ids.len(), if ids.len() == 1 { "" } else { "s" },
                            if typ.len() == 1 { "" } else { "s" },
                            typ.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join(" "));
            parser.parser_err(e)
        }
    } else {
        parser.parser_err(format!("Expected '=' in let expression, but got {}",
                                  parser.curr_char().map(|c| format!("'{}'", c))
                                      .unwrap_or_else(|| "EOF".to_string())))
    }
}

fn parse_if(parser: &mut Parser,
            state: &mut ParsingState,
) -> Result<Expression, TypeError> {
    let is_within_parens = state.symbols().is_inside(&GroupingSymbol::Parens);
    let cond = {
        let (cond, cond_type) =
            parse_expr_with_clean_state(parser, state, "")?;
        if cond_type == vec![Type::I32] {
            remove_last_n(state.stack, 1);
            cond
        } else {
            ExprError(parser.error(
                &format!("condition in if expression must have type i32, but \
              found type {}", types_to_string(&cond_type))))
        }
    };

    if is_within_parens {
        parser.skip_spaces();
        if parser.curr_char() == Some(')') {
            parser.next();
            return Err(parser.error(
                "incomplete if expressions, missing then expression"));
        }
    }

    let (then, then_type) =
        parse_expr_with_clean_state(parser, state, "then")?;

    if !has_error(&then_type) {
        remove_last_n(state.stack, then_type.len());
    }

    let (mut els, else_type) = if is_within_parens {
        parser.skip_spaces();
        if parser.curr_char() == Some(')') {
            (Expression::Empty, vec![])
        } else {
            parse_expr_with_clean_state(parser, state, "else")?
        }
    } else {
        parse_expr_with_clean_state(parser, state, "else")?
    };

    if then_type != else_type {
        els = ExprError(parser.error(
            &format!("if expression has different types in each branch:\n  \
            - then: {}\n  \
            - else: {}\n\
          To be valid, an if expression must have the same type on both branches.",
                     types_to_string(&then_type),
                     types_to_string(&else_type))));
    }

    Ok(Expression::If(Box::new(cond), Box::new(then), Box::new(els)))
}

fn parse_loop(parser: &mut Parser, state: &mut ParsingState) -> Expression {
    state.start_block();
    match parse_expr_with_clean_state(parser, state, "") {
        Ok((expr, typ)) => {
            let error = if typ.is_empty() {
                let mut breaks = vec![];
                expr.get_nested_breaks(usize::MAX, &mut breaks);
                if breaks.is_empty() { None } else {
                    let first_break = breaks.remove(0);
                    let mut err = None;
                    for br in breaks {
                        if br.types != first_break.types {
                            err = Some(TypeError {
                                pos: parser.pos(),
                                reason: format!("break has type(s) '{}', but the first break in \
                                    this loop breaks with type(s) '{}'. All breaks should have the \
                                    same type(s)", types_to_string(&br.types),
                                                types_to_string(&first_break.types)),
                            });
                            break;
                        }
                    }
                    err
                }
            } else {
                Some(TypeError {
                    pos: parser.pos(),
                    reason: format!("loop leaving values of type(s) {} on \
                        the stack. Loops cannot leave values on the stack \
                        without returning them with break instructions",
                                    types_to_string(&typ)),
                })
            };
            Loop { expr: Box::new(expr), error }
        }
        Err(e) => ExprError(e)
    }
}

fn parse_expr_with_clean_state(
    parser: &mut Parser,
    state: &mut ParsingState,
    skip_word: &'static str,
) -> Result<(Expression, Vec<Type>), TypeError> {
    let mut state = ParsingState::new_nested(state);
    let expr = parse_expr_with_state(parser, &mut state, skip_word)
        .map_err(|e| e.into())?;
    let typ = expr.get_type();
    Ok((expr, typ))
}

fn assignment_expr(parser: &Parser, word: &str, assignment: Assignment) -> Expression {
    if word.starts_with('l') {
        Expression::Let(assignment)
    } else if word.starts_with('m') {
        Expression::Mut(assignment)
    } else {
        let (ids, ..) = &assignment;
        let globals = ids.iter().map(|id|
            parser.stack().get_is_global(id.as_str())
                .map(|(_, global)| global))
            .map(|global| global.unwrap_or(false)).collect();
        Expression::Set(ReAssignment { assignment, globals })
    }
}

fn consume_optional_semi_colon(parser: &mut Parser) {
    parser.skip_spaces();
    if parser.curr_char() == Some(';') {
        parser.next();
    }
}

fn create_expr(
    mut parts: Vec<ExprPart>,
    state: &mut ParsingState,
    parser: &mut Parser,
) -> Vec<Expression> {
    if parts.is_empty() {
        return vec![];
    }
    let mut fun: Option<String> = None;
    let mut result = Vec::with_capacity(parts.len() + 1);
    loop {
        match parts.remove(0) {
            // remember the fun for later, as we need to push its arguments first
            ExprPart::Fun(fun_name) => {
                if let Some(f) = fun {
                    push_fun(state, parser, &mut result, f)
                }
                fun = Some(fun_name);
            }
            ExprPart::Arg(w) => result.push(word_expr(parser, w, &mut state.stack)),
            ExprPart::Expr(e) => result.push(e),
        }
        if parts.is_empty() { break; }
    }
    // if there was a pending function call, we can push it now that its args have been pushed
    if let Some(f) = fun {
        push_fun(state, parser, &mut result, f)
    }
    result
}

fn push_fun(
    state: &mut ParsingState,
    parser: &mut Parser,
    exprs: &mut Vec<Expression>,
    name: String,
) {
    if name == "break" {
        exprs.push(create_break(parser, state));
    } else {
        exprs.push(word_expr(parser, name, &mut state.stack));
    }
}

fn create_break(parser: &mut Parser, state: &mut ParsingState) -> Expression {
    if let Some(start_len) = state.get_stack_count_at_block_start() {
        if state.stack.len() < start_len {
            ExprError(parser.error(
                "cannot 'break' at this point because loop would be consuming \
                 items from the stack indefinitely, depleting the stack."))
        } else {
            let types = state.stack.drain(start_len..).collect();
            Expression::Br(Break { types })
        }
    } else {
        ExprError(parser.error("cannot use 'break' outside a 'loop' block"))
    }
}

fn word_expr(
    parser: &mut Parser,
    word: String,
    stack: &mut Vec<Type>,
) -> Expression {
    let typ = match type_of(word.as_str(), parser.stack()) {
        Ok(t) => t,
        Err(e) => return Expression::ExprError(TypeError { reason: e, pos: parser.pos() })
    };
    match typ.typ {
        Type::I64 | Type::I32 | Type::F64 | Type::F32 => {
            stack.push(typ.typ.clone());
            match typ.kind {
                Kind::Const => Expression::Const(word, typ.typ),
                Kind::Local => Expression::Local(word, typ.typ),
                Kind::Global => Expression::Global(word, typ.typ)
            }
        }
        Type::Empty => Expression::Empty,
        Type::Fn(fun_types) => select_fun(fun_types, word, parser, stack, false),
        Type::WasmFn(fun_types) => select_fun(fun_types, word, parser, stack, true),
        Type::Error(e) => Expression::ExprError(e),
    }
}

fn select_fun(
    mut fun_types: Vec<FunType>,
    fun_name: String,
    parser: &mut Parser,
    stack: &mut Vec<Type>,
    is_wasm_fun: bool,
) -> Expression {
    fun_types.sort_unstable_by(|a, b| b.ins.len().cmp(&a.ins.len()));
    for (i, typ) in fun_types.iter().enumerate() {
        if fun_can_be_called(typ, stack) {
            remove_last_n(stack, typ.ins.len());
            push_all(&typ.outs, stack);
            return Expression::FunCall { name: fun_name, typ: Ok(typ.clone()), fun_index: i, is_wasm_fun };
        }
    }
    ExprError(TypeError {
        reason: format!("cannot call fun '{}' with current stack: '{}'",
                        fun_name, types_to_string(stack)),
        pos: parser.pos(),
    })
}

fn fun_can_be_called(typ: &FunType, stack: &Vec<Type>) -> bool {
    typ.ins.len() <= stack.len() && typ.ins.iter().zip(stack.iter())
        .all(|(t, s)| s.is_assignable_to(t))
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
