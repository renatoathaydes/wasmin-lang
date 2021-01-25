use std::str::Chars;

use crate::ast::{Assignment, Expression, ReAssignment};
use crate::ast::Expression::{ExprError, Group};
use crate::parse::Parser;
use crate::parse::parser::{GroupingState, GroupingSymbol};
use crate::parse::stack::Stack;
use crate::types::{FunType, Kind, Type, TypedElement, TypeError};
use crate::vec_utils::{remove_last_n, get_last, push_all};

enum ExprPart {
    Word(String),
    Expr(Expression),
}

struct ParsingState {
    symbols: GroupingState,
    stack: Vec<Type>,
    expr_parts: Vec<Vec<ExprPart>>,
    exprs: Vec<Vec<Expression>>,
}

impl ParsingState {
    fn new() -> ParsingState {
        ParsingState {
            symbols: GroupingState::new(),
            stack: vec![],
            exprs: vec![vec![]],
            expr_parts: vec![vec![]],
        }
    }

    fn enter(&mut self, symbol: GroupingSymbol) {
        self.symbols.enter(symbol);
        self.expr_parts.push(vec![]);
        self.exprs.push(vec![]);
    }

    fn drop_level(&mut self, parser: &mut Parser) -> Result<(), String> {
        if self.symbols.is_inside(GroupingSymbol::Parens) {
            self.symbols.exit_symbol();
            self.end_expr(parser);
            remove_last_n(&mut self.expr_parts, 1);
            let len = self.exprs.len();
            let dropped_level = self.exprs.remove(len - 1);
            merge(self.curr_level(), dropped_level);
            Ok(())
        } else {
            Err("Closing parens does not match any opening parens".to_owned())
        }
    }

    fn curr_level(&mut self) -> &mut Vec<Expression> {
        get_last(&mut self.exprs)
    }

    fn push_expr_part(&mut self, part: String, parser: &mut Parser) {
        let parts = get_last(&mut self.expr_parts);
        if parts.is_empty() {
            let expr = match part.as_str() {
                "let" | "mut" | "set" => {
                    let is_mut = !part.starts_with('l');
                    match parser.parse_assignment(is_mut) {
                        Ok(e) => Some(assignment_expr(parser, part.as_str(), e)),
                        Err(e) => Some(ExprError(e.into()))
                    }
                }
                "if" => {
                    // match parse_if(parser, &mut exprs, state) {
                    //     Ok(is_done) => if is_done { break; }
                    //     Err(e) => return ExprError(e)
                    // }
                    unimplemented!()
                }
                _ => None
            };
            if let Some(e) = expr {
                parts.push(ExprPart::Expr(e))
            }
        }
        parts.push(ExprPart::Word(part));
    }

    fn end_expr(&mut self, parser: &mut Parser) {
        let len = self.expr_parts.len();
        let parts = self.expr_parts.get_mut(len - 1).unwrap().drain(..).collect();
        let mut exprs = create_expr(parts, &mut self.stack, parser);
        push_all(&exprs, self.curr_level());
    }

    fn finish_off(&mut self) -> Expression {
        if self.exprs.len() != 1 {
            panic!("Expected one level of expressions left, but got {}", self.exprs.len());
        }
        let mut level = self.exprs.remove(0);
        if level.is_empty() {
            Expression::Empty
        } else if level.len() == 1 {
            level.remove(0)
        } else {
            Group(level)
        }
    }
}

pub fn parse_expr(parser: &mut Parser) -> Result<Expression, String> {
    let mut state = ParsingState::new();
    loop {
        parser.skip_spaces();
        match parser.curr_char() {
            Some('(') => {
                parser.next();
                parser.stack_mut().new_level();
                state.enter(GroupingSymbol::Parens);
            }
            Some(')') => {
                parser.next();
                parser.stack_mut().drop_level();
                state.drop_level(parser)?;
                consume_optional_semi_colon(parser);
                if state.symbols.is_empty() {
                    break;
                }
            }
            Some(';') => {
                parser.next();
                state.end_expr(parser);
                if state.symbols.is_empty() {
                    break;
                }
            }
            Some(',') => {
                parser.next();
                state.end_expr(parser);
            }
            Some(c) => {
                if let Some(word) = parser.parse_word() {
                    state.push_expr_part(word, parser);
                } else {
                    parser.next();
                    return Err(format!("unexpected character: '{}'", c));
                }
            }
            None => {
                break;
            }
        }
    };
    state.end_expr(parser);
    Ok(state.finish_off())
}

// fn parse_if(parser: &mut Parser,
//             exprs: &mut Vec<Expression>,
//             state: &GroupingState, ) -> Result<bool, TypeError> {
//     let state_len = state.len();
//     let cond = {
//         let expr = parse_expr_start(parser, true);
//         let typ = expr.get_type();
//         if typ == vec![Type::I32] {
//             expr
//         } else {
//             ExprError(parser.error(
//                 &format!("condition in if expression must have type i32, but \
//               found type {}", types_to_string(&typ))))
//         }
//     };
//     if state.len() < state_len { // closed parens, terminating expr
//         return Err(parser.error(
//             "incomplete if expressions, missing then expression"));
//     }
//     let then = parse_expr_start(parser, true);
//     let mut els = if state.len() < state_len { // closed parens, terminating expr
//         Expression::Empty
//     } else {
//         parse_expr_start(parser, true)
//     };
//
//     let then_type = then.get_type();
//     let else_type = els.get_type();
//     if then_type != else_type {
//         els = ExprError(parser.error(
//             &format!("if expression has different types in each branch:\n  \
//             - then: {}\n  \
//             - else: {}\n\
//           To be valid, an if expression must have the same type on both branches.",
//                      types_to_string(&then_type),
//                      types_to_string(&else_type))));
//     }
//
//     exprs.push(Expression::If(
//         Box::new(cond), Box::new(then), Box::new(els)));
//     Ok(state.len() < state_len || state.is_empty())
// }

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

fn merge(exprs: &mut Vec<Expression>, mut arg: Vec<Expression>) {
    if exprs.is_empty() {
        exprs.append(&mut arg);
    } else {
        match get_last(exprs) {
            Group(ref mut v) => v.append(&mut arg),
            _ => exprs.push(Group(arg))
        };
    }
}

fn create_expr(
    mut parts: Vec<ExprPart>,
    stack: &mut Vec<Type>,
    parser: &mut Parser,
) -> Vec<Expression> {
    if parts.is_empty() {
        return vec![];
    }
    let first = parts.remove(0);
    match first {
        ExprPart::Word(word) => {
            let mut result = Vec::with_capacity(parts.len() + 1);
            while !parts.is_empty() {
                match parts.remove(0) {
                    ExprPart::Word(w) => result.push(word_expr(parser, w, stack)),
                    ExprPart::Expr(e) => result.push(e),
                };
            }
            result.push(word_expr(parser, word, stack));
            result
        }
        ExprPart::Expr(e) => {
            if parts.is_empty() {
                vec![e]
            } else {
                vec![Expression::ExprError(TypeError {
                    reason: format!("invocation of function selected by expression is not yet \
                supported. Expression: {:?}", e),
                    pos: parser.pos(),
                })]
            }
        }
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
        reason: format!("cannot call fun '{}' with current stack", fun_name),
        pos: parser.pos(),
    })
}

fn fun_can_be_called(typ: &FunType, stack: &Vec<Type>) -> bool {
    typ.ins.len() <= stack.len()
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
