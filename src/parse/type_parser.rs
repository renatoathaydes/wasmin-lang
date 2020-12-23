use crate::parse::parser::{*};
use crate::parse::parser::GroupingSymbol::{Parens, SquareBracket};
use crate::types::{*, Type::*};

pub fn parse_type(parser: &mut Parser) -> Type {
    let mut state = GroupingState::new();
    let typ = parse_type_internal(parser, &mut state);
    parser.skip_spaces();
    if let Some(';') = parser.curr_char() { parser.next(); }
    if state.is_empty() { typ } else {
        Type::Error(parser.error(&format!("Unclosed '{}' in type definition", state)))
    }
}

fn parse_type_internal(parser: &mut Parser, state: &mut GroupingState) -> Type {
    if let Some(word) = parser.parse_word() {
        match word.as_ref() {
            "i32" => Type::I32,
            "f32" => Type::F32,
            "i64" => Type::I64,
            "f64" => Type::F64,
            _ => Type::Error(parser.error(&format!("type does not exist: {}", word.clone())))
        }
    } else if let Some('[') = parser.curr_char() {
        parser.next();
        state.enter(GroupingSymbol::SquareBracket);
        parse_fn_type(parser, state)
    } else if let Some(c) = parser.curr_char() {
        Type::Error(parser.error(&format!("unexpected character: '{}'", c)))
    } else {
        Type::Error(parser.error("EOF reached (type was expected)"))
    }
}

fn parse_fn_type(parser: &mut Parser, state: &mut GroupingState) -> Type {
    let ins = parse_fn_ins(parser, state);
    let outs = parse_fn_outs(parser, state);
    Fn(vec![FnType { ins, outs }])
}

fn parse_fn_ins(parser: &mut Parser, state: &mut GroupingState) -> Vec<Type> {
    let mut ins = Vec::with_capacity(2);
    loop {
        parser.skip_spaces();
        if let Some(']') = parser.curr_char() {
            parser.next();
            state.exit_symbol();
            break;
        }
        let typ = parse_type_internal(parser, state);
        let is_error = typ.is_error();
        ins.push(typ);
        if is_error { break; }
    }
    ins
}

fn parse_fn_outs(parser: &mut Parser, state: &mut GroupingState) -> Vec<Type> {
    let mut outs = Vec::with_capacity(2);
    parser.skip_spaces();
    let in_parens = match parser.curr_char() {
        Some('(') => {
            state.enter(GroupingSymbol::Parens);
            parser.next();
            true
        }
        _ => false
    };
    loop {
        parser.skip_spaces();
        match parser.curr_char() {
            Some(')') => {
                if in_parens {
                    state.exit_symbol();
                    parser.next();
                    consume_optional_semi_colon(parser);
                    break;
                }
                if state.is_inside(Parens) {
                    break;
                }
            }
            Some(']') => {
                if state.is_inside(SquareBracket) {
                    break;
                }
            }
            Some(';') => {
                if !in_parens {
                    break;
                }
            }
            _ => {}
        }
        let typ = parse_type_internal(parser, state);
        let is_error = typ.is_error();
        outs.push(typ);
        if is_error { break; }
    }
    outs
}

fn consume_optional_semi_colon(parser: &mut Parser) {
    parser.skip_spaces();
    if let Some(';') = parser.curr_char() {
        parser.next();
    }
}
