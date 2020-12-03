use crate::parse::structs::{*};
use crate::parse::structs::GroupingSymbol::{Parens, SquareBracket};
use crate::types::{*, Type::*};

pub fn parse_type_internal(parser: &mut Parser) -> Type {
    let mut state = GroupingState::new();
    let typ = parse_type(parser, &mut state);
    if state.is_empty() {
        typ
    } else {
        parser.error("Unclosed grouping")
    }
}

fn parse_type(parser: &mut Parser, state: &mut GroupingState) -> Type {
    println!("Parsing type at {:?}", parser.pos());
    if let Some(word) = parser.parse_word() {
        match word.as_ref() {
            "i32" => Type::I32,
            "f32" => Type::F32,
            "i64" => Type::I64,
            "f64" => Type::F64,
            _ => parser.error(&format!("type does not exist: {}", word.clone()))
        }
    } else if let Some('[') = parser.curr_char() {
        println!("enter args");
        parser.next();
        state.enter(GroupingSymbol::SquareBracket);
        parse_fn_type(parser, state)
    } else if let Some(c) = parser.curr_char() {
        parser.error(&format!("unexpected character: '{}'", c))
    } else {
        parser.error("EOF reached (type was expected)")
    }
}

fn parse_fn_type(parser: &mut Parser, state: &mut GroupingState) -> Type {
    let ins = parse_fn_ins(parser, state);
    let outs = parse_fn_outs(parser, state);
    println!("end fun");
    Fn { ins, outs }
}

fn parse_fn_ins(parser: &mut Parser, state: &mut GroupingState) -> Vec<Type> {
    let mut ins = Vec::with_capacity(2);
    loop {
        parser.skip_spaces();
        if let Some(']') = parser.curr_char() {
            println!("end args");
            parser.next();
            state.exit_symbol();
            break;
        }
        let typ = parse_type(parser, state);
        println!("Type: {:?}", &typ);
        let is_error = typ.is_error();
        ins.push(typ);
        if is_error { break; }
    }
    ins
}

fn parse_fn_outs(parser: &mut Parser, state: &mut GroupingState) -> Vec<Type> {
    println!("in outs");
    let mut outs = Vec::with_capacity(2);
    parser.skip_spaces();
    let in_parens = match parser.curr_char() {
        Some('(') => {
            println!("enter parens");
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
                    println!("end parens");
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
        let typ = parse_type(parser, state);
        println!("Type: {:?}", &typ);
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
