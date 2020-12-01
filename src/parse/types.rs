use crate::parse::structs::{*};
use crate::types::{*, Type::*};

pub fn parse_type_internal(parser: &mut Parser) -> Type {
    let mut state = GroupingState::new();
    let typ = parse_type(parser, &mut state);
    if state.is_empty() {
        typ
    } else {
        Type::error("?", "Unclosed grouping")
    }
}

fn parse_type(parser: &mut Parser, state: &mut GroupingState) -> Type {
    if let Some(word) = parser.parse_word() {
        match word.as_ref() {
            "i32" => Type::I32,
            "f32" => Type::F32,
            "i64" => Type::I64,
            "f64" => Type::F64,
            _ => Type::error(word.as_ref(), "type does not exist")
        }
    } else if let Some('[') = parser.curr_char() {
        parser.next();
        state.enter(GroupingSymbol::SquareBracket);
        parse_fn_type(parser, state)
    } else if let Some(c) = parser.curr_char() {
        Type::error(c.to_string().as_ref(), "unexpected character")
    } else {
        Type::error("", "EOF reached")
    }
}

fn parse_fn_type(parser: &mut Parser, state: &mut GroupingState) -> Type {
    let mut ins = Vec::with_capacity(2);
    let mut outs = Vec::with_capacity(1);
    let mut starting_outs = false;
    loop {
        parser.skip_spaces();
        if starting_outs {
            // this is the only place we may expect '('
            if let Some('(') = parser.curr_char() {
                parser.next();
                state.enter(GroupingSymbol::Parens);
            }
            starting_outs = false;
        }
        if state.is_inside(GroupingSymbol::SquareBracket) {
            println!("in args");
            if let Some(']') = parser.curr_char() {
                println!("got ]");
                parser.next();
                state.exit_symbol();
                starting_outs = true;
                continue;
            }
        } else if state.is_inside(GroupingSymbol::Parens) {
            println!("in parens");
            if let Some(')') = parser.curr_char() {
                parser.next();
                state.exit_symbol();
                break;
            }
        }
        if let Some(';') = parser.curr_char() {
            break;
        }
        println!("parsing type");
        let typ = parse_type(parser, state);
        println!("Type: {:?}", &typ);
        match typ {
            Type::Error { text: _, reason: _ } => {
                return typ;
            }
            _ => {
                if state.is_inside(GroupingSymbol::SquareBracket) {
                    ins.push(typ);
                } else {
                    outs.push(typ);
                }
            }
        }
    }
    Fn { ins, outs }
}
