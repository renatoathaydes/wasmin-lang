use crate::ast::{ExtDef};
use crate::parse::Parser;
use crate::parse::parser::ParserError;

pub fn parse_ext(parser: &mut Parser) -> Result<(String, Vec<ExtDef>), ParserError> {
    if let Some(mod_name) = parser.parse_word() {
        parser.skip_spaces();
        if let Some('{') = parser.curr_char() {
            parser.next();
            let defs = parse_defs(parser, &mod_name)?;
            Ok((mod_name, defs))
        } else {
            error(&parser, "expected '{' after module name")
        }
    } else {
        error(&parser, "expected module name")
    }
}

fn parse_defs(parser: &mut Parser, mod_name: &str) -> Result<Vec<ExtDef>, ParserError> {
    let mod_pos = parser.pos();
    parser.stack_mut().new_def_only_level();
    loop {
        parser.skip_spaces();
        if let Some('}') = parser.curr_char() {
            parser.next();
            break;
        }
        let res = parser.parse_def();
        if res.is_err() {
            parser.stack_mut().drop_level();
            return res.map(|_| vec![]);
        }
    }
    let defs = parser.stack_mut().drop_level_and_get_its_defs();
    let ext_defs = defs.iter()
        .map(|(name, typ)| ExtDef { id: name.clone(), typ: typ.clone() })
        .collect();
    parser.stack_mut().push_namespace(mod_name.to_owned(), defs)
        .map_err(|msg| ParserError { pos: mod_pos, msg })?;
    Ok(ext_defs)
}

fn error(parser: &Parser, reason: &str) -> Result<(String, Vec<ExtDef>), ParserError> {
    let err = if let Some(c) = parser.curr_char() {
        parser.error_unexpected_char(c, reason).into()
    } else {
        ParserError { msg: "unexpected EOF".to_owned(), pos: parser.pos() }
    };
    Err(err)
}
