use crate::ast::ExtDef;
use crate::errors::WasminError;
use crate::parse::Parser;
use crate::parse::parser::ParserError;

pub fn parse_ext(parser: &mut Parser) -> Result<(String, Vec<ExtDef>), WasminError> {
    if let Some(mod_name) = parser.parse_word() {
        parser.skip_spaces();
        if let Some('{') = parser.curr_char() {
            parser.next();
            let defs = parse_defs(parser, &mod_name)?;
            Ok((mod_name, defs))
        } else {
            werr_syntax!("expected '{' after module name, got EOF", parser.pos())
        }
    } else {
        werr_syntax!("expected module name, got EOF", parser.pos())
    }
}

fn parse_defs(parser: &mut Parser, mod_name: &str) -> Result<Vec<ExtDef>, WasminError> {
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
