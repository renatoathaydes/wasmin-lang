use Visibility::Public;

use crate::ast::Visibility::Private;
use crate::ast::{TopLevelElement, Visibility};
use crate::parse::Parser;

pub fn parse(parser: &mut Parser) {
    parser.store_comments(true);
    while let Some(word) = parser.parse_word() {
        parse_top(parser, word.as_ref(), false);
        parser.store_comments(true);
    }
}

fn parse_top(parser: &mut Parser, word: &str, is_pub: bool) {
    let expr = match word {
        "pub" if !is_pub => {
            if let Some(w) = parser.parse_word() {
                return parse_top(parser, &w, true);
            } else {
                let c = parser
                    .curr_char()
                    .map(|c| c.to_string())
                    .unwrap_or("EOF".to_owned());
                let err = werr_syntax!(
                    format!("{} was expected, but got {}", allowed_at_top(true), c),
                    parser.pos()
                );
                Some(TopLevelElement::Error(err))
            }
        }
        "def" if !is_pub => {
            parser.store_comments(false);
            if let Err(e) = parser.parse_def() {
                Some(TopLevelElement::Error(e))
            } else {
                None
            }
        }
        "let" | "mut" => {
            let comment = parser.take_comment();
            match parser.parse_assignment(word.starts_with('m')) {
                Ok(items) => {
                    let visibility = if is_pub { Public } else { Private };
                    if word.starts_with('m') {
                        Some(TopLevelElement::Mut(items, visibility, comment))
                    } else {
                        Some(TopLevelElement::Let(items, visibility, comment))
                    }
                }
                Err(e) => Some(TopLevelElement::Error(e)),
            }
        }
        "fun" => {
            let comment = parser.take_comment();
            let visibility = if is_pub { Public } else { Private };
            match parser.parse_fun() {
                Ok(fun) => Some(TopLevelElement::Fun(fun, visibility, comment)),
                Err(e) => Some(TopLevelElement::Error(e)),
            }
        }
        "ext" => {
            let comment = parser.take_comment();
            let visibility = if is_pub { Public } else { Private };
            match parser.parse_ext() {
                Ok((mod_name, defs)) => {
                    Some(TopLevelElement::Ext(mod_name, defs, visibility, comment))
                }
                Err(e) => Some(TopLevelElement::Error(e)),
            }
        }
        _ => {
            let err = werr_syntax!(
                format!("{} was expected, but got {}", allowed_at_top(true), word),
                parser.pos()
            );
            Some(TopLevelElement::Error(err))
        }
    };

    if let Some(e) = expr {
        // errors are reported by the other thread
        let _ = parser.sink().send(e);
    }
}

fn allowed_at_top(is_pub: bool) -> String {
    format!("{}let, mut or fun", if is_pub { "" } else { "pub, def, " })
}
