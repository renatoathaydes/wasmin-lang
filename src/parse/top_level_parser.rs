use Visibility::Public;

use crate::ast::{TopLevelElement, Visibility};
use crate::ast::Visibility::Private;
use crate::parse::Parser;

pub fn parse(parser: &mut Parser) {
    parser.store_comments(true);
    while let Some(word) = parser.parse_word() {
        parse_top(parser, word.as_ref(), false);
        parser.store_comments(true);
    }
}

fn parse_top(parser: &mut Parser, word: &str, is_pub: bool) {
    let expr = match word.as_ref() {
        "pub" if !is_pub => {
            if let Some(w) = parser.parse_word() {
                return parse_top(parser, &w, true);
            } else {
                Some(TopLevelElement::Error(format!("Unexpected: '{}'. \
                Expected let, mut or fun", word), parser.pos()))
            }
        }
        "def" if !is_pub => {
            parser.store_comments(false);
            if let Err(e) = parser.parse_def() {
                Some(e.into())
            } else {
                None
            }
        }
        "let" | "mut" => {
            let comment = parser.get_comment(false);
            match parser.parse_assignment(word.starts_with('m')) {
                Ok(items) => {
                    let visibility = if is_pub { Public } else { Private };
                    if word.starts_with("m") {
                        Some(TopLevelElement::Mut(items, visibility, comment))
                    } else {
                        Some(TopLevelElement::Let(items, visibility, comment))
                    }
                }
                Err(e) => Some(e.into())
            }
        }
        "fun" => {
            let comment = parser.get_comment(false);
            let visibility = if is_pub { Public } else { Private };
            match parser.parse_fun() {
                Ok(fun) => Some(TopLevelElement::Fun(fun, visibility, comment)),
                Err(e) => Some(e.into())
            }
        }
        "ext" => {
            let comment = parser.get_comment(false);
            let visibility = if is_pub { Public } else { Private };
            match parser.parse_ext() {
                Ok((mod_name, defs)) =>
                    Some(TopLevelElement::Ext(mod_name, defs, visibility, comment)),
                Err(e) => Some(e.into())
            }
        }
        _ => {
            let allowed = format!("{}let, mut or fun", if is_pub { "" } else { "pub, def, " });
            Some(TopLevelElement::Error(format!("Unexpected: '{}'. \
                Expected {} here.", word, allowed), parser.pos()))
        }
    };

    if let Some(e) = expr {
        parser.sink().send(e).expect("Wasmin Program Receiver Error");
    }
}
