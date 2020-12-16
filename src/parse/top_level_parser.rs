use Visibility::Public;

use crate::ast::{TopLevelExpression, Visibility};
use crate::ast::Visibility::Private;
use crate::parse::Parser;

pub fn parse(parser: &mut Parser) {
    while let Some(word) = parser.parse_word() {
        parse_top(parser, word.as_ref(), false);
    }
}

fn parse_top(parser: &mut Parser, word: &str, is_pub: bool) {
    let expr = match word.as_ref() {
        "pub" if !is_pub => {
            if let Some(word) = parser.parse_word() {
                return parse_top(parser, &word, true);
            } else {
                TopLevelExpression::Error(format!("Unexpected: '{}'. \
                Expected let, mut or fun", word), parser.pos())
            }
        }
        "let" | "mut" => {
            match parser.parse_assignment() {
                Ok(items) => {
                    let visibility = if is_pub { Public } else { Private };
                    if word.starts_with("m") {
                        TopLevelExpression::Mut(items, visibility)
                    } else {
                        TopLevelExpression::Let(items, visibility)
                    }
                }
                Err(e) => TopLevelExpression::Error(e.msg, e.pos)
            }
        }
        _ => {
            let allowed = format!("{}def, let, mut or fun", if is_pub { "" } else { "pub, " });
            TopLevelExpression::Error(format!("Unexpected: '{}'. \
                Expected {}", word, allowed), parser.pos())
        }
    };

    parser.sink().send(expr).expect("Wasmin Program Receiver Error");
}
