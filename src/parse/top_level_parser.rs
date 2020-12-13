use Visibility::Public;

use crate::ast::{TopLevelExpression, Visibility};
use crate::parse::Parser;

pub fn parse(parser: &mut Parser) {
    while let Some(word) = parser.parse_word() {
        if word == "let" {
            match parser.parse_let() {
                Ok(items) => {
                    parser.sink().send(TopLevelExpression::Let(items, Public))
                        .expect("Wasmin Program Receiver Error");
                }
                Err(e) => { println!("ERROR at {}", e); }
            }
        } else {
            println!("ERROR: only let supported, got {}", word);
            break;
        }
    }
}
