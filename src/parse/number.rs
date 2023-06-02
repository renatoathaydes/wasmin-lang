use std::os::macos::raw::stat;

use crate::parse::model::{Numeric, Position, Token};

pub(crate) fn number(text: &str, position: Position) -> Token {
    if text.ends_with("i32") {
        number_i32(&text[0..text.len() - 3], position)
    } else if text.ends_with("i64") {
        todo!() // number_i64()
    } else if text.ends_with("i32") {
        todo!() // number_i32()
    } else if text.ends_with("i32") {
        todo!() // &text[0..text.len() - 3]
    } else {
        todo!()
    }
}

fn number_i32(text: &str, position: Position) -> Token {
    let mut result = 0i32;
    let mut iter = text.chars().rev();
    let mut multiplier: i32 = 1;
    let mut overflow = false;
    let mut overflow_seen = false;
    for c in iter {
        if c == '_' { continue; }
        if overflow {
            overflow_seen = true;
            break;
        }
        if let Some(d) = c.to_digit(10) {
            if let Some(e) = add_i32(result, d, multiplier) {
                result = e;
            } else {
                return Token::Error(position, "i32 overflow".to_owned());
            }
        } else {
            return Token::Error(position, format!("invalid number: {}", text));
        }
        // cannot fail immediately as this may be the last iteration
        if let Some(m) = multiplier.checked_mul(10) {
            multiplier = m
        } else {
            overflow = true;
        }
    }
    if overflow_seen {
        Token::Error(position, "i32 overflow".to_owned())
    } else {
        Token::Number(position, Numeric::I32(result))
    }
}

fn add_i32(n: i32, add: u32, multiplier: i32) -> Option<i32> {
    let m = (add as i32).checked_mul(multiplier)?;
    n.checked_add(m)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token_i32(n: i32) -> Token { Token::Number(0, Numeric::I32(n)) }

    #[test]
    fn test_i32() {
        assert_eq!(token_i32(0), number("0i32", 0));
        assert_eq!(token_i32(1), number("1i32", 0));
        assert_eq!(token_i32(2), number("2i32", 0));
        assert_eq!(token_i32(100), number("100i32", 0));
        assert_eq!(token_i32(1_234_567_890_i32), number("1_234_567_890_i32", 0));
        assert_eq!(token_i32(i32::MAX), number(&format!("{}i32", i32::MAX), 0));
    }

    #[test]
    fn test_i32_overflow() {
        assert_eq!(Token::Error(0, "i32 overflow".into()),
                   number("2_500_000_000i32", 0));
        assert_eq!(Token::Error(0, "i32 overflow".into()),
                   number("999_888_777_666i32", 0));
    }
}