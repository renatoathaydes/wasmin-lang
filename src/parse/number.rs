use crate::parse::model::{Numeric, Position, Token, Token::Number};

pub(crate) fn number(text: &str, position: Position) -> Token {
    if text.ends_with("i32") {
        number_i32(&text[0..text.len() - 3], position)
    } else if text.ends_with("i64") {
        number_i64(&text[0..text.len() - 3], position)
    } else if text.ends_with("f32") {
        todo!() // number_i32()
    } else if text.ends_with("f64") {
        todo!() // &text[0..text.len() - 3]
    } else if text.contains('.') {
        todo!()
    } else {
        number_i32(&text, position)
    }
}

macro_rules! parse_number {
    ($text:expr, $pos:expr, $zero:expr, $one:expr, $adder:ident, $overflow_err:expr, $new:expr) => {{
        let mut result = $zero;
        let mut iter = $text.chars().rev();
        let mut multiplier = $one;
        let mut overflow = false;
        let mut overflow_seen = false;
        for c in iter {
            if c == '_' { continue; }
            if overflow {
                overflow_seen = true;
                break;
            }
            if let Some(d) = c.to_digit(10) {
                if let Some(e) = $adder(result, d, multiplier) {
                    result = e;
                } else {
                    return Token::Error($pos, $overflow_err.to_owned());
                }
            } else {
                return Token::Error($pos, format!("invalid number: {}", $text));
            }
            // cannot fail immediately as this may be the last iteration
            if let Some(m) = multiplier.checked_mul(10) {
                multiplier = m
            } else {
                overflow = true;
            }
        }
        if overflow_seen {
            Token::Error($pos, $overflow_err.to_owned())
        } else {
            Token::Number($pos, $new(result))
        }

    }};
}

fn number_i32(text: &str, position: Position) -> Token {
    parse_number!(text, position, 0i32, 1i32, add_i32, "i32 overflow", Numeric::I32)
}

fn number_i64(text: &str, position: Position) -> Token {
    parse_number!(text, position, 0i64, 1i64, add_i64, "i64 overflow", Numeric::I64)
}

fn add_i32(n: i32, add: u32, multiplier: i32) -> Option<i32> {
    let m = (add as i32).checked_mul(multiplier)?;
    n.checked_add(m)
}

fn add_i64(n: i64, add: u32, multiplier: i64) -> Option<i64> {
    let m = (add as i64).checked_mul(multiplier)?;
    n.checked_add(m)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token_i32(n: i32) -> Token { Number(0, Numeric::I32(n)) }

    fn token_i64(n: i64) -> Token { Number(0, Numeric::I64(n)) }

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
    fn test_i32_inferred() {
        assert_eq!(token_i32(0), number("0", 0));
        assert_eq!(token_i32(1), number("1", 0));
        assert_eq!(token_i32(2), number("2", 0));
        assert_eq!(token_i32(100), number("100", 0));
        assert_eq!(token_i32(1_234_567_890_i32), number("1_234_567_890", 0));
        assert_eq!(token_i32(i32::MAX), number(&format!("{}", i32::MAX), 0));
    }

    #[test]
    fn test_i32_overflow() {
        assert_eq!(Token::Error(0, "i32 overflow".into()),
                   number("2_500_000_000i32", 0));
        assert_eq!(Token::Error(0, "i32 overflow".into()),
                   number("999_888_777_666i32", 0));
    }

    #[test]
    fn test_i64() {
        assert_eq!(token_i64(0), number("0i64", 0));
        assert_eq!(token_i64(1), number("1i64", 0));
        assert_eq!(token_i64(2), number("2i64", 0));
        assert_eq!(token_i64(100), number("100i64", 0));
        assert_eq!(token_i64(1_234_567_890_i64), number("1_234_567_890_i64", 0));
        assert_eq!(token_i64(i64::MAX), number(&format!("{}i64", i64::MAX), 0));
    }

    #[test]
    fn test_i64_overflow() {
        assert_eq!(Token::Error(0, "i64 overflow".into()),
                   number("9_223_372_036_854_775_808i64", 0));
        assert_eq!(Token::Error(0, "i64 overflow".into()),
                   number("9_999_999_999_999_999_999i64", 0));
    }
}
