use crate::parse::model::{Numeric, Position, Token, Token::Number};

pub(crate) fn number(text: &str, position: Position) -> Token {
    if text.ends_with("i32") {
        number_i32(&text[0..text.len() - 3], position)
    } else if text.ends_with("i64") {
        number_i64(&text[0..text.len() - 3], position)
    } else if text.ends_with("f32") {
        number_f32(&text[0..text.len() - 3], position)
    } else if text.ends_with("f64") {
        number_f64(&text[0..text.len() - 3], position)
    } else if text.contains('.') {
        number_f32(text, position)
    } else {
        number_i32(text, position)
    }
}

macro_rules! parse_number {
    ($text:expr, $pos:expr, $zero:expr, $one:expr, $adder:ident, $overflow_err:expr, $new:expr) => {{
        let mut result = $zero;
        let iter = $text.chars().rev();
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

fn number_f32(text: &str, position: Position) -> Token {
    if let Some(dot) = text.find('.') {
        match decimal_parts_i32(text, dot, position) {
            Ok((int, dec)) => {
                let result = int as f32;
                let dec = dec as f32 * 10f32.powi(-count_digits(dec as i64));
                let result = result + dec;
                Number(position, Numeric::F32(result))
            }
            Err(token) => token
        }
    } else {
        let token = number_i32(text, position);
        if let Number(_, Numeric::I32(int)) = token {
            Number(position, Numeric::F32(int as f32))
        } else {
            token
        }
    }
}

fn number_f64(text: &str, position: Position) -> Token {
    if let Some(dot) = text.find('.') {
        match decimal_parts_i64(text, dot, position) {
            Ok((int, dec)) => {
                let result = int as f64;
                let dec = dec as f64 * 10f64.powi(-count_digits(dec));
                let result = result + dec;
                Number(position, Numeric::F64(result))
            }
            Err(token) => token
        }
    } else {
        let token = number_i64(text, position);
        if let Number(_, Numeric::I64(int)) = token {
            Number(position, Numeric::F64(int as f64))
        } else {
            token
        }
    }
}

fn decimal_parts_i32(text: &str, dot: usize, position: Position) -> Result<(i32, i32), Token> {
    let mut token = number_i32(&text[0..dot], position);
    if let Number(_, Numeric::I32(int)) = token {
        token = number_i32(&text[dot + 1..], position);
        if let Number(_, Numeric::I32(dec)) = token {
            return Ok((int, dec));
        }
    }
    // something went wrong, return whatever error we got
    Err(token)
}

fn decimal_parts_i64(text: &str, dot: usize, position: Position) -> Result<(i64, i64), Token> {
    let mut token = number_i64(&text[0..dot], position);
    if let Number(_, Numeric::I64(int)) = token {
        token = number_i64(&text[dot + 1..], position);
        if let Number(_, Numeric::I64(dec)) = token {
            return Ok((int, dec));
        }
    }
    // something went wrong, return whatever error we got
    Err(token)
}

fn count_digits(n: i64) -> i32 {
    if n == 0i64 { return 1; }
    // the log10 integer part (its characteristic) gives the number of digits - 1
    // see: https://en.wikipedia.org/wiki/Common_logarithm
    ((n as f64).log10().floor() + 1.0) as i32
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

    fn token_f32(n: f32) -> Token { Number(0, Numeric::F32(n)) }

    fn token_f64(n: f64) -> Token { Number(0, Numeric::F64(n)) }

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

    #[test]
    fn test_count_digits() {
        assert_eq!(count_digits(0), 1);
        assert_eq!(count_digits(1), 1);
        assert_eq!(count_digits(10), 2);
        assert_eq!(count_digits(123456789), 9);
        assert_eq!(count_digits(1_000_000_000), 10);
    }

    #[test]
    fn test_f32() {
        assert_eq!(token_f32(0f32), number("0f32", 0));
        assert_eq!(token_f32(0f32), number("0.0f32", 0));
        assert_eq!(token_f32(1f32), number("1f32", 0));
        assert_eq!(token_f32(0.1f32), number("0.1f32", 0));
        assert_eq!(token_f32(2f32), number("2f32", 0));
        assert_eq!(token_f32(0.25f32), number("0.25f32", 0));
        assert_eq!(token_f32(123.123456f32), number("123.123456f32", 0));
        assert_eq!(token_f32(1_234_567_890.98765f32), number("1_234_567_890.98765_f32", 0));
        assert_eq!(token_f32(1_234_567_890.1_234_567_890f32),
                   number("1_234_567_890.1_234_567_890f32", 0));
    }

    #[test]
    fn test_f32_inferred() {
        assert_eq!(token_f32(0f32), number("0.0", 0));
        assert_eq!(token_f32(1f32), number("1.0", 0));
        assert_eq!(token_f32(0.1f32), number("0.1", 0));
        assert_eq!(token_f32(2f32), number("2.0", 0));
        assert_eq!(token_f32(0.25f32), number("0.25", 0));
        assert_eq!(token_f32(123.123456f32), number("123.123456", 0));
        assert_eq!(token_f32(1_234_567_890.98765f32), number("1_234_567_890.98765", 0));
        assert_eq!(token_f32(1_234_567_890.1_234_567_890f32),
                   number("1_234_567_890.1_234_567_890", 0));
    }

    #[test]
    fn test_f64() {
        assert_eq!(token_f64(0f64), number("0f64", 0));
        assert_eq!(token_f64(0f64), number("0.0f64", 0));
        assert_eq!(token_f64(1f64), number("1f64", 0));
        assert_eq!(token_f64(0.1f64), number("0.1f64", 0));
        assert_eq!(token_f64(2f64), number("2f64", 0));
        assert_eq!(token_f64(0.25f64), number("0.25f64", 0));
        assert_eq!(token_f64(123.123456f64), number("123.123456f64", 0));
        assert_eq!(token_f64(1_234_567_890.98765f64), number("1_234_567_890.98765_f64", 0));
        assert_eq!(token_f64(1_234_567_890.1_234_567_890f64),
                   number("1_234_567_890.1_234_567_890f64", 0));
    }
}
