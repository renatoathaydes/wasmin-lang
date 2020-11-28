use std::str::Chars;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type {
    I64,
    I32,
    F64,
    F32,
    Empty,
    Error { text: String, reason: String },
}

impl Type {
    pub fn error(text: &str, reason: &str) -> Type {
        Type::Error { text: text.to_string(), reason: reason.to_string() }
    }
}

pub fn type_of(str: &String) -> Type {
    let mut chars = str.chars();
    let c = chars.next();
    return match c {
        Some('0'..='9') => { type_of_num(str, &mut chars) }
        None => { Type::Empty }
        _ => { Type::Error { text: str.clone(), reason: "not a number".to_string() } }
    };
}

fn type_of_num(text: &String, chars: &mut Chars) -> Type {
    let mut dots = 0;
    let mut digits = 0;
    let mut error = false;

    loop {
        if let Some(c) = chars.next() {
            match c {
                '0'..='9' => { digits += 1 }
                '_' => {}
                '.' => { dots += 1; }
                _ => { error = true; }
            }
        } else {
            break;
        }
    }
    if error || dots > 1 {
        Type::Error { text: text.clone(), reason: "number contains invalid digits".to_string() }
    } else if dots == 1 {
        Type::F32
    } else {
        Type::I32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        assert_eq!(type_of(&"".to_string()), Type::Empty);
    }

    #[test]
    fn test_i32() {
        for i in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "10", "11", "20", "100",
            "1_000", "1_111_222_333", "0_1_2_3_4"].iter() {
            assert_eq!(type_of(&i.to_string()), Type::I32, "Example: {}", i);
        }
    }

    #[test]
    fn test_f32() {
        for f in ["0.1", "2.0", "1.3", "3.14151695", "234566788.4344566",
            "1_000.0", "0.1_111_222_333", "0_1_2_.3_4"].iter() {
            assert_eq!(type_of(&f.to_string()), Type::F32, "Example: {}", f);
        }
    }
}
