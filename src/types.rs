
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type {
    I64,
    I32,
    F64,
    F32,
    Empty,
    Fn { ins: Vec<Type>, outs: Vec<Type> },
    Error { reason: String, pos: (usize, usize) },
}

impl Type {
    pub fn is_error(&self) -> bool {
        match self {
            Type::Error { pos: _, reason: _ } => true,
            Type::Fn { ins, outs } => {
                ins.iter().any(|t| t.is_error()) ||
                    outs.iter().any(|t| t.is_error())
            }
            _ => false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_is_error() {
        let error = || { Type::Error { pos: (0, 0), reason: "".to_string() } };
        assert_eq!(Type::I32.is_error(), false);
        assert_eq!(Type::I64.is_error(), false);
        assert_eq!(Type::F32.is_error(), false);
        assert_eq!(Type::F64.is_error(), false);
        assert_eq!(Type::Fn { ins: vec![Type::I64], outs: vec![] }.is_error(), false);
        assert_eq!(Type::Fn { ins: vec![Type::I64], outs: vec![Type::I32] }.is_error(), false);
        assert_eq!(Type::Empty.is_error(), false);

        assert_eq!(error().is_error(), true);
        assert_eq!(Type::Fn { ins: vec![Type::I64], outs: vec![error()] }.is_error(), true);
        assert_eq!(Type::Fn { ins: vec![error()], outs: vec![Type::I32] }.is_error(), true);
        assert_eq!(Type::Fn {
            ins: vec![Type::I64],
            outs: vec![
                Type::Fn { ins: vec![Type::I64], outs: vec![error()] }
            ],
        }.is_error(), true);
        assert_eq!(Type::Fn {
            ins: vec![Type::I64],
            outs: vec![
                Type::Fn { ins: vec![Type::I64, error()], outs: vec![] }
            ],
        }.is_error(), true);
    }
}
