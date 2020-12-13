use std::fmt;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct FnType {
    pub ins: Vec<Type>,
    pub outs: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct TypeError {
    pub reason: String,
    pub pos: (usize, usize),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Type {
    I64,
    I32,
    F64,
    F32,
    Empty,
    Fn(FnType),
    Error(TypeError),
}

impl Type {
    pub fn is_error(&self) -> bool {
        match self {
            Type::Error(..) => true,
            Type::Fn(FnType { ins, outs }) => {
                ins.iter().any(|t| t.is_error()) ||
                    outs.iter().any(|t| t.is_error())
            }
            _ => false
        }
    }

    pub fn is_empty(&self) -> bool {
        self == &Type::Empty
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I64 => write!(f, "i64")?,
            Type::I32 => write!(f, "i32")?,
            Type::F64 => write!(f, "f64")?,
            Type::F32 => write!(f, "f32")?,
            Type::Empty => write!(f, "()")?,
            Type::Fn(FnType { ins, outs }) => {
                write!(f, "fun[ ")?;
                for t in ins { write!(f, "{} ", t)?; }
                write!(f, " ]( ")?;
                for t in outs { write!(f, "{} ", t)?; }
                write!(f, ")")?;
            }
            Type::Error(TypeError { reason, pos, .. }) => {
                write!(f, "ERROR([{}, {}] {})", pos.0, pos.1, reason)?
            }
        };
        Ok(())
    }
}

impl FnType {
    pub fn get_type(&self) -> Vec<Type> {
        self.outs.clone()
    }
}

impl TypeError {
    pub fn get_type(&self) -> Vec<Type> {
        vec![Type::Error(self.clone())]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_is_error() {
        let error = || { Type::Error(TypeError { pos: (0, 0), reason: "".to_string() }) };
        assert_eq!(Type::I32.is_error(), false);
        assert_eq!(Type::I64.is_error(), false);
        assert_eq!(Type::F32.is_error(), false);
        assert_eq!(Type::F64.is_error(), false);
        assert_eq!(Type::Fn(FnType { ins: vec![Type::I64], outs: vec![] }).is_error(), false);
        assert_eq!(Type::Fn(FnType { ins: vec![Type::I64], outs: vec![Type::I32] }).is_error(), false);
        assert_eq!(Type::Empty.is_error(), false);

        assert_eq!(error().is_error(), true);
        assert_eq!(Type::Fn(FnType { ins: vec![Type::I64], outs: vec![error()] }).is_error(), true);
        assert_eq!(Type::Fn(FnType { ins: vec![error()], outs: vec![Type::I32] }).is_error(), true);
        assert_eq!(Type::Fn(FnType {
            ins: vec![Type::I64],
            outs: vec![
                Type::Fn(FnType { ins: vec![Type::I64], outs: vec![error()] })
            ],
        }).is_error(), true);
        assert_eq!(Type::Fn(FnType {
            ins: vec![Type::I64],
            outs: vec![
                Type::Fn(FnType { ins: vec![Type::I64, error()], outs: vec![] })
            ],
        }).is_error(), true);
    }
}
