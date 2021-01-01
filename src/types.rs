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
    Fn(Vec<FnType>),
    WasmFn(Vec<FnType>),
    Error(TypeError),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq, Copy)]
pub enum Kind { Const, Local, Global }

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct TypedElement {
    pub typ: Type,
    pub kind: Kind,
}

impl Type {
    pub fn is_assignable_to(&self, other: &Type) -> bool {
        match (self, other) {
            (a, b) if *a == *b => true,
            (&Type::I32, &Type::I64) => true,
            (&Type::I32, &Type::F32) => true,
            (&Type::I64, &Type::F64) => true,
            (&Type::F32, &Type::F64) => true,
            (&Type::Fn(ref v), &Type::Fn(ref w)) => {
                if v.len() == 1 {
                    w.contains(v.get(0).unwrap())
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            Type::Error(..) => true,
            Type::Fn(types) => {
                types.iter()
                    .any(|t|
                        t.ins.iter().any(|t| t.is_error()) ||
                            t.outs.iter().any(|t| t.is_error()))
            }
            _ => false
        }
    }

    pub fn is_empty(&self) -> bool {
        self == &Type::Empty
    }
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[")?;
        f.write_str(&types_to_string(&self.ins))?;
        f.write_str("](")?;
        f.write_str(&types_to_string(&self.outs))?;
        f.write_str(")")
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
            Type::Fn(types) | Type::WasmFn(types) => {
                write!(f, "(")?;
                let text = types.iter().map(|t| format!("{}", t))
                    .collect::<Vec<_>>().join(" | ");
                write!(f, "{})", text)?;
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

pub(crate) fn type_refs_to_string(types: &Vec<&Type>) -> String {
    let mut res = String::new();
    if types.is_empty() { return res; }
    let max = types.len() - 1;
    for (i, t) in types.iter().enumerate() {
        res.push_str(&format!("{}", t));
        if i != max {
            res.push(' ');
        }
    }
    res
}

pub(crate) fn types_to_string(types: &Vec<Type>) -> String {
    let v = types.iter().collect();
    type_refs_to_string(&v)
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Type::{*};

    #[test]
    fn test_type_is_assignable_to() {
        assert!(F32.is_assignable_to(&F32));
        assert!(F64.is_assignable_to(&F64));
        assert!(I32.is_assignable_to(&I32));
        assert!(I64.is_assignable_to(&I64));

        assert!(I32.is_assignable_to(&F32));
        assert!(I64.is_assignable_to(&F64));

        // cannot truncate
        assert!(!I64.is_assignable_to(&I32));
        assert!(!F64.is_assignable_to(&F32));

        // incompatible types
        assert!(!F32.is_assignable_to(&I32));
        assert!(!F32.is_assignable_to(&I64));
        assert!(!F64.is_assignable_to(&I32));
        assert!(!F64.is_assignable_to(&I64));
        assert!(!I32.is_assignable_to(&F64));
        assert!(!I64.is_assignable_to(&F32));

        // widening conversion
        assert!(I32.is_assignable_to(&I64));
        assert!(F32.is_assignable_to(&F64));
    }

    #[test]
    fn test_fn_type_is_assignable_to() {
        assert!(Fn(vec![fun_type!([]())]).is_assignable_to(&Fn(vec![fun_type!([]())])));
        assert!(Fn(vec![fun_type!([I32]())]).is_assignable_to(&Fn(vec![fun_type!([I32]())])));
        assert!(Fn(vec![fun_type!([I32](I64))]).is_assignable_to(&Fn(vec![fun_type!([I32](I64))])));
        assert!(Fn(vec![fun_type!([I32 F32](I64 F64))]).is_assignable_to(&Fn(vec![fun_type!([I32 F32](I64 F64))])));

        assert!(Fn(vec![fun_type!([I32 F32](I64 F64))]).is_assignable_to(
            &Fn(vec![fun_type!([I32 F32](I64 F64)), fun_type!([](I32))])));
        assert!(Fn(vec![fun_type!([I32 F32](I64 F64))]).is_assignable_to(
            &Fn(vec![fun_type!([](I32)), fun_type!([I32 F32](I64 F64))])));
    }

    #[test]
    fn test_type_is_error() {
        let error = || { Error(TypeError { pos: (0, 0), reason: "".to_string() }) };
        assert_eq!(I32.is_error(), false);
        assert_eq!(I64.is_error(), false);
        assert_eq!(F32.is_error(), false);
        assert_eq!(F64.is_error(), false);
        assert_eq!(Fn(vec!(FnType { ins: vec![I64], outs: vec![] })).is_error(), false);
        assert_eq!(Fn(vec!(FnType { ins: vec![I64], outs: vec![I32] })).is_error(), false);
        assert_eq!(Empty.is_error(), false);

        assert_eq!(error().is_error(), true);
        assert_eq!(Fn(vec!((FnType { ins: vec![I64], outs: vec![error()] }))).is_error(), true);
        assert_eq!(Fn(vec!(FnType { ins: vec![error()], outs: vec![I32] })).is_error(), true);
        assert_eq!(Fn(vec!(FnType {
            ins: vec![I64],
            outs: vec![
                Fn(vec!(FnType { ins: vec![I64], outs: vec![error()] })),
            ],
        })).is_error(), true);
        assert_eq!(Fn(vec!(FnType {
            ins: vec![I64],
            outs: vec![
                Fn(vec!(FnType { ins: vec![I64, error()], outs: vec![] })),
            ],
        })).is_error(), true);
    }
}
