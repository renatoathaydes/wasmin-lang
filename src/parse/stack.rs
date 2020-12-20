use std::collections::HashMap;
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct Stack {
    items: Vec<HashMap<String, StackEntry>>
}

#[derive(Debug, Clone)]
pub struct StackEntry {
    typ: Type,
    is_def: bool,
}

impl Stack {
    pub fn new() -> Stack {
        let mut s = Stack { items: Vec::with_capacity(4) };
        s.new_level();
        s
    }

    /// Push a definition onto the stack, returning an empty value if the definition is accepted
    /// with the exact same type as provided, another type if the implementation type is different
    /// from the definition but still assignable, or an error message in case something goes wrong.
    ///
    /// If [is_def] is `true`, then the definition is only accepted if it did not exist yet,
    /// otherwise this is considered as a direct implementation, hence it will be accepted
    /// if either the type matches a previous definition or if no definition exists yet.
    pub fn push(&mut self, id: String, typ: Type, is_def: bool) -> Result<Option<Type>, String> {
        let last_index = self.items.len() - 1;
        let symbols = self.items.get_mut(last_index).unwrap();
        if let Some(mut entry) = symbols.get_mut(&id) {
            println!("ID={}, T is {}, was_def={}", &id, entry.typ, entry.is_def);
            if entry.is_def && !is_def {
                if !typ.is_assignable_to(&entry.typ) {
                    return Err(format!("Cannot implement '{}' with type '{}' because its \
                      defined type '{}' does not match", &id, &typ, &entry.typ));
                }
                entry.is_def = false;
                if typ == entry.typ {
                    Ok(None)
                } else {
                    Ok(Some(entry.typ.clone()))
                }
            } else {
                Err(format!("Cannot re-implement '{}'", &id))
            }
        } else {
            symbols.insert(id, StackEntry { typ, is_def });
            Ok(None)
        }
    }

    pub fn get(&self, id: &str) -> Option<&Type> {
        (0..self.items.len()).rev().find_map(|i| {
            let symbols = self.items.get(i).unwrap();
            if let Some(entry) = symbols.get(id) {
                Some(&entry.typ)
            } else { None }
        })
    }

    pub fn new_level(&mut self) {
        self.items.push(HashMap::new());
    }

    pub fn drop_level(&mut self) {
        let len = self.items.len();
        if len > 1 {
            self.items.remove(len - 1);
        } else {
            panic!("attempt to drop single stack level");
        }
    }
}

impl Default for Stack {
    fn default() -> Self {
        Stack::new()
    }
}

#[cfg(test)]
mod stack_tests {
    use super::*;

    #[test]
    fn stack_can_have_bindings() {
        let mut stack = Stack::new();
        assert_eq!(stack.get(&"foo"), None);
        stack.push("foo".to_string(), Type::Empty, false).unwrap();
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.push("bar".to_string(), Type::I64, false).unwrap();
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        assert_eq!(stack.get(&"z"), None);
    }

    #[test]
    fn stack_can_have_multi_level_bindings() {
        let mut stack = Stack::new();
        stack.push("foo".to_string(), Type::Empty, false).unwrap();
        stack.new_level();
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.new_level();
        stack.push("bar".to_string(), Type::I64, false).unwrap();
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.new_level();
        stack.push("z".to_string(), Type::F32, false).unwrap();
        assert_eq!(stack.get(&"z"), Some(&Type::F32));
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.new_level();
        stack.push("foo".to_string(), Type::F64, false).unwrap();
        assert_eq!(stack.get(&"z"), Some(&Type::F32));
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::F64));

        stack.drop_level();
        assert_eq!(stack.get(&"z"), Some(&Type::F32));
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.drop_level();
        assert_eq!(stack.get(&"z"), None);
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.drop_level();
        assert_eq!(stack.get(&"z"), None);
        assert_eq!(stack.get(&"bar"), None);
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
    }
}
