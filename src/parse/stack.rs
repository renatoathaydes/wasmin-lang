use std::collections::HashMap;

use crate::types::Type;
use crate::wasm_funs::wasm_std_funs;

#[derive(Debug, Clone)]
pub struct Stack {
    items: Vec<HashMap<String, StackEntry>>
}

#[derive(Debug, Clone)]
struct StackEntry {
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
            match (entry.is_def, is_def, &mut entry.typ) {
                // was def, is def
                (true, false, _) => {
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
                }
                // was def, is def
                (false, true, &mut Type::Fn(ref mut current_types)) => {
                    let t = match typ {
                        Type::Fn(mut new_type) => {
                            if current_types.iter().any(|t| new_type.contains(t)) {
                                return Err(format!("Cannot re-define '{}' with the same type", &id));
                            }
                            assert_eq!(new_type.len(), 1);
                            new_type.remove(0)
                        }
                        _ => return Err(format!("Cannot re-implement '{}' as a non-function", &id))
                    };
                    entry.is_def = true;
                    current_types.push(t);
                    Ok(None)
                }
                (true, true, _) => Err(format!("Cannot re-define '{}'. Try implementing it first, \
                    then re-defining it with different types.", &id)),
                _ => Err(format!("Cannot re-implement '{}'", &id))
            }
        } else {
            symbols.insert(id, StackEntry { typ, is_def });
            Ok(None)
        }
    }

    pub fn get_is_global(&self, id: &str) -> Option<(&Type, bool)> {
        (0..self.items.len()).rev().find_map(|i| {
            let symbols = self.items.get(i).unwrap();
            if let Some(entry) = symbols.get(id) {
                Some((&entry.typ, i == 0))
            } else { None }
        })
    }

    pub fn get(&self, id: &str) -> Option<&Type> {
        self.get_is_global(id).map(|(t, _)| t)
    }

    pub fn get_def(&self, id: &str) -> Option<&Type> {
        self.items.get(self.items.len() - 1).and_then(|entries|
            entries.get(id).filter(|e| e.is_def).map(|e| &e.typ))
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
        wasm_std_funs()
    }
}

#[cfg(test)]
mod stack_tests {
    use crate::types::FnType;

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

    #[test]
    fn stack_gets_closer_bindings_first() {
        let mut stack = Stack::new();
        stack.push("foo".to_string(), Type::I32, false).unwrap();
        stack.new_level();
        stack.push("foo".to_string(), Type::I64, false).unwrap();
        stack.new_level();
        stack.push("foo".to_string(), Type::F32, false).unwrap();

        assert_eq!(stack.get(&"foo"), Some(&Type::F32));
        stack.drop_level();
        assert_eq!(stack.get(&"foo"), Some(&Type::I64));
        stack.drop_level();
        assert_eq!(stack.get(&"foo"), Some(&Type::I32));
    }

    #[test]
    fn stack_knows_globals_and_locals() {
        let mut stack = Stack::new();
        stack.push("foo".to_string(), Type::I32, false).unwrap();
        stack.push("bar".to_string(), Type::I32, false).unwrap();
        stack.new_level();
        stack.push("foo".to_string(), Type::I64, false).unwrap();
        stack.new_level();
        stack.push("zort".to_string(), Type::F64, false).unwrap();

        assert_eq!(stack.get_is_global(&"foo"), Some((&Type::I64, false)));
        assert_eq!(stack.get_is_global(&"bar"), Some((&Type::I32, true)));
        assert_eq!(stack.get_is_global(&"zort"), Some((&Type::F64, false)));
        stack.drop_level();
        assert_eq!(stack.get_is_global(&"foo"), Some((&Type::I64, false)));
        assert_eq!(stack.get_is_global(&"bar"), Some((&Type::I32, true)));
        assert_eq!(stack.get_is_global(&"zort"), None);
        stack.drop_level();
        assert_eq!(stack.get_is_global(&"foo"), Some((&Type::I32, true)));
        assert_eq!(stack.get_is_global(&"bar"), Some((&Type::I32, true)));
    }

    #[test]
    fn stack_allows_def_then_implement() {
        let mut stack = Stack::new();

        // same type
        stack.push("foo".to_string(), Type::I32, true).unwrap();
        stack.push("foo".to_string(), Type::I32, false).unwrap();

        // convertible type
        stack.push("bar".to_string(), Type::I64, true).unwrap();
        stack.push("bar".to_string(), Type::I32, false).unwrap();

        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::I32));
    }

    #[test]
    fn stack_allows_multiple_def_then_implement_for_functions() {
        let mut stack = Stack::new();

        // first fun types
        stack.push("foo".to_string(), Type::Fn(vec![FnType { ins: vec![], outs: vec![] }]), true).unwrap();
        stack.push("foo".to_string(), Type::Fn(vec![FnType { ins: vec![], outs: vec![] }]), false).unwrap();

        // second fun types
        stack.push("foo".to_string(), Type::Fn(vec![FnType { ins: vec![Type::I32], outs: vec![] }]), true).unwrap();
        stack.push("foo".to_string(), Type::Fn(vec![FnType { ins: vec![Type::I32], outs: vec![] }]), false).unwrap();

        assert_eq!(stack.get(&"foo"), Some(&Type::Fn(vec![
            FnType { ins: vec![], outs: vec![] },
            FnType { ins: vec![Type::I32], outs: vec![] },
        ])));
    }
}
