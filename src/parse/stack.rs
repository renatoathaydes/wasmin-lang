use std::collections::HashMap;

use crate::types::{FnType, Type};
use crate::wasm_funs::wasm_std_funs;

#[derive(Debug, Clone)]
pub struct Stack {
    items: Vec<HashMap<String, StackEntry>>,
    namespaces: HashMap<String, Namespace>,
    def_only_level: bool,
}

pub type Namespace = HashMap<String, Type>;

#[derive(Debug, Clone)]
struct StackEntry {
    typ: Type,
    is_def: bool,
    is_mut: bool,
}

impl Stack {
    pub fn new() -> Stack {
        let mut s = Stack {
            items: Vec::with_capacity(4),
            namespaces: HashMap::new(),
            def_only_level: false,
        };
        s.new_level();
        s
    }

    fn get_entry(&mut self, id: &String) -> Option<&mut StackEntry> {
        for symbols in self.items.iter_mut().rev() {
            let value = symbols.get_mut(id);
            if value.is_some() { return value; }
        }
        None
    }

    /// Push a definition onto the stack, returning an empty value if the definition is accepted
    /// with the exact same type as provided, another type if the implementation type is different
    /// from the definition but still assignable, or an error message in case something goes wrong.
    ///
    /// If [is_def] is `true`, then the definition is only accepted if it did not exist yet or
    /// it existed but [is_mut] was true,
    /// otherwise this is considered as a direct implementation, hence it will be accepted
    /// if either the type matches a previous definition or if no definition exists yet.
    pub fn push(&mut self,
                id: String,
                typ: Type,
                is_def: bool,
                is_mut: bool,
    ) -> Result<Option<Type>, String> {
        let is_def_only_level = self.def_only_level;
        if let Some(mut entry) = self.get_entry(&id) {
            if entry.is_def {
                entry.is_mut = is_mut;
            } else if entry.is_mut != is_mut {
                return Err(format!("Cannot change mutability of '{}'", &id));
            }
            match (entry.is_def, is_def, is_mut, &mut entry.typ) {
                // was def, is def, is mut, ...
                (true, true, _, &mut Type::WasmFn(ref mut current_types)) => {
                    match typ {
                        Type::WasmFn(mut t) => {
                            current_types.push(t.remove(0));
                        }
                        _ => {
                            return Err(format!("Cannot re-define WASM native function '{}'", &id));
                        }
                    }
                    Ok(None)
                }
                // was def, is def, is mut, ...
                (true, false, _, _) | (false, false, true, _) => {
                    if !typ.is_assignable_to(&entry.typ) {
                        return Err(format!("Cannot implement '{}' with type '{}' because its \
                      defined type '{}' does not match", &id, &typ, &entry.typ));
                    }
                    entry.is_def = is_def;
                    if typ == entry.typ {
                        Ok(None)
                    } else {
                        Ok(Some(entry.typ.clone()))
                    }
                }
                // was def, is def, is mut, ...
                (false, true, _, &mut Type::Fn(ref mut current_types)) => {
                    entry.is_def = true;
                    add_function_overload(id.as_str(), current_types, typ)
                }
                // was def, is def, is mut, ...
                (true, true, _, &mut Type::Fn(ref mut current_types))
                if is_def_only_level => {
                    add_function_overload(id.as_str(), current_types, typ)
                }
                // was def, is def, is mut, ...
                (true, true, _, _) => Err(format!("Cannot re-define '{}'. Try implementing it first, \
                    then re-defining it with different types.", &id)),
                // was def, is def, is mut, ...
                _ => Err(format!("Cannot re-implement '{}'", &id))
            }
        } else {
            let last_index = self.items.len() - 1;
            let symbols = self.items.get_mut(last_index).unwrap();
            symbols.insert(id, StackEntry { typ, is_def, is_mut });
            Ok(None)
        }
    }

    pub fn get_is_global(&self, id: &str) -> Option<(&Type, bool)> {
        (0..self.items.len()).rev().find_map(|i| {
            if let Some(idx) = id.find('.') {
                let (mod_name, elem) = id.split_at(idx);
                let elem = &elem[1..];
                return if let Some(ns) = self.namespaces.get(mod_name) {
                    ns.get(elem).map(|e| (e, true))
                } else { None };
            }
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
        if self.def_only_level {
            panic!("attempting to create new level inside def-only stack level");
        }
        self.items.push(HashMap::new());
        self.def_only_level = false;
    }

    pub fn new_def_only_level(&mut self) {
        self.items.push(HashMap::new());
        self.def_only_level = true;
    }

    pub fn drop_level(&mut self) {
        if self.def_only_level {
            panic!("attempting to drop level inside def-only stack level");
        }
        let len = self.items.len();
        if len > 1 {
            self.items.remove(len - 1);
        } else {
            panic!("attempt to drop single stack level");
        }
    }

    pub fn get_namespace(&self, namespace: &str) -> Option<&Namespace> {
        self.namespaces.get(namespace)
    }

    pub fn push_namespace(&mut self, namespace: String, mut entries: Vec<(String, Type)>) -> Result<(), String> {
        if self.namespaces.contains_key(namespace.as_str()) {
            return Err(format!("namespace '{}' already exists", &namespace));
        }
        let mut value = HashMap::with_capacity(entries.len());
        for (id, typ) in entries.drain(..) {
            value.insert(id, typ);
        }
        self.namespaces.insert(namespace, value);
        Ok(())
    }

    pub fn drop_level_and_get_its_defs(&mut self) -> Vec<(String, Type)> {
        self.def_only_level = false;
        self.items.remove(self.items.len() - 1).drain()
            .filter(|(_, entry)| entry.is_def)
            .map(|(id, entry)| (id, entry.typ))
            .collect()
    }
}

fn add_function_overload(id: &str,
                         current_types: &mut Vec<FnType>,
                         typ: Type,
) -> Result<Option<Type>, String> {
    let t = match typ {
        Type::Fn(mut new_type) => {
            if current_types.iter().any(|t| new_type.contains(t)) {
                return Err(format!("Cannot re-define '{}' with the same type", id));
            }
            assert_eq!(new_type.len(), 1);
            new_type.remove(0)
        }
        _ => return Err(format!("Cannot re-implement '{}' as a non-function", id))
    };
    current_types.push(t);
    Ok(None)
}

impl Default for Stack {
    fn default() -> Self {
        wasm_std_funs()
    }
}

#[cfg(test)]
mod stack_tests {
    use crate::types::{FnType, Type::{*}};

    use super::*;

    #[test]
    fn stack_can_have_bindings() {
        let mut stack = Stack::new();
        assert_eq!(stack.get(&"foo"), None);
        stack.push("foo".to_string(), Type::Empty, false, false).unwrap();
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.push("bar".to_string(), Type::I64, false, false).unwrap();
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        assert_eq!(stack.get(&"z"), None);
    }

    #[test]
    fn stack_can_have_multi_level_bindings() {
        let mut stack = Stack::new();
        stack.push("foo".to_string(), Type::Empty, false, false).unwrap();
        stack.new_level();
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.new_level();
        stack.push("bar".to_string(), Type::I64, false, false).unwrap();
        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::Empty));
        stack.new_level();
        stack.push("z".to_string(), Type::F32, false, false).unwrap();
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
    fn stack_knows_globals_and_locals() {
        let mut stack = Stack::new();
        stack.push("foo".to_string(), Type::I64, false, false).unwrap();
        stack.push("bar".to_string(), Type::I32, false, false).unwrap();
        stack.new_level();
        stack.push("zed".to_string(), Type::I64, false, false).unwrap();
        stack.new_level();
        stack.push("zort".to_string(), Type::F64, false, false).unwrap();

        assert_eq!(stack.get_is_global(&"foo"), Some((&Type::I64, true)));
        assert_eq!(stack.get_is_global(&"bar"), Some((&Type::I32, true)));
        assert_eq!(stack.get_is_global(&"zed"), Some((&Type::I64, false)));
        assert_eq!(stack.get_is_global(&"zort"), Some((&Type::F64, false)));
        stack.drop_level();
        assert_eq!(stack.get_is_global(&"foo"), Some((&Type::I64, true)));
        assert_eq!(stack.get_is_global(&"bar"), Some((&Type::I32, true)));
        assert_eq!(stack.get_is_global(&"zed"), Some((&Type::I64, false)));
        assert_eq!(stack.get_is_global(&"zort"), None);
        stack.drop_level();
        assert_eq!(stack.get_is_global(&"foo"), Some((&Type::I64, true)));
        assert_eq!(stack.get_is_global(&"bar"), Some((&Type::I32, true)));
        assert_eq!(stack.get_is_global(&"zed"), None);
    }

    #[test]
    fn stack_can_set_mut_globals() {
        let mut stack = Stack::new();
        stack.push("counter".to_string(), Type::I32, false, true).unwrap();
        stack.new_level();
        stack.push("counter".to_string(), Type::I32, false, true).unwrap();

        assert_eq!(stack.get_is_global(&"counter"), Some((&Type::I32, true)));
        stack.drop_level();
        assert_eq!(stack.get_is_global(&"counter"), Some((&Type::I32, true)));
    }

    #[test]
    fn stack_allows_def_then_implement() {
        let mut stack = Stack::new();

        // same type
        stack.push("foo".to_string(), Type::I32, true, false).unwrap();
        stack.push("foo".to_string(), Type::I32, false, false).unwrap();

        // convertible type
        stack.push("bar".to_string(), Type::I64, true, false).unwrap();
        stack.push("bar".to_string(), Type::I32, false, false).unwrap();

        assert_eq!(stack.get(&"bar"), Some(&Type::I64));
        assert_eq!(stack.get(&"foo"), Some(&Type::I32));
    }

    #[test]
    fn stack_allows_multiple_def_then_implement_for_functions() {
        let mut stack = Stack::new();

        // first fun types
        stack.push("foo".to_string(), Type::Fn(vec![FnType { ins: vec![], outs: vec![] }]), true, false).unwrap();
        stack.push("foo".to_string(), Type::Fn(vec![FnType { ins: vec![], outs: vec![] }]), false, false).unwrap();

        // second fun types
        stack.push("foo".to_string(), Type::Fn(vec![FnType { ins: vec![Type::I32], outs: vec![] }]), true, false).unwrap();
        stack.push("foo".to_string(), Type::Fn(vec![FnType { ins: vec![Type::I32], outs: vec![] }]), false, false).unwrap();

        assert_eq!(stack.get(&"foo"), Some(&Type::Fn(vec![
            FnType { ins: vec![], outs: vec![] },
            FnType { ins: vec![Type::I32], outs: vec![] },
        ])));
    }

    #[test]
    fn stack_can_have_namespaces() {
        let mut stack = Stack::new();
        let entries = vec![
            ("number".to_owned(), Type::I32),
            ("function".to_owned(), Type::Fn(vec![fun_type!([I64 I32](I64)), fun_type!([I64](I64))])),
        ];
        stack.push_namespace("env".to_owned(), entries).unwrap();

        let ns = stack.get_namespace("env").unwrap();
        assert_eq!(ns.get("number"), Some(Type::I32).as_ref());
        assert_eq!(ns.get("function"), Some(Type::Fn(vec![
            fun_type!([I64 I32](I64)),
            fun_type!([I64](I64))
        ])).as_ref());
        assert_eq!(ns.get("func"), None.as_ref());
        assert_eq!(ns.get("other"), None.as_ref());
        assert_eq!(stack.get_namespace("other"), None.as_ref());
    }
}
