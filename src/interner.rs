use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Interner {
    buckets: [Vec<String>; 1024],
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct InternedStr {
    bucket_index: usize,
    str_index: usize,
}

const SIZE: usize = 1024;

impl Default for Interner {
    fn default() -> Self {
        Interner::new()
    }
}

impl Interner {
    pub fn new() -> Interner {
        const BTREE: Vec<String> = Vec::new();
        let buckets: [Vec<String>; SIZE] = [BTREE; SIZE];
        Interner { buckets }
    }

    pub fn intern(self: &mut Self, s: &str) -> InternedStr {
        let mut hasher = DefaultHasher::new();
        s.hash(&mut hasher);
        let bucket_index = (hasher.finish() % 1024) as usize;
        let bucket = &mut self.buckets[bucket_index];
        if let Some(str_index) = bucket.iter().rposition(|mys| mys == s) {
            return InternedStr { bucket_index, str_index };
        }
        let news = s.to_owned();
        let str_index = bucket.len();
        bucket.push(news);
        InternedStr { bucket_index, str_index }
    }

    pub fn get(&self, interned_ref: &InternedStr) -> &str {
        let bucket = &self.buckets[interned_ref.bucket_index];
        &bucket[interned_ref.str_index]
    }
}

#[cfg(test)]
mod tests {
    use std::ptr;

    use crate::interner::Interner;

    #[test]
    fn test_interner() {
        let mut int = Interner::new();
        let (foo1, bar1, foo2, bar2) = {
            let foo = String::from("foo");
            let bar = String::from("bar");
            let foo1 = int.intern(&foo);
            let bar1 = int.intern(&bar);
            let foo2 = int.intern(&foo);
            let bar2 = int.intern(&bar);
            (foo1, bar1, foo2, bar2)
        };
        assert_eq!(foo1, foo2);
        assert_eq!(bar1, bar2);
        assert_ne!(foo1, bar2);
        assert_ne!(foo2, bar1);

        assert_eq!(int.get(&foo1), "foo");
        assert_eq!(int.get(&foo2), "foo");
        assert_eq!(int.get(&bar1), "bar");
        assert_eq!(int.get(&bar2), "bar");

        assert!(ptr::eq(int.get(&foo1), int.get(&foo2)));
        assert!(ptr::eq(int.get(&bar1), int.get(&bar2)));
        assert!(!ptr::eq(int.get(&foo1), int.get(&bar1)));
        assert!(!ptr::eq(int.get(&foo1), "foo"));
    }
}
