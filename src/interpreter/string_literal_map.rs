use std::{collections::{HashMap, hash_map::DefaultHasher}, hash::{BuildHasherDefault, Hasher, Hash}};

#[derive(Clone, Debug)]
pub struct NoHasher;

impl Default for NoHasher {
    fn default() -> Self {
        NoHasher
    }
}

impl Hasher for NoHasher {
    fn write(&mut self, _bytes: &[u8]) {
        // No action is taken; we're not actually hashing anything here.
    }

    fn finish(&self) -> u64 {
        0 // Return a constant hash value (0 in this case)
    }
}

pub type ManualHashMap<K, V> = HashMap<K, V, BuildHasherDefault<NoHasher>>;
pub type StringLiteralMap = ManualHashMap<u64, String>;

pub trait MapStringLiterals {
    fn add_lit(&mut self, s: String, hasher: &mut DefaultHasher) -> u64;

    fn get_lit(&self, h: &u64) -> &String;
}

impl MapStringLiterals for StringLiteralMap {
    fn add_lit(&mut self, s: String, hasher: &mut DefaultHasher) -> u64 {
        s.hash(hasher);
        let hash = hasher.finish();
        if !self.contains_key(&hash) {
            self.insert(hash, s);
        }
        hash
    }

    fn get_lit(&self, h: &u64) -> &String {
        self.get(h)
            .expect(&format!("string literal does not exist: {}", h))
    }
}