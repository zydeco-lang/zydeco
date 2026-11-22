//! Undo-trail scoped maps and sets for lexical and backtracking checker state.

use rustc_hash::FxHashMap;
use std::hash::Hash;

/// A mutable hash map supporting O(1) lookup and O(changes) rollback to a saved watermark.
#[derive(Clone, Debug)]
pub struct ScopedMap<K: Eq + Hash + Clone, V> {
    map: FxHashMap<K, V>,
    trail: Vec<(K, Option<V>)>,
}

impl<K: Eq + Hash + Clone, V> Default for ScopedMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash + Clone, V> ScopedMap<K, V> {
    pub fn new() -> Self {
        Self { map: FxHashMap::default(), trail: Vec::new() }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    #[inline]
    pub fn watermark(&self) -> usize {
        self.trail.len()
    }

    #[inline]
    pub fn get(&self, key: &K) -> Option<&V> {
        self.map.get(key)
    }

    #[inline]
    pub fn contains_key(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }

    #[inline]
    pub fn insert(&mut self, key: K, value: V) {
        let previous = self.map.insert(key.clone(), value);
        self.trail.push((key, previous));
    }

    #[inline]
    pub fn remove(&mut self, key: &K) {
        if let Some(previous) = self.map.remove(key) {
            self.trail.push((key.clone(), Some(previous)));
        }
    }

    #[inline]
    pub fn rollback(&mut self, watermark: usize) {
        while self.trail.len() > watermark {
            let (key, previous) = self.trail.pop().expect("trail length checked above");
            match previous {
                | Some(value) => {
                    self.map.insert(key, value);
                }
                | None => {
                    self.map.remove(&key);
                }
            }
        }
    }
}

/// A mutable hash set with undo-trail rollback, built on [`ScopedMap`].
#[derive(Clone, Debug)]
pub struct ScopedSet<K: Eq + Hash + Clone>(ScopedMap<K, ()>);

impl<K: Eq + Hash + Clone> Default for ScopedSet<K> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash + Clone> ScopedSet<K> {
    pub fn new() -> Self {
        Self(ScopedMap::new())
    }

    #[inline]
    pub fn watermark(&self) -> usize {
        self.0.watermark()
    }

    #[inline]
    pub fn insert(&mut self, key: K) {
        if !self.0.contains_key(&key) {
            self.0.insert(key, ());
        }
    }

    #[inline]
    pub fn remove(&mut self, key: &K) {
        self.0.remove(key);
    }

    #[inline]
    pub fn contains(&self, key: &K) -> bool {
        self.0.contains_key(key)
    }

    #[inline]
    pub fn rollback(&mut self, watermark: usize) {
        self.0.rollback(watermark);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scoped_map_supports_nested_shadowing_removal_and_rollback() {
        let mut map = ScopedMap::new();
        map.insert("a", 1);
        let mark1 = map.watermark();
        map.insert("a", 2);
        map.insert("b", 3);
        let mark2 = map.watermark();
        map.remove(&"a");
        map.insert("c", 4);
        assert_eq!(map.get(&"a"), None);
        assert_eq!(map.get(&"b"), Some(&3));
        assert_eq!(map.get(&"c"), Some(&4));

        map.rollback(mark2);
        assert_eq!(map.get(&"a"), Some(&2));
        assert_eq!(map.get(&"b"), Some(&3));
        assert_eq!(map.get(&"c"), None);

        map.rollback(mark1);
        assert_eq!(map.get(&"a"), Some(&1));
        assert_eq!(map.get(&"b"), None);
        assert_eq!(map.len(), 1);
    }
}
