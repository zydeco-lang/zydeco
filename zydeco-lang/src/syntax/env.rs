use std::hash::Hash;

#[derive(Clone, Debug)]
pub struct Env<K, V>
where
    K: Hash + Eq,
{
    inner: im::HashMap<K, V>,
}

impl<K, V> Env<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Env { inner: im::HashMap::new() }
    }

    #[inline]
    #[must_use]
    pub fn update(&self, key: K, value: V) -> Self {
        Env { inner: self.inner.update(key, value) }
    }

    #[must_use]
    pub fn lookup(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }
}
