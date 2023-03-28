use std::{
    hash::Hash,
    ops::{Deref, DerefMut},
};

use crate::utils::fmt::FmtArgs;

#[derive(Clone, Debug)]
pub struct Env<K, V>
where
    K: Hash + Eq,
{
    inner: im::HashMap<K, V>,
}

impl<K, V> Deref for Env<K, V>
where
    K: Hash + Eq,
{
    type Target = im::HashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<K, V> DerefMut for Env<K, V>
where
    K: Hash + Eq,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<K, V> Default for Env<K, V>
where
    K: Hash + Eq,
{
    fn default() -> Self {
        Self { inner: im::HashMap::new() }
    }
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

impl<K, V> FromIterator<(K, V)> for Env<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Env { inner: iter.into_iter().collect() }
    }
}

impl<'a, K, V> IntoIterator for &'a Env<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    type Item = (&'a K, &'a V);
    type IntoIter = im::hashmap::Iter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a, K, V> IntoIterator for &'a mut Env<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    type Item = (&'a K, &'a mut V);
    type IntoIter = im::hashmap::IterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}

impl<K, V> IntoIterator for Env<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    type Item = (K, V);
    type IntoIter = im::hashmap::ConsumingIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<K, V> FmtArgs for Env<K, V>
where
    K: Eq + Hash + FmtArgs,
    V: FmtArgs,
{
    fn fmt_args(&self, fargs: crate::utils::fmt::Args) -> String {
        let mut s = String::new();
        s += &"=".repeat(20);
        s += &fargs.force_space();
        for (k, v) in self.inner.iter() {
            s += &format!("[{} := {}]", k.fmt_args(fargs), v.fmt_args(fargs));
            s += &fargs.force_space();
        }
        s += &"=".repeat(20);
        s
    }
}
