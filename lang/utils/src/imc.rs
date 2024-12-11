//! Immutable collections.

use std::{
    borrow::Borrow,
    hash::Hash,
    // ops::{Add, AddAssign, Index},
};

pub trait ImmutableMonoidMap<K, V> {
    fn new() -> Self;
    fn singleton(id: K, value: V) -> Self;
    // fn get<BK>(&self, id: &BK) -> Option<&V>
    fn get<'b, BK>(&'b self, id: &'b BK) -> Option<&'b V>
    where
        BK: Hash + Eq + ?Sized,
        K: 'b + Borrow<BK>;
    fn extended(&self, iter: impl IntoIterator<Item = (K, V)>) -> Self;
}

impl<K, V, Map> ImmutableMonoidMap<K, V> for Map
where
    K: Clone + Hash + Eq,
    V: Clone,
    Map: Clone + From<im::HashMap<K, V>> + Into<im::HashMap<K, V>> + AsRef<im::HashMap<K, V>>,
{
    fn new() -> Self {
        im::HashMap::new().into()
    }
    fn singleton(id: K, value: V) -> Self {
        im::HashMap::unit(id, value).into()
    }
    fn get<'b, BK>(&'b self, id: &'b BK) -> Option<&'b V>
    where
        BK: Hash + Eq + ?Sized,
        K: 'b + Borrow<BK>,
    {
        (self.as_ref()).get(id)
    }
    fn extended(&self, iter: impl IntoIterator<Item = (K, V)>) -> Self {
        let mut map: im::HashMap<_, _> = self.clone().into();
        map.extend(iter);
        map.into()
    }
}
