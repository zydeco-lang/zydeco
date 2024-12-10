//! Immutable collections.

use std::{borrow::Borrow, hash::Hash};

pub trait ImmutableMonoidMap<K, V> {
    fn new() -> Self;
    fn singleton(id: K, value: V) -> Self;
    fn get<BK>(&self, id: &BK) -> Option<&V>
    where
        BK: Hash + Eq + ?Sized,
        K: Borrow<BK>;
    fn extended(&self, iter: impl IntoIterator<Item = (K, V)>) -> Self;
}

// impl<'a, K, V, Map> ImmutableMonoidMap<K, V> for Map
// where
//     K: 'a + Clone + Hash + Eq,
//     V: 'a + Clone,
//     Map: From<im::HashMap<K, V>> + Clone + Deref<Target = &'a im::HashMap<K, V>>,
//     im::HashMap<K, V>: From<Map>,
// {
//     fn new() -> Self {
//         im::HashMap::new().into()
//     }
//     fn singleton(id: K, value: V) -> Self {
//         im::HashMap::unit(id, value).into()
//     }
//     fn get<BK>(&self, id: &BK) -> Option<&V>
//     where
//         BK: Hash + Eq + ?Sized,
//         K: Borrow<BK>,
//     {
//         (self.deref()).get(id)
//     }
//     fn extended(&self, iter: impl IntoIterator<Item = (K, V)>) -> Self {
//         let mut map: im::HashMap<_, _> = self.clone().into();
//         map.extend(iter);
//         map.into()
//     }
// }

// use std::ops::{Add, AddAssign, Index};
// impl<T> Context<T>
// where
//     T: Clone,
// {
//     pub fn new() -> Self {
//         Self { defs: im::HashMap::new() }
//     }
//     pub fn singleton(def: DefId, t: T) -> Self {
//         let mut defs = im::HashMap::new();
//         defs.insert(def, t);
//         Self { defs }
//     }
//     pub fn get(&self, def: &DefId) -> Option<&T> {
//         self.defs.get(def)
//     }
//     pub fn extended(&self, iter: impl IntoIterator<Item = (DefId, T)>) -> Self {
//         let Context { mut defs } = self.clone();
//         defs.extend(iter);
//         Self { defs }
//     }
// }
