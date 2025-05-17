//! Immutable collections.

use std::{borrow::Borrow, hash::Hash};

pub trait ImmutableMonoidMap<K, V>:
    Clone + From<im::HashMap<K, V>> + Into<im::HashMap<K, V>> + AsRef<im::HashMap<K, V>>
{
    fn new() -> Self;
    fn singleton(id: K, value: V) -> Self;
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

#[derive(Clone, Debug, Default)]
#[repr(transparent)]
pub struct ImMap<K, V>(im::HashMap<K, V>)
where
    K: Hash + Eq;

mod impls_im_map {
    use crate::monoid::Monoid;

    use super::*;
    use std::ops::{Add, AddAssign, Index};

    impl<K, V> From<im::HashMap<K, V>> for ImMap<K, V>
    where
        K: Hash + Eq,
    {
        fn from(map: im::HashMap<K, V>) -> Self {
            Self(map)
        }
    }

    impl<K, V> Into<im::HashMap<K, V>> for ImMap<K, V>
    where
        K: Hash + Eq,
    {
        fn into(self) -> im::HashMap<K, V> {
            self.0
        }
    }

    impl<K, V> AsRef<im::HashMap<K, V>> for ImMap<K, V>
    where
        K: Hash + Eq,
    {
        fn as_ref(&self) -> &im::HashMap<K, V> {
            &self.0
        }
    }

    impl<K, V> Monoid for ImMap<K, V>
    where
        K: Clone + Hash + Eq + Default,
        V: Clone + Default,
    {
    }

    impl<K, V> Add for ImMap<K, V>
    where
        K: Clone + Hash + Eq,
        V: Clone,
    {
        type Output = Self;
        fn add(self, other: Self) -> Self {
            let mut map: im::HashMap<_, _> = self.0;
            map.extend(other.0);
            Self(map)
        }
    }
    impl<K, V> Add<(K, V)> for ImMap<K, V>
    where
        K: Clone + Hash + Eq,
        V: Clone,
    {
        type Output = Self;
        fn add(self, (k, v): (K, V)) -> Self {
            let mut map: im::HashMap<_, _> = self.0;
            map.insert(k, v);
            Self(map)
        }
    }
    impl<K, V> AddAssign<(K, V)> for ImMap<K, V>
    where
        K: Clone + Hash + Eq,
        V: Clone,
    {
        fn add_assign(&mut self, (k, v): (K, V)) {
            *self = self.clone() + (k, v);
        }
    }
    impl<K, V> Index<&K> for ImMap<K, V>
    where
        K: Clone + Hash + Eq,
        V: Clone,
    {
        type Output = V;
        fn index(&self, k: &K) -> &V {
            &self.0[k]
        }
    }
}
