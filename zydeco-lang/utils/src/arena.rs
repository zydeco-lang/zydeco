use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

pub use crate::new_key_type;

#[derive(Debug)]
pub struct IndexAlloc<Meta>(Meta, usize);
impl IndexAlloc<()> {
    pub fn new() -> Self {
        IndexAlloc((), 0)
    }
}
impl<Meta: Copy> Iterator for IndexAlloc<Meta> {
    type Item = (Meta, usize);
    fn next(&mut self) -> Option<Self::Item> {
        let IndexAlloc(meta, ref mut idx) = self;
        let old = *idx;
        *idx += 1;
        Some((*meta, old))
    }
}

pub unsafe trait IndexLike: Clone + Copy + Eq + std::hash::Hash {
    type Meta;
    fn new(meta: Self::Meta, idx: usize) -> Self;
    fn index(&self) -> usize;
}

#[derive(Debug)]
pub struct ArenaDense<Id, T, Meta = usize> {
    allocator: IndexAlloc<Meta>,
    vec: Vec<T>,
    _marker: std::marker::PhantomData<Id>,
}

#[derive(Debug)]
pub struct ArenaSparse<Id, T, Meta = usize> {
    allocator: IndexAlloc<Meta>,
    map: HashMap<Id, T>,
    _marker: std::marker::PhantomData<Id>,
}

#[derive(Debug, Clone)]
pub struct ArenaAssoc<Id, T> {
    map: HashMap<Id, T>,
}

#[derive(Debug, Clone)]
pub struct ArenaForth<P, Q> {
    forward: ArenaAssoc<P, Vec<Q>>,
    backward: ArenaAssoc<Q, P>,
}

pub trait ArenaAccess<Id, T, Meta>: Index<Id, Output = T> + IndexMut<Id, Output = T> {
    fn get(&self, id: Id) -> Option<&T>;
    fn get_mut(&mut self, id: Id) -> Option<&mut T>;
}

pub struct GlobalAlloc(IndexAlloc<()>);
impl GlobalAlloc {
    pub fn new() -> Self {
        GlobalAlloc(IndexAlloc((), 0))
    }
    pub fn alloc(&mut self) -> IndexAlloc<usize> {
        IndexAlloc(self.0.next().unwrap().1, 0)
    }
}

mod impls {
    use super::*;
    use std::{hash::Hash, ops::AddAssign};

    /* ------------------------------- ArenaDense ------------------------------- */

    impl<Id, T> Default for ArenaDense<Id, T, ()>
    where
        Id: IndexLike<Meta = ()>,
    {
        fn default() -> Self {
            Self {
                allocator: IndexAlloc((), 0),
                vec: Default::default(),
                _marker: Default::default(),
            }
        }
    }

    impl<Id, T, Meta> Index<&Id> for ArenaDense<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta>,
    {
        type Output = T;
        fn index(&self, id: &Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }
    impl<Id, T, Meta> IndexMut<&Id> for ArenaDense<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta>,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id, T, Meta> ArenaDense<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta>,
    {
        pub fn new(allocator: IndexAlloc<Meta>) -> Self {
            ArenaDense { allocator, vec: Vec::new(), _marker: std::marker::PhantomData }
        }
        pub fn alloc(&mut self, val: T) -> Id {
            let id = self.allocator.next().unwrap();
            self.vec.push(val);
            IndexLike::new(id.0, id.1)
        }
    }

    impl<Id, T, Meta> ArenaAccess<&Id, T, Meta> for ArenaDense<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta>,
    {
        fn get(&self, id: &Id) -> Option<&T> {
            self.vec.get(id.index())
        }
        fn get_mut(&mut self, id: &Id) -> Option<&mut T> {
            self.vec.get_mut(id.index())
        }
    }

    /* ------------------------------- ArenaSparse ------------------------------ */

    impl<Id, T> Default for ArenaSparse<Id, T, ()>
    where
        Id: IndexLike<Meta = ()> + Eq + Hash,
    {
        fn default() -> Self {
            Self {
                allocator: IndexAlloc((), 0),
                map: Default::default(),
                _marker: Default::default(),
            }
        }
    }

    impl<Id, T, Meta> Index<&Id> for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        type Output = T;
        fn index(&self, id: &Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }

    impl<Id, T, Meta> IndexMut<&Id> for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id, T, Meta> ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        pub fn new(allocator: IndexAlloc<Meta>) -> Self {
            ArenaSparse { allocator, map: HashMap::new(), _marker: std::marker::PhantomData }
        }
        pub fn alloc(&mut self, val: T) -> Id {
            let id = self.allocator.next().unwrap();
            let id = IndexLike::new(id.0, id.1);
            self.map.insert(id, val);
            id
        }
    }

    impl<Id, T, Meta> ArenaAccess<&Id, T, Meta> for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        fn get(&self, id: &Id) -> Option<&T> {
            self.map.get(&id)
        }
        fn get_mut(&mut self, id: &Id) -> Option<&mut T> {
            self.map.get_mut(&id)
        }
    }

    impl<Id, T, Meta> IntoIterator for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        type Item = (Id, T);
        type IntoIter = std::collections::hash_map::IntoIter<Id, T>;
        fn into_iter(self) -> Self::IntoIter {
            self.map.into_iter()
        }
    }
    impl<'a, Id, T, Meta> IntoIterator for &'a ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        type Item = (&'a Id, &'a T);
        type IntoIter = std::collections::hash_map::Iter<'a, Id, T>;
        fn into_iter(self) -> Self::IntoIter {
            self.map.iter()
        }
    }

    impl<Id, T, Meta> Extend<(Id, T)> for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        fn extend<I: IntoIterator<Item = (Id, T)>>(&mut self, iter: I) {
            self.map.extend(iter);
        }
    }

    impl<Id, T, Meta> AddAssign for ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        fn add_assign(&mut self, rhs: ArenaSparse<Id, T, Meta>) {
            self.extend(rhs);
        }
    }

    /* ------------------------------- ArenaAssoc ------------------------------- */

    impl<Id, T> ArenaAssoc<Id, T> {
        pub fn new() -> Self {
            ArenaAssoc { map: HashMap::new() }
        }
    }

    impl<Id, T> Default for ArenaAssoc<Id, T> {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<Id, T> ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
        pub fn insert(&mut self, id: Id, val: T) {
            let None = self.map.insert(id, val) else { panic!("duplicate key") };
        }
    }

    impl<Id, T> Index<&Id> for ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
        type Output = T;
        fn index(&self, id: &Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }

    impl<Id, T> IndexMut<&Id> for ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<'a, Id, T> ArenaAccess<&'a Id, T, ()> for ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
        fn get(&self, id: &Id) -> Option<&T> {
            self.map.get(&id)
        }
        fn get_mut(&mut self, id: &Id) -> Option<&mut T> {
            self.map.get_mut(&id)
        }
    }

    impl<Id, T> IntoIterator for ArenaAssoc<Id, T> {
        type Item = (Id, T);
        type IntoIter = std::collections::hash_map::IntoIter<Id, T>;
        fn into_iter(self) -> Self::IntoIter {
            self.map.into_iter()
        }
    }
    impl<'a, Id, T> IntoIterator for &'a ArenaAssoc<Id, T> {
        type Item = (&'a Id, &'a T);
        type IntoIter = std::collections::hash_map::Iter<'a, Id, T>;
        fn into_iter(self) -> Self::IntoIter {
            self.map.iter()
        }
    }

    impl<Id, T> Extend<(Id, T)> for ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
        fn extend<I: IntoIterator<Item = (Id, T)>>(&mut self, iter: I) {
            self.map.extend(iter);
        }
    }

    impl<Id, T> AddAssign for ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
        fn add_assign(&mut self, rhs: ArenaAssoc<Id, T>) {
            self.extend(rhs);
        }
    }

    /* ------------------------------- ArenaForth ------------------------------- */

    impl<P, Q> ArenaForth<P, Q> {
        pub fn new() -> Self {
            ArenaForth { forward: ArenaAssoc::new(), backward: ArenaAssoc::new() }
        }
    }

    impl<P, Q> Default for ArenaForth<P, Q> {
        fn default() -> Self {
            Self { forward: Default::default(), backward: Default::default() }
        }
    }

    impl<P, Q> ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        /// previous and qurrent
        pub fn insert(&mut self, prev: P, qurr: Q) {
            self.forward.map.entry(prev.clone()).or_insert_with(Vec::new).push(qurr.clone());
            self.backward.insert(qurr, prev);
        }
    }

    impl<P, Q> Index<&P> for ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Output = [Q];
        fn index(&self, p: &P) -> &Self::Output {
            self.forward.get(p).map(|q| q.as_slice()).unwrap_or_default()
        }
    }

    impl<P, Q> ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        pub fn forth(&self, p: &P) -> &[Q] {
            self.forward.get(p).map(|q| q.as_slice()).unwrap_or_default()
        }
        pub fn back(&self, q: &Q) -> Option<&P> {
            self.backward.get(q)
        }
    }

    impl<P, Q> IntoIterator for ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Item = (P, Vec<Q>);
        type IntoIter = std::collections::hash_map::IntoIter<P, Vec<Q>>;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.into_iter()
        }
    }

    impl<'a, P, Q> IntoIterator for &'a ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Item = (&'a P, &'a Vec<Q>);
        type IntoIter = std::collections::hash_map::Iter<'a, P, Vec<Q>>;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.iter()
        }
    }

    impl<P, Q> Extend<(P, Vec<Q>)> for ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn extend<I: IntoIterator<Item = (P, Vec<Q>)>>(&mut self, iter: I) {
            for (p, qs) in iter {
                for q in qs {
                    self.insert(p.clone(), q);
                }
            }
        }
    }

    impl<P, Q> AddAssign for ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn add_assign(&mut self, rhs: ArenaForth<P, Q>) {
            self.extend(rhs);
        }
    }
}

#[macro_export]
macro_rules! new_key_type {
    ( $(#[$outer:meta])* $vis:vis struct $name:ident < $meta:ty > ; $($rest:tt)* ) => {
        $(#[$outer])*
        #[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
        $vis struct $name($meta, usize);

        unsafe impl $crate::arena::IndexLike for $name {
            type Meta = $meta;
            fn new(meta: Self::Meta, idx: usize) -> Self {
                Self(meta, idx)
            }
            fn index(&self) -> usize {
                self.1
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:?}, {})", stringify!($name), self.0, self.1)
            }
        }

        $crate::new_key_type!($($rest)*);
    };

    // a nice default only for compiler use
    ( $(#[$outer:meta])* $vis:vis struct $name:ident ; $($rest:tt)* ) => {
        $crate::new_key_type!( $(#[$outer])* $vis struct $name<usize> ; $($rest)* );
    };

    () => {}
}
