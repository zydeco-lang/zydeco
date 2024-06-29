use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

/* ---------------------------------- Index --------------------------------- */

pub use crate::new_key_type;

pub unsafe trait IndexLike: Clone + Copy + Eq + std::hash::Hash {
    type Meta;
    fn new(meta: Self::Meta, idx: usize) -> Self;
    fn index(&self) -> usize;
}

#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct TrivId<Meta = usize>(std::marker::PhantomData<Meta>);
unsafe impl<Meta: Eq + std::hash::Hash + std::marker::Copy> IndexLike for TrivId<Meta> {
    type Meta = Meta;
    fn new(_: Self::Meta, _idx: usize) -> Self {
        TrivId(Default::default())
    }
    fn index(&self) -> usize {
        unreachable!()
    }
}

/* -------------------------------- Allocator ------------------------------- */

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

pub struct GlobalAlloc(IndexAlloc<()>);
impl GlobalAlloc {
    pub fn new() -> Self {
        GlobalAlloc(IndexAlloc((), 0))
    }
    pub fn alloc(&mut self) -> IndexAlloc<usize> {
        IndexAlloc(self.0.next().unwrap().1, 0)
    }
}

/* ---------------------------------- Arena --------------------------------- */

pub trait ArenaAccess<Id, T, Meta>: Index<Id, Output = T> + IndexMut<Id, Output = T> {
    fn get(&self, id: Id) -> Option<&T>;
    fn get_mut(&mut self, id: Id) -> Option<&mut T>;
}

// pub trait ArenaBidirectional<'t, P, Q> {
//     type RightView<'a: 't>;
//     type LeftView<'b: 't>;
//     fn forth(&self, p: &P) -> Self::RightView<'t>;
//     fn back(&self, q: &Q) -> Self::LeftView<'t>;
// }

pub struct Forth<'a, T>(pub &'a T);
pub struct Back<'a, T>(pub &'a T);

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

/// A bidirectional single-to-multi-map; a "widen" map.
#[derive(Debug, Clone)]
pub struct ArenaForth<P, Q> {
    forward: ArenaAssoc<P, Vec<Q>>,
    backward: ArenaAssoc<Q, P>,
}

/// A bidirectional multi-to-single-map; a "narrowing" map.
#[derive(Debug, Clone)]
pub struct ArenaBack<P, Q> {
    forward: ArenaAssoc<P, Q>,
    backward: ArenaAssoc<Q, Vec<P>>,
}

/// A bidirectional bijective map.
#[derive(Debug, Clone)]
pub struct ArenaBijective<P, Q> {
    forward: ArenaAssoc<P, Q>,
    backward: ArenaAssoc<Q, P>,
}

/// A bidirectional multi-map.
#[derive(Debug, Clone)]
pub struct ArenaBipartite<P, Q> {
    forward: ArenaAssoc<P, Vec<Q>>,
    backward: ArenaAssoc<Q, Vec<P>>,
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

    impl<Id, T, Meta> ArenaSparse<Id, T, Meta>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        pub fn map_id<U>(self, f: impl Fn(Id) -> U) -> ArenaSparse<Id, U, Meta> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().map(|(id, _val)| (id, f(id))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn map_value<U>(self, f: impl Fn(T) -> U) -> ArenaSparse<Id, U, Meta> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().map(|(id, val)| (id, f(val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn map<U>(self, f: impl Fn(Id, T) -> U) -> ArenaSparse<Id, U, Meta> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().map(|(id, val)| (id, f(id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn filter_map_id<U>(self, f: impl Fn(Id) -> Option<U>) -> ArenaSparse<Id, U, Meta> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().filter_map(|(id, _val)| f(id).map(|val| (id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn filter_map_id_mut<U>(
            self, mut f: impl FnMut(Id) -> Option<U>,
        ) -> ArenaSparse<Id, U, Meta> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().filter_map(|(id, _val)| f(id).map(|val| (id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn filter_map_value<U>(self, f: impl Fn(T) -> Option<U>) -> ArenaSparse<Id, U, Meta> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().filter_map(|(id, val)| f(val).map(|val| (id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn filter_map<U>(self, f: impl Fn(Id, T) -> Option<U>) -> ArenaSparse<Id, U, Meta> {
            let Self { allocator, map, _marker } = self;
            let map =
                map.into_iter().filter_map(|(id, val)| f(id, val).map(|val| (id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn len(&self) -> usize {
            self.map.len()
        }
        pub fn replace(&mut self, id: Id, val: T) {
            let Some(_) = self.map.insert(id, val) else { panic!("key not found") };
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
            let None = self.map.insert(id, val) else { return };
            // let None = self.map.insert(id, val) else { panic!("duplicate key") };
        }
        pub fn replace(&mut self, id: Id, val: T) -> T {
            let Some(val) = self.map.insert(id, val) else { panic!("key not found") };
            val
        }
        pub fn insert_or_else<E>(
            &mut self, id: Id, val: T, f: impl FnOnce(T, T) -> Result<T, E>,
        ) -> Result<(), E> {
            match self.map.remove(&id) {
                | Some(old) => {
                    self.map.insert(id, f(old, val)?);
                }
                | None => {
                    self.map.insert(id, val);
                }
            }
            Ok(())
        }
        #[must_use]
        pub fn insert_or_replace(&mut self, id: Id, val: T) -> Option<T> {
            self.map.insert(id, val)
        }
        pub fn remove(&mut self, id: &Id) -> Option<T> {
            self.map.remove(id)
        }
    }

    impl<Id, T> ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
        T: Clone,
    {
        pub fn insert_or_get(&mut self, id: Id, val: T) -> Option<T> {
            if let Some(val) = self.map.get(&id) {
                Some(val.clone())
            } else {
                let None = self.map.insert(id, val) else { unreachable!() };
                None
            }
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

    impl<Id, T> ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
        pub fn len(&self) -> usize {
            self.map.len()
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

    impl<'a, P, Q> Index<&P> for Forth<'a, ArenaForth<P, Q>>
    where
        P: Eq + Hash + Clone,
    {
        type Output = [Q];
        fn index(&self, p: &P) -> &Self::Output {
            let Forth(arena) = self;
            arena.forth(p)
        }
    }

    impl<'a, P, Q> Index<&Q> for Back<'a, ArenaForth<P, Q>>
    where
        Q: Eq + Hash + Clone,
    {
        type Output = P;
        fn index(&self, q: &Q) -> &Self::Output {
            let Back(arena) = self;
            arena.back(q).unwrap()
        }
    }

    impl<P, Q> ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        pub fn forth(&self, p: &P) -> &[Q] {
            self.forward.get(p).map(|q| q.as_slice()).unwrap_or_default()
        }
    }

    impl<P, Q> ArenaForth<P, Q>
    where
        Q: Eq + Hash + Clone,
    {
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

    /* -------------------------------- ArenaBack ------------------------------- */

    impl<P, Q> ArenaBack<P, Q> {
        pub fn new() -> Self {
            ArenaBack { forward: ArenaAssoc::new(), backward: ArenaAssoc::new() }
        }
    }

    impl<P, Q> Default for ArenaBack<P, Q> {
        fn default() -> Self {
            Self { forward: Default::default(), backward: Default::default() }
        }
    }

    impl<P, Q> ArenaBack<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        pub fn insert(&mut self, p: P, q: Q) {
            self.forward.insert(p.clone(), q.clone());
            self.backward.map.entry(q).or_insert_with(Vec::new).push(p);
        }
    }

    impl<P, Q> Index<&P> for ArenaBack<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Output = Q;
        fn index(&self, p: &P) -> &Self::Output {
            &self.forward[p]
        }
    }

    impl<'a, P, Q> Index<&P> for Forth<'a, ArenaBack<P, Q>>
    where
        P: Eq + Hash + Clone,
    {
        type Output = Q;
        fn index(&self, p: &P) -> &Self::Output {
            let Forth(arena) = self;
            arena.forth(p)
        }
    }

    impl<'a, P, Q> Index<&Q> for Back<'a, ArenaBack<P, Q>>
    where
        Q: Eq + Hash + Clone,
    {
        type Output = [P];
        fn index(&self, q: &Q) -> &Self::Output {
            let Back(arena) = self;
            arena.back(q).unwrap()
        }
    }

    impl<P, Q> ArenaBack<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        pub fn forth(&self, p: &P) -> &Q {
            self.forward.get(p).unwrap()
        }
    }

    impl<P, Q> ArenaBack<P, Q>
    where
        Q: Eq + Hash + Clone,
    {
        pub fn back(&self, q: &Q) -> Option<&[P]> {
            self.backward.get(q).map(|p| p.as_slice())
        }
    }

    impl<P, Q> IntoIterator for ArenaBack<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Item = (P, Q);
        type IntoIter = std::collections::hash_map::IntoIter<P, Q>;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.into_iter()
        }
    }

    impl<'a, P, Q> IntoIterator for &'a ArenaBack<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Item = (&'a P, &'a Q);
        type IntoIter = std::collections::hash_map::Iter<'a, P, Q>;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.iter()
        }
    }

    impl<P, Q> Extend<(P, Q)> for ArenaBack<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn extend<I: IntoIterator<Item = (P, Q)>>(&mut self, iter: I) {
            for (p, q) in iter {
                self.insert(p, q);
            }
        }
    }

    impl<P, Q> AddAssign for ArenaBack<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn add_assign(&mut self, rhs: ArenaBack<P, Q>) {
            self.extend(rhs);
        }
    }

    /* ----------------------------- ArenaBijective ----------------------------- */

    impl<P, Q> ArenaBijective<P, Q> {
        pub fn new() -> Self {
            ArenaBijective { forward: ArenaAssoc::new(), backward: ArenaAssoc::new() }
        }
    }

    impl<P, Q> Default for ArenaBijective<P, Q> {
        fn default() -> Self {
            Self { forward: Default::default(), backward: Default::default() }
        }
    }

    impl<P, Q> ArenaBijective<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        pub fn insert(&mut self, p: P, q: Q) {
            self.forward.insert(p.clone(), q.clone());
            self.backward.insert(q, p);
        }
    }

    impl<P, Q> Index<&P> for ArenaBijective<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Output = Q;
        fn index(&self, p: &P) -> &Self::Output {
            self.forward.get(p).unwrap()
        }
    }

    impl<'a, P, Q> Index<&P> for Forth<'a, ArenaBijective<P, Q>>
    where
        P: Eq + Hash + Clone,
    {
        type Output = Q;
        fn index(&self, p: &P) -> &Self::Output {
            let Forth(arena) = self;
            arena.forth(p)
        }
    }

    impl<'a, P, Q> Index<&Q> for Back<'a, ArenaBijective<P, Q>>
    where
        Q: Eq + Hash + Clone,
    {
        type Output = P;
        fn index(&self, q: &Q) -> &Self::Output {
            let Back(arena) = self;
            arena.back(q).unwrap()
        }
    }

    impl<P, Q> ArenaBijective<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        pub fn forth(&self, p: &P) -> &Q {
            self.forward.get(p).unwrap()
        }
    }

    impl<P, Q> ArenaBijective<P, Q>
    where
        Q: Eq + Hash + Clone,
    {
        pub fn back(&self, q: &Q) -> Option<&P> {
            self.backward.get(q)
        }
    }

    impl<P, Q> IntoIterator for ArenaBijective<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Item = (P, Q);
        type IntoIter = std::collections::hash_map::IntoIter<P, Q>;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.into_iter()
        }
    }

    impl<'a, P, Q> IntoIterator for &'a ArenaBijective<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Item = (&'a P, &'a Q);
        type IntoIter = std::collections::hash_map::Iter<'a, P, Q>;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.iter()
        }
    }

    impl<P, Q> Extend<(P, Q)> for ArenaBijective<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn extend<I: IntoIterator<Item = (P, Q)>>(&mut self, iter: I) {
            for (p, q) in iter {
                self.insert(p, q);
            }
        }
    }

    impl<P, Q> AddAssign for ArenaBijective<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn add_assign(&mut self, rhs: ArenaBijective<P, Q>) {
            self.extend(rhs);
        }
    }

    /* ----------------------------- ArenaBipartite ----------------------------- */

    impl<P, Q> ArenaBipartite<P, Q> {
        pub fn new() -> Self {
            ArenaBipartite { forward: ArenaAssoc::new(), backward: ArenaAssoc::new() }
        }
    }

    impl<P, Q> Default for ArenaBipartite<P, Q> {
        fn default() -> Self {
            Self { forward: Default::default(), backward: Default::default() }
        }
    }

    impl<P, Q> ArenaBipartite<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        pub fn insert(&mut self, p: P, q: Q) {
            self.forward.map.entry(p.clone()).or_insert_with(Vec::new).push(q.clone());
            self.backward.map.entry(q).or_insert_with(Vec::new).push(p);
        }
    }

    impl<P, Q> Index<&P> for ArenaBipartite<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Output = [Q];
        fn index(&self, p: &P) -> &Self::Output {
            self.forward.get(p).map(|q| q.as_slice()).unwrap_or_default()
        }
    }

    impl<'a, P, Q> Index<&P> for Forth<'a, ArenaBipartite<P, Q>>
    where
        P: Eq + Hash + Clone,
    {
        type Output = [Q];
        fn index(&self, p: &P) -> &Self::Output {
            let Forth(arena) = self;
            arena.forth(p)
        }
    }

    impl<'a, P, Q> Index<&Q> for Back<'a, ArenaBipartite<P, Q>>
    where
        Q: Eq + Hash + Clone,
    {
        type Output = [P];
        fn index(&self, q: &Q) -> &Self::Output {
            let Back(arena) = self;
            arena.back(q)
        }
    }

    impl<P, Q> ArenaBipartite<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        pub fn forth(&self, p: &P) -> &[Q] {
            self.forward.get(p).map(|q| q.as_slice()).unwrap_or_default()
        }
    }

    impl<P, Q> ArenaBipartite<P, Q>
    where
        Q: Eq + Hash + Clone,
    {
        pub fn back(&self, q: &Q) -> &[P] {
            self.backward.get(q).map(|p| p.as_slice()).unwrap_or_default()
        }
    }

    impl<P, Q> IntoIterator for ArenaBipartite<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Item = (P, Vec<Q>);
        type IntoIter = std::collections::hash_map::IntoIter<P, Vec<Q>>;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.into_iter()
        }
    }

    impl<'a, P, Q> IntoIterator for &'a ArenaBipartite<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Item = (&'a P, &'a Vec<Q>);
        type IntoIter = std::collections::hash_map::Iter<'a, P, Vec<Q>>;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.iter()
        }
    }

    impl<P, Q> Extend<(P, Vec<Q>)> for ArenaBipartite<P, Q>
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

    impl<P, Q> AddAssign for ArenaBipartite<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn add_assign(&mut self, rhs: ArenaBipartite<P, Q>) {
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

        impl $name {
            pub fn concise(&self) -> String {
                format!("[{:?}#{:?}]", self.0, self.1)
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
