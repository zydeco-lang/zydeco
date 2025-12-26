use std::{
    collections::HashMap,
    hash::Hash,
    ops::AddAssign,
    ops::{Index, IndexMut},
    sync::{Arc, Mutex},
};

/* ---------------------------------- Index --------------------------------- */

pub use crate::new_key_type;

pub unsafe trait IndexLike: Clone + Copy + Eq + std::hash::Hash {
    type Meta: Clone;
    fn new(meta: Self::Meta, idx: usize) -> Self;
    fn index(&self) -> usize;
}

#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct TrivId<Meta = usize>(std::marker::PhantomData<Meta>);
unsafe impl<Meta: Eq + Hash + Copy> IndexLike for TrivId<Meta> {
    type Meta = Meta;
    fn new(_: Self::Meta, _idx: usize) -> Self {
        TrivId(Default::default())
    }
    fn index(&self) -> usize {
        unreachable!()
    }
}

/* -------------------------------- Allocator ------------------------------- */

#[derive(Clone, Debug)]
pub struct IndexAlloc<Meta>(Meta, usize);
impl IndexAlloc<()> {
    pub fn new() -> Self {
        IndexAlloc((), 0)
    }
}
impl<Meta: Copy> Iterator for IndexAlloc<Meta> {
    type Item = (Meta, usize);
    fn next(&mut self) -> Option<Self::Item> {
        let IndexAlloc(meta, idx) = self;
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

#[derive(Clone)]
pub struct ArcGlobalAlloc(Arc<Mutex<GlobalAlloc>>);
impl ArcGlobalAlloc {
    pub fn new() -> Self {
        ArcGlobalAlloc(Arc::new(Mutex::new(GlobalAlloc::new())))
    }
    pub fn alloc(&self) -> IndexAlloc<usize> {
        self.0.lock().unwrap().alloc()
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

/// A dense arena with a sequential allocator.
/// Conceptually, it owns all data stored in the arena, and data are densely allocated.
#[derive(Debug, Clone)]
pub struct ArenaDense<Id: IndexLike, T> {
    allocator: IndexAlloc<Id::Meta>,
    vec: Vec<T>,
    _marker: std::marker::PhantomData<Id>,
}

/// A sparse arena with a sparse allocator.
/// Conceptually, it owns all data stored in the arena, and data are sparsely allocated.
#[derive(Debug, Clone)]
pub struct ArenaSparse<Id: IndexLike, T> {
    allocator: IndexAlloc<Id::Meta>,
    map: HashMap<Id, T>,
    _marker: std::marker::PhantomData<Id>,
}

/// An arena that maps keys of externally-owned data to their properties.
/// Conceptually, it doesn't own the data, but it owns the properties bound to the data.
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

/// An arena of equivalence classes, designed for types, and structurally shared
/// `data` and `codata` definitions.
pub struct ArenaEquiv<Id, Definition, Query>
where
    Id: IndexLike,
{
    /// arena for definitions
    pub defs: ArenaDense<Id, Definition>,
    /// arena for query hashmap
    pub tbls: ArenaAssoc<Id, Query>,
    /// arena for equivalence classes
    pub eqs: ArenaAssoc<Query, Id>,
}
impl<Id, Definition, Query> ArenaEquiv<Id, Definition, Query>
where
    Id: IndexLike,
    Id::Meta: Copy,
    Query: Clone + Eq + std::hash::Hash,
{
    pub fn new_arc(alloc: IndexAlloc<Id::Meta>) -> Self {
        Self { defs: ArenaDense::new(alloc), tbls: ArenaAssoc::new(), eqs: ArenaAssoc::new() }
    }
    pub fn lookup_or_alloc(&mut self, def: Definition, query: Query) -> Id {
        if let Some(id) = self.eqs.get(&query) {
            // if the query is already registered, just return the id
            *id
        } else {
            // else, register the query
            let id = self.defs.alloc(def);
            self.tbls.insert(id, query.clone());
            self.eqs.insert(query, id);
            id
        }
    }
}

mod impls {
    use super::*;

    /* ------------------------------- ArenaDense ------------------------------- */

    impl<Id, T> Default for ArenaDense<Id, T>
    where
        Id: IndexLike,
        Id::Meta: Default,
    {
        fn default() -> Self {
            Self {
                allocator: IndexAlloc(Default::default(), 0),
                vec: Default::default(),
                _marker: Default::default(),
            }
        }
    }

    impl<Id, T> Index<&Id> for ArenaDense<Id, T>
    where
        Id: IndexLike,
        Id::Meta: Copy,
    {
        type Output = T;
        fn index(&self, id: &Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }
    impl<Id, T> IndexMut<&Id> for ArenaDense<Id, T>
    where
        Id: IndexLike,
        Id::Meta: Copy,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id, T> ArenaDense<Id, T>
    where
        Id: IndexLike,
        Id::Meta: Copy,
    {
        pub fn new(allocator: IndexAlloc<Id::Meta>) -> Self {
            ArenaDense { allocator, vec: Vec::new(), _marker: std::marker::PhantomData }
        }
        pub fn alloc(&mut self, val: T) -> Id {
            let id = self.allocator.next().unwrap();
            self.vec.push(val);
            IndexLike::new(id.0, id.1)
        }
        pub fn iter(&self) -> impl Iterator<Item = (Id, &T)> {
            self.into_iter()
        }
    }

    impl<Id, T> ArenaAccess<&Id, T, Id::Meta> for ArenaDense<Id, T>
    where
        Id: IndexLike,
        Id::Meta: Copy,
    {
        fn get(&self, id: &Id) -> Option<&T> {
            self.vec.get(id.index())
        }
        fn get_mut(&mut self, id: &Id) -> Option<&mut T> {
            self.vec.get_mut(id.index())
        }
    }

    impl<Id, T> IntoIterator for ArenaDense<Id, T>
    where
        Id: IndexLike,
    {
        type Item = (Id, T);
        type IntoIter = <Vec<(Id, T)> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.vec
                .into_iter()
                .enumerate()
                .map(|(idx, val)| {
                    let id = IndexLike::new(self.allocator.0.clone(), idx);
                    (id, val)
                })
                .collect::<Vec<_>>()
                .into_iter()
        }
    }

    impl<'a, Id, T> IntoIterator for &'a ArenaDense<Id, T>
    where
        Id: IndexLike,
    {
        type Item = (Id, &'a T);
        type IntoIter = <Vec<(Id, &'a T)> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.vec
                .iter()
                .enumerate()
                .map(|(idx, val)| {
                    let id = IndexLike::new(self.allocator.0.clone(), idx);
                    (id, val)
                })
                .collect::<Vec<_>>()
                .into_iter()
        }
    }

    /* ------------------------------- ArenaSparse ------------------------------ */

    impl<Id, T> Default for ArenaSparse<Id, T>
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

    impl<Id, T, Meta> Index<&Id> for ArenaSparse<Id, T>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        type Output = T;
        fn index(&self, id: &Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }

    impl<Id, T, Meta> IndexMut<&Id> for ArenaSparse<Id, T>
    where
        Meta: Copy,
        Id: IndexLike<Meta = Meta> + Eq + Hash,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id, T, Meta> ArenaSparse<Id, T>
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
        pub fn iter(&self) -> impl Iterator<Item = (&Id, &T)> {
            self.into_iter()
        }
    }

    impl<Id, T, Meta> ArenaAccess<&Id, T, Meta> for ArenaSparse<Id, T>
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

    // no FromIterator for ArenaSparse because it's designed to be allocated ground up

    impl<Id, T> IntoIterator for ArenaSparse<Id, T>
    where
        Id: IndexLike,
    {
        type Item = (Id, T);
        type IntoIter = <HashMap<Id, T> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.map.into_iter()
        }
    }
    impl<'a, Id, T> IntoIterator for &'a ArenaSparse<Id, T>
    where
        Id: IndexLike,
    {
        type Item = (&'a Id, &'a T);
        type IntoIter = <&'a HashMap<Id, T> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.map.iter()
        }
    }

    impl<Id, T> Extend<(Id, T)> for ArenaSparse<Id, T>
    where
        Id: IndexLike,
    {
        fn extend<I: IntoIterator<Item = (Id, T)>>(&mut self, iter: I) {
            self.map.extend(iter);
        }
    }

    impl<Id, T> AddAssign for ArenaSparse<Id, T>
    where
        Id: IndexLike,
    {
        fn add_assign(&mut self, rhs: ArenaSparse<Id, T>) {
            self.extend(rhs);
        }
    }

    impl<Id, T> ArenaSparse<Id, T>
    where
        Id: IndexLike,
    {
        pub fn map_id<U>(self, f: impl Fn(Id) -> U) -> ArenaSparse<Id, U> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_keys().map(|id| (id, f(id))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn map_value<U>(self, f: impl Fn(T) -> U) -> ArenaSparse<Id, U> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().map(|(id, val)| (id, f(val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn map<U>(self, f: impl Fn(Id, T) -> U) -> ArenaSparse<Id, U> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().map(|(id, val)| (id, f(id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn filter_map_id<U>(self, f: impl Fn(Id) -> Option<U>) -> ArenaSparse<Id, U> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().filter_map(|(id, _val)| f(id).map(|val| (id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn filter_map_id_mut<U>(
            self, mut f: impl FnMut(Id) -> Option<U>,
        ) -> ArenaSparse<Id, U> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().filter_map(|(id, _val)| f(id).map(|val| (id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn filter_map_value<U>(self, f: impl Fn(T) -> Option<U>) -> ArenaSparse<Id, U> {
            let Self { allocator, map, _marker } = self;
            let map = map.into_iter().filter_map(|(id, val)| f(val).map(|val| (id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn filter_map<U>(self, f: impl Fn(Id, T) -> Option<U>) -> ArenaSparse<Id, U> {
            let Self { allocator, map, _marker } = self;
            let map =
                map.into_iter().filter_map(|(id, val)| f(id, val).map(|val| (id, val))).collect();
            ArenaSparse { allocator, map, _marker }
        }
        pub fn len(&self) -> usize {
            self.map.len()
        }
        pub fn replace(&mut self, id: Id, val: impl Into<T>) {
            let Some(_) = self.map.insert(id, val.into()) else { panic!("key not found") };
        }
    }

    /* ------------------------------- ArenaAssoc ------------------------------- */

    impl<Id, T> ArenaAssoc<Id, T> {
        pub fn new() -> Self {
            ArenaAssoc { map: HashMap::new() }
        }
        pub fn iter(&self) -> impl Iterator<Item = (&Id, &T)> {
            self.into_iter()
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
        pub fn entry(&mut self, id: Id) -> std::collections::hash_map::Entry<'_, Id, T> {
            self.map.entry(id)
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
        pub fn insert_or_get(&mut self, id: Id, val: T) -> Option<T>
        where
            T: Clone,
        {
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

    impl<Id, T> ArenaAccess<&Id, T, ()> for ArenaAssoc<Id, T>
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

    impl<Id, T> FromIterator<(Id, T)> for ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
        fn from_iter<I: IntoIterator<Item = (Id, T)>>(iter: I) -> Self {
            let mut arena = Self::new();
            arena.extend(iter);
            arena
        }
    }

    impl<Id, T> IntoIterator for ArenaAssoc<Id, T> {
        type Item = (Id, T);
        type IntoIter = <HashMap<Id, T> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.map.into_iter()
        }
    }
    impl<'a, Id, T> IntoIterator for &'a ArenaAssoc<Id, T> {
        type Item = (&'a Id, &'a T);
        type IntoIter = <&'a HashMap<Id, T> as IntoIterator>::IntoIter;
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
        pub fn iter(&self) -> impl Iterator<Item = (&P, &Vec<Q>)> {
            self.into_iter()
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

    impl<P, Q> Index<&P> for Forth<'_, ArenaForth<P, Q>>
    where
        P: Eq + Hash + Clone,
    {
        type Output = [Q];
        fn index(&self, p: &P) -> &Self::Output {
            let Forth(arena) = self;
            arena.forth(p)
        }
    }

    impl<P, Q> Index<&Q> for Back<'_, ArenaForth<P, Q>>
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
        pub fn try_back(&self, q: &Q) -> Option<&P> {
            self.backward.get(q)
        }
    }

    impl<P, Q> FromIterator<(P, Vec<Q>)> for ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn from_iter<I: IntoIterator<Item = (P, Vec<Q>)>>(iter: I) -> Self {
            let mut arena = Self::new();
            arena.extend(iter);
            arena
        }
    }

    impl<P, Q> IntoIterator for ArenaForth<P, Q> {
        type Item = (P, Vec<Q>);
        type IntoIter = <HashMap<P, Vec<Q>> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.into_iter()
        }
    }

    impl<'a, P, Q> IntoIterator for &'a ArenaForth<P, Q> {
        type Item = (&'a P, &'a Vec<Q>);
        type IntoIter = <&'a HashMap<P, Vec<Q>> as IntoIterator>::IntoIter;
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
        pub fn iter(&self) -> impl Iterator<Item = (&P, &Q)> {
            self.into_iter()
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

    impl<P, Q> Index<&P> for Forth<'_, ArenaBack<P, Q>>
    where
        P: Eq + Hash + Clone,
    {
        type Output = Q;
        fn index(&self, p: &P) -> &Self::Output {
            let Forth(arena) = self;
            arena.forth(p)
        }
    }

    impl<P, Q> Index<&Q> for Back<'_, ArenaBack<P, Q>>
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
        pub fn try_forth(&self, p: &P) -> Option<&Q> {
            self.forward.get(p)
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

    impl<P, Q> FromIterator<(P, Q)> for ArenaBack<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn from_iter<I: IntoIterator<Item = (P, Q)>>(iter: I) -> Self {
            let mut arena = Self::new();
            arena.extend(iter);
            arena
        }
    }

    impl<P, Q> IntoIterator for ArenaBack<P, Q> {
        type Item = (P, Q);
        type IntoIter = <HashMap<P, Q> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.into_iter()
        }
    }

    impl<'a, P, Q> IntoIterator for &'a ArenaBack<P, Q> {
        type Item = (&'a P, &'a Q);
        type IntoIter = <&'a HashMap<P, Q> as IntoIterator>::IntoIter;
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
        pub fn iter(&self) -> impl Iterator<Item = (&P, &Q)> {
            self.into_iter()
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

    impl<P, Q> Index<&P> for Forth<'_, ArenaBijective<P, Q>>
    where
        P: Eq + Hash + Clone,
    {
        type Output = Q;
        fn index(&self, p: &P) -> &Self::Output {
            let Forth(arena) = self;
            arena.forth(p)
        }
    }

    impl<P, Q> Index<&Q> for Back<'_, ArenaBijective<P, Q>>
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
        pub fn try_forth(&self, p: &P) -> Option<&Q> {
            self.forward.get(p)
        }
    }

    impl<P, Q> ArenaBijective<P, Q>
    where
        Q: Eq + Hash + Clone,
    {
        pub fn back(&self, q: &Q) -> Option<&P> {
            self.backward.get(q)
        }
        pub fn try_back(&self, q: &Q) -> Option<&P> {
            self.backward.get(q)
        }
    }

    impl<P, Q> FromIterator<(P, Q)> for ArenaBijective<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn from_iter<I: IntoIterator<Item = (P, Q)>>(iter: I) -> Self {
            let mut arena = Self::new();
            arena.extend(iter);
            arena
        }
    }

    impl<P, Q> IntoIterator for ArenaBijective<P, Q> {
        type Item = (P, Q);
        type IntoIter = <HashMap<P, Q> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.into_iter()
        }
    }

    impl<'a, P, Q> IntoIterator for &'a ArenaBijective<P, Q> {
        type Item = (&'a P, &'a Q);
        type IntoIter = <&'a HashMap<P, Q> as IntoIterator>::IntoIter;
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
        pub fn iter(&self) -> impl Iterator<Item = (&P, &Vec<Q>)> {
            self.into_iter()
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

    impl<P, Q> Index<&P> for Forth<'_, ArenaBipartite<P, Q>>
    where
        P: Eq + Hash + Clone,
    {
        type Output = [Q];
        fn index(&self, p: &P) -> &Self::Output {
            let Forth(arena) = self;
            arena.forth(p)
        }
    }

    impl<P, Q> Index<&Q> for Back<'_, ArenaBipartite<P, Q>>
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

    impl<P, Q> IntoIterator for ArenaBipartite<P, Q> {
        type Item = (P, Vec<Q>);
        type IntoIter = <HashMap<P, Vec<Q>> as IntoIterator>::IntoIter;
        fn into_iter(self) -> Self::IntoIter {
            self.forward.map.into_iter()
        }
    }

    impl<'a, P, Q> IntoIterator for &'a ArenaBipartite<P, Q> {
        type Item = (&'a P, &'a Vec<Q>);
        type IntoIter = <&'a HashMap<P, Vec<Q>> as IntoIterator>::IntoIter;
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
            pub fn concise_inner(&self) -> String {
                format!("{:?}#{:?}", self.0, self.1)
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
