use derive_more::{Index, IntoIterator};
pub use la_arena::RawIdx;
use la_arena::{Arena as LaArena, Idx as LaIdx};
use std::{
    collections::HashMap,
    hash::Hash,
    marker::PhantomData,
    ops::AddAssign,
    ops::{Index, IndexMut},
    sync::atomic::{AtomicU64, Ordering},
};

/* ---------------------------------- Index --------------------------------- */

pub use crate::new_key_type;

/// Construction capability kept private to arena implementations.
#[doc(hidden)]
pub struct ArenaIdToken(());

pub trait ArenaId: Copy + Eq + Hash {
    #[doc(hidden)]
    fn from_raw_parts(token: ArenaIdToken, key_space: KeySpaceId, raw: RawIdx) -> Self;
    fn key_space(self) -> KeySpaceId;
    fn raw(self) -> RawIdx;
}

/* -------------------------------- KeySpace -------------------------------- */

/// Process-unique identity domain for generated IDs.
#[derive(Copy, Clone, derive_more::Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[debug("{_0}")]
pub struct KeySpaceId(u64);

impl KeySpaceId {
    fn fresh() -> Self {
        static NEXT_KEY_SPACE_ID: AtomicU64 = AtomicU64::new(0);

        let id = NEXT_KEY_SPACE_ID
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |id| id.checked_add(1))
            .expect("key-space identity range exhausted");
        Self(id)
    }
}

/// Issues typed keys within one identity domain.
///
/// A key space deliberately cannot be cloned: cloning its cursor would allow two
/// owners to issue the same future key.
#[derive(Debug)]
pub struct KeySpace {
    id: KeySpaceId,
    next: u32,
}

impl KeySpace {
    /// Create an independent sequential ID issuer.
    ///
    /// Construction claims one identity atomically. Allocation after that only
    /// mutates this value's local cursor, so separate key spaces do not contend.
    pub fn new() -> Self {
        Self { id: KeySpaceId::fresh(), next: 0 }
    }
    pub fn alloc<Id>(&mut self) -> Id
    where
        Id: ArenaId,
    {
        let raw = self.next;
        self.next = self.next.checked_add(1).expect("key space exhausted its u32 index range");
        Id::from_raw_parts(ArenaIdToken(()), self.id, RawIdx::from_u32(raw))
    }
}

/* ---------------------------------- Arena --------------------------------- */

pub trait ArenaAccess<Id, T>: Index<Id, Output = T> + IndexMut<Id, Output = T> {
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

/// Dense owning storage whose raw indices are issued by `la-arena` itself.
/// It retains an identity tag, but no separate sequential [`KeySpace`] cursor.
#[derive(Debug)]
pub struct ArenaDense<Id: ArenaId, T> {
    key_space: KeySpaceId,
    arena: LaArena<T>,
    marker: PhantomData<fn() -> Id>,
}

/// Sparse owning storage for externally issued IDs.
/// Allocation belongs to the pass that owns the appropriate [`KeySpace`].
#[derive(Debug, Index, IntoIterator)]
pub struct ArenaSparse<Id: ArenaId, T> {
    #[into_iterator(owned, ref)]
    map: HashMap<Id, T>,
}

/// An arena that maps keys of externally-owned data to their properties.
/// Conceptually, it doesn't own the data, but it owns the properties bound to the data.
#[derive(Debug, Clone, Index, IntoIterator)]
pub struct ArenaAssoc<Id, T> {
    #[into_iterator(owned, ref)]
    map: HashMap<Id, T>,
}

/// A bidirectional single-to-multi-map; a "widen" map.
#[derive(Debug, Clone, IntoIterator)]
pub struct ArenaForth<P, Q> {
    #[into_iterator(owned, ref)]
    forward: ArenaAssoc<P, Vec<Q>>,
    backward: ArenaAssoc<Q, P>,
}

/// A bidirectional multi-to-single-map; a "narrowing" map.
#[derive(Debug, Clone, IntoIterator)]
pub struct ArenaBack<P, Q> {
    #[into_iterator(owned, ref)]
    forward: ArenaAssoc<P, Q>,
    backward: ArenaAssoc<Q, Vec<P>>,
}

/// A bidirectional bijective map.
#[derive(Debug, Clone, IntoIterator)]
pub struct ArenaBijective<P, Q> {
    #[into_iterator(owned, ref)]
    forward: ArenaAssoc<P, Q>,
    backward: ArenaAssoc<Q, P>,
}

/// A bidirectional multi-map.
#[derive(Debug, Clone, IntoIterator)]
pub struct ArenaBipartite<P, Q> {
    #[into_iterator(owned, ref)]
    forward: ArenaAssoc<P, Vec<Q>>,
    backward: ArenaAssoc<Q, Vec<P>>,
}

/// An arena of equivalence classes, designed for types, and structurally shared
/// `data` and `codata` definitions.
pub struct ArenaEquiv<Id, Definition, Query>
where
    Id: ArenaId,
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
    Id: ArenaId,
    Query: Clone + Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        Self { defs: ArenaDense::new(), tbls: ArenaAssoc::new(), eqs: ArenaAssoc::new() }
    }
    pub fn lookup_or_alloc(&mut self, def: Definition, query: Query) -> Id {
        if let Some(id) = self.eqs.get(&query) {
            // if the query is already registered, just return the id
            *id
        } else {
            // else, register the query
            let id = self.defs.alloc(def);
            self.tbls.insert_new(id, query.clone());
            self.eqs.insert_new(query, id);
            id
        }
    }
}

mod impls {
    use super::*;

    /* ------------------------------- ArenaDense ------------------------------- */

    impl<Id, T> Default for ArenaDense<Id, T>
    where
        Id: ArenaId,
    {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<Id, T> Index<&Id> for ArenaDense<Id, T>
    where
        Id: ArenaId,
    {
        type Output = T;
        fn index(&self, id: &Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }
    impl<Id, T> IndexMut<&Id> for ArenaDense<Id, T>
    where
        Id: ArenaId,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id, T> ArenaDense<Id, T>
    where
        Id: ArenaId,
    {
        pub fn new() -> Self {
            Self { key_space: KeySpaceId::fresh(), arena: LaArena::new(), marker: PhantomData }
        }
        pub fn alloc(&mut self, val: T) -> Id {
            let idx = self.arena.alloc(val);
            Id::from_raw_parts(ArenaIdToken(()), self.key_space, idx.into_raw())
        }
        pub fn iter(&self) -> impl Iterator<Item = (Id, &T)> {
            let key_space = self.key_space;
            self.arena.iter().map(move |(idx, val)| {
                (Id::from_raw_parts(ArenaIdToken(()), key_space, idx.into_raw()), val)
            })
        }
        pub fn len(&self) -> usize {
            self.arena.len()
        }
        fn index(&self, id: Id) -> Option<LaIdx<T>> {
            if id.key_space() != self.key_space {
                return None;
            }
            let idx = id.raw();
            ((idx.into_u32() as usize) < self.arena.len()).then(|| LaIdx::from_raw(idx))
        }
    }

    impl<Id, T> ArenaAccess<&Id, T> for ArenaDense<Id, T>
    where
        Id: ArenaId,
    {
        fn get(&self, id: &Id) -> Option<&T> {
            self.index(*id).map(|idx| &self.arena[idx])
        }
        fn get_mut(&mut self, id: &Id) -> Option<&mut T> {
            let idx = self.index(*id)?;
            Some(&mut self.arena[idx])
        }
    }

    pub struct ArenaDenseIntoIter<Id: ArenaId, T> {
        key_space: KeySpaceId,
        inner: la_arena::IntoIter<T>,
        marker: PhantomData<fn() -> Id>,
    }

    impl<Id: ArenaId, T> Iterator for ArenaDenseIntoIter<Id, T> {
        type Item = (Id, T);
        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|(idx, val)| {
                (Id::from_raw_parts(ArenaIdToken(()), self.key_space, idx.into_raw()), val)
            })
        }
    }

    impl<Id, T> IntoIterator for ArenaDense<Id, T>
    where
        Id: ArenaId,
    {
        type Item = (Id, T);
        type IntoIter = ArenaDenseIntoIter<Id, T>;
        fn into_iter(self) -> Self::IntoIter {
            ArenaDenseIntoIter {
                key_space: self.key_space,
                inner: self.arena.into_iter(),
                marker: PhantomData,
            }
        }
    }

    /* ------------------------------- ArenaSparse ------------------------------ */

    impl<Id, T> Default for ArenaSparse<Id, T>
    where
        Id: ArenaId,
    {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<Id, T> IndexMut<&Id> for ArenaSparse<Id, T>
    where
        Id: ArenaId,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id, T> ArenaSparse<Id, T>
    where
        Id: ArenaId,
    {
        pub fn new() -> Self {
            Self { map: HashMap::new() }
        }
        /// Insert a value whose externally-issued ID must not already be present.
        pub fn insert_new(&mut self, id: Id, val: T) {
            use std::collections::hash_map::Entry;
            match self.map.entry(id) {
                | Entry::Vacant(entry) => {
                    entry.insert(val);
                }
                | Entry::Occupied(_) => panic!("duplicate key in sparse arena"),
            }
        }
        pub fn iter(&self) -> impl Iterator<Item = (&Id, &T)> {
            self.into_iter()
        }
    }

    impl<Id, T> ArenaAccess<&Id, T> for ArenaSparse<Id, T>
    where
        Id: ArenaId,
    {
        fn get(&self, id: &Id) -> Option<&T> {
            self.map.get(&id)
        }
        fn get_mut(&mut self, id: &Id) -> Option<&mut T> {
            self.map.get_mut(&id)
        }
    }

    // No FromIterator: sparse owning storage is assembled explicitly or by
    // merging fragments with duplicate validation.

    impl<Id, T> Extend<(Id, T)> for ArenaSparse<Id, T>
    where
        Id: ArenaId,
    {
        fn extend<I: IntoIterator<Item = (Id, T)>>(&mut self, iter: I) {
            for (id, val) in iter {
                self.insert_new(id, val);
            }
        }
    }

    impl<Id, T> AddAssign for ArenaSparse<Id, T>
    where
        Id: ArenaId,
    {
        fn add_assign(&mut self, rhs: ArenaSparse<Id, T>) {
            self.extend(rhs);
        }
    }

    impl<Id, T> ArenaSparse<Id, T>
    where
        Id: ArenaId,
    {
        pub fn map_id<U>(self, f: impl Fn(Id) -> U) -> ArenaSparse<Id, U> {
            let Self { map } = self;
            let map = map.into_keys().map(|id| (id, f(id))).collect();
            ArenaSparse { map }
        }
        pub fn map_value<U>(self, f: impl Fn(T) -> U) -> ArenaSparse<Id, U> {
            let Self { map } = self;
            let map = map.into_iter().map(|(id, val)| (id, f(val))).collect();
            ArenaSparse { map }
        }
        pub fn map<U>(self, f: impl Fn(Id, T) -> U) -> ArenaSparse<Id, U> {
            let Self { map } = self;
            let map = map.into_iter().map(|(id, val)| (id, f(id, val))).collect();
            ArenaSparse { map }
        }
        pub fn filter_map_id<U>(self, f: impl Fn(Id) -> Option<U>) -> ArenaSparse<Id, U> {
            let Self { map } = self;
            let map = map.into_keys().filter_map(|id| f(id).map(|val| (id, val))).collect();
            ArenaSparse { map }
        }
        pub fn filter_map_id_mut<U>(
            self, mut f: impl FnMut(Id) -> Option<U>,
        ) -> ArenaSparse<Id, U> {
            let Self { map } = self;
            let map = map.into_keys().filter_map(|id| f(id).map(|val| (id, val))).collect();
            ArenaSparse { map }
        }
        pub fn filter_map_value<U>(self, f: impl Fn(T) -> Option<U>) -> ArenaSparse<Id, U> {
            let Self { map } = self;
            let map = map.into_iter().filter_map(|(id, val)| f(val).map(|val| (id, val))).collect();
            ArenaSparse { map }
        }
        pub fn filter_map<U>(self, f: impl Fn(Id, T) -> Option<U>) -> ArenaSparse<Id, U> {
            let Self { map } = self;
            let map =
                map.into_iter().filter_map(|(id, val)| f(id, val).map(|val| (id, val))).collect();
            ArenaSparse { map }
        }
        pub fn len(&self) -> usize {
            self.map.len()
        }
        pub fn replace_existing(&mut self, id: Id, val: impl Into<T>) {
            use std::collections::hash_map::Entry;
            match self.map.entry(id) {
                | Entry::Occupied(mut entry) => {
                    entry.insert(val.into());
                }
                | Entry::Vacant(_) => panic!("key not found"),
            }
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
        /// Insert a value whose key must not already be present.
        pub fn insert_new(&mut self, id: Id, val: T) {
            use std::collections::hash_map::Entry;
            match self.map.entry(id) {
                | Entry::Vacant(entry) => {
                    entry.insert(val);
                }
                | Entry::Occupied(_) => panic!("duplicate key in associative arena"),
            }
        }
        /// Replace the value at the given id with the given value. Returns the old value.
        pub fn replace_existing(&mut self, id: Id, val: T) -> T {
            use std::collections::hash_map::Entry;
            match self.map.entry(id) {
                | Entry::Occupied(mut entry) => entry.insert(val),
                | Entry::Vacant(_) => panic!("key not found"),
            }
        }
        /// Replace the value at the given id with the given value. Returns the old value.
        pub fn replace_existing_with(&mut self, id: Id, val: impl Into<T>) -> T {
            use std::collections::hash_map::Entry;
            match self.map.entry(id) {
                | Entry::Occupied(mut entry) => entry.insert(val.into()),
                | Entry::Vacant(_) => panic!("key not found"),
            }
        }
        pub fn entry(&mut self, id: Id) -> std::collections::hash_map::Entry<'_, Id, T> {
            self.map.entry(id)
        }
        #[must_use]
        pub fn upsert(&mut self, id: Id, val: T) -> Option<T> {
            self.map.insert(id, val)
        }
        /// Insert a value, or verify that the existing value is identical.
        pub fn insert_or_same(&mut self, id: Id, val: T)
        where
            T: PartialEq,
        {
            use std::collections::hash_map::Entry;
            match self.map.entry(id) {
                | Entry::Vacant(entry) => {
                    entry.insert(val);
                }
                | Entry::Occupied(entry) => {
                    assert!(entry.get() == &val, "conflicting value in associative arena");
                }
            }
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
                self.insert_new(id, val);
                None
            }
        }
    }

    impl<Id> ArenaAssoc<Id, ()>
    where
        Id: Eq + Hash,
    {
        /// Ensure that an id is a member of this set-like associative arena.
        pub fn ensure(&mut self, id: Id) {
            self.map.entry(id).or_insert(());
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

    impl<Id, T> ArenaAccess<&Id, T> for ArenaAssoc<Id, T>
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

    impl<Id, T> Extend<(Id, T)> for ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
        fn extend<I: IntoIterator<Item = (Id, T)>>(&mut self, iter: I) {
            for (id, val) in iter {
                self.insert_new(id, val);
            }
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
        pub fn insert_new(&mut self, prev: P, qurr: Q) {
            assert!(self.backward.get(&qurr).is_none(), "derived key already has a source");
            self.forward.map.entry(prev.clone()).or_insert_with(Vec::new).push(qurr.clone());
            self.backward.insert_new(qurr, prev);
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

    impl<P, Q> Extend<(P, Vec<Q>)> for ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn extend<I: IntoIterator<Item = (P, Vec<Q>)>>(&mut self, iter: I) {
            for (p, qs) in iter {
                for q in qs {
                    self.insert_new(p.clone(), q);
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
        pub fn insert_new(&mut self, p: P, q: Q) {
            assert!(self.forward.get(&p).is_none(), "source key already has a target");
            self.forward.insert_new(p.clone(), q.clone());
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

    impl<P, Q> Extend<(P, Q)> for ArenaBack<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn extend<I: IntoIterator<Item = (P, Q)>>(&mut self, iter: I) {
            for (p, q) in iter {
                self.insert_new(p, q);
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
        pub fn insert_new(&mut self, p: P, q: Q) {
            assert!(self.forward.get(&p).is_none(), "left key already has a partner");
            assert!(self.backward.get(&q).is_none(), "right key already has a partner");
            self.forward.insert_new(p.clone(), q.clone());
            self.backward.insert_new(q, p);
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

    impl<P, Q> Extend<(P, Q)> for ArenaBijective<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn extend<I: IntoIterator<Item = (P, Q)>>(&mut self, iter: I) {
            for (p, q) in iter {
                self.insert_new(p, q);
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
        pub fn insert_new(&mut self, p: P, q: Q) {
            assert!(
                !self.forward.get(&p).is_some_and(|qs| qs.contains(&q)),
                "duplicate many-to-many edge"
            );
            self.forward.map.entry(p.clone()).or_insert_with(Vec::new).push(q.clone());
            self.backward.map.entry(q).or_insert_with(Vec::new).push(p);
        }

        /// Ensure that an edge exists, without duplicating an existing edge.
        pub fn ensure(&mut self, p: P, q: Q) {
            if self.forward.get(&p).is_some_and(|qs| qs.contains(&q)) {
                return;
            }
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

    impl<P, Q> Extend<(P, Vec<Q>)> for ArenaBipartite<P, Q>
    where
        P: Eq + Hash + Clone,
        Q: Eq + Hash + Clone,
    {
        fn extend<I: IntoIterator<Item = (P, Vec<Q>)>>(&mut self, iter: I) {
            for (p, qs) in iter {
                for q in qs {
                    self.insert_new(p.clone(), q);
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
    ( $(#[$outer:meta])* $vis:vis struct $name:ident ; $($rest:tt)* ) => {
        $(#[$outer])*
        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
        $vis struct $name($crate::arena::KeySpaceId, $crate::arena::RawIdx);

        impl $crate::arena::ArenaId for $name {
            fn from_raw_parts(
                _token: $crate::arena::ArenaIdToken,
                key_space: $crate::arena::KeySpaceId,
                raw: $crate::arena::RawIdx,
            ) -> Self {
                Self(key_space, raw)
            }
            fn key_space(self) -> $crate::arena::KeySpaceId {
                self.0
            }
            fn raw(self) -> $crate::arena::RawIdx {
                self.1
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:?}, {})", stringify!($name), self.0, self.1.into_u32())
            }
        }

        impl $name {
            pub fn concise(&self) -> String {
                format!("[{:?}#{}]", self.0, self.1.into_u32())
            }
            pub fn concise_inner(&self) -> String {
                format!("{:?}#{}", self.0, self.1.into_u32())
            }
        }

        $crate::new_key_type!($($rest)*);
    };

    () => {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::panic::{AssertUnwindSafe, catch_unwind};

    crate::new_key_type! {
        struct DenseId;
        struct SparseId;
    }

    #[test]
    fn dense_arena_rejects_an_id_from_another_key_space() {
        let mut left = ArenaDense::<DenseId, _>::new();
        let mut right = ArenaDense::<DenseId, _>::new();
        let left_id = left.alloc("left");
        let right_id = right.alloc("right");

        assert_eq!(left_id.raw(), right_id.raw());
        assert_ne!(left_id.key_space(), right_id.key_space());
        assert_ne!(left_id.concise(), right_id.concise());
        assert_ne!(left_id.concise_inner(), right_id.concise_inner());
        assert_eq!(left.get(&left_id), Some(&"left"));
        assert_eq!(left.get(&right_id), None);
    }

    #[test]
    fn sparse_storage_accepts_ids_from_independent_key_spaces() {
        let mut left_keys = KeySpace::new();
        let mut right_keys = KeySpace::new();
        let left_id: SparseId = left_keys.alloc();
        let right_id: SparseId = right_keys.alloc();
        assert_eq!(left_id.raw(), right_id.raw());
        assert_ne!(left_id.key_space(), right_id.key_space());
        assert_ne!(left_id.concise(), right_id.concise());
        assert_ne!(left_id.concise_inner(), right_id.concise_inner());

        let mut arena = ArenaSparse::new();
        arena.insert_new(left_id, "left");
        arena.insert_new(right_id, "right");
        assert_eq!(arena.len(), 2);
        assert_eq!(arena.get(&left_id), Some(&"left"));
        assert_eq!(arena.get(&right_id), Some(&"right"));
    }

    #[test]
    fn associative_insertions_state_their_overwrite_semantics() {
        let mut arena = ArenaAssoc::new();
        arena.insert_new(1, "first");
        assert_eq!(arena.replace_existing(1, "second"), "first");
        assert_eq!(arena.upsert(1, "third"), Some("second"));
        assert_eq!(arena.upsert(2, "new"), None);
    }

    #[test]
    #[should_panic(expected = "duplicate key in associative arena")]
    fn associative_insert_new_rejects_duplicates() {
        let mut arena = ArenaAssoc::new();
        arena.insert_new(1, "first");
        arena.insert_new(1, "second");
    }

    #[test]
    fn associative_replace_missing_does_not_insert_before_panicking() {
        let mut arena = ArenaAssoc::new();
        let missing = catch_unwind(AssertUnwindSafe(|| arena.replace_existing(1, "value")));
        assert!(missing.is_err());
        assert_eq!(arena.get(&1), None);
    }

    #[test]
    fn sparse_arena_merge_rejects_duplicate_ids() {
        let mut key_space = KeySpace::new();
        let id = key_space.alloc();
        let mut left = ArenaSparse::<SparseId, _>::new();
        let mut right = ArenaSparse::<SparseId, _>::new();
        left.insert_new(id, "left");
        right.insert_new(id, "right");
        let conflict = catch_unwind(AssertUnwindSafe(|| left += right));
        assert!(conflict.is_err());
        assert_eq!(left.get(&id), Some(&"left"));
    }

    #[test]
    fn forth_relation_is_one_source_to_many_derived_nodes() {
        let mut relation = ArenaForth::new();
        relation.insert_new("source", 1);
        relation.insert_new("source", 2);
        assert_eq!(relation.forth(&"source"), &[1, 2]);
        assert_eq!(relation.back(&1), Some(&"source"));

        let conflict = catch_unwind(AssertUnwindSafe(|| relation.insert_new("other", 1)));
        assert!(conflict.is_err());
        assert_eq!(relation.forth(&"other"), &[]);
        assert_eq!(relation.back(&1), Some(&"source"));
    }

    #[test]
    fn back_relation_is_many_sources_to_one_derived_node() {
        let mut relation = ArenaBack::new();
        relation.insert_new("inner", 1);
        relation.insert_new("outer", 1);
        assert_eq!(relation.back(&1), Some(["inner", "outer"].as_slice()));

        let conflict = catch_unwind(AssertUnwindSafe(|| relation.insert_new("inner", 2)));
        assert!(conflict.is_err());
        assert_eq!(relation.try_forth(&"inner"), Some(&1));
        assert_eq!(relation.back(&2), None);
    }

    #[test]
    fn bijection_checks_both_sides_before_mutating() {
        let mut relation = ArenaBijective::new();
        relation.insert_new("left", 1);

        let conflict = catch_unwind(AssertUnwindSafe(|| relation.insert_new("other", 1)));
        assert!(conflict.is_err());
        assert_eq!(relation.try_forth(&"other"), None);
        assert_eq!(relation.try_back(&1), Some(&"left"));
    }

    #[test]
    fn bipartite_ensure_is_idempotent() {
        let mut relation = ArenaBipartite::new();
        relation.ensure("source", 1);
        relation.ensure("source", 1);
        relation.ensure("source", 2);
        relation.ensure("other", 1);

        assert_eq!(relation.forth(&"source"), &[1, 2]);
        assert_eq!(relation.back(&1), &["source", "other"]);
    }
}
