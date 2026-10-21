use derive_more::{Index, IntoIterator};
pub use la_arena::RawIdx;
use la_arena::{Arena as LaArena, Idx as LaIdx};
use rustc_hash::FxHashMap as HashMap;
use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::{NonZeroU32, NonZeroU64},
    ops::AddAssign,
    ops::{Index, IndexMut},
    sync::{
        Mutex,
        atomic::{AtomicU64, Ordering},
    },
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

/* ------------------------------ IdAllocator ------------------------------- */

/// Opaque identity domain for arena IDs.
#[derive(Copy, Clone, derive_more::Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[debug("{_0}")]
pub struct KeySpaceId(NonZeroU64);

/// The shared issuer for both sequential and interned key spaces.
#[derive(Default)]
struct KeySpaceIssuer {
    last: AtomicU64,
}

impl KeySpaceIssuer {
    fn fresh(&self) -> KeySpaceId {
        let previous = self
            .last
            .try_update(Ordering::Relaxed, Ordering::Relaxed, |id| id.checked_add(1))
            .expect("key-space identity range exhausted");
        KeySpaceId(NonZeroU64::new(previous + 1).expect("a fresh key-space identity is nonzero"))
    }
}

impl KeySpaceId {
    fn fresh() -> Self {
        static ISSUER: KeySpaceIssuer = KeySpaceIssuer { last: AtomicU64::new(0) };
        ISSUER.fresh()
    }

    /// The numeric identity, for compact storage and diagnostic rendering.
    pub fn as_u64(self) -> u64 {
        self.0.get()
    }
}

/// Interns complete allocation-site keys into process-unique key spaces.
///
/// Equal sites in this registry reuse a key space. Distinct sites, independent
/// registries, and sequential allocators receive different key spaces from one
/// non-recycling issuer. Hashes only accelerate lookup; full equality decides
/// identity. The lookup and first issuance are atomic with respect to other
/// callers of this registry.
///
/// Share the registry with `Arc`, and retain it for as long as its sites may be
/// replayed. Dropping it releases the site map without recycling any issued ID.
/// See the [compiler identity account](../../../docs/references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance).
///
/// ```
/// use zydeco_utils::arena::KeySpaceRegistry;
///
/// #[derive(Eq, PartialEq, Hash)]
/// struct Site { entity: u32, occurrence: u32 }
///
/// let registry = KeySpaceRegistry::default();
/// let first = registry.intern(Site { entity: 7, occurrence: 0 });
/// assert_eq!(first, registry.intern(Site { entity: 7, occurrence: 0 }));
/// assert_ne!(first, registry.intern(Site { entity: 7, occurrence: 1 }));
/// ```
///
/// The registry accepts only its declared site type:
///
/// ```compile_fail,E0308
/// use zydeco_utils::arena::KeySpaceRegistry;
///
/// let registry = KeySpaceRegistry::<(u32, u32)>::default();
/// registry.intern(7_u64);
/// ```
#[derive(Debug)]
pub struct KeySpaceRegistry<Site> {
    sites: Mutex<HashMap<Site, KeySpaceId>>,
}

impl<Site> Default for KeySpaceRegistry<Site> {
    fn default() -> Self {
        Self { sites: Mutex::new(HashMap::default()) }
    }
}

impl<Site: Eq + Hash> KeySpaceRegistry<Site> {
    /// Reuse a site's key space or issue a new one, failing on issuer exhaustion.
    pub fn intern(&self, site: Site) -> KeySpaceId {
        let mut sites = self.sites.lock().expect("key-space registry poisoned");
        *sites.entry(site).or_insert_with(KeySpaceId::fresh)
    }
}

/// Padding-free storage for a [`KeySpaceId`] inside a generated arena ID.
///
/// Keeping the high and low words separate lowers the field alignment to four
/// bytes without losing any identity bits. A generated ID can then store this
/// eight-byte value beside its four-byte raw slot in exactly twelve bytes.
#[doc(hidden)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct CompactKeySpaceId {
    high: u32,
    low: u32,
}

impl CompactKeySpaceId {
    #[inline]
    pub fn new(id: KeySpaceId) -> Self {
        let value = id.as_u64();
        Self { high: (value >> 32) as u32, low: value as u32 }
    }

    #[inline]
    pub fn expand(self) -> KeySpaceId {
        let value = (u64::from(self.high) << 32) | u64::from(self.low);
        KeySpaceId(NonZeroU64::new(value).expect("a compact key-space identity remains nonzero"))
    }
}

impl Hash for CompactKeySpaceId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.expand().as_u64().hash(state);
    }
}

/// The category-independent identity of one generated arena ID.
///
/// Use this representation only when an enclosing typed structure or shard
/// preserves the ID category. It removes the phantom Rust type while retaining
/// every key-space and raw-slot bit in the same twelve-byte layout.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ArenaIdIdentity {
    key_space: CompactKeySpaceId,
    raw: RawIdx,
}

impl ArenaIdIdentity {
    pub fn new<Id: ArenaId>(id: Id) -> Self {
        Self { key_space: CompactKeySpaceId::new(id.key_space()), raw: id.raw() }
    }

    pub fn restore<Id: ArenaId>(self) -> Id {
        restore_id(self.key_space.expand(), self.raw)
    }
}

/// Construct an arena identifier from a derived key space and a local slot.
///
/// This is the sanctioned way to build identifiers without an [`IdAllocator`].
/// Obtain the key space from [`KeySpaceRegistry`] and assign a distinct local
/// slot to each allocation at that site. Reusing both reconstructs the same ID.
pub fn derived_id<Id: ArenaId>(key_space: KeySpaceId, slot: u32) -> Id {
    Id::from_raw_parts(ArenaIdToken(()), key_space, RawIdx::from_u32(slot))
}

/// Reconstruct an identifier that was previously issued by an allocator.
///
/// Dense storage may omit repeated identifier values when it separately
/// retains the original key space, raw slot, and identifier category. Callers
/// must not use this function to invent a new allocation.
#[doc(hidden)]
pub fn restore_id<Id: ArenaId>(key_space: KeySpaceId, raw: RawIdx) -> Id {
    Id::from_raw_parts(ArenaIdToken(()), key_space, raw)
}

/// Declares that an allocator scope may issue a particular identifier type.
///
/// This relation is intentionally independent from [`ArenaSchema`]: a pass may
/// create identifiers whose contents are stored in another compiler arena.
pub trait Allocates<Id: ArenaId> {}

/// Issues typed identifiers within one identity domain.
///
/// An ID allocator deliberately cannot be cloned: cloning its cursor would
/// allow two owners to issue the same future identifier. Independent allocators
/// receive independent [`KeySpaceId`]s, so they can allocate in parallel and
/// their sparse arena fragments can later be merged without collisions.
///
/// An allocator cannot issue an identifier absent from its scope:
///
/// ```compile_fail
/// use zydeco_utils::{
///     arena::IdAllocator,
///     new_key_type,
/// };
///
/// new_key_type! { struct NodeId; }
/// enum EmptyScope {}
///
/// let mut allocator = IdAllocator::<EmptyScope>::new();
/// let _: NodeId = allocator.alloc();
/// ```
#[derive(Debug)]
pub struct IdAllocator<Scope> {
    key_space: KeySpaceId,
    next: u32,
    marker: PhantomData<fn() -> Scope>,
}

impl<Scope> IdAllocator<Scope> {
    /// Create an independent sequential ID issuer.
    ///
    /// Construction claims one identity atomically. Allocation after that only
    /// mutates this value's local cursor, so separate allocators do not contend.
    pub fn new() -> Self {
        Self { key_space: KeySpaceId::fresh(), next: 0, marker: PhantomData }
    }
    pub fn alloc<Id>(&mut self) -> Id
    where
        Id: ArenaId,
        Scope: Allocates<Id>,
    {
        let raw = self.next;
        self.next = self.next.checked_add(1).expect("ID allocator exhausted its u32 index range");
        Id::from_raw_parts(ArenaIdToken(()), self.key_space, RawIdx::from_u32(raw))
    }
}

/* ---------------------------------- Arena --------------------------------- */

/// Associates an owning arena scope and identifier type with its stored item.
///
/// The identifier is a trait parameter rather than an associated type so one
/// arena scope can own several identifier categories and one identifier type
/// can inhabit several representation-specific arena scopes.
///
/// The schema fixes the item type accepted by owning storage:
///
/// ```compile_fail
/// use zydeco_utils::{
///     arena::{Allocates, ArenaSchema, ArenaSparse, IdAllocator},
///     new_key_type,
/// };
///
/// new_key_type! { struct NodeId; }
/// enum Nodes {}
/// impl Allocates<NodeId> for Nodes {}
/// impl ArenaSchema<NodeId> for Nodes { type Item = u32; }
///
/// let mut allocator = IdAllocator::<Nodes>::new();
/// let id = allocator.alloc();
/// let mut arena = ArenaSparse::<Nodes, NodeId>::new();
/// arena.insert_new(id, "not a u32");
/// ```
pub trait ArenaSchema<Id: ArenaId> {
    type Item;
}

/// Read capability for arena storage.
///
/// Keeping this independent from [`ArenaAccessMut`] lets consumers describe
/// immutable phase inputs without acquiring a write capability accidentally.
pub trait ArenaAccess<Id, T>: Index<Id, Output = T> {
    fn get(&self, id: Id) -> Option<&T>;
}

/// Write capability for arena storage under construction.
pub trait ArenaAccessMut<Id, T>: ArenaAccess<Id, T> + IndexMut<Id, Output = T> {
    fn get_mut(&mut self, id: Id) -> Option<&mut T>;
}

/* ----------------------------- FrozenArena ------------------------------ */

/// An arena after its producing phase has finished.
///
/// `FrozenArena` is a transparent, allocation-free ownership boundary. It
/// deliberately implements [`Deref`] but not `DerefMut`, so downstream phases
/// can read the arena at the same cost as the underlying value without being
/// able to update a completed phase product accidentally. A consuming
/// transformation may recover ownership with [`FrozenArena::into_inner`] and
/// establish a new frozen boundary for its own output.
///
/// ```compile_fail
/// use zydeco_utils::arena::FrozenArena;
///
/// let mut arena = FrozenArena::new(Vec::<u32>::new());
/// arena.push(1);
/// ```
#[repr(transparent)]
#[derive(Clone, Debug, Default)]
pub struct FrozenArena<A>(A);

impl<A> FrozenArena<A> {
    pub fn new(arena: A) -> Self {
        Self(arena)
    }

    pub fn into_inner(self) -> A {
        self.0
    }
}

impl<A> AsRef<A> for FrozenArena<A> {
    fn as_ref(&self) -> &A {
        &self.0
    }
}

impl<A> std::ops::Deref for FrozenArena<A> {
    type Target = A;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
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
/// It retains an identity tag, but no separate sequential [`IdAllocator`].
#[derive(Debug)]
pub struct ArenaDense<Scope, Id>
where
    Id: ArenaId,
    Scope: ArenaSchema<Id>,
{
    key_space: KeySpaceId,
    arena: LaArena<Scope::Item>,
    marker: PhantomData<fn() -> (Scope, Id)>,
}

impl<Scope, Id> Clone for ArenaDense<Scope, Id>
where
    Id: ArenaId,
    Scope: ArenaSchema<Id>,
    Scope::Item: Clone,
{
    fn clone(&self) -> Self {
        Self { key_space: self.key_space, arena: self.arena.clone(), marker: PhantomData }
    }
}

/// Sparse owning storage for externally issued IDs.
/// Allocation belongs to the pass that owns the appropriate [`IdAllocator`].
#[derive(Debug, Index, IntoIterator)]
pub struct ArenaSparse<Scope, Id>
where
    Id: ArenaId,
    Scope: ArenaSchema<Id>,
{
    #[index]
    #[into_iterator(owned, ref)]
    map: HashMap<Id, Scope::Item>,
    marker: PhantomData<fn() -> Scope>,
}

impl<Scope, Id> Clone for ArenaSparse<Scope, Id>
where
    Id: ArenaId,
    Scope: ArenaSchema<Id>,
    Scope::Item: Clone,
{
    fn clone(&self) -> Self {
        Self { map: self.map.clone(), marker: PhantomData }
    }
}

/// An arena that maps keys of externally-owned data to their properties.
/// Conceptually, it doesn't own the data, but it owns the properties bound to the data.
#[derive(Debug, Clone, Index, IntoIterator)]
pub struct ArenaAssoc<Id, T> {
    #[into_iterator(owned, ref)]
    map: HashMap<Id, T>,
}

/// An associative arena paged by the key-space component of an [`ArenaId`].
#[derive(Debug, Clone)]
pub struct ArenaPagedAssoc<Id: ArenaId, T> {
    pages: HashMap<KeySpaceId, Vec<Option<T>>>,
    len: usize,
    marker: PhantomData<fn() -> Id>,
}

/// Owning storage that separates sparse external identity from dense payloads.
///
/// The paged index costs one compact optional `u32` per raw ID slot, while the
/// full items occupy a dense vector with no gaps or repeated IDs. This is the
/// useful middle ground when IDs are sparse across categories but the payloads
/// are too large to leave gaps in storage and too numerous for [`ArenaSparse`].
#[derive(Debug)]
pub struct ArenaIndexed<Scope, Id>
where
    Id: ArenaId,
    Scope: ArenaSchema<Id>,
{
    indexes: ArenaPagedAssoc<Id, DenseArenaIndex>,
    values: Vec<Scope::Item>,
    marker: PhantomData<fn() -> Scope>,
}

/// One-based so `Option<DenseArenaIndex>` retains the four-byte niche.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct DenseArenaIndex(NonZeroU32);

impl DenseArenaIndex {
    fn from_offset(offset: usize) -> Self {
        let one_based = offset.checked_add(1).expect("indexed arena payload range exhausted");
        let one_based = u32::try_from(one_based).expect("indexed arena payload range exhausted");
        Self(NonZeroU32::new(one_based).expect("an indexed arena offset is one-based"))
    }

    fn offset(self) -> usize {
        self.0.get() as usize - 1
    }
}

impl<Scope, Id> Clone for ArenaIndexed<Scope, Id>
where
    Id: ArenaId,
    Scope: ArenaSchema<Id>,
    Scope::Item: Clone,
{
    fn clone(&self) -> Self {
        Self { indexes: self.indexes.clone(), values: self.values.clone(), marker: PhantomData }
    }
}

/// Capacity policy shared by paged owning and associative arenas.
struct PageSlots;

impl PageSlots {
    /// Grow large pages by 50% instead of `Vec`'s doubling policy. Derived-ID
    /// pages are append-dense, so this trades a few logarithmic reallocations
    /// for substantially less unused capacity at the end of a check.
    fn ensure<T>(page: &mut Vec<Option<T>>, required: usize) {
        if page.capacity() < required {
            let grown = page.capacity().saturating_add(page.capacity() / 2).max(4);
            let target = required.max(grown);
            page.reserve_exact(target - page.len());
        }
        if page.len() < required {
            page.resize_with(required, || None);
        }
    }

    fn reserve_ids<Id: ArenaId, T>(
        pages: &mut HashMap<KeySpaceId, Vec<Option<T>>>, ids: impl IntoIterator<Item = Id>,
    ) -> usize {
        let (required, count) = ids.into_iter().fold(
            (HashMap::<KeySpaceId, usize>::default(), 0usize),
            |(mut required, count), id| {
                let slots = id.raw().into_u32() as usize + 1;
                required
                    .entry(id.key_space())
                    .and_modify(|current| *current = (*current).max(slots))
                    .or_insert(slots);
                (required, count.checked_add(1).expect("arena item count exhausted usize"))
            },
        );
        pages.reserve(required.len());
        required.into_iter().for_each(|(key_space, slots)| {
            let page = pages.entry(key_space).or_default();
            if page.capacity() < slots {
                page.reserve_exact(slots - page.len());
            }
        });
        count
    }
}

/// A non-empty sequence that keeps its overwhelmingly common singleton case
/// inline. Bidirectional provenance relations have one edge at nearly every
/// syntax site; allocating a `Vec` buffer per direction would cost more than
/// the identifiers themselves.
#[derive(Debug, Clone)]
enum OneOrMany<T> {
    One(T),
    Many(Vec<T>),
}

impl<T> OneOrMany<T> {
    fn push(&mut self, value: T) {
        match self {
            | Self::Many(values) => values.push(value),
            | Self::One(_) => {
                let Self::One(first) = std::mem::replace(self, Self::Many(Vec::with_capacity(2)))
                else {
                    unreachable!("the singleton variant was matched above")
                };
                let Self::Many(values) = self else {
                    unreachable!("the replacement installs the many variant")
                };
                values.extend([first, value]);
            }
        }
    }

    fn as_slice(&self) -> &[T] {
        match self {
            | Self::One(value) => std::slice::from_ref(value),
            | Self::Many(values) => values,
        }
    }
}

impl<T> IntoIterator for OneOrMany<T> {
    type Item = T;
    type IntoIter = std::iter::Chain<std::option::IntoIter<T>, std::vec::IntoIter<T>>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            | Self::One(value) => Some(value).into_iter().chain(Vec::new()),
            | Self::Many(values) => None.into_iter().chain(values),
        }
    }
}

/// A bidirectional single-to-multi-map; a "widen" map.
#[derive(Debug, Clone)]
pub struct ArenaForth<P, Q> {
    forward: ArenaAssoc<P, OneOrMany<Q>>,
    backward: ArenaAssoc<Q, P>,
}

/// A bidirectional bijective map.
#[derive(Debug, Clone, IntoIterator)]
pub struct ArenaBijective<P, Q> {
    #[into_iterator(owned, ref)]
    forward: ArenaAssoc<P, Q>,
    backward: ArenaAssoc<Q, P>,
}

mod impls {
    use super::*;

    /* ------------------------------- ArenaDense ------------------------------- */

    impl<Scope, Id> Default for ArenaDense<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<Scope, Id> Index<&Id> for ArenaDense<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        type Output = Scope::Item;
        fn index(&self, id: &Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }
    impl<Scope, Id> IndexMut<&Id> for ArenaDense<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Scope, Id> ArenaDense<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        pub fn new() -> Self {
            Self { key_space: KeySpaceId::fresh(), arena: LaArena::new(), marker: PhantomData }
        }
        pub fn alloc(&mut self, val: Scope::Item) -> Id {
            let idx = self.arena.alloc(val);
            Id::from_raw_parts(ArenaIdToken(()), self.key_space, idx.into_raw())
        }
        pub fn iter(&self) -> impl Iterator<Item = (Id, &Scope::Item)> {
            let key_space = self.key_space;
            self.arena.iter().map(move |(idx, val)| {
                (Id::from_raw_parts(ArenaIdToken(()), key_space, idx.into_raw()), val)
            })
        }
        pub fn len(&self) -> usize {
            self.arena.len()
        }
        fn index(&self, id: Id) -> Option<LaIdx<Scope::Item>> {
            if id.key_space() != self.key_space {
                return None;
            }
            let idx = id.raw();
            ((idx.into_u32() as usize) < self.arena.len()).then(|| LaIdx::from_raw(idx))
        }
    }

    impl<Scope, Id> ArenaAccess<&Id, Scope::Item> for ArenaDense<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn get(&self, id: &Id) -> Option<&Scope::Item> {
            self.index(*id).map(|idx| &self.arena[idx])
        }
    }

    impl<Scope, Id> ArenaAccessMut<&Id, Scope::Item> for ArenaDense<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn get_mut(&mut self, id: &Id) -> Option<&mut Scope::Item> {
            let idx = self.index(*id)?;
            Some(&mut self.arena[idx])
        }
    }

    pub struct ArenaDenseIntoIter<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        key_space: KeySpaceId,
        inner: la_arena::IntoIter<Scope::Item>,
        marker: PhantomData<fn() -> (Scope, Id)>,
    }

    impl<Scope, Id> Iterator for ArenaDenseIntoIter<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        type Item = (Id, Scope::Item);
        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|(idx, val)| {
                (Id::from_raw_parts(ArenaIdToken(()), self.key_space, idx.into_raw()), val)
            })
        }
    }

    impl<Scope, Id> IntoIterator for ArenaDense<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        type Item = (Id, Scope::Item);
        type IntoIter = ArenaDenseIntoIter<Scope, Id>;
        fn into_iter(self) -> Self::IntoIter {
            ArenaDenseIntoIter {
                key_space: self.key_space,
                inner: self.arena.into_iter(),
                marker: PhantomData,
            }
        }
    }

    /* ------------------------------- ArenaSparse ------------------------------ */

    impl<Scope, Id> Default for ArenaSparse<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<Scope, Id> IndexMut<&Id> for ArenaSparse<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Scope, Id> ArenaSparse<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        pub fn new() -> Self {
            Self { map: HashMap::default(), marker: PhantomData }
        }
        /// Insert a value whose externally-issued ID must not already be present.
        pub fn insert_new(&mut self, id: Id, val: Scope::Item) {
            use std::collections::hash_map::Entry;
            match self.map.entry(id) {
                | Entry::Vacant(entry) => {
                    entry.insert(val);
                }
                | Entry::Occupied(_) => panic!("duplicate key in sparse arena"),
            }
        }
        pub fn iter(&self) -> impl Iterator<Item = (&Id, &Scope::Item)> {
            self.into_iter()
        }
        /// Reserve capacity for `additional` entries before a bulk fill.
        pub fn reserve(&mut self, additional: usize) {
            self.map.reserve(additional);
        }
    }

    impl<Scope, Id> ArenaAccess<&Id, Scope::Item> for ArenaSparse<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn get(&self, id: &Id) -> Option<&Scope::Item> {
            self.map.get(&id)
        }
    }

    impl<Scope, Id> ArenaAccessMut<&Id, Scope::Item> for ArenaSparse<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn get_mut(&mut self, id: &Id) -> Option<&mut Scope::Item> {
            self.map.get_mut(&id)
        }
    }

    // No FromIterator: sparse owning storage is assembled explicitly or by
    // merging fragments with duplicate validation.

    impl<Scope, Id> Extend<(Id, Scope::Item)> for ArenaSparse<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn extend<I: IntoIterator<Item = (Id, Scope::Item)>>(&mut self, iter: I) {
            for (id, val) in iter {
                self.insert_new(id, val);
            }
        }
    }

    impl<Scope, Id> AddAssign for ArenaSparse<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn add_assign(&mut self, rhs: ArenaSparse<Scope, Id>) {
            self.extend(rhs);
        }
    }

    impl<Scope, Id> ArenaSparse<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        /// Rebind the same identifiers and items to another owning arena scope.
        pub fn rebind<Target>(self) -> ArenaSparse<Target, Id>
        where
            Target: ArenaSchema<Id, Item = Scope::Item>,
        {
            let Self { map, marker: _ } = self;
            ArenaSparse { map, marker: PhantomData }
        }
        pub fn map_id<Target>(self, f: impl Fn(Id) -> Target::Item) -> ArenaSparse<Target, Id>
        where
            Target: ArenaSchema<Id>,
        {
            let Self { map, marker: _ } = self;
            let map = map.into_keys().map(|id| (id, f(id))).collect();
            ArenaSparse { map, marker: PhantomData }
        }
        pub fn map_value<Target>(
            self, f: impl Fn(Scope::Item) -> Target::Item,
        ) -> ArenaSparse<Target, Id>
        where
            Target: ArenaSchema<Id>,
        {
            let Self { map, marker: _ } = self;
            let map = map.into_iter().map(|(id, val)| (id, f(val))).collect();
            ArenaSparse { map, marker: PhantomData }
        }
        pub fn map<Target>(
            self, f: impl Fn(Id, Scope::Item) -> Target::Item,
        ) -> ArenaSparse<Target, Id>
        where
            Target: ArenaSchema<Id>,
        {
            let Self { map, marker: _ } = self;
            let map = map.into_iter().map(|(id, val)| (id, f(id, val))).collect();
            ArenaSparse { map, marker: PhantomData }
        }
        pub fn filter_map_id<Target>(
            self, f: impl Fn(Id) -> Option<Target::Item>,
        ) -> ArenaSparse<Target, Id>
        where
            Target: ArenaSchema<Id>,
        {
            let Self { map, marker: _ } = self;
            let map = map.into_keys().filter_map(|id| f(id).map(|val| (id, val))).collect();
            ArenaSparse { map, marker: PhantomData }
        }
        pub fn filter_map_id_mut<Target>(
            self, mut f: impl FnMut(Id) -> Option<Target::Item>,
        ) -> ArenaSparse<Target, Id>
        where
            Target: ArenaSchema<Id>,
        {
            let Self { map, marker: _ } = self;
            let map = map.into_keys().filter_map(|id| f(id).map(|val| (id, val))).collect();
            ArenaSparse { map, marker: PhantomData }
        }
        pub fn filter_map_value<Target>(
            self, f: impl Fn(Scope::Item) -> Option<Target::Item>,
        ) -> ArenaSparse<Target, Id>
        where
            Target: ArenaSchema<Id>,
        {
            let Self { map, marker: _ } = self;
            let map = map.into_iter().filter_map(|(id, val)| f(val).map(|val| (id, val))).collect();
            ArenaSparse { map, marker: PhantomData }
        }
        pub fn filter_map<Target>(
            self, f: impl Fn(Id, Scope::Item) -> Option<Target::Item>,
        ) -> ArenaSparse<Target, Id>
        where
            Target: ArenaSchema<Id>,
        {
            let Self { map, marker: _ } = self;
            let map =
                map.into_iter().filter_map(|(id, val)| f(id, val).map(|val| (id, val))).collect();
            ArenaSparse { map, marker: PhantomData }
        }
        pub fn len(&self) -> usize {
            self.map.len()
        }
        /// Replace an existing owned item.
        pub fn replace_existing(&mut self, id: Id, val: Scope::Item) {
            use std::collections::hash_map::Entry;
            match self.map.entry(id) {
                | Entry::Occupied(mut entry) => {
                    entry.insert(val);
                }
                | Entry::Vacant(_) => panic!("key not found"),
            }
        }
        /// Convert and replace an existing owned item.
        pub fn replace_existing_with(&mut self, id: Id, val: impl Into<Scope::Item>) {
            self.replace_existing(id, val.into())
        }
    }

    /* ------------------------------ ArenaIndexed ------------------------------ */

    impl<Scope, Id> Default for ArenaIndexed<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<Scope, Id> Index<&Id> for ArenaIndexed<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        type Output = Scope::Item;

        fn index(&self, id: &Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }

    impl<Scope, Id> IndexMut<&Id> for ArenaIndexed<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Scope, Id> ArenaIndexed<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        pub fn new() -> Self {
            Self { indexes: ArenaPagedAssoc::new(), values: Vec::new(), marker: PhantomData }
        }

        /// Insert a value whose externally-issued ID must not already exist.
        pub fn insert_new(&mut self, id: Id, value: Scope::Item) {
            let index = DenseArenaIndex::from_offset(self.values.len());
            self.indexes.insert_new(id, index);
            self.values.push(value);
        }

        pub fn iter(&self) -> impl Iterator<Item = (Id, &Scope::Item)> {
            self.indexes.iter().map(|(id, index)| (id, &self.values[index.offset()]))
        }

        pub fn len(&self) -> usize {
            self.values.len()
        }

        pub fn is_empty(&self) -> bool {
            self.values.is_empty()
        }

        /// Reserve sparse ID extents and dense payload slots for a future bulk
        /// fill. Supplied IDs must describe new values rather than existing ones.
        pub fn reserve_ids(&mut self, ids: impl IntoIterator<Item = Id>) {
            self.reserve_ids_with_additional(ids, 0);
        }

        /// Reserve an external ID domain plus payloads whose IDs will be issued
        /// during the consuming pass. The additional IDs need no sparse extents
        /// yet, but including their payloads prevents one late insertion from
        /// geometrically growing an otherwise exact vector.
        pub fn reserve_ids_with_additional(
            &mut self, ids: impl IntoIterator<Item = Id>, additional: usize,
        ) {
            let external = PageSlots::reserve_ids(&mut self.indexes.pages, ids);
            let total = external
                .checked_add(additional)
                .expect("indexed arena reservation exhausted usize");
            self.values.reserve_exact(total);
        }

        /// Replace an existing owned item without changing its dense position.
        pub fn replace_existing(&mut self, id: Id, value: Scope::Item) {
            let index = self.indexes.get(&id).copied().expect("key not found");
            self.values[index.offset()] = value;
        }
    }

    impl<Scope, Id> ArenaAccess<&Id, Scope::Item> for ArenaIndexed<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn get(&self, id: &Id) -> Option<&Scope::Item> {
            self.indexes.get(id).map(|index| &self.values[index.offset()])
        }
    }

    impl<Scope, Id> ArenaAccessMut<&Id, Scope::Item> for ArenaIndexed<Scope, Id>
    where
        Id: ArenaId,
        Scope: ArenaSchema<Id>,
    {
        fn get_mut(&mut self, id: &Id) -> Option<&mut Scope::Item> {
            let index = self.indexes.get(id)?.offset();
            self.values.get_mut(index)
        }
    }

    /* ---------------------------- ArenaPagedAssoc ----------------------------- */

    impl<Id: ArenaId, T> Default for ArenaPagedAssoc<Id, T> {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<Id: ArenaId, T> Index<&Id> for ArenaPagedAssoc<Id, T> {
        type Output = T;

        fn index(&self, id: &Id) -> &Self::Output {
            self.get(id).unwrap()
        }
    }

    impl<Id: ArenaId, T> IndexMut<&Id> for ArenaPagedAssoc<Id, T> {
        fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
            self.get_mut(id).unwrap()
        }
    }

    impl<Id: ArenaId, T> ArenaPagedAssoc<Id, T> {
        pub fn new() -> Self {
            Self { pages: HashMap::default(), len: 0, marker: PhantomData }
        }

        /// Insert a value whose key must not already be present.
        pub fn insert_new(&mut self, id: Id, val: T) {
            let index = id.raw().into_u32() as usize;
            let page = self.pages.entry(id.key_space()).or_default();
            PageSlots::ensure(page, index + 1);
            assert!(page[index].is_none(), "duplicate key in paged associative arena");
            page[index] = Some(val);
            self.len += 1;
        }

        pub fn iter(&self) -> impl Iterator<Item = (Id, &T)> {
            self.pages.iter().flat_map(|(key_space, page)| {
                let key_space = *key_space;
                page.iter().enumerate().filter_map(move |(raw, value)| {
                    let value = value.as_ref()?;
                    let raw = u32::try_from(raw).expect("arena page exceeded the raw ID range");
                    let id = Id::from_raw_parts(ArenaIdToken(()), key_space, RawIdx::from_u32(raw));
                    Some((id, value))
                })
            })
        }

        pub fn len(&self) -> usize {
            self.len
        }

        /// Reserve every page to the greatest supplied raw ID without filling
        /// its gaps.
        pub fn reserve_ids(&mut self, ids: impl IntoIterator<Item = Id>) {
            PageSlots::reserve_ids(&mut self.pages, ids);
        }

        /// Reserve outer page entries, one per expected key space.
        pub fn reserve_pages(&mut self, additional: usize) {
            self.pages.reserve(additional);
        }
    }

    impl<Id: ArenaId, T> ArenaAccess<&Id, T> for ArenaPagedAssoc<Id, T> {
        fn get(&self, id: &Id) -> Option<&T> {
            self.pages.get(&id.key_space())?.get(id.raw().into_u32() as usize)?.as_ref()
        }
    }

    impl<Id: ArenaId, T> ArenaAccessMut<&Id, T> for ArenaPagedAssoc<Id, T> {
        fn get_mut(&mut self, id: &Id) -> Option<&mut T> {
            self.pages.get_mut(&id.key_space())?.get_mut(id.raw().into_u32() as usize)?.as_mut()
        }
    }

    /* ------------------------------- ArenaAssoc ------------------------------- */

    impl<Id, T> ArenaAssoc<Id, T> {
        pub fn new() -> Self {
            ArenaAssoc { map: HashMap::default() }
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
        /// Reserve capacity for `additional` entries before a bulk fill.
        pub fn reserve(&mut self, additional: usize) {
            self.map.reserve(additional);
        }
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
    }

    impl<Id, T> ArenaAccessMut<&Id, T> for ArenaAssoc<Id, T>
    where
        Id: Eq + Hash,
    {
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
        pub fn iter(&self) -> impl Iterator<Item = (&P, &[Q])> {
            self.forward.iter().map(|(p, qs)| (p, qs.as_slice()))
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
            self.forward
                .map
                .entry(prev.clone())
                .and_modify(|values| values.push(qurr.clone()))
                .or_insert_with(|| OneOrMany::One(qurr.clone()));
            self.backward.insert_new(qurr, prev);
        }
    }

    impl<P, Q> Index<&P> for ArenaForth<P, Q>
    where
        P: Eq + Hash + Clone,
    {
        type Output = [Q];
        fn index(&self, p: &P) -> &Self::Output {
            self.forward.get(p).map(OneOrMany::as_slice).unwrap_or_default()
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
            self.forward.get(p).map(OneOrMany::as_slice).unwrap_or_default()
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
            rhs.forward.into_iter().for_each(|(p, qs)| {
                qs.into_iter().for_each(|q| self.insert_new(p.clone(), q));
            });
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
}

#[macro_export]
macro_rules! new_key_type {
    ( $(#[$outer:meta])* $vis:vis struct $name:ident ; $($rest:tt)* ) => {
        $(#[$outer])*
        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
        $vis struct $name($crate::arena::CompactKeySpaceId, $crate::arena::RawIdx);

        impl $crate::arena::ArenaId for $name {
            #[inline]
            fn from_raw_parts(
                _token: $crate::arena::ArenaIdToken,
                key_space: $crate::arena::KeySpaceId,
                raw: $crate::arena::RawIdx,
            ) -> Self {
                Self($crate::arena::CompactKeySpaceId::new(key_space), raw)
            }
            #[inline]
            fn key_space(self) -> $crate::arena::KeySpaceId {
                self.0.expand()
            }
            #[inline]
            fn raw(self) -> $crate::arena::RawIdx {
                self.1
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:?}, {})", stringify!($name), self.0.expand(), self.1.into_u32())
            }
        }

        impl $name {
            pub fn concise(&self) -> String {
                format!("[{:?}#{}]", self.0.expand(), self.1.into_u32())
            }
            pub fn concise_inner(&self) -> String {
                format!("{:?}#{}", self.0.expand(), self.1.into_u32())
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

    enum TestScope {}
    enum AlternateScope {}

    impl Allocates<SparseId> for TestScope {}
    impl ArenaSchema<DenseId> for TestScope {
        type Item = &'static str;
    }
    impl ArenaSchema<SparseId> for TestScope {
        type Item = &'static str;
    }
    impl ArenaSchema<SparseId> for AlternateScope {
        type Item = usize;
    }

    #[test]
    fn frozen_arena_is_a_zero_cost_read_only_boundary() {
        use std::mem::{align_of, size_of};

        assert_eq!(size_of::<FrozenArena<Vec<u32>>>(), size_of::<Vec<u32>>());
        assert_eq!(align_of::<FrozenArena<Vec<u32>>>(), align_of::<Vec<u32>>());

        let arena = FrozenArena::new(vec![1, 2]);
        assert_eq!(arena.as_slice(), &[1, 2]);
        let mut arena = arena.into_inner();
        arena.push(3);
        assert_eq!(arena, [1, 2, 3]);
    }

    #[test]
    fn generated_ids_store_key_space_and_raw_slot_without_tail_padding() {
        let key_space = KeySpaceId::fresh();
        let id: DenseId = derived_id(key_space, u32::MAX);
        let below_word_boundary = KeySpaceId(NonZeroU64::new(u64::from(u32::MAX)).unwrap());
        let above_word_boundary = KeySpaceId(NonZeroU64::new(1_u64 << 32).unwrap());

        assert_eq!(id.key_space(), key_space);
        assert_eq!(id.raw().into_u32(), u32::MAX);
        assert_eq!(std::mem::size_of::<CompactKeySpaceId>(), 8);
        assert_eq!(std::mem::align_of::<CompactKeySpaceId>(), 4);
        assert_eq!(std::mem::size_of::<DenseId>(), 12);
        assert_eq!(std::mem::align_of::<DenseId>(), 4);
        assert_eq!(std::mem::size_of::<Option<DenseId>>(), 16);
        let identity = ArenaIdIdentity::new(id);
        assert_eq!(std::mem::size_of::<ArenaIdIdentity>(), 12);
        assert_eq!(identity.restore::<DenseId>(), id);
        assert!(below_word_boundary < above_word_boundary);
        assert!(
            CompactKeySpaceId::new(below_word_boundary)
                < CompactKeySpaceId::new(above_word_boundary)
        );
    }

    #[test]
    fn dense_arena_rejects_an_id_from_another_key_space() {
        let mut left = ArenaDense::<TestScope, DenseId>::new();
        let mut right = ArenaDense::<TestScope, DenseId>::new();
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
        let mut left_allocator = IdAllocator::<TestScope>::new();
        let mut right_allocator = IdAllocator::<TestScope>::new();
        let left_id: SparseId = left_allocator.alloc();
        let right_id: SparseId = right_allocator.alloc();
        assert_eq!(left_id.raw(), right_id.raw());
        assert_ne!(left_id.key_space(), right_id.key_space());
        assert_ne!(left_id.concise(), right_id.concise());
        assert_ne!(left_id.concise_inner(), right_id.concise_inner());

        let mut arena = ArenaSparse::<TestScope, SparseId>::new();
        arena.insert_new(left_id, "left");
        arena.insert_new(right_id, "right");
        assert_eq!(arena.len(), 2);
        assert_eq!(arena.get(&left_id), Some(&"left"));
        assert_eq!(arena.get(&right_id), Some(&"right"));
    }

    #[test]
    fn one_id_type_can_inhabit_multiple_arena_scopes() {
        let mut allocator = IdAllocator::<TestScope>::new();
        let id = allocator.alloc();
        let mut source = ArenaSparse::<TestScope, SparseId>::new();
        let mut alternate = ArenaSparse::<AlternateScope, SparseId>::new();

        source.insert_new(id, "source");
        alternate.insert_new(id, 42);

        assert_eq!(source.get(&id), Some(&"source"));
        assert_eq!(alternate.get(&id), Some(&42));
    }

    #[test]
    fn indexed_storage_keeps_sparse_identity_and_dense_payloads() {
        let left_space = KeySpaceId::fresh();
        let right_space = KeySpaceId::fresh();
        let left_first = derived_id::<SparseId>(left_space, 0);
        let left_gap = derived_id::<SparseId>(left_space, 95);
        let right_first = derived_id::<SparseId>(right_space, 0);
        let generated = derived_id::<SparseId>(KeySpaceId::fresh(), 0);
        let mut arena = ArenaIndexed::<TestScope, SparseId>::new();

        arena.reserve_ids_with_additional(
            [left_first, left_gap, right_first].into_iter().filter(|_| true),
            1,
        );
        let payload_capacity = arena.values.capacity();
        arena.insert_new(left_gap, "left gap");
        arena.insert_new(right_first, "right first");
        arena.insert_new(left_first, "left first");
        arena.insert_new(generated, "generated");

        assert_eq!(size_of::<Option<DenseArenaIndex>>(), 4);
        assert_eq!(arena.len(), 4);
        assert_eq!(arena.values.len(), 4);
        assert_eq!(payload_capacity, 4);
        assert_eq!(arena.values.capacity(), payload_capacity);
        assert_eq!(arena.get(&left_first), Some(&"left first"));
        assert_eq!(arena.get(&derived_id::<SparseId>(left_space, 1)), None);
        assert_eq!(arena.get(&left_gap), Some(&"left gap"));
        assert_eq!(arena.get(&right_first), Some(&"right first"));
        assert_eq!(arena.get(&generated), Some(&"generated"));
        assert_eq!(arena.iter().count(), 4);
    }

    #[test]
    #[should_panic(expected = "duplicate key in paged associative arena")]
    fn indexed_storage_rejects_duplicate_ids_without_appending_payloads() {
        let id = derived_id::<SparseId>(KeySpaceId::fresh(), 0);
        let mut arena = ArenaIndexed::<TestScope, SparseId>::new();
        arena.insert_new(id, "first");
        arena.insert_new(id, "second");
    }

    #[test]
    fn paged_associations_preserve_sparse_gaps() {
        let space = KeySpaceId::fresh();
        let first = derived_id::<SparseId>(space, 1);
        let second = derived_id::<SparseId>(space, 4);
        let mut arena = ArenaPagedAssoc::new();

        arena.insert_new(first, 10);
        arena.insert_new(second, 40);

        assert_eq!(arena.len(), 2);
        assert_eq!(arena.get(&first), Some(&10));
        assert_eq!(arena.get(&derived_id::<SparseId>(space, 2)), None);
        assert_eq!(arena.get(&second), Some(&40));
        assert_eq!(arena.iter().count(), 2);
    }

    #[test]
    fn paged_associations_use_known_id_extents_without_growth_slack() {
        let space = KeySpaceId::fresh();
        let first = derived_id::<SparseId>(space, 0);
        let last = derived_id::<SparseId>(space, 95);
        let mut arena = ArenaPagedAssoc::new();

        arena.reserve_ids([first, last]);
        let capacity = arena.pages[&space].capacity();
        assert!(capacity >= 96);

        arena.insert_new(first, 20);
        arena.insert_new(last, 95);
        assert_eq!(arena.len(), 2);
        assert_eq!(arena.pages[&space].capacity(), capacity);
        assert_eq!(arena.get(&first), Some(&20));
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
        let mut allocator = IdAllocator::<TestScope>::new();
        let id = allocator.alloc();
        let mut left = ArenaSparse::<TestScope, SparseId>::new();
        let mut right = ArenaSparse::<TestScope, SparseId>::new();
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
    fn bijection_checks_both_sides_before_mutating() {
        let mut relation = ArenaBijective::new();
        relation.insert_new("left", 1);

        let conflict = catch_unwind(AssertUnwindSafe(|| relation.insert_new("other", 1)));
        assert!(conflict.is_err());
        assert_eq!(relation.try_forth(&"other"), None);
        assert_eq!(relation.try_back(&1), Some(&"left"));
    }
}

#[cfg(test)]
mod registry_tests {
    use super::*;
    use std::{
        collections::HashSet,
        panic::catch_unwind,
        sync::{Arc, Barrier},
    };

    new_key_type! { struct TestId; }
    enum TestScope {}
    impl Allocates<TestId> for TestScope {}

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    struct CollidingSite {
        entity: u64,
        occurrence: u32,
    }

    impl Hash for CollidingSite {
        fn hash<H: Hasher>(&self, state: &mut H) {
            // Force every lookup into the same hash bucket.
            0_u64.hash(state);
        }
    }

    #[test]
    fn registry_resolves_hash_collisions_by_full_site_equality() {
        let registry = KeySpaceRegistry::default();
        let sites = [
            CollidingSite { entity: 7, occurrence: 0 },
            CollidingSite { entity: 7, occurrence: 1 },
            CollidingSite { entity: 8, occurrence: 0 },
            CollidingSite { entity: u64::MAX, occurrence: u32::MAX },
        ];
        let spaces = sites.map(|site| registry.intern(site));
        assert_eq!(spaces.into_iter().collect::<HashSet<_>>().len(), sites.len());
        for (site, space) in sites.into_iter().zip(spaces) {
            assert_eq!(registry.intern(site), space);
        }
        assert_eq!(registry.sites.lock().unwrap().len(), sites.len());
    }

    #[test]
    fn registry_keeps_old_xor_collision_inputs_distinct() {
        #[derive(Eq, PartialEq, Hash)]
        struct Site {
            family: u64,
            entity_space: u64,
            entity_raw: u32,
            occurrence: u32,
        }

        let left = Site { family: 1, entity_space: 2, entity_raw: 42, occurrence: 0 };
        let right = Site { family: 2, entity_space: 1, entity_raw: 42, occurrence: 0 };
        // The removed mixer started with family ^ entity_space, losing this distinction.
        assert_eq!(left.family ^ left.entity_space, right.family ^ right.entity_space);
        let registry = KeySpaceRegistry::default();
        assert_ne!(registry.intern(left), registry.intern(right));
    }

    #[test]
    fn independent_registries_and_sequential_allocators_never_recycle_key_spaces() {
        let mut sequential = IdAllocator::<TestScope>::new();
        let before: TestId = sequential.alloc();
        let first = KeySpaceRegistry::default();
        let old = first.intern(0);
        let second = KeySpaceRegistry::default();
        let other_session = second.intern(0);
        let other_site = second.intern(1);
        drop(first);
        let replacement = KeySpaceRegistry::default().intern(0);
        let after: TestId = IdAllocator::<TestScope>::new().alloc();
        let spaces =
            [before.key_space(), old, other_session, other_site, replacement, after.key_space()];
        assert_eq!(spaces.into_iter().collect::<HashSet<_>>().len(), spaces.len());
    }

    #[test]
    fn concurrent_first_interns_share_one_identity_and_keep_distinct_keys_apart() {
        let registry = KeySpaceRegistry::default();
        let barrier = Barrier::new(8);
        let ids = std::thread::scope(|scope| {
            let handles = (0..8)
                .map(|entity| {
                    let registry = &registry;
                    let barrier = &barrier;
                    scope.spawn(move || {
                        barrier.wait();
                        let shared = registry.intern(CollidingSite { entity: 8, occurrence: 0 });
                        let own = registry.intern(CollidingSite { entity, occurrence: 0 });
                        (shared, own)
                    })
                })
                .collect::<Vec<_>>();
            handles.into_iter().map(|handle| handle.join().unwrap()).collect::<Vec<_>>()
        });
        let shared = ids[0].0;
        assert!(ids.iter().all(|(common, own)| *common == shared && *own != shared));
        assert_eq!(ids.iter().map(|(_, own)| *own).collect::<HashSet<_>>().len(), 8);
        assert_eq!(registry.sites.lock().unwrap().len(), 9);
    }

    #[test]
    fn dropping_a_registry_releases_its_site_keys() {
        let key = Arc::new(7_u32);
        let registry = KeySpaceRegistry::default();
        let id = registry.intern(key.clone());
        assert_eq!(Arc::strong_count(&key), 2);
        drop(registry);
        assert_eq!(Arc::strong_count(&key), 1);
        assert_ne!(KeySpaceRegistry::default().intern(key), id);
    }

    #[test]
    fn key_space_exhaustion_rejects_repeated_issuance_without_wrapping() {
        let issuer = KeySpaceIssuer { last: AtomicU64::new(u64::MAX - 1) };
        assert_eq!(issuer.fresh().as_u64(), u64::MAX);
        for _ in 0..2 {
            let failure = catch_unwind(|| issuer.fresh()).unwrap_err();
            let message = failure.downcast_ref::<String>().unwrap();
            assert!(message.starts_with("key-space identity range exhausted"));
            assert_eq!(issuer.last.load(Ordering::Relaxed), u64::MAX);
        }
    }

    #[test]
    fn interned_sites_replay_identifiers_and_keep_local_slots_distinct() {
        let registry = KeySpaceRegistry::default();
        let site = CollidingSite { entity: 7, occurrence: 0 };
        let space = registry.intern(site);
        let first: TestId = derived_id(space, 0);
        let next: TestId = derived_id(space, 1);
        let retry: TestId = derived_id(registry.intern(CollidingSite { occurrence: 1, ..site }), 0);
        assert_eq!(first, derived_id(registry.intern(site), 0));
        assert_eq!(first.key_space(), space);
        assert_eq!(first.raw().into_u32(), 0);
        assert_ne!(first, next);
        assert_ne!(first, retry);
        assert_ne!(first.concise(), next.concise());
        assert_ne!(first.concise_inner(), retry.concise_inner());
    }
}
