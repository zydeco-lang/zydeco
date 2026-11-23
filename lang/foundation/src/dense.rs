//! Compact 4-byte dense arena identifiers, dense vectors, secondary maps, and bitsets.

use std::{
    borrow::Borrow,
    fmt,
    hash::Hash,
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

/// A 4-byte non-zero dense arena identifier convertible to and from zero-based slot offsets.
///
/// Keeping slot indices one-based reserves `0` as the niche so `Option<Id>` remains 4 bytes.
pub trait DenseId: Copy + Eq + Ord + Hash + fmt::Debug {
    /// Construct an identifier from its zero-based slot offset in a [`DenseArena`].
    fn from_index(index: usize) -> Self;

    /// Zero-based slot offset of this identifier.
    fn index(self) -> usize;

    /// Raw one-based `NonZeroU32` representation.
    fn raw_u32(self) -> u32;
}

/// Generic phantom-tagged 4-byte dense identifier.
///
/// Use [`define_idx!`] when downstream crates need distinct local newtype structs
/// (for example to attach inherent methods or trait implementations).
pub struct Idx<Tag>(NonZeroU32, PhantomData<fn() -> Tag>);

impl<Tag> Copy for Idx<Tag> {}

impl<Tag> Clone for Idx<Tag> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Tag> Eq for Idx<Tag> {}

impl<Tag> PartialEq for Idx<Tag> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<Tag> Ord for Idx<Tag> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<Tag> PartialOrd for Idx<Tag> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<Tag> Hash for Idx<Tag> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<Tag> fmt::Debug for Idx<Tag> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Idx({})", self.0.get())
    }
}

impl<Tag> DenseId for Idx<Tag> {
    #[inline]
    fn from_index(index: usize) -> Self {
        let one_based = index
            .checked_add(1)
            .and_then(|value| u32::try_from(value).ok())
            .expect("dense arena index exceeded u32 range");
        Self(NonZeroU32::new(one_based).expect("one-based index is nonzero"), PhantomData)
    }

    #[inline]
    fn index(self) -> usize {
        (self.0.get() - 1) as usize
    }

    #[inline]
    fn raw_u32(self) -> u32 {
        self.0.get()
    }
}

/// Define 4-byte non-zero dense identifier newtypes implementing [`DenseId`].
#[macro_export]
macro_rules! define_idx {
    ($( $(#[$attr:meta])* $vis:vis struct $name:ident; )*) => {
        $(
            $(#[$attr])*
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[repr(transparent)]
            $vis struct $name(std::num::NonZeroU32);

            impl std::fmt::Debug for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}({})", stringify!($name), self.0.get())
                }
            }

            impl $crate::dense::DenseId for $name {
                #[inline]
                fn from_index(index: usize) -> Self {
                    let one_based = index
                        .checked_add(1)
                        .and_then(|value| u32::try_from(value).ok())
                        .expect("dense arena index exceeded u32 range");
                    Self(
                        std::num::NonZeroU32::new(one_based)
                            .expect("one-based index is nonzero"),
                    )
                }

                #[inline]
                fn index(self) -> usize {
                    (self.0.get() - 1) as usize
                }

                #[inline]
                fn raw_u32(self) -> u32 {
                    self.0.get()
                }
            }
        )*
    };
}

/// Contiguous `Vec`-backed owning arena indexed by a 4-byte [`DenseId`].
#[derive(Clone, Debug)]
pub struct DenseArena<Id: DenseId, V> {
    items: Vec<V>,
    marker: PhantomData<fn() -> Id>,
}

impl<Id: DenseId, V> Default for DenseArena<Id, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: DenseId, V> DenseArena<Id, V> {
    pub fn new() -> Self {
        Self { items: Vec::new(), marker: PhantomData }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { items: Vec::with_capacity(capacity), marker: PhantomData }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn reserve_capacity(&mut self, additional: usize) {
        self.items.reserve(additional);
    }

    #[inline]
    pub fn alloc(&mut self, value: V) -> Id {
        let id = Id::from_index(self.items.len());
        self.items.push(value);
        id
    }

    /// Allocate a placeholder slot initialized with `Default` (for example for recursive definitions)
    /// to be overwritten later with [`Self::replace_existing`].
    #[inline]
    pub fn reserve(&mut self) -> Id
    where
        V: Default,
    {
        self.alloc(V::default())
    }

    #[inline]
    pub fn replace_existing(&mut self, id: Id, value: V) {
        self.items[id.index()] = value;
    }

    #[inline]
    pub fn get(&self, id: impl Borrow<Id>) -> Option<&V> {
        self.items.get(id.borrow().index())
    }

    #[inline]
    pub fn get_mut(&mut self, id: impl Borrow<Id>) -> Option<&mut V> {
        self.items.get_mut(id.borrow().index())
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (Id, &V)> {
        self.items.iter().enumerate().map(|(index, value)| (Id::from_index(index), value))
    }

    pub fn ids(&self) -> impl ExactSizeIterator<Item = Id> {
        (0..self.items.len()).map(Id::from_index)
    }

    pub fn values(&self) -> &[V] {
        &self.items
    }
}

impl<Id: DenseId, V> Index<Id> for DenseArena<Id, V> {
    type Output = V;

    #[inline]
    fn index(&self, id: Id) -> &Self::Output {
        &self.items[id.index()]
    }
}

impl<Id: DenseId, V> Index<&Id> for DenseArena<Id, V> {
    type Output = V;

    #[inline]
    fn index(&self, id: &Id) -> &Self::Output {
        &self.items[id.index()]
    }
}

impl<Id: DenseId, V> IndexMut<Id> for DenseArena<Id, V> {
    #[inline]
    fn index_mut(&mut self, id: Id) -> &mut Self::Output {
        &mut self.items[id.index()]
    }
}

impl<Id: DenseId, V> IndexMut<&Id> for DenseArena<Id, V> {
    #[inline]
    fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
        &mut self.items[id.index()]
    }
}

/// Optional per-ID side table backed by a contiguous vector of `Option<V>`.
#[derive(Clone, Debug)]
pub struct SecondaryMap<Id: DenseId, V> {
    slots: Vec<Option<V>>,
    len: usize,
    marker: PhantomData<fn() -> Id>,
}

impl<Id: DenseId, V> Default for SecondaryMap<Id, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: DenseId, V> SecondaryMap<Id, V> {
    pub fn new() -> Self {
        Self { slots: Vec::new(), len: 0, marker: PhantomData }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { slots: Vec::with_capacity(capacity), len: 0, marker: PhantomData }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn clear(&mut self) {
        self.slots.clear();
        self.len = 0;
    }

    #[inline]
    fn ensure_slot(&mut self, index: usize) -> &mut Option<V> {
        if index >= self.slots.len() {
            let grown = self.slots.capacity().saturating_add(self.slots.capacity() / 2).max(4);
            let required = index + 1;
            if self.slots.capacity() < required {
                self.slots.reserve_exact(required.max(grown) - self.slots.len());
            }
            self.slots.resize_with(required, || None);
        }
        &mut self.slots[index]
    }

    #[inline]
    pub fn insert_new(&mut self, id: Id, value: V) {
        let slot = self.ensure_slot(id.index());
        assert!(slot.is_none(), "duplicate secondary map entry for {id:?}");
        *slot = Some(value);
        self.len += 1;
    }

    #[inline]
    pub fn upsert(&mut self, id: Id, value: V) -> Option<V> {
        let slot = self.ensure_slot(id.index());
        let previous = slot.replace(value);
        if previous.is_none() {
            self.len += 1;
        }
        previous
    }

    #[inline]
    pub fn replace_existing(&mut self, id: Id, value: V) -> V {
        let slot = self.slots.get_mut(id.index()).expect("secondary map entry does not exist");
        slot.replace(value).expect("secondary map entry does not exist")
    }

    #[inline]
    pub fn insert_or_get(&mut self, id: Id, value: V) -> Option<V>
    where
        V: Clone,
    {
        let slot = self.ensure_slot(id.index());
        match slot {
            | Some(existing) => Some(existing.clone()),
            | None => {
                *slot = Some(value);
                self.len += 1;
                None
            }
        }
    }

    #[inline]
    pub fn remove(&mut self, id: impl Borrow<Id>) -> Option<V> {
        let slot = self.slots.get_mut(id.borrow().index())?;
        let previous = slot.take()?;
        self.len -= 1;
        Some(previous)
    }

    #[inline]
    pub fn contains_key(&self, id: impl Borrow<Id>) -> bool {
        self.get(id).is_some()
    }

    #[inline]
    pub fn get(&self, id: impl Borrow<Id>) -> Option<&V> {
        self.slots.get(id.borrow().index())?.as_ref()
    }

    #[inline]
    pub fn get_mut(&mut self, id: impl Borrow<Id>) -> Option<&mut V> {
        self.slots.get_mut(id.borrow().index())?.as_mut()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id, &V)> {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| slot.as_ref().map(|value| (Id::from_index(index), value)))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Id, &mut V)> {
        self.slots
            .iter_mut()
            .enumerate()
            .filter_map(|(index, slot)| slot.as_mut().map(|value| (Id::from_index(index), value)))
    }
}

impl<Id: DenseId, V: PartialEq> PartialEq for SecondaryMap<Id, V> {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }
        let max_len = self.slots.len().max(other.slots.len());
        (0..max_len).all(|index| {
            let left = self.slots.get(index).and_then(Option::as_ref);
            let right = other.slots.get(index).and_then(Option::as_ref);
            left == right
        })
    }
}

impl<Id: DenseId, V: Eq> Eq for SecondaryMap<Id, V> {}

impl<Id: DenseId, V> Index<Id> for SecondaryMap<Id, V> {
    type Output = V;

    #[inline]
    fn index(&self, id: Id) -> &Self::Output {
        self.get(id).expect("key not found in SecondaryMap")
    }
}

impl<Id: DenseId, V> Index<&Id> for SecondaryMap<Id, V> {
    type Output = V;

    #[inline]
    fn index(&self, id: &Id) -> &Self::Output {
        self.get(id).expect("key not found in SecondaryMap")
    }
}

impl<Id: DenseId, V> IndexMut<Id> for SecondaryMap<Id, V> {
    #[inline]
    fn index_mut(&mut self, id: Id) -> &mut Self::Output {
        self.get_mut(id).expect("key not found in SecondaryMap")
    }
}

impl<Id: DenseId, V> IndexMut<&Id> for SecondaryMap<Id, V> {
    #[inline]
    fn index_mut(&mut self, id: &Id) -> &mut Self::Output {
        self.get_mut(id).expect("key not found in SecondaryMap")
    }
}

/// Compact bitset over a [`DenseId`] domain (1 bit per ID instead of hash-table buckets).
#[derive(Clone, Debug)]
pub struct IdBitSet<Id: DenseId> {
    words: Vec<u64>,
    len: usize,
    marker: PhantomData<fn() -> Id>,
}

impl<Id: DenseId> Default for IdBitSet<Id> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Id: DenseId> IdBitSet<Id> {
    pub fn new() -> Self {
        Self { words: Vec::new(), len: 0, marker: PhantomData }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn clear(&mut self) {
        self.words.clear();
        self.len = 0;
    }

    #[inline]
    pub fn insert(&mut self, id: Id) -> bool {
        self.ensure(id)
    }

    #[inline]
    pub fn ensure(&mut self, id: Id) -> bool {
        let index = id.index();
        let word = index >> 6;
        let bit = 1u64 << (index & 63);
        if word >= self.words.len() {
            self.words.resize(word + 1, 0);
        }
        let slot = &mut self.words[word];
        let inserted = (*slot & bit) == 0;
        if inserted {
            *slot |= bit;
            self.len += 1;
        }
        inserted
    }

    #[inline]
    pub fn contains(&self, id: impl Borrow<Id>) -> bool {
        let index = id.borrow().index();
        self.words.get(index >> 6).is_some_and(|word| (word & (1u64 << (index & 63))) != 0)
    }

    #[inline]
    pub fn remove(&mut self, id: impl Borrow<Id>) -> bool {
        let index = id.borrow().index();
        let Some(slot) = self.words.get_mut(index >> 6) else {
            return false;
        };
        let bit = 1u64 << (index & 63);
        let removed = (*slot & bit) != 0;
        if removed {
            *slot &= !bit;
            self.len -= 1;
        }
        removed
    }

    pub fn iter(&self) -> impl Iterator<Item = Id> + '_ {
        self.words.iter().enumerate().flat_map(|(word_idx, word)| {
            let mut bits = *word;
            std::iter::from_fn(move || {
                if bits == 0 {
                    return None;
                }
                let tz = bits.trailing_zeros() as usize;
                bits &= bits - 1;
                Some(Id::from_index((word_idx << 6) + tz))
            })
        })
    }
}

impl<Id: DenseId> PartialEq for IdBitSet<Id> {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }
        let max_len = self.words.len().max(other.words.len());
        (0..max_len).all(|index| {
            let left = self.words.get(index).copied().unwrap_or(0);
            let right = other.words.get(index).copied().unwrap_or(0);
            left == right
        })
    }
}

impl<Id: DenseId> Eq for IdBitSet<Id> {}

impl<Id: DenseId> Extend<Id> for IdBitSet<Id> {
    fn extend<T: IntoIterator<Item = Id>>(&mut self, iter: T) {
        for id in iter {
            self.insert(id);
        }
    }
}

impl<Id: DenseId> FromIterator<Id> for IdBitSet<Id> {
    fn from_iter<T: IntoIterator<Item = Id>>(iter: T) -> Self {
        let mut set = Self::new();
        set.extend(iter);
        set
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    define_idx! {
        pub struct SampleId;
        struct PrivateId;
    }

    #[test]
    fn dense_ids_are_four_bytes_with_option_niche() {
        assert_eq!(std::mem::size_of::<SampleId>(), 4);
        assert_eq!(std::mem::size_of::<Option<SampleId>>(), 4);
        assert_eq!(std::mem::size_of::<Idx<PrivateId>>(), 4);
        assert_eq!(std::mem::size_of::<Option<Idx<PrivateId>>>(), 4);
    }

    #[test]
    fn dense_arena_secondary_map_and_bitset_round_trip() {
        let mut arena: DenseArena<SampleId, &str> = DenseArena::new();
        let first = arena.alloc("alpha");
        let second = arena.alloc("beta");
        assert_eq!(format!("{first:?}"), "SampleId(1)");
        assert_eq!(arena[first], "alpha");
        assert_eq!(arena[&second], "beta");
        assert_eq!(arena.get(first), Some(&"alpha"));
        assert_eq!(arena.get(&second), Some(&"beta"));

        let mut secondary: SecondaryMap<SampleId, usize> = SecondaryMap::new();
        secondary.insert_new(second, 20);
        assert_eq!(secondary.get(first), None);
        assert_eq!(secondary[&second], 20);
        assert_eq!(secondary.upsert(second, 22), Some(20));
        assert_eq!(secondary[second], 22);
        assert_eq!(secondary.remove(second), Some(22));
        assert!(secondary.is_empty());

        let mut bits: IdBitSet<SampleId> = IdBitSet::new();
        assert!(bits.ensure(second));
        assert!(!bits.ensure(second));
        assert!(bits.ensure(first));
        assert!(bits.contains(first));
        assert!(bits.contains(&second));
        assert_eq!(bits.iter().collect::<Vec<_>>(), vec![first, second]);
        assert!(bits.remove(first));
        assert!(!bits.contains(first));
        assert_eq!(bits.len(), 1);
    }

    #[test]
    fn secondary_map_and_bitset_equality_ignore_trailing_empty_slots_after_removals() {
        let id0 = SampleId::from_index(0);
        let id1 = SampleId::from_index(1);
        let id70 = SampleId::from_index(70);

        let mut left_map = SecondaryMap::new();
        left_map.insert_new(id0, 10);
        left_map.insert_new(id70, 99);
        assert!(left_map.contains_key(id0));
        assert_eq!(left_map.remove(id70), Some(99));
        for (_, value) in left_map.iter_mut() {
            *value += 1;
        }

        let mut right_map = SecondaryMap::new();
        right_map.insert_new(id0, 11);
        assert_eq!(left_map, right_map);

        let mut left_bits: IdBitSet<SampleId> = [id0, id1, id70].into_iter().collect();
        assert!(left_bits.remove(id70));
        let mut right_bits = IdBitSet::new();
        right_bits.extend([id0, id1]);
        assert_eq!(left_bits, right_bits);
    }
}
