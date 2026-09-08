//! Reusable active storage with compact, nested suspension snapshots.
//! Captured values live in a separate Rust allocation, outside the managed heap.
//! Snapshots are materialized only when entry will reuse the active region.

use super::{Environment, FrameError, Layout, LayoutId, Token, sealed, storage::Storage};
use crate::{native::Word, word::RuntimeWord};
use alloc::vec::Vec;

#[derive(Clone, Copy)]
enum Residence {
    Active,
    Saved { base: usize },
}

struct Fragment {
    layout: Layout,
    token: Token,
    slots: &'static [Word],
    residence: Residence,
}

/// Copy a suspension's captures only when a fresh entry reuses the active region.
///
/// The two stores grow independently and cache their historical capacities. The
/// snapshot frontier follows token nesting, so return reclaims words without an
/// allocation per continuation. Multiple suspensions of one activation have
/// independent snapshots once saved, including copies of overlapping captures.
/// Before entry they share active slots, so host returns need no capture copying.
/// This trades copying for compact suspended storage; it does not add escaping
/// or duplicable machine continuations to the native protocol.
pub struct Fragments<S: Storage> {
    scratch: S,
    saved: S,
    active: Option<Layout>,
    fragments: Vec<Fragment>,
    // Saved fragments form a prefix; the remaining suffix refers to active slots.
    saved_count: usize,
    saved_words: usize,
    next_token: u64,
    high_water: usize,
}

impl<S: Storage> Fragments<S> {
    pub const EMPTY: Self = Self {
        scratch: S::EMPTY,
        saved: S::EMPTY,
        active: None,
        fragments: Vec::new(),
        saved_count: 0,
        saved_words: 0,
        next_token: 0,
        high_water: 0,
    };

    /// Logical active layout plus materialized snapshots, excluding metadata.
    #[inline]
    pub fn used_words(&self) -> usize {
        self.active.map_or(0, |layout| layout.words) + self.saved_words
    }

    pub fn high_water_words(&self) -> usize {
        self.high_water
    }

    /// Sum of both word buffers' capacities, excluding allocator rounding.
    pub fn reserved_words(&self) -> usize {
        self.scratch.reserved_words() + self.saved.reserved_words()
    }

    /// Rust owner plus allocated fragment records, excluding word buffers and
    /// allocator bookkeeping. Includes unused vector capacity after reclamation.
    pub fn metadata_reserved_bytes(&self) -> usize {
        size_of::<Self>() + self.fragments.capacity() * size_of::<Fragment>()
    }

    #[inline]
    fn active(&self, expected: LayoutId) -> Result<Layout, FrameError> {
        let layout = self.active.ok_or(FrameError::MissingActivation)?;
        Self::check_layout(layout, expected)?;
        Ok(layout)
    }

    #[inline]
    fn check_layout(layout: Layout, expected: LayoutId) -> Result<(), FrameError> {
        if layout.id == expected {
            Ok(())
        } else {
            Err(FrameError::WrongLayout { expected, actual: layout.id })
        }
    }

    #[inline]
    fn check_slots(layout: Layout, slots: &[Word]) -> Result<(), FrameError> {
        slots.iter().try_for_each(|&slot| {
            if slot < layout.words {
                Ok(())
            } else {
                Err(FrameError::InvalidSlot { slot, words: layout.words })
            }
        })
    }
}

impl<S: Storage> sealed::Sealed for Fragments<S> {}

impl<S: Storage> Environment for Fragments<S> {
    #[inline]
    fn enter(&mut self, layout: Layout) -> Result<*mut Word, FrameError> {
        let mut end = self.saved_words;
        if self.saved_count != self.fragments.len() {
            let pending = &self.fragments[self.saved_count..];
            let words = pending.iter().try_fold(0usize, |words, fragment| {
                words
                    .checked_add(fragment.slots.len())
                    .ok_or(FrameError::Allocation { words: usize::MAX })
            })?;
            self.saved.reserve(self.saved_words, words)?;
            let source = self.scratch.base();
            let destination = self.saved.base();
            for fragment in pending {
                for &slot in fragment.slots {
                    // Stage captures before scratch reservation can relocate
                    // their source. On failure this unpublished suffix is dead.
                    unsafe { destination.add(end).write(source.add(slot).read()) };
                    end += 1;
                }
            }
        }
        self.scratch.reserve(0, layout.words)?;
        // Both reservations succeeded. Until here, saved tokens and the active
        // base survive failure; root addresses expire on any attempted transition.
        let mut base = self.saved_words;
        for fragment in &mut self.fragments[self.saved_count..] {
            fragment.residence = Residence::Saved { base };
            base += fragment.slots.len();
        }
        self.saved_count = self.fragments.len();
        self.saved_words = end;
        // Include the temporary overlap of the outgoing active layout and its
        // completed snapshots, then the new active layout's demand.
        self.high_water = self.high_water.max(self.used_words());
        self.active = Some(layout);
        self.high_water = self.high_water.max(self.used_words());
        Ok(self.scratch.base())
    }

    #[inline]
    fn suspend(&mut self, layout: LayoutId, slots: &'static [Word]) -> Result<Token, FrameError> {
        let layout = self.active(layout)?;
        Self::check_slots(layout, slots)?;
        let token = RuntimeWord::unsigned(self.next_token).ok_or(FrameError::TokenOverflow)?;
        let token = Token(Word::try_from(token).map_err(|_| FrameError::TokenOverflow)?);
        self.fragments.push(Fragment { layout, token, slots, residence: Residence::Active });
        self.next_token += 1;
        Ok(token)
    }

    #[inline]
    fn resume(&mut self, layout: LayoutId, token: Token) -> Result<*mut Word, FrameError> {
        let fragment = self.fragments.last().ok_or(FrameError::InvalidResumption)?;
        if fragment.token != token {
            return Err(FrameError::InvalidResumption);
        }
        Self::check_layout(fragment.layout, layout)?;
        let destination = self.scratch.base();
        if let Residence::Saved { base } = fragment.residence {
            let source = self.saved.base().wrapping_add(base);
            for (index, &slot) in fragment.slots.iter().enumerate() {
                // Scratch keeps every previously reserved extent addressable,
                // even across smaller layouts. Neither copy can collect.
                unsafe { destination.add(slot).write(source.add(index).read()) };
            }
            self.saved_words = base;
            self.saved_count -= 1;
        }
        self.active = Some(fragment.layout);
        self.fragments.pop();
        Ok(destination)
    }

    #[inline]
    fn roots(&mut self, layout: LayoutId, slots: &[Word]) -> Result<Vec<*mut Word>, FrameError> {
        let active = self.active(layout)?;
        Self::check_slots(active, slots)?;
        let mut indices = slots
            .iter()
            .copied()
            .chain(
                self.fragments[self.saved_count..]
                    .iter()
                    .flat_map(|fragment| fragment.slots.iter().copied()),
            )
            .collect::<Vec<_>>();
        indices.sort_unstable();
        indices.dedup();
        let active_base = self.scratch.base();
        let saved_base = self.saved.base();
        Ok(indices
            .into_iter()
            .map(|slot| active_base.wrapping_add(slot))
            .chain((0..self.saved_words).map(|slot| saved_base.wrapping_add(slot)))
            .collect())
    }
}

#[cfg(test)]
mod tests {
    use super::super::storage::{Fixed, Growable};
    use super::*;

    const CALLER: Layout = Layout { id: LayoutId(1), words: 8 };
    const CALLEE: Layout = Layout { id: LayoutId(2), words: 2 };

    #[test]
    fn overlapping_snapshots_restore_after_reuse_and_root_updates() {
        let mut frames = Fragments::<Growable>::EMPTY;
        let caller = frames.enter(CALLER).unwrap();
        unsafe {
            caller.add(1).write(71);
            caller.add(7).write(93);
        }
        let outer = frames.suspend(CALLER.id, &[1]).unwrap();
        let inner = frames.suspend(CALLER.id, &[1, 7]).unwrap();
        assert_eq!(frames.used_words(), 8);
        assert_eq!(frames.saved_words, 0);
        assert_eq!(
            frames.roots(CALLER.id, &[1]).unwrap(),
            [caller.wrapping_add(1), caller.wrapping_add(7)]
        );
        let callee = frames.enter(CALLEE).unwrap();
        unsafe { callee.add(1).write(105) };
        assert_eq!(frames.used_words(), 5);
        let roots = frames.roots(CALLEE.id, &[1, 1]).unwrap();
        assert_eq!(roots.len(), 4);
        assert_eq!(
            roots.iter().map(|p| unsafe { p.read() }).collect::<Vec<_>>(),
            [105, 71, 71, 93]
        );
        // Simulate moving the object shared by both snapshots. Every copy must
        // be published and updated; deduplicating equal values would be wrong.
        for root in roots {
            unsafe {
                if root.read() == 71 {
                    root.write(73)
                }
            };
        }
        assert_eq!(frames.resume(CALLER.id, outer), Err(FrameError::InvalidResumption));
        let restored = frames.resume(CALLER.id, inner).unwrap();
        assert_eq!(unsafe { restored.add(1).read() }, 73);
        assert_eq!(unsafe { restored.add(7).read() }, 93);
        assert_eq!(frames.used_words(), 9);
        assert_eq!(frames.resume(CALLER.id, inner), Err(FrameError::InvalidResumption));
        let callee = frames.enter(CALLEE).unwrap();
        unsafe { callee.add(1).write(107) };
        let restored = frames.resume(CALLER.id, outer).unwrap();
        assert_eq!(unsafe { restored.add(1).read() }, 73);
        assert_eq!(frames.used_words(), 8);
    }

    #[test]
    fn growth_empty_snapshots_and_tail_chains_keep_bounded_storage() {
        let mut frames = Fragments::<Growable>::EMPTY;
        let caller = frames.enter(CALLER).unwrap();
        unsafe { caller.add(7).write(71) };
        let outer = frames.suspend(CALLER.id, &[7]).unwrap();
        let empty = frames.suspend(CALLER.id, &[]).unwrap();
        let large = Layout { id: CALLEE.id, words: 200_000 };
        let base = frames.enter(large).unwrap();
        assert_eq!(frames.used_words(), 200_001);
        let capacity = frames.reserved_words();
        for _ in 0..100_000 {
            assert_eq!(frames.enter(large).unwrap(), base);
        }
        assert_eq!(frames.high_water_words(), 200_001);
        assert_eq!(frames.reserved_words(), capacity);
        assert_eq!(frames.fragments.len(), 2);
        frames.resume(CALLER.id, empty).unwrap();
        let restored = frames.resume(CALLER.id, outer).unwrap();
        assert_eq!(unsafe { restored.add(7).read() }, 71);
        assert_eq!(frames.roots(CALLER.id, &[]).unwrap(), []);
        assert_eq!(frames.used_words(), 8);
    }

    #[test]
    fn rejected_operations_preserve_tokens_bases_and_values() {
        let mut frames = Fragments::<Fixed<8>>::EMPTY;
        assert_eq!(frames.suspend(CALLER.id, &[]), Err(FrameError::MissingActivation));
        let caller = frames.enter(CALLER).unwrap();
        unsafe { caller.write(71) };
        assert_eq!(
            frames.suspend(CALLER.id, &[8]),
            Err(FrameError::InvalidSlot { slot: 8, words: 8 })
        );
        assert_eq!(
            frames.roots(CALLER.id, &[8]),
            Err(FrameError::InvalidSlot { slot: 8, words: 8 })
        );
        let token = frames.suspend(CALLER.id, &[0]).unwrap();
        assert_eq!(
            frames.enter(Layout { words: 9, ..CALLEE }),
            Err(FrameError::Capacity { requested: 9, available: 8 })
        );
        assert_eq!(
            frames.resume(CALLEE.id, token),
            Err(FrameError::WrongLayout { expected: CALLEE.id, actual: CALLER.id })
        );
        assert_eq!(unsafe { caller.read() }, 71);
        assert_eq!(frames.fragments.len(), 1);
        assert_eq!(frames.saved_words, 0);
        frames.next_token = u64::MAX;
        assert_eq!(frames.suspend(CALLER.id, &[0]), Err(FrameError::TokenOverflow));
        assert_eq!(frames.resume(CALLER.id, token).unwrap(), caller);
        assert_eq!(unsafe { caller.read() }, 71);
        assert_eq!(frames.resume(CALLER.id, token), Err(FrameError::InvalidResumption));
    }

    #[test]
    fn immediate_resumptions_share_active_roots_without_allocating_snapshots() {
        let mut frames = Fragments::<Growable>::EMPTY;
        let caller = frames.enter(CALLER).unwrap();
        unsafe { caller.add(7).write(71) };
        for _ in 0..10_000 {
            let outer = frames.suspend(CALLER.id, &[7]).unwrap();
            let inner = frames.suspend(CALLER.id, &[7]).unwrap();
            let roots = frames.roots(CALLER.id, &[]).unwrap();
            assert_eq!(roots, [caller.wrapping_add(7)]);
            unsafe { roots[0].write(73) };
            assert_eq!(frames.resume(CALLER.id, inner).unwrap(), caller);
            assert_eq!(frames.resume(CALLER.id, outer).unwrap(), caller);
            assert_eq!(unsafe { caller.add(7).read() }, 73);
        }
        assert_eq!(frames.saved.reserved_words(), 0);
        assert_eq!(frames.saved_words, 0);
        assert_eq!(frames.high_water_words(), CALLER.words);
    }

    #[test]
    fn failed_materialization_preserves_active_and_saved_captures() {
        let mut frames = Fragments::<Growable<8>>::EMPTY;
        let caller = frames.enter(CALLER).unwrap();
        unsafe { caller.write(71) };
        let outer = frames.suspend(CALLER.id, &[0]).unwrap();
        let callee = frames.enter(CALLEE).unwrap();
        unsafe { callee.write(93) };
        let inner = frames.suspend(CALLEE.id, &[0]).unwrap();
        // The snapshot buffer grows successfully, but the active layout cannot.
        // Previously returned root addresses expire at the attempted transition.
        assert_eq!(
            frames.enter(Layout { words: 9, ..CALLER }),
            Err(FrameError::Capacity { requested: 9, available: 8 })
        );
        assert_eq!(frames.saved_count, 1);
        assert_eq!(frames.saved_words, 1);
        assert_eq!(
            frames
                .roots(CALLEE.id, &[])
                .unwrap()
                .iter()
                .map(|p| unsafe { p.read() })
                .collect::<Vec<_>>(),
            [93, 71]
        );
        assert_eq!(frames.resume(CALLEE.id, inner).unwrap(), callee);
        let inner = frames.suspend(CALLEE.id, &[0; 8]).unwrap();
        assert_eq!(frames.enter(CALLER), Err(FrameError::Capacity { requested: 8, available: 7 }));
        assert_eq!(frames.saved_count, 1);
        assert_eq!(frames.saved_words, 1);
        assert_eq!(frames.resume(CALLEE.id, inner).unwrap(), callee);
        assert_eq!(unsafe { callee.read() }, 93);
        let restored = frames.resume(CALLER.id, outer).unwrap();
        assert_eq!(unsafe { restored.read() }, 71);
    }
}
