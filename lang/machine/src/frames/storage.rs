//! Word storage for the same nested frame protocol. Allocation policy is separate
//! from activation ownership: both implementations use relocatable word offsets.

use super::FrameError;
use crate::native::Word;
use alloc::vec::Vec;

mod sealed {
    pub trait Sealed {}
}

/// Storage may relocate only during `reserve`, before generated code reloads its
/// active base. Root addresses are borrowed until the next entry or owner drop.
/// Implementations are sealed because frame transitions rely on these guarantees.
pub trait Storage: sealed::Sealed {
    const EMPTY: Self;
    /// Preserve existing words on success and leave them untouched on failure.
    /// Make the complete `[base, base + words)` extent addressable on success.
    /// Previously reserved extents stay addressable for the lifetime of the store.
    fn reserve(&mut self, base: usize, words: usize) -> Result<(), FrameError>;
    fn base(&mut self) -> *mut Word;
    fn reserved_words(&self) -> usize;
}

/// Allocate the complete capacity on first entry and never move it afterwards.
pub struct Fixed<const WORDS: usize> {
    words: Vec<Word>,
}

impl<const WORDS: usize> sealed::Sealed for Fixed<WORDS> {}

impl<const WORDS: usize> Storage for Fixed<WORDS> {
    const EMPTY: Self = Self { words: Vec::new() };

    #[inline]
    fn reserve(&mut self, base: usize, words: usize) -> Result<(), FrameError> {
        let available = WORDS.saturating_sub(base);
        if words > available || base > WORDS {
            return Err(FrameError::Capacity { requested: words, available });
        }
        if self.words.is_empty() {
            self.words
                .try_reserve_exact(WORDS)
                .map_err(|_| FrameError::Allocation { words: WORDS })?;
            self.words.resize(WORDS, 0);
        }
        Ok(())
    }

    #[inline]
    fn base(&mut self) -> *mut Word {
        self.words.as_mut_ptr()
    }

    fn reserved_words(&self) -> usize {
        self.words.capacity()
    }
}

/// Grow a contiguous Rust allocation geometrically on demand. Suspended frames
/// contain offsets, not raw bases, so growth preserves their logical addresses.
/// Capacity is cached at the high-water mark; a tail chain does not keep growing.
/// This allocation is separate from the moving managed-value heap.
pub struct Growable<const LIMIT: usize = { isize::MAX as usize / size_of::<Word>() }> {
    words: Vec<Word>,
}

impl<const LIMIT: usize> sealed::Sealed for Growable<LIMIT> {}

impl<const LIMIT: usize> Storage for Growable<LIMIT> {
    const EMPTY: Self = Self { words: Vec::new() };

    #[inline]
    fn reserve(&mut self, base: usize, words: usize) -> Result<(), FrameError> {
        let available = LIMIT.saturating_sub(base);
        if words > available || base > LIMIT {
            return Err(FrameError::Capacity { requested: words, available });
        }
        let end = base.checked_add(words).ok_or(FrameError::Allocation { words })?;
        if end > self.words.len() {
            // Use an explicit capped geometric policy so the model's reservation
            // bound also holds when a deliberately small limit is used in tests.
            if end > self.words.capacity() {
                let capacity = end.max(self.words.capacity().saturating_mul(2).min(LIMIT));
                self.words
                    .try_reserve_exact(capacity - self.words.len())
                    .map_err(|_| FrameError::Allocation { words: capacity })?;
            }
            self.words.resize(end, 0);
        }
        Ok(())
    }

    #[inline]
    fn base(&mut self) -> *mut Word {
        self.words.as_mut_ptr()
    }

    fn reserved_words(&self) -> usize {
        self.words.capacity()
    }
}
