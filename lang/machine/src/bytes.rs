//! Immutable contiguous storage shared by the interpreter and native host.

use alloc::{rc::Rc, vec::Vec};

/// A visible byte window retaining its allocation. Alignment concerns the
/// window's address, rather than the allocation's original element type.
#[derive(Clone, Debug)]
pub struct ByteBuffer {
    buffer: Rc<[u8]>,
    start: usize,
    len: usize,
}

impl ByteBuffer {
    pub fn with_buffer(buffer: Rc<[u8]>) -> Self {
        let len = buffer.len();
        Self { buffer, start: 0, len }
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.buffer[self.start..self.start + self.len]
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn slice(&self, start: usize, len: usize) -> Option<Self> {
        let end = start.checked_add(len)?;
        (end <= self.len).then(|| Self {
            buffer: self.buffer.clone(),
            start: self.start + start,
            len,
        })
    }

    /// Preserve the visible contents at an address divisible by `alignment`.
    /// Invalid alignments and unrepresentable reservations fail before copying.
    pub fn aligned(&self, alignment: usize) -> Option<Self> {
        if !alignment.is_power_of_two() {
            return None;
        }
        if self.as_slice().as_ptr().align_offset(alignment) == 0 {
            return Some(self.clone());
        }
        let capacity = self.len.checked_add(alignment - 1)?;
        let mut bytes = Vec::new();
        bytes.try_reserve_exact(capacity).ok()?;
        bytes.resize(capacity, 0);
        let mut buffer: Rc<[u8]> = bytes.into();
        // Rc's allocation can differ from the temporary Vec's allocation.
        let start = buffer.as_ptr().align_offset(alignment);
        let end = start.checked_add(self.len)?;
        Rc::get_mut(&mut buffer)?.get_mut(start..end)?.copy_from_slice(self.as_slice());
        Some(Self { buffer, start, len: self.len })
    }
}

impl From<Vec<u8>> for ByteBuffer {
    fn from(bytes: Vec<u8>) -> Self {
        Self::with_buffer(bytes.into())
    }
}

impl PartialEq for ByteBuffer {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl PartialOrd for ByteBuffer {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.as_slice().cmp(other.as_slice()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    #[test]
    fn realignment_preserves_windows_and_rejects_invalid_requests() {
        let parent = ByteBuffer::from(vec![9, 1, 2, 3, 8]);
        let window = parent.slice(1, 3).unwrap();
        drop(parent);
        for alignment in [1, 2, 4, 8, 16, 64, 4096] {
            let aligned = window.aligned(alignment).unwrap();
            assert_eq!(aligned.as_slice(), &[1, 2, 3]);
            assert_eq!(aligned.as_slice().as_ptr().align_offset(alignment), 0);
        }
        for alignment in [0, 3, 6, usize::MAX] {
            assert!(window.aligned(alignment).is_none());
            assert_eq!(window.as_slice(), &[1, 2, 3]);
        }
        // A valid power of two can still exceed the addressable reservation.
        assert!(window.aligned(1usize << (usize::BITS - 1)).is_none());
        assert_eq!(window.as_slice(), &[1, 2, 3]);
        assert!(window.slice(3, 0).is_some());
        assert!(window.slice(4, 0).is_none());
        assert!(window.slice(usize::MAX, 1).is_none());
    }

    #[test]
    fn aligned_empty_windows_retain_valid_addresses() {
        let bytes = ByteBuffer::from(vec![1, 2, 3]);
        let empty = bytes.slice(3, 0).unwrap().aligned(64).unwrap();
        drop(bytes);
        assert!(empty.is_empty());
        assert_eq!(empty.as_slice().as_ptr().align_offset(64), 0);
    }
}
