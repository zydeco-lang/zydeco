//! Mutable fixed-capacity storage behind checked, non-reused resource handles.

use crate::memory::{Address, MemoryState};
use alloc::{collections::BTreeMap, vec::Vec};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(i64)]
pub enum BufferError {
    InvalidLayout = 0,
    Closed = 1,
    Bounds = 2,
    AllocationFailed = 3,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BufferHandle(usize);

impl BufferHandle {
    pub fn with_raw(raw: usize) -> Self {
        Self(raw)
    }

    pub fn raw(self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub(crate) struct Buffer {
    pub(crate) allocation: Vec<u8>,
    pub(crate) start: usize,
    pub(crate) len: usize,
    pub(crate) initialized: Vec<bool>,
    pub(crate) pointers: BTreeMap<usize, Address>,
}

impl Buffer {
    fn new(size: i64, alignment: i64) -> Result<Self, BufferError> {
        let len = usize::try_from(size).map_err(|_| BufferError::InvalidLayout)?;
        let alignment = usize::try_from(alignment).map_err(|_| BufferError::InvalidLayout)?;
        if !alignment.is_power_of_two() {
            return Err(BufferError::InvalidLayout);
        }
        let capacity = len.checked_add(alignment - 1).ok_or(BufferError::AllocationFailed)?;
        // An empty allocation still needs an address with the requested alignment.
        let capacity = capacity.max(1);
        let mut allocation = Vec::new();
        allocation.try_reserve_exact(capacity).map_err(|_| BufferError::AllocationFailed)?;
        allocation.resize(capacity, 0);
        let start = allocation.as_ptr().align_offset(alignment);
        let mut initialized = Vec::new();
        initialized.try_reserve_exact(len).map_err(|_| BufferError::AllocationFailed)?;
        initialized.resize(len, true);
        Ok(Self { allocation, start, len, initialized, pointers: BTreeMap::new() })
    }

    fn range(&self, offset: i64, length: usize) -> Result<core::ops::Range<usize>, BufferError> {
        let offset = usize::try_from(offset).map_err(|_| BufferError::Bounds)?;
        let end =
            offset.checked_add(length).filter(|end| *end <= self.len).ok_or(BufferError::Bounds)?;
        Ok(self.start + offset..self.start + end)
    }
}

#[derive(Debug, Default)]
pub struct BufferArena {
    next: usize,
    pub(crate) buffers: BTreeMap<BufferHandle, Buffer>,
    pub(crate) frozen: BTreeMap<BufferHandle, Buffer>,
    pub(crate) memory: MemoryState,
}

impl BufferArena {
    pub fn allocate_uninitialized(
        &mut self, size: i64, alignment: i64,
    ) -> Result<BufferHandle, BufferError> {
        let handle = self.allocate(size, alignment)?;
        self.buffers.get_mut(&handle).expect("allocated buffer").initialized.fill(false);
        Ok(handle)
    }

    pub fn allocate(&mut self, size: i64, alignment: i64) -> Result<BufferHandle, BufferError> {
        let next = self
            .next
            .checked_add(1)
            .filter(|next| *next <= i64::MAX as usize / 2)
            .ok_or(BufferError::AllocationFailed)?;
        let buffer = Buffer::new(size, alignment)?;
        let handle = BufferHandle(self.next);
        self.buffers.insert(handle, buffer);
        self.next = next;
        Ok(handle)
    }

    pub fn write(
        &mut self, handle: BufferHandle, offset: i64, source: &[u8],
    ) -> Result<(), BufferError> {
        let buffer = self.buffers.get_mut(&handle).ok_or(BufferError::Closed)?;
        let range = buffer.range(offset, source.len())?;
        let relative = range.start - buffer.start..range.end - buffer.start;
        buffer.invalidate_pointers(relative.clone());
        buffer.initialized[relative].fill(true);
        buffer.allocation[range].copy_from_slice(source);
        Ok(())
    }

    pub fn close(&mut self, handle: BufferHandle) -> Result<(), BufferError> {
        self.buffers.remove(&handle).map(|_| ()).ok_or(BufferError::Closed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn failed_requests_preserve_the_allocation_and_closed_handles_are_not_reused() {
        let mut arena = BufferArena::default();
        for (size, alignment) in [(-1, 8), (1, 0), (1, -8), (1, 3)] {
            assert_eq!(arena.allocate(size, alignment), Err(BufferError::InvalidLayout));
        }
        assert!(arena.buffers.is_empty());
        assert_eq!(arena.next, 0);
        let handle = arena.allocate(4, 8).unwrap();
        arena.write(handle, 0, &[1, 2, 3, 4]).unwrap();
        for offset in [-1, 3, i64::MAX] {
            assert_eq!(arena.write(handle, offset, &[9, 9]), Err(BufferError::Bounds));
        }
        let buffer = &arena.buffers[&handle];
        assert_eq!(&buffer.allocation[buffer.start..buffer.start + buffer.len], &[1, 2, 3, 4]);
        arena.write(handle, 4, &[]).unwrap();
        arena.close(handle).unwrap();
        assert_eq!(arena.write(handle, 0, &[]), Err(BufferError::Closed));
        assert_eq!(arena.close(handle), Err(BufferError::Closed));
        assert_ne!(arena.allocate(4, 8).unwrap(), handle);
    }
}
