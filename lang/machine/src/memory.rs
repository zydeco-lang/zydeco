//! Manual storage and explicit retention of immutable allocations.
//!
//! Raw addresses carry no ownership, extent, or access permission. Unsafe callers
//! establish validity; accesses have no grant lookup or initialization bitmap.

use alloc::{
    alloc::{alloc, dealloc},
    vec::Vec,
};
use core::{alloc::Layout, ptr};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(i64)]
pub enum MemoryError {
    InvalidLayout = 0,
    AllocationFailed = 1,
    InvalidEncoding = 2,
}

impl MemoryError {
    pub fn message(self) -> &'static str {
        match self {
            | Self::InvalidLayout => "invalid memory layout",
            | Self::AllocationFailed => "memory allocation failed",
            | Self::InvalidEncoding => "invalid text encoding",
        }
    }
}

/// An unmanaged data pointer, including an interior or zero-length address.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct Address(*mut u8);

impl Address {
    pub const NULL: Self = Self(ptr::null_mut());
    pub fn from_pointer(pointer: *mut u8) -> Self {
        Self(pointer)
    }
    pub fn pointer(self) -> *mut u8 {
        self.0
    }
    pub fn expose(self) -> usize {
        self.0.expose_provenance()
    }
    pub fn from_exposed(address: usize) -> Self {
        Self(ptr::with_exposed_provenance_mut(address))
    }
    pub fn offset(self, displacement: i64) -> Self {
        Self(self.0.wrapping_offset(displacement as isize))
    }

    /// # Safety
    /// The range must be live and initialized for the returned slice's entire use.
    pub unsafe fn bytes<'a>(self, length: usize) -> &'a [u8] {
        if length == 0 { &[] } else { unsafe { core::slice::from_raw_parts(self.0, length) } }
    }
    /// # Safety
    /// The destination must be live and writable for `bytes.len()` bytes.
    pub unsafe fn write(self, bytes: &[u8]) {
        if !bytes.is_empty() {
            unsafe { ptr::copy(bytes.as_ptr(), self.0, bytes.len()) };
        }
    }
    /// # Safety
    /// The address must identify an initialized pointer slot in live storage.
    pub unsafe fn load_address(self) -> Self {
        Self(unsafe { self.0.cast::<*mut u8>().read_unaligned() })
    }
    /// # Safety
    /// The destination must be writable for one native pointer slot.
    pub unsafe fn store_address(self, value: Self) {
        unsafe { self.0.cast::<*mut u8>().write_unaligned(value.0) };
    }
}

/// A validated allocation request. Zero-sized requests allocate no backing storage.
#[derive(Clone, Copy, Debug)]
pub struct MemoryLayout(Layout);

impl MemoryLayout {
    pub fn for_request(size: i64, alignment: i64) -> Result<Self, MemoryError> {
        let size = usize::try_from(size).map_err(|_| MemoryError::InvalidLayout)?;
        let alignment = usize::try_from(alignment).map_err(|_| MemoryError::InvalidLayout)?;
        Layout::from_size_align(size, alignment).map(Self).map_err(|_| MemoryError::InvalidLayout)
    }
    pub fn size(self) -> usize {
        self.0.size()
    }
    pub fn alignment(self) -> usize {
        self.0.align()
    }
    pub fn allocate(self) -> Result<Address, MemoryError> {
        if self.size() == 0 {
            return Ok(Address::from_exposed(self.alignment()));
        }
        let pointer = unsafe { alloc(self.0) };
        if pointer.is_null() {
            Err(MemoryError::AllocationFailed)
        } else {
            Ok(Address::from_pointer(pointer))
        }
    }
    /// # Safety
    /// Supply the base of an allocation from this allocator with exactly this layout.
    /// It must not have been freed or retained. No alias may subsequently access it.
    pub unsafe fn deallocate(self, address: Address) {
        if self.size() != 0 {
            unsafe { dealloc(address.pointer(), self.0) };
        }
    }
}

#[derive(Debug)]
struct RetainedAllocation {
    address: Address,
    layout: MemoryLayout,
}
impl Drop for RetainedAllocation {
    fn drop(&mut self) {
        unsafe { self.layout.deallocate(self.address) };
    }
}

/// Explicit ownership for immutable bytes until one runtime instance is destroyed.
#[derive(Debug, Default)]
pub struct RetainedMemory {
    allocations: Vec<RetainedAllocation>,
}

impl RetainedMemory {
    /// # Safety
    /// Transfer one allocation from `layout`'s allocator with the original base and exact layout.
    /// Only initialized ranges may be published to readers; spare capacity may remain uninitialized.
    /// no alias may mutate or free it after success. Failure leaves ownership with the caller.
    pub unsafe fn retain(
        &mut self, address: Address, layout: MemoryLayout,
    ) -> Result<(), MemoryError> {
        if layout.size() != 0 {
            self.allocations.try_reserve(1).map_err(|_| MemoryError::AllocationFailed)?;
            self.allocations.push(RetainedAllocation { address, layout });
        }
        Ok(())
    }
    pub fn import(&mut self, bytes: &[u8]) -> Result<Address, MemoryError> {
        let size = i64::try_from(bytes.len()).map_err(|_| MemoryError::InvalidLayout)?;
        let layout = MemoryLayout::for_request(size, 1)?;
        let address = layout.allocate()?;
        unsafe { address.write(bytes) };
        if let Err(error) = unsafe { self.retain(address, layout) } {
            unsafe { layout.deallocate(address) };
            return Err(error);
        }
        Ok(address)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn allocation_validates_layout_without_creating_state() {
        for (size, alignment) in [(-1, 8), (1, 0), (1, -8), (1, 3), (i64::MAX, 8)] {
            assert!(matches!(
                MemoryLayout::for_request(size, alignment),
                Err(MemoryError::InvalidLayout)
            ));
        }
        for (size, alignment) in [(0, 64), (16, 64), (33, 8)] {
            let layout = MemoryLayout::for_request(size, alignment).unwrap();
            let address = layout.allocate().unwrap();
            assert_eq!(address.expose() % alignment as usize, 0);
            unsafe { layout.deallocate(address) };
        }
    }
    #[test]
    fn raw_slots_preserve_bits_and_native_pointer_values() {
        let layout = MemoryLayout::for_request(32, 16).unwrap();
        let address = layout.allocate().unwrap();
        let target = address.offset(19);
        unsafe {
            address.write(&[0xff; 8]);
            address.offset(8).store_address(target);
            assert_eq!(address.bytes(8), &[0xff; 8]);
            assert_eq!(address.offset(8).load_address(), target);
            layout.deallocate(address);
        }
    }
    #[test]
    fn retained_imports_remain_live_while_the_owner_grows() {
        let mut retained = RetainedMemory::default();
        let first = retained.import(b"hello").unwrap();
        for _ in 0..64 {
            retained.import(b"another allocation").unwrap();
        }
        assert_eq!(unsafe { first.offset(1).bytes(3) }, b"ell");
        assert_eq!(unsafe { Address::NULL.bytes(0) }, b"");
    }
}
