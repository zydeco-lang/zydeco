//! Checked addresses and revocable range grants over owned buffer allocations.

use crate::buffer::{Buffer, BufferArena, BufferError, BufferHandle};
use alloc::collections::BTreeMap;
use core::ops::Range;

/// Stable failures at the memory capability boundary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(i64)]
pub enum MemoryError {
    Closed = 0,
    Bounds = 1,
    Permission = 2,
    Overflow = 3,
    Alignment = 4,
    Uninitialized = 5,
    InvalidValue = 6,
    Unavailable = 7,
    AllocationFailed = 8,
}

impl MemoryError {
    pub fn message(self) -> &'static str {
        match self {
            | Self::Closed => "memory capability is closed",
            | Self::Bounds => "memory access is out of bounds",
            | Self::Permission => "memory permission denied",
            | Self::Overflow => "memory address or size overflow",
            | Self::Alignment => "memory alignment is invalid",
            | Self::Uninitialized => "memory is not initialized",
            | Self::InvalidValue => "invalid memory value or layout",
            | Self::Unavailable => "memory service is unavailable",
            | Self::AllocationFailed => "memory allocation failed",
        }
    }
}

impl From<BufferError> for MemoryError {
    fn from(error: BufferError) -> Self {
        match error {
            | BufferError::InvalidLayout => Self::InvalidValue,
            | BufferError::Closed => Self::Closed,
            | BufferError::Bounds => Self::Bounds,
            | BufferError::AllocationFailed => Self::AllocationFailed,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Permission {
    Read,
    Write,
    ReadWrite,
}

impl TryFrom<i64> for Permission {
    type Error = MemoryError;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            | 1 => Ok(Self::Read),
            | 2 => Ok(Self::Write),
            | 3 => Ok(Self::ReadWrite),
            | _ => Err(MemoryError::InvalidValue),
        }
    }
}

macro_rules! handle {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
        pub struct $name(usize);

        impl $name {
            pub fn with_raw(raw: usize) -> Self {
                Self(raw)
            }

            pub fn raw(self) -> usize {
                self.0
            }
        }
    };
}

handle!(AddressHandle);
handle!(AccessHandle);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Address {
    allocation: BufferHandle,
    offset: usize,
}

#[derive(Debug)]
struct Grant {
    allocation: BufferHandle,
    range: Range<usize>,
    permission: Permission,
    owner: OwnerState,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum OwnerState {
    Mutable,
    Frozen,
}

/// Host handles retain allocation identities, even when a machine address is reused.
#[derive(Debug, Default)]
pub(crate) struct MemoryState {
    next_grant: usize,
    grants: BTreeMap<AccessHandle, Grant>,
    addresses: BTreeMap<AddressHandle, Address>,
    address_handles: BTreeMap<Address, AddressHandle>,
}

impl MemoryState {
    fn grant(&mut self, grant: Grant) -> Result<AccessHandle, MemoryError> {
        let next = self
            .next_grant
            .checked_add(1)
            .filter(|next| *next <= i64::MAX as usize / 2)
            .ok_or(MemoryError::Overflow)?;
        let handle = AccessHandle(self.next_grant);
        self.grants.insert(handle, grant);
        self.next_grant = next;
        Ok(handle)
    }

    fn intern(&mut self, address: Address) -> Result<AddressHandle, MemoryError> {
        if let Some(handle) = self.address_handles.get(&address) {
            return Ok(*handle);
        }
        let raw = self.addresses.len();
        if raw >= i64::MAX as usize / 2 {
            return Err(MemoryError::Overflow);
        }
        let handle = AddressHandle(raw);
        self.addresses.insert(handle, address);
        self.address_handles.insert(address, handle);
        Ok(handle)
    }
}

impl Buffer {
    fn check_memory(
        &self, offset: usize, size: usize, alignment: usize,
    ) -> Result<(), MemoryError> {
        if !alignment.is_power_of_two() {
            return Err(MemoryError::Alignment);
        }
        let end = offset.checked_add(size).ok_or(MemoryError::Overflow)?;
        if end > self.len {
            return Err(MemoryError::Bounds);
        }
        let location = self.allocation.as_ptr().wrapping_add(self.start + offset);
        if location.align_offset(alignment) != 0 {
            return Err(MemoryError::Alignment);
        }
        Ok(())
    }

    pub(crate) fn invalidate_pointers(&mut self, range: Range<usize>) {
        if !range.is_empty() {
            self.pointers.retain(|offset, _| *offset + 8 <= range.start || *offset >= range.end);
        }
    }
}

impl BufferArena {
    /// Transfer initialized storage into an immutable owner. Old mutable grants
    /// and every Buffer alias close; the new read grant retains the frozen owner.
    pub fn freeze_memory(&mut self, owner: BufferHandle) -> Result<AccessHandle, MemoryError> {
        let buffer = self.buffers.get(&owner).ok_or(MemoryError::Closed)?;
        if !buffer.initialized.iter().all(|value| *value) {
            return Err(MemoryError::Uninitialized);
        }
        let access = self.memory.grant(Grant {
            allocation: owner,
            range: 0..buffer.len,
            permission: Permission::Read,
            owner: OwnerState::Frozen,
        })?;
        let buffer = self.buffers.remove(&owner).expect("checked owner");
        self.frozen.insert(owner, buffer);
        Ok(access)
    }

    pub fn immutable_length(&self, access: AccessHandle) -> Result<i64, MemoryError> {
        let grant = self.live_grant(access)?;
        if grant.owner != OwnerState::Frozen {
            return Err(MemoryError::Permission);
        }
        i64::try_from(grant.range.len()).map_err(|_| MemoryError::Overflow)
    }

    /// Import initialized octets from a host service into retained immutable memory.
    pub fn import_memory(&mut self, bytes: &[u8]) -> Result<AccessHandle, MemoryError> {
        let size = i64::try_from(bytes.len()).map_err(|_| MemoryError::Overflow)?;
        let owner = self.allocate(size, 1)?;
        self.write(owner, 0, bytes).expect("allocated extent matches source");
        match self.freeze_memory(owner) {
            | Ok(access) => Ok(access),
            | Err(error) => {
                self.buffers.remove(&owner);
                Err(error)
            }
        }
    }

    fn allocation(&self, identity: BufferHandle) -> Result<&Buffer, MemoryError> {
        self.buffers
            .get(&identity)
            .or_else(|| self.frozen.get(&identity))
            .ok_or(MemoryError::Closed)
    }

    pub fn grant(
        &mut self, allocation: BufferHandle, offset: i64, length: i64, permission: Permission,
    ) -> Result<AccessHandle, MemoryError> {
        let buffer = self.buffers.get(&allocation).ok_or(MemoryError::Closed)?;
        let start = usize::try_from(offset).map_err(|_| MemoryError::Bounds)?;
        let size = usize::try_from(length).map_err(|_| MemoryError::Bounds)?;
        let end = start.checked_add(size).ok_or(MemoryError::Overflow)?;
        if end > buffer.len {
            return Err(MemoryError::Bounds);
        }
        self.memory.grant(Grant {
            allocation,
            range: start..end,
            permission,
            owner: OwnerState::Mutable,
        })
    }

    pub fn revoke(&mut self, access: AccessHandle) -> Result<(), MemoryError> {
        if self.memory.grants.get(&access).is_some_and(|grant| grant.owner == OwnerState::Frozen) {
            return Err(MemoryError::Permission);
        }
        self.memory.grants.remove(&access).map(|_| ()).ok_or(MemoryError::Closed)
    }

    fn live_grant(&self, access: AccessHandle) -> Result<&Grant, MemoryError> {
        let grant = self.memory.grants.get(&access).ok_or(MemoryError::Closed)?;
        let allocations = match grant.owner {
            | OwnerState::Mutable => &self.buffers,
            | OwnerState::Frozen => &self.frozen,
        };
        if !allocations.contains_key(&grant.allocation) {
            return Err(MemoryError::Closed);
        }
        Ok(grant)
    }

    pub fn base_address(&mut self, access: AccessHandle) -> Result<AddressHandle, MemoryError> {
        let grant = self.live_grant(access)?;
        self.memory.intern(Address { allocation: grant.allocation, offset: grant.range.start })
    }

    fn granted_address(
        &self, access: AccessHandle, handle: AddressHandle,
    ) -> Result<(&Grant, Address), MemoryError> {
        let grant = self.live_grant(access)?;
        let address = *self.memory.addresses.get(&handle).ok_or(MemoryError::InvalidValue)?;
        if address.allocation != grant.allocation
            || address.offset < grant.range.start
            || address.offset > grant.range.end
        {
            return Err(MemoryError::Bounds);
        }
        Ok((grant, address))
    }

    pub fn offset_address(
        &mut self, access: AccessHandle, origin: AddressHandle, displacement: i64,
    ) -> Result<AddressHandle, MemoryError> {
        let (grant, address) = self.granted_address(access, origin)?;
        let offset = if displacement < 0 {
            let magnitude =
                usize::try_from(displacement.unsigned_abs()).map_err(|_| MemoryError::Overflow)?;
            address.offset.checked_sub(magnitude)
        } else {
            let displacement = usize::try_from(displacement).map_err(|_| MemoryError::Overflow)?;
            address.offset.checked_add(displacement)
        }
        .ok_or(MemoryError::Overflow)?;
        if offset < grant.range.start || offset > grant.range.end {
            return Err(MemoryError::Bounds);
        }
        self.memory.intern(Address { offset, ..address })
    }

    fn checked_address(
        &self, access: AccessHandle, handle: AddressHandle, size: i64, alignment: i64, write: bool,
    ) -> Result<Address, MemoryError> {
        let (grant, address) = self.granted_address(access, handle)?;
        if matches!(
            (grant.permission, write),
            (Permission::Read, true) | (Permission::Write, false)
        ) {
            return Err(MemoryError::Permission);
        }
        let size = usize::try_from(size).map_err(|_| MemoryError::Bounds)?;
        let alignment = usize::try_from(alignment).map_err(|_| MemoryError::Alignment)?;
        let end = address.offset.checked_add(size).ok_or(MemoryError::Overflow)?;
        if end > grant.range.end {
            return Err(MemoryError::Bounds);
        }
        self.allocation(address.allocation)?.check_memory(address.offset, size, alignment)?;
        Ok(address)
    }

    /// Validate the complete footprint, including padding, without reading it.
    pub fn check_access(
        &self, access: AccessHandle, address: AddressHandle, size: i64, alignment: i64,
    ) -> Result<(), MemoryError> {
        self.checked_address(access, address, size, alignment, false).map(|_| ())
    }

    pub fn check_write(
        &self, access: AccessHandle, address: AddressHandle, size: i64, alignment: i64,
    ) -> Result<(), MemoryError> {
        self.checked_address(access, address, size, alignment, true).map(|_| ())
    }

    /// Validate an initialized readable window before handing it to a host service.
    pub fn read_memory(
        &self, access: AccessHandle, handle: AddressHandle, size: i64,
    ) -> Result<&[u8], MemoryError> {
        let address = self.checked_address(access, handle, size, 1, false)?;
        let buffer = self.allocation(address.allocation)?;
        let range = address.offset..address.offset + size as usize;
        if !buffer.initialized[range.clone()].iter().all(|value| *value) {
            return Err(MemoryError::Uninitialized);
        }
        Ok(&buffer.allocation[buffer.start + range.start..buffer.start + range.end])
    }

    /// Store a scalar's bytes after validating its full extent. Cell alignment is
    /// imposed separately; portable byte codecs may use unaligned windows.
    pub fn write_memory(
        &mut self, access: AccessHandle, handle: AddressHandle, bytes: &[u8],
    ) -> Result<(), MemoryError> {
        let size = i64::try_from(bytes.len()).map_err(|_| MemoryError::Overflow)?;
        let address = self.checked_address(access, handle, size, 1, true)?;
        let buffer = self.buffers.get_mut(&address.allocation).expect("checked mutable grant");
        let range = address.offset..address.offset + bytes.len();
        buffer.invalidate_pointers(range.clone());
        buffer.initialized[range.clone()].fill(true);
        buffer.allocation[buffer.start + range.start..buffer.start + range.end]
            .copy_from_slice(bytes);
        Ok(())
    }

    fn load<const N: usize>(
        &self, access: AccessHandle, handle: AddressHandle,
    ) -> Result<[u8; N], MemoryError> {
        let address = self.checked_address(access, handle, N as i64, N as i64, false)?;
        let buffer = self.allocation(address.allocation)?;
        let range = address.offset..address.offset + N;
        if !buffer.initialized[range.clone()].iter().all(|value| *value) {
            return Err(MemoryError::Uninitialized);
        }
        Ok(buffer.allocation[buffer.start + range.start..buffer.start + range.end]
            .try_into()
            .expect("checked leaf extent"))
    }

    fn store<const N: usize>(
        &mut self, access: AccessHandle, handle: AddressHandle, bytes: [u8; N],
    ) -> Result<Address, MemoryError> {
        let address = self.checked_address(access, handle, N as i64, N as i64, true)?;
        let buffer = self.buffers.get_mut(&address.allocation).expect("checked allocation");
        let range = address.offset..address.offset + N;
        buffer.invalidate_pointers(range.clone());
        buffer.initialized[range.clone()].fill(true);
        buffer.allocation[buffer.start + range.start..buffer.start + range.end]
            .copy_from_slice(&bytes);
        Ok(address)
    }

    pub fn load_i64(
        &self, access: AccessHandle, address: AddressHandle,
    ) -> Result<i64, MemoryError> {
        self.load(access, address).map(i64::from_ne_bytes)
    }

    pub fn load_u8(&self, access: AccessHandle, address: AddressHandle) -> Result<u8, MemoryError> {
        self.load(access, address).map(|[byte]| byte)
    }

    pub fn store_i64(
        &mut self, access: AccessHandle, address: AddressHandle, value: i64,
    ) -> Result<(), MemoryError> {
        self.store(access, address, value.to_ne_bytes()).map(|_| ())
    }

    pub fn store_u8(
        &mut self, access: AccessHandle, address: AddressHandle, value: u8,
    ) -> Result<(), MemoryError> {
        self.store(access, address, [value]).map(|_| ())
    }

    pub fn load_address(
        &mut self, access: AccessHandle, handle: AddressHandle,
    ) -> Result<AddressHandle, MemoryError> {
        self.load::<8>(access, handle)?;
        let address = self.memory.addresses[&handle];
        let target = self
            .allocation(address.allocation)?
            .pointers
            .get(&address.offset)
            .copied()
            .ok_or(MemoryError::InvalidValue)?;
        self.memory.intern(target)
    }

    pub fn store_address(
        &mut self, access: AccessHandle, handle: AddressHandle, value: AddressHandle,
    ) -> Result<(), MemoryError> {
        // Validate both addresses before changing bytes or pointer-slot information.
        self.checked_address(access, handle, 8, 8, true)?;
        let target = *self.memory.addresses.get(&value).ok_or(MemoryError::InvalidValue)?;
        let buffer = self.allocation(target.allocation)?;
        let pointer = buffer.allocation.as_ptr().wrapping_add(buffer.start + target.offset);
        let address = self.store(access, handle, (pointer as usize as u64).to_ne_bytes())?;
        self.buffers
            .get_mut(&address.allocation)
            .expect("checked allocation")
            .pointers
            .insert(address.offset, target);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::BufferError;

    #[test]
    fn freezing_retains_storage_and_revokes_every_mutable_owner_alias() {
        let mut arena = BufferArena::default();
        let owner = arena.allocate_uninitialized(8, 64).unwrap();
        let access = arena.grant(owner, 0, 8, Permission::ReadWrite).unwrap();
        let original_address = arena.base_address(access).unwrap();
        assert_eq!(arena.freeze_memory(owner), Err(MemoryError::Uninitialized));
        assert_eq!(arena.immutable_length(access), Err(MemoryError::Permission));
        arena.store_i64(access, original_address, 42).unwrap();
        let frozen = arena.freeze_memory(owner).unwrap();
        let address = arena.base_address(frozen).unwrap();
        assert_eq!(address, original_address);
        assert_eq!(arena.immutable_length(frozen), Ok(8));
        assert_eq!(arena.check_access(frozen, address, 8, 64), Ok(()));
        assert_eq!(arena.load_i64(frozen, address), Ok(42));
        assert_eq!(arena.store_i64(frozen, address, 0), Err(MemoryError::Permission));
        assert_eq!(arena.revoke(frozen), Err(MemoryError::Permission));
        assert_eq!(arena.load_i64(access, original_address), Err(MemoryError::Closed));
        assert_eq!(arena.write(owner, 0, &[0]), Err(BufferError::Closed));
        assert_eq!(arena.close(owner), Err(BufferError::Closed));
        assert_eq!(arena.grant(owner, 0, 8, Permission::ReadWrite), Err(MemoryError::Closed));
        assert_eq!(arena.load_i64(frozen, address), Ok(42));
    }

    #[test]
    fn footprint_checks_allow_padding_but_leaf_reads_require_initialization() {
        let mut arena = BufferArena::default();
        let owner = arena.allocate_uninitialized(64, 64).unwrap();
        let access = arena.grant(owner, 0, 64, Permission::ReadWrite).unwrap();
        let base = arena.base_address(access).unwrap();
        assert_eq!(arena.check_access(access, base, 64, 64), Ok(()));
        assert_eq!(arena.load_i64(access, base), Err(MemoryError::Uninitialized));
        assert_eq!(arena.freeze_memory(owner), Err(MemoryError::Uninitialized));
        arena.store_i64(access, base, i64::MIN).unwrap();
        assert_eq!(arena.load_i64(access, base), Ok(i64::MIN));
        let second = arena.offset_address(access, base, 8).unwrap();
        assert_eq!(arena.check_access(access, second, 8, 64), Err(MemoryError::Alignment));
        assert_eq!(arena.check_access(access, base, 65, 64), Err(MemoryError::Bounds));
        assert_eq!(arena.check_access(access, base, 8, 3), Err(MemoryError::Alignment));
        assert_eq!(arena.load_i64(access, second), Err(MemoryError::Uninitialized));
        assert_eq!(arena.read_memory(access, base, 8).unwrap(), &i64::MIN.to_ne_bytes());
        arena.write(owner, 8, &[0; 56]).unwrap();
        let frozen = arena.freeze_memory(owner).unwrap();
        assert_eq!(arena.immutable_length(frozen), Ok(64));
        assert_eq!(arena.check_access(access, base, 0, 1), Err(MemoryError::Closed));
    }

    #[test]
    fn access_checks_permissions_ranges_identity_and_revocation() {
        let mut arena = BufferArena::default();
        let owner = arena.allocate(32, 32).unwrap();
        let full = arena.grant(owner, 0, 32, Permission::ReadWrite).unwrap();
        let narrow = arena.grant(owner, 8, 8, Permission::Read).unwrap();
        let write = arena.grant(owner, 0, 8, Permission::Write).unwrap();
        let base = arena.base_address(full).unwrap();
        let payload = arena.base_address(narrow).unwrap();
        assert_eq!(arena.offset_address(full, payload, -8), Ok(base));
        assert_eq!(arena.offset_address(narrow, payload, -8), Err(MemoryError::Bounds));
        assert_eq!(arena.load_i64(write, base), Err(MemoryError::Permission));
        assert_eq!(arena.store_i64(narrow, payload, 1), Err(MemoryError::Permission));
        arena.store_i64(write, base, 7).unwrap();
        assert_eq!(arena.load_i64(full, base), Ok(7));
        let another = arena.allocate(32, 32).unwrap();
        let different = arena.grant(another, 0, 32, Permission::ReadWrite).unwrap();
        assert_eq!(arena.load_i64(different, base), Err(MemoryError::Bounds));
        let alias = narrow;
        arena.revoke(narrow).unwrap();
        assert_eq!(arena.load_i64(alias, payload), Err(MemoryError::Closed));
        assert_eq!(arena.revoke(alias), Err(MemoryError::Closed));
        assert_eq!(arena.load_i64(full, payload), Ok(0));
        arena.close(owner).unwrap();
        assert_eq!(arena.load_i64(full, base), Err(MemoryError::Closed));
        assert_eq!(arena.base_address(write), Err(MemoryError::Closed));
        assert_ne!(arena.allocate(32, 32).unwrap(), owner);
    }

    #[test]
    fn pointer_slots_preserve_provenance_and_byte_writes_invalidate_it() {
        let mut arena = BufferArena::default();
        let owner = arena.allocate(24, 8).unwrap();
        let access = arena.grant(owner, 0, 24, Permission::ReadWrite).unwrap();
        let slot = arena.base_address(access).unwrap();
        let target = arena.offset_address(access, slot, 8).unwrap();
        arena.store_address(access, slot, target).unwrap();
        assert_eq!(arena.load_address(access, slot), Ok(target));
        let bits = arena.read_memory(access, slot, 8).unwrap().to_vec();
        assert_eq!(arena.write(owner, 23, &[1, 2]), Err(BufferError::Bounds));
        assert_eq!(arena.load_address(access, slot), Ok(target));
        // Rewriting identical bytes still cannot establish a pointer value.
        arena.write(owner, 1, &bits[1..2]).unwrap();
        assert_eq!(arena.load_address(access, slot), Err(MemoryError::InvalidValue));
        arena.store_address(access, slot, target).unwrap();
        arena.store_i64(access, slot, 0).unwrap();
        assert_eq!(arena.load_address(access, slot), Err(MemoryError::InvalidValue));
        arena.store_address(access, slot, target).unwrap();
        arena.write(owner, 8, &[]).unwrap();
        assert_eq!(arena.load_address(access, slot), Ok(target));
    }

    #[test]
    fn invalid_stores_do_not_change_bytes_initialization_or_pointer_slots() {
        let mut arena = BufferArena::default();
        let owner = arena.allocate_uninitialized(16, 16).unwrap();
        let access = arena.grant(owner, 0, 16, Permission::ReadWrite).unwrap();
        let base = arena.base_address(access).unwrap();
        let misaligned = arena.offset_address(access, base, 1).unwrap();
        assert_eq!(arena.store_i64(access, misaligned, 1), Err(MemoryError::Alignment));
        assert_eq!(arena.load_u8(access, misaligned), Err(MemoryError::Uninitialized));
        let target_owner = arena.allocate(1, 1).unwrap();
        let target_access = arena.grant(target_owner, 0, 1, Permission::Read).unwrap();
        let target = arena.base_address(target_access).unwrap();
        arena.store_address(access, base, target).unwrap();
        arena.close(target_owner).unwrap();
        assert_eq!(arena.store_address(access, base, target), Err(MemoryError::Closed));
        assert_eq!(arena.load_address(access, base), Ok(target));
        assert_eq!(arena.load_u8(target_access, target), Err(MemoryError::Closed));
    }

    #[test]
    fn empty_allocations_and_one_past_addresses_never_allow_nonempty_loads() {
        let mut arena = BufferArena::default();
        let owner = arena.allocate_uninitialized(0, 4096).unwrap();
        let access = arena.grant(owner, 0, 0, Permission::Read).unwrap();
        let base = arena.base_address(access).unwrap();
        assert_eq!(arena.check_access(access, base, 0, 4096), Ok(()));
        assert_eq!(arena.offset_address(access, base, 0), Ok(base));
        assert_eq!(arena.offset_address(access, base, i64::MIN), Err(MemoryError::Overflow));
        assert_eq!(arena.offset_address(access, base, i64::MAX), Err(MemoryError::Bounds));
        assert_eq!(arena.load_u8(access, base), Err(MemoryError::Bounds));
        assert_eq!(arena.grant(owner, 0, 1, Permission::Read), Err(MemoryError::Bounds));
        let frozen = arena.freeze_memory(owner).unwrap();
        assert_eq!(arena.read_memory(frozen, base, 0), Ok(&[][..]));
        assert_eq!(arena.check_access(frozen, base, 0, 4096), Ok(()));
        assert_eq!(Permission::try_from(0), Err(MemoryError::InvalidValue));
        assert_eq!(Permission::try_from(4), Err(MemoryError::InvalidValue));
    }
}
