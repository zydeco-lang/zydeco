//! Native binding for the shared checked address and grant model.

use super::*;
use zydeco_machine::memory::{AccessHandle, AddressHandle, MemoryError, Permission};

struct MemoryBranch;

impl MemoryBranch {
    fn finish(result: Result<Option<Word>, MemoryError>, error: Word, success: Word) -> Word {
        match result {
            | Ok(None) => HostControl::without_arguments(success),
            | Ok(Some(value)) => HostControl::with_one_argument(success, value),
            | Err(code) => {
                HostControl::with_one_argument(error, Immediate::expect_signed(code as i64))
            }
        }
    }

    fn address(value: Word) -> AddressHandle {
        AddressHandle::with_raw(HostHandle::decode(value))
    }

    fn access(value: Word) -> AccessHandle {
        AccessHandle::with_raw(HostHandle::decode(value))
    }
}

#[unsafe(export_name = "\x01zydeco_memory_allocate")]
extern "sysv64" fn allocate(size: Word, alignment: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .allocate_uninitialized(
                <i64 as RuntimeInteger>::decode(size),
                <i64 as RuntimeInteger>::decode(alignment),
            )
            .map(|handle| Some(HostHandle::encode(handle.raw())))
    });
    BufferBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_grant")]
extern "sysv64" fn grant(
    buffer: Word, offset: Word, length: Word, permission: Word, error: Word, success: Word,
) -> Word {
    let result =
        Permission::try_from(<i64 as RuntimeInteger>::decode(permission)).and_then(|permission| {
            HOST_BUFFERS.with(|arena| {
                arena
                    .borrow_mut()
                    .grant(
                        BufferBranch::handle(buffer),
                        <i64 as RuntimeInteger>::decode(offset),
                        <i64 as RuntimeInteger>::decode(length),
                        permission,
                    )
                    .map(|handle| Some(HostHandle::encode(handle.raw())))
            })
        });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_revoke")]
extern "sysv64" fn revoke(access: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS
        .with(|arena| arena.borrow_mut().revoke(MemoryBranch::access(access)).map(|()| None));
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_base")]
extern "sysv64" fn base(access: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .base_address(MemoryBranch::access(access))
            .map(|handle| Some(HostHandle::encode(handle.raw())))
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_offset")]
extern "sysv64" fn offset(
    access: Word, address: Word, offset: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .offset_address(
                MemoryBranch::access(access),
                MemoryBranch::address(address),
                <i64 as RuntimeInteger>::decode(offset),
            )
            .map(|handle| Some(HostHandle::encode(handle.raw())))
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_check")]
extern "sysv64" fn check(
    access: Word, address: Word, size: Word, alignment: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow()
            .check_access(
                MemoryBranch::access(access),
                MemoryBranch::address(address),
                <i64 as RuntimeInteger>::decode(size),
                <i64 as RuntimeInteger>::decode(alignment),
            )
            .map(|()| None)
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_load_i64")]
extern "sysv64" fn load_i64(
    access: Word, address: Word, error: Word, success: Word, spare: *mut Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow()
            .load_i64(MemoryBranch::access(access), MemoryBranch::address(address))
            .map(|value| Some(value.encode(spare)))
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_load_u8")]
extern "sysv64" fn load_u8(access: Word, address: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow()
            .load_u8(MemoryBranch::access(access), MemoryBranch::address(address))
            .map(|value| Some(Immediate::expect_unsigned(value as Word)))
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_load_addr")]
extern "sysv64" fn load_addr(access: Word, address: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .load_address(MemoryBranch::access(access), MemoryBranch::address(address))
            .map(|handle| Some(HostHandle::encode(handle.raw())))
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_store_i64")]
extern "sysv64" fn store_i64(
    access: Word, address: Word, value: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .store_i64(
                MemoryBranch::access(access),
                MemoryBranch::address(address),
                <i64 as RuntimeInteger>::decode(value),
            )
            .map(|()| None)
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_store_u8")]
extern "sysv64" fn store_u8(
    access: Word, address: Word, value: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .store_u8(
                MemoryBranch::access(access),
                MemoryBranch::address(address),
                <u8 as RuntimeInteger>::decode(value),
            )
            .map(|()| None)
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_store_addr")]
extern "sysv64" fn store_addr(
    access: Word, address: Word, value: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .store_address(
                MemoryBranch::access(access),
                MemoryBranch::address(address),
                MemoryBranch::address(value),
            )
            .map(|()| None)
    });
    MemoryBranch::finish(result, error, success)
}
