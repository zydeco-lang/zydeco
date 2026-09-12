//! Native binding for the shared checked address and grant model.

use super::*;
use zydeco_machine::memory::{AccessHandle, AddressHandle, MemoryError, Permission};

pub(super) struct MemoryBranch;

impl MemoryBranch {
    fn buffer(value: Word) -> BufferHandle {
        BufferHandle::with_raw(HostHandle::decode(value))
    }
    pub(super) fn finish(
        result: Result<Option<Word>, MemoryError>, error: Word, success: Word,
    ) -> Word {
        match result {
            | Ok(None) => HostControl::without_arguments(success),
            | Ok(Some(value)) => HostControl::with_one_argument(success, value),
            | Err(code) => {
                HostControl::with_one_argument(error, Immediate::expect_signed(code as i64))
            }
        }
    }

    pub(super) fn address(value: Word) -> AddressHandle {
        AddressHandle::with_raw(HostHandle::decode(value))
    }

    pub(super) fn access(value: Word) -> AccessHandle {
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
            .map_err(MemoryError::from)
            .map(|handle| Some(HostHandle::encode(handle.raw())))
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_close")]
extern "sysv64" fn close(owner: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .close(MemoryBranch::buffer(owner))
            .map_err(MemoryError::from)
            .map(|()| None)
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_freeze")]
extern "sysv64" fn freeze(owner: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .freeze_memory(MemoryBranch::buffer(owner))
            .map(|access| Some(HostHandle::encode(access.raw())))
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_immutable_length")]
extern "sysv64" fn immutable_length(
    access: Word, error: Word, success: Word, spare: *mut Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow()
            .immutable_length(MemoryBranch::access(access))
            .map(|length| Some(length.encode(spare)))
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_check_write")]
extern "sysv64" fn check_write(
    access: Word, address: Word, size: Word, alignment: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow()
            .check_write(
                MemoryBranch::access(access),
                MemoryBranch::address(address),
                <i64 as RuntimeInteger>::decode(size),
                <i64 as RuntimeInteger>::decode(alignment),
            )
            .map(|()| None)
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_from_string")]
extern "sysv64" fn from_string(string: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .import_memory(unsafe { HostString::borrow(string) }.as_bytes())
            .map(|access| Some(HostHandle::encode(access.raw())))
    });
    MemoryBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_memory_to_string")]
extern "sysv64" fn to_string(
    access: Word, address: Word, size: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        let arena = arena.borrow();
        let bytes = arena.read_memory(
            MemoryBranch::access(access),
            MemoryBranch::address(address),
            <i64 as RuntimeInteger>::decode(size),
        )?;
        let string = std::str::from_utf8(bytes).map_err(|_| MemoryError::InvalidValue)?;
        Ok(Some(HostString::leak(string.to_owned())))
    });
    MemoryBranch::finish(result, error, success)
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
                        MemoryBranch::buffer(buffer),
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

// Scalar representation leaves preserve every payload bit, including floating
// NaN payloads. Product composition, padding, and field alignment are library code.
macro_rules! scalar_memory {
    ($type:ty, $store:ident => $store_symbol:literal, $load:ident => $load_symbol:literal,
     $decode:expr, $encode:expr) => {
        #[unsafe(export_name = $store_symbol)]
        extern "sysv64" fn $store(
            access: Word, address: Word, value: Word, error: Word, success: Word,
        ) -> Word {
            let value: $type = ($decode)(value);
            let result = HOST_BUFFERS.with(|arena| {
                arena
                    .borrow_mut()
                    .write_memory(
                        MemoryBranch::access(access),
                        MemoryBranch::address(address),
                        &value.to_le_bytes(),
                    )
                    .map(|()| None)
            });
            MemoryBranch::finish(result, error, success)
        }
        #[unsafe(export_name = $load_symbol)]
        extern "sysv64" fn $load(
            access: Word, address: Word, error: Word, success: Word, spare: *mut Word,
        ) -> Word {
            let result = HOST_BUFFERS.with(|arena| {
                let arena = arena.borrow();
                let bytes = arena.read_memory(
                    MemoryBranch::access(access),
                    MemoryBranch::address(address),
                    core::mem::size_of::<$type>() as i64,
                )?;
                let value = <$type>::from_le_bytes(bytes.try_into().expect("checked scalar width"));
                Ok(Some(($encode)(value, spare)))
            });
            MemoryBranch::finish(result, error, success)
        }
    };
}
scalar_memory!(
    i8,
    zydeco_int8_store_le_branch => "\x01zydeco_int8_store_le_branch",
    zydeco_int8_load_le_branch => "\x01zydeco_int8_load_le_branch",
    <i8 as RuntimeInteger>::decode, <i8 as RuntimeInteger>::encode
);
scalar_memory!(
    i16,
    zydeco_int16_store_le_branch => "\x01zydeco_int16_store_le_branch",
    zydeco_int16_load_le_branch => "\x01zydeco_int16_load_le_branch",
    <i16 as RuntimeInteger>::decode, <i16 as RuntimeInteger>::encode
);
scalar_memory!(
    i32,
    zydeco_int32_store_le_branch => "\x01zydeco_int32_store_le_branch",
    zydeco_int32_load_le_branch => "\x01zydeco_int32_load_le_branch",
    <i32 as RuntimeInteger>::decode, <i32 as RuntimeInteger>::encode
);
scalar_memory!(
    i64,
    zydeco_int64_store_le_branch => "\x01zydeco_int64_store_le_branch",
    zydeco_int64_load_le_branch => "\x01zydeco_int64_load_le_branch",
    <i64 as RuntimeInteger>::decode, <i64 as RuntimeInteger>::encode
);
scalar_memory!(
    u8,
    zydeco_uint8_store_le_branch => "\x01zydeco_uint8_store_le_branch",
    zydeco_uint8_load_le_branch => "\x01zydeco_uint8_load_le_branch",
    <u8 as RuntimeInteger>::decode, <u8 as RuntimeInteger>::encode
);
scalar_memory!(
    u16,
    zydeco_uint16_store_le_branch => "\x01zydeco_uint16_store_le_branch",
    zydeco_uint16_load_le_branch => "\x01zydeco_uint16_load_le_branch",
    <u16 as RuntimeInteger>::decode, <u16 as RuntimeInteger>::encode
);
scalar_memory!(
    u32,
    zydeco_uint32_store_le_branch => "\x01zydeco_uint32_store_le_branch",
    zydeco_uint32_load_le_branch => "\x01zydeco_uint32_load_le_branch",
    <u32 as RuntimeInteger>::decode, <u32 as RuntimeInteger>::encode
);
scalar_memory!(
    u64,
    zydeco_uint64_store_le_branch => "\x01zydeco_uint64_store_le_branch",
    zydeco_uint64_load_le_branch => "\x01zydeco_uint64_load_le_branch",
    <u64 as RuntimeInteger>::decode, <u64 as RuntimeInteger>::encode
);
scalar_memory!(
    u32,
    zydeco_float32_store_le_branch => "\x01zydeco_float32_store_le_branch",
    zydeco_float32_load_le_branch => "\x01zydeco_float32_load_le_branch",
    |word| Immediate::decode_unsigned(word) as u32,
    |bits: u32, _spare| Immediate::expect_unsigned(bits as Word)
);
scalar_memory!(
    u64,
    zydeco_float64_store_le_branch => "\x01zydeco_float64_store_le_branch",
    zydeco_float64_load_le_branch => "\x01zydeco_float64_load_le_branch",
    |word| OpaqueScalar::load(word) as u64,
    |bits: u64, spare| OpaqueScalar::store(spare, bits as Word)
);
