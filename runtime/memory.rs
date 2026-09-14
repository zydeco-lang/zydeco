//! Native raw memory. The source unsafe contract supplies validity and lifetime.
use super::*;
use zydeco_machine::memory::{Address, MemoryError, MemoryLayout};

pub(super) struct MemoryBranch;
impl MemoryBranch {
    pub(super) fn address(word: Word) -> Address {
        Address::from_exposed(word)
    }
    fn finish(result: Result<Option<Word>, MemoryError>, error: Word, success: Word) -> Word {
        match result {
            | Ok(Some(value)) => HostControl::with_one_argument(success, value),
            | Ok(None) => HostControl::without_arguments(success),
            | Err(code) => {
                HostControl::with_one_argument(error, Immediate::expect_signed(code as i64))
            }
        }
    }
}
#[unsafe(export_name = "\x01zydeco_memory_null")]
extern "sysv64" fn null() -> Word {
    0
}
#[unsafe(export_name = "\x01zydeco_memory_offset")]
extern "sysv64" fn offset(address: Word, displacement: Word) -> Word {
    MemoryBranch::address(address).offset(<i64 as RuntimeInteger>::decode(displacement)).expose()
}
#[unsafe(export_name = "\x01zydeco_memory_allocate")]
extern "sysv64" fn allocate(size: Word, alignment: Word, error: Word, success: Word) -> Word {
    let result = MemoryLayout::for_request(
        <i64 as RuntimeInteger>::decode(size),
        <i64 as RuntimeInteger>::decode(alignment),
    )
    .and_then(MemoryLayout::allocate)
    .map(|address| Some(address.expose()));
    MemoryBranch::finish(result, error, success)
}
#[unsafe(export_name = "\x01zydeco_memory_free")]
extern "sysv64" fn free(
    address: Word, size: Word, alignment: Word, error: Word, success: Word,
) -> Word {
    let result = MemoryLayout::for_request(
        <i64 as RuntimeInteger>::decode(size),
        <i64 as RuntimeInteger>::decode(alignment),
    )
    .map(|layout| {
        unsafe { layout.deallocate(MemoryBranch::address(address)) };
        None
    });
    MemoryBranch::finish(result, error, success)
}
#[unsafe(export_name = "\x01zydeco_memory_retain")]
extern "sysv64" fn retain(
    address: Word, size: Word, alignment: Word, error: Word, success: Word,
) -> Word {
    let result = MemoryLayout::for_request(
        <i64 as RuntimeInteger>::decode(size),
        <i64 as RuntimeInteger>::decode(alignment),
    )
    .and_then(|layout| {
        RuntimeInstance::with_memory(|memory| unsafe {
            memory.borrow_mut().retain(MemoryBranch::address(address), layout)
        })
    })
    .map(|()| None);
    MemoryBranch::finish(result, error, success)
}
#[unsafe(export_name = "\x01zydeco_memory_copy")]
extern "sysv64" fn copy(source: Word, destination: Word, count: Word, success: Word) -> Word {
    let count = <i64 as RuntimeInteger>::decode(count) as usize;
    if count != 0 {
        unsafe {
            std::ptr::copy(
                MemoryBranch::address(source).pointer(),
                MemoryBranch::address(destination).pointer(),
                count,
            )
        };
    }
    HostControl::without_arguments(success)
}
#[unsafe(export_name = "\x01zydeco_memory_fill")]
extern "sysv64" fn fill(address: Word, count: Word, value: Word, success: Word) -> Word {
    let count = <i64 as RuntimeInteger>::decode(count) as usize;
    if count != 0 {
        unsafe {
            MemoryBranch::address(address)
                .pointer()
                .write_bytes(<u8 as RuntimeInteger>::decode(value), count)
        };
    }
    HostControl::without_arguments(success)
}
#[unsafe(export_name = "\x01zydeco_memory_load_addr")]
extern "sysv64" fn load_addr(address: Word, success: Word) -> Word {
    let value = unsafe { MemoryBranch::address(address).load_address() };
    HostControl::with_one_argument(success, value.expose())
}
#[unsafe(export_name = "\x01zydeco_memory_store_addr")]
extern "sysv64" fn store_addr(address: Word, value: Word, success: Word) -> Word {
    unsafe { MemoryBranch::address(address).store_address(MemoryBranch::address(value)) };
    HostControl::without_arguments(success)
}
#[unsafe(export_name = "\x01zydeco_memory_from_string")]
extern "sysv64" fn from_string(string: Word, error: Word, success: Word) -> Word {
    let bytes = unsafe { HostString::borrow(string) }.as_bytes();
    match RuntimeInstance::with_memory(|memory| memory.borrow_mut().import(bytes)) {
        | Ok(address) => HostControl::with_two_arguments(
            success,
            address.expose(),
            Immediate::expect_signed(bytes.len() as i64),
        ),
        | Err(fault) => MemoryBranch::finish(Err(fault), error, success),
    }
}
#[unsafe(export_name = "\x01zydeco_memory_to_string")]
extern "sysv64" fn to_string(address: Word, count: Word, error: Word, success: Word) -> Word {
    let bytes = unsafe {
        MemoryBranch::address(address).bytes(<i64 as RuntimeInteger>::decode(count) as usize)
    };
    let result = std::str::from_utf8(bytes)
        .map(|s| Some(HostString::own(s.to_owned())))
        .map_err(|_| MemoryError::InvalidEncoding);
    MemoryBranch::finish(result, error, success)
}
macro_rules! scalar_memory {
    ($type:ty, $store:ident => $store_symbol:literal, $load:ident => $load_symbol:literal,
     $decode:expr, $encode:expr) => {
        #[unsafe(export_name = $store_symbol)]
        extern "sysv64" fn $store(address: Word, value: Word, success: Word) -> Word {
            let value: $type = ($decode)(value);
            unsafe { MemoryBranch::address(address).write(&value.to_le_bytes()) };
            HostControl::without_arguments(success)
        }
        #[unsafe(export_name = $load_symbol)]
        extern "sysv64" fn $load(address: Word, success: Word, spare: *mut Word) -> Word {
            let bytes =
                unsafe { MemoryBranch::address(address).bytes(core::mem::size_of::<$type>()) };
            let value = <$type>::from_le_bytes(bytes.try_into().expect("scalar width"));
            HostControl::with_one_argument(success, ($encode)(value, spare))
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
