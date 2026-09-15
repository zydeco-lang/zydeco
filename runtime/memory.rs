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
