//! Portable raw memory helpers for native Cranelift targets.

use super::clif::ClifRuntimeInstance;
use zydeco_host::string::borrow_heap_str;
use zydeco_machine::{
    clif::ClifVmCtxHeader,
    memory::{Address, MemoryError, MemoryLayout},
    native::{Immediate, Word},
};

fn finish_memory_branch(
    instance: &mut ClifRuntimeInstance, result: Result<Option<Word>, MemoryError>, error: Word,
    success: Word,
) {
    match result {
        | Ok(Some(value)) => instance.set_transfer_one(success, value),
        | Ok(None) => instance.set_transfer_none(success),
        | Err(code) => instance.set_transfer_one(error, Immediate::expect_signed(code as i64)),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_memory_null(_vmctx: *mut ClifVmCtxHeader) -> Word {
    0
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_memory_allocate(
    vmctx: *mut ClifVmCtxHeader, size: Word, alignment: Word, error: Word, success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let result = MemoryLayout::for_request(
        Immediate::decode_signed(size),
        Immediate::decode_signed(alignment),
    )
    .and_then(MemoryLayout::allocate)
    .map(|address| Some(address.expose()));
    finish_memory_branch(instance, result, error, success);
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_memory_free(
    vmctx: *mut ClifVmCtxHeader, address: Word, size: Word, alignment: Word, error: Word,
    success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let result = MemoryLayout::for_request(
        Immediate::decode_signed(size),
        Immediate::decode_signed(alignment),
    )
    .map(|layout| {
        unsafe { layout.deallocate(Address::from_exposed(address)) };
        None
    });
    finish_memory_branch(instance, result, error, success);
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_memory_retain(
    vmctx: *mut ClifVmCtxHeader, address: Word, size: Word, alignment: Word, error: Word,
    success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let result = MemoryLayout::for_request(
        Immediate::decode_signed(size),
        Immediate::decode_signed(alignment),
    )
    .and_then(|layout| unsafe {
        instance.memory.borrow_mut().retain(Address::from_exposed(address), layout)
    })
    .map(|()| None);
    finish_memory_branch(instance, result, error, success);
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_memory_copy(
    vmctx: *mut ClifVmCtxHeader, source: Word, destination: Word, count: Word, success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let count = Immediate::decode_signed(count) as usize;
    if count != 0 {
        unsafe {
            std::ptr::copy(
                Address::from_exposed(source).pointer(),
                Address::from_exposed(destination).pointer(),
                count,
            );
        }
    }
    instance.set_transfer_none(success);
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_memory_fill(
    vmctx: *mut ClifVmCtxHeader, address: Word, count: Word, value: Word, success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let count = Immediate::decode_signed(count) as usize;
    if count != 0 {
        unsafe {
            Address::from_exposed(address)
                .pointer()
                .write_bytes(Immediate::decode_unsigned(value) as u8, count);
        }
    }
    instance.set_transfer_none(success);
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_memory_from_string(
    vmctx: *mut ClifVmCtxHeader, string: Word, error: Word, success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let bytes = unsafe { borrow_heap_str(string) }.as_bytes();
    let imported = instance.memory.borrow_mut().import(bytes);
    match imported {
        | Ok(address) => instance.set_transfer_two(
            success,
            address.expose(),
            Immediate::expect_signed(bytes.len() as i64),
        ),
        | Err(code) => finish_memory_branch(instance, Err(code), error, success),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_memory_to_string(
    vmctx: *mut ClifVmCtxHeader, address: Word, count: Word, error: Word, mut success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let bytes =
        unsafe { Address::from_exposed(address).bytes(Immediate::decode_signed(count) as usize) };
    match std::str::from_utf8(bytes) {
        | Ok(text) => {
            let owned =
                unsafe { instance.allocate_string_bytes(text.as_bytes(), &mut [&raw mut success]) };
            instance.set_transfer_one(success, owned);
        }
        | Err(_) => {
            finish_memory_branch(instance, Err(MemoryError::InvalidEncoding), error, success)
        }
    }
}
