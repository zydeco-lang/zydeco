//! Portable 64-bit native runtime ABI contract shared by `zydeco-cranelift` and `runtime`.

use crate::native::{AllocationKind, WORD_BYTES, Word};

/// A host control-call continuation transfer written by control builtins and consumed
/// immediately by generated Cranelift code.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct ClifHostTransfer<W> {
    /// Number of arguments delivered to the target closure (`0`, `1`, or `2`).
    pub arity: W,
    /// Pointer to the target `Closure` record `[environment, code]`.
    pub closure: W,
    /// First continuation argument when `arity >= 1`.
    pub first: W,
    /// Second continuation argument when `arity == 2`.
    pub second: W,
}

/// Fixed-offset VM context header shared between `zydeco-cranelift` generated code and
/// `runtime/clif.rs`.
///
/// The Cranelift runtime instance places this header at offset 0 so all hot bump-allocation,
/// SPS-stack, and host-transfer fields live at constant byte displacements from `vmctx`.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct ClifVmCtxHeader {
    pub alloc_ptr: *mut u8,
    pub alloc_limit: *mut u8,
    pub sps_sp: *mut Word,
    pub sps_base: *mut Word,
    pub sps_limit: *mut Word,
    pub transfer: ClifHostTransfer<Word>,
}

pub const VMCTX_ALLOC_PTR: i32 = 0;
pub const VMCTX_ALLOC_LIMIT: i32 = 8;
pub const VMCTX_SPS_SP: i32 = 16;
pub const VMCTX_SPS_BASE: i32 = 24;
pub const VMCTX_SPS_LIMIT: i32 = 32;
pub const VMCTX_TRANSFER_ARITY: i32 = 40;
pub const VMCTX_TRANSFER_CLOSURE: i32 = 48;
pub const VMCTX_TRANSFER_FIRST: i32 = 56;
pub const VMCTX_TRANSFER_SECOND: i32 = 64;

pub const BLOCK_HEADER_BYTES: usize = 16;
pub const BLOCK_HEADER_SIZE_OFFSET: i32 = 0;
pub const BLOCK_HEADER_META_OFFSET: i32 = 8;
pub const ALLOC_KIND_SCANNED: usize = AllocationKind::Scanned as usize;
pub const ALLOC_KIND_OPAQUE: usize = AllocationKind::Opaque as usize;

pub const CLIF_HOST_PREFIX: &str = "zydeco_clif_";
pub const CLIF_ALLOC_SLOW_SYMBOL: &str = "zydeco_clif_alloc_slow";
pub const CLIF_GROW_SPS_SYMBOL: &str = "zydeco_clif_grow_sps";
pub const CLIF_FAIL_SYMBOL: &str = "zydeco_clif_fail";
pub const CLIF_ENTRY_BEGIN_SYMBOL: &str = "zydeco_clif_entry_begin";
pub const CLIF_ENTRY_END_SYMBOL: &str = "zydeco_clif_entry_end";

#[cfg(target_pointer_width = "64")]
const _: () = {
    use core::mem::{offset_of, size_of};
    assert!(size_of::<Word>() == WORD_BYTES);
    assert!(offset_of!(ClifVmCtxHeader, alloc_ptr) as i32 == VMCTX_ALLOC_PTR);
    assert!(offset_of!(ClifVmCtxHeader, alloc_limit) as i32 == VMCTX_ALLOC_LIMIT);
    assert!(offset_of!(ClifVmCtxHeader, sps_sp) as i32 == VMCTX_SPS_SP);
    assert!(offset_of!(ClifVmCtxHeader, sps_base) as i32 == VMCTX_SPS_BASE);
    assert!(offset_of!(ClifVmCtxHeader, sps_limit) as i32 == VMCTX_SPS_LIMIT);
    assert!(
        offset_of!(ClifVmCtxHeader, transfer) + offset_of!(ClifHostTransfer<Word>, arity)
            == VMCTX_TRANSFER_ARITY as usize
    );
    assert!(
        offset_of!(ClifVmCtxHeader, transfer) + offset_of!(ClifHostTransfer<Word>, closure)
            == VMCTX_TRANSFER_CLOSURE as usize
    );
    assert!(
        offset_of!(ClifVmCtxHeader, transfer) + offset_of!(ClifHostTransfer<Word>, first)
            == VMCTX_TRANSFER_FIRST as usize
    );
    assert!(
        offset_of!(ClifVmCtxHeader, transfer) + offset_of!(ClifHostTransfer<Word>, second)
            == VMCTX_TRANSFER_SECOND as usize
    );
};

include!(concat!(env!("OUT_DIR"), "/clif_contract.rs"));
