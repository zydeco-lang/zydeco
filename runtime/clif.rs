//! Portable native Cranelift runtime support (host builtins, GC slow path, growable SPS stack).

use std::{
    cell::{Cell, RefCell},
    io::{self, Write},
    ptr,
    sync::atomic::{AtomicBool, Ordering},
};
use zydeco_abi::{HostIoErrorKind, RuntimeFailure};
use zydeco_host::{
    gc::{CheneyHeap, OutOfMemory, RootRange, Roots},
    io::{HostIoError, HostIoRuntime, LineInput, ReaderHandle, WriterHandle},
    random::random_signed_int,
    string::{
        StaticHeapString, borrow_heap_str, char_from_codepoint, heap_string_words,
        parse_signed_int, split_at_scalar_byte, split_once_byte_ranges, write_heap_string,
        write_heap_string_concat,
    },
};
use zydeco_machine::{
    clif::{ClifHostTransfer, ClifVmCtxHeader},
    memory::RetainedMemory,
    native::{AllocationKind, IMMEDIATE_TAG, Immediate, Word},
};

const INITIAL_SPS_WORDS: usize = 1024;
const SHRINK_SPS_WORDS: usize = 8192;

/// Isolated execution state for one native Cranelift entry (`Process` or `C` export).
///
/// Placing `header` at offset 0 lets generated code load and store `alloc_ptr`,
/// `alloc_limit`, `sps_sp`, `sps_base`, `sps_limit`, and `transfer` at fixed displacements.
#[repr(C)]
pub struct ClifRuntimeInstance {
    pub header: ClifVmCtxHeader,
    heap: CheneyHeap,
    sps_storage: Box<[Word]>,
    pub(super) memory: RefCell<RetainedMemory>,
    io: RefCell<HostIoRuntime>,
    arguments: Vec<StaticHeapString>,
    guard: *const AtomicBool,
}

const _: () = {
    assert!(core::mem::offset_of!(ClifRuntimeInstance, header) == 0);
};

struct ParkedClifInstance(Cell<*mut ClifRuntimeInstance>);

impl Drop for ParkedClifInstance {
    fn drop(&mut self) {
        let instance = self.0.replace(ptr::null_mut());
        if !instance.is_null() {
            unsafe { drop(Box::from_raw(instance)) };
        }
    }
}

thread_local! {
    static REUSABLE_CLIF_INSTANCE: ParkedClifInstance =
        const { ParkedClifInstance(Cell::new(ptr::null_mut())) };
}

impl ClifRuntimeInstance {
    fn new(guard: *const AtomicBool) -> Box<Self> {
        let mut heap = CheneyHeap::new();
        let (alloc_ptr, alloc_limit) = heap.bump_bounds();
        let mut sps_storage = vec![0_usize; INITIAL_SPS_WORDS].into_boxed_slice();
        let sps_base = sps_storage.as_mut_ptr();
        let sps_limit = unsafe { sps_base.add(sps_storage.len()) };
        Box::new(Self {
            header: ClifVmCtxHeader {
                alloc_ptr,
                alloc_limit,
                sps_sp: sps_base,
                sps_base,
                sps_limit,
                transfer: ClifHostTransfer::default(),
            },
            heap,
            sps_storage,
            memory: RefCell::new(RetainedMemory::default()),
            io: RefCell::new(HostIoRuntime::new()),
            arguments: Vec::new(),
            guard,
        })
    }

    pub(super) unsafe fn from_vmctx<'a>(vmctx: *mut ClifVmCtxHeader) -> &'a mut Self {
        debug_assert!(!vmctx.is_null(), "native Cranelift runtime helper received null vmctx");
        unsafe { &mut *(vmctx.cast::<Self>()) }
    }

    pub(super) fn refresh_bump_bounds(&mut self) {
        let (ptr, limit) = self.heap.bump_bounds();
        self.header.alloc_ptr = ptr;
        self.header.alloc_limit = limit;
    }

    fn refresh_sps_bounds(&mut self, used_words: usize) {
        let base = self.sps_storage.as_mut_ptr();
        let cap = self.sps_storage.len();
        self.header.sps_base = base;
        self.header.sps_sp = unsafe { base.add(used_words) };
        self.header.sps_limit = unsafe { base.add(cap) };
    }

    fn reset_for_park(&mut self) {
        self.heap.reset();
        self.memory.borrow_mut().clear();
        self.io.borrow_mut().reset();
        self.arguments.clear();
        self.guard = ptr::null();
        if self.sps_storage.len() > SHRINK_SPS_WORDS {
            self.sps_storage = vec![0_usize; INITIAL_SPS_WORDS].into_boxed_slice();
        }
        self.header.transfer = ClifHostTransfer::default();
        self.refresh_bump_bounds();
        self.refresh_sps_bounds(0);
    }

    pub(super) unsafe fn allocate_opaque(
        &mut self, size_words: usize, extra: &mut [*mut Word],
    ) -> *mut u8 {
        unsafe { self.heap.sync_bump_ptr(self.header.alloc_ptr) };
        let stack = RootRange { start: self.header.sps_base, end: self.header.sps_sp };
        let roots = Roots { stack, frame: RootRange::EMPTY, slots: extra };
        let payload = unsafe { self.heap.allocate(size_words, AllocationKind::Opaque, roots) }
            .unwrap_or_else(|err| out_of_memory(err));
        self.refresh_bump_bounds();
        payload
    }

    pub(super) unsafe fn allocate_string_bytes(
        &mut self, bytes: &[u8], extra: &mut [*mut Word],
    ) -> Word {
        let payload = unsafe { self.allocate_opaque(heap_string_words(bytes.len()), extra) };
        unsafe { write_heap_string(payload, bytes) };
        payload as Word
    }

    pub(super) fn set_transfer_none(&mut self, closure: Word) {
        self.header.transfer = ClifHostTransfer { arity: 0, closure, first: 0, second: 0 };
    }

    pub(super) fn set_transfer_one(&mut self, closure: Word, first: Word) {
        self.header.transfer = ClifHostTransfer { arity: 1, closure, first, second: 0 };
    }

    pub(super) fn set_transfer_two(&mut self, closure: Word, first: Word, second: Word) {
        self.header.transfer = ClifHostTransfer { arity: 2, closure, first, second };
    }
}

/* ------------------------------ Entry / Exit ------------------------------ */

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_entry_begin(guard: *const AtomicBool) -> *mut ClifVmCtxHeader {
    if !guard.is_null()
        && unsafe { &*guard }
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
    {
        let _ = writeln!(
            io::stderr().lock(),
            "Zydeco runtime: concurrent or reentrant entry into an active compiled library"
        );
        std::process::exit(1);
    }
    let reused = REUSABLE_CLIF_INSTANCE.with(|slot| slot.0.replace(ptr::null_mut()));
    let instance = if reused.is_null() {
        Box::into_raw(ClifRuntimeInstance::new(guard))
    } else {
        unsafe {
            (*reused).guard = guard;
        }
        reused
    };
    unsafe { &raw mut (*instance).header }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_entry_end(vmctx: *mut ClifVmCtxHeader) {
    let instance_ptr = vmctx.cast::<ClifRuntimeInstance>();
    let guard = unsafe { (*instance_ptr).guard };
    unsafe { (*instance_ptr).reset_for_park() };
    let parked = REUSABLE_CLIF_INSTANCE.with(|slot| {
        if slot.0.get().is_null() {
            slot.0.set(instance_ptr);
            true
        } else {
            false
        }
    });
    if !parked {
        unsafe { drop(Box::from_raw(instance_ptr)) };
    }
    if !guard.is_null() {
        unsafe { &*guard }.store(false, Ordering::Release);
    }
}

#[cfg(feature = "process-entry")]
#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_run_process() {
    run_process();
}

#[cfg(feature = "process-entry")]
pub fn run_process() {
    run_process_with(zydeco_machine::clif::clif_entry);
}

pub fn run_process_with(entry: unsafe extern "C" fn(*mut ClifVmCtxHeader)) {
    let vmctx = unsafe { zydeco_clif_entry_begin(ptr::null()) };
    let arguments = std::env::args().skip(1).map(|arg| StaticHeapString::new(&arg)).collect();
    unsafe {
        ClifRuntimeInstance::from_vmctx(vmctx).arguments = arguments;
        entry(vmctx);
        zydeco_clif_entry_end(vmctx);
    }
}

/* ------------------------------- GC & Stack ------------------------------- */

fn out_of_memory(error: OutOfMemory) -> ! {
    let _ = writeln!(
        io::stderr().lock(),
        "Zydeco runtime: out of memory (requested {} words; {} of {} bytes remain live)",
        error.requested_words,
        error.live_bytes,
        error.capacity_bytes,
    );
    std::process::exit(1)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_clif_fail(code: i32) -> ! {
    let message = RuntimeFailure::from_code(code).map_or("runtime error", RuntimeFailure::message);
    let _ = writeln!(io::stderr().lock(), "Zydeco runtime: {message}");
    std::process::exit(1)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_alloc_slow(
    vmctx: *mut ClifVmCtxHeader, size_words: usize, kind: usize, roots_ptr: *mut Word,
    roots_len: usize,
) -> *mut u8 {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    unsafe { instance.heap.sync_bump_ptr(instance.header.alloc_ptr) };
    let stack = RootRange { start: instance.header.sps_base, end: instance.header.sps_sp };
    let frame = if roots_len == 0 {
        RootRange::EMPTY
    } else {
        RootRange { start: roots_ptr, end: unsafe { roots_ptr.add(roots_len) } }
    };
    let tag = if kind == AllocationKind::Opaque as usize {
        AllocationKind::Opaque
    } else {
        AllocationKind::Scanned
    };
    let payload =
        unsafe { instance.heap.allocate(size_words, tag, Roots { stack, frame, slots: &mut [] }) }
            .unwrap_or_else(|err| out_of_memory(err));
    instance.refresh_bump_bounds();
    payload
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_grow_sps(
    vmctx: *mut ClifVmCtxHeader, current_sp: *mut Word, needed_words: usize,
) -> *mut Word {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let old_base = instance.sps_storage.as_ptr();
    let used_words = unsafe { current_sp.offset_from(old_base) };
    debug_assert!(used_words >= 0, "SPS stack pointer precedes stack base");
    let used_words = used_words as usize;
    let old_cap = instance.sps_storage.len();
    debug_assert!(used_words <= old_cap, "SPS stack pointer overflowed capacity");

    let min_cap = used_words.checked_add(needed_words).expect("SPS stack capacity overflow");
    let new_cap =
        old_cap.saturating_mul(2).max(min_cap).checked_next_power_of_two().unwrap_or(min_cap);
    let mut new_storage = vec![0_usize; new_cap].into_boxed_slice();
    if used_words != 0 {
        new_storage[..used_words].copy_from_slice(&instance.sps_storage[..used_words]);
    }
    instance.sps_storage = new_storage;
    instance.refresh_sps_bounds(used_words);
    instance.header.sps_sp
}

/* ----------------------------- Scalar Helpers ----------------------------- */

fn load_opaque(word: Word) -> Word {
    debug_assert_eq!(word & IMMEDIATE_TAG, 0, "expected boxed scalar pointer");
    debug_assert_ne!(word, 0, "boxed scalar pointer is null");
    unsafe { (word as *const Word).read() }
}

fn store_opaque(spare: *mut Word, bits: Word) -> Word {
    debug_assert!(!spare.is_null(), "wide scalar op missing spare box");
    unsafe { spare.write(bits) };
    spare as Word
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_exit(_vmctx: *mut ClifVmCtxHeader, code: Word) -> ! {
    std::process::exit(Immediate::decode_signed(code) as i32)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_int64_from_int(
    _vmctx: *mut ClifVmCtxHeader, value: Word, spare: *mut Word,
) -> Word {
    store_opaque(spare, Immediate::decode_signed(value) as Word)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_uint64_from_uint(
    _vmctx: *mut ClifVmCtxHeader, value: Word, spare: *mut Word,
) -> Word {
    store_opaque(spare, Immediate::decode_unsigned(value))
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_int64_to_int_branch(
    vmctx: *mut ClifVmCtxHeader, value: Word, when_none: Word, when_some: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    match Immediate::signed(load_opaque(value) as i64) {
        | Some(encoded) => instance.set_transfer_one(when_some, encoded),
        | None => instance.set_transfer_none(when_none),
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_uint64_to_uint_branch(
    vmctx: *mut ClifVmCtxHeader, value: Word, when_none: Word, when_some: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    match Immediate::unsigned(load_opaque(value)) {
        | Some(encoded) => instance.set_transfer_one(when_some, encoded),
        | None => instance.set_transfer_none(when_none),
    }
}

macro_rules! clif_signed_render {
    ($fn_name:ident, $ty:ty) => {
        #[unsafe(no_mangle)]
        unsafe extern "C" fn $fn_name(vmctx: *mut ClifVmCtxHeader, value: Word) -> Word {
            let rendered = (Immediate::decode_signed(value) as $ty).to_string();
            let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
            unsafe { instance.allocate_string_bytes(rendered.as_bytes(), &mut []) }
        }
    };
}

macro_rules! clif_unsigned_render {
    ($fn_name:ident, $ty:ty) => {
        #[unsafe(no_mangle)]
        unsafe extern "C" fn $fn_name(vmctx: *mut ClifVmCtxHeader, value: Word) -> Word {
            let rendered = (Immediate::decode_unsigned(value) as $ty).to_string();
            let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
            unsafe { instance.allocate_string_bytes(rendered.as_bytes(), &mut []) }
        }
    };
}

clif_signed_render!(zydeco_clif_int8_to_string, i8);
clif_signed_render!(zydeco_clif_int16_to_string, i16);
clif_signed_render!(zydeco_clif_int32_to_string, i32);
clif_signed_render!(zydeco_clif_int_to_string, i64);
clif_unsigned_render!(zydeco_clif_uint8_to_string, u8);
clif_unsigned_render!(zydeco_clif_uint16_to_string, u16);
clif_unsigned_render!(zydeco_clif_uint32_to_string, u32);
clif_unsigned_render!(zydeco_clif_uint_to_string, u64);

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_int64_to_string(vmctx: *mut ClifVmCtxHeader, value: Word) -> Word {
    let rendered = (load_opaque(value) as i64).to_string();
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    unsafe { instance.allocate_string_bytes(rendered.as_bytes(), &mut []) }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_uint64_to_string(
    vmctx: *mut ClifVmCtxHeader, value: Word,
) -> Word {
    let rendered = (load_opaque(value) as u64).to_string();
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    unsafe { instance.allocate_string_bytes(rendered.as_bytes(), &mut []) }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_float32_to_string(
    vmctx: *mut ClifVmCtxHeader, value: Word,
) -> Word {
    let rendered = f32::from_bits(Immediate::decode_unsigned(value) as u32).to_string();
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    unsafe { instance.allocate_string_bytes(rendered.as_bytes(), &mut []) }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_float64_to_string(
    vmctx: *mut ClifVmCtxHeader, value: Word,
) -> Word {
    let rendered = f64::from_bits(load_opaque(value) as u64).to_string();
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    unsafe { instance.allocate_string_bytes(rendered.as_bytes(), &mut []) }
}

/* ----------------------------- String & Char ------------------------------ */

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_str_scalar_length(
    _vmctx: *mut ClifVmCtxHeader, string: Word,
) -> Word {
    Immediate::expect_signed(unsafe { borrow_heap_str(string) }.chars().count() as i64)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_str_byte_length(
    _vmctx: *mut ClifVmCtxHeader, string: Word,
) -> Word {
    Immediate::expect_signed(unsafe { borrow_heap_str(string) }.len() as i64)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_str_append(
    vmctx: *mut ClifVmCtxHeader, mut first: Word, mut second: Word,
) -> Word {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let total_len = unsafe { borrow_heap_str(first).len() + borrow_heap_str(second).len() };
    let payload = unsafe {
        instance
            .allocate_opaque(heap_string_words(total_len), &mut [&raw mut first, &raw mut second])
    };
    let first_bytes = unsafe { borrow_heap_str(first).as_bytes() };
    let second_bytes = unsafe { borrow_heap_str(second).as_bytes() };
    unsafe { write_heap_string_concat(payload, first_bytes, second_bytes) };
    payload as Word
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_str_get_branch(
    vmctx: *mut ClifVmCtxHeader, string: Word, index: Word, when_none: Word, when_some: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let character = usize::try_from(Immediate::decode_signed(index))
        .ok()
        .and_then(|idx| unsafe { borrow_heap_str(string) }.chars().nth(idx));
    match character {
        | None => instance.set_transfer_none(when_none),
        | Some(ch) => instance.set_transfer_one(when_some, Immediate::expect_unsigned(ch as Word)),
    }
}

fn emit_optional_pair_ranges(
    instance: &mut ClifRuntimeInstance, mut string: Word, ranges: Option<(usize, usize, usize)>,
    when_none: Word, mut when_some: Word,
) {
    let Some((first_len, second_start, second_len)) = ranges else {
        instance.set_transfer_none(when_none);
        return;
    };
    let first_payload = unsafe {
        instance.allocate_opaque(
            heap_string_words(first_len),
            &mut [&raw mut string, &raw mut when_some],
        )
    };
    let first_slice = unsafe { &borrow_heap_str(string).as_bytes()[..first_len] };
    unsafe { write_heap_string(first_payload, first_slice) };
    let mut first_word = first_payload as Word;

    let second_payload = unsafe {
        instance.allocate_opaque(
            heap_string_words(second_len),
            &mut [&raw mut string, &raw mut when_some, &raw mut first_word],
        )
    };
    let second_slice =
        unsafe { &borrow_heap_str(string).as_bytes()[second_start..second_start + second_len] };
    unsafe { write_heap_string(second_payload, second_slice) };
    instance.set_transfer_two(when_some, first_word, second_payload as Word);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_str_split_once_branch(
    vmctx: *mut ClifVmCtxHeader, string: Word, separator: Word, when_none: Word, when_some: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let separator = char::from_u32(Immediate::decode_unsigned(separator) as u32)
        .expect("invalid separator char");
    let ranges = split_once_byte_ranges(unsafe { borrow_heap_str(string) }, separator);
    emit_optional_pair_ranges(instance, string, ranges, when_none, when_some);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_str_split_at_branch(
    vmctx: *mut ClifVmCtxHeader, string: Word, index: Word, when_none: Word, when_some: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let text = unsafe { borrow_heap_str(string) };
    let ranges = split_at_scalar_byte(text, Immediate::decode_signed(index))
        .map(|split| (split, split, text.len() - split));
    emit_optional_pair_ranges(instance, string, ranges, when_none, when_some);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_str_eq_branch(
    vmctx: *mut ClifVmCtxHeader, first: Word, second: Word, when_true: Word, when_false: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let equal = unsafe { borrow_heap_str(first) == borrow_heap_str(second) };
    instance.set_transfer_none(if equal { when_true } else { when_false });
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_str_parse_int_branch(
    vmctx: *mut ClifVmCtxHeader, string: Word, when_none: Word, when_some: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    match parse_signed_int(unsafe { borrow_heap_str(string) }).and_then(Immediate::signed) {
        | None => instance.set_transfer_none(when_none),
        | Some(value) => instance.set_transfer_one(when_some, value),
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_char_to_str(vmctx: *mut ClifVmCtxHeader, character: Word) -> Word {
    let ch =
        char::from_u32(Immediate::decode_unsigned(character) as u32).expect("invalid character");
    let mut buffer = [0_u8; 4];
    let encoded = ch.encode_utf8(&mut buffer);
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    unsafe { instance.allocate_string_bytes(encoded.as_bytes(), &mut []) }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_char_codepoint(
    _vmctx: *mut ClifVmCtxHeader, character: Word,
) -> Word {
    Immediate::expect_signed(Immediate::decode_unsigned(character) as i64)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_char_from_codepoint_branch(
    vmctx: *mut ClifVmCtxHeader, codepoint: Word, when_none: Word, when_some: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    match char_from_codepoint(Immediate::decode_signed(codepoint)) {
        | None => instance.set_transfer_none(when_none),
        | Some(ch) => instance.set_transfer_one(when_some, Immediate::expect_unsigned(ch as Word)),
    }
}

/* ----------------------------------- I/O ---------------------------------- */

pub(super) struct ClifIoBranch;

impl ClifIoBranch {
    fn import_memory(instance: &ClifRuntimeInstance, bytes: &[u8]) -> io::Result<(Word, Word)> {
        instance
            .memory
            .borrow_mut()
            .import(bytes)
            .map(|address| (address.expose(), Immediate::expect_signed(bytes.len() as i64)))
            .map_err(HostIoError::memory)
    }

    fn error(instance: &mut ClifRuntimeInstance, mut continuation: Word, error: io::Error) {
        let kind = HostIoErrorKind::from_io_error(&error) as i64;
        let message = error.to_string();
        let owned = unsafe {
            instance.allocate_string_bytes(message.as_bytes(), &mut [&raw mut continuation])
        };
        instance.set_transfer_two(continuation, Immediate::expect_signed(kind), owned);
    }

    fn bytes(
        instance: &mut ClifRuntimeInstance, result: io::Result<(Word, Word)>, error: Word,
        success: Word,
    ) {
        match result {
            | Ok((address, length)) => instance.set_transfer_two(success, address, length),
            | Err(fault) => Self::error(instance, error, fault),
        }
    }

    fn unit(
        instance: &mut ClifRuntimeInstance, result: io::Result<()>, when_error: Word,
        when_success: Word,
    ) {
        match result {
            | Ok(()) => instance.set_transfer_none(when_success),
            | Err(error) => Self::error(instance, when_error, error),
        }
    }

    fn value(
        instance: &mut ClifRuntimeInstance, result: io::Result<Word>, when_error: Word,
        when_success: Word,
    ) {
        match result {
            | Ok(value) => instance.set_transfer_one(when_success, value),
            | Err(error) => Self::error(instance, when_error, error),
        }
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_stdin(_vmctx: *mut ClifVmCtxHeader) -> Word {
    Immediate::expect_unsigned(ReaderHandle::STDIN.raw())
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_stdout(_vmctx: *mut ClifVmCtxHeader) -> Word {
    Immediate::expect_unsigned(WriterHandle::STDOUT.raw())
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_stderr(_vmctx: *mut ClifVmCtxHeader) -> Word {
    Immediate::expect_unsigned(WriterHandle::STDERR.raw())
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_io_read(
    vmctx: *mut ClifVmCtxHeader, reader: Word, count: Word, when_error: Word, when_success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let reader = ReaderHandle::from_raw(Immediate::decode_unsigned(reader));
    let count = Immediate::decode_signed(count);
    let result = instance.io.borrow_mut().read_process(reader, |handle| {
        let bytes = LineInput::read_up_to(handle, count)?;
        ClifIoBranch::import_memory(instance, &bytes)
    });
    ClifIoBranch::bytes(instance, result, when_error, when_success);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_io_read_line(
    vmctx: *mut ClifVmCtxHeader, reader: Word, when_error: Word, when_eof: Word, when_line: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let reader = ReaderHandle::from_raw(Immediate::decode_unsigned(reader));
    let result = instance.io.borrow_mut().read_process(reader, LineInput::read_line_bytes);
    match result {
        | Ok((0, _)) => instance.set_transfer_none(when_eof),
        | Ok((_, bytes)) => {
            let imported = ClifIoBranch::import_memory(instance, &bytes);
            ClifIoBranch::bytes(instance, imported, when_error, when_line);
        }
        | Err(error) => ClifIoBranch::error(instance, when_error, error),
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_io_read_all(
    vmctx: *mut ClifVmCtxHeader, reader: Word, when_error: Word, when_success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let reader = ReaderHandle::from_raw(Immediate::decode_unsigned(reader));
    let result = instance.io.borrow_mut().read_process(reader, |handle| {
        let bytes = LineInput::read_all(handle)?;
        ClifIoBranch::import_memory(instance, &bytes)
    });
    ClifIoBranch::bytes(instance, result, when_error, when_success);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_io_write_all(
    vmctx: *mut ClifVmCtxHeader, writer: Word, address: Word, length: Word, when_error: Word,
    when_success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let writer = WriterHandle::from_raw(Immediate::decode_unsigned(writer));
    let bytes = unsafe {
        zydeco_machine::memory::Address::from_exposed(address)
            .bytes(Immediate::decode_signed(length) as usize)
    };
    let result = instance.io.borrow_mut().write_process(writer, |handle| handle.write_all(bytes));
    ClifIoBranch::unit(instance, result, when_error, when_success);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_io_flush(
    vmctx: *mut ClifVmCtxHeader, writer: Word, when_error: Word, when_success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let writer = WriterHandle::from_raw(Immediate::decode_unsigned(writer));
    let result = instance.io.borrow_mut().write_process(writer, |handle| handle.flush());
    ClifIoBranch::unit(instance, result, when_error, when_success);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_io_close_reader(
    vmctx: *mut ClifVmCtxHeader, reader: Word, when_error: Word, when_success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let reader = ReaderHandle::from_raw(Immediate::decode_unsigned(reader));
    let result = instance.io.borrow_mut().close_reader(reader);
    ClifIoBranch::unit(instance, result, when_error, when_success);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_io_close_writer(
    vmctx: *mut ClifVmCtxHeader, writer: Word, when_error: Word, when_success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let writer = WriterHandle::from_raw(Immediate::decode_unsigned(writer));
    let result = instance.io.borrow_mut().close_writer_process(writer);
    ClifIoBranch::unit(instance, result, when_error, when_success);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_fs_open_reader(
    vmctx: *mut ClifVmCtxHeader, path: Word, when_error: Word, when_success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let result = instance
        .io
        .borrow_mut()
        .open_reader(unsafe { borrow_heap_str(path) })
        .map(|handle| Immediate::expect_unsigned(handle.raw()));
    ClifIoBranch::value(instance, result, when_error, when_success);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_fs_create_writer(
    vmctx: *mut ClifVmCtxHeader, path: Word, when_error: Word, when_success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let result = instance
        .io
        .borrow_mut()
        .create_writer(unsafe { borrow_heap_str(path) })
        .map(|handle| Immediate::expect_unsigned(handle.raw()));
    ClifIoBranch::value(instance, result, when_error, when_success);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_fs_append_writer(
    vmctx: *mut ClifVmCtxHeader, path: Word, when_error: Word, when_success: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let result = instance
        .io
        .borrow_mut()
        .append_writer(unsafe { borrow_heap_str(path) })
        .map(|handle| Immediate::expect_unsigned(handle.raw()));
    ClifIoBranch::value(instance, result, when_error, when_success);
}

/* ------------------------------- Legacy OS -------------------------------- */

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_read_line(vmctx: *mut ClifVmCtxHeader, mut continuation: Word) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let line = instance
        .io
        .borrow_mut()
        .read_process(ReaderHandle::STDIN, LineInput::read_line)
        .expect("legacy standard-input read failed");
    let owned =
        unsafe { instance.allocate_string_bytes(line.as_bytes(), &mut [&raw mut continuation]) };
    instance.set_transfer_one(continuation, owned);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_read_line_as_int_branch(
    vmctx: *mut ClifVmCtxHeader, when_invalid: Word, when_valid: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let line = instance
        .io
        .borrow_mut()
        .read_process(ReaderHandle::STDIN, LineInput::read_line)
        .expect("legacy standard-input read failed");
    match parse_signed_int(&line).and_then(Immediate::signed) {
        | Some(integer) => instance.set_transfer_one(when_valid, integer),
        | None => instance.set_transfer_none(when_invalid),
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_read_till_eof(
    vmctx: *mut ClifVmCtxHeader, mut continuation: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let remaining = instance
        .io
        .borrow_mut()
        .read_process(ReaderHandle::STDIN, LineInput::read_remaining)
        .expect("legacy standard-input read failed");
    let owned = unsafe {
        instance.allocate_string_bytes(remaining.as_bytes(), &mut [&raw mut continuation])
    };
    instance.set_transfer_one(continuation, owned);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_write_str(
    vmctx: *mut ClifVmCtxHeader, string: Word, continuation: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let bytes = unsafe { borrow_heap_str(string).as_bytes() };
    instance
        .io
        .borrow_mut()
        .write_process(WriterHandle::STDOUT, |writer| {
            writer.write_all(bytes)?;
            writer.flush()
        })
        .expect("legacy standard-output write failed");
    instance.set_transfer_none(continuation);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_write_int(
    vmctx: *mut ClifVmCtxHeader, integer: Word, continuation: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let value = Immediate::decode_signed(integer);
    instance
        .io
        .borrow_mut()
        .write_process(WriterHandle::STDOUT, |writer| {
            write!(writer, "{value}")?;
            writer.flush()
        })
        .expect("legacy standard-output write failed");
    instance.set_transfer_none(continuation);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_write_line(
    vmctx: *mut ClifVmCtxHeader, line: Word, continuation: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let text = unsafe { borrow_heap_str(line) };
    instance
        .io
        .borrow_mut()
        .write_process(WriterHandle::STDOUT, |writer| {
            writeln!(writer, "{text}")?;
            writer.flush()
        })
        .expect("legacy standard-output write failed");
    instance.set_transfer_none(continuation);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_arg_at(
    vmctx: *mut ClifVmCtxHeader, index: Word, when_none: Word, when_some: Word,
) {
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    let index = usize::try_from(Immediate::decode_signed(index)).ok();
    let argument = index.and_then(|idx| instance.arguments.get(idx)).map(StaticHeapString::as_word);
    match argument {
        | Some(arg) => instance.set_transfer_one(when_some, arg),
        | None => instance.set_transfer_none(when_none),
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn zydeco_clif_random_int(vmctx: *mut ClifVmCtxHeader, continuation: Word) {
    let Ok(integer) = random_signed_int() else {
        let _ = writeln!(io::stderr().lock(), "Zydeco runtime: cannot read random source");
        std::process::exit(1);
    };
    let instance = unsafe { ClifRuntimeInstance::from_vmctx(vmctx) };
    instance.set_transfer_one(continuation, Immediate::expect_signed(integer));
}

#[cfg(test)]
mod clif_tests {
    use super::*;

    #[test]
    fn clif_instance_lifecycle_and_reentrant_guard_behave() {
        let first = AtomicBool::new(false);
        let second = AtomicBool::new(false);

        unsafe {
            let outer = zydeco_clif_entry_begin(&first);
            let outer_str =
                ClifRuntimeInstance::from_vmctx(outer).allocate_string_bytes(b"outer", &mut []);

            let inner = zydeco_clif_entry_begin(&second);
            assert_ne!(inner, outer);
            zydeco_clif_entry_end(inner);
            assert!(!second.load(Ordering::Relaxed));
            assert!(first.load(Ordering::Relaxed));

            assert_eq!(borrow_heap_str(outer_str), "outer");
            zydeco_clif_entry_end(outer);
            assert!(!first.load(Ordering::Relaxed));

            let reused = zydeco_clif_entry_begin(&first);
            assert_eq!(reused, inner);
            zydeco_clif_entry_end(reused);
        }
    }

    #[test]
    fn grow_sps_and_slow_alloc_preserve_roots() {
        unsafe {
            let vmctx = zydeco_clif_entry_begin(ptr::null());
            let sps_start = (*vmctx).sps_base;
            let mut sp = sps_start;
            for i in 0..INITIAL_SPS_WORDS + 16 {
                let cap = (*vmctx).sps_limit.offset_from(sp) as usize;
                if cap == 0 {
                    sp = zydeco_clif_grow_sps(vmctx, sp, 1);
                }
                sp.write(Immediate::expect_signed(i as i64));
                sp = sp.add(1);
            }
            (*vmctx).sps_sp = sp;
            assert!(
                ((*vmctx).sps_limit.offset_from((*vmctx).sps_base) as usize) > INITIAL_SPS_WORDS
            );

            let mut frame = [1_usize; 2];
            let s1 = zydeco_clif_alloc_slow(
                vmctx,
                1,
                AllocationKind::Scanned as usize,
                frame.as_mut_ptr(),
                frame.len(),
            );
            s1.cast::<Word>().write(Immediate::expect_signed(99));
            frame[0] = s1 as Word;
            let s2 = zydeco_clif_alloc_slow(
                vmctx,
                16384,
                AllocationKind::Opaque as usize,
                frame.as_mut_ptr(),
                frame.len(),
            );
            assert!(!s2.is_null());
            assert_eq!((frame[0] as *const Word).read(), Immediate::expect_signed(99));
            zydeco_clif_entry_end(vmctx);
        }
    }
}
