//! Standalone WASI (`wasm32-wasip1`) host runtime linked with `wasm-sps` object files.
//!
//! Shares process I/O handles, managed byte buffers, string algorithms, and OS entropy
//! with the native runtime while keeping `wasm32` linear-memory representation helpers
//! (`u64`-length strings, boxed 64-bit scalars, and single-threaded transfer records).

use alloc::{
    alloc::{Layout, alloc, dealloc, handle_alloc_error},
    string::ToString,
    vec::Vec,
};
use core::{
    mem::MaybeUninit,
    ptr::{self, NonNull},
    slice, str,
};
use std::io::{self, Write};
use zydeco_abi::{HostIoErrorKind, RuntimeFailure};
use zydeco_machine::{
    memory::{Address, MemoryError, MemoryLayout, RetainedMemory},
    word::RuntimeWord,
};

use crate::{
    io::{HostIoError, HostIoRuntime, LineInput, ReaderHandle, WriterHandle},
    random::random_signed_int,
    string::{
        char_from_codepoint, parse_signed_int, scalar_at, scalar_len, split_at_scalar_byte,
        split_once_byte_ranges,
    },
};

const ARENA_ALIGNMENT: usize = 8;
const ARENA_CHUNK_BYTES: usize = 64 * 1024;
const WASM_STRING_LEN_BYTES: usize = core::mem::size_of::<u64>();

/// Published control-transfer record returned by every `Control`-mode Wasm host call.
///
/// The Wasm caller reads fields at fixed offsets `0` (`count`), `8` (`closure`),
/// `16` (`first`), and `24` (`second`) before any subsequent host call occurs.
#[repr(C)]
pub struct WasmHostTransfer {
    pub count: u64,
    pub closure: u64,
    pub first: u64,
    pub second: u64,
}

struct ArenaChunk {
    ptr: NonNull<u8>,
    layout: Layout,
}

impl Drop for ArenaChunk {
    fn drop(&mut self) {
        unsafe { dealloc(self.ptr.as_ptr(), self.layout) };
    }
}

/// Bump arena delivering non-null 8-byte-aligned linear-memory blocks for SPS closures
/// and dynamic strings.
struct WasmArena {
    cursor: usize,
    end: usize,
    chunks: Vec<ArenaChunk>,
}

impl WasmArena {
    const fn new() -> Self {
        Self { cursor: 0, end: 0, chunks: Vec::new() }
    }

    fn alloc(&mut self, bytes: usize) -> *mut u8 {
        let size = bytes.max(ARENA_ALIGNMENT).next_multiple_of(ARENA_ALIGNMENT);
        if self.end.saturating_sub(self.cursor) < size {
            let chunk_bytes = size.max(ARENA_CHUNK_BYTES);
            let layout =
                Layout::from_size_align(chunk_bytes, ARENA_ALIGNMENT).expect("valid arena layout");
            let raw = unsafe { alloc(layout) };
            let Some(ptr) = NonNull::new(raw) else {
                handle_alloc_error(layout);
            };
            let base = ptr.as_ptr() as usize;
            debug_assert!(base != 0 && base.is_multiple_of(ARENA_ALIGNMENT));
            self.cursor = base;
            self.end = base + chunk_bytes;
            self.chunks.push(ArenaChunk { ptr, layout });
        }
        let allocated = self.cursor as *mut u8;
        self.cursor += size;
        allocated
    }
}

struct WasmHostState {
    arena: WasmArena,
    io: HostIoRuntime,
    retained: RetainedMemory,
    arguments: Vec<u64>,
    transfer: WasmHostTransfer,
}

impl WasmHostState {
    fn new() -> Self {
        Self {
            arena: WasmArena::new(),
            io: HostIoRuntime::new(),
            retained: RetainedMemory::default(),
            arguments: Vec::new(),
            transfer: WasmHostTransfer { count: 0, closure: 0, first: 0, second: 0 },
        }
    }

    fn alloc_string(&mut self, bytes: &[u8]) -> u64 {
        let total = WASM_STRING_LEN_BYTES.checked_add(bytes.len()).expect("string length overflow");
        let payload = self.arena.alloc(total);
        let len_bytes = (bytes.len() as u64).to_le_bytes();
        unsafe {
            ptr::copy_nonoverlapping(len_bytes.as_ptr(), payload, WASM_STRING_LEN_BYTES);
            if !bytes.is_empty() {
                ptr::copy_nonoverlapping(
                    bytes.as_ptr(),
                    payload.add(WASM_STRING_LEN_BYTES),
                    bytes.len(),
                );
            }
        }
        payload as usize as u64
    }

    fn alloc_concat(&mut self, first: &[u8], second: &[u8]) -> u64 {
        let total_bytes = first.len().checked_add(second.len()).expect("concat length overflow");
        let total = WASM_STRING_LEN_BYTES.checked_add(total_bytes).expect("string length overflow");
        let payload = self.arena.alloc(total);
        let len_bytes = (total_bytes as u64).to_le_bytes();
        unsafe {
            ptr::copy_nonoverlapping(len_bytes.as_ptr(), payload, WASM_STRING_LEN_BYTES);
            if !first.is_empty() {
                ptr::copy_nonoverlapping(
                    first.as_ptr(),
                    payload.add(WASM_STRING_LEN_BYTES),
                    first.len(),
                );
            }
            if !second.is_empty() {
                ptr::copy_nonoverlapping(
                    second.as_ptr(),
                    payload.add(WASM_STRING_LEN_BYTES + first.len()),
                    second.len(),
                );
            }
        }
        payload as usize as u64
    }

    fn transfer_none(&mut self, closure: u64) -> *const WasmHostTransfer {
        self.transfer = WasmHostTransfer { count: 0, closure, first: 0, second: 0 };
        ptr::addr_of!(self.transfer)
    }

    fn transfer_one(&mut self, closure: u64, first: u64) -> *const WasmHostTransfer {
        self.transfer = WasmHostTransfer { count: 1, closure, first, second: 0 };
        ptr::addr_of!(self.transfer)
    }

    fn transfer_two(&mut self, closure: u64, first: u64, second: u64) -> *const WasmHostTransfer {
        self.transfer = WasmHostTransfer { count: 2, closure, first, second };
        ptr::addr_of!(self.transfer)
    }

    fn transfer_io_error(
        &mut self, continuation: u64, error: io::Error,
    ) -> *const WasmHostTransfer {
        let kind = HostIoErrorKind::from_io_error(&error) as i64;
        let message = self.alloc_string(error.to_string().as_bytes());
        self.transfer_two(continuation, encode_signed(kind), message)
    }

    fn transfer_io_unit(
        &mut self, result: io::Result<()>, when_error: u64, when_success: u64,
    ) -> *const WasmHostTransfer {
        match result {
            | Ok(()) => self.transfer_none(when_success),
            | Err(error) => self.transfer_io_error(when_error, error),
        }
    }

    fn transfer_io_value(
        &mut self, result: io::Result<u64>, when_error: u64, when_success: u64,
    ) -> *const WasmHostTransfer {
        match result {
            | Ok(value) => self.transfer_one(when_success, value),
            | Err(error) => self.transfer_io_error(when_error, error),
        }
    }

    fn transfer_io_bytes(
        &mut self, result: io::Result<(u64, u64)>, when_error: u64, when_success: u64,
    ) -> *const WasmHostTransfer {
        match result {
            | Ok((address, length)) => self.transfer_two(when_success, address, length),
            | Err(error) => self.transfer_io_error(when_error, error),
        }
    }

    fn transfer_memory_result(
        &mut self, result: Result<Option<u64>, MemoryError>, when_error: u64, when_success: u64,
    ) -> *const WasmHostTransfer {
        match result {
            | Ok(Some(value)) => self.transfer_one(when_success, value),
            | Ok(None) => self.transfer_none(when_success),
            | Err(code) => self.transfer_one(when_error, encode_signed(code as i64)),
        }
    }
}

static mut HOST_STATE: MaybeUninit<WasmHostState> = MaybeUninit::uninit();
static mut HOST_INITIALIZED: bool = false;

fn state() -> &'static mut WasmHostState {
    debug_assert!(unsafe { HOST_INITIALIZED }, "Wasm host state used before run_process");
    unsafe { &mut *ptr::addr_of_mut!(HOST_STATE).cast::<WasmHostState>() }
}

struct StateResetGuard;

impl Drop for StateResetGuard {
    fn drop(&mut self) {
        if unsafe { HOST_INITIALIZED } {
            unsafe {
                ptr::addr_of_mut!(HOST_STATE).cast::<WasmHostState>().drop_in_place();
                HOST_INITIALIZED = false;
            }
        }
    }
}

/// Initializes the WASI host state, populates CLI arguments (skipping `argv[0]`), and
/// invokes the compiled SPS entry function.
pub fn run_process(entry: unsafe extern "C" fn()) {
    unsafe {
        if HOST_INITIALIZED {
            ptr::addr_of_mut!(HOST_STATE).cast::<WasmHostState>().drop_in_place();
        }
        ptr::addr_of_mut!(HOST_STATE).cast::<WasmHostState>().write(WasmHostState::new());
        HOST_INITIALIZED = true;
    }
    let _guard = StateResetGuard;
    let host = state();
    let args: Vec<u64> =
        std::env::args().skip(1).map(|arg| host.alloc_string(arg.as_bytes())).collect();
    host.arguments = args;
    unsafe { entry() };
}

fn decode_signed(word: u64) -> i64 {
    RuntimeWord::decode_signed(word).expect("expected tagged signed integer")
}

fn decode_unsigned(word: u64) -> u64 {
    RuntimeWord::decode_unsigned(word).expect("expected tagged unsigned integer")
}

fn encode_signed(value: i64) -> u64 {
    RuntimeWord::signed(value).expect("signed integer out of tagged range")
}

fn encode_unsigned(value: u64) -> u64 {
    RuntimeWord::unsigned(value).expect("unsigned integer out of tagged range")
}

fn address_from_word(word: u64) -> Address {
    Address::from_exposed(word as usize)
}

unsafe fn load_opaque(word: u64) -> u64 {
    let ptr = (word as usize) as *const u64;
    unsafe { ptr::read_unaligned(ptr) }
}

unsafe fn store_opaque(spare: *mut u64, bits: u64) -> u64 {
    unsafe { ptr::write_unaligned(spare, bits) };
    (spare as usize) as u64
}

unsafe fn borrow_wasm_str<'a>(raw: u64) -> &'a str {
    let base = (raw as usize) as *const u8;
    let mut len_bytes = [0_u8; WASM_STRING_LEN_BYTES];
    unsafe { ptr::copy_nonoverlapping(base, len_bytes.as_mut_ptr(), WASM_STRING_LEN_BYTES) };
    let len = usize::try_from(u64::from_le_bytes(len_bytes)).expect("string length fits usize");
    let bytes = if len == 0 {
        &[]
    } else {
        unsafe { slice::from_raw_parts(base.add(WASM_STRING_LEN_BYTES), len) }
    };
    unsafe { str::from_utf8_unchecked(bytes) }
}

fn import_bytes(host: &mut WasmHostState, bytes: &[u8]) -> io::Result<(u64, u64)> {
    host.retained
        .import(bytes)
        .map(|address| (address.expose() as u64, encode_signed(bytes.len() as i64)))
        .map_err(HostIoError::memory)
}

/* ---------------------------- Runtime Mechanics --------------------------- */

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_wasm_alloc(words: u32) -> u32 {
    let bytes = (words as usize).checked_mul(ARENA_ALIGNMENT).expect("allocation size overflow");
    state().arena.alloc(bytes) as usize as u32
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_runtime_error(code: i32) -> ! {
    let message = RuntimeFailure::from_code(code).map_or("runtime error", RuntimeFailure::message);
    let _ = writeln!(io::stderr().lock(), "Zydeco runtime: {message}");
    std::process::exit(1);
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_exit(code: u64) -> *const WasmHostTransfer {
    std::process::exit(decode_signed(code) as i32);
}

/* ----------------------------- Pure Primitives ---------------------------- */

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_str_scalar_length(string: u64) -> u64 {
    encode_signed(scalar_len(unsafe { borrow_wasm_str(string) }) as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_str_byte_length(string: u64) -> u64 {
    encode_signed(unsafe { borrow_wasm_str(string) }.len() as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_str_append(first: u64, second: u64) -> u64 {
    let first_bytes = unsafe { borrow_wasm_str(first) }.as_bytes().to_vec();
    let second_bytes = unsafe { borrow_wasm_str(second) }.as_bytes().to_vec();
    state().alloc_concat(&first_bytes, &second_bytes)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_str_get_branch(
    string: u64, index: u64, when_none: u64, when_some: u64,
) -> *const WasmHostTransfer {
    let host = state();
    match scalar_at(unsafe { borrow_wasm_str(string) }, decode_signed(index)) {
        | None => host.transfer_none(when_none),
        | Some(ch) => host.transfer_one(when_some, encode_unsigned(ch as u64)),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_str_split_once_branch(
    string: u64, separator: u64, when_none: u64, when_some: u64,
) -> *const WasmHostTransfer {
    let separator = char::from_u32(decode_unsigned(separator) as u32).expect("invalid separator");
    let text = unsafe { borrow_wasm_str(string) };
    let host = state();
    let Some((first_len, second_start, second_len)) = split_once_byte_ranges(text, separator)
    else {
        return host.transfer_none(when_none);
    };
    let bytes = text.as_bytes().to_vec();
    let first = host.alloc_string(&bytes[..first_len]);
    let second = host.alloc_string(&bytes[second_start..second_start + second_len]);
    host.transfer_two(when_some, first, second)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_str_split_at_branch(
    string: u64, index: u64, when_none: u64, when_some: u64,
) -> *const WasmHostTransfer {
    let text = unsafe { borrow_wasm_str(string) };
    let host = state();
    let Some(split) = split_at_scalar_byte(text, decode_signed(index)) else {
        return host.transfer_none(when_none);
    };
    let bytes = text.as_bytes().to_vec();
    let first = host.alloc_string(&bytes[..split]);
    let second = host.alloc_string(&bytes[split..]);
    host.transfer_two(when_some, first, second)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_str_eq_branch(
    first: u64, second: u64, when_true: u64, when_false: u64,
) -> *const WasmHostTransfer {
    let equal = unsafe { borrow_wasm_str(first) == borrow_wasm_str(second) };
    state().transfer_none(if equal { when_true } else { when_false })
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_str_parse_int_branch(
    string: u64, when_none: u64, when_some: u64,
) -> *const WasmHostTransfer {
    let host = state();
    match parse_signed_int(unsafe { borrow_wasm_str(string) }).and_then(RuntimeWord::signed) {
        | None => host.transfer_none(when_none),
        | Some(value) => host.transfer_one(when_some, value),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_char_to_str(character: u64) -> u64 {
    let ch = char::from_u32(decode_unsigned(character) as u32).expect("invalid character");
    let mut buffer = [0_u8; 4];
    let encoded = ch.encode_utf8(&mut buffer);
    state().alloc_string(encoded.as_bytes())
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_char_codepoint(character: u64) -> u64 {
    encode_signed(decode_unsigned(character) as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_char_from_codepoint_branch(
    codepoint: u64, when_none: u64, when_some: u64,
) -> *const WasmHostTransfer {
    let host = state();
    match char_from_codepoint(decode_signed(codepoint)) {
        | None => host.transfer_none(when_none),
        | Some(ch) => host.transfer_one(when_some, encode_unsigned(ch as u64)),
    }
}

/* ----------------------------- Numeric Codecs ----------------------------- */

macro_rules! wasm_signed_render {
    ($fn_name:ident, $ty:ty) => {
        #[unsafe(no_mangle)]
        pub extern "C" fn $fn_name(value: u64) -> u64 {
            let rendered = (decode_signed(value) as $ty).to_string();
            state().alloc_string(rendered.as_bytes())
        }
    };
}

macro_rules! wasm_unsigned_render {
    ($fn_name:ident, $ty:ty) => {
        #[unsafe(no_mangle)]
        pub extern "C" fn $fn_name(value: u64) -> u64 {
            let rendered = (decode_unsigned(value) as $ty).to_string();
            state().alloc_string(rendered.as_bytes())
        }
    };
}

wasm_signed_render!(zydeco_int8_to_string, i8);
wasm_signed_render!(zydeco_int16_to_string, i16);
wasm_signed_render!(zydeco_int32_to_string, i32);
wasm_signed_render!(zydeco_int_to_string, i64);
wasm_unsigned_render!(zydeco_uint8_to_string, u8);
wasm_unsigned_render!(zydeco_uint16_to_string, u16);
wasm_unsigned_render!(zydeco_uint32_to_string, u32);
wasm_unsigned_render!(zydeco_uint_to_string, u64);

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_int64_to_string(value: u64) -> u64 {
    let rendered = (unsafe { load_opaque(value) } as i64).to_string();
    state().alloc_string(rendered.as_bytes())
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_uint64_to_string(value: u64) -> u64 {
    let rendered = unsafe { load_opaque(value) }.to_string();
    state().alloc_string(rendered.as_bytes())
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_float32_to_string(value: u64) -> u64 {
    let rendered = f32::from_bits(decode_unsigned(value) as u32).to_string();
    state().alloc_string(rendered.as_bytes())
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_float64_to_string(value: u64) -> u64 {
    let rendered = f64::from_bits(unsafe { load_opaque(value) }).to_string();
    state().alloc_string(rendered.as_bytes())
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_int64_from_int(value: u64, spare: u32) -> u64 {
    unsafe { store_opaque(spare as usize as *mut u64, decode_signed(value) as u64) }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_uint64_from_uint(value: u64, spare: u32) -> u64 {
    unsafe { store_opaque(spare as usize as *mut u64, decode_unsigned(value)) }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_int64_to_int_branch(
    value: u64, when_none: u64, when_some: u64,
) -> *const WasmHostTransfer {
    let host = state();
    match RuntimeWord::signed(unsafe { load_opaque(value) } as i64) {
        | Some(encoded) => host.transfer_one(when_some, encoded),
        | None => host.transfer_none(when_none),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_uint64_to_uint_branch(
    value: u64, when_none: u64, when_some: u64,
) -> *const WasmHostTransfer {
    let host = state();
    match RuntimeWord::unsigned(unsafe { load_opaque(value) }) {
        | Some(encoded) => host.transfer_one(when_some, encoded),
        | None => host.transfer_none(when_none),
    }
}

/* ------------------------------ Manual Memory ----------------------------- */

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_memory_null() -> u64 {
    0
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_memory_allocate(
    size: u64, alignment: u64, error: u64, success: u64,
) -> *const WasmHostTransfer {
    let result = MemoryLayout::for_request(decode_signed(size), decode_signed(alignment))
        .and_then(MemoryLayout::allocate)
        .map(|address| Some(address.expose() as u64));
    state().transfer_memory_result(result, error, success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_memory_free(
    address: u64, size: u64, alignment: u64, error: u64, success: u64,
) -> *const WasmHostTransfer {
    let result =
        MemoryLayout::for_request(decode_signed(size), decode_signed(alignment)).map(|layout| {
            unsafe { layout.deallocate(address_from_word(address)) };
            None
        });
    state().transfer_memory_result(result, error, success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_memory_retain(
    address: u64, size: u64, alignment: u64, error: u64, success: u64,
) -> *const WasmHostTransfer {
    let host = state();
    let result = MemoryLayout::for_request(decode_signed(size), decode_signed(alignment))
        .and_then(|layout| unsafe { host.retained.retain(address_from_word(address), layout) })
        .map(|()| None);
    host.transfer_memory_result(result, error, success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_memory_copy(
    source: u64, destination: u64, count: u64, success: u64,
) -> *const WasmHostTransfer {
    let count = decode_signed(count) as usize;
    if count != 0 {
        unsafe {
            ptr::copy(
                address_from_word(source).pointer(),
                address_from_word(destination).pointer(),
                count,
            );
        }
    }
    state().transfer_none(success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_memory_fill(
    address: u64, count: u64, value: u64, success: u64,
) -> *const WasmHostTransfer {
    let count = decode_signed(count) as usize;
    if count != 0 {
        unsafe {
            address_from_word(address).pointer().write_bytes(decode_unsigned(value) as u8, count);
        }
    }
    state().transfer_none(success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_memory_from_string(
    string: u64, error: u64, success: u64,
) -> *const WasmHostTransfer {
    let bytes = unsafe { borrow_wasm_str(string) }.as_bytes();
    let host = state();
    match host.retained.import(bytes) {
        | Ok(address) => {
            host.transfer_two(success, address.expose() as u64, encode_signed(bytes.len() as i64))
        }
        | Err(code) => host.transfer_memory_result(Err(code), error, success),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_memory_to_string(
    address: u64, count: u64, error: u64, success: u64,
) -> *const WasmHostTransfer {
    let bytes = unsafe { address_from_word(address).bytes(decode_signed(count) as usize) };
    let host = state();
    match str::from_utf8(bytes) {
        | Ok(text) => {
            let owned = host.alloc_string(text.as_bytes());
            host.transfer_one(success, owned)
        }
        | Err(_) => host.transfer_memory_result(Err(MemoryError::InvalidEncoding), error, success),
    }
}

/* ----------------------------------- I/O ---------------------------------- */

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_stdin() -> u64 {
    encode_unsigned(ReaderHandle::STDIN.raw() as u64)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_stdout() -> u64 {
    encode_unsigned(WriterHandle::STDOUT.raw() as u64)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_stderr() -> u64 {
    encode_unsigned(WriterHandle::STDERR.raw() as u64)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_io_read(
    reader: u64, count: u64, when_error: u64, when_success: u64,
) -> *const WasmHostTransfer {
    let reader = ReaderHandle::from_raw(decode_unsigned(reader) as usize);
    let count = decode_signed(count);
    let host = state();
    let result = host
        .io
        .read_process(reader, |handle| LineInput::read_up_to(handle, count))
        .and_then(|bytes| import_bytes(host, &bytes));
    host.transfer_io_bytes(result, when_error, when_success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_io_read_line(
    reader: u64, when_error: u64, when_eof: u64, when_line: u64,
) -> *const WasmHostTransfer {
    let reader = ReaderHandle::from_raw(decode_unsigned(reader) as usize);
    let host = state();
    match host.io.read_process(reader, LineInput::read_line_bytes) {
        | Ok((0, _)) => host.transfer_none(when_eof),
        | Ok((_, bytes)) => {
            let imported = import_bytes(host, &bytes);
            host.transfer_io_bytes(imported, when_error, when_line)
        }
        | Err(error) => host.transfer_io_error(when_error, error),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_io_read_all(
    reader: u64, when_error: u64, when_success: u64,
) -> *const WasmHostTransfer {
    let reader = ReaderHandle::from_raw(decode_unsigned(reader) as usize);
    let host = state();
    let result = host
        .io
        .read_process(reader, LineInput::read_all)
        .and_then(|bytes| import_bytes(host, &bytes));
    host.transfer_io_bytes(result, when_error, when_success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_io_write_all(
    writer: u64, address: u64, length: u64, when_error: u64, when_success: u64,
) -> *const WasmHostTransfer {
    let writer = WriterHandle::from_raw(decode_unsigned(writer) as usize);
    let bytes = unsafe { address_from_word(address).bytes(decode_signed(length) as usize) };
    let host = state();
    let result = host.io.write_process(writer, |handle| handle.write_all(bytes));
    host.transfer_io_unit(result, when_error, when_success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_io_flush(
    writer: u64, when_error: u64, when_success: u64,
) -> *const WasmHostTransfer {
    let writer = WriterHandle::from_raw(decode_unsigned(writer) as usize);
    let host = state();
    let result = host.io.write_process(writer, |handle| handle.flush());
    host.transfer_io_unit(result, when_error, when_success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_io_close_reader(
    reader: u64, when_error: u64, when_success: u64,
) -> *const WasmHostTransfer {
    let reader = ReaderHandle::from_raw(decode_unsigned(reader) as usize);
    let host = state();
    let result = host.io.close_reader(reader);
    host.transfer_io_unit(result, when_error, when_success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_io_close_writer(
    writer: u64, when_error: u64, when_success: u64,
) -> *const WasmHostTransfer {
    let writer = WriterHandle::from_raw(decode_unsigned(writer) as usize);
    let host = state();
    let result = host.io.close_writer_process(writer);
    host.transfer_io_unit(result, when_error, when_success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_fs_open_reader(
    path: u64, when_error: u64, when_success: u64,
) -> *const WasmHostTransfer {
    let host = state();
    let result = host
        .io
        .open_reader(unsafe { borrow_wasm_str(path) })
        .map(|handle| encode_unsigned(handle.raw() as u64));
    host.transfer_io_value(result, when_error, when_success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_fs_create_writer(
    path: u64, when_error: u64, when_success: u64,
) -> *const WasmHostTransfer {
    let host = state();
    let result = host
        .io
        .create_writer(unsafe { borrow_wasm_str(path) })
        .map(|handle| encode_unsigned(handle.raw() as u64));
    host.transfer_io_value(result, when_error, when_success)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_fs_append_writer(
    path: u64, when_error: u64, when_success: u64,
) -> *const WasmHostTransfer {
    let host = state();
    let result = host
        .io
        .append_writer(unsafe { borrow_wasm_str(path) })
        .map(|handle| encode_unsigned(handle.raw() as u64));
    host.transfer_io_value(result, when_error, when_success)
}

/* ------------------------------- Legacy OS -------------------------------- */

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_read_line(continuation: u64) -> *const WasmHostTransfer {
    let host = state();
    let line = host
        .io
        .read_process(ReaderHandle::STDIN, LineInput::read_line)
        .expect("legacy standard-input read failed");
    let owned = host.alloc_string(line.as_bytes());
    host.transfer_one(continuation, owned)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_read_line_as_int_branch(
    when_invalid: u64, when_valid: u64,
) -> *const WasmHostTransfer {
    let host = state();
    let line = host
        .io
        .read_process(ReaderHandle::STDIN, LineInput::read_line)
        .expect("legacy standard-input read failed");
    match parse_signed_int(&line).and_then(RuntimeWord::signed) {
        | Some(integer) => host.transfer_one(when_valid, integer),
        | None => host.transfer_none(when_invalid),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_read_till_eof(continuation: u64) -> *const WasmHostTransfer {
    let host = state();
    let remaining = host
        .io
        .read_process(ReaderHandle::STDIN, LineInput::read_remaining)
        .expect("legacy standard-input read failed");
    let owned = host.alloc_string(remaining.as_bytes());
    host.transfer_one(continuation, owned)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_write_str(string: u64, continuation: u64) -> *const WasmHostTransfer {
    let bytes = unsafe { borrow_wasm_str(string) }.as_bytes();
    let host = state();
    host.io
        .write_process(WriterHandle::STDOUT, |writer| {
            writer.write_all(bytes)?;
            writer.flush()
        })
        .expect("legacy standard-output write failed");
    host.transfer_none(continuation)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_write_int(integer: u64, continuation: u64) -> *const WasmHostTransfer {
    let value = decode_signed(integer);
    let host = state();
    host.io
        .write_process(WriterHandle::STDOUT, |writer| {
            write!(writer, "{value}")?;
            writer.flush()
        })
        .expect("legacy standard-output write failed");
    host.transfer_none(continuation)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_write_line(line: u64, continuation: u64) -> *const WasmHostTransfer {
    let text = unsafe { borrow_wasm_str(line) };
    let host = state();
    host.io
        .write_process(WriterHandle::STDOUT, |writer| {
            writeln!(writer, "{text}")?;
            writer.flush()
        })
        .expect("legacy standard-output write failed");
    host.transfer_none(continuation)
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_arg_at(
    index: u64, when_none: u64, when_some: u64,
) -> *const WasmHostTransfer {
    let host = state();
    let argument = usize::try_from(decode_signed(index))
        .ok()
        .and_then(|index| host.arguments.get(index).copied());
    match argument {
        | Some(argument) => host.transfer_one(when_some, argument),
        | None => host.transfer_none(when_none),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn zydeco_random_int(continuation: u64) -> *const WasmHostTransfer {
    let Ok(integer) = random_signed_int() else {
        let _ = writeln!(io::stderr().lock(), "Zydeco runtime: cannot read random source");
        std::process::exit(1);
    };
    state().transfer_one(continuation, encode_signed(integer))
}
