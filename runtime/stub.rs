mod gc;
mod memory;

use gc::{CheneyHeap, OutOfMemory, RootRange, RootSource, Roots};
use std::{
    cell::{RefCell, UnsafeCell},
    collections::HashMap,
    fs::{File, OpenOptions},
    io::{self, BufRead, BufReader, Read, Write},
};
use zydeco_machine::buffer::{BufferArena, BufferError, BufferHandle};
use zydeco_machine::bytes::ByteBuffer;
#[cfg(not(feature = "compact-environments"))]
use zydeco_machine::frames::Frames as NativeFrames;
#[cfg(feature = "compact-environments")]
use zydeco_machine::frames::fragments::Fragments as NativeFrames;
use zydeco_machine::frames::{Action, FrameError, storage::Growable};
use zydeco_machine::native::{
    AllocationKind, HostArguments, HostTransfer, IMMEDIATE_TAG, Immediate, Word, entry,
};

/// One full-width scalar payload in an opaque managed block.
struct OpaqueScalar;

impl OpaqueScalar {
    fn store(spare: *mut Word, bits: Word) -> Word {
        assert!(!spare.is_null(), "wide scalar operation did not receive a spare box");
        unsafe { spare.write(bits) };
        spare as Word
    }

    fn load(value: Word) -> Word {
        assert_eq!(value & IMMEDIATE_TAG, 0, "expected a boxed scalar value");
        assert_ne!(value, 0, "boxed scalar pointer is null");
        unsafe { (value as *const Word).read() }
    }
}

trait RuntimeInteger: Copy {
    fn decode(value: Word) -> Self;
    fn encode(self, spare: *mut Word) -> Word;
}

macro_rules! immediate_signed_integer {
    ($($type:ty),+ $(,)?) => {
        $(
            impl RuntimeInteger for $type {
                fn decode(value: Word) -> Self {
                    Immediate::decode_signed(value) as Self
                }

                fn encode(self, _spare: *mut Word) -> Word {
                    Immediate::expect_signed(self.into())
                }
            }
        )+
    };
}

macro_rules! immediate_unsigned_integer {
    ($($type:ty),+ $(,)?) => {
        $(
            impl RuntimeInteger for $type {
                fn decode(value: Word) -> Self {
                    Immediate::decode_unsigned(value) as Self
                }

                fn encode(self, _spare: *mut Word) -> Word {
                    Immediate::expect_unsigned(self as Word)
                }
            }
        )+
    };
}

immediate_signed_integer!(i8, i16, i32);
immediate_unsigned_integer!(u8, u16, u32);

impl RuntimeInteger for i64 {
    fn decode(value: Word) -> Self {
        if value & IMMEDIATE_TAG != 0 {
            Immediate::decode_signed(value)
        } else {
            OpaqueScalar::load(value) as Self
        }
    }

    fn encode(self, spare: *mut Word) -> Word {
        Immediate::signed(self).unwrap_or_else(|| OpaqueScalar::store(spare, self as Word))
    }
}

impl RuntimeInteger for u64 {
    fn decode(value: Word) -> Self {
        if value & IMMEDIATE_TAG != 0 {
            Immediate::decode_unsigned(value) as Self
        } else {
            OpaqueScalar::load(value) as Self
        }
    }

    fn encode(self, spare: *mut Word) -> Word {
        usize::try_from(self)
            .ok()
            .and_then(Immediate::unsigned)
            .unwrap_or_else(|| OpaqueScalar::store(spare, self as Word))
    }
}

/// Interior mutability for the runtime's process-wide, single-threaded state.
///
/// Generated Zydeco code and all callbacks run on the entry thread. Keeping the
/// heap buffers here puts both semispaces in the executable's fixed static storage.
/// The frame model owns a separate fixed word allocation and growable metadata.
struct RuntimeCell<T>(UnsafeCell<T>);

impl<T> RuntimeCell<T> {
    const fn new(value: T) -> Self {
        Self(UnsafeCell::new(value))
    }

    fn get(&self) -> *mut T {
        self.0.get()
    }
}

// SAFETY: the native runtime is single-threaded; `main` is the only entry point.
unsafe impl<T> Sync for RuntimeCell<T> {}

/// Publishes a host-control result for immediate consumption by generated code.
struct HostControl;

impl HostControl {
    fn without_arguments(closure: Word) -> Word {
        Self::store(closure, HostArguments::None)
    }

    fn with_one_argument(closure: Word, argument: Word) -> Word {
        Self::store(closure, HostArguments::One(argument))
    }

    fn with_two_arguments(closure: Word, first: Word, second: Word) -> Word {
        Self::store(closure, HostArguments::Two(first, second))
    }

    fn store(closure: Word, arguments: HostArguments<Word>) -> Word {
        let transfer = CONTROL_TRANSFER.get();
        // The assembly bridge consumes this record before another host call can occur.
        unsafe { transfer.write(HostTransfer::for_closure(closure, arguments)) };
        transfer as Word
    }
}

struct HostString;

impl HostString {
    fn leak(string: String) -> Word {
        Box::into_raw(Box::new(string)) as Word
    }

    unsafe fn borrow<'a>(raw: Word) -> &'a str {
        unsafe { &*(raw as *const String) }.as_str()
    }
}

struct HostFloat64;

impl HostFloat64 {
    fn decode(word: Word) -> f64 {
        f64::from_bits(OpaqueScalar::load(word) as u64)
    }
}

struct HostFloat32;

impl HostFloat32 {
    fn decode(word: Word) -> f32 {
        f32::from_bits(Immediate::decode_unsigned(word) as u32)
    }
}

struct HostBytes;

impl HostBytes {
    fn leak(bytes: Vec<u8>) -> Word {
        Self::store(bytes.into())
    }

    fn store(bytes: ByteBuffer) -> Word {
        Box::into_raw(Box::new(bytes)) as Word
    }

    unsafe fn buffer<'a>(raw: Word) -> &'a ByteBuffer {
        unsafe { &*(raw as *const ByteBuffer) }
    }

    unsafe fn borrow<'a>(raw: Word) -> &'a [u8] {
        unsafe { Self::buffer(raw) }.as_slice()
    }
}

/// Borrowed view passed across the C ABI for the duration of one foreign call.
#[derive(Clone, Copy)]
#[repr(C)]
struct ForeignBytes {
    pointer: *const u8,
    length: usize,
}

const STDIN_HANDLE: Word = 0;
const STDOUT_HANDLE: Word = 0;
const STDERR_HANDLE: Word = 1;

struct HostHandle;

impl HostHandle {
    fn encode(handle: Word) -> Word {
        Immediate::expect_unsigned(handle)
    }

    fn decode(value: Word) -> Word {
        Immediate::decode_unsigned(value)
    }
}

struct HostIoRuntime {
    next_reader: Word,
    next_writer: Word,
    readers: HashMap<Word, BufReader<File>>,
    writers: HashMap<Word, File>,
}

impl HostIoRuntime {
    fn new() -> Self {
        Self { next_reader: 1, next_writer: 2, readers: HashMap::new(), writers: HashMap::new() }
    }

    fn open_reader(&mut self, path: &str) -> io::Result<Word> {
        let reader = BufReader::new(File::open(path)?);
        let handle = self.next_reader;
        self.next_reader += 1;
        self.readers.insert(handle, reader);
        Ok(handle)
    }

    fn create_writer(&mut self, path: &str) -> io::Result<Word> {
        self.open_writer(path, false)
    }

    fn append_writer(&mut self, path: &str) -> io::Result<Word> {
        self.open_writer(path, true)
    }

    fn open_writer(&mut self, path: &str, append: bool) -> io::Result<Word> {
        let writer = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(!append)
            .append(append)
            .open(path)?;
        let handle = self.next_writer;
        self.next_writer += 1;
        self.writers.insert(handle, writer);
        Ok(handle)
    }

    fn read<T>(
        &mut self, handle: Word, operation: impl FnOnce(&mut dyn BufRead) -> io::Result<T>,
    ) -> io::Result<T> {
        if handle == STDIN_HANDLE {
            operation(&mut std::io::stdin().lock())
        } else {
            let reader = self.readers.get_mut(&handle).ok_or_else(HostIoError::closed)?;
            operation(reader)
        }
    }

    fn write<T>(
        &mut self, handle: Word, operation: impl FnOnce(&mut dyn Write) -> io::Result<T>,
    ) -> io::Result<T> {
        match handle {
            | STDOUT_HANDLE => operation(&mut std::io::stdout().lock()),
            | STDERR_HANDLE => operation(&mut std::io::stderr().lock()),
            | handle => {
                let writer = self.writers.get_mut(&handle).ok_or_else(HostIoError::closed)?;
                operation(writer)
            }
        }
    }

    fn close_reader(&mut self, handle: Word) -> io::Result<()> {
        if handle == STDIN_HANDLE || self.readers.remove(&handle).is_some() {
            Ok(())
        } else {
            Err(HostIoError::closed())
        }
    }

    fn close_writer(&mut self, handle: Word) -> io::Result<()> {
        if matches!(handle, STDOUT_HANDLE | STDERR_HANDLE) {
            self.write(handle, |writer| writer.flush())
        } else if let Some(mut writer) = self.writers.remove(&handle) {
            writer.flush()
        } else {
            Err(HostIoError::closed())
        }
    }
}

#[derive(Clone, Copy)]
#[repr(i64)]
enum HostIoErrorKind {
    NotFound = 0,
    PermissionDenied = 1,
    AlreadyExists = 2,
    InvalidInput = 3,
    InvalidData = 4,
    BrokenPipe = 5,
    Closed = 6,
    Other = 7,
}

impl HostIoErrorKind {
    fn from_error(error: &io::Error) -> Self {
        match error.kind() {
            | io::ErrorKind::NotFound => Self::NotFound,
            | io::ErrorKind::PermissionDenied => Self::PermissionDenied,
            | io::ErrorKind::AlreadyExists => Self::AlreadyExists,
            | io::ErrorKind::InvalidInput => Self::InvalidInput,
            | io::ErrorKind::InvalidData => Self::InvalidData,
            | io::ErrorKind::BrokenPipe => Self::BrokenPipe,
            | io::ErrorKind::NotConnected => Self::Closed,
            | _ => Self::Other,
        }
    }
}

struct HostIoError;

impl HostIoError {
    fn closed() -> io::Error {
        io::Error::new(io::ErrorKind::NotConnected, "I/O capability is closed")
    }
}

struct IoBranch;

impl IoBranch {
    fn error(continuation: Word, error: io::Error) -> Word {
        HostControl::with_two_arguments(
            continuation,
            Immediate::expect_signed(HostIoErrorKind::from_error(&error) as i64),
            HostString::leak(error.to_string()),
        )
    }

    fn unit(result: io::Result<()>, when_error: Word, when_success: Word) -> Word {
        match result {
            | Ok(()) => HostControl::without_arguments(when_success),
            | Err(error) => Self::error(when_error, error),
        }
    }

    fn value(result: io::Result<Word>, when_error: Word, when_success: Word) -> Word {
        match result {
            | Ok(value) => HostControl::with_one_argument(when_success, value),
            | Err(error) => Self::error(when_error, error),
        }
    }
}

struct Input;

impl Input {
    fn line() -> String {
        let mut line = HOST_IO
            .with(|runtime| {
                runtime.borrow_mut().read(STDIN_HANDLE, |reader| {
                    let mut line = String::new();
                    reader.read_line(&mut line)?;
                    Ok(line)
                })
            })
            .expect("legacy standard-input read failed");
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }
        line
    }

    fn remaining() -> String {
        HOST_IO
            .with(|runtime| {
                runtime.borrow_mut().read(STDIN_HANDLE, |reader| {
                    let mut input = String::new();
                    reader.read_to_string(&mut input)?;
                    Ok(input)
                })
            })
            .expect("legacy standard-input read failed")
    }
}

struct Branch;

impl Branch {
    fn select(condition: bool, when_true: Word, when_false: Word) -> Word {
        HostControl::without_arguments(if condition { when_true } else { when_false })
    }
}

struct OptionalPairBranch;

impl OptionalPairBranch {
    fn select(pair: Option<(String, String)>, when_none: Word, when_some: Word) -> Word {
        match pair {
            | None => HostControl::without_arguments(when_none),
            | Some((first, second)) => HostControl::with_two_arguments(
                when_some,
                HostString::leak(first),
                HostString::leak(second),
            ),
        }
    }

    fn split_at(string: &str, index: i64) -> Option<(String, String)> {
        let index = usize::try_from(index).ok()?;
        let byte = match string.char_indices().nth(index) {
            | Some((byte, _)) => byte,
            | None if index == string.chars().count() => string.len(),
            | None => return None,
        };
        let (first, second) = string.split_at(byte);
        Some((first.to_string(), second.to_string()))
    }
}

#[unsafe(export_name = "\x01zydeco_abort")]
extern "sysv64" fn zydeco_abort() -> ! {
    RuntimeFailure::PatternMatch.exit()
}

#[unsafe(export_name = "\x01zydeco_integer_division_by_zero")]
extern "sysv64" fn zydeco_integer_division_by_zero() -> ! {
    RuntimeFailure::IntegerDivisionByZero.exit()
}

#[unsafe(export_name = "\x01zydeco_integer_remainder_by_zero")]
extern "sysv64" fn zydeco_integer_remainder_by_zero() -> ! {
    RuntimeFailure::IntegerRemainderByZero.exit()
}

enum RuntimeFailure {
    PatternMatch,
    IntegerDivisionByZero,
    IntegerRemainderByZero,
}

impl RuntimeFailure {
    fn exit(self) -> ! {
        let message = match self {
            | Self::PatternMatch => "pattern match failed",
            | Self::IntegerDivisionByZero => "integer division by zero",
            | Self::IntegerRemainderByZero => "integer remainder by zero",
        };
        eprintln!("Zydeco runtime: {message}");
        std::process::exit(1)
    }
}

struct ManagedHeap;

struct GeneratedRoots {
    stack: RootRange,
    action: &'static Action<Word>,
}

impl RootSource for GeneratedRoots {
    fn with_roots<T>(self, trace: impl FnOnce(Roots<'_>) -> T) -> T {
        let mut slots = unsafe { self.action.root_slots(&mut *FRAMES.get()) }
            .unwrap_or_else(|error| out_of_frames(error));
        trace(Roots { stack: self.stack, slots: &mut slots })
    }
}

impl ManagedHeap {
    fn allocate(
        size_words: usize, tag: AllocationKind, stack_start: *mut Word,
        roots: &'static Action<Word>,
    ) -> *mut u8 {
        let stack_end = unsafe { *STACK_END.get() };
        let heap = unsafe { &mut *HEAP.get() };
        let roots = GeneratedRoots {
            stack: RootRange { start: stack_start, end: stack_end },
            action: roots,
        };
        // SAFETY: the compiler supplies live initialized slots; pending frames add
        // their own slot sets. The control cursor precedes temporary host-call padding.
        unsafe { heap.allocate(size_words, tag, roots) }
            .unwrap_or_else(|error| out_of_memory(error))
    }
}

#[unsafe(export_name = "\x01zydeco_alloc_scanned")]
extern "sysv64" fn zydeco_alloc_scanned(
    size_words: usize, stack_start: *mut Word, roots: &'static Action<Word>,
) -> *mut u8 {
    ManagedHeap::allocate(size_words, AllocationKind::Scanned, stack_start, roots)
}

#[unsafe(export_name = "\x01zydeco_alloc_opaque")]
extern "sysv64" fn zydeco_alloc_opaque(
    size_words: usize, stack_start: *mut Word, roots: &'static Action<Word>,
) -> *mut u8 {
    ManagedHeap::allocate(size_words, AllocationKind::Opaque, stack_start, roots)
}

#[unsafe(export_name = "\x01zydeco_frame_step")]
extern "sysv64" fn zydeco_frame_step(action: &'static Action<Word>, token: Word) -> Word {
    // SAFETY: action and trailing indices are static descriptors from matched codegen.
    unsafe { action.apply(&mut *FRAMES.get(), token) }.unwrap_or_else(|error| out_of_frames(error))
}

#[unsafe(export_name = "\x01zydeco_ffi_borrow_bytes")]
extern "sysv64" fn zydeco_ffi_borrow_bytes(bytes: Word) -> ForeignBytes {
    let bytes = unsafe { HostBytes::borrow(bytes) };
    ForeignBytes { pointer: bytes.as_ptr(), length: bytes.len() }
}

macro_rules! foreign_integer {
    ($type:ty, $decode:ident, $encode:ident) => {
        #[unsafe(export_name = concat!("\x01", stringify!($decode)))]
        extern "sysv64" fn $decode(value: Word) -> Word {
            <$type as RuntimeInteger>::decode(value) as Word
        }

        #[unsafe(export_name = concat!("\x01", stringify!($encode)))]
        extern "sysv64" fn $encode(value: Word, spare: *mut Word) -> Word {
            // The C ABI leaves excess register bits unspecified for narrow integer results.
            <$type as RuntimeInteger>::encode(value as $type, spare)
        }
    };
}

foreign_integer!(i8, zydeco_ffi_decode_int8, zydeco_ffi_encode_int8);
foreign_integer!(i16, zydeco_ffi_decode_int16, zydeco_ffi_encode_int16);
foreign_integer!(i32, zydeco_ffi_decode_int32, zydeco_ffi_encode_int32);
foreign_integer!(i64, zydeco_ffi_decode_int64, zydeco_ffi_encode_int64);
foreign_integer!(u8, zydeco_ffi_decode_uint8, zydeco_ffi_encode_uint8);
foreign_integer!(u16, zydeco_ffi_decode_uint16, zydeco_ffi_encode_uint16);
foreign_integer!(u32, zydeco_ffi_decode_uint32, zydeco_ffi_encode_uint32);
foreign_integer!(u64, zydeco_ffi_decode_uint64, zydeco_ffi_encode_uint64);

#[unsafe(export_name = "\x01zydeco_exit")]
extern "sysv64" fn zydeco_exit(code: Word) -> ! {
    std::process::exit(<i64 as RuntimeInteger>::decode(code) as i32);
}

/* ---------------------------------- Pure ---------------------------------- */

#[unsafe(export_name = "\x01zydeco_str_scalar_length")]
extern "sysv64" fn zydeco_str_scalar_length(string: Word) -> Word {
    Immediate::expect_signed(unsafe { HostString::borrow(string) }.chars().count() as i64)
}

#[unsafe(export_name = "\x01zydeco_str_byte_length")]
extern "sysv64" fn zydeco_str_byte_length(string: Word) -> Word {
    Immediate::expect_signed(unsafe { HostString::borrow(string) }.len() as i64)
}

#[unsafe(export_name = "\x01zydeco_string_literal")]
extern "sysv64" fn zydeco_string_literal(bytes: *const u8, length: usize) -> Word {
    let bytes = unsafe { std::slice::from_raw_parts(bytes, length) };
    let string = std::str::from_utf8(bytes).expect("invalid UTF-8 string literal");
    HostString::leak(string.to_string())
}

#[unsafe(export_name = "\x01zydeco_str_append")]
extern "sysv64" fn zydeco_str_append(first: Word, second: Word) -> Word {
    let first = unsafe { HostString::borrow(first) };
    let second = unsafe { HostString::borrow(second) };
    HostString::leak([first, second].concat())
}

#[unsafe(export_name = "\x01zydeco_str_get_branch")]
extern "sysv64" fn zydeco_str_get_branch(
    string: Word, index: Word, when_none: Word, when_some: Word,
) -> Word {
    let index = <i64 as RuntimeInteger>::decode(index);
    let character = usize::try_from(index)
        .ok()
        .and_then(|index| unsafe { HostString::borrow(string) }.chars().nth(index));
    match character {
        | None => HostControl::without_arguments(when_none),
        | Some(character) => {
            HostControl::with_one_argument(when_some, Immediate::expect_unsigned(character as Word))
        }
    }
}

// Scalar representation leaves preserve every payload bit, including floating
// NaN payloads. Product composition, padding, and field alignment are library code.
macro_rules! scalar_bytes {
    ($type:ty, $to:ident => $to_symbol:literal, $from:ident => $from_symbol:literal,
     $decode:expr, $encode:expr) => {
        #[unsafe(export_name = $to_symbol)]
        extern "sysv64" fn $to(value: Word) -> Word {
            let value: $type = ($decode)(value);
            HostBytes::leak(value.to_le_bytes().to_vec())
        }

        #[unsafe(export_name = $from_symbol)]
        extern "sysv64" fn $from(
            bytes: Word, when_none: Word, when_some: Word, spare: *mut Word,
        ) -> Word {
            match unsafe { HostBytes::borrow(bytes) }.try_into() {
                | Err(_) => HostControl::without_arguments(when_none),
                | Ok(bytes) => {
                    let value = <$type>::from_le_bytes(bytes);
                    HostControl::with_one_argument(when_some, ($encode)(value, spare))
                }
            }
        }
    };
}
scalar_bytes!(
    i8,
    zydeco_int8_to_le_bytes => "\x01zydeco_int8_to_le_bytes",
    zydeco_int8_from_le_bytes_branch => "\x01zydeco_int8_from_le_bytes_branch",
    <i8 as RuntimeInteger>::decode, <i8 as RuntimeInteger>::encode
);
scalar_bytes!(
    i16,
    zydeco_int16_to_le_bytes => "\x01zydeco_int16_to_le_bytes",
    zydeco_int16_from_le_bytes_branch => "\x01zydeco_int16_from_le_bytes_branch",
    <i16 as RuntimeInteger>::decode, <i16 as RuntimeInteger>::encode
);
scalar_bytes!(
    i32,
    zydeco_int32_to_le_bytes => "\x01zydeco_int32_to_le_bytes",
    zydeco_int32_from_le_bytes_branch => "\x01zydeco_int32_from_le_bytes_branch",
    <i32 as RuntimeInteger>::decode, <i32 as RuntimeInteger>::encode
);
scalar_bytes!(
    i64,
    zydeco_int64_to_le_bytes => "\x01zydeco_int64_to_le_bytes",
    zydeco_int64_from_le_bytes_branch => "\x01zydeco_int64_from_le_bytes_branch",
    <i64 as RuntimeInteger>::decode, <i64 as RuntimeInteger>::encode
);
scalar_bytes!(
    u8,
    zydeco_uint8_to_le_bytes => "\x01zydeco_uint8_to_le_bytes",
    zydeco_uint8_from_le_bytes_branch => "\x01zydeco_uint8_from_le_bytes_branch",
    <u8 as RuntimeInteger>::decode, <u8 as RuntimeInteger>::encode
);
scalar_bytes!(
    u16,
    zydeco_uint16_to_le_bytes => "\x01zydeco_uint16_to_le_bytes",
    zydeco_uint16_from_le_bytes_branch => "\x01zydeco_uint16_from_le_bytes_branch",
    <u16 as RuntimeInteger>::decode, <u16 as RuntimeInteger>::encode
);
scalar_bytes!(
    u32,
    zydeco_uint32_to_le_bytes => "\x01zydeco_uint32_to_le_bytes",
    zydeco_uint32_from_le_bytes_branch => "\x01zydeco_uint32_from_le_bytes_branch",
    <u32 as RuntimeInteger>::decode, <u32 as RuntimeInteger>::encode
);
scalar_bytes!(
    u64,
    zydeco_uint64_to_le_bytes => "\x01zydeco_uint64_to_le_bytes",
    zydeco_uint64_from_le_bytes_branch => "\x01zydeco_uint64_from_le_bytes_branch",
    <u64 as RuntimeInteger>::decode, <u64 as RuntimeInteger>::encode
);
scalar_bytes!(
    u32,
    zydeco_float32_to_le_bytes => "\x01zydeco_float32_to_le_bytes",
    zydeco_float32_from_le_bytes_branch => "\x01zydeco_float32_from_le_bytes_branch",
    |word| Immediate::decode_unsigned(word) as u32,
    |bits: u32, _spare| Immediate::expect_unsigned(bits as Word)
);
scalar_bytes!(
    u64,
    zydeco_float64_to_le_bytes => "\x01zydeco_float64_to_le_bytes",
    zydeco_float64_from_le_bytes_branch => "\x01zydeco_float64_from_le_bytes_branch",
    |word| OpaqueScalar::load(word) as u64,
    |bits: u64, spare| OpaqueScalar::store(spare, bits as Word)
);

#[unsafe(export_name = "\x01zydeco_bytes_aligned_branch")]
extern "sysv64" fn zydeco_bytes_aligned_branch(
    bytes: Word, alignment: Word, when_none: Word, when_some: Word,
) -> Word {
    let alignment = <i64 as RuntimeInteger>::decode(alignment);
    let aligned = usize::try_from(alignment)
        .ok()
        .and_then(|alignment| unsafe { HostBytes::buffer(bytes) }.aligned(alignment));
    match aligned {
        | None => HostControl::without_arguments(when_none),
        | Some(bytes) => HostControl::with_one_argument(when_some, HostBytes::store(bytes)),
    }
}

thread_local! {
    static HOST_BUFFERS: RefCell<BufferArena> = RefCell::new(BufferArena::default());
}

struct BufferBranch;

impl BufferBranch {
    fn finish(result: Result<Option<Word>, BufferError>, error: Word, success: Word) -> Word {
        match result {
            | Ok(None) => HostControl::without_arguments(success),
            | Ok(Some(value)) => HostControl::with_one_argument(success, value),
            | Err(code) => {
                HostControl::with_one_argument(error, Immediate::expect_signed(code as i64))
            }
        }
    }

    fn handle(word: Word) -> BufferHandle {
        BufferHandle::with_raw(HostHandle::decode(word))
    }
}

#[unsafe(export_name = "\x01zydeco_buffer_allocate")]
extern "sysv64" fn zydeco_buffer_allocate(
    size: Word, alignment: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .allocate(
                <i64 as RuntimeInteger>::decode(size),
                <i64 as RuntimeInteger>::decode(alignment),
            )
            .map(|handle| Some(HostHandle::encode(handle.raw())))
    });
    BufferBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_buffer_write")]
extern "sysv64" fn zydeco_buffer_write(
    buffer: Word, offset: Word, bytes: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .write(BufferBranch::handle(buffer), <i64 as RuntimeInteger>::decode(offset), unsafe {
                HostBytes::borrow(bytes)
            })
            .map(|()| None)
    });
    BufferBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_buffer_read")]
extern "sysv64" fn zydeco_buffer_read(
    buffer: Word, offset: Word, length: Word, error: Word, success: Word,
) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow()
            .read(
                BufferBranch::handle(buffer),
                <i64 as RuntimeInteger>::decode(offset),
                <i64 as RuntimeInteger>::decode(length),
            )
            .map(|bytes| Some(HostBytes::store(bytes)))
    });
    BufferBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_buffer_freeze")]
extern "sysv64" fn zydeco_buffer_freeze(buffer: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS.with(|arena| {
        arena
            .borrow_mut()
            .freeze(BufferBranch::handle(buffer))
            .map(|bytes| Some(HostBytes::store(bytes)))
    });
    BufferBranch::finish(result, error, success)
}

#[unsafe(export_name = "\x01zydeco_buffer_close")]
extern "sysv64" fn zydeco_buffer_close(buffer: Word, error: Word, success: Word) -> Word {
    let result = HOST_BUFFERS
        .with(|arena| arena.borrow_mut().close(BufferBranch::handle(buffer)).map(|()| None));
    BufferBranch::finish(result, error, success)
}

macro_rules! integer_runtime {
    (
        $type:ty,
        $eq:ident => $eq_symbol:literal,
        $lt:ident => $lt_symbol:literal,
        $gt:ident => $gt_symbol:literal,
        $to_string:ident => $to_string_symbol:literal
    ) => {
        #[unsafe(export_name = $eq_symbol)]
        extern "sysv64" fn $eq(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select(
                <$type as RuntimeInteger>::decode(first)
                    == <$type as RuntimeInteger>::decode(second),
                when_true,
                when_false,
            )
        }

        #[unsafe(export_name = $lt_symbol)]
        extern "sysv64" fn $lt(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select(
                <$type as RuntimeInteger>::decode(first)
                    < <$type as RuntimeInteger>::decode(second),
                when_true,
                when_false,
            )
        }

        #[unsafe(export_name = $gt_symbol)]
        extern "sysv64" fn $gt(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select(
                <$type as RuntimeInteger>::decode(first)
                    > <$type as RuntimeInteger>::decode(second),
                when_true,
                when_false,
            )
        }

        #[unsafe(export_name = $to_string_symbol)]
        extern "sysv64" fn $to_string(value: Word) -> Word {
            HostString::leak(<$type as RuntimeInteger>::decode(value).to_string())
        }
    };
}

integer_runtime!(
    i8,
    zydeco_int8_eq_branch => "\x01zydeco_int8_eq_branch",
    zydeco_int8_lt_branch => "\x01zydeco_int8_lt_branch",
    zydeco_int8_gt_branch => "\x01zydeco_int8_gt_branch",
    zydeco_int8_to_string => "\x01zydeco_int8_to_string"
);
integer_runtime!(
    i16,
    zydeco_int16_eq_branch => "\x01zydeco_int16_eq_branch",
    zydeco_int16_lt_branch => "\x01zydeco_int16_lt_branch",
    zydeco_int16_gt_branch => "\x01zydeco_int16_gt_branch",
    zydeco_int16_to_string => "\x01zydeco_int16_to_string"
);
integer_runtime!(
    i32,
    zydeco_int32_eq_branch => "\x01zydeco_int32_eq_branch",
    zydeco_int32_lt_branch => "\x01zydeco_int32_lt_branch",
    zydeco_int32_gt_branch => "\x01zydeco_int32_gt_branch",
    zydeco_int32_to_string => "\x01zydeco_int32_to_string"
);
integer_runtime!(
    i64,
    zydeco_int64_eq_branch => "\x01zydeco_int64_eq_branch",
    zydeco_int64_lt_branch => "\x01zydeco_int64_lt_branch",
    zydeco_int64_gt_branch => "\x01zydeco_int64_gt_branch",
    zydeco_int64_to_string => "\x01zydeco_int64_to_string"
);
integer_runtime!(
    u8,
    zydeco_uint8_eq_branch => "\x01zydeco_uint8_eq_branch",
    zydeco_uint8_lt_branch => "\x01zydeco_uint8_lt_branch",
    zydeco_uint8_gt_branch => "\x01zydeco_uint8_gt_branch",
    zydeco_uint8_to_string => "\x01zydeco_uint8_to_string"
);
integer_runtime!(
    u16,
    zydeco_uint16_eq_branch => "\x01zydeco_uint16_eq_branch",
    zydeco_uint16_lt_branch => "\x01zydeco_uint16_lt_branch",
    zydeco_uint16_gt_branch => "\x01zydeco_uint16_gt_branch",
    zydeco_uint16_to_string => "\x01zydeco_uint16_to_string"
);
integer_runtime!(
    u32,
    zydeco_uint32_eq_branch => "\x01zydeco_uint32_eq_branch",
    zydeco_uint32_lt_branch => "\x01zydeco_uint32_lt_branch",
    zydeco_uint32_gt_branch => "\x01zydeco_uint32_gt_branch",
    zydeco_uint32_to_string => "\x01zydeco_uint32_to_string"
);
integer_runtime!(
    u64,
    zydeco_uint64_eq_branch => "\x01zydeco_uint64_eq_branch",
    zydeco_uint64_lt_branch => "\x01zydeco_uint64_lt_branch",
    zydeco_uint64_gt_branch => "\x01zydeco_uint64_gt_branch",
    zydeco_uint64_to_string => "\x01zydeco_uint64_to_string"
);

macro_rules! float_runtime {
    (
        $type:ty, $codec:ident,
        $eq:ident => $eq_symbol:literal,
        $lt:ident => $lt_symbol:literal,
        $gt:ident => $gt_symbol:literal,
        $to_string:ident => $to_string_symbol:literal
    ) => {
        #[unsafe(export_name = $eq_symbol)]
        extern "sysv64" fn $eq(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select($codec::decode(first) == $codec::decode(second), when_true, when_false)
        }

        #[unsafe(export_name = $lt_symbol)]
        extern "sysv64" fn $lt(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select($codec::decode(first) < $codec::decode(second), when_true, when_false)
        }

        #[unsafe(export_name = $gt_symbol)]
        extern "sysv64" fn $gt(
            first: Word, second: Word, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select($codec::decode(first) > $codec::decode(second), when_true, when_false)
        }

        #[unsafe(export_name = $to_string_symbol)]
        extern "sysv64" fn $to_string(value: Word) -> Word {
            let value: $type = $codec::decode(value);
            HostString::leak(value.to_string())
        }
    };
}

float_runtime!(
    f32, HostFloat32,
    zydeco_float32_eq_branch => "\x01zydeco_float32_eq_branch",
    zydeco_float32_lt_branch => "\x01zydeco_float32_lt_branch",
    zydeco_float32_gt_branch => "\x01zydeco_float32_gt_branch",
    zydeco_float32_to_string => "\x01zydeco_float32_to_string"
);
float_runtime!(
    f64, HostFloat64,
    zydeco_float64_eq_branch => "\x01zydeco_float64_eq_branch",
    zydeco_float64_lt_branch => "\x01zydeco_float64_lt_branch",
    zydeco_float64_gt_branch => "\x01zydeco_float64_gt_branch",
    zydeco_float64_to_string => "\x01zydeco_float64_to_string"
);

#[unsafe(export_name = "\x01zydeco_char_to_str")]
extern "sysv64" fn zydeco_char_to_str(character: Word) -> Word {
    let character =
        char::from_u32(Immediate::decode_unsigned(character) as u32).expect("invalid character");
    HostString::leak(character.to_string())
}

#[unsafe(export_name = "\x01zydeco_char_codepoint")]
extern "sysv64" fn zydeco_char_codepoint(character: Word) -> Word {
    Immediate::expect_signed(Immediate::decode_unsigned(character) as i64)
}

#[unsafe(export_name = "\x01zydeco_char_from_codepoint_branch")]
extern "sysv64" fn zydeco_char_from_codepoint_branch(
    codepoint: Word, when_none: Word, when_some: Word,
) -> Word {
    let codepoint = <i64 as RuntimeInteger>::decode(codepoint);
    match u32::try_from(codepoint).ok().and_then(char::from_u32) {
        | None => HostControl::without_arguments(when_none),
        | Some(character) => {
            HostControl::with_one_argument(when_some, Immediate::expect_unsigned(character as Word))
        }
    }
}

#[unsafe(export_name = "\x01zydeco_str_parse_int_branch")]
extern "sysv64" fn zydeco_str_parse_int_branch(
    string: Word, when_none: Word, when_some: Word, spare: *mut Word,
) -> Word {
    match unsafe { HostString::borrow(string) }.parse::<i64>() {
        | Err(_) => HostControl::without_arguments(when_none),
        | Ok(integer) => HostControl::with_one_argument(when_some, integer.encode(spare)),
    }
}

#[unsafe(export_name = "\x01zydeco_bytes_empty")]
extern "sysv64" fn zydeco_bytes_empty() -> Word {
    HostBytes::leak(Vec::new())
}

#[unsafe(export_name = "\x01zydeco_bytes_length")]
extern "sysv64" fn zydeco_bytes_length(bytes: Word) -> Word {
    Immediate::expect_signed(unsafe { HostBytes::borrow(bytes) }.len() as i64)
}

#[unsafe(export_name = "\x01zydeco_bytes_append")]
extern "sysv64" fn zydeco_bytes_append(first: Word, second: Word) -> Word {
    HostBytes::leak(
        [unsafe { HostBytes::borrow(first) }, unsafe { HostBytes::borrow(second) }].concat(),
    )
}

#[unsafe(export_name = "\x01zydeco_bytes_from_str")]
extern "sysv64" fn zydeco_bytes_from_str(string: Word) -> Word {
    HostBytes::leak(unsafe { HostString::borrow(string) }.as_bytes().to_vec())
}

#[unsafe(export_name = "\x01zydeco_bytes_to_str_branch")]
extern "sysv64" fn zydeco_bytes_to_str_branch(
    bytes: Word, when_invalid: Word, when_valid: Word,
) -> Word {
    match std::str::from_utf8(unsafe { HostBytes::borrow(bytes) }) {
        | Err(_) => HostControl::without_arguments(when_invalid),
        | Ok(string) => {
            HostControl::with_one_argument(when_valid, HostString::leak(string.to_string()))
        }
    }
}

#[unsafe(export_name = "\x01zydeco_bytes_get_branch")]
extern "sysv64" fn zydeco_bytes_get_branch(
    bytes: Word, index: Word, when_none: Word, when_some: Word,
) -> Word {
    let index = <i64 as RuntimeInteger>::decode(index);
    let octet = usize::try_from(index)
        .ok()
        .and_then(|index| unsafe { HostBytes::borrow(bytes) }.get(index).copied());
    match octet {
        | None => HostControl::without_arguments(when_none),
        | Some(octet) => HostControl::with_one_argument(
            when_some,
            <u8 as RuntimeInteger>::encode(octet, std::ptr::null_mut()),
        ),
    }
}

#[unsafe(export_name = "\x01zydeco_bytes_slice_branch")]
extern "sysv64" fn zydeco_bytes_slice_branch(
    bytes: Word, start: Word, length: Word, when_none: Word, when_some: Word,
) -> Word {
    let start = <i64 as RuntimeInteger>::decode(start);
    let length = <i64 as RuntimeInteger>::decode(length);
    let window = usize::try_from(start).ok().and_then(|start| {
        usize::try_from(length).ok().and_then(|length| {
            let end = start.checked_add(length)?;
            let source = unsafe { HostBytes::borrow(bytes) };
            (end <= source.len()).then(|| source[start..end].to_vec())
        })
    });
    match window {
        | None => HostControl::without_arguments(when_none),
        | Some(window) => HostControl::with_one_argument(when_some, HostBytes::leak(window)),
    }
}

/// Build a one-octet buffer; every `UInt8` word is a valid octet, so no branch is needed.
#[unsafe(export_name = "\x01zydeco_bytes_singleton")]
extern "sysv64" fn zydeco_bytes_singleton(octet: Word) -> Word {
    HostBytes::leak(vec![<u8 as RuntimeInteger>::decode(octet)])
}

#[unsafe(export_name = "\x01zydeco_bytes_eq_branch")]
extern "sysv64" fn zydeco_bytes_eq_branch(
    first: Word, second: Word, when_true: Word, when_false: Word,
) -> Word {
    let condition = unsafe { HostBytes::borrow(first) } == unsafe { HostBytes::borrow(second) };
    Branch::select(condition, when_true, when_false)
}

#[unsafe(export_name = "\x01zydeco_bytes_lt_branch")]
extern "sysv64" fn zydeco_bytes_lt_branch(
    first: Word, second: Word, when_true: Word, when_false: Word,
) -> Word {
    let condition = unsafe { HostBytes::borrow(first) } < unsafe { HostBytes::borrow(second) };
    Branch::select(condition, when_true, when_false)
}

/* -------------------------------- Branches -------------------------------- */

#[unsafe(export_name = "\x01zydeco_str_eq_branch")]
extern "sysv64" fn zydeco_str_eq_branch(
    first: Word, second: Word, when_true: Word, when_false: Word,
) -> Word {
    let condition = unsafe { HostString::borrow(first) == HostString::borrow(second) };
    Branch::select(condition, when_true, when_false)
}

#[unsafe(export_name = "\x01zydeco_str_split_once_branch")]
extern "sysv64" fn zydeco_str_split_once_branch(
    string: Word, separator: Word, when_none: Word, when_some: Word,
) -> Word {
    let string = unsafe { HostString::borrow(string) };
    let separator =
        char::from_u32(Immediate::decode_unsigned(separator) as u32).expect("invalid separator");
    let pair =
        string.split_once(separator).map(|(first, second)| (first.to_string(), second.to_string()));
    OptionalPairBranch::select(pair, when_none, when_some)
}

#[unsafe(export_name = "\x01zydeco_str_split_at_branch")]
extern "sysv64" fn zydeco_str_split_at_branch(
    string: Word, index: Word, when_none: Word, when_some: Word,
) -> Word {
    let index = <i64 as RuntimeInteger>::decode(index);
    let pair = OptionalPairBranch::split_at(unsafe { HostString::borrow(string) }, index);
    OptionalPairBranch::select(pair, when_none, when_some)
}

/* ----------------------------------- IO ----------------------------------- */

#[unsafe(export_name = "\x01zydeco_stdin")]
extern "sysv64" fn zydeco_stdin() -> Word {
    HostHandle::encode(STDIN_HANDLE)
}

#[unsafe(export_name = "\x01zydeco_stdout")]
extern "sysv64" fn zydeco_stdout() -> Word {
    HostHandle::encode(STDOUT_HANDLE)
}

#[unsafe(export_name = "\x01zydeco_stderr")]
extern "sysv64" fn zydeco_stderr() -> Word {
    HostHandle::encode(STDERR_HANDLE)
}

#[unsafe(export_name = "\x01zydeco_io_read")]
extern "sysv64" fn zydeco_io_read(
    reader: Word, count: Word, when_error: Word, when_success: Word,
) -> Word {
    let reader = HostHandle::decode(reader);
    let count = <i64 as RuntimeInteger>::decode(count);
    let count = match u64::try_from(count) {
        | Ok(count) => count,
        | Err(_) => {
            return IoBranch::error(
                when_error,
                io::Error::new(io::ErrorKind::InvalidInput, "byte count cannot be negative"),
            );
        }
    };
    let result = HOST_IO.with(|runtime| {
        runtime.borrow_mut().read(reader, |reader| {
            let mut bytes = Vec::new();
            reader.take(count).read_to_end(&mut bytes)?;
            Ok(HostBytes::leak(bytes))
        })
    });
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_read_line")]
extern "sysv64" fn zydeco_io_read_line(
    reader: Word, when_error: Word, when_eof: Word, when_line: Word,
) -> Word {
    let reader = HostHandle::decode(reader);
    let result = HOST_IO.with(|runtime| {
        runtime.borrow_mut().read(reader, |reader| {
            let mut bytes = Vec::new();
            let read = reader.read_until(b'\n', &mut bytes)?;
            if bytes.last() == Some(&b'\n') {
                bytes.pop();
                if bytes.last() == Some(&b'\r') {
                    bytes.pop();
                }
            }
            Ok((read, bytes))
        })
    });
    match result {
        | Ok((0, _)) => HostControl::without_arguments(when_eof),
        | Ok((_, bytes)) => HostControl::with_one_argument(when_line, HostBytes::leak(bytes)),
        | Err(error) => IoBranch::error(when_error, error),
    }
}

#[unsafe(export_name = "\x01zydeco_io_read_all")]
extern "sysv64" fn zydeco_io_read_all(reader: Word, when_error: Word, when_success: Word) -> Word {
    let reader = HostHandle::decode(reader);
    let result = HOST_IO.with(|runtime| {
        runtime.borrow_mut().read(reader, |reader| {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes)?;
            Ok(HostBytes::leak(bytes))
        })
    });
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_write_all")]
extern "sysv64" fn zydeco_io_write_all(
    writer: Word, bytes: Word, when_error: Word, when_success: Word,
) -> Word {
    let writer = HostHandle::decode(writer);
    let result = HOST_IO.with(|runtime| {
        runtime
            .borrow_mut()
            .write(writer, |writer| writer.write_all(unsafe { HostBytes::borrow(bytes) }))
    });
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_flush")]
extern "sysv64" fn zydeco_io_flush(writer: Word, when_error: Word, when_success: Word) -> Word {
    let writer = HostHandle::decode(writer);
    let result =
        HOST_IO.with(|runtime| runtime.borrow_mut().write(writer, |writer| writer.flush()));
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_close_reader")]
extern "sysv64" fn zydeco_io_close_reader(
    reader: Word, when_error: Word, when_success: Word,
) -> Word {
    let reader = HostHandle::decode(reader);
    let result = HOST_IO.with(|runtime| runtime.borrow_mut().close_reader(reader));
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_close_writer")]
extern "sysv64" fn zydeco_io_close_writer(
    writer: Word, when_error: Word, when_success: Word,
) -> Word {
    let writer = HostHandle::decode(writer);
    let result = HOST_IO.with(|runtime| runtime.borrow_mut().close_writer(writer));
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_fs_open_reader")]
extern "sysv64" fn zydeco_fs_open_reader(path: Word, when_error: Word, when_success: Word) -> Word {
    let result = HOST_IO
        .with(|runtime| runtime.borrow_mut().open_reader(unsafe { HostString::borrow(path) }))
        .map(HostHandle::encode);
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_fs_create_writer")]
extern "sysv64" fn zydeco_fs_create_writer(
    path: Word, when_error: Word, when_success: Word,
) -> Word {
    let result = HOST_IO
        .with(|runtime| runtime.borrow_mut().create_writer(unsafe { HostString::borrow(path) }))
        .map(HostHandle::encode);
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_fs_append_writer")]
extern "sysv64" fn zydeco_fs_append_writer(
    path: Word, when_error: Word, when_success: Word,
) -> Word {
    let result = HOST_IO
        .with(|runtime| runtime.borrow_mut().append_writer(unsafe { HostString::borrow(path) }))
        .map(HostHandle::encode);
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_read_line")]
extern "sysv64" fn zydeco_read_line(continuation: Word) -> Word {
    let line = Input::line();
    HostControl::with_one_argument(continuation, HostString::leak(line))
}

#[unsafe(export_name = "\x01zydeco_read_line_as_int_branch")]
extern "sysv64" fn zydeco_read_line_as_int_branch(
    when_invalid: Word, when_valid: Word, spare: *mut Word,
) -> Word {
    match Input::line().parse::<i64>() {
        | Ok(integer) => HostControl::with_one_argument(when_valid, integer.encode(spare)),
        | Err(_) => HostControl::without_arguments(when_invalid),
    }
}

#[unsafe(export_name = "\x01zydeco_read_till_eof")]
extern "sysv64" fn zydeco_read_till_eof(continuation: Word) -> Word {
    HostControl::with_one_argument(continuation, HostString::leak(Input::remaining()))
}

#[unsafe(export_name = "\x01zydeco_write_str")]
extern "sysv64" fn zydeco_write_str(string: Word, continuation: Word) -> Word {
    HOST_IO
        .with(|runtime| {
            runtime.borrow_mut().write(STDOUT_HANDLE, |writer| {
                writer.write_all(unsafe { HostString::borrow(string) }.as_bytes())?;
                writer.flush()
            })
        })
        .expect("legacy standard-output write failed");
    HostControl::without_arguments(continuation)
}

#[unsafe(export_name = "\x01zydeco_write_int")]
extern "sysv64" fn zydeco_write_int(integer: Word, continuation: Word) -> Word {
    let integer = <i64 as RuntimeInteger>::decode(integer);
    HOST_IO
        .with(|runtime| {
            runtime.borrow_mut().write(STDOUT_HANDLE, |writer| {
                write!(writer, "{integer}")?;
                writer.flush()
            })
        })
        .expect("legacy standard-output write failed");
    HostControl::without_arguments(continuation)
}

#[unsafe(export_name = "\x01zydeco_write_line")]
extern "sysv64" fn zydeco_write_line(line: Word, continuation: Word) -> Word {
    HOST_IO
        .with(|runtime| {
            runtime.borrow_mut().write(STDOUT_HANDLE, |writer| {
                writeln!(writer, "{}", unsafe { HostString::borrow(line) })?;
                writer.flush()
            })
        })
        .expect("legacy standard-output write failed");
    HostControl::without_arguments(continuation)
}

#[unsafe(export_name = "\x01zydeco_arg_at")]
extern "sysv64" fn zydeco_arg_at(index: Word, when_none: Word, when_some: Word) -> Word {
    let index = usize::try_from(<i64 as RuntimeInteger>::decode(index)).ok();
    let argument =
        HOST_ARGUMENTS.with(|arguments| index.and_then(|index| arguments.get(index)).copied());
    match argument {
        | Some(argument) => HostControl::with_one_argument(when_some, argument),
        | None => HostControl::without_arguments(when_none),
    }
}

#[unsafe(export_name = "\x01zydeco_random_int")]
extern "sysv64" fn zydeco_random_int(continuation: Word, spare: *mut Word) -> Word {
    use rand::RngExt;
    let integer = rand::rng().random_range(i64::MIN..=i64::MAX);
    HostControl::with_one_argument(continuation, integer.encode(spare))
}

/* ---------------------------------- Entry --------------------------------- */

const HEAP_SPACE_BYTES: usize = 1024 * 1024;
const HEAP_INDEX_REGIONS: usize = HEAP_SPACE_BYTES.div_ceil(gc::INDEX_REGION_BYTES);
fn out_of_memory(error: OutOfMemory) -> ! {
    let _ = writeln!(
        std::io::stderr().lock(),
        "Zydeco runtime: out of memory (requested {} words; {} of {} bytes remain live)",
        error.requested_words,
        error.live_bytes,
        error.capacity_bytes,
    );
    std::process::exit(1)
}

fn out_of_frames(error: FrameError) -> ! {
    let _ = writeln!(std::io::stderr().lock(), "Zydeco runtime: {error}");
    std::process::exit(1)
}

static HEAP: RuntimeCell<CheneyHeap<HEAP_SPACE_BYTES, HEAP_INDEX_REGIONS>> =
    RuntimeCell::new(CheneyHeap::new());
static FRAMES: RuntimeCell<NativeFrames<Growable>> = RuntimeCell::new(NativeFrames::EMPTY);
static STACK_END: RuntimeCell<*mut Word> = RuntimeCell::new(std::ptr::null_mut());
static CONTROL_TRANSFER: RuntimeCell<HostTransfer<Word>> =
    RuntimeCell::new(HostTransfer { resume: 0, closure: 0, first: 0, second: 0 });

thread_local! {
    // Host strings contain no managed references. Lookup returns the same stable snapshot.
    static HOST_ARGUMENTS: Vec<Word> = std::env::args().skip(1).map(HostString::leak).collect();
    static HOST_IO: RefCell<HostIoRuntime> = RefCell::new(HostIoRuntime::new());
}

fn main() {
    let stack_anchor: Word = 0;
    unsafe {
        *STACK_END.get() = std::ptr::addr_of!(stack_anchor).add(1).cast_mut();
        entry();
    }
}
