mod gc;
mod memory;

use gc::{CheneyHeap, OutOfMemory, RootRange, RootSource, Roots};
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fs::{File, OpenOptions},
    io::{self, BufRead, BufReader, Read, Write},
};
use zydeco_machine::buffer::{BufferArena, BufferHandle};
#[cfg(not(feature = "compact-environments"))]
use zydeco_machine::frames::Frames as NativeFrames;
#[cfg(feature = "compact-environments")]
use zydeco_machine::frames::fragments::Fragments as NativeFrames;
use zydeco_machine::frames::{Action, FrameError, storage::Growable};
use zydeco_machine::native::{
    AllocationKind, HostArguments, HostTransfer, IMMEDIATE_TAG, Immediate, Word,
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
        let transfer = RuntimeInstance::transfer();
        // The assembly bridge consumes this record before another host call can occur.
        unsafe { transfer.write(HostTransfer::for_closure(closure, arguments)) };
        transfer as Word
    }
}

struct HostString;

impl HostString {
    fn own(string: String) -> Word {
        let owned = Box::new(string);
        let word = (&*owned as *const String) as Word;
        // Boxes keep their addresses when the ownership vector grows.
        unsafe { (*RuntimeInstance::current()).strings.push(owned) };
        word
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

    fn encode(value: f64, spare: *mut Word) -> Word {
        OpaqueScalar::store(spare, value.to_bits() as Word)
    }
}

struct HostFloat32;

impl HostFloat32 {
    fn decode(word: Word) -> f32 {
        f32::from_bits(Immediate::decode_unsigned(word) as u32)
    }

    fn encode(value: f32, _spare: *mut Word) -> Word {
        Immediate::expect_unsigned(value.to_bits() as Word)
    }
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
    fn memory(error: zydeco_machine::memory::MemoryError) -> io::Error {
        let kind = match error {
            | zydeco_machine::memory::MemoryError::Closed => io::ErrorKind::NotConnected,
            | zydeco_machine::memory::MemoryError::Permission => io::ErrorKind::PermissionDenied,
            | zydeco_machine::memory::MemoryError::Uninitialized => io::ErrorKind::InvalidData,
            | zydeco_machine::memory::MemoryError::AllocationFailed => io::ErrorKind::OutOfMemory,
            | _ => io::ErrorKind::InvalidInput,
        };
        io::Error::new(kind, error.message())
    }

    fn closed() -> io::Error {
        io::Error::new(io::ErrorKind::NotConnected, "I/O capability is closed")
    }
}

struct IoBranch;

impl IoBranch {
    fn memory(bytes: &[u8]) -> io::Result<Word> {
        RuntimeInstance::with_buffers(|arena| arena.borrow_mut().import_memory(bytes))
            .map(|access| HostHandle::encode(access.raw()))
            .map_err(HostIoError::memory)
    }

    fn error(continuation: Word, error: io::Error) -> Word {
        HostControl::with_two_arguments(
            continuation,
            Immediate::expect_signed(HostIoErrorKind::from_error(&error) as i64),
            HostString::own(error.to_string()),
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
        let mut line = RuntimeInstance::with_io(|runtime| {
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
        RuntimeInstance::with_io(|runtime| {
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
                HostString::own(first),
                HostString::own(second),
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
    ForeignMemory(zydeco_machine::memory::MemoryError),
    IntegerDivisionByZero,
    IntegerRemainderByZero,
}

impl RuntimeFailure {
    fn exit(self) -> ! {
        let message = match self {
            | Self::PatternMatch => "pattern match failed",
            | Self::ForeignMemory(error) => error.message(),
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
        let mut slots = unsafe { self.action.root_slots(&mut *RuntimeInstance::frames()) }
            .unwrap_or_else(|error| out_of_frames(error));
        trace(Roots { stack: self.stack, slots: &mut slots })
    }
}

impl ManagedHeap {
    fn allocate(
        size_words: usize, tag: AllocationKind, stack_start: *mut Word,
        roots: &'static Action<Word>,
    ) -> *mut u8 {
        let stack_end = unsafe { *RuntimeInstance::stack_end() };
        let heap = unsafe { &mut *RuntimeInstance::heap() };
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
    unsafe { action.apply(&mut *RuntimeInstance::frames(), token) }
        .unwrap_or_else(|error| out_of_frames(error))
}

#[unsafe(export_name = "\x01zydeco_ffi_borrow_memory")]
extern "sysv64" fn zydeco_ffi_borrow_memory(window: Word) -> *const u8 {
    // The checked classifier fixes this ordinary product's three word fields.
    let fields = unsafe { std::slice::from_raw_parts(window as *const Word, 3) };
    RuntimeInstance::with_buffers(|arena| {
        arena
            .borrow()
            .read_memory(
                memory::MemoryBranch::access(fields[0]),
                memory::MemoryBranch::address(fields[1]),
                <i64 as RuntimeInteger>::decode(fields[2]),
            )
            .map(|bytes| bytes.as_ptr())
    })
    .unwrap_or_else(|error| RuntimeFailure::ForeignMemory(error).exit())
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
    HostString::own(string.to_string())
}

#[unsafe(export_name = "\x01zydeco_str_append")]
extern "sysv64" fn zydeco_str_append(first: Word, second: Word) -> Word {
    let first = unsafe { HostString::borrow(first) };
    let second = unsafe { HostString::borrow(second) };
    HostString::own([first, second].concat())
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

// Optional normalization may leave arithmetic calls intact. These entries obey
// the same wrapping/trapping and spare-box contracts as emitted primitives.
macro_rules! integer_arithmetic {
    ($type:ty, [$($extra:tt)*], $spare:expr;
        $add:ident, $sub:ident, $mul:ident, $div:ident, $rem:ident) => {
        #[unsafe(export_name = concat!("\x01", stringify!($add)))]
        extern "sysv64" fn $add(first: Word, second: Word $($extra)*) -> Word {
            <$type as RuntimeInteger>::decode(first)
                .wrapping_add(<$type as RuntimeInteger>::decode(second)).encode($spare)
        }
        #[unsafe(export_name = concat!("\x01", stringify!($sub)))]
        extern "sysv64" fn $sub(first: Word, second: Word $($extra)*) -> Word {
            <$type as RuntimeInteger>::decode(first)
                .wrapping_sub(<$type as RuntimeInteger>::decode(second)).encode($spare)
        }
        #[unsafe(export_name = concat!("\x01", stringify!($mul)))]
        extern "sysv64" fn $mul(first: Word, second: Word $($extra)*) -> Word {
            <$type as RuntimeInteger>::decode(first)
                .wrapping_mul(<$type as RuntimeInteger>::decode(second)).encode($spare)
        }
        #[unsafe(export_name = concat!("\x01", stringify!($div)))]
        extern "sysv64" fn $div(first: Word, second: Word $($extra)*) -> Word {
            let first = <$type as RuntimeInteger>::decode(first);
            let second = <$type as RuntimeInteger>::decode(second);
            if second == 0 { zydeco_integer_division_by_zero(); }
            first.wrapping_div(second).encode($spare)
        }
        #[unsafe(export_name = concat!("\x01", stringify!($rem)))]
        extern "sysv64" fn $rem(first: Word, second: Word $($extra)*) -> Word {
            let first = <$type as RuntimeInteger>::decode(first);
            let second = <$type as RuntimeInteger>::decode(second);
            if second == 0 { zydeco_integer_remainder_by_zero(); }
            first.wrapping_rem(second).encode($spare)
        }
    };
}

integer_arithmetic!(i8, [], std::ptr::null_mut(); zydeco_int8_add, zydeco_int8_sub, zydeco_int8_mul, zydeco_int8_div, zydeco_int8_mod);
integer_arithmetic!(i16, [], std::ptr::null_mut(); zydeco_int16_add, zydeco_int16_sub, zydeco_int16_mul, zydeco_int16_div, zydeco_int16_mod);
integer_arithmetic!(i32, [], std::ptr::null_mut(); zydeco_int32_add, zydeco_int32_sub, zydeco_int32_mul, zydeco_int32_div, zydeco_int32_mod);
integer_arithmetic!(i64, [, spare: *mut Word], spare; zydeco_int64_add, zydeco_int64_sub, zydeco_int64_mul, zydeco_int64_div, zydeco_int64_mod);
integer_arithmetic!(u8, [], std::ptr::null_mut(); zydeco_uint8_add, zydeco_uint8_sub, zydeco_uint8_mul, zydeco_uint8_div, zydeco_uint8_mod);
integer_arithmetic!(u16, [], std::ptr::null_mut(); zydeco_uint16_add, zydeco_uint16_sub, zydeco_uint16_mul, zydeco_uint16_div, zydeco_uint16_mod);
integer_arithmetic!(u32, [], std::ptr::null_mut(); zydeco_uint32_add, zydeco_uint32_sub, zydeco_uint32_mul, zydeco_uint32_div, zydeco_uint32_mod);
integer_arithmetic!(u64, [, spare: *mut Word], spare; zydeco_uint64_add, zydeco_uint64_sub, zydeco_uint64_mul, zydeco_uint64_div, zydeco_uint64_mod);

macro_rules! float_arithmetic {
    ($host:ty, $extra:tt, $spare:expr; $( $name:ident => $operation:tt ),+ $(,)?) => {
        $(
            float_arithmetic!(@one $host, $extra, $spare; $name => $operation);
        )+
    };
    (@one $host:ty, [$($extra:tt)*], $spare:expr; $name:ident => $operation:tt) => {
        #[unsafe(export_name = concat!("\x01", stringify!($name)))]
        extern "sysv64" fn $name(first: Word, second: Word $($extra)*) -> Word {
            <$host>::encode(<$host>::decode(first) $operation <$host>::decode(second), $spare)
        }
    };
}

float_arithmetic!(HostFloat32, [], std::ptr::null_mut();
    zydeco_float32_add => +, zydeco_float32_sub => -, zydeco_float32_mul => *, zydeco_float32_div => /);
float_arithmetic!(HostFloat64, [, spare: *mut Word], spare;
    zydeco_float64_add => +, zydeco_float64_sub => -, zydeco_float64_mul => *, zydeco_float64_div => /);

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
            HostString::own(<$type as RuntimeInteger>::decode(value).to_string())
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
            HostString::own(value.to_string())
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
    HostString::own(character.to_string())
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
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().read(reader, |reader| {
            let mut bytes = Vec::new();
            reader.take(count).read_to_end(&mut bytes)?;
            IoBranch::memory(&bytes)
        })
    });
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_read_line")]
extern "sysv64" fn zydeco_io_read_line(
    reader: Word, when_error: Word, when_eof: Word, when_line: Word,
) -> Word {
    let reader = HostHandle::decode(reader);
    let result = RuntimeInstance::with_io(|runtime| {
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
        | Ok((_, bytes)) => IoBranch::value(IoBranch::memory(&bytes), when_error, when_line),
        | Err(error) => IoBranch::error(when_error, error),
    }
}

#[unsafe(export_name = "\x01zydeco_io_read_all")]
extern "sysv64" fn zydeco_io_read_all(reader: Word, when_error: Word, when_success: Word) -> Word {
    let reader = HostHandle::decode(reader);
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().read(reader, |reader| {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes)?;
            IoBranch::memory(&bytes)
        })
    });
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_write_all")]
extern "sysv64" fn zydeco_io_write_all(
    writer: Word, access: Word, address: Word, length: Word, when_error: Word, when_success: Word,
) -> Word {
    let writer = HostHandle::decode(writer);
    let result = RuntimeInstance::with_buffers(|arena| {
        let arena = arena.borrow();
        let bytes = arena
            .read_memory(
                memory::MemoryBranch::access(access),
                memory::MemoryBranch::address(address),
                <i64 as RuntimeInteger>::decode(length),
            )
            .map_err(HostIoError::memory)?;
        RuntimeInstance::with_io(|runtime| {
            runtime.borrow_mut().write(writer, |writer| writer.write_all(bytes))
        })
    });
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_flush")]
extern "sysv64" fn zydeco_io_flush(writer: Word, when_error: Word, when_success: Word) -> Word {
    let writer = HostHandle::decode(writer);
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().write(writer, |writer| writer.flush())
    });
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_close_reader")]
extern "sysv64" fn zydeco_io_close_reader(
    reader: Word, when_error: Word, when_success: Word,
) -> Word {
    let reader = HostHandle::decode(reader);
    let result = RuntimeInstance::with_io(|runtime| runtime.borrow_mut().close_reader(reader));
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_io_close_writer")]
extern "sysv64" fn zydeco_io_close_writer(
    writer: Word, when_error: Word, when_success: Word,
) -> Word {
    let writer = HostHandle::decode(writer);
    let result = RuntimeInstance::with_io(|runtime| runtime.borrow_mut().close_writer(writer));
    IoBranch::unit(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_fs_open_reader")]
extern "sysv64" fn zydeco_fs_open_reader(path: Word, when_error: Word, when_success: Word) -> Word {
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().open_reader(unsafe { HostString::borrow(path) })
    })
    .map(HostHandle::encode);
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_fs_create_writer")]
extern "sysv64" fn zydeco_fs_create_writer(
    path: Word, when_error: Word, when_success: Word,
) -> Word {
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().create_writer(unsafe { HostString::borrow(path) })
    })
    .map(HostHandle::encode);
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_fs_append_writer")]
extern "sysv64" fn zydeco_fs_append_writer(
    path: Word, when_error: Word, when_success: Word,
) -> Word {
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().append_writer(unsafe { HostString::borrow(path) })
    })
    .map(HostHandle::encode);
    IoBranch::value(result, when_error, when_success)
}

#[unsafe(export_name = "\x01zydeco_read_line")]
extern "sysv64" fn zydeco_read_line(continuation: Word) -> Word {
    let line = Input::line();
    HostControl::with_one_argument(continuation, HostString::own(line))
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
    HostControl::with_one_argument(continuation, HostString::own(Input::remaining()))
}

#[unsafe(export_name = "\x01zydeco_write_str")]
extern "sysv64" fn zydeco_write_str(string: Word, continuation: Word) -> Word {
    RuntimeInstance::with_io(|runtime| {
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
    RuntimeInstance::with_io(|runtime| {
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
    RuntimeInstance::with_io(|runtime| {
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
    let argument = RuntimeInstance::with_arguments(|arguments| {
        index.and_then(|index| arguments.get(index)).copied()
    });
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

/// Each external entry owns all mutable language and host state. The TLS pointer only dispatches
/// helpers; a suspended caller retains its complete instance while another unit is active.
struct RuntimeInstance {
    heap: CheneyHeap<HEAP_SPACE_BYTES, HEAP_INDEX_REGIONS>,
    frames: NativeFrames<Growable>,
    stack_end: *mut Word,
    transfer: HostTransfer<Word>,
    buffers: RefCell<BufferArena>,
    io: RefCell<HostIoRuntime>,
    arguments: Vec<Word>,
    // Stable, host-owned strings contain no managed references.
    #[allow(clippy::vec_box)]
    strings: Vec<Box<String>>,
    previous: *mut Self,
    guard: *const std::sync::atomic::AtomicBool,
}

thread_local! {
    static CURRENT_INSTANCE: Cell<*mut RuntimeInstance> = const { Cell::new(std::ptr::null_mut()) };
}

impl RuntimeInstance {
    fn current() -> *mut Self {
        CURRENT_INSTANCE.with(|current| {
            let instance = current.get();
            assert!(!instance.is_null(), "native runtime helper outside an entry");
            instance
        })
    }

    fn heap() -> *mut CheneyHeap<HEAP_SPACE_BYTES, HEAP_INDEX_REGIONS> {
        unsafe { &raw mut (*Self::current()).heap }
    }

    fn frames() -> *mut NativeFrames<Growable> {
        unsafe { &raw mut (*Self::current()).frames }
    }

    fn stack_end() -> *mut *mut Word {
        unsafe { &raw mut (*Self::current()).stack_end }
    }

    fn transfer() -> *mut HostTransfer<Word> {
        unsafe { &raw mut (*Self::current()).transfer }
    }

    fn with_buffers<T>(f: impl FnOnce(&RefCell<BufferArena>) -> T) -> T {
        f(unsafe { &(*Self::current()).buffers })
    }

    fn with_io<T>(f: impl FnOnce(&RefCell<HostIoRuntime>) -> T) -> T {
        f(unsafe { &(*Self::current()).io })
    }

    fn with_arguments<T>(f: impl FnOnce(&[Word]) -> T) -> T {
        f(unsafe { &(*Self::current()).arguments })
    }
}

zydeco_machine::export_native_entry! {
/// Acquire the unit guard before any source initialization. Raw C arguments and saved ABI
/// registers are above `stack_end` and never scanned by the collector.
extern "sysv64" fn entry_begin(stack_end: *mut Word, guard: *const std::sync::atomic::AtomicBool) {
    use std::sync::atomic::Ordering;
    if !guard.is_null() && unsafe { &*guard }.compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed).is_err() {
        eprintln!("Zydeco runtime: concurrent or reentrant entry into an active compiled library");
        std::process::exit(1);
    }
    let previous = CURRENT_INSTANCE.with(Cell::get);
    let instance = Box::new(RuntimeInstance {
        heap: CheneyHeap::new(), frames: NativeFrames::EMPTY, stack_end,
        transfer: HostTransfer { resume: 0, closure: 0, first: 0, second: 0 },
        buffers: RefCell::new(BufferArena::default()), io: RefCell::new(HostIoRuntime::new()),
        arguments: Vec::new(), strings: Vec::new(), previous, guard,
    });
    CURRENT_INSTANCE.with(|current| current.set(Box::into_raw(instance)));
}
}

#[unsafe(export_name = "\x01zydeco_entry_end")]
extern "sysv64" fn entry_end() {
    use std::sync::atomic::Ordering;
    let instance = unsafe { Box::from_raw(RuntimeInstance::current()) };
    CURRENT_INSTANCE.with(|current| current.set(instance.previous));
    let guard = instance.guard;
    drop(instance);
    if !guard.is_null() {
        unsafe { &*guard }.store(false, Ordering::Release);
    }
}

/// Root already encoded arguments before allocating the next full-width input box.
#[unsafe(export_name = "\x01zydeco_entry_box")]
extern "sysv64" fn entry_box(stack_start: *mut Word) -> *mut Word {
    let end = unsafe { *RuntimeInstance::stack_end() };
    let roots = Roots { stack: RootRange { start: stack_start, end }, slots: &mut [] };
    unsafe { (&mut *RuntimeInstance::heap()).allocate(1, AllocationKind::Opaque, roots) }
        .unwrap_or_else(|error| out_of_memory(error))
        .cast()
}

#[cfg(feature = "process-entry")]
pub fn run_process() {
    let stack_anchor: Word = 0;
    entry_begin(std::ptr::addr_of!(stack_anchor).cast_mut(), std::ptr::null());
    let arguments = std::env::args().skip(1).map(HostString::own).collect();
    unsafe {
        (*RuntimeInstance::current()).arguments = arguments;
        zydeco_machine::native::entry();
    }
    entry_end();
}

#[cfg(test)]
mod entry_tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, Ordering};

    #[test]
    fn independent_entries_restore_the_caller_and_start_with_fresh_host_state() {
        let first = AtomicBool::new(false);
        let second = AtomicBool::new(false);
        let mut anchor: Word = 0;
        entry_begin(&mut anchor, &first);
        let caller = RuntimeInstance::current();
        let caller_heap = RuntimeInstance::heap();
        let string = HostString::own("caller state".into());
        unsafe {
            (*caller).arguments.push(string);
        }
        entry_begin(&mut anchor, &second);
        assert_ne!(RuntimeInstance::current(), caller);
        assert_ne!(RuntimeInstance::heap(), caller_heap);
        RuntimeInstance::with_arguments(|arguments| assert!(arguments.is_empty()));
        assert!(unsafe { (*RuntimeInstance::current()).strings.is_empty() });
        HostString::own("callee state".into());
        entry_end();
        assert!(!second.load(Ordering::Relaxed));
        assert!(first.load(Ordering::Relaxed));
        assert_eq!(RuntimeInstance::current(), caller);
        RuntimeInstance::with_arguments(|arguments| assert_eq!(arguments, &[string]));
        assert_eq!(unsafe { HostString::borrow(string) }, "caller state");
        entry_end();
        assert!(!first.load(Ordering::Relaxed));
        assert!(CURRENT_INSTANCE.with(Cell::get).is_null());
        entry_begin(&mut anchor, &first);
        assert!(unsafe { (*RuntimeInstance::current()).strings.is_empty() });
        RuntimeInstance::with_arguments(|arguments| assert!(arguments.is_empty()));
        entry_end();
    }
}
