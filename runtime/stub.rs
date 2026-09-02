mod gc;

use gc::{BlockTag, CheneyHeap, IMMEDIATE_TAG, OutOfMemory, RootRange, Roots};
use std::{
    cell::{RefCell, UnsafeCell},
    collections::HashMap,
    fs::{File, OpenOptions},
    io::{self, BufRead, BufReader, Read, Write},
};

type Word = usize;

const IMMEDIATE_UNSIGNED_MAX: Word = Word::MAX >> 1;
const IMMEDIATE_SIGNED_MIN: i64 = -(1_i64 << 62);
const IMMEDIATE_SIGNED_MAX: i64 = (1_i64 << 62) - 1;

/// Odd immediate values in the native one-word representation.
struct Immediate;

impl Immediate {
    fn unsigned(value: Word) -> Option<Word> {
        (value <= IMMEDIATE_UNSIGNED_MAX).then_some((value << 1) | IMMEDIATE_TAG)
    }

    fn signed(value: i64) -> Option<Word> {
        (IMMEDIATE_SIGNED_MIN..=IMMEDIATE_SIGNED_MAX)
            .contains(&value)
            .then_some(((value as Word) << 1) | IMMEDIATE_TAG)
    }

    fn expect_unsigned(value: Word) -> Word {
        Self::unsigned(value).expect("runtime value does not fit an unsigned immediate")
    }

    fn expect_signed(value: i64) -> Word {
        Self::signed(value).expect("runtime value does not fit a signed immediate")
    }

    fn decode_unsigned(value: Word) -> Word {
        assert_eq!(value & IMMEDIATE_TAG, IMMEDIATE_TAG, "expected an immediate value");
        value >> 1
    }

    fn decode_signed(value: Word) -> i64 {
        assert_eq!(value & IMMEDIATE_TAG, IMMEDIATE_TAG, "expected an immediate value");
        (value as i64) >> 1
    }
}

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
/// buffers here, rather than behind `Box` or `std::alloc`, puts both semispaces and
/// the environment in the executable's fixed static storage.
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

#[repr(C)]
struct ZydecoClosure {
    environment: *mut u8,
    code: *mut u8,
}

#[repr(C)]
struct Continuation {
    resume: *mut u8,
    closure: Word,
    first: Word,
    second: Word,
}

impl Continuation {
    fn without_arguments(closure: Word) -> Word {
        Self::store(rust_resume_zydeco_0, closure, 0, 0)
    }

    fn with_one_argument(closure: Word, argument: Word) -> Word {
        Self::store(rust_resume_zydeco_1, closure, argument, 0)
    }

    fn with_two_arguments(closure: Word, first: Word, second: Word) -> Word {
        Self::store(rust_resume_zydeco_2, closure, first, second)
    }

    fn store(
        resume: unsafe extern "sysv64" fn(), closure: Word, first: Word, second: Word,
    ) -> Word {
        let resume = resume as *const () as *mut u8;
        let transfer = CONTROL_TRANSFER.get();
        // A control transfer is consumed by the assembly bridge before another host
        // call can occur, so one fixed protocol slot is sufficient.
        unsafe { transfer.write(Self { resume, closure, first, second }) };
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

struct HostBytes;

impl HostBytes {
    fn leak(bytes: Vec<u8>) -> Word {
        Box::into_raw(Box::new(bytes)) as Word
    }

    unsafe fn borrow<'a>(raw: Word) -> &'a [u8] {
        unsafe { &*(raw as *const Vec<u8>) }.as_slice()
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
        Continuation::with_two_arguments(
            continuation,
            Immediate::expect_signed(HostIoErrorKind::from_error(&error) as i64),
            HostString::leak(error.to_string()),
        )
    }

    fn unit(result: io::Result<()>, when_error: Word, when_success: Word) -> Word {
        match result {
            | Ok(()) => Continuation::without_arguments(when_success),
            | Err(error) => Self::error(when_error, error),
        }
    }

    fn value(result: io::Result<Word>, when_error: Word, when_success: Word) -> Word {
        match result {
            | Ok(value) => Continuation::with_one_argument(when_success, value),
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
        Continuation::without_arguments(if condition { when_true } else { when_false })
    }
}

struct OptionalPairBranch;

impl OptionalPairBranch {
    fn select(pair: Option<(String, String)>, when_none: Word, when_some: Word) -> Word {
        match pair {
            | None => Continuation::without_arguments(when_none),
            | Some((first, second)) => Continuation::with_two_arguments(
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

struct ArgumentFold {
    closure: ZydecoClosure,
    arguments: std::vec::IntoIter<String>,
    when_empty: Word,
    when_item: Word,
}

impl ArgumentFold {
    fn from_process(when_empty: Word, when_item: Word) -> Self {
        let arguments = std::env::args().skip(1).collect::<Vec<_>>().into_iter();
        let closure =
            ZydecoClosure { environment: std::ptr::null_mut(), code: std::ptr::null_mut() };
        Self { closure, arguments, when_empty, when_item }
    }

    fn into_thunk(self) -> Word {
        let environment = Box::into_raw(Box::new(self));
        // This host object outlives the callback that creates it. Register the two
        // managed words it owns so a collection can rewrite them before the thunk
        // is resumed.
        let roots = unsafe {
            [
                std::ptr::addr_of_mut!((*environment).when_empty),
                std::ptr::addr_of_mut!((*environment).when_item),
            ]
        };
        unsafe { &mut *HOST_ROOTS.get() }
            .register(&roots)
            .unwrap_or_else(|error| out_of_host_roots(error));
        unsafe {
            (*environment).closure.environment = environment.cast::<u8>();
            (*environment).closure.code = rust_arg_fold_tail as *const () as *mut u8;
            std::ptr::addr_of_mut!((*environment).closure) as Word
        }
    }

    unsafe fn from_environment(environment: *mut u8) -> Box<Self> {
        let environment = environment.cast::<Self>();
        let roots = unsafe {
            [
                std::ptr::addr_of_mut!((*environment).when_empty),
                std::ptr::addr_of_mut!((*environment).when_item),
            ]
        };
        unsafe { &mut *HOST_ROOTS.get() }.unregister(&roots);
        unsafe { Box::from_raw(environment) }
    }

    fn resume(mut self) -> Word {
        match self.arguments.next() {
            | None => Continuation::without_arguments(self.when_empty),
            | Some(argument) => {
                let when_item = self.when_item;
                let tail = self.into_thunk();
                Continuation::with_two_arguments(when_item, HostString::leak(argument), tail)
            }
        }
    }
}

#[unsafe(export_name = "\x01zydeco_abort")]
extern "sysv64" fn zydeco_abort() -> ! {
    std::process::abort()
}

struct ManagedHeap;

impl ManagedHeap {
    fn allocate(
        size_words: usize, tag: BlockTag, stack_start: *mut Word, environment: *mut Word,
        environment_words: usize,
    ) -> *mut u8 {
        let stack_end = unsafe { *STACK_END.get() };
        let heap = unsafe { &mut *HEAP.get() };
        let host_roots = unsafe { &mut *HOST_ROOTS.get() };
        let roots = Roots {
            stack: RootRange { start: stack_start, end: stack_end },
            environment: RootRange {
                start: environment,
                end: unsafe { environment.add(environment_words) },
            },
            host: &mut host_roots.slots,
        };
        // SAFETY: the emitter passes the live Zydeco control-stack cursor and current
        // environment frame. `main` records the upper stack boundary before entry.
        unsafe { heap.allocate(size_words, tag, roots) }
            .unwrap_or_else(|error| out_of_memory(error))
    }
}

#[unsafe(export_name = "\x01zydeco_alloc_scanned")]
extern "sysv64" fn zydeco_alloc_scanned(
    size_words: usize, stack_start: *mut Word, environment: *mut Word, environment_words: usize,
) -> *mut u8 {
    ManagedHeap::allocate(
        size_words,
        BlockTag::Scanned,
        stack_start,
        environment,
        environment_words,
    )
}

#[unsafe(export_name = "\x01zydeco_alloc_opaque")]
extern "sysv64" fn zydeco_alloc_opaque(
    size_words: usize, stack_start: *mut Word, environment: *mut Word, environment_words: usize,
) -> *mut u8 {
    ManagedHeap::allocate(size_words, BlockTag::Opaque, stack_start, environment, environment_words)
}

#[unsafe(export_name = "\x01zydeco_ffi_borrow_bytes")]
extern "sysv64" fn zydeco_ffi_borrow_bytes(bytes: Word) -> ForeignBytes {
    let bytes = unsafe { HostBytes::borrow(bytes) };
    ForeignBytes { pointer: bytes.as_ptr(), length: bytes.len() }
}

#[unsafe(export_name = "\x01zydeco_ffi_decode_u64")]
extern "sysv64" fn zydeco_ffi_decode_u64(value: Word) -> u64 {
    <u64 as RuntimeInteger>::decode(value)
}

#[unsafe(export_name = "\x01zydeco_ffi_encode_u64")]
extern "sysv64" fn zydeco_ffi_encode_u64(value: u64, spare: *mut Word) -> Word {
    <u64 as RuntimeInteger>::encode(value, spare)
}

unsafe extern "sysv64" {
    #[link_name = "\x01rust_arg_fold_tail"]
    fn rust_arg_fold_tail();
    #[link_name = "\x01rust_resume_zydeco_0"]
    fn rust_resume_zydeco_0();
    #[link_name = "\x01rust_resume_zydeco_1"]
    fn rust_resume_zydeco_1();
    #[link_name = "\x01rust_resume_zydeco_2"]
    fn rust_resume_zydeco_2();
}

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
        | None => Continuation::without_arguments(when_none),
        | Some(character) => Continuation::with_one_argument(
            when_some,
            Immediate::expect_unsigned(character as Word),
        ),
    }
}

macro_rules! integer_runtime {
    (
        $type:ty,
        $add:ident => $add_symbol:literal,
        $sub:ident => $sub_symbol:literal,
        $mul:ident => $mul_symbol:literal,
        $div:ident => $div_symbol:literal,
        $modulo:ident => $modulo_symbol:literal,
        $eq:ident => $eq_symbol:literal,
        $lt:ident => $lt_symbol:literal,
        $gt:ident => $gt_symbol:literal,
        $to_string:ident => $to_string_symbol:literal
    ) => {
        #[unsafe(export_name = $add_symbol)]
        extern "sysv64" fn $add(first: Word, second: Word, spare: *mut Word) -> Word {
            let first = <$type as RuntimeInteger>::decode(first);
            let second = <$type as RuntimeInteger>::decode(second);
            RuntimeInteger::encode(first.wrapping_add(second), spare)
        }

        #[unsafe(export_name = $sub_symbol)]
        extern "sysv64" fn $sub(first: Word, second: Word, spare: *mut Word) -> Word {
            let first = <$type as RuntimeInteger>::decode(first);
            let second = <$type as RuntimeInteger>::decode(second);
            RuntimeInteger::encode(first.wrapping_sub(second), spare)
        }

        #[unsafe(export_name = $mul_symbol)]
        extern "sysv64" fn $mul(first: Word, second: Word, spare: *mut Word) -> Word {
            let first = <$type as RuntimeInteger>::decode(first);
            let second = <$type as RuntimeInteger>::decode(second);
            RuntimeInteger::encode(first.wrapping_mul(second), spare)
        }

        #[unsafe(export_name = $div_symbol)]
        extern "sysv64" fn $div(first: Word, second: Word, spare: *mut Word) -> Word {
            let first = <$type as RuntimeInteger>::decode(first);
            let second = <$type as RuntimeInteger>::decode(second);
            RuntimeInteger::encode(first.wrapping_div(second), spare)
        }

        #[unsafe(export_name = $modulo_symbol)]
        extern "sysv64" fn $modulo(first: Word, second: Word, spare: *mut Word) -> Word {
            let first = <$type as RuntimeInteger>::decode(first);
            let second = <$type as RuntimeInteger>::decode(second);
            RuntimeInteger::encode(first.wrapping_rem(second), spare)
        }

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
    zydeco_int8_add => "\x01zydeco_int8_add",
    zydeco_int8_sub => "\x01zydeco_int8_sub",
    zydeco_int8_mul => "\x01zydeco_int8_mul",
    zydeco_int8_div => "\x01zydeco_int8_div",
    zydeco_int8_mod => "\x01zydeco_int8_mod",
    zydeco_int8_eq_branch => "\x01zydeco_int8_eq_branch",
    zydeco_int8_lt_branch => "\x01zydeco_int8_lt_branch",
    zydeco_int8_gt_branch => "\x01zydeco_int8_gt_branch",
    zydeco_int8_to_string => "\x01zydeco_int8_to_string"
);
integer_runtime!(
    i16,
    zydeco_int16_add => "\x01zydeco_int16_add",
    zydeco_int16_sub => "\x01zydeco_int16_sub",
    zydeco_int16_mul => "\x01zydeco_int16_mul",
    zydeco_int16_div => "\x01zydeco_int16_div",
    zydeco_int16_mod => "\x01zydeco_int16_mod",
    zydeco_int16_eq_branch => "\x01zydeco_int16_eq_branch",
    zydeco_int16_lt_branch => "\x01zydeco_int16_lt_branch",
    zydeco_int16_gt_branch => "\x01zydeco_int16_gt_branch",
    zydeco_int16_to_string => "\x01zydeco_int16_to_string"
);
integer_runtime!(
    i32,
    zydeco_int32_add => "\x01zydeco_int32_add",
    zydeco_int32_sub => "\x01zydeco_int32_sub",
    zydeco_int32_mul => "\x01zydeco_int32_mul",
    zydeco_int32_div => "\x01zydeco_int32_div",
    zydeco_int32_mod => "\x01zydeco_int32_mod",
    zydeco_int32_eq_branch => "\x01zydeco_int32_eq_branch",
    zydeco_int32_lt_branch => "\x01zydeco_int32_lt_branch",
    zydeco_int32_gt_branch => "\x01zydeco_int32_gt_branch",
    zydeco_int32_to_string => "\x01zydeco_int32_to_string"
);
integer_runtime!(
    i64,
    zydeco_int64_add => "\x01zydeco_int64_add",
    zydeco_int64_sub => "\x01zydeco_int64_sub",
    zydeco_int64_mul => "\x01zydeco_int64_mul",
    zydeco_int64_div => "\x01zydeco_int64_div",
    zydeco_int64_mod => "\x01zydeco_int64_mod",
    zydeco_int64_eq_branch => "\x01zydeco_int64_eq_branch",
    zydeco_int64_lt_branch => "\x01zydeco_int64_lt_branch",
    zydeco_int64_gt_branch => "\x01zydeco_int64_gt_branch",
    zydeco_int64_to_string => "\x01zydeco_int64_to_string"
);
integer_runtime!(
    u8,
    zydeco_uint8_add => "\x01zydeco_uint8_add",
    zydeco_uint8_sub => "\x01zydeco_uint8_sub",
    zydeco_uint8_mul => "\x01zydeco_uint8_mul",
    zydeco_uint8_div => "\x01zydeco_uint8_div",
    zydeco_uint8_mod => "\x01zydeco_uint8_mod",
    zydeco_uint8_eq_branch => "\x01zydeco_uint8_eq_branch",
    zydeco_uint8_lt_branch => "\x01zydeco_uint8_lt_branch",
    zydeco_uint8_gt_branch => "\x01zydeco_uint8_gt_branch",
    zydeco_uint8_to_string => "\x01zydeco_uint8_to_string"
);
integer_runtime!(
    u16,
    zydeco_uint16_add => "\x01zydeco_uint16_add",
    zydeco_uint16_sub => "\x01zydeco_uint16_sub",
    zydeco_uint16_mul => "\x01zydeco_uint16_mul",
    zydeco_uint16_div => "\x01zydeco_uint16_div",
    zydeco_uint16_mod => "\x01zydeco_uint16_mod",
    zydeco_uint16_eq_branch => "\x01zydeco_uint16_eq_branch",
    zydeco_uint16_lt_branch => "\x01zydeco_uint16_lt_branch",
    zydeco_uint16_gt_branch => "\x01zydeco_uint16_gt_branch",
    zydeco_uint16_to_string => "\x01zydeco_uint16_to_string"
);
integer_runtime!(
    u32,
    zydeco_uint32_add => "\x01zydeco_uint32_add",
    zydeco_uint32_sub => "\x01zydeco_uint32_sub",
    zydeco_uint32_mul => "\x01zydeco_uint32_mul",
    zydeco_uint32_div => "\x01zydeco_uint32_div",
    zydeco_uint32_mod => "\x01zydeco_uint32_mod",
    zydeco_uint32_eq_branch => "\x01zydeco_uint32_eq_branch",
    zydeco_uint32_lt_branch => "\x01zydeco_uint32_lt_branch",
    zydeco_uint32_gt_branch => "\x01zydeco_uint32_gt_branch",
    zydeco_uint32_to_string => "\x01zydeco_uint32_to_string"
);
integer_runtime!(
    u64,
    zydeco_uint64_add => "\x01zydeco_uint64_add",
    zydeco_uint64_sub => "\x01zydeco_uint64_sub",
    zydeco_uint64_mul => "\x01zydeco_uint64_mul",
    zydeco_uint64_div => "\x01zydeco_uint64_div",
    zydeco_uint64_mod => "\x01zydeco_uint64_mod",
    zydeco_uint64_eq_branch => "\x01zydeco_uint64_eq_branch",
    zydeco_uint64_lt_branch => "\x01zydeco_uint64_lt_branch",
    zydeco_uint64_gt_branch => "\x01zydeco_uint64_gt_branch",
    zydeco_uint64_to_string => "\x01zydeco_uint64_to_string"
);

struct IntrinsicInt64;

impl IntrinsicInt64 {
    fn compare(first: Word, second: Word, predicate: impl FnOnce(i64, i64) -> bool) -> Word {
        Immediate::expect_unsigned(predicate(
            <i64 as RuntimeInteger>::decode(first),
            <i64 as RuntimeInteger>::decode(second),
        ) as Word)
    }

    fn bitwise(
        first: Word, second: Word, spare: *mut Word, op: impl FnOnce(i64, i64) -> i64,
    ) -> Word {
        op(<i64 as RuntimeInteger>::decode(first), <i64 as RuntimeInteger>::decode(second))
            .encode(spare)
    }
}

#[unsafe(export_name = "\x01zydeco_intrinsic_int64_eq")]
extern "sysv64" fn zydeco_intrinsic_int64_eq(first: Word, second: Word) -> Word {
    IntrinsicInt64::compare(first, second, |first, second| first == second)
}

#[unsafe(export_name = "\x01zydeco_intrinsic_int64_lt")]
extern "sysv64" fn zydeco_intrinsic_int64_lt(first: Word, second: Word) -> Word {
    IntrinsicInt64::compare(first, second, |first, second| first < second)
}

#[unsafe(export_name = "\x01zydeco_intrinsic_int64_gt")]
extern "sysv64" fn zydeco_intrinsic_int64_gt(first: Word, second: Word) -> Word {
    IntrinsicInt64::compare(first, second, |first, second| first > second)
}

#[unsafe(export_name = "\x01zydeco_intrinsic_int64_and")]
extern "sysv64" fn zydeco_intrinsic_int64_and(first: Word, second: Word, spare: *mut Word) -> Word {
    IntrinsicInt64::bitwise(first, second, spare, |first, second| first & second)
}

#[unsafe(export_name = "\x01zydeco_intrinsic_int64_or")]
extern "sysv64" fn zydeco_intrinsic_int64_or(first: Word, second: Word, spare: *mut Word) -> Word {
    IntrinsicInt64::bitwise(first, second, spare, |first, second| first | second)
}

#[unsafe(export_name = "\x01zydeco_intrinsic_int64_xor")]
extern "sysv64" fn zydeco_intrinsic_int64_xor(first: Word, second: Word, spare: *mut Word) -> Word {
    IntrinsicInt64::bitwise(first, second, spare, |first, second| first ^ second)
}

macro_rules! float_runtime {
    (
        $type:ty, $codec:ident,
        $add:ident => $add_symbol:literal,
        $sub:ident => $sub_symbol:literal,
        $mul:ident => $mul_symbol:literal,
        $div:ident => $div_symbol:literal,
        $eq:ident => $eq_symbol:literal,
        $lt:ident => $lt_symbol:literal,
        $gt:ident => $gt_symbol:literal,
        $to_string:ident => $to_string_symbol:literal
    ) => {
        #[unsafe(export_name = $add_symbol)]
        extern "sysv64" fn $add(first: Word, second: Word, spare: *mut Word) -> Word {
            $codec::encode($codec::decode(first) + $codec::decode(second), spare)
        }

        #[unsafe(export_name = $sub_symbol)]
        extern "sysv64" fn $sub(first: Word, second: Word, spare: *mut Word) -> Word {
            $codec::encode($codec::decode(first) - $codec::decode(second), spare)
        }

        #[unsafe(export_name = $mul_symbol)]
        extern "sysv64" fn $mul(first: Word, second: Word, spare: *mut Word) -> Word {
            $codec::encode($codec::decode(first) * $codec::decode(second), spare)
        }

        #[unsafe(export_name = $div_symbol)]
        extern "sysv64" fn $div(first: Word, second: Word, spare: *mut Word) -> Word {
            $codec::encode($codec::decode(first) / $codec::decode(second), spare)
        }

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
    zydeco_float32_add => "\x01zydeco_float32_add",
    zydeco_float32_sub => "\x01zydeco_float32_sub",
    zydeco_float32_mul => "\x01zydeco_float32_mul",
    zydeco_float32_div => "\x01zydeco_float32_div",
    zydeco_float32_eq_branch => "\x01zydeco_float32_eq_branch",
    zydeco_float32_lt_branch => "\x01zydeco_float32_lt_branch",
    zydeco_float32_gt_branch => "\x01zydeco_float32_gt_branch",
    zydeco_float32_to_string => "\x01zydeco_float32_to_string"
);
float_runtime!(
    f64, HostFloat64,
    zydeco_float64_add => "\x01zydeco_float64_add",
    zydeco_float64_sub => "\x01zydeco_float64_sub",
    zydeco_float64_mul => "\x01zydeco_float64_mul",
    zydeco_float64_div => "\x01zydeco_float64_div",
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
        | None => Continuation::without_arguments(when_none),
        | Some(character) => Continuation::with_one_argument(
            when_some,
            Immediate::expect_unsigned(character as Word),
        ),
    }
}

#[unsafe(export_name = "\x01zydeco_str_parse_int_branch")]
extern "sysv64" fn zydeco_str_parse_int_branch(
    string: Word, when_none: Word, when_some: Word, spare: *mut Word,
) -> Word {
    match unsafe { HostString::borrow(string) }.parse::<i64>() {
        | Err(_) => Continuation::without_arguments(when_none),
        | Ok(integer) => Continuation::with_one_argument(when_some, integer.encode(spare)),
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
        | Err(_) => Continuation::without_arguments(when_invalid),
        | Ok(string) => {
            Continuation::with_one_argument(when_valid, HostString::leak(string.to_string()))
        }
    }
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
        | Ok((0, _)) => Continuation::without_arguments(when_eof),
        | Ok((_, bytes)) => Continuation::with_one_argument(when_line, HostBytes::leak(bytes)),
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
    Continuation::with_one_argument(continuation, HostString::leak(line))
}

#[unsafe(export_name = "\x01zydeco_read_line_as_int_branch")]
extern "sysv64" fn zydeco_read_line_as_int_branch(
    when_invalid: Word, when_valid: Word, spare: *mut Word,
) -> Word {
    match Input::line().parse::<i64>() {
        | Ok(integer) => Continuation::with_one_argument(when_valid, integer.encode(spare)),
        | Err(_) => Continuation::without_arguments(when_invalid),
    }
}

#[unsafe(export_name = "\x01zydeco_read_till_eof")]
extern "sysv64" fn zydeco_read_till_eof(continuation: Word) -> Word {
    Continuation::with_one_argument(continuation, HostString::leak(Input::remaining()))
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
    Continuation::without_arguments(continuation)
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
    Continuation::without_arguments(continuation)
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
    Continuation::without_arguments(continuation)
}

#[unsafe(export_name = "\x01zydeco_arg_fold")]
extern "sysv64" fn zydeco_arg_fold(when_empty: Word, when_item: Word) -> Word {
    ArgumentFold::from_process(when_empty, when_item).resume()
}

#[unsafe(export_name = "\x01zydeco_arg_fold_resume")]
extern "sysv64" fn zydeco_arg_fold_resume(environment: *mut u8) -> Word {
    unsafe { ArgumentFold::from_environment(environment) }.resume()
}

#[unsafe(export_name = "\x01zydeco_random_int")]
extern "sysv64" fn zydeco_random_int(continuation: Word, spare: *mut Word) -> Word {
    use rand::RngExt;
    let integer = rand::rng().random_range(i64::MIN..=i64::MAX);
    Continuation::with_one_argument(continuation, integer.encode(spare))
}

/* ---------------------------------- Entry --------------------------------- */

unsafe extern "sysv64" {
    #[link_name = "\x01entry"]
    fn entry(environment: *mut u8) -> Word;
}

const HEAP_SPACE_BYTES: usize = 1024 * 1024;
const ENVIRONMENT_BYTES: usize = 1024 * 1024;
const HOST_ROOT_CAPACITY: usize = 256;

#[repr(align(8))]
struct Environment([u8; ENVIRONMENT_BYTES]);

impl Environment {
    const fn new() -> Self {
        Self([0; ENVIRONMENT_BYTES])
    }
}

struct HostRoots {
    slots: [*mut Word; HOST_ROOT_CAPACITY],
}

struct HostRootOverflow;

impl HostRoots {
    const fn new() -> Self {
        Self { slots: [std::ptr::null_mut(); HOST_ROOT_CAPACITY] }
    }

    fn register(&mut self, roots: &[*mut Word]) -> Result<(), HostRootOverflow> {
        if self.slots.iter().filter(|slot| slot.is_null()).count() < roots.len() {
            return Err(HostRootOverflow);
        }
        roots.iter().for_each(|root| {
            let slot = self.slots.iter_mut().find(|slot| slot.is_null()).unwrap();
            *slot = *root;
        });
        Ok(())
    }

    fn unregister(&mut self, roots: &[*mut Word]) {
        roots.iter().for_each(|root| {
            let slot =
                self.slots.iter_mut().find(|slot| **slot == *root).expect("unregistered host root");
            *slot = std::ptr::null_mut();
        });
    }
}

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

fn out_of_host_roots(_error: HostRootOverflow) -> ! {
    let _ = writeln!(
        std::io::stderr().lock(),
        "Zydeco runtime: out of memory (host root table is full)",
    );
    std::process::exit(1)
}

static HEAP: RuntimeCell<CheneyHeap<HEAP_SPACE_BYTES>> = RuntimeCell::new(CheneyHeap::new());
static ENVIRONMENT: RuntimeCell<Environment> = RuntimeCell::new(Environment::new());
static STACK_END: RuntimeCell<*mut Word> = RuntimeCell::new(std::ptr::null_mut());
static HOST_ROOTS: RuntimeCell<HostRoots> = RuntimeCell::new(HostRoots::new());
static CONTROL_TRANSFER: RuntimeCell<Continuation> = RuntimeCell::new(Continuation {
    resume: std::ptr::null_mut(),
    closure: 0,
    first: 0,
    second: 0,
});

thread_local! {
    static HOST_IO: RefCell<HostIoRuntime> = RefCell::new(HostIoRuntime::new());
}

fn main() {
    let stack_anchor: Word = 0;
    unsafe {
        *STACK_END.get() = std::ptr::addr_of!(stack_anchor).add(1).cast_mut();
        let environment = (&mut *ENVIRONMENT.get()).0.as_mut_ptr();
        entry(environment);
    }
}
