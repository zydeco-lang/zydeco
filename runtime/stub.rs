mod memory;

use std::{
    cell::{Cell, RefCell},
    io::{self, Write},
};
use zydeco_abi::{HostIoErrorKind, RuntimeFailure};
use zydeco_host::{
    gc::{CheneyHeap, OutOfMemory, RootRange, RootSource, Roots},
    io::{HostIoError, HostIoRuntime, LineInput, ReaderHandle, WriterHandle},
    random::random_signed_int,
    string::{
        StaticHeapString, borrow_heap_str, char_from_codepoint, heap_string_words,
        parse_signed_int, split_at_scalar_byte, split_once_byte_ranges, write_heap_string,
        write_heap_string_concat,
    },
};
use zydeco_machine::frames::{Action, FrameError, Frames, storage::Growable};
use zydeco_machine::memory::RetainedMemory;
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

/// Immediate source integers; i64/u64 are the carriers of Int/UInt here.
trait RuntimeInteger: Copy {
    fn decode(value: Word) -> Self;
    fn encode(self) -> Word;

    fn out_of_range() -> ! {
        fail_runtime(RuntimeFailure::IntegerRange)
    }
}

fn fail_runtime(failure: RuntimeFailure) -> ! {
    let _ = writeln!(std::io::stderr().lock(), "Zydeco runtime: {}", failure.message());
    std::process::exit(1)
}

#[unsafe(export_name = "\x01zydeco_integer_out_of_range")]
extern "sysv64" fn integer_out_of_range() -> ! {
    <i64 as RuntimeInteger>::out_of_range()
}

macro_rules! immediate_signed_integer {
    ($($type:ty),+ $(,)?) => {
        $(
            impl RuntimeInteger for $type {
                fn decode(value: Word) -> Self {
                    Immediate::decode_signed(value) as Self
                }

                fn encode(self) -> Word {
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

                fn encode(self) -> Word {
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
        Immediate::decode_signed(value)
    }

    fn encode(self) -> Word {
        Immediate::signed(self).unwrap_or_else(|| Self::out_of_range())
    }
}

impl RuntimeInteger for u64 {
    fn decode(value: Word) -> Self {
        Immediate::decode_unsigned(value) as Self
    }

    fn encode(self) -> Word {
        usize::try_from(self)
            .ok()
            .and_then(Immediate::unsigned)
            .unwrap_or_else(|| Self::out_of_range())
    }
}

macro_rules! boxed_integer {
    ($host:ident, $raw:ty) => {
        struct $host;

        impl $host {
            fn decode(value: Word) -> $raw {
                OpaqueScalar::load(value) as $raw
            }

            fn encode(value: $raw, spare: *mut Word) -> Word {
                OpaqueScalar::store(spare, value as Word)
            }
        }
    };
}

boxed_integer!(HostInt64, i64);
boxed_integer!(HostUInt64, u64);

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

struct HostRoots<'a> {
    stack: RootRange,
    extra: &'a mut [*mut Word],
}

impl RootSource for HostRoots<'_> {
    fn with_roots<T>(self, trace: impl FnOnce(Roots<'_>) -> T) -> T {
        let frames = unsafe { &mut *RuntimeInstance::frames() };
        let mut slots = frames.suspension_roots();
        slots.extend(self.extra.iter().copied());
        trace(Roots { stack: self.stack, slots: &mut slots })
    }
}

struct HostString;

impl HostString {
    unsafe fn borrow<'a>(raw: Word) -> &'a str {
        unsafe { borrow_heap_str(raw) }
    }

    fn allocate_opaque(
        size_words: usize, stack_start: *mut Word, extra: &mut [*mut Word],
    ) -> *mut u8 {
        let stack_end = unsafe { *RuntimeInstance::stack_end() };
        let heap = unsafe { &mut *RuntimeInstance::heap() };
        let roots = HostRoots {
            stack: RootRange { start: stack_start, end: stack_end },
            extra,
        };
        unsafe { heap.allocate(size_words, AllocationKind::Opaque, roots) }
            .unwrap_or_else(|error| out_of_memory(error))
    }

    fn allocate_bytes(bytes: &[u8], stack_start: *mut Word, extra: &mut [*mut Word]) -> Word {
        let payload = Self::allocate_opaque(heap_string_words(bytes.len()), stack_start, extra);
        unsafe { write_heap_string(payload, bytes) };
        payload as Word
    }

    fn allocate_concat(first: &mut Word, second: &mut Word, stack_start: *mut Word) -> Word {
        let total_len = unsafe { Self::borrow(*first).len() + Self::borrow(*second).len() };
        let mut extra = [first as *mut Word, second as *mut Word];
        let payload = Self::allocate_opaque(heap_string_words(total_len), stack_start, &mut extra);
        let first_bytes = unsafe { Self::borrow(*first).as_bytes() };
        let second_bytes = unsafe { Self::borrow(*second).as_bytes() };
        unsafe { write_heap_string_concat(payload, first_bytes, second_bytes) };
        payload as Word
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

struct HostHandle;

impl HostHandle {
    fn encode(handle: Word) -> Word {
        Immediate::expect_unsigned(handle)
    }

    fn decode(value: Word) -> Word {
        Immediate::decode_unsigned(value)
    }
}

struct IoBranch;

impl IoBranch {
    fn memory(bytes: &[u8]) -> io::Result<(Word, Word)> {
        RuntimeInstance::with_memory(|memory| memory.borrow_mut().import(bytes))
            .map(|address| (address.expose(), Immediate::expect_signed(bytes.len() as i64)))
            .map_err(HostIoError::memory)
    }

    fn bytes(
        result: io::Result<(Word, Word)>, error: Word, success: Word, stack_start: *mut Word,
    ) -> Word {
        match result {
            | Ok((address, length)) => HostControl::with_two_arguments(success, address, length),
            | Err(fault) => Self::error(error, fault, stack_start),
        }
    }

    fn error(mut continuation: Word, error: io::Error, stack_start: *mut Word) -> Word {
        let kind = HostIoErrorKind::from_io_error(&error) as i64;
        let message = error.to_string();
        let owned = HostString::allocate_bytes(
            message.as_bytes(),
            stack_start,
            &mut [std::ptr::addr_of_mut!(continuation)],
        );
        HostControl::with_two_arguments(continuation, Immediate::expect_signed(kind), owned)
    }

    fn unit(
        result: io::Result<()>, when_error: Word, when_success: Word, stack_start: *mut Word,
    ) -> Word {
        match result {
            | Ok(()) => HostControl::without_arguments(when_success),
            | Err(error) => Self::error(when_error, error, stack_start),
        }
    }

    fn value(
        result: io::Result<Word>, when_error: Word, when_success: Word, stack_start: *mut Word,
    ) -> Word {
        match result {
            | Ok(value) => HostControl::with_one_argument(when_success, value),
            | Err(error) => Self::error(when_error, error, stack_start),
        }
    }
}

struct Input;

impl Input {
    fn line() -> String {
        RuntimeInstance::with_io(|runtime| {
            runtime.borrow_mut().read_process(ReaderHandle::STDIN, LineInput::read_line)
        })
        .expect("legacy standard-input read failed")
    }

    fn remaining() -> String {
        RuntimeInstance::with_io(|runtime| {
            runtime.borrow_mut().read_process(ReaderHandle::STDIN, LineInput::read_remaining)
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
    fn emit_ranges(
        mut string: Word, ranges: Option<(usize, usize, usize)>, when_none: Word,
        mut when_some: Word, stack_start: *mut Word,
    ) -> Word {
        let Some((first_len, second_start, second_len)) = ranges else {
            return HostControl::without_arguments(when_none);
        };
        let mut first_roots = [std::ptr::addr_of_mut!(string), std::ptr::addr_of_mut!(when_some)];
        let first_payload = HostString::allocate_opaque(
            heap_string_words(first_len),
            stack_start,
            &mut first_roots,
        );
        let first_slice = unsafe { &HostString::borrow(string).as_bytes()[..first_len] };
        unsafe { write_heap_string(first_payload, first_slice) };
        let mut first_word = first_payload as Word;

        let mut second_roots = [
            std::ptr::addr_of_mut!(string),
            std::ptr::addr_of_mut!(when_some),
            std::ptr::addr_of_mut!(first_word),
        ];
        let second_payload = HostString::allocate_opaque(
            heap_string_words(second_len),
            stack_start,
            &mut second_roots,
        );
        let second_slice =
            unsafe { &HostString::borrow(string).as_bytes()[second_start..second_start + second_len] };
        unsafe { write_heap_string(second_payload, second_slice) };
        HostControl::with_two_arguments(when_some, first_word, second_payload as Word)
    }
}

#[unsafe(export_name = "\x01zydeco_abort")]
extern "sysv64" fn zydeco_abort() -> ! {
    fail_runtime(RuntimeFailure::PatternMatch)
}

#[unsafe(export_name = "\x01zydeco_integer_division_by_zero")]
extern "sysv64" fn zydeco_integer_division_by_zero() -> ! {
    fail_runtime(RuntimeFailure::IntegerDivisionByZero)
}

#[unsafe(export_name = "\x01zydeco_integer_remainder_by_zero")]
extern "sysv64" fn zydeco_integer_remainder_by_zero() -> ! {
    fail_runtime(RuntimeFailure::IntegerRemainderByZero)
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
extern "sysv64" fn zydeco_frame_step(action: &'static Action<Word>, suspension: Word) -> Word {
    // SAFETY: action and trailing indices are static descriptors from matched codegen.
    unsafe { action.apply(&mut *RuntimeInstance::frames(), suspension) }
        .unwrap_or_else(|error| out_of_frames(error))
}

macro_rules! foreign_integer {
    ($type:ty, $decode:ident, $encode:ident) => {
        #[unsafe(export_name = concat!("\x01", stringify!($decode)))]
        extern "sysv64" fn $decode(value: Word) -> Word {
            <$type as RuntimeInteger>::decode(value) as Word
        }

        #[unsafe(export_name = concat!("\x01", stringify!($encode)))]
        extern "sysv64" fn $encode(value: Word) -> Word {
            // The C ABI leaves excess register bits unspecified for narrow integer results.
            <$type as RuntimeInteger>::encode(value as $type)
        }
    };
}

foreign_integer!(i8, zydeco_ffi_decode_int8, zydeco_ffi_encode_int8);
foreign_integer!(i16, zydeco_ffi_decode_int16, zydeco_ffi_encode_int16);
foreign_integer!(i32, zydeco_ffi_decode_int32, zydeco_ffi_encode_int32);
foreign_integer!(i64, zydeco_ffi_decode_int, zydeco_ffi_encode_int);
foreign_integer!(u8, zydeco_ffi_decode_uint8, zydeco_ffi_encode_uint8);
foreign_integer!(u16, zydeco_ffi_decode_uint16, zydeco_ffi_encode_uint16);
foreign_integer!(u32, zydeco_ffi_decode_uint32, zydeco_ffi_encode_uint32);
foreign_integer!(u64, zydeco_ffi_decode_uint, zydeco_ffi_encode_uint);

macro_rules! foreign_boxed_integer {
    ($host:ty, $decode:ident, $encode:ident) => {
        #[unsafe(export_name = concat!("\x01", stringify!($decode)))]
        extern "sysv64" fn $decode(value: Word) -> Word {
            <$host>::decode(value) as Word
        }

        #[unsafe(export_name = concat!("\x01", stringify!($encode)))]
        extern "sysv64" fn $encode(value: Word, spare: *mut Word) -> Word {
            <$host>::encode(value as _, spare)
        }
    };
}
foreign_boxed_integer!(HostInt64, zydeco_ffi_decode_int64, zydeco_ffi_encode_int64);
foreign_boxed_integer!(HostUInt64, zydeco_ffi_decode_uint64, zydeco_ffi_encode_uint64);

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

#[unsafe(export_name = "\x01zydeco_str_append")]
extern "sysv64" fn zydeco_str_append(
    mut first: Word, mut second: Word, stack_start: *mut Word,
) -> Word {
    HostString::allocate_concat(&mut first, &mut second, stack_start)
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

macro_rules! integer_render {
    ($type:ty, $to_string:ident => $symbol:literal) => {
        #[unsafe(export_name = $symbol)]
        extern "sysv64" fn $to_string(value: Word, stack_start: *mut Word) -> Word {
            let rendered = <$type>::decode(value).to_string();
            HostString::allocate_bytes(rendered.as_bytes(), stack_start, &mut [])
        }
    };
}

integer_render!(i8, zydeco_int8_to_string => "\x01zydeco_int8_to_string");
integer_render!(i16, zydeco_int16_to_string => "\x01zydeco_int16_to_string");
integer_render!(i32, zydeco_int32_to_string => "\x01zydeco_int32_to_string");
integer_render!(HostInt64, zydeco_int64_to_string => "\x01zydeco_int64_to_string");
integer_render!(i64, zydeco_int_to_string => "\x01zydeco_int_to_string");
integer_render!(u8, zydeco_uint8_to_string => "\x01zydeco_uint8_to_string");
integer_render!(u16, zydeco_uint16_to_string => "\x01zydeco_uint16_to_string");
integer_render!(u32, zydeco_uint32_to_string => "\x01zydeco_uint32_to_string");
integer_render!(HostUInt64, zydeco_uint64_to_string => "\x01zydeco_uint64_to_string");
integer_render!(u64, zydeco_uint_to_string => "\x01zydeco_uint_to_string");

macro_rules! float_render {
    ($type:ty, $codec:ident, $to_string:ident => $symbol:literal) => {
        #[unsafe(export_name = $symbol)]
        extern "sysv64" fn $to_string(value: Word, stack_start: *mut Word) -> Word {
            let value: $type = $codec::decode(value);
            let rendered = value.to_string();
            HostString::allocate_bytes(rendered.as_bytes(), stack_start, &mut [])
        }
    };
}

float_render!(f32, HostFloat32, zydeco_float32_to_string => "\x01zydeco_float32_to_string");
float_render!(f64, HostFloat64, zydeco_float64_to_string => "\x01zydeco_float64_to_string");

#[unsafe(export_name = "\x01zydeco_char_to_str")]
extern "sysv64" fn zydeco_char_to_str(character: Word, stack_start: *mut Word) -> Word {
    let character =
        char::from_u32(Immediate::decode_unsigned(character) as u32).expect("invalid character");
    let mut buffer = [0_u8; 4];
    let encoded = character.encode_utf8(&mut buffer);
    HostString::allocate_bytes(encoded.as_bytes(), stack_start, &mut [])
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
    match char_from_codepoint(codepoint) {
        | None => HostControl::without_arguments(when_none),
        | Some(character) => {
            HostControl::with_one_argument(when_some, Immediate::expect_unsigned(character as Word))
        }
    }
}

#[unsafe(export_name = "\x01zydeco_str_parse_int_branch")]
extern "sysv64" fn zydeco_str_parse_int_branch(
    string: Word, when_none: Word, when_some: Word,
) -> Word {
    match parse_signed_int(unsafe { HostString::borrow(string) }).and_then(Immediate::signed) {
        | None => HostControl::without_arguments(when_none),
        | Some(integer) => HostControl::with_one_argument(when_some, integer),
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
    string: Word, separator: Word, when_none: Word, when_some: Word, stack_start: *mut Word,
) -> Word {
    let separator =
        char::from_u32(Immediate::decode_unsigned(separator) as u32).expect("invalid separator");
    let ranges = split_once_byte_ranges(unsafe { HostString::borrow(string) }, separator);
    OptionalPairBranch::emit_ranges(string, ranges, when_none, when_some, stack_start)
}

#[unsafe(export_name = "\x01zydeco_str_split_at_branch")]
extern "sysv64" fn zydeco_str_split_at_branch(
    string: Word, index: Word, when_none: Word, when_some: Word, stack_start: *mut Word,
) -> Word {
    let index = <i64 as RuntimeInteger>::decode(index);
    let text = unsafe { HostString::borrow(string) };
    let ranges = split_at_scalar_byte(text, index)
        .map(|split| (split, split, text.len() - split));
    OptionalPairBranch::emit_ranges(string, ranges, when_none, when_some, stack_start)
}

/* ----------------------------------- IO ----------------------------------- */

#[unsafe(export_name = "\x01zydeco_stdin")]
extern "sysv64" fn zydeco_stdin() -> Word {
    HostHandle::encode(ReaderHandle::STDIN.raw())
}

#[unsafe(export_name = "\x01zydeco_stdout")]
extern "sysv64" fn zydeco_stdout() -> Word {
    HostHandle::encode(WriterHandle::STDOUT.raw())
}

#[unsafe(export_name = "\x01zydeco_stderr")]
extern "sysv64" fn zydeco_stderr() -> Word {
    HostHandle::encode(WriterHandle::STDERR.raw())
}

#[unsafe(export_name = "\x01zydeco_io_read")]
extern "sysv64" fn zydeco_io_read(
    reader: Word, count: Word, when_error: Word, when_success: Word, stack_start: *mut Word,
) -> Word {
    let reader = ReaderHandle::from_raw(HostHandle::decode(reader));
    let count = <i64 as RuntimeInteger>::decode(count);
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().read_process(reader, |reader| {
            let bytes = LineInput::read_up_to(reader, count)?;
            IoBranch::memory(&bytes)
        })
    });
    IoBranch::bytes(result, when_error, when_success, stack_start)
}

#[unsafe(export_name = "\x01zydeco_io_read_line")]
extern "sysv64" fn zydeco_io_read_line(
    reader: Word, when_error: Word, when_eof: Word, when_line: Word, stack_start: *mut Word,
) -> Word {
    let reader = ReaderHandle::from_raw(HostHandle::decode(reader));
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().read_process(reader, LineInput::read_line_bytes)
    });
    match result {
        | Ok((0, _)) => HostControl::without_arguments(when_eof),
        | Ok((_, bytes)) => {
            IoBranch::bytes(IoBranch::memory(&bytes), when_error, when_line, stack_start)
        }
        | Err(error) => IoBranch::error(when_error, error, stack_start),
    }
}

#[unsafe(export_name = "\x01zydeco_io_read_all")]
extern "sysv64" fn zydeco_io_read_all(
    reader: Word, when_error: Word, when_success: Word, stack_start: *mut Word,
) -> Word {
    let reader = ReaderHandle::from_raw(HostHandle::decode(reader));
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().read_process(reader, |reader| {
            let bytes = LineInput::read_all(reader)?;
            IoBranch::memory(&bytes)
        })
    });
    IoBranch::bytes(result, when_error, when_success, stack_start)
}

#[unsafe(export_name = "\x01zydeco_io_write_all")]
extern "sysv64" fn zydeco_io_write_all(
    writer: Word, address: Word, length: Word, when_error: Word, when_success: Word,
    stack_start: *mut Word,
) -> Word {
    let writer = WriterHandle::from_raw(HostHandle::decode(writer));
    let bytes = unsafe {
        memory::MemoryBranch::address(address)
            .bytes(<i64 as RuntimeInteger>::decode(length) as usize)
    };
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().write_process(writer, |writer| writer.write_all(bytes))
    });
    IoBranch::unit(result, when_error, when_success, stack_start)
}

#[unsafe(export_name = "\x01zydeco_io_flush")]
extern "sysv64" fn zydeco_io_flush(
    writer: Word, when_error: Word, when_success: Word, stack_start: *mut Word,
) -> Word {
    let writer = WriterHandle::from_raw(HostHandle::decode(writer));
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().write_process(writer, |writer| writer.flush())
    });
    IoBranch::unit(result, when_error, when_success, stack_start)
}

#[unsafe(export_name = "\x01zydeco_io_close_reader")]
extern "sysv64" fn zydeco_io_close_reader(
    reader: Word, when_error: Word, when_success: Word, stack_start: *mut Word,
) -> Word {
    let reader = ReaderHandle::from_raw(HostHandle::decode(reader));
    let result = RuntimeInstance::with_io(|runtime| runtime.borrow_mut().close_reader(reader));
    IoBranch::unit(result, when_error, when_success, stack_start)
}

#[unsafe(export_name = "\x01zydeco_io_close_writer")]
extern "sysv64" fn zydeco_io_close_writer(
    writer: Word, when_error: Word, when_success: Word, stack_start: *mut Word,
) -> Word {
    let writer = WriterHandle::from_raw(HostHandle::decode(writer));
    let result =
        RuntimeInstance::with_io(|runtime| runtime.borrow_mut().close_writer_process(writer));
    IoBranch::unit(result, when_error, when_success, stack_start)
}

#[unsafe(export_name = "\x01zydeco_fs_open_reader")]
extern "sysv64" fn zydeco_fs_open_reader(
    path: Word, when_error: Word, when_success: Word, stack_start: *mut Word,
) -> Word {
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().open_reader(unsafe { HostString::borrow(path) })
    })
    .map(|handle| HostHandle::encode(handle.raw()));
    IoBranch::value(result, when_error, when_success, stack_start)
}

#[unsafe(export_name = "\x01zydeco_fs_create_writer")]
extern "sysv64" fn zydeco_fs_create_writer(
    path: Word, when_error: Word, when_success: Word, stack_start: *mut Word,
) -> Word {
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().create_writer(unsafe { HostString::borrow(path) })
    })
    .map(|handle| HostHandle::encode(handle.raw()));
    IoBranch::value(result, when_error, when_success, stack_start)
}

#[unsafe(export_name = "\x01zydeco_fs_append_writer")]
extern "sysv64" fn zydeco_fs_append_writer(
    path: Word, when_error: Word, when_success: Word, stack_start: *mut Word,
) -> Word {
    let result = RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().append_writer(unsafe { HostString::borrow(path) })
    })
    .map(|handle| HostHandle::encode(handle.raw()));
    IoBranch::value(result, when_error, when_success, stack_start)
}

#[unsafe(export_name = "\x01zydeco_read_line")]
extern "sysv64" fn zydeco_read_line(mut continuation: Word, stack_start: *mut Word) -> Word {
    let line = Input::line();
    let owned = HostString::allocate_bytes(
        line.as_bytes(),
        stack_start,
        &mut [std::ptr::addr_of_mut!(continuation)],
    );
    HostControl::with_one_argument(continuation, owned)
}

#[unsafe(export_name = "\x01zydeco_read_line_as_int_branch")]
extern "sysv64" fn zydeco_read_line_as_int_branch(when_invalid: Word, when_valid: Word) -> Word {
    match parse_signed_int(&Input::line()).and_then(Immediate::signed) {
        | Some(integer) => HostControl::with_one_argument(when_valid, integer),
        | None => HostControl::without_arguments(when_invalid),
    }
}

#[unsafe(export_name = "\x01zydeco_read_till_eof")]
extern "sysv64" fn zydeco_read_till_eof(mut continuation: Word, stack_start: *mut Word) -> Word {
    let remaining = Input::remaining();
    let owned = HostString::allocate_bytes(
        remaining.as_bytes(),
        stack_start,
        &mut [std::ptr::addr_of_mut!(continuation)],
    );
    HostControl::with_one_argument(continuation, owned)
}

#[unsafe(export_name = "\x01zydeco_write_str")]
extern "sysv64" fn zydeco_write_str(string: Word, continuation: Word) -> Word {
    RuntimeInstance::with_io(|runtime| {
        runtime.borrow_mut().write_process(WriterHandle::STDOUT, |writer| {
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
        runtime.borrow_mut().write_process(WriterHandle::STDOUT, |writer| {
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
        runtime.borrow_mut().write_process(WriterHandle::STDOUT, |writer| {
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
        index.and_then(|index| arguments.get(index)).map(StaticHeapString::as_word)
    });
    match argument {
        | Some(argument) => HostControl::with_one_argument(when_some, argument),
        | None => HostControl::without_arguments(when_none),
    }
}

#[unsafe(export_name = "\x01zydeco_random_int")]
extern "sysv64" fn zydeco_random_int(continuation: Word) -> Word {
    let Ok(integer) = random_signed_int() else {
        let _ = writeln!(std::io::stderr().lock(), "Zydeco runtime: cannot read random source");
        std::process::exit(1);
    };
    HostControl::with_one_argument(continuation, Immediate::expect_signed(integer))
}

/* ---------------------------------- Entry --------------------------------- */

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

/// Each external entry owns all mutable language and host state. Top-level entries
/// recycle a parked instance across sequential calls so C FFI calls avoid allocating fresh
/// semispaces and handle tables.
struct RuntimeInstance {
    heap: CheneyHeap,
    frames: Frames<Growable>,
    stack_end: *mut Word,
    transfer: HostTransfer<Word>,
    memory: RefCell<RetainedMemory>,
    io: RefCell<HostIoRuntime>,
    arguments: Vec<StaticHeapString>,
    previous: *mut Self,
    guard: *const std::sync::atomic::AtomicBool,
}

struct ParkedInstance(Cell<*mut RuntimeInstance>);

impl Drop for ParkedInstance {
    fn drop(&mut self) {
        let instance = self.0.replace(std::ptr::null_mut());
        if !instance.is_null() {
            unsafe { drop(Box::from_raw(instance)) };
        }
    }
}

thread_local! {
    static CURRENT_INSTANCE: Cell<*mut RuntimeInstance> = const { Cell::new(std::ptr::null_mut()) };
    static REUSABLE_INSTANCE: ParkedInstance = const { ParkedInstance(Cell::new(std::ptr::null_mut())) };
}

impl RuntimeInstance {
    fn current() -> *mut Self {
        CURRENT_INSTANCE.with(|current| {
            let instance = current.get();
            assert!(!instance.is_null(), "native runtime helper outside an entry");
            instance
        })
    }

    fn heap() -> *mut CheneyHeap {
        unsafe { &raw mut (*Self::current()).heap }
    }

    fn frames() -> *mut Frames<Growable> {
        unsafe { &raw mut (*Self::current()).frames }
    }

    fn stack_end() -> *mut *mut Word {
        unsafe { &raw mut (*Self::current()).stack_end }
    }

    fn transfer() -> *mut HostTransfer<Word> {
        unsafe { &raw mut (*Self::current()).transfer }
    }

    fn with_memory<T>(f: impl FnOnce(&RefCell<RetainedMemory>) -> T) -> T {
        f(unsafe { &(*Self::current()).memory })
    }

    fn with_io<T>(f: impl FnOnce(&RefCell<HostIoRuntime>) -> T) -> T {
        f(unsafe { &(*Self::current()).io })
    }

    fn with_arguments<T>(f: impl FnOnce(&[StaticHeapString]) -> T) -> T {
        f(unsafe { &(*Self::current()).arguments })
    }
}

zydeco_machine::export_native_entry! {
/// Acquire the unit guard and prepare an isolated instance before any source initialization.
extern "sysv64" fn entry_begin(stack_end: *mut Word, guard: *const std::sync::atomic::AtomicBool) {
    use std::sync::atomic::Ordering;
    if !guard.is_null() && unsafe { &*guard }.compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed).is_err() {
        eprintln!("Zydeco runtime: concurrent or reentrant entry into an active compiled library");
        std::process::exit(1);
    }
    let previous = CURRENT_INSTANCE.with(Cell::get);
    let reused = REUSABLE_INSTANCE.with(|parked| parked.0.replace(std::ptr::null_mut()));
    let instance = if reused.is_null() {
        Box::into_raw(Box::new(RuntimeInstance {
            heap: CheneyHeap::new(),
            frames: Frames::EMPTY,
            stack_end,
            transfer: HostTransfer { resume: 0, closure: 0, first: 0, second: 0 },
            memory: RefCell::new(RetainedMemory::default()),
            io: RefCell::new(HostIoRuntime::new()),
            arguments: Vec::new(),
            previous,
            guard,
        }))
    } else {
        unsafe {
            let instance = &mut *reused;
            instance.stack_end = stack_end;
            instance.transfer = HostTransfer { resume: 0, closure: 0, first: 0, second: 0 };
            instance.previous = previous;
            instance.guard = guard;
        }
        reused
    };
    CURRENT_INSTANCE.with(|current| current.set(instance));
}
}

#[unsafe(export_name = "\x01zydeco_entry_end")]
extern "sysv64" fn entry_end() {
    use std::sync::atomic::Ordering;
    let instance_ptr = RuntimeInstance::current();
    let (previous, guard) = unsafe { ((*instance_ptr).previous, (*instance_ptr).guard) };
    CURRENT_INSTANCE.with(|current| current.set(previous));

    unsafe {
        let instance = &mut *instance_ptr;
        instance.heap.reset();
        instance.frames.reset();
        instance.memory.borrow_mut().clear();
        instance.io.borrow_mut().reset();
        instance.arguments.clear();
        instance.previous = std::ptr::null_mut();
        instance.guard = std::ptr::null();
    }

    let parked = REUSABLE_INSTANCE.with(|slot| {
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
#[unsafe(export_name = "\x01zydeco_run_process")]
pub extern "C" fn zydeco_run_process() {
    run_process();
}

#[cfg(feature = "process-entry")]
pub fn run_process() {
    let stack_anchor: Word = 0;
    entry_begin(std::ptr::addr_of!(stack_anchor).cast_mut(), std::ptr::null());
    let arguments = std::env::args().skip(1).map(|arg| StaticHeapString::new(&arg)).collect();
    unsafe {
        (*RuntimeInstance::current()).arguments = arguments;
        zydeco_machine::native::entry();
    }
    entry_end();
}

#[unsafe(export_name = "\x01zydeco_int64_from_int")]
extern "sysv64" fn int64_from_int(value: Word, spare: *mut Word) -> Word {
    HostInt64::encode(<i64 as RuntimeInteger>::decode(value), spare)
}

#[unsafe(export_name = "\x01zydeco_uint64_from_uint")]
extern "sysv64" fn uint64_from_uint(value: Word, spare: *mut Word) -> Word {
    HostUInt64::encode(<u64 as RuntimeInteger>::decode(value), spare)
}

#[unsafe(export_name = "\x01zydeco_int64_to_int_branch")]
extern "sysv64" fn int64_to_int(value: Word, when_none: Word, when_some: Word) -> Word {
    match Immediate::signed(HostInt64::decode(value)) {
        | Some(value) => HostControl::with_one_argument(when_some, value),
        | None => HostControl::without_arguments(when_none),
    }
}

#[unsafe(export_name = "\x01zydeco_uint64_to_uint_branch")]
extern "sysv64" fn uint64_to_uint(value: Word, when_none: Word, when_some: Word) -> Word {
    match Immediate::unsigned(HostUInt64::decode(value) as Word) {
        | Some(value) => HostControl::with_one_argument(when_some, value),
        | None => HostControl::without_arguments(when_none),
    }
}

#[cfg(test)]
mod entry_tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, Ordering};
    use zydeco_host::gc::DEFAULT_INITIAL_SPACE_BYTES;

    #[test]
    fn entry_marshalling_roots_boxes_without_tracing_their_payloads() {
        let mut values = [1; 6];
        let start = values.as_mut_ptr();
        entry_begin(unsafe { start.add(values.len()) }, std::ptr::null());
        let mut bits = [0, 1, i64::MIN as Word, Word::MAX, 0, 0];
        for index in 0..values.len() {
            // These numeric payloads deliberately coincide with managed pointers.
            if index >= 4 {
                bits[index] = values[0] + (index - 4) * 8;
            }
            let spare = entry_box(start);
            values[index] = HostUInt64::encode(bits[index] as u64, spare);
        }
        // Force collections past the initial 64 KiB semispace during marshalling.
        for _ in 0..DEFAULT_INITIAL_SPACE_BYTES / 8 {
            let spare = entry_box(start);
            HostUInt64::encode(0, spare);
        }
        for (value, expected) in values.into_iter().zip(bits) {
            assert_eq!(HostUInt64::decode(value), expected as u64);
            assert_eq!(HostInt64::decode(value), expected as i64);
        }
        entry_end();
    }

    #[test]
    fn independent_entries_restore_caller_and_sequential_entries_reuse_instance() {
        let first = AtomicBool::new(false);
        let second = AtomicBool::new(false);
        let mut anchor: Word = 0;
        entry_begin(&mut anchor, &first);
        let caller = RuntimeInstance::current();
        let caller_heap = RuntimeInstance::heap();
        let caller_str = HostString::allocate_bytes(b"caller state", &mut anchor, &mut []);
        let caller_arg = StaticHeapString::new("caller-arg");
        unsafe {
            (*caller).arguments.push(caller_arg);
        }

        entry_begin(&mut anchor, &second);
        let callee = RuntimeInstance::current();
        assert_ne!(callee, caller);
        assert_ne!(RuntimeInstance::heap(), caller_heap);
        RuntimeInstance::with_arguments(|arguments| assert!(arguments.is_empty()));
        assert_eq!(unsafe { (*RuntimeInstance::heap()).used_bytes() }, 0);
        let _ = HostString::allocate_bytes(b"callee state", &mut anchor, &mut []);
        entry_end();

        assert!(!second.load(Ordering::Relaxed));
        assert!(first.load(Ordering::Relaxed));
        assert_eq!(RuntimeInstance::current(), caller);
        RuntimeInstance::with_arguments(|arguments| {
            assert_eq!(arguments.len(), 1);
            assert_eq!(unsafe { HostString::borrow(arguments[0].as_word()) }, "caller-arg");
        });
        assert_eq!(unsafe { HostString::borrow(caller_str) }, "caller state");
        entry_end();

        assert!(!first.load(Ordering::Relaxed));
        assert!(CURRENT_INSTANCE.with(Cell::get).is_null());

        entry_begin(&mut anchor, &first);
        let reused = RuntimeInstance::current();
        assert_eq!(reused, callee);
        assert_eq!(unsafe { (*RuntimeInstance::heap()).used_bytes() }, 0);
        RuntimeInstance::with_arguments(|arguments| assert!(arguments.is_empty()));
        entry_end();

        entry_begin(&mut anchor, &first);
        assert_eq!(RuntimeInstance::current(), reused);
        entry_end();
    }
}
