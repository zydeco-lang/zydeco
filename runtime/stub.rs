use std::{
    cell::UnsafeCell,
    io::{BufRead, Read, Write},
};

type Word = usize;

#[repr(C)]
struct ZydecoClosure {
    environment: *mut u8,
    code: *mut u8,
}

#[repr(C)]
struct ControlTransfer {
    resume: *mut u8,
    closure: Word,
    first: Word,
    second: Word,
}

impl ControlTransfer {
    fn without_arguments(closure: Word) -> Word {
        Self::leak(rust_resume_zydeco_0, closure, 0, 0)
    }

    fn with_one_argument(closure: Word, argument: Word) -> Word {
        Self::leak(rust_resume_zydeco_1, closure, argument, 0)
    }

    fn with_two_arguments(closure: Word, first: Word, second: Word) -> Word {
        Self::leak(rust_resume_zydeco_2, closure, first, second)
    }

    fn leak(resume: unsafe extern "sysv64" fn(), closure: Word, first: Word, second: Word) -> Word {
        let resume = resume as *const () as *mut u8;
        Box::into_raw(Box::new(Self { resume, closure, first, second })) as Word
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

struct Input;

impl Input {
    fn line() -> String {
        let mut line = String::new();
        std::io::stdin().lock().read_line(&mut line).unwrap();
        line.truncate(line.trim_end_matches(['\r', '\n']).len());
        line
    }

    fn remaining() -> String {
        let mut input = String::new();
        std::io::stdin().lock().read_to_string(&mut input).unwrap();
        input
    }
}

struct Branch;

impl Branch {
    fn select(condition: bool, when_true: Word, when_false: Word) -> Word {
        ControlTransfer::without_arguments(if condition { when_true } else { when_false })
    }
}

struct OptionalPairBranch;

impl OptionalPairBranch {
    fn select(pair: Option<(String, String)>, when_none: Word, when_some: Word) -> Word {
        match pair {
            | None => ControlTransfer::without_arguments(when_none),
            | Some((first, second)) => ControlTransfer::with_two_arguments(
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
    arguments: std::vec::IntoIter<String>,
    when_empty: Word,
    when_item: Word,
}

impl ArgumentFold {
    fn from_process(when_empty: Word, when_item: Word) -> Self {
        let arguments = std::env::args().skip(1).collect::<Vec<_>>().into_iter();
        Self { arguments, when_empty, when_item }
    }

    fn into_thunk(self) -> Word {
        let environment = Box::into_raw(Box::new(self)).cast::<u8>();
        let code = rust_arg_fold_tail as *const () as *mut u8;
        Box::into_raw(Box::new(ZydecoClosure { environment, code })) as Word
    }

    unsafe fn from_environment(environment: *mut u8) -> Box<Self> {
        unsafe { Box::from_raw(environment.cast::<Self>()) }
    }

    fn resume(mut self) -> Word {
        match self.arguments.next() {
            | None => ControlTransfer::without_arguments(self.when_empty),
            | Some(argument) => {
                let when_item = self.when_item;
                let tail = self.into_thunk();
                ControlTransfer::with_two_arguments(when_item, HostString::leak(argument), tail)
            }
        }
    }
}

#[unsafe(export_name = "\x01zydeco_abort")]
extern "sysv64" fn zydeco_abort() -> ! {
    std::process::abort()
}

#[unsafe(export_name = "\x01zydeco_alloc")]
extern "sysv64" fn zydeco_alloc(size: usize) -> *mut u8 {
    HEAP.with(|heap| {
        HEAP_SIZE.with(|heap_size| unsafe {
            let heap_ptr = *heap.get();
            let heap_size_ptr = heap_size.get();
            let ptr = heap_ptr.add(*heap_size_ptr);
            assert!(
                ptr as usize % ALIGNMENT == 0,
                "allocated pointer is not aligned to {}-byte boundary",
                ALIGNMENT
            );
            *heap_size_ptr += size * 8;
            assert!(*heap_size_ptr <= BUFFER_SIZE, "Zydeco heap exhausted");
            ptr
        })
    })
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
extern "sysv64" fn zydeco_exit(code: i64) -> ! {
    std::process::exit(code as i32);
}

/* ---------------------------------- Pure ---------------------------------- */

#[unsafe(export_name = "\x01zydeco_str_length")]
extern "sysv64" fn zydeco_str_length(string: Word) -> i64 {
    unsafe { HostString::borrow(string) }.chars().count() as i64
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

#[unsafe(export_name = "\x01zydeco_str_index")]
extern "sysv64" fn zydeco_str_index(string: Word, index: i64) -> Word {
    let index = usize::try_from(index).expect("negative string index");
    unsafe { HostString::borrow(string) }.chars().nth(index).expect("string index out of bounds")
        as Word
}

#[unsafe(export_name = "\x01zydeco_int_to_str")]
extern "sysv64" fn zydeco_int_to_str(integer: i64) -> Word {
    HostString::leak(integer.to_string())
}

#[unsafe(export_name = "\x01zydeco_char_to_str")]
extern "sysv64" fn zydeco_char_to_str(character: Word) -> Word {
    let character = char::from_u32(character as u32).expect("invalid character");
    HostString::leak(character.to_string())
}

#[unsafe(export_name = "\x01zydeco_char_to_int")]
extern "sysv64" fn zydeco_char_to_int(character: Word) -> i64 {
    character as i64
}

#[unsafe(export_name = "\x01zydeco_str_to_int")]
extern "sysv64" fn zydeco_str_to_int(string: Word) -> i64 {
    unsafe { HostString::borrow(string) }.parse().expect("invalid integer")
}

/* -------------------------------- Branches -------------------------------- */

macro_rules! integer_branch {
    ($name:ident, $symbol:literal, $operation:tt) => {
        #[unsafe(export_name = $symbol)]
        extern "sysv64" fn $name(
            first: i64, second: i64, when_true: Word, when_false: Word,
        ) -> Word {
            Branch::select(first $operation second, when_true, when_false)
        }
    };
}

integer_branch!(zydeco_int_eq_branch, "\x01zydeco_int_eq_branch", ==);
integer_branch!(zydeco_int_lt_branch, "\x01zydeco_int_lt_branch", <);
integer_branch!(zydeco_int_gt_branch, "\x01zydeco_int_gt_branch", >);

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
    let separator = char::from_u32(separator as u32).expect("invalid separator");
    let pair =
        string.split_once(separator).map(|(first, second)| (first.to_string(), second.to_string()));
    OptionalPairBranch::select(pair, when_none, when_some)
}

#[unsafe(export_name = "\x01zydeco_str_split_n_branch")]
extern "sysv64" fn zydeco_str_split_n_branch(
    string: Word, index: i64, when_none: Word, when_some: Word,
) -> Word {
    let pair = OptionalPairBranch::split_at(unsafe { HostString::borrow(string) }, index);
    OptionalPairBranch::select(pair, when_none, when_some)
}

/* ----------------------------------- IO ----------------------------------- */

#[unsafe(export_name = "\x01zydeco_read_line")]
extern "sysv64" fn zydeco_read_line(continuation: Word) -> Word {
    let line = Input::line();
    ControlTransfer::with_one_argument(continuation, HostString::leak(line))
}

#[unsafe(export_name = "\x01zydeco_read_line_as_int")]
extern "sysv64" fn zydeco_read_line_as_int(continuation: Word) -> Word {
    let integer = Input::line().parse::<i64>().expect("invalid integer");
    ControlTransfer::with_one_argument(continuation, integer as Word)
}

#[unsafe(export_name = "\x01zydeco_read_line_as_int_branch")]
extern "sysv64" fn zydeco_read_line_as_int_branch(when_invalid: Word, when_valid: Word) -> Word {
    match Input::line().parse::<i64>() {
        | Ok(integer) => ControlTransfer::with_one_argument(when_valid, integer as Word),
        | Err(_) => ControlTransfer::without_arguments(when_invalid),
    }
}

#[unsafe(export_name = "\x01zydeco_read_till_eof")]
extern "sysv64" fn zydeco_read_till_eof(continuation: Word) -> Word {
    ControlTransfer::with_one_argument(continuation, HostString::leak(Input::remaining()))
}

#[unsafe(export_name = "\x01zydeco_write_str")]
extern "sysv64" fn zydeco_write_str(string: Word, continuation: Word) -> Word {
    let mut stdout = std::io::stdout().lock();
    stdout.write_all(unsafe { HostString::borrow(string) }.as_bytes()).unwrap();
    stdout.flush().unwrap();
    ControlTransfer::without_arguments(continuation)
}

#[unsafe(export_name = "\x01zydeco_write_int")]
extern "sysv64" fn zydeco_write_int(integer: i64, continuation: Word) -> Word {
    let mut stdout = std::io::stdout().lock();
    write!(stdout, "{integer}").unwrap();
    stdout.flush().unwrap();
    ControlTransfer::without_arguments(continuation)
}

#[unsafe(export_name = "\x01zydeco_write_line")]
extern "sysv64" fn zydeco_write_line(line: Word, continuation: Word) -> Word {
    let mut stdout = std::io::stdout().lock();
    writeln!(stdout, "{}", unsafe { HostString::borrow(line) }).unwrap();
    stdout.flush().unwrap();
    ControlTransfer::without_arguments(continuation)
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
extern "sysv64" fn zydeco_random_int(continuation: Word) -> Word {
    use rand::RngExt;
    let integer = rand::rng().random_range(i64::MIN..=i64::MAX);
    ControlTransfer::with_one_argument(continuation, integer as Word)
}

/* ---------------------------------- Entry --------------------------------- */

unsafe extern "sysv64" {
    #[link_name = "\x01entry"]
    fn entry(environment: *mut u8) -> i64;
}

const BUFFER_SIZE: usize = 1024 * 1024;
const ALIGNMENT: usize = 8;

thread_local! {
    static ENV: UnsafeCell<*mut u8> = UnsafeCell::new(init_buffer());
    static HEAP: UnsafeCell<*mut u8> = UnsafeCell::new(init_buffer());
    static HEAP_SIZE: UnsafeCell<usize> = const { UnsafeCell::new(0) };
}

fn init_buffer() -> *mut u8 {
    use std::alloc::{Layout, alloc};
    unsafe {
        let layout = Layout::from_size_align(BUFFER_SIZE, ALIGNMENT).unwrap();
        let pointer = alloc(layout);
        pointer.write_bytes(0, BUFFER_SIZE);
        pointer
    }
}

fn main() {
    ENV.with(|environment| {
        unsafe {
            let environment = *environment.get();
            entry(environment);
        }
    });
}
