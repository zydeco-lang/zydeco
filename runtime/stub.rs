#[unsafe(export_name = "\x01zydeco_abort")]
extern "sysv64" fn zydeco_abort() {
    panic!("Aborted");
}

#[unsafe(export_name = "\x01zydeco_alloc")]
extern "sysv64" fn zydeco_alloc(size: usize) -> *mut u8 {
    HEAP.with(|heap| {
        HEAP_SIZE.with(|heap_size| unsafe {
            #[cfg(feature = "log_rt")]
            log::trace!("[zydeco_alloc]");
            let heap_ptr = *heap.get();
            let heap_size_ptr = heap_size.get();
            let ptr = heap_ptr.add(*heap_size_ptr);
            // check that the allocated pointer is aligned
            assert!(
                // Fixme: should be correct but somehow not working
                // (ptr as usize).is_multiple_of(ALIGNMENT),
                ptr as usize % ALIGNMENT == 0,
                "allocated pointer is not aligned to {}-byte boundary",
                ALIGNMENT
            );
            *heap_size_ptr += size * 8;
            #[cfg(feature = "log_rt")]
            log::trace!(
                "[zydeco_alloc] ptr: {:p}, heap_ptr: {:p}, heap_size: 0x{:x}",
                ptr,
                heap_ptr,
                *heap_size_ptr
            );
            ptr
        })
    })
}

unsafe extern "sysv64" {
    #[link_name = "\x01rust_call_zydeco_0"]
    fn rust_call_zydeco_0(code: *mut u8, env: *mut u8);
    #[link_name = "\x01rust_call_zydeco_1"]
    fn rust_call_zydeco_1(code: *mut u8, env: *mut u8, arg0: *mut u8);
    // variadic function call in C-style and is FFI-safe
    // #[link_name = "\x01rust_call_zydeco"]
    // fn rust_call_zydeco(code: *mut u8, env: *mut u8, num: usize, args: *const *mut u8);
}

unsafe fn kont_call_zydeco_0(kont: *mut *mut u8) {
    unsafe {
        let env: *mut u8 = std::mem::transmute(*kont);
        let code: *mut u8 = std::mem::transmute(*kont.add(1));
        #[cfg(feature = "log_rt")]
        log::trace!("[kont_call_zydeco_0] kont: {:p}, env: {:p}, code: {:p}", kont, env, code);
        rust_call_zydeco_0(code, env)
    }
}

unsafe fn kont_call_zydeco_1(kont: *mut *mut u8, arg0: *mut u8) {
    unsafe {
        let env: *mut u8 = std::mem::transmute(*kont);
        let code: *mut u8 = std::mem::transmute(*kont.add(1));
        let arg0: *mut u8 = std::mem::transmute(arg0);
        #[cfg(feature = "log_rt")]
        log::trace!(
            "[kont_call_zydeco_1] kont: {:p}, env: {:p}, code: {:p}, arg0: {:p}",
            kont,
            env,
            code,
            arg0
        );
        rust_call_zydeco_1(code, env, arg0)
    }
}

#[unsafe(export_name = "\x01zydeco_exit")]
extern "sysv64" fn zydeco_exit(code: i64) {
    std::process::exit(code as i32);
}

/* ----------------------------------- IO ----------------------------------- */

#[unsafe(export_name = "\x01zydeco_read_line")]
extern "sysv64" fn zydeco_read_line(kont: *mut *mut u8) {
    #[cfg(feature = "log_rt")]
    log::trace!("[zydeco_read_line]");
    let mut line = String::new();
    {
        use std::io::BufRead;
        let mut stdin = std::io::stdin().lock();
        stdin.read_line(&mut line).unwrap();
    }
    line.pop();
    #[cfg(feature = "log_rt")]
    log::trace!("[zydeco_read_line] line: {}", line);
    unsafe { kont_call_zydeco_1(kont, std::mem::transmute(Box::new(line))) }
}

#[unsafe(export_name = "\x01zydeco_read_line_as_int")]
extern "sysv64" fn zydeco_read_line_as_int(kont: *mut *mut u8) {
    #[cfg(feature = "log_rt")]
    log::trace!("[zydeco_read_line_as_int]");
    let mut line = String::new();
    {
        use std::io::BufRead;
        let mut stdin = std::io::stdin().lock();
        stdin.read_line(&mut line).unwrap();
    }
    line.pop();
    #[cfg(feature = "log_rt")]
    log::trace!("[zydeco_read_line_as_int] line: {}", line);
    let int = line.parse::<i64>().unwrap();
    unsafe { kont_call_zydeco_1(kont, std::mem::transmute(&int)) }
}

#[unsafe(export_name = "\x01zydeco_write_str")]
extern "sysv64" fn zydeco_write_str(str: Box<String>, kont: *mut *mut u8) {
    #[cfg(feature = "log_rt")]
    log::trace!("[zydeco_write_str]");
    {
        use std::io::Write;
        let mut stdout = std::io::stdout();
        stdout.write_all(str.as_bytes()).unwrap();
        stdout.flush().unwrap();
    }
    unsafe { kont_call_zydeco_0(kont) }
}

#[unsafe(export_name = "\x01zydeco_write_int")]
extern "sysv64" fn zydeco_write_int(int: i64, kont: *mut *mut u8) {
    #[cfg(feature = "log_rt")]
    log::trace!("[zydeco_write_str]");
    {
        use std::io::Write;
        let mut stdout = std::io::stdout();
        stdout.write_all(int.to_string().as_bytes()).unwrap();
        stdout.flush().unwrap();
    }
    unsafe { kont_call_zydeco_0(kont) }
}

#[unsafe(export_name = "\x01zydeco_write_line")]
extern "sysv64" fn zydeco_write_line(line: Box<String>, kont: *mut *mut u8) {
    #[cfg(feature = "log_rt")]
    log::trace!("[zydeco_write_line]");
    {
        use std::io::Write;
        let mut stdout = std::io::stdout();
        stdout.write_all(line.as_bytes()).unwrap();
        stdout.write_all(b"\n").unwrap();
        stdout.flush().unwrap();
    }
    unsafe { kont_call_zydeco_0(kont) }
}

#[unsafe(export_name = "\x01zydeco_random_int")]
extern "sysv64" fn zydeco_random_int(kont: *mut *mut u8) {
    #[cfg(feature = "log_rt")]
    log::trace!("[zydeco_random_int]");
    use rand::RngExt;
    let int = rand::rng().random_range(i64::MIN..=i64::MAX);
    unsafe { kont_call_zydeco_1(kont, std::mem::transmute(&int)) }
}

/* --------------------------------- String --------------------------------- */

// extern "sysv64" fn zydeco_str_length(str: Box<String>) -> i64 {
//     str.len() as i64
// }

/* ---------------------------------- Entry --------------------------------- */

unsafe extern "sysv64" {
    #[link_name = "\x01entry"]
    fn entry(env: *mut u8) -> i64;
}

const BUFFER_SIZE: usize = 1024 * 1024;
const ALIGNMENT: usize = 8;

use std::cell::UnsafeCell;
thread_local! {
    static ENV: UnsafeCell<*mut u8> = UnsafeCell::new(init_buffer());
    static HEAP: UnsafeCell<*mut u8> = UnsafeCell::new(init_buffer());
    static HEAP_SIZE: UnsafeCell<usize> = const { UnsafeCell::new(0) };
}

fn init_buffer() -> *mut u8 {
    use std::alloc::{Layout, alloc};
    unsafe {
        let layout = Layout::from_size_align(BUFFER_SIZE, ALIGNMENT).unwrap();
        let ptr = alloc(layout);
        ptr.write_bytes(0, BUFFER_SIZE);
        ptr
    }
}

fn main() {
    #[cfg(feature = "log_rt")]
    env_logger::init();
    #[cfg(feature = "log_rt")]
    log::trace!("[main]");
    ENV.with(|env| {
        HEAP.with(|heap| unsafe {
            let env_ptr = *env.get();
            let _heap_ptr = *heap.get();
            #[cfg(feature = "log_rt")]
            log::trace!("[env_ptr: {:p}, heap_ptr: {:p}]", env_ptr, _heap_ptr);
            let output = entry(env_ptr);
            println!("{}", output);
        })
    });
}
