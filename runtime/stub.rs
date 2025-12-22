#[unsafe(export_name = "\x01zydeco_abort")]
extern "sysv64" fn zydeco_abort() {
    panic!("Aborted");
}

#[unsafe(export_name = "\x01zydeco_alloc")]
extern "sysv64" fn zydeco_alloc(size: usize) -> *mut u8 {
    HEAP.with(|heap| {
        HEAP_SIZE.with(|heap_size| unsafe {
            println!("[zydeco_alloc]");
            let heap_ptr = *heap.get();
            // align the heap pointer to the next 8-byte boundary
            // this line should not be needed
            // *heap_ptr += *heap_ptr % 8;
            assert_eq!(*heap_ptr % 8, 0, "heap pointer is not aligned to 8-byte boundary");
            let heap_size_ptr = heap_size.get();
            let ptr = heap_ptr.add(*heap_size_ptr);
            *heap_size_ptr += size * 8;
            println!(
                "[zydeco_alloc] ptr: {:p}, heap_ptr: {:p}, heap_size: 0x{:x}",
                ptr, heap_ptr, *heap_size_ptr
            );
            ptr
        })
    })
}

#[unsafe(export_name = "\x01zydeco_exit")]
extern "sysv64" fn zydeco_exit(code: i64) {
    std::process::exit(code as i32);
}

#[unsafe(export_name = "\x01zydeco_read_line")]
extern "sysv64" fn zydeco_read_line(kont: *mut *mut u8) {
    println!("[zydeco_read_line]");
    let mut line = String::new();
    {
        use std::io::BufRead;
        let mut stdin = std::io::stdin().lock();
        stdin.read_line(&mut line).unwrap();
    }
    line.pop();
    println!("[zydeco_read_line] line: {}", line);
    unsafe {
        let env: *mut u8 = std::mem::transmute(*kont);
        let code: fn(*mut u8) -> fn(Box<String>) = std::mem::transmute(*kont.add(8));
        (code(env))(Box::new(line))
    }
}

#[unsafe(export_name = "\x01zydeco_write_line")]
extern "sysv64" fn zydeco_write_line(line: Box<String>, kont: *mut *mut u8) {
    println!("[zydeco_write_line]");
    {
        use std::io::Write;
        let mut stdout = std::io::stdout();
        stdout.write_all(line.as_bytes()).unwrap();
        stdout.flush().unwrap();
    }
    unsafe {
        let env: *mut u8 = std::mem::transmute(*kont);
        let code: fn(*mut u8) = std::mem::transmute(*kont.add(8));
        code(env)
    }
}

unsafe extern "sysv64" {
    #[link_name = "\x01entry"]
    fn entry(env: *mut u8, heap: *mut u8) -> i64;
}

const BUFFER_SIZE: usize = 1024 * 1024;
const ALIGNMENT: usize = 8;

use std::cell::UnsafeCell;
thread_local! {
    static ENV: UnsafeCell<*mut u8> = UnsafeCell::new(init_buffer());
    static HEAP: UnsafeCell<*mut u8> = UnsafeCell::new(init_buffer());
    static HEAP_SIZE: UnsafeCell<usize> = UnsafeCell::new(0);
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
    println!("[main]");
    ENV.with(|env| {
        HEAP.with(|heap| unsafe {
            let env_ptr = *env.get();
            let heap_ptr = *heap.get();
            println!("[env_ptr: {:p}, heap_ptr: {:p}]", env_ptr, heap_ptr);
            let output = entry(env_ptr, heap_ptr);
            println!("{}", output);
        })
    });
}
