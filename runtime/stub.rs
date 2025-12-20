use std::cell::UnsafeCell;

#[unsafe(export_name = "\x01zydeco_abort")]
extern "sysv64" fn zydeco_abort() {
    panic!("Aborted");
}

#[unsafe(export_name = "\x01zydeco_alloc")]
extern "sysv64" fn zydeco_alloc(size: usize) -> *mut u8 {
    HEAP.with(|heap| {
        HEAP_SIZE.with(|heap_size| unsafe {
            let heap_ptr = heap.get();
            let heap_size_ptr = heap_size.get();
            let ptr = (*heap_ptr).as_mut_ptr().add(*heap_size_ptr);
            *heap_size_ptr += size * 8 * 8;
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
    let mut line = String::new();
    std::io::stdin().read_line(&mut line).unwrap();
    line.pop();
    unsafe {
        let env: *mut u8 = std::mem::transmute(*kont);
        let code: fn(*mut u8) -> fn(Box<String>) = std::mem::transmute(*kont.add(8));
        (code(env))(Box::new(line))
    }
}

#[unsafe(export_name = "\x01zydeco_write_line")]
extern "sysv64" fn zydeco_write_line(line: Box<String>, kont: *mut *mut u8) {
    println!("{}", line);
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

thread_local! {
    static ENV: UnsafeCell<[u8; 1024 * 1024]> = UnsafeCell::new([0; 1024 * 1024]);
    static HEAP: UnsafeCell<[u8; 1024 * 1024]> = UnsafeCell::new([0; 1024 * 1024]);
    static HEAP_SIZE: UnsafeCell<usize> = UnsafeCell::new(0);
}

fn main() {
    ENV.with(|env| {
        HEAP.with(|heap| unsafe {
            let env_ptr = (*env.get()).as_mut_ptr();
            let heap_ptr = (*heap.get()).as_mut_ptr();
            let output = entry(env_ptr, heap_ptr);
            println!("{}", output);
        })
    });
}
