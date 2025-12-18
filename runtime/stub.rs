use std::cell::RefCell;

#[unsafe(export_name = "\x01zydeco_abort")]
extern "sysv64" fn zydeco_abort() {
    panic!("Aborted");
}

#[unsafe(export_name = "\x01zydeco_alloc")]
extern "sysv64" fn zydeco_alloc(size: usize) -> *mut u8 {
    HEAP.with(|heap| {
        HEAP_SIZE.with(|heap_size| {
            let mut heap_size_ref = heap_size.borrow_mut();
            let ptr = unsafe { heap.borrow_mut().as_mut_ptr().add(*heap_size_ref) };
            *heap_size_ref += size * 8;
            ptr
        })
    })
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
    static ENV: RefCell<[u8; 1024 * 1024]> = RefCell::new([0; 1024 * 1024]);
    static HEAP: RefCell<[u8; 1024 * 1024]> = RefCell::new([0; 1024 * 1024]);
    static HEAP_SIZE: RefCell<usize> = RefCell::new(0);
}

fn main() {
    ENV.with(|env| {
        HEAP.with(|heap| {
            let mut env_ref = env.borrow_mut();
            let mut heap_ref = heap.borrow_mut();
            let output = unsafe { entry(env_ref.as_mut_ptr(), heap_ref.as_mut_ptr()) };
            println!("{}", output);
        })
    });
}
