#[export_name = "\x01zydeco_abort"]
extern "sysv64" fn zydeco_abort() {
    panic!("Aborted");
}

#[export_name = "\x01zydeco_alloc"]
extern "sysv64" fn zydeco_alloc(size: usize) -> *mut u8 {
    let ptr = unsafe { HEAP.as_mut_ptr().add(HEAP_SIZE) };
    unsafe { HEAP_SIZE += size * 8; }
    ptr
}

#[link(name = "zyprog", kind = "static")]
extern "sysv64" {
    #[link_name = "\x01entry"]
    fn entry(env: *mut u8, heap: *mut u8) -> i64;
}

static mut ENV: [u8; 1024 * 1024] = [0; 1024 * 1024];
static mut HEAP: [u8; 1024 * 1024] = [0; 1024 * 1024];
static mut HEAP_SIZE: usize = 0;

fn main() {
    let output = unsafe { entry(ENV.as_mut_ptr(), HEAP.as_mut_ptr()) };
    println!("{}", output);
}
