#[path = "assembly_bench/common.rs"]
mod common;

use common::{AllocationStats, Experiment};
use std::{
    alloc::{GlobalAlloc, Layout, System},
    sync::atomic::{AtomicBool, AtomicUsize, Ordering::Relaxed},
};

struct Counted;
#[global_allocator]
static ALLOCATOR: Counted = Counted;
static ENABLED: AtomicBool = AtomicBool::new(false);
static LIVE: AtomicUsize = AtomicUsize::new(0);
static BASE: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);
static ALLOCS: AtomicUsize = AtomicUsize::new(0);
static REALLOCS: AtomicUsize = AtomicUsize::new(0);
static BYTES: AtomicUsize = AtomicUsize::new(0);

impl Counted {
    fn allocated(size: usize) {
        let live = LIVE.fetch_add(size, Relaxed) + size;
        if ENABLED.load(Relaxed) {
            ALLOCS.fetch_add(1, Relaxed);
            BYTES.fetch_add(size, Relaxed);
            PEAK.fetch_max(live, Relaxed);
        }
    }
    fn begin() {
        let base = LIVE.load(Relaxed);
        BASE.store(base, Relaxed);
        PEAK.store(base, Relaxed);
        ALLOCS.store(0, Relaxed);
        REALLOCS.store(0, Relaxed);
        BYTES.store(0, Relaxed);
        ENABLED.store(true, Relaxed);
    }
    fn finish() -> AllocationStats {
        ENABLED.store(false, Relaxed);
        AllocationStats {
            allocs: ALLOCS.load(Relaxed),
            reallocs: REALLOCS.load(Relaxed),
            requested_bytes: BYTES.load(Relaxed),
            peak_extra_bytes: PEAK.load(Relaxed).saturating_sub(BASE.load(Relaxed)),
        }
    }
}

// SAFETY: all operations delegate to System with the original layout and pointer.
// The atomics observe sizes without allocating or modifying the returned storage.
unsafe impl GlobalAlloc for Counted {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        if !pointer.is_null() {
            Self::allocated(layout.size());
        }
        pointer
    }
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        if !pointer.is_null() {
            Self::allocated(layout.size());
        }
        pointer
    }
    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        LIVE.fetch_sub(layout.size(), Relaxed);
        unsafe { System.dealloc(pointer, layout) };
    }
    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        let pointer = unsafe { System.realloc(pointer, layout, size) };
        if !pointer.is_null() {
            let live = if size >= layout.size() {
                LIVE.fetch_add(size - layout.size(), Relaxed) + size - layout.size()
            } else {
                LIVE.fetch_sub(layout.size() - size, Relaxed) - (layout.size() - size)
            };
            if ENABLED.load(Relaxed) {
                REALLOCS.fetch_add(1, Relaxed);
                BYTES.fetch_add(size, Relaxed);
                PEAK.fetch_max(live, Relaxed);
            }
        }
        pointer
    }
}

fn main() {
    Experiment::allocations(Counted::begin, Counted::finish).unwrap();
}
