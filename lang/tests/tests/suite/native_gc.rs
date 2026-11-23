use zydeco_host::gc;

struct CountedRoots<'a> {
    calls: &'a std::cell::Cell<usize>,
    roots: gc::Roots<'a>,
}

impl gc::RootSource for CountedRoots<'_> {
    fn with_roots<T>(self, trace: impl FnOnce(gc::Roots<'_>) -> T) -> T {
        self.calls.set(self.calls.get() + 1);
        trace(self.roots)
    }
}

#[test]
fn allocation_publishes_roots_only_when_collection_needs_them() {
    use gc::{CheneyHeap, RootRange, Roots};
    use zydeco_machine::native::{AllocationKind, Word};
    let mut heap = CheneyHeap::with_limits(64, Some(64));
    let calls = std::cell::Cell::new(0);
    let empty = RootRange::EMPTY;
    let roots =
        CountedRoots { calls: &calls, roots: Roots { stack: empty, frame: empty, slots: &mut [] } };
    let retained = unsafe { heap.allocate(2, AllocationKind::Opaque, roots).unwrap() };
    unsafe {
        retained.cast::<Word>().write(71);
    }
    assert_eq!(calls.get(), 0);
    let roots =
        CountedRoots { calls: &calls, roots: Roots { stack: empty, frame: empty, slots: &mut [] } };
    unsafe {
        heap.allocate(2, AllocationKind::Opaque, roots).unwrap();
    }
    assert_eq!(calls.get(), 0);

    let mut live = retained as Word;
    let mut slots = [&mut live as *mut Word];
    let roots = CountedRoots {
        calls: &calls,
        roots: Roots { stack: empty, frame: empty, slots: &mut slots },
    };
    unsafe {
        heap.allocate(2, AllocationKind::Opaque, roots).unwrap();
    }
    assert_eq!(calls.get(), 1);
    assert_ne!(live, retained as Word);
    assert_eq!(unsafe { (live as *const Word).read() }, 71);

    let roots = CountedRoots {
        calls: &calls,
        roots: Roots { stack: empty, frame: empty, slots: &mut slots },
    };
    let error = unsafe { heap.allocate(usize::MAX, AllocationKind::Opaque, roots) }.unwrap_err();
    assert_eq!(error.requested_words, usize::MAX);
    assert_eq!(calls.get(), 1);

    // One live 32-byte cell leaves insufficient space for this 40-byte request.
    let roots = CountedRoots {
        calls: &calls,
        roots: Roots { stack: empty, frame: empty, slots: &mut slots },
    };
    let error = unsafe { heap.allocate(3, AllocationKind::Opaque, roots) }.unwrap_err();
    assert_eq!(error.requested_words, 3);
    assert_eq!(error.live_bytes, 32);
    assert_eq!(calls.get(), 2);
    assert_eq!(unsafe { (live as *const Word).read() }, 71);
}

#[test]
fn native_heap_grows_beyond_one_megabyte_live_data() {
    use zydeco_tests::utils::{ExecutionTarget, SourceProgram};
    for backend in [ExecutionTarget::Interpreter, ExecutionTarget::Exe] {
        SourceProgram::setup("tests/core/native-heap-growth.zy").test(backend);
    }
}

#[test]
fn managed_strings_reclaim_garbage_and_preserve_live_roots() {
    use zydeco_tests::utils::{ExecutionTarget, SourceProgram};
    for backend in [ExecutionTarget::Interpreter, ExecutionTarget::Exe, ExecutionTarget::WasmSps] {
        SourceProgram::setup("tests/core/gc-string-churn.zy").test(backend);
    }
}
