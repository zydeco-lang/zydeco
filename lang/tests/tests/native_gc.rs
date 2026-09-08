// The native executable links a generated entry point. Include its independent
// collector here so its unit tests also run in the workspace suite on every host.
#[path = "../../../runtime/gc.rs"]
mod gc;

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

struct HeapFrameRoots<'a> {
    frames: Vec<zydeco_machine::frames::moving::LiveFrame<'a>>,
}

impl gc::RootSource for HeapFrameRoots<'_> {
    fn with_roots<T>(self, trace: impl FnOnce(gc::Roots<'_>) -> T) -> T {
        unsafe {
            zydeco_machine::frames::moving::MovingRoots::with_roots(self.frames, |slots| {
                trace(gc::Roots {
                    stack: gc::RootRange { start: std::ptr::null_mut(), end: std::ptr::null_mut() },
                    slots,
                })
            })
            .unwrap()
        }
    }
}

#[test]
fn managed_frames_restore_shared_live_fields_after_movement_and_collecting_failure() {
    use gc::{CheneyHeap, RootRange, Roots};
    use zydeco_machine::{
        frames::{
            Layout, LayoutId,
            moving::{ALLOCATION_KIND, LiveFrame},
        },
        native::{AllocationKind, Word},
    };
    let mut heap = CheneyHeap::<256, 1>::new();
    let empty = RootRange { start: std::ptr::null_mut(), end: std::ptr::null_mut() };
    // All three initial cells fit, so these allocation calls cannot collect.
    let value = unsafe {
        heap.allocate(1, AllocationKind::Opaque, Roots { stack: empty, slots: &mut [] }).unwrap()
    };
    let frame = unsafe {
        heap.allocate(3, ALLOCATION_KIND, Roots { stack: empty, slots: &mut [] }).unwrap()
    };
    let dead = unsafe {
        heap.allocate(12, AllocationKind::Opaque, Roots { stack: empty, slots: &mut [] }).unwrap()
    };
    unsafe {
        value.cast::<Word>().write(71);
        frame.cast::<Word>().write(value as Word);
        frame.cast::<Word>().add(1).write(dead as Word);
        frame.cast::<Word>().add(2).write(value as Word);
    }
    let layout = Layout { id: LayoutId(0), words: 3 };
    let mut handle = frame as Word;
    let roots =
        HeapFrameRoots { frames: vec![LiveFrame { layout, handle: &mut handle, slots: &[0, 2] }] };
    // This fits only if the dead field is excluded. Two live fields share one object.
    unsafe { heap.allocate(9, AllocationKind::Opaque, roots).unwrap() };
    assert_ne!(handle, frame as Word);
    let relocated = unsafe { (handle as *const Word).read() };
    assert_ne!(relocated, value as Word);
    assert_eq!(unsafe { (handle as *const Word).add(2).read() }, relocated);
    assert_eq!(unsafe { (relocated as *const Word).read() }, 71);
    let previous_handle = handle;
    let roots =
        HeapFrameRoots { frames: vec![LiveFrame { layout, handle: &mut handle, slots: &[0, 2] }] };
    // Collection succeeds, allocation fails, and both the base and live fields
    // still need repair before a caller can inspect its previous environment.
    let error = unsafe { heap.allocate(24, AllocationKind::Opaque, roots).unwrap_err() };
    assert_eq!(error.live_bytes, 64);
    assert_ne!(handle, previous_handle);
    let value = unsafe { (handle as *const Word).read() };
    assert_eq!(unsafe { (value as *const Word).read() }, 71);
    assert_eq!(unsafe { (handle as *const Word).add(2).read() }, value);
}

#[test]
fn managed_root_publication_rejects_an_invalid_map_before_running_the_collector() {
    use zydeco_machine::frames::{
        FrameError, Layout, LayoutId,
        moving::{LiveFrame, MovingRoots},
    };
    let mut words = [71];
    let mut handle = words.as_mut_ptr() as usize;
    let frames = vec![LiveFrame {
        layout: Layout { id: LayoutId(0), words: 1 },
        handle: &mut handle,
        slots: &[1],
    }];
    let result =
        unsafe { MovingRoots::with_roots(frames, |_| panic!("invalid map was published")) };
    assert_eq!(result, Err::<(), _>(FrameError::InvalidSlot { slot: 1, words: 1 }));
    assert_eq!(handle, words.as_mut_ptr() as usize);
    assert_eq!(words, [71]);
}

#[test]
fn allocation_publishes_roots_only_when_collection_needs_them() {
    use gc::{CheneyHeap, RootRange, Roots};
    use zydeco_machine::native::{AllocationKind, Word};
    let mut heap = CheneyHeap::<64, 1>::new();
    let calls = std::cell::Cell::new(0);
    let empty = RootRange { start: std::ptr::null_mut(), end: std::ptr::null_mut() };
    let roots = CountedRoots { calls: &calls, roots: Roots { stack: empty, slots: &mut [] } };
    let retained = unsafe { heap.allocate(2, AllocationKind::Opaque, roots).unwrap() };
    unsafe {
        retained.cast::<Word>().write(71);
    }
    assert_eq!(calls.get(), 0);
    let roots = CountedRoots { calls: &calls, roots: Roots { stack: empty, slots: &mut [] } };
    unsafe {
        heap.allocate(2, AllocationKind::Opaque, roots).unwrap();
    }
    assert_eq!(calls.get(), 0);

    let mut live = retained as Word;
    let mut slots = [&mut live as *mut Word];
    let roots = CountedRoots { calls: &calls, roots: Roots { stack: empty, slots: &mut slots } };
    unsafe {
        heap.allocate(2, AllocationKind::Opaque, roots).unwrap();
    }
    assert_eq!(calls.get(), 1);
    assert_ne!(live, retained as Word);
    assert_eq!(unsafe { (live as *const Word).read() }, 71);

    let roots = CountedRoots { calls: &calls, roots: Roots { stack: empty, slots: &mut slots } };
    let error = unsafe { heap.allocate(usize::MAX, AllocationKind::Opaque, roots) }.unwrap_err();
    assert_eq!(error.requested_words, usize::MAX);
    assert_eq!(calls.get(), 1);

    // One live 32-byte cell leaves insufficient space for this 40-byte request.
    let roots = CountedRoots { calls: &calls, roots: Roots { stack: empty, slots: &mut slots } };
    let error = unsafe { heap.allocate(3, AllocationKind::Opaque, roots) }.unwrap_err();
    assert_eq!(error.requested_words, 3);
    assert_eq!(error.live_bytes, 32);
    assert_eq!(calls.get(), 2);
    assert_eq!(unsafe { (live as *const Word).read() }, 71);
}

#[test]
fn collection_updates_suspended_slots_without_retaining_dead_frame_slots() {
    use gc::{CheneyHeap, RootRange, Roots};
    use zydeco_machine::{
        frames::{Frames, Layout, LayoutId},
        native::{AllocationKind, Word},
    };
    let mut heap = CheneyHeap::<128, 1>::new();
    let mut frames = Frames::<zydeco_machine::frames::storage::Fixed<8>>::EMPTY;
    let owner = Layout { id: LayoutId(0), words: 2 };
    let callee = Layout { id: LayoutId(1), words: 1 };
    let base = frames.enter(owner).unwrap();
    let empty = RootRange { start: std::ptr::null_mut(), end: std::ptr::null_mut() };
    let roots = Roots { stack: empty, slots: &mut [] };
    let retained = unsafe { heap.allocate(1, AllocationKind::Scanned, roots).unwrap() };
    unsafe {
        retained.cast::<Word>().write(71);
        base.write(retained as Word);
    }
    let mut slots = frames.roots(owner.id, &[0]).unwrap();
    let roots = Roots { stack: empty, slots: &mut slots };
    let dead = unsafe { heap.allocate(5, AllocationKind::Opaque, roots).unwrap() };
    unsafe {
        base.add(1).write(dead as Word);
    }
    let token = frames.suspend(owner.id, &[0]).unwrap();
    frames.enter(callee).unwrap();
    let mut slots = frames.roots(callee.id, &[]).unwrap();
    let roots = Roots { stack: empty, slots: &mut slots };
    // This allocation fits after copying the one live object, but fails if the
    // dead pointer in the same retained frame is also treated as a root.
    unsafe {
        heap.allocate(7, AllocationKind::Opaque, roots).unwrap();
    }
    assert_eq!(frames.resume(owner.id, token).unwrap(), base);
    let relocated = unsafe { base.read() as *mut Word };
    assert_ne!(relocated, retained.cast());
    assert_eq!(unsafe { relocated.read() }, 71);
}
