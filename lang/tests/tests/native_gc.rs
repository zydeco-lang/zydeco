// The native executable links a generated entry point. Include its independent
// collector here so its unit tests also run in the workspace suite on every host.
#[path = "../../../runtime/gc.rs"]
mod gc;

#[test]
fn collection_updates_suspended_slots_without_retaining_dead_frame_slots() {
    use gc::{CheneyHeap, RootRange, Roots};
    use zydeco_machine::{
        frames::{Frames, Layout, LayoutId},
        native::{AllocationKind, Word},
    };
    let mut heap = CheneyHeap::<128, 1>::new();
    let mut frames = Frames::<8>::EMPTY;
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
