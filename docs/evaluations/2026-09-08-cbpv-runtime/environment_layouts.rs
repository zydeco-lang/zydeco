//! Executable storage/collection traces, not native performance measurements.
//! The managed case uses the production collector and experimental moving-root
//! contract. Its explicit handle table models registered nested environments.

#[allow(dead_code)]
#[path = "../../../runtime/gc.rs"]
mod gc;

use gc::{CheneyHeap, OutOfMemory, RootRange, RootSource, Roots};
use std::cell::Cell;
use zydeco_machine::{
    frames::{
        Environment, FrameError, Frames, Layout, LayoutId,
        moving::{ALLOCATION_KIND, LiveFrame, MovingRoots},
        storage::{Fixed, Growable, Storage},
    },
    native::{ENVIRONMENT_BYTES, WORD_BYTES, Word},
};

const WORDS: usize = ENVIRONMENT_BYTES / WORD_BYTES;
const ALLOCATIONS: usize = 100_000;

struct Scenario {
    name: &'static str,
    words: usize,
    slots: &'static [Word],
    depth: usize,
}

impl Scenario {
    fn layout(&self, depth: usize) -> Layout {
        Layout { id: LayoutId(depth), words: self.words }
    }

    fn sentinel(depth: usize) -> Word {
        71 + 2 * depth
    }

    fn linear<S: Storage>(&self, scheme: &str) {
        let mut frames = Frames::<S>::EMPTY;
        let mut tokens = Vec::new();
        let mut growths = 0;
        let mut previous = 0;
        for depth in 0..=self.depth {
            let base = match frames.enter(self.layout(depth)) {
                | Ok(base) => base,
                | Err(FrameError::Capacity { .. }) => {
                    let (owner, token) = tokens.pop().unwrap();
                    let base = frames.resume(owner, token).unwrap();
                    for &slot in self.slots {
                        assert_eq!(unsafe { base.add(slot).read() }, Self::sentinel(depth - 1));
                    }
                    println!(
                        "{},{scheme},capacity-rejected,{depth},{},{},{growths},,,,",
                        self.name,
                        frames.high_water_words(),
                        frames.reserved_words()
                    );
                    return;
                }
                | Err(error) => panic!("{error}"),
            };
            if frames.reserved_words() != previous {
                growths += 1;
                previous = frames.reserved_words();
            }
            if depth != self.depth {
                for &slot in self.slots {
                    unsafe { base.add(slot).write(Self::sentinel(depth)) };
                }
                tokens
                    .push((LayoutId(depth), frames.suspend(LayoutId(depth), self.slots).unwrap()));
            }
        }
        let used = frames.used_words();
        let reserved = frames.reserved_words();
        for _ in 0..ALLOCATIONS {
            frames.enter(self.layout(self.depth)).unwrap();
        }
        assert_eq!(frames.used_words(), used);
        assert_eq!(frames.reserved_words(), reserved);
        for (owner, token) in tokens.into_iter().rev() {
            let base = frames.resume(owner, token).unwrap();
            for &slot in self.slots {
                assert_eq!(unsafe { base.add(slot).read() }, Self::sentinel(owner.0));
            }
        }
        println!("{},{scheme},ok,{},{used},{reserved},{growths},,,,", self.name, self.depth);
    }

    fn managed(&self) {
        let mut machine = Managed {
            heap: Box::new(CheneyHeap::<
                ENVIRONMENT_BYTES,
                { ENVIRONMENT_BYTES / gc::INDEX_REGION_BYTES },
            >::new()),
            frames: Vec::new(),
            work: Cell::new(Work::default()),
        };
        for depth in 0..=self.depth {
            let slots = if depth == self.depth { &[][..] } else { self.slots };
            if let Err(error) = machine.enter(self.layout(depth), slots, Self::sentinel(depth)) {
                machine.verify();
                let work = machine.work.get();
                println!(
                    "{},managed,heap-rejected,{depth},{},,,{},{},{},{}",
                    self.name,
                    machine.frames.iter().map(|frame| frame.layout.words).sum::<usize>(),
                    work.collections,
                    work.frame_words_copied,
                    work.value_words_lifted,
                    error.live_bytes
                );
                return;
            }
        }
        for _ in 0..ALLOCATIONS {
            // The active frame is reused across tail transfers. Allocation here
            // is value-heap churn, independent of entering a new environment.
            machine.allocate(1, ALLOCATION_KIND).unwrap();
        }
        machine.verify();
        let work = machine.work.get();
        let words = machine.frames.iter().map(|frame| frame.layout.words).sum::<usize>();
        println!(
            "{},managed,ok,{},{words},,,{},{},{},0",
            self.name,
            self.depth,
            work.collections,
            work.frame_words_copied,
            work.value_words_lifted
        );
        // Return to the oldest environment. The next collection only retains
        // that registered frame and its live values, regardless of former depth.
        machine.frames.truncate(1);
        for _ in 0..ALLOCATIONS {
            machine.allocate(1, ALLOCATION_KIND).unwrap();
        }
        machine.verify();
    }
}

struct HeapFrame {
    layout: Layout,
    handle: Word,
    slots: &'static [Word],
    expected: Word,
}

#[derive(Clone, Copy, Default)]
struct Work {
    collections: usize,
    frame_words_copied: usize,
    value_words_lifted: usize,
}

struct Published<'a> {
    frames: &'a mut [HeapFrame],
    work: &'a Cell<Work>,
}

impl RootSource for Published<'_> {
    fn with_roots<T>(self, trace: impl FnOnce(Roots<'_>) -> T) -> T {
        let old = self.work.get();
        self.work.set(Work {
            collections: old.collections + 1,
            frame_words_copied: old.frame_words_copied
                + self.frames.iter().map(|frame| frame.layout.words).sum::<usize>(),
            value_words_lifted: old.value_words_lifted
                + self.frames.iter().map(|frame| frame.slots.len()).sum::<usize>(),
        });
        let frames = self
            .frames
            .iter_mut()
            .map(|frame| LiveFrame {
                layout: frame.layout,
                handle: &mut frame.handle,
                slots: frame.slots,
            })
            .collect();
        unsafe {
            MovingRoots::with_roots(frames, |slots| {
                trace(Roots {
                    stack: RootRange { start: std::ptr::null_mut(), end: std::ptr::null_mut() },
                    slots,
                })
            })
            .unwrap()
        }
    }
}

struct Managed {
    heap: Box<CheneyHeap<ENVIRONMENT_BYTES, { ENVIRONMENT_BYTES / gc::INDEX_REGION_BYTES }>>,
    frames: Vec<HeapFrame>,
    work: Cell<Work>,
}

impl Managed {
    fn allocate(
        &mut self, words: usize, kind: zydeco_machine::native::AllocationKind,
    ) -> Result<*mut u8, OutOfMemory> {
        unsafe {
            self.heap.allocate(
                words,
                kind,
                Published { frames: &mut self.frames, work: &self.work },
            )
        }
    }

    fn enter(
        &mut self, layout: Layout, slots: &'static [Word], expected: Word,
    ) -> Result<(), OutOfMemory> {
        let handle = self.allocate(layout.words, ALLOCATION_KIND)? as Word;
        // Opaque collection may copy the full cell, so initialize every byte.
        unsafe { (handle as *mut Word).write_bytes(0, layout.words) };
        self.frames.push(HeapFrame { layout, handle, slots: &[], expected });
        if !slots.is_empty() {
            let value = match self.allocate(1, ALLOCATION_KIND) {
                | Ok(value) => value,
                | Err(error) => {
                    self.frames.pop();
                    return Err(error);
                }
            };
            unsafe { value.cast::<Word>().write(expected) };
            // Allocation may have moved this frame. Reload before storing.
            let frame = self.frames.last_mut().unwrap();
            for &slot in slots {
                unsafe { (frame.handle as *mut Word).add(slot).write(value as Word) };
            }
            frame.slots = slots;
        }
        Ok(())
    }

    fn verify(&self) {
        for frame in &self.frames {
            for &slot in frame.slots {
                let value = unsafe { (frame.handle as *const Word).add(slot).read() };
                assert_eq!(unsafe { (value as *const Word).read() }, frame.expected);
            }
        }
    }
}

fn main() {
    println!(
        "scenario,scheme,result,depth,frame_words,reserved_words,growth_events,collections,frame_payload_words_copied,live_value_words_lifted,oom_live_bytes"
    );
    for scenario in [
        Scenario { name: "sparse", words: 256, slots: &[0, 63, 127, 255], depth: 32 },
        Scenario { name: "packed", words: 4, slots: &[0, 1, 2, 3], depth: 32 },
        Scenario { name: "empty-captures", words: 256, slots: &[], depth: 32 },
        Scenario { name: "fixed-boundary", words: 1024, slots: &[0, 63, 127, 255], depth: 127 },
        Scenario {
            name: "past-fixed-boundary",
            words: 1024,
            slots: &[0, 63, 127, 255],
            depth: 128,
        },
        Scenario { name: "packed-deep", words: 4, slots: &[0, 1, 2, 3], depth: 128 },
    ] {
        scenario.linear::<Fixed<WORDS>>("fixed");
        scenario.linear::<Growable>("growable");
        scenario.managed();
    }
}
