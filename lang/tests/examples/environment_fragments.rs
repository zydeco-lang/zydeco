//! Compare the actual nested environments with identical transition/GC traces.
//! Timings belong to environment-study.py; this example checks value survival,
//! root updates, word storage, sharing, and bounded tail reuse.

#[allow(dead_code)]
#[path = "../../../runtime/gc.rs"]
mod gc;

use gc::{CheneyHeap, RootRange, RootSource, Roots};
use zydeco_machine::{
    frames::{Environment, Frames, Layout, LayoutId, fragments::Fragments, storage::Growable},
    native::{AllocationKind, Word},
};

const HEAP_BYTES: usize = 64 * 1024;
const TAIL_ENTRIES: usize = 100_000;

struct Scenario {
    name: &'static str,
    words: usize,
    captures: &'static [Word],
    owners: usize,
    suspensions_per_owner: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Usage {
    used: usize,
    peak: usize,
    reserved: usize,
}

#[derive(Default)]
struct CollectionWork {
    collections: usize,
    root_slots: usize,
}

struct Published<'a, E> {
    environment: &'a mut E,
    layout: LayoutId,
    work: &'a mut CollectionWork,
}

impl<E: Environment> RootSource for Published<'_, E> {
    fn with_roots<T>(self, trace: impl FnOnce(Roots<'_>) -> T) -> T {
        let mut slots = self.environment.roots(self.layout, &[]).unwrap();
        self.work.collections += 1;
        self.work.root_slots += slots.len();
        trace(Roots {
            stack: RootRange { start: std::ptr::null_mut(), end: std::ptr::null_mut() },
            slots: &mut slots,
        })
    }
}

impl Scenario {
    fn layout(&self, owner: usize) -> Layout {
        Layout { id: LayoutId(owner), words: self.words }
    }

    fn run<E: Environment>(
        &self, scheme: &str, mut environment: E, usage: impl Fn(&E) -> Usage, copies: bool,
    ) {
        let mut heap =
            Box::new(CheneyHeap::<HEAP_BYTES, { HEAP_BYTES / gc::INDEX_REGION_BYTES }>::new());
        let mut work = CollectionWork::default();
        let mut tokens = Vec::new();
        for owner in 0..self.owners {
            let layout = self.layout(owner);
            let base = environment.enter(layout).unwrap();
            let roots =
                Published { environment: &mut environment, layout: layout.id, work: &mut work };
            // Entry and value collection share the current noncollecting-frame
            // contract: collecting managed values cannot move `base`.
            let value = unsafe { heap.allocate(1, AllocationKind::Opaque, roots).unwrap() };
            unsafe { value.cast::<Word>().write(71 + 2 * owner) };
            for &slot in self.captures {
                unsafe { base.add(slot).write(value as Word) };
            }
            for _ in 0..self.suspensions_per_owner {
                tokens.push((layout.id, environment.suspend(layout.id, self.captures).unwrap()));
            }
        }
        let leaf = self.layout(self.owners);
        let base = environment.enter(leaf).unwrap();
        // Every local location is reusable in the leaf, even if a suspended
        // owner used the same offset. Snapshot and retained engines differ here.
        for slot in 0..leaf.words {
            unsafe { base.add(slot).write(3) };
        }
        let peak = usage(&environment);
        let roots = environment.roots(leaf.id, &[]).unwrap().len();
        let expected_roots =
            self.owners * self.captures.len() * if copies { self.suspensions_per_owner } else { 1 };
        assert_eq!(roots, expected_roots);
        for _ in 0..TAIL_ENTRIES {
            environment.enter(leaf).unwrap();
            let roots =
                Published { environment: &mut environment, layout: leaf.id, work: &mut work };
            unsafe { heap.allocate(1, AllocationKind::Opaque, roots).unwrap() };
        }
        assert_eq!(usage(&environment), peak);
        assert!(work.collections > 0);
        for (owner, token) in tokens.into_iter().rev() {
            let base = environment.resume(owner, token).unwrap();
            for &slot in self.captures {
                let value = unsafe { base.add(slot).read() as *const Word };
                assert_eq!(unsafe { value.read() }, 71 + 2 * owner.0);
            }
        }
        let final_usage = usage(&environment);
        assert_eq!(final_usage.used, self.words);
        assert_eq!(final_usage.reserved, peak.reserved);
        assert_eq!(environment.roots(LayoutId(0), &[]).unwrap(), []);
        // Exact trace-derived copy count: each snapshot is made and consumed
        // once. Collector visits and buffer capacities above are observed.
        let copied = if copies {
            2 * self.owners * self.suspensions_per_owner * self.captures.len()
        } else {
            0
        };
        println!(
            "{},{scheme},{},{},{},{},{},{},{},{},{roots},{},{},{copied}",
            self.name,
            self.owners,
            self.suspensions_per_owner,
            self.words,
            self.captures.len(),
            peak.peak,
            peak.reserved,
            final_usage.used,
            final_usage.reserved,
            work.collections,
            work.root_slots,
        );
    }
}

fn main() {
    println!(
        "scenario,scheme,owners,suspensions_per_owner,frame_words,captures,peak_words,reserved_words,final_words,reserved_after_return,suspended_root_slots,collections,root_slots_visited,copied_value_words"
    );
    for scenario in [
        Scenario {
            name: "sparse-low",
            words: 256,
            captures: &[0, 1, 2, 3],
            owners: 32,
            suspensions_per_owner: 1,
        },
        Scenario {
            name: "sparse-high",
            words: 256,
            captures: &[0, 63, 127, 255],
            owners: 32,
            suspensions_per_owner: 1,
        },
        Scenario {
            name: "dense",
            words: 4,
            captures: &[0, 1, 2, 3],
            owners: 32,
            suspensions_per_owner: 1,
        },
        Scenario { name: "empty", words: 256, captures: &[], owners: 32, suspensions_per_owner: 1 },
        Scenario {
            name: "shared",
            words: 8,
            captures: &[0, 1, 2, 3, 4, 5, 6, 7],
            owners: 1,
            suspensions_per_owner: 32,
        },
        Scenario {
            name: "shared-sparse",
            words: 256,
            captures: &[0, 63, 127, 255],
            owners: 1,
            suspensions_per_owner: 32,
        },
    ] {
        scenario.run(
            "retained",
            Frames::<Growable>::EMPTY,
            |frames| Usage {
                used: frames.used_words(),
                peak: frames.high_water_words(),
                reserved: frames.reserved_words(),
            },
            false,
        );
        scenario.run(
            "compact",
            Fragments::<Growable>::EMPTY,
            |frames| Usage {
                used: frames.used_words(),
                peak: frames.high_water_words(),
                reserved: frames.reserved_words(),
            },
            true,
        );
    }
}
