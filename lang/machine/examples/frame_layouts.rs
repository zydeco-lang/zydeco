//! Storage accounting for nested continuations, using the production frame model.
//! This is a synthetic trace, not an alternative backend or a timing benchmark.
//! See docs/ideas/cbpv-runtime-evaluation.md for the accounting boundary.

use zydeco_machine::{
    frames::{FrameError, Frames, Layout, LayoutId},
    native::{ENVIRONMENT_BYTES, WORD_BYTES, Word},
};

const CAPACITY: usize = ENVIRONMENT_BYTES / WORD_BYTES;

struct Scenario {
    name: &'static str,
    frame_words: usize,
    captures: &'static [Word],
    depth: usize,
    overflows: bool,
}

impl Scenario {
    fn layout(&self, depth: usize) -> Layout {
        Layout { id: LayoutId(depth), words: self.frame_words }
    }

    fn run(&self) {
        let mut frames = Frames::<CAPACITY>::EMPTY;
        let mut base = frames.enter(self.layout(0)).unwrap();
        let mut saved = Vec::new();
        for depth in 0..self.depth {
            for &slot in self.captures {
                // Only initialized slots are published to the frame model.
                unsafe { base.add(slot).write((depth + slot) * 2 + 1) };
            }
            let token = frames.suspend(LayoutId(depth), self.captures).unwrap();
            saved.push((LayoutId(depth), token));
            match frames.enter(self.layout(depth + 1)) {
                | Ok(next) => base = next,
                | Err(FrameError::Capacity { .. }) if self.overflows => {
                    // Failed entry preserves the owner, its captured values, and
                    // its still-valid token; it must not overwrite a caller.
                    assert_eq!(frames.used_words(), CAPACITY);
                    let restored = frames.resume(LayoutId(depth), token).unwrap();
                    for &slot in self.captures {
                        assert_eq!(unsafe { restored.add(slot).read() }, (depth + slot) * 2 + 1);
                    }
                    println!(
                        "{},{},{},{},capacity-rejected,{CAPACITY},,,,,",
                        self.name,
                        self.captures.len(),
                        self.frame_words,
                        self.depth
                    );
                    return;
                }
                | Err(error) => panic!("{}: {error}", self.name),
            }
        }
        assert!(!self.overflows, "expected a capacity rejection");
        let high_water = frames.high_water_words();
        for _ in 0..10_000 {
            // A fixed suspended prefix survives arbitrary tail replacements.
            frames.enter(self.layout(self.depth)).unwrap();
        }
        assert_eq!(frames.high_water_words(), high_water);
        let root_count = frames.roots(LayoutId(self.depth), &[]).unwrap().len();
        assert_eq!(root_count, self.depth * self.captures.len());
        for (layout, token) in saved.into_iter().rev() {
            let restored = frames.resume(layout, token).unwrap();
            for &slot in self.captures {
                assert_eq!(unsafe { restored.add(slot).read() }, (layout.0 + slot) * 2 + 1);
            }
        }
        assert_eq!(frames.used_words(), self.frame_words);

        let captures = self.captures.len();
        // Code + tuple pointer (or trivial immediate for an empty capture).
        let heap_control_words = 2 * self.depth;
        // Cheney cells carry a two-word header; empty captures need no cell.
        let heap_capture_words = self.depth * if captures == 0 { 0 } else { captures + 2 };
        let flat_control_words = self.depth * (1 + captures);
        let copied_words = 2 * self.depth * captures;
        println!(
            "{},{captures},{},{},ok,{high_water},{root_count},{heap_control_words},\
            {heap_capture_words},{flat_control_words},{copied_words}",
            self.name, self.frame_words, self.depth
        );
    }
}

fn main() {
    println!(
        "scenario,captures,frame_words,depth,result,retained_high_water_words,\
        suspended_root_slots,heap_control_words,heap_capture_cell_words,\
        flat_control_words,copied_value_words"
    );
    for scenario in [
        Scenario { name: "empty", frame_words: 256, captures: &[], depth: 32, overflows: false },
        Scenario {
            name: "compact",
            frame_words: 8,
            captures: &[0, 1, 2, 3],
            depth: 32,
            overflows: false,
        },
        Scenario {
            name: "sparse",
            frame_words: 256,
            captures: &[0, 63, 127, 255],
            depth: 32,
            overflows: false,
        },
        Scenario {
            name: "capacity-fits",
            frame_words: 1024,
            captures: &[0, 63, 127, 255],
            depth: 127,
            overflows: false,
        },
        Scenario {
            name: "capacity-exceeded",
            frame_words: 1024,
            captures: &[0, 63, 127, 255],
            depth: 128,
            overflows: true,
        },
    ] {
        scenario.run();
    }
}
