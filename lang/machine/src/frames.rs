//! Retained activation frames. Entry returns a fresh base after any storage growth.
//! Transitions do not collect the managed heap. Suspensions are consumed in nesting
//! order, and their slot sets supply precise roots independently of reserved capacity.

use crate::{native::Word, word::RuntimeWord};
use alloc::vec::Vec;

pub mod storage;
pub mod moving;
use storage::Storage;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LayoutId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Layout {
    pub id: LayoutId,
    pub words: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token(Word);

impl Token {
    pub fn word(self) -> Word {
        self.0
    }
    pub fn from_word(word: Word) -> Self {
        Self(word)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameError {
    Capacity { requested: usize, available: usize },
    Allocation { words: usize },
    MissingActivation,
    WrongLayout { expected: LayoutId, actual: LayoutId },
    InvalidSlot { slot: usize, words: usize },
    InvalidResumption,
    TokenOverflow,
    InvalidAction,
}

impl core::fmt::Display for FrameError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            | Self::Capacity { requested, available } => write!(
                f,
                "environment stack overflow: requested {requested} words, {available} available"
            ),
            | Self::MissingActivation => f.write_str("missing active environment frame"),
            | Self::Allocation { words } => write!(f, "cannot allocate {words} environment words"),
            | Self::WrongLayout { expected, actual } => {
                write!(f, "frame layout mismatch: expected {}, found {}", expected.0, actual.0)
            }
            | Self::InvalidSlot { slot, words } => {
                write!(f, "frame slot {slot} is outside its {words}-word layout")
            }
            | Self::InvalidResumption => f.write_str("stale or out-of-order frame resumption"),
            | Self::TokenOverflow => f.write_str("frame resumption token space exhausted"),
            | Self::InvalidAction => f.write_str("invalid native frame action"),
        }
    }
}
impl core::error::Error for FrameError {}

#[derive(Clone, Copy)]
struct Activation {
    layout: Layout,
    base: usize,
    end: usize,
    retained: usize,
}

struct Suspension {
    frame: usize,
    token: Token,
    slots: &'static [Word],
}

/// A nested environment with independently chosen word storage.
/// Saved references are checked tokens, never pointers into the metadata vectors.
/// Enter may relocate words; callers must use its returned base and discard all
/// earlier slot addresses. Suspend and Roots never relocate words. Managed values
/// cannot contain pointers into these frames. Root addresses are temporary, and
/// cannot survive another entry. Moving the Rust owner does not move its words.
pub struct Frames<S: Storage> {
    storage: S,
    activations: Vec<Activation>,
    suspensions: Vec<Suspension>,
    next_token: u64,
    high_water: usize,
}

impl<S: Storage> Frames<S> {
    pub const EMPTY: Self = Self {
        storage: S::EMPTY,
        activations: Vec::new(),
        suspensions: Vec::new(),
        next_token: 0,
        high_water: 0,
    };

    pub fn high_water_words(&self) -> usize {
        self.high_water
    }
    pub fn used_words(&self) -> usize {
        self.activations.last().map_or(0, |frame| frame.end)
    }

    pub fn reserved_words(&self) -> usize {
        self.storage.reserved_words()
    }

    /// Establish a closure activation, reusing an unretained active frame.
    /// All validation precedes mutation, including failure during tail replacement.
    pub fn enter(&mut self, layout: Layout) -> Result<*mut Word, FrameError> {
        let replace = self.activations.last().is_some_and(|frame| frame.retained == 0);
        let base = if replace { self.activations.last().unwrap().base } else { self.used_words() };
        self.storage.reserve(base, layout.words)?;
        if replace {
            self.activations.pop();
        }
        let end = base + layout.words;
        self.activations.push(Activation { layout, base, end, retained: 0 });
        self.high_water = self.high_water.max(end);
        Ok(self.storage.base().wrapping_add(base))
    }

    fn active(&self, layout: LayoutId) -> Result<Activation, FrameError> {
        let frame = *self.activations.last().ok_or(FrameError::MissingActivation)?;
        Self::check_layout(frame, layout)?;
        Ok(frame)
    }

    fn check_layout(frame: Activation, expected: LayoutId) -> Result<(), FrameError> {
        if frame.layout.id != expected {
            return Err(FrameError::WrongLayout { expected, actual: frame.layout.id });
        }
        Ok(())
    }

    fn check_slots(frame: Activation, slots: &[Word]) -> Result<(), FrameError> {
        slots.iter().try_for_each(|&slot| {
            if slot < frame.layout.words {
                Ok(())
            } else {
                Err(FrameError::InvalidSlot { slot, words: frame.layout.words })
            }
        })
    }

    pub fn suspend(
        &mut self, layout: LayoutId, slots: &'static [Word],
    ) -> Result<Token, FrameError> {
        let frame = self.active(layout)?;
        Self::check_slots(frame, slots)?;
        let token = RuntimeWord::unsigned(self.next_token).ok_or(FrameError::TokenOverflow)?;
        let token = Token(Word::try_from(token).map_err(|_| FrameError::TokenOverflow)?);
        self.next_token += 1;
        let frame = self.activations.len() - 1;
        self.activations[frame].retained += 1;
        self.suspensions.push(Suspension { frame, token, slots });
        Ok(token)
    }

    pub fn resume(&mut self, layout: LayoutId, token: Token) -> Result<*mut Word, FrameError> {
        let suspension = self.suspensions.last().ok_or(FrameError::InvalidResumption)?;
        if suspension.token != token {
            return Err(FrameError::InvalidResumption);
        }
        let index = suspension.frame;
        let frame = self.activations[index];
        Self::check_layout(frame, layout)?;
        if self.activations[index + 1..].iter().any(|frame| frame.retained != 0) {
            return Err(FrameError::InvalidResumption);
        }
        self.suspensions.pop();
        self.activations.truncate(index + 1);
        self.activations[index].retained -= 1;
        Ok(self.storage.base().wrapping_add(frame.base))
    }

    /// Addresses of active live slots and the union of pending suspensions' slots.
    /// Compiler validation establishes initialization before publishing a slot set.
    pub fn roots(
        &mut self, layout: LayoutId, slots: &[Word],
    ) -> Result<Vec<*mut Word>, FrameError> {
        let active = self.active(layout)?;
        Self::check_slots(active, slots)?;
        let mut indices = slots
            .iter()
            .map(|slot| active.base + slot)
            .chain(self.suspensions.iter().flat_map(|suspension| {
                let base = self.activations[suspension.frame].base;
                suspension.slots.iter().map(move |slot| base + slot)
            }))
            .collect::<Vec<_>>();
        indices.sort_unstable();
        indices.dedup();
        let base = self.storage.base();
        Ok(indices.into_iter().map(|index| base.wrapping_add(index)).collect())
    }
}

// Keep the Rust record and emitter traversal in one declaration, as for Closure.
macro_rules! frame_action {
    ($($field:ident),+ $(,)?) => {
        /// Native action header, followed by slot indices for Suspend and Roots.
        #[derive(Clone, Copy, Debug)]
        #[repr(C)]
        pub struct Action<W> { $(pub $field: W),+ }

        impl<W> Action<W> {
            pub fn into_words(self) -> [W; { [$(stringify!($field)),+].len() }] {
                [$(self.$field),+]
            }
        }
    };
}

frame_action! { kind, layout, words }

#[derive(Clone, Copy, Debug)]
#[repr(u64)]
pub enum ActionKind {
    Enter,
    Suspend,
    Resume,
    Roots,
}

impl Action<u64> {
    pub fn enter(layout: Layout) -> Self {
        Self {
            kind: ActionKind::Enter as u64,
            layout: layout.id.0 as u64,
            words: layout.words as u64,
        }
    }
    pub fn suspend(layout: LayoutId, slots: usize) -> Self {
        Self { kind: ActionKind::Suspend as u64, layout: layout.0 as u64, words: slots as u64 }
    }
    pub fn resume(layout: LayoutId) -> Self {
        Self { kind: ActionKind::Resume as u64, layout: layout.0 as u64, words: 0 }
    }
    pub fn roots(layout: LayoutId, slots: usize) -> Self {
        Self { kind: ActionKind::Roots as u64, layout: layout.0 as u64, words: slots as u64 }
    }
}

impl Action<Word> {
    /// # Safety
    /// The descriptor and its `words` trailing slot indices must be valid static
    /// storage emitted by the matching compiler. Enter/Resume have no trailing slots.
    pub unsafe fn apply<S: Storage>(
        &'static self, frames: &mut Frames<S>, token: Word,
    ) -> Result<Word, FrameError> {
        let layout = LayoutId(self.layout);
        match self.kind as u64 {
            | kind if kind == ActionKind::Enter as u64 => {
                frames.enter(Layout { id: layout, words: self.words }).map(|base| base as Word)
            }
            | kind if kind == ActionKind::Suspend as u64 => {
                frames.suspend(layout, unsafe { self.slots() }).map(Token::word)
            }
            | kind if kind == ActionKind::Resume as u64 => {
                frames.resume(layout, Token::from_word(token)).map(|base| base as Word)
            }
            | _ => Err(FrameError::InvalidAction),
        }
    }

    unsafe fn slots(&'static self) -> &'static [Word] {
        unsafe { core::slice::from_raw_parts((self as *const Self).add(1).cast(), self.words) }
    }

    /// # Safety
    /// The descriptor must be followed by `words` static slot indices.
    pub unsafe fn root_slots<S: Storage>(
        &'static self, frames: &mut Frames<S>,
    ) -> Result<Vec<*mut Word>, FrameError> {
        if self.kind as u64 != ActionKind::Roots as u64 {
            return Err(FrameError::InvalidAction);
        }
        frames.roots(LayoutId(self.layout), unsafe { self.slots() })
    }
}

pub const STEP_SYMBOL: &str = "zydeco_frame_step";

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::boxed::Box;

    const CALLER: Layout = Layout { id: LayoutId(1), words: 3 };
    const CALLEE: Layout = Layout { id: LayoutId(2), words: 5 };

    struct Descriptor;

    impl Descriptor {
        fn serialize(action: Action<u64>, slots: &[u64]) -> &'static Action<Word> {
            let words = action
                .into_words()
                .into_iter()
                .chain(slots.iter().copied())
                .map(|word| Word::try_from(word).unwrap())
                .collect::<Vec<_>>();
            let words = Box::leak(words.into_boxed_slice());
            // The emitter supplies the same word sequence in static aligned storage.
            unsafe { &*words.as_ptr().cast::<Action<Word>>() }
        }
    }

    #[test]
    fn emitted_descriptors_drive_the_runtime_transitions_and_roots() {
        let mut frames = Frames::<storage::Fixed<8>>::EMPTY;
        let enter = Descriptor::serialize(Action::enter(CALLER), &[]);
        let caller = unsafe { enter.apply(&mut frames, 0).unwrap() } as *mut Word;
        unsafe {
            caller.write(71);
        }
        let suspend = Descriptor::serialize(Action::suspend(CALLER.id, 1), &[0]);
        let token = unsafe { suspend.apply(&mut frames, 0).unwrap() };
        let callee = Descriptor::serialize(Action::enter(CALLEE), &[]);
        unsafe {
            callee.apply(&mut frames, 0).unwrap();
        }
        let roots = Descriptor::serialize(Action::roots(CALLEE.id, 0), &[]);
        assert_eq!(unsafe { roots.root_slots(&mut frames).unwrap() }, [caller]);
        assert_eq!(unsafe { roots.apply(&mut frames, 0) }, Err(FrameError::InvalidAction));
        assert_eq!(unsafe { enter.root_slots(&mut frames) }, Err(FrameError::InvalidAction));
        let resume = Descriptor::serialize(Action::resume(CALLER.id), &[]);
        assert_eq!(unsafe { resume.apply(&mut frames, token).unwrap() }, caller as Word);
        assert_eq!(unsafe { caller.read() }, 71);
    }

    #[test]
    fn nested_resumptions_preserve_slots_and_reclaim_younger_storage() {
        let mut frames = Frames::<storage::Fixed<8>>::EMPTY;
        let caller = frames.enter(CALLER).unwrap();
        unsafe {
            caller.write(11);
            caller.add(1).write(23);
            caller.add(2).write(35);
        }
        let outer = frames.suspend(CALLER.id, &[0]).unwrap();
        let inner = frames.suspend(CALLER.id, &[2]).unwrap();
        let callee = frames.enter(CALLEE).unwrap();
        unsafe {
            callee.write(99);
        }
        assert_eq!(frames.resume(CALLER.id, outer), Err(FrameError::InvalidResumption));
        assert_eq!(frames.used_words(), 8);
        assert_eq!(
            frames.resume(CALLEE.id, inner),
            Err(FrameError::WrongLayout { expected: CALLEE.id, actual: CALLER.id })
        );
        assert_eq!(frames.resume(CALLER.id, inner).unwrap(), caller);
        assert_eq!(frames.used_words(), 3);
        let roots = frames.roots(CALLER.id, &[2]).unwrap();
        assert_eq!(roots, [caller, caller.wrapping_add(2)]);
        assert_eq!(unsafe { caller.add(2).read() }, 35);
        assert_eq!(frames.resume(CALLER.id, inner), Err(FrameError::InvalidResumption));
        assert_eq!(frames.resume(CALLER.id, outer).unwrap(), caller);
        assert_eq!(unsafe { caller.read() }, 11);
    }

    #[test]
    fn a_tail_chain_under_a_retained_caller_has_bounded_storage() {
        let mut frames = Frames::<storage::Fixed<8>>::EMPTY;
        let caller = frames.enter(CALLER).unwrap();
        unsafe {
            caller.write(71);
        }
        // Control metadata may move; the allocation addressed by native code cannot.
        let mut frames = Box::new(frames);
        let saved = frames.suspend(CALLER.id, &[0]).unwrap();
        let callee = frames.enter(CALLEE).unwrap();
        for _ in 0..100_000 {
            assert_eq!(frames.enter(CALLEE).unwrap(), callee);
        }
        assert_eq!(frames.high_water_words(), 8);
        assert_eq!(frames.activations.len(), 2);
        assert_eq!(frames.resume(CALLER.id, saved).unwrap(), caller);
        assert_eq!(unsafe { caller.read() }, 71);
        assert_eq!(frames.enter(CALLEE).unwrap(), caller);
        assert_eq!(frames.used_words(), 5);
    }

    #[test]
    fn invalid_slots_and_overflow_leave_the_existing_activation_usable() {
        let mut frames = Frames::<storage::Fixed<8>>::EMPTY;
        assert_eq!(frames.suspend(CALLER.id, &[]), Err(FrameError::MissingActivation));
        let caller = frames.enter(CALLER).unwrap();
        unsafe {
            caller.write(71);
        }
        assert_eq!(
            frames.suspend(CALLER.id, &[3]),
            Err(FrameError::InvalidSlot { slot: 3, words: 3 })
        );
        assert_eq!(
            frames.roots(CALLER.id, &[3]),
            Err(FrameError::InvalidSlot { slot: 3, words: 3 })
        );
        let oversized = Layout { id: LayoutId(3), words: 9 };
        assert_eq!(
            frames.enter(oversized),
            Err(FrameError::Capacity { requested: 9, available: 8 })
        );
        assert_eq!(frames.used_words(), 3);
        assert_eq!(unsafe { caller.read() }, 71);
        let saved = frames.suspend(CALLER.id, &[0]).unwrap();
        assert_eq!(
            frames.enter(oversized),
            Err(FrameError::Capacity { requested: 9, available: 5 })
        );
        frames.enter(CALLEE).unwrap();
        assert_eq!(frames.resume(CALLER.id, saved).unwrap(), caller);
    }

    #[test]
    fn growth_preserves_offsets_and_tokens_beyond_the_old_fixed_capacity() {
        let mut frames = Frames::<storage::Growable>::EMPTY;
        let first = frames.enter(CALLER).unwrap();
        unsafe { first.write(71) };
        let outer = frames.suspend(CALLER.id, &[0]).unwrap();
        let inner = frames.suspend(CALLER.id, &[0]).unwrap();
        let large = Layout { id: CALLEE.id, words: 200_000 };
        let active = frames.enter(large).unwrap();
        // Only the new base and checked tokens may be used after entry. Never
        // dereference `first`: the allocator is allowed to have moved its words.
        assert_eq!(frames.used_words(), 200_003);
        assert_eq!(unsafe { frames.roots(large.id, &[]).unwrap()[0].read() }, 71);
        let reserved = frames.reserved_words();
        for _ in 0..10_000 {
            assert_eq!(frames.enter(large).unwrap(), active);
        }
        assert_eq!(frames.reserved_words(), reserved);
        assert_eq!(frames.resume(CALLER.id, outer), Err(FrameError::InvalidResumption));
        let restored = frames.resume(CALLER.id, inner).unwrap();
        assert_eq!(unsafe { restored.read() }, 71);
        assert_eq!(frames.roots(CALLER.id, &[0]).unwrap(), [restored]);
        frames.resume(CALLER.id, outer).unwrap();
        assert_eq!(frames.used_words(), CALLER.words);
    }

    #[test]
    fn failed_growth_preserves_the_active_base_and_pending_roots() {
        let mut frames = Frames::<storage::Growable<8>>::EMPTY;
        let base = frames.enter(CALLER).unwrap();
        unsafe { base.write(71) };
        let token = frames.suspend(CALLER.id, &[0]).unwrap();
        assert_eq!(
            frames.enter(Layout { id: CALLEE.id, words: 6 }),
            Err(FrameError::Capacity { requested: 6, available: 5 })
        );
        assert_eq!(frames.roots(CALLER.id, &[]).unwrap(), [base]);
        assert_eq!(frames.resume(CALLER.id, token).unwrap(), base);
        assert_eq!(unsafe { base.read() }, 71);
        let mut impossible = Frames::<storage::Fixed<{ usize::MAX }>>::EMPTY;
        assert_eq!(impossible.enter(CALLER), Err(FrameError::Allocation { words: usize::MAX }));
        assert_eq!(impossible.used_words(), 0);
        assert_eq!(impossible.reserved_words(), 0);
    }
}

#[cfg(feature = "runtime")]
const _: () = {
    assert!(core::mem::size_of::<Action<Word>>() == core::mem::size_of::<Action<u64>>());
    assert!(core::mem::align_of::<Action<Word>>() == 8);
};
