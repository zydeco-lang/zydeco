//! Portable 64-bit native value and heap allocation tags.

pub use crate::closure::{Closure, ClosureField};
use crate::word::RuntimeWord;
#[cfg(feature = "runtime")]
use core::mem::offset_of;
use core::mem::size_of;

pub type Word = usize;
pub const WORD_BYTES: usize = size_of::<u64>();
pub const IMMEDIATE_TAG: Word = RuntimeWord::TAG as Word;

/// Collector policy for an allocated payload. The low metadata bit is reserved
/// for forwarding; only scanned blocks contain values to trace.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum AllocationKind {
    Scanned = 0,
    Opaque = 1 << 1,
}

/// Native-sized access to the common scalar encoding.
pub struct Immediate;

#[cfg(target_pointer_width = "64")]
impl Immediate {
    pub fn unsigned(value: Word) -> Option<Word> {
        RuntimeWord::unsigned(value as u64).map(|word| word as Word)
    }

    pub fn signed(value: i64) -> Option<Word> {
        RuntimeWord::signed(value).map(|word| word as Word)
    }

    pub fn expect_unsigned(value: Word) -> Word {
        Self::unsigned(value).expect("runtime value does not fit an unsigned immediate")
    }

    pub fn expect_signed(value: i64) -> Word {
        Self::signed(value).expect("runtime value does not fit a signed immediate")
    }

    pub fn decode_unsigned(word: Word) -> Word {
        RuntimeWord::decode_unsigned(word as u64).expect("expected an immediate value") as Word
    }

    pub fn decode_signed(word: Word) -> i64 {
        RuntimeWord::decode_signed(word as u64).expect("expected an immediate value")
    }
}

#[cfg(feature = "runtime")]
const _: () = {
    assert!(size_of::<Word>() == WORD_BYTES);
    assert!(size_of::<Closure<Word>>() == size_of::<Closure<u64>>());
    assert!(offset_of!(Closure<Word>, environment) == ClosureField::Environment.offset());
    assert!(offset_of!(Closure<Word>, code) == ClosureField::Code.offset());
};
