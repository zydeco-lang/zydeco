//! The current AMD64 scheme: a reusable environment, machine control stack, and
//! heap captures. Layouts use explicit 64-bit words when inspected by a compiler
//! on another host. The stub instantiates the same records with native words.

pub use crate::closure::{Closure, ClosureField};
use crate::word::RuntimeWord;
use core::mem::{offset_of, size_of};

pub type Word = usize;
pub const WORD_BYTES: usize = size_of::<u64>();
pub const IMMEDIATE_TAG: Word = RuntimeWord::TAG as Word;
pub const ENVIRONMENT_BYTES: usize = 1024 * 1024;

/// Collector policy for an allocated payload. The low metadata bit is reserved
/// for forwarding; only scanned blocks contain values to trace.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(usize)]
pub enum AllocationKind {
    Scanned = 0,
    Opaque = 1 << 1,
}

impl AllocationKind {
    pub const fn symbol(self) -> &'static str {
        match self {
            | Self::Scanned => "zydeco_alloc_scanned",
            | Self::Opaque => "zydeco_alloc_opaque",
        }
    }
}

/// A host result consumed immediately by an assembly resumption bridge.
/// This is distinct from a source-language `Ret` continuation on the control stack.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct HostTransfer<W> {
    pub resume: W,
    pub closure: W,
    pub first: W,
    pub second: W,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransferField {
    Resume,
    Closure,
    First,
    Second,
}

impl TransferField {
    pub const fn offset(self) -> usize {
        match self {
            | Self::Resume => offset_of!(HostTransfer<u64>, resume),
            | Self::Closure => offset_of!(HostTransfer<u64>, closure),
            | Self::First => offset_of!(HostTransfer<u64>, first),
            | Self::Second => offset_of!(HostTransfer<u64>, second),
        }
    }
}

// One catalog supplies both the emitter's argument order and the stub's symbols.
macro_rules! resume_bridges {
    ($($arity:ident => $symbol:ident [$($field:ident),*]),* $(,)?) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum ResumeArity { $($arity),* }

        impl ResumeArity {
            pub const ALL: &[Self] = &[$(Self::$arity),*];

            pub const fn symbol(self) -> &'static str {
                match self { $(Self::$arity => stringify!($symbol)),* }
            }

            /// Arguments in the order that resumed Zydeco code consumes them.
            /// The closure environment precedes these arguments on the stack.
            pub const fn arguments(self) -> &'static [TransferField] {
                match self { $(Self::$arity => &[$(TransferField::$field),*]),* }
            }

            #[cfg(feature = "runtime")]
            fn address(self) -> Word {
                match self { $(Self::$arity => $symbol as *const () as Word),* }
            }
        }

        #[cfg(feature = "runtime")]
        unsafe extern "sysv64" {
            $(#[link_name = concat!("\x01", stringify!($symbol))] fn $symbol();)*
        }
    };
}

resume_bridges! {
    Zero => rust_resume_zydeco_0 [],
    One => rust_resume_zydeco_1 [First],
    Two => rust_resume_zydeco_2 [First, Second],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HostArguments<W> {
    None,
    One(W),
    Two(W, W),
}

impl<W> HostArguments<W> {
    pub const fn arity(&self) -> ResumeArity {
        match self {
            | Self::None => ResumeArity::Zero,
            | Self::One(_) => ResumeArity::One,
            | Self::Two(_, _) => ResumeArity::Two,
        }
    }
}

impl<W: Default> HostTransfer<W> {
    pub fn with_arguments(resume: W, closure: W, arguments: HostArguments<W>) -> Self {
        let (first, second) = match arguments {
            | HostArguments::None => (W::default(), W::default()),
            | HostArguments::One(first) => (first, W::default()),
            | HostArguments::Two(first, second) => (first, second),
        };
        Self { resume, closure, first, second }
    }
}

#[cfg(feature = "runtime")]
impl HostTransfer<Word> {
    pub fn for_closure(closure: Word, arguments: HostArguments<Word>) -> Self {
        let resume = arguments.arity().address();
        Self::with_arguments(resume, closure, arguments)
    }
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
    assert!(size_of::<HostTransfer<Word>>() == size_of::<HostTransfer<u64>>());
    assert!(core::mem::align_of::<HostTransfer<Word>>() == WORD_BYTES);
    assert!(offset_of!(Closure<Word>, environment) == ClosureField::Environment.offset());
    assert!(offset_of!(Closure<Word>, code) == ClosureField::Code.offset());
    assert!(offset_of!(HostTransfer<Word>, resume) == TransferField::Resume.offset());
    assert!(offset_of!(HostTransfer<Word>, closure) == TransferField::Closure.offset());
    assert!(offset_of!(HostTransfer<Word>, first) == TransferField::First.offset());
    assert!(offset_of!(HostTransfer<Word>, second) == TransferField::Second.offset());
};

// The entry symbol includes the model source identity, making stale pairs fail to link.
include!(concat!(env!("OUT_DIR"), "/contract.rs"));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transfers_preserve_arguments_and_clear_unused_slots() {
        for (arguments, arity, payload) in [
            (HostArguments::None, ResumeArity::Zero, [0, 0]),
            (HostArguments::One(11), ResumeArity::One, [11, 0]),
            (HostArguments::Two(11, 23), ResumeArity::Two, [11, 23]),
        ] {
            assert_eq!(arguments.arity(), arity);
            let record = HostTransfer::with_arguments(8_u64, 16, arguments);
            assert_eq!(record.resume, 8);
            assert_eq!(record.closure, 16);
            assert_eq!([record.first, record.second], payload);
        }
    }
}
