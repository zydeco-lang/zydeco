//! The tagged runtime-word representation shared by every backend.
//!
//! A runtime word is one machine word. Odd words are immediates carrying their payload
//! shifted left with the low tag bit set; even words are pointer-shaped values, and the
//! collector only moves those that point into its active semispace. Signed payloads use
//! the inclusive range [`RuntimeWord::SIGNED_MIN`] through [`RuntimeWord::SIGNED_MAX`],
//! unsigned payloads `[0, RuntimeWord::UNSIGNED_MAX]`. Scalars that cannot surrender one
//! tag bit — wide integers, double floats — live in one-word opaque heap blocks instead.
//!
//! This boundary is load-bearing: the native runtime's `Immediate`, the garbage
//! collector's tag test, and the WebAssembly backends must all recognize the same words.
//! The native runtime is a standalone binary crate that cannot share this type, so its
//! copy is pinned here by a source-level test; every compiler-side backend consumes this
//! one directly.

use crate::{FloatLiteral, IntegerLiteral};

/// A scalar constant as either an immediate word or the bits of a boxed word.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EncodedScalar {
    /// The tagged immediate word.
    Immediate(u64),
    /// The payload bits to store in a one-word opaque box.
    Boxed(u64),
}

/// Why a scalar could not be encoded as a runtime word.
#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
pub enum WordError {
    #[error("unresolved integer literal reached runtime-word encoding")]
    UnresolvedInteger,
    #[error("runtime tag index {0} does not fit an immediate")]
    TagIndex(usize),
}

/// The tagged runtime-word encoding.
pub struct RuntimeWord;

impl RuntimeWord {
    /// The low bit distinguishing immediates from pointer-shaped words.
    pub const TAG: u64 = 1;
    /// The largest unsigned payload that fits an immediate.
    pub const UNSIGNED_MAX: u64 = 0x7fff_ffff_ffff_ffff;
    /// The smallest signed payload that fits an immediate.
    pub const SIGNED_MIN: i64 = -0x4000_0000_0000_0000;
    /// The largest signed payload that fits an immediate.
    pub const SIGNED_MAX: i64 = 0x3fff_ffff_ffff_ffff;

    /// The immediate word of an unsigned payload, or `None` outside the range.
    pub fn unsigned(value: u64) -> Option<u64> {
        (value <= Self::UNSIGNED_MAX).then_some((value << 1) | Self::TAG)
    }

    /// The immediate word of a signed payload, or `None` outside the range.
    pub fn signed(value: i64) -> Option<u64> {
        (Self::SIGNED_MIN..=Self::SIGNED_MAX)
            .contains(&value)
            .then_some(((value as u64) << 1) | Self::TAG)
    }

    /// The encoding of an integer literal, boxing payloads outside the immediate range.
    pub fn integer(value: IntegerLiteral) -> Result<EncodedScalar, WordError> {
        use IntegerLiteral::*;
        let immediate = match value {
            | Int8(value) => Self::signed(value.into()),
            | Int16(value) => Self::signed(value.into()),
            | Int32(value) => Self::signed(value.into()),
            | Int64(value) => Self::signed(value),
            | UInt8(value) => Self::unsigned(value.into()),
            | UInt16(value) => Self::unsigned(value.into()),
            | UInt32(value) => Self::unsigned(value.into()),
            | UInt64(value) => Self::unsigned(value),
            | Unresolved(_) => return Err(WordError::UnresolvedInteger),
        };
        Ok(immediate
            .map_or_else(|| EncodedScalar::Boxed(value.to_word_bits()), EncodedScalar::Immediate))
    }

    /// The encoding of a float literal; `Float32` always fits an immediate.
    pub fn float(value: FloatLiteral) -> EncodedScalar {
        match value {
            | FloatLiteral::Float32(bits) => EncodedScalar::Immediate(
                Self::unsigned(bits.into()).expect("Float32 payload fits an immediate"),
            ),
            | FloatLiteral::Float64(bits) => EncodedScalar::Boxed(bits),
        }
    }

    /// The immediate word of a runtime tag or constructor index.
    pub fn index(value: usize) -> Result<u64, WordError> {
        u64::try_from(value).ok().and_then(Self::unsigned).ok_or(WordError::TagIndex(value))
    }

    /// The immediate word of a case-table code index.
    pub fn code(index: u32) -> u64 {
        (u64::from(index) << 1) | Self::TAG
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn immediates_pack_and_round_trip() {
        assert_eq!(RuntimeWord::unsigned(0), Some(RuntimeWord::TAG));
        assert_eq!(RuntimeWord::code(0), 1);
        assert_eq!(RuntimeWord::code(1), 3);
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MIN), Some(0x8000_0000_0000_0001));
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MAX), Some(0x7fff_ffff_ffff_ffff));
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MIN - 1), None);
        assert_eq!(RuntimeWord::unsigned(RuntimeWord::UNSIGNED_MAX), Some(u64::MAX));
        assert_eq!(
            RuntimeWord::integer(IntegerLiteral::Int64(i64::MAX)).unwrap(),
            EncodedScalar::Boxed(i64::MAX as u64)
        );
    }

    /// The native runtime is a standalone binary crate that cannot share this type,
    /// and the WebAssembly host is JavaScript; both restate the boundary as literals.
    /// Pin those copies to this one so they cannot drift.
    #[test]
    fn external_boundary_copies_match() {
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../runtime");
        let pin = |text: &str, name: &str, expected: &str| {
            let needle = format!("const {name}");
            let line = text
                .lines()
                .find(|line| line.contains(&needle))
                .unwrap_or_else(|| panic!("source no longer defines {name}"));
            let value = line.split_once('=').unwrap().1.trim().trim_end_matches(';');
            assert_eq!(value, expected, "boundary constant {name} diverged");
        };

        let stub = std::fs::read_to_string(root.join("stub.rs"))
            .expect("runtime sources are part of this repository");
        let gc = std::fs::read_to_string(root.join("gc.rs"))
            .expect("runtime sources are part of this repository");
        let host = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/wasm-host.mjs"),
        )
        .expect("the wasm host is part of this repository");

        pin(&gc, "IMMEDIATE_TAG: Word", "1");
        pin(&stub, "IMMEDIATE_UNSIGNED_MAX: Word", "0x7fff_ffff_ffff_ffff");
        pin(&stub, "IMMEDIATE_SIGNED_MIN: i64", "-0x4000_0000_0000_0000");
        pin(&stub, "IMMEDIATE_SIGNED_MAX: i64", "0x3fff_ffff_ffff_ffff");
        pin(&host, "IMMEDIATE_SIGNED_MIN", "-(0x4000_0000_0000_0000n)");
        pin(&host, "IMMEDIATE_SIGNED_MAX", "0x3fff_ffff_ffff_ffffn");
        pin(&host, "IMMEDIATE_UNSIGNED_MAX", "0x7fff_ffff_ffff_ffffn");

        assert_eq!(RuntimeWord::TAG, 1);
        assert_eq!(RuntimeWord::UNSIGNED_MAX, 0x7fff_ffff_ffff_ffff);
        assert_eq!(RuntimeWord::SIGNED_MIN, -0x4000_0000_0000_0000);
        assert_eq!(RuntimeWord::SIGNED_MAX, 0x3fff_ffff_ffff_ffff);
    }
}
