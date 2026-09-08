//! Compiler literal adapters for the shared runtime-word representation.

use crate::{FloatLiteral, IntegerLiteral};
pub use zydeco_machine::word::{EncodedScalar, RuntimeWord, WordError};

impl IntegerLiteral {
    /// The encoding of an integer literal, boxing payloads outside the immediate range.
    pub fn encode_runtime(self) -> Result<EncodedScalar, WordError> {
        use IntegerLiteral::*;
        let immediate = match self {
            | Int8(value) => RuntimeWord::signed(value.into()),
            | Int16(value) => RuntimeWord::signed(value.into()),
            | Int32(value) => RuntimeWord::signed(value.into()),
            | Int64(value) => RuntimeWord::signed(value),
            | UInt8(value) => RuntimeWord::unsigned(value.into()),
            | UInt16(value) => RuntimeWord::unsigned(value.into()),
            | UInt32(value) => RuntimeWord::unsigned(value.into()),
            | UInt64(value) => RuntimeWord::unsigned(value),
            | Unresolved(_) => return Err(WordError::UnresolvedInteger),
        };
        Ok(immediate
            .map_or_else(|| EncodedScalar::Boxed(self.to_word_bits()), EncodedScalar::Immediate))
    }
}

impl FloatLiteral {
    /// The encoding of a float literal; `Float32` always fits an immediate.
    pub fn encode_runtime(self) -> EncodedScalar {
        match self {
            | FloatLiteral::Float32(bits) => EncodedScalar::Immediate(
                RuntimeWord::unsigned(bits.into()).expect("Float32 payload fits an immediate"),
            ),
            | FloatLiteral::Float64(bits) => EncodedScalar::Boxed(bits),
        }
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
            IntegerLiteral::Int64(i64::MAX).encode_runtime().unwrap(),
            EncodedScalar::Boxed(i64::MAX as u64)
        );
    }

    /// The JavaScript host still restates the scalar boundary outside Rust.
    #[test]
    fn javascript_boundary_matches() {
        let pin = |text: &str, name: &str, expected: &str| {
            let needle = format!("const {name}");
            let line = text
                .lines()
                .find(|line| line.contains(&needle))
                .unwrap_or_else(|| panic!("source no longer defines {name}"));
            let value = line.split_once('=').unwrap().1.trim().trim_end_matches(';');
            assert_eq!(value, expected, "boundary constant {name} diverged");
        };

        let host = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/wasm-host.mjs"),
        )
        .expect("the wasm host is part of this repository");

        pin(&host, "IMMEDIATE_SIGNED_MIN", "-(0x4000_0000_0000_0000n)");
        pin(&host, "IMMEDIATE_SIGNED_MAX", "0x3fff_ffff_ffff_ffffn");
        pin(&host, "IMMEDIATE_UNSIGNED_MAX", "0x7fff_ffff_ffff_ffffn");

        assert_eq!(RuntimeWord::TAG, 1);
        assert_eq!(RuntimeWord::UNSIGNED_MAX, 0x7fff_ffff_ffff_ffff);
        assert_eq!(RuntimeWord::SIGNED_MIN, -0x4000_0000_0000_0000);
        assert_eq!(RuntimeWord::SIGNED_MAX, 0x3fff_ffff_ffff_ffff);
    }
}
