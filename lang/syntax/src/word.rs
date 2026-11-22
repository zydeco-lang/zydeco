//! Compiler literal adapters for the shared runtime-word representation.

use crate::{FloatLiteral, IntegerLiteral};
pub use zydeco_machine::word::{EncodedScalar, RuntimeWord, ScalarRepresentation, WordError};

impl IntegerLiteral {
    /// Exact-width 64-bit integers always use opaque boxes, including small payloads.
    pub fn encode_runtime(self) -> Result<EncodedScalar, WordError> {
        use IntegerLiteral::*;
        let immediate = match self {
            | Int8(value) => RuntimeWord::signed(value.into()),
            | Int16(value) => RuntimeWord::signed(value.into()),
            | Int32(value) => RuntimeWord::signed(value.into()),
            | Int64(value) => return Ok(EncodedScalar::Boxed(value as u64)),
            | UInt64(value) => return Ok(EncodedScalar::Boxed(value)),
            | Int(value) => RuntimeWord::signed(value),
            | UInt8(value) => RuntimeWord::unsigned(value.into()),
            | UInt16(value) => RuntimeWord::unsigned(value.into()),
            | UInt32(value) => RuntimeWord::unsigned(value.into()),
            | UInt(value) => RuntimeWord::unsigned(value),
            | Unresolved(_) => return Err(WordError::UnresolvedInteger),
        };
        immediate.map(EncodedScalar::Immediate).ok_or(WordError::IntegerRange)
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
        for value in [RuntimeWord::SIGNED_MIN, -1, 0, RuntimeWord::SIGNED_MAX] {
            assert_eq!(
                IntegerLiteral::Int(value).encode_runtime(),
                Ok(EncodedScalar::Immediate(RuntimeWord::signed(value).unwrap()))
            );
        }
        for value in [RuntimeWord::SIGNED_MIN - 1, RuntimeWord::SIGNED_MAX + 1] {
            assert_eq!(IntegerLiteral::Int(value).encode_runtime(), Err(WordError::IntegerRange));
        }
        assert_eq!(
            IntegerLiteral::UInt(RuntimeWord::UNSIGNED_MAX).encode_runtime(),
            Ok(EncodedScalar::Immediate(u64::MAX))
        );
        assert_eq!(
            IntegerLiteral::UInt(RuntimeWord::UNSIGNED_MAX + 1).encode_runtime(),
            Err(WordError::IntegerRange)
        );
    }

    #[test]
    fn exact_width_integers_always_box_without_losing_payload_bits() {
        for bits in [0, 1, 0x1000, 0x7fff_ffff_ffff_ffff, 0x8000_0000_0000_0000, u64::MAX] {
            assert_eq!(
                IntegerLiteral::Int64(bits as i64).encode_runtime(),
                Ok(EncodedScalar::Boxed(bits))
            );
            assert_eq!(
                IntegerLiteral::UInt64(bits).encode_runtime(),
                Ok(EncodedScalar::Boxed(bits))
            );
        }
    }
}
