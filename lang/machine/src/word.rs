//! One-bit tagged, 64-bit words used by the current native and WebAssembly backends.

/// The representation of a scalar at ordinary value and compilation-unit boundaries.
/// Raw payloads may be used locally, but must be encoded before entering a traced slot.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScalarRepresentation {
    Immediate,
    OpaqueBox,
}

/// A scalar as either an immediate word or the payload of a one-word opaque box.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EncodedScalar {
    Immediate(u64),
    Boxed(u64),
}

/// An input that cannot be represented at the runtime-word boundary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WordError {
    UnresolvedInteger,
    IntegerRange,
    TagIndex(usize),
}

impl core::fmt::Display for WordError {
    fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            | Self::UnresolvedInteger => {
                formatter.write_str("unresolved integer literal reached runtime-word encoding")
            }
            | Self::IntegerRange => formatter.write_str("integer exceeds the tagged payload range"),
            | Self::TagIndex(value) => {
                write!(formatter, "runtime tag index {value} does not fit an immediate")
            }
        }
    }
}

impl core::error::Error for WordError {}

/// Odd words contain immediate payloads; even words are pointer-shaped values.
pub struct RuntimeWord;

impl RuntimeWord {
    pub const TAG: u64 = 1;
    pub const INTEGER_BITS: u8 = 63;
    pub const UNSIGNED_MAX: u64 = 0x7fff_ffff_ffff_ffff;
    pub const SIGNED_MIN: i64 = -0x4000_0000_0000_0000;
    pub const SIGNED_MAX: i64 = 0x3fff_ffff_ffff_ffff;

    /// Arithmetic on source machine integers wraps within the tagged payload.
    pub const fn wrap_signed(value: i64) -> i64 {
        value.wrapping_shl(1) >> 1
    }

    pub const fn wrap_unsigned(value: u64) -> u64 {
        value & Self::UNSIGNED_MAX
    }

    pub fn unsigned(value: u64) -> Option<u64> {
        (value <= Self::UNSIGNED_MAX).then_some((value << 1) | Self::TAG)
    }

    pub fn signed(value: i64) -> Option<u64> {
        (Self::SIGNED_MIN..=Self::SIGNED_MAX)
            .contains(&value)
            .then_some(((value as u64) << 1) | Self::TAG)
    }

    pub fn decode_unsigned(word: u64) -> Option<u64> {
        (word & Self::TAG != 0).then_some(word >> 1)
    }

    pub fn decode_signed(word: u64) -> Option<i64> {
        (word & Self::TAG != 0).then_some((word as i64) >> 1)
    }

    pub fn index(value: usize) -> Result<u64, WordError> {
        u64::try_from(value).ok().and_then(Self::unsigned).ok_or(WordError::TagIndex(value))
    }

    pub fn code(index: u32) -> u64 {
        (u64::from(index) << 1) | Self::TAG
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signed_payloads_round_trip_and_reject_overflow() {
        for value in [RuntimeWord::SIGNED_MIN, -1, 0, 1, RuntimeWord::SIGNED_MAX] {
            let word = RuntimeWord::signed(value).unwrap();
            assert_eq!(RuntimeWord::decode_signed(word), Some(value));
        }
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MIN - 1), None);
        assert_eq!(RuntimeWord::signed(RuntimeWord::SIGNED_MAX + 1), None);
        assert_eq!(RuntimeWord::decode_signed(0), None);
        assert_eq!(RuntimeWord::decode_signed(0x1000), None);
    }

    #[test]
    fn unsigned_payloads_round_trip_and_reject_overflow() {
        for value in [0, 1, RuntimeWord::UNSIGNED_MAX] {
            let word = RuntimeWord::unsigned(value).unwrap();
            assert_eq!(RuntimeWord::decode_unsigned(word), Some(value));
        }
        assert_eq!(RuntimeWord::unsigned(RuntimeWord::UNSIGNED_MAX + 1), None);
        assert_eq!(RuntimeWord::decode_unsigned(0), None);
        assert_eq!(RuntimeWord::decode_unsigned(0x1000), None);
        assert_eq!(RuntimeWord::code(1), 3);
    }

    #[test]
    fn indices_report_unrepresentable_payloads() {
        assert_eq!(RuntimeWord::index(0), Ok(1));
        if usize::BITS == 64 {
            assert_eq!(RuntimeWord::index(usize::MAX), Err(WordError::TagIndex(usize::MAX)));
        }
    }
}
