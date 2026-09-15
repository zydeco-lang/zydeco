//! Scalar memory access contracts, independent of temporary value representation.

use crate::{BuiltinValueRole, FloatOperation, FloatType, IntegerOperation, IntegerType};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MemoryScalar {
    Integer(IntegerType),
    Float(FloatType),
    Address,
}

impl From<crate::scalar::ScalarType> for MemoryScalar {
    fn from(ty: crate::scalar::ScalarType) -> Self {
        match ty {
            | crate::scalar::ScalarType::Integer(ty) => Self::Integer(ty),
            | crate::scalar::ScalarType::Float(ty) => Self::Float(ty),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ByteOrder {
    Little,
    Native,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AccessWidth {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

impl AccessWidth {
    pub fn bytes(self) -> u8 {
        match self {
            | Self::Bits8 => 1,
            | Self::Bits16 => 2,
            | Self::Bits32 => 4,
            | Self::Bits64 => 8,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AccessKind {
    Load,
    Store,
}

/// An ordinary, unaligned scalar access. Stronger alignment, volatile accesses,
/// and atomic accesses require separate evidence and operation contracts.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MemoryAccess {
    pub scalar: MemoryScalar,
    pub kind: AccessKind,
}

impl MemoryScalar {
    pub fn value_type(self) -> Option<crate::scalar::ScalarType> {
        use crate::scalar::ScalarType;
        match self {
            | Self::Integer(ty) => Some(ScalarType::Integer(ty)),
            | Self::Float(ty) => Some(ScalarType::Float(ty)),
            | Self::Address => None,
        }
    }

    pub fn bytes(self) -> u8 {
        self.width().bytes()
    }

    pub fn width(self) -> AccessWidth {
        match self {
            | Self::Integer(IntegerType::Int8 | IntegerType::UInt8) => AccessWidth::Bits8,
            | Self::Integer(IntegerType::Int16 | IntegerType::UInt16) => AccessWidth::Bits16,
            | Self::Integer(IntegerType::Int32 | IntegerType::UInt32)
            | Self::Float(FloatType::Float32) => AccessWidth::Bits32,
            | Self::Integer(
                IntegerType::Int64 | IntegerType::UInt64 | IntegerType::Int | IntegerType::UInt,
            )
            | Self::Float(FloatType::Float64)
            | Self::Address => AccessWidth::Bits64,
        }
    }

    pub fn byte_order(self) -> ByteOrder {
        match self {
            | Self::Address => ByteOrder::Native,
            | Self::Integer(_) | Self::Float(_) => ByteOrder::Little,
        }
    }

    /// Interpret bits read from the exact-width carrier, retaining `Int`/`UInt` range checks.
    pub fn literal(self, bits: u64) -> Option<crate::Literal> {
        use crate::{FloatLiteral, IntegerLiteral, Literal};
        Some(match self {
            | Self::Integer(ty) => {
                let shift = 64 - ty.storage_bits();
                let value = if ty.is_signed() {
                    ((bits << shift) as i64 >> shift) as i128
                } else {
                    bits as i128
                };
                Literal::Integer(IntegerLiteral::new(value).with_type(ty)?)
            }
            | Self::Float(FloatType::Float32) => Literal::Float(FloatLiteral::Float32(bits as u32)),
            | Self::Float(FloatType::Float64) => Literal::Float(FloatLiteral::Float64(bits)),
            | Self::Address => return None,
        })
    }

    pub fn bits(self, literal: &crate::Literal) -> Option<u64> {
        use crate::Literal;
        match (self, literal) {
            | (Self::Integer(ty), Literal::Integer(value)) if value.integer_type() == Some(ty) => {
                Some(value.value() as u64)
            }
            | (Self::Float(ty), Literal::Float(value)) if value.float_type() == ty => {
                Some(match value {
                    | crate::FloatLiteral::Float32(bits) => (*bits).into(),
                    | crate::FloatLiteral::Float64(bits) => *bits,
                })
            }
            | _ => None,
        }
    }
}

impl MemoryAccess {
    pub fn from_builtin(role: BuiltinValueRole) -> Option<Self> {
        let (scalar, kind) = match role {
            | BuiltinValueRole::Integer(ty, IntegerOperation::LoadLe) => {
                (MemoryScalar::Integer(ty), AccessKind::Load)
            }
            | BuiltinValueRole::Integer(ty, IntegerOperation::StoreLe) => {
                (MemoryScalar::Integer(ty), AccessKind::Store)
            }
            | BuiltinValueRole::Float(ty, FloatOperation::LoadLe) => {
                (MemoryScalar::Float(ty), AccessKind::Load)
            }
            | BuiltinValueRole::Float(ty, FloatOperation::StoreLe) => {
                (MemoryScalar::Float(ty), AccessKind::Store)
            }
            | BuiltinValueRole::MemoryLoadAddr => (MemoryScalar::Address, AccessKind::Load),
            | BuiltinValueRole::MemoryStoreAddr => (MemoryScalar::Address, AccessKind::Store),
            | _ => return None,
        };
        Some(Self { scalar, kind })
    }

    pub fn builtin(self) -> BuiltinValueRole {
        match (self.scalar, self.kind) {
            | (MemoryScalar::Integer(ty), AccessKind::Load) => {
                BuiltinValueRole::Integer(ty, IntegerOperation::LoadLe)
            }
            | (MemoryScalar::Integer(ty), AccessKind::Store) => {
                BuiltinValueRole::Integer(ty, IntegerOperation::StoreLe)
            }
            | (MemoryScalar::Float(ty), AccessKind::Load) => {
                BuiltinValueRole::Float(ty, FloatOperation::LoadLe)
            }
            | (MemoryScalar::Float(ty), AccessKind::Store) => {
                BuiltinValueRole::Float(ty, FloatOperation::StoreLe)
            }
            | (MemoryScalar::Address, AccessKind::Load) => BuiltinValueRole::MemoryLoadAddr,
            | (MemoryScalar::Address, AccessKind::Store) => BuiltinValueRole::MemoryStoreAddr,
        }
    }

    pub fn alignment(self) -> u8 {
        1
    }

    pub fn inputs(self) -> usize {
        match self.kind {
            | AccessKind::Load => 1,
            | AccessKind::Store => 2,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{IntegerLiteral, Literal};

    #[test]
    fn scalar_memory_distinguishes_carriers_from_value_domains() {
        let int = MemoryScalar::Integer(IntegerType::Int);
        let int64 = MemoryScalar::Integer(IntegerType::Int64);
        let uint = MemoryScalar::Integer(IntegerType::UInt);
        assert_eq!(int.bytes(), 8);
        assert_eq!(int64.bytes(), 8);
        assert_eq!(
            int.literal((1 << 62) - 1),
            Some(Literal::Integer(IntegerLiteral::Int((1 << 62) - 1)))
        );
        assert_eq!(int.literal(1 << 62), None);
        assert_eq!(int.literal((3 << 62) - 1), None);
        assert!(uint.literal((1 << 63) - 1).is_some());
        assert_eq!(uint.literal(1 << 63), None);
        assert_eq!(int64.literal(u64::MAX), Some(Literal::Integer(IntegerLiteral::Int64(-1))));
        assert_eq!(int.bits(&Literal::Integer(IntegerLiteral::Int64(7))), None);
        assert_eq!(MemoryScalar::Address.literal(0), None);
    }

    #[test]
    fn scalar_memory_roles_have_fixed_access_contracts() {
        use strum::VariantArray;
        let scalars = IntegerType::VARIANTS
            .iter()
            .copied()
            .map(MemoryScalar::Integer)
            .chain(FloatType::VARIANTS.iter().copied().map(MemoryScalar::Float))
            .chain([MemoryScalar::Address]);
        for scalar in scalars {
            for kind in [AccessKind::Load, AccessKind::Store] {
                let access = MemoryAccess { scalar, kind };
                assert_eq!(MemoryAccess::from_builtin(access.builtin()), Some(access));
                assert_eq!(access.alignment(), 1);
                assert_eq!(
                    scalar.byte_order(),
                    if scalar == MemoryScalar::Address {
                        ByteOrder::Native
                    } else {
                        ByteOrder::Little
                    }
                );
            }
        }
        assert_eq!(
            MemoryAccess::from_builtin(BuiltinValueRole::Integer(
                IntegerType::Int64,
                IntegerOperation::Add
            )),
            None
        );
        assert_eq!(MemoryAccess::from_builtin(BuiltinValueRole::MemoryOffset), None);
    }
}
