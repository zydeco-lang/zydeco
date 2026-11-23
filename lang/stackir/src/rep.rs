//! Compact representation types derived from source protocols and Stack IR syntax.
//!
//! Unlike `ValueProtocol`, which preserves full nested product structure and source type parameters
//! for validation, `RepType` summarizes the local physical word/scalar representation that backends
//! and representation analyses need without allocating `Box`/`Vec` structures per lookup.

use crate::protocol::ValueProtocol;
use zydeco_syntax::{Literal, PrimitiveType, scalar::ScalarType, word::ScalarRepresentation};

/// Physical representation summary of a value or pattern binder.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum RepType {
    /// Polymorphic or heap-traced tagged word.
    #[default]
    Word,
    /// Zero-payload unit (`Triv`).
    Unit,
    /// Typed numeric scalar (`Int*`, `UInt*`, `Float*`).
    Scalar(ScalarType),
    /// Unmanaged raw data address (`Memory` operations).
    Address,
    /// Product value with static positive field arity.
    Product { arity: u16 },
    /// Closure package `(environment, code)` or direct code block.
    Closure,
}

impl RepType {
    /// Summarize a source [`ValueProtocol`] into its flat physical [`RepType`].
    pub fn from_protocol(protocol: &ValueProtocol) -> Self {
        match protocol {
            | ValueProtocol::Unknown | ValueProtocol::Parameter(_) => Self::Word,
            | ValueProtocol::Unit => Self::Unit,
            | ValueProtocol::Primitive(PrimitiveType::Integer(ty)) => {
                Self::Scalar(ScalarType::Integer(*ty))
            }
            | ValueProtocol::Primitive(PrimitiveType::Float(ty)) => {
                Self::Scalar(ScalarType::Float(*ty))
            }
            | ValueProtocol::Primitive(PrimitiveType::Char | PrimitiveType::String) => Self::Word,
            | ValueProtocol::Address => Self::Address,
            | ValueProtocol::Product(fields) => {
                u16::try_from(fields.len()).map_or(Self::Word, |arity| Self::Product { arity })
            }
            | ValueProtocol::Thunk(_) => Self::Closure,
        }
    }

    /// Infer the representation of a source literal.
    pub fn from_literal(literal: &Literal) -> Self {
        match ScalarType::of_literal(literal) {
            | Some(ty) => Self::Scalar(ty),
            | None => Self::Word,
        }
    }

    /// Return the underlying [`ScalarType`] when this representation is numeric.
    pub fn scalar_type(self) -> Option<ScalarType> {
        match self {
            | Self::Scalar(ty) => Some(ty),
            | _ => None,
        }
    }

    /// Whether a value of this representation fits directly in an immediate machine word
    /// without needing heap allocation or GC tracing when boxed under current runtime encoding.
    pub fn is_immediate_word(self) -> bool {
        match self {
            | Self::Unit | Self::Address => true,
            | Self::Scalar(ty) => ty.representation() == ScalarRepresentation::Immediate,
            | Self::Word | Self::Product { .. } | Self::Closure => false,
        }
    }

    /// Whether values with this representation may hold heap pointers or boxed cells that need GC tracing.
    pub fn needs_gc_root_when_boxed(self) -> bool {
        !self.is_immediate_word()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::protocol::StackProtocol;
    use zydeco_syntax::{FloatLiteral, FloatType, IntegerLiteral, IntegerType, Utf8String};

    #[test]
    fn rep_type_summarizes_protocols_and_literals_consistently() {
        assert_eq!(RepType::from_protocol(&ValueProtocol::Unknown), RepType::Word);
        assert_eq!(RepType::from_protocol(&ValueProtocol::Unit), RepType::Unit);
        assert!(RepType::Unit.is_immediate_word());
        assert!(!RepType::Unit.needs_gc_root_when_boxed());

        assert_eq!(RepType::from_protocol(&ValueProtocol::Address), RepType::Address);
        assert!(RepType::Address.is_immediate_word());

        let i32_rep = RepType::from_protocol(&ValueProtocol::Primitive(PrimitiveType::Integer(
            IntegerType::Int32,
        )));
        assert_eq!(i32_rep, RepType::Scalar(ScalarType::Integer(IntegerType::Int32)));
        assert!(i32_rep.is_immediate_word());
        assert!(!i32_rep.needs_gc_root_when_boxed());

        let i64_rep = RepType::from_literal(&Literal::Integer(IntegerLiteral::Int64(9)));
        assert_eq!(i64_rep, RepType::Scalar(ScalarType::Integer(IntegerType::Int64)));
        assert!(!i64_rep.is_immediate_word());
        assert!(i64_rep.needs_gc_root_when_boxed());

        let f64_rep =
            RepType::from_literal(&Literal::Float(FloatLiteral::Float64(1.5_f64.to_bits())));
        assert_eq!(f64_rep, RepType::Scalar(ScalarType::Float(FloatType::Float64)));
        assert!(!f64_rep.is_immediate_word());

        assert_eq!(RepType::from_literal(&Literal::Char('z')), RepType::Word);
        assert_eq!(RepType::from_literal(&Literal::String(Utf8String::from("hi"))), RepType::Word);

        let prod_rep = RepType::from_protocol(&ValueProtocol::Product(vec![
            ValueProtocol::Unit,
            ValueProtocol::Unknown,
        ]));
        assert_eq!(prod_rep, RepType::Product { arity: 2 });
        assert!(prod_rep.needs_gc_root_when_boxed());

        let closure_rep =
            RepType::from_protocol(&ValueProtocol::Thunk(Box::new(StackProtocol::Unknown)));
        assert_eq!(closure_rep, RepType::Closure);
        assert!(closure_rep.needs_gc_root_when_boxed());
    }
}
