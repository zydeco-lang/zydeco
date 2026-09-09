//! Primitive scalar storage operations. Layout composition belongs to Zydeco code.

use crate::{host::HostValue, syntax::*};
use std::rc::Rc;

pub(crate) struct ScalarBytes;

impl ScalarBytes {
    pub(crate) fn encode(arguments: Vec<SemValue>) -> Result<Computation, i32> {
        let [SemValue::Literal(literal)] = arguments.as_slice() else {
            unreachable!("checked scalar encoder received a non-scalar")
        };
        let bytes = match literal {
            | Literal::Integer(integer) => match integer {
                | IntegerLiteral::Int8(value) => value.to_le_bytes().to_vec(),
                | IntegerLiteral::Int16(value) => value.to_le_bytes().to_vec(),
                | IntegerLiteral::Int32(value) => value.to_le_bytes().to_vec(),
                | IntegerLiteral::Int64(value) => value.to_le_bytes().to_vec(),
                | IntegerLiteral::UInt8(value) => value.to_le_bytes().to_vec(),
                | IntegerLiteral::UInt16(value) => value.to_le_bytes().to_vec(),
                | IntegerLiteral::UInt32(value) => value.to_le_bytes().to_vec(),
                | IntegerLiteral::UInt64(value) => value.to_le_bytes().to_vec(),
                | IntegerLiteral::Unresolved(_) => unreachable!("runtime integer is resolved"),
            },
            | Literal::Float(FloatLiteral::Float32(bits)) => bits.to_le_bytes().to_vec(),
            | Literal::Float(FloatLiteral::Float64(bits)) => bits.to_le_bytes().to_vec(),
            | _ => unreachable!("checked encoder received a non-numeric literal"),
        };
        let value: SemValue = HostValue::Bytes(bytes.into()).into();
        Ok(Return(Rc::new(value.into())).into())
    }

    pub(crate) fn decode(
        primitive: PrimitiveType, arguments: Vec<SemValue>,
    ) -> Result<Computation, i32> {
        let [SemValue::Host(HostValue::Bytes(bytes)), when_none, when_some] = arguments.as_slice()
        else {
            unreachable!("checked decoder received invalid arguments")
        };
        let value = Self::literal(primitive, bytes.as_slice()).map(SemValue::from);
        Ok(Self::optional(value, when_none, when_some))
    }

    fn literal(primitive: PrimitiveType, bytes: &[u8]) -> Option<Literal> {
        macro_rules! integer {
            ($variant:ident, $type:ty) => {
                Literal::Integer(IntegerLiteral::$variant(<$type>::from_le_bytes(
                    bytes.try_into().ok()?,
                )))
            };
        }
        Some(match primitive {
            | PrimitiveType::Integer(integer) => match integer {
                | IntegerType::Int8 => integer!(Int8, i8),
                | IntegerType::Int16 => integer!(Int16, i16),
                | IntegerType::Int32 => integer!(Int32, i32),
                | IntegerType::Int64 => integer!(Int64, i64),
                | IntegerType::UInt8 => integer!(UInt8, u8),
                | IntegerType::UInt16 => integer!(UInt16, u16),
                | IntegerType::UInt32 => integer!(UInt32, u32),
                | IntegerType::UInt64 => integer!(UInt64, u64),
            },
            | PrimitiveType::Float(FloatType::Float32) => {
                Literal::Float(FloatLiteral::Float32(u32::from_le_bytes(bytes.try_into().ok()?)))
            }
            | PrimitiveType::Float(FloatType::Float64) => {
                Literal::Float(FloatLiteral::Float64(u64::from_le_bytes(bytes.try_into().ok()?)))
            }
            | _ => unreachable!("only numeric primitives have scalar byte operations"),
        })
    }

    pub(crate) fn aligned(arguments: Vec<SemValue>) -> Result<Computation, i32> {
        let [
            SemValue::Host(HostValue::Bytes(bytes)),
            SemValue::Literal(Literal::Integer(IntegerLiteral::Int64(alignment))),
            when_none,
            when_some,
        ] = arguments.as_slice()
        else {
            unreachable!("checked alignment operation received invalid arguments")
        };
        let value = usize::try_from(*alignment)
            .ok()
            .and_then(|alignment| bytes.aligned(alignment))
            .map(|bytes| HostValue::Bytes(bytes).into());
        Ok(Self::optional(value, when_none, when_some))
    }

    fn optional(
        value: Option<SemValue>, when_none: &SemValue, when_some: &SemValue,
    ) -> Computation {
        match value {
            | None => Force(Rc::new(when_none.clone().into())).into(),
            | Some(value) => {
                let function: Computation = Force(Rc::new(when_some.clone().into())).into();
                App(Rc::new(function), Rc::new(value.into())).into()
            }
        }
    }
}
