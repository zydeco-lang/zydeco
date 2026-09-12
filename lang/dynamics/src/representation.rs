//! Primitive scalar storage operations. Layout composition belongs to Zydeco code.

use crate::memory::MemoryRuntime;
use crate::{host::HostRuntime, syntax::*};

pub(crate) struct ScalarMemory;

impl ScalarMemory {
    pub(crate) fn store(
        arguments: Vec<SemValue>, host: &mut HostRuntime,
    ) -> Result<Computation, i32> {
        let [access, address, SemValue::Literal(literal), error, success] = arguments.as_slice()
        else {
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
        let result = host
            .buffers
            .write_memory(MemoryRuntime::access(access), MemoryRuntime::address(address), &bytes)
            .map(|()| None);
        Ok(MemoryRuntime::finish(result, error, success))
    }

    pub(crate) fn load(
        primitive: PrimitiveType, arguments: Vec<SemValue>, host: &mut HostRuntime,
    ) -> Result<Computation, i32> {
        let [access, address, error, success] = arguments.as_slice() else {
            unreachable!("checked scalar load received invalid arguments")
        };
        let width = match primitive {
            | PrimitiveType::Integer(integer) => integer.bits() / 8,
            | PrimitiveType::Float(FloatType::Float32) => 4,
            | PrimitiveType::Float(FloatType::Float64) => 8,
            | _ => unreachable!("only numeric primitives have scalar loads"),
        };
        let result = host
            .buffers
            .read_memory(
                MemoryRuntime::access(access),
                MemoryRuntime::address(address),
                i64::from(width),
            )
            .map(|bytes| {
                Some(Self::literal(primitive, bytes).expect("checked scalar width").into())
            });
        Ok(MemoryRuntime::finish(result, error, success))
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
}
