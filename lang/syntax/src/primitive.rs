//! Typed arithmetic for static value calculation and instruction selection.

use crate::{
    BuiltinValueRole, FloatLiteral, FloatOperation, FloatType, IntegerLiteral, IntegerOperation,
    IntegerType, Literal,
};

/// Total integer leaves for value computation. Layout laws are source functions.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ValueInt64Op {
    Add,
    Sub,
    And,
    Compare,
}

impl ValueInt64Op {
    pub const ALL: &[Self] = &[Self::Add, Self::Sub, Self::And, Self::Compare];

    pub fn intrinsic_name(self) -> &'static str {
        match self {
            | Self::Add => "i64_add",
            | Self::Sub => "i64_sub",
            | Self::And => "i64_and",
            | Self::Compare => "i64_compare",
        }
    }

    pub fn evaluate(self, [left, right]: [i64; 2]) -> i64 {
        match self {
            | Self::Add => left.wrapping_add(right),
            | Self::Sub => left.wrapping_sub(right),
            | Self::And => left & right,
            | Self::Compare => match left.cmp(&right) {
                | std::cmp::Ordering::Less => -1,
                | std::cmp::Ordering::Equal => 0,
                | std::cmp::Ordering::Greater => 1,
            },
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum IntegerArithmetic {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum FloatArithmetic {
    Add,
    Sub,
    Mul,
    Div,
}

/// A returning scalar operation. Its two operands and result have the stated type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum PrimitiveOp {
    Integer(IntegerType, IntegerArithmetic),
    Float(FloatType, FloatArithmetic),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum PrimitiveError {
    #[error("integer division by zero")]
    DivisionByZero,
    #[error("integer remainder by zero")]
    RemainderByZero,
    #[error("primitive operands do not match the operation's scalar type")]
    OperandType,
}

impl PrimitiveOp {
    pub fn from_builtin(role: BuiltinValueRole) -> Option<Self> {
        Some(match role {
            | BuiltinValueRole::Integer(ty, op) => Self::Integer(
                ty,
                match op {
                    | IntegerOperation::Add => IntegerArithmetic::Add,
                    | IntegerOperation::Sub => IntegerArithmetic::Sub,
                    | IntegerOperation::Mul => IntegerArithmetic::Mul,
                    | IntegerOperation::Div => IntegerArithmetic::Div,
                    | IntegerOperation::Mod => IntegerArithmetic::Mod,
                    | _ => return None,
                },
            ),
            | BuiltinValueRole::Float(ty, op) => Self::Float(
                ty,
                match op {
                    | FloatOperation::Add => FloatArithmetic::Add,
                    | FloatOperation::Sub => FloatArithmetic::Sub,
                    | FloatOperation::Mul => FloatArithmetic::Mul,
                    | FloatOperation::Div => FloatArithmetic::Div,
                    | _ => return None,
                },
            ),
            | _ => return None,
        })
    }

    pub fn builtin(self) -> BuiltinValueRole {
        match self {
            | Self::Integer(ty, op) => BuiltinValueRole::Integer(
                ty,
                match op {
                    | IntegerArithmetic::Add => IntegerOperation::Add,
                    | IntegerArithmetic::Sub => IntegerOperation::Sub,
                    | IntegerArithmetic::Mul => IntegerOperation::Mul,
                    | IntegerArithmetic::Div => IntegerOperation::Div,
                    | IntegerArithmetic::Mod => IntegerOperation::Mod,
                },
            ),
            | Self::Float(ty, op) => BuiltinValueRole::Float(
                ty,
                match op {
                    | FloatArithmetic::Add => FloatOperation::Add,
                    | FloatArithmetic::Sub => FloatOperation::Sub,
                    | FloatArithmetic::Mul => FloatOperation::Mul,
                    | FloatArithmetic::Div => FloatOperation::Div,
                },
            ),
        }
    }

    pub fn may_trap(self) -> bool {
        matches!(self, Self::Integer(_, IntegerArithmetic::Div | IntegerArithmetic::Mod))
    }

    /// Evaluate only literal arithmetic. A trapping operation remains residual
    /// during normalization; interpreters report the same failure at execution.
    pub fn evaluate(self, operands: &[Literal; 2]) -> Result<Literal, PrimitiveError> {
        match (self, operands) {
            | (Self::Integer(ty, op), [Literal::Integer(first), Literal::Integer(second)])
                if first.integer_type() == Some(ty) && second.integer_type() == Some(ty) =>
            {
                if second.value() == 0 {
                    match op {
                        | IntegerArithmetic::Div => return Err(PrimitiveError::DivisionByZero),
                        | IntegerArithmetic::Mod => return Err(PrimitiveError::RemainderByZero),
                        | _ => {}
                    }
                }
                macro_rules! integer {
                    ($variant:ident, $ty:ty) => {{
                        let first = first.value() as $ty;
                        let second = second.value() as $ty;
                        IntegerLiteral::$variant(match op {
                            | IntegerArithmetic::Add => first.wrapping_add(second),
                            | IntegerArithmetic::Sub => first.wrapping_sub(second),
                            | IntegerArithmetic::Mul => first.wrapping_mul(second),
                            | IntegerArithmetic::Div => first.wrapping_div(second),
                            | IntegerArithmetic::Mod => first.wrapping_rem(second),
                        })
                    }};
                }
                Ok(Literal::Integer(match ty {
                    | IntegerType::Int8 => integer!(Int8, i8),
                    | IntegerType::Int16 => integer!(Int16, i16),
                    | IntegerType::Int32 => integer!(Int32, i32),
                    | IntegerType::Int64 => integer!(Int64, i64),
                    | IntegerType::UInt8 => integer!(UInt8, u8),
                    | IntegerType::UInt16 => integer!(UInt16, u16),
                    | IntegerType::UInt32 => integer!(UInt32, u32),
                    | IntegerType::UInt64 => integer!(UInt64, u64),
                }))
            }
            | (Self::Float(ty, op), [Literal::Float(first), Literal::Float(second)])
                if first.float_type() == ty && second.float_type() == ty =>
            {
                macro_rules! float {
                    ($ty:ty, $constructor:ident) => {{
                        let first = first.value() as $ty;
                        let second = second.value() as $ty;
                        let result = match op {
                            | FloatArithmetic::Add => first + second,
                            | FloatArithmetic::Sub => first - second,
                            | FloatArithmetic::Mul => first * second,
                            | FloatArithmetic::Div => first / second,
                        };
                        FloatLiteral::$constructor(result.to_bits())
                    }};
                }
                Ok(Literal::Float(match ty {
                    | FloatType::Float32 => float!(f32, from_f32_bits),
                    | FloatType::Float64 => float!(f64, from_bits),
                }))
            }
            | _ => Err(PrimitiveError::OperandType),
        }
    }
}

impl std::fmt::Display for PrimitiveOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.builtin().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use strum::VariantArray;

    #[test]
    fn integer_folding_wraps_at_the_selected_width() {
        for &ty in IntegerType::VARIANTS {
            let literal = |value| Literal::Integer(IntegerLiteral::from_value(value, ty));
            let bits = ty.bits();
            let (min, max) = if ty.is_signed() {
                (-(1_i128 << (bits - 1)), (1_i128 << (bits - 1)) - 1)
            } else {
                (0, (1_i128 << bits) - 1)
            };
            for (op, first, second, expected) in [
                (IntegerArithmetic::Add, max, 1, min),
                (IntegerArithmetic::Sub, min, 1, max),
                (IntegerArithmetic::Mul, max, 2, if ty.is_signed() { -2 } else { max - 1 }),
                (IntegerArithmetic::Div, 20, 3, 6),
                (IntegerArithmetic::Mod, 20, 3, 2),
            ] {
                let operation = PrimitiveOp::Integer(ty, op);
                assert_eq!(
                    operation.evaluate(&[literal(first), literal(second)]),
                    Ok(literal(expected)),
                    "{operation}"
                );
            }
            if ty.is_signed() {
                assert_eq!(
                    PrimitiveOp::Integer(ty, IntegerArithmetic::Div)
                        .evaluate(&[literal(min), literal(-1)]),
                    Ok(literal(min))
                );
                assert_eq!(
                    PrimitiveOp::Integer(ty, IntegerArithmetic::Mod)
                        .evaluate(&[literal(min), literal(-1)]),
                    Ok(literal(0))
                );
            }
        }
    }

    #[test]
    fn integer_folding_rejects_zero_divisors_and_mismatched_types() {
        for &ty in IntegerType::VARIANTS {
            let literal = |value| Literal::Integer(IntegerLiteral::from_value(value, ty));
            for (op, error) in [
                (IntegerArithmetic::Div, PrimitiveError::DivisionByZero),
                (IntegerArithmetic::Mod, PrimitiveError::RemainderByZero),
            ] {
                let operation = PrimitiveOp::Integer(ty, op);
                assert_eq!(operation.evaluate(&[literal(7), literal(0)]), Err(error));
                assert!(operation.evaluate(&[literal(7), literal(2)]).is_ok());
                assert_eq!(
                    operation.evaluate(&[literal(7), Literal::Float(FloatLiteral::from(2.0))]),
                    Err(PrimitiveError::OperandType)
                );
            }
        }
        assert_eq!(
            PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Add)
                .evaluate(&[IntegerLiteral::Int64(1).into(), IntegerLiteral::Int32(2).into()]),
            Err(PrimitiveError::OperandType)
        );
    }

    #[test]
    fn float_folding_preserves_precision_signed_zero_and_infinities() {
        for &ty in FloatType::VARIANTS {
            let literal = |value| Literal::Float(FloatLiteral::from(value).with_type(ty).unwrap());
            let add = PrimitiveOp::Float(ty, FloatArithmetic::Add);
            assert_eq!(add.evaluate(&[literal(-0.0), literal(-0.0)]), Ok(literal(-0.0)));
            let expected = if ty == FloatType::Float32 {
                f64::from(0.1_f32 + 0.2_f32)
            } else {
                0.1_f64 + 0.2_f64
            };
            assert_eq!(add.evaluate(&[literal(0.1), literal(0.2)]), Ok(literal(expected)));
            let division = PrimitiveOp::Float(ty, FloatArithmetic::Div);
            // Construct infinity directly: literal type resolution rejects overflow.
            let infinity = match ty {
                | FloatType::Float32 => FloatLiteral::from_f32_bits(f32::INFINITY.to_bits()),
                | FloatType::Float64 => FloatLiteral::from(f64::INFINITY),
            };
            assert_eq!(division.evaluate(&[literal(1.0), literal(0.0)]), Ok(infinity.into()));
            let Literal::Float(nan) = division.evaluate(&[literal(0.0), literal(0.0)]).unwrap()
            else {
                panic!("float division must return a float")
            };
            assert!(nan.value().is_nan());
            assert_eq!(
                add.evaluate(&[literal(1.0), IntegerLiteral::Int64(2).into()]),
                Err(PrimitiveError::OperandType)
            );
        }
    }
}
