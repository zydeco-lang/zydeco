use crate::{
    host::HostRuntime,
    syntax::{HostAction, RuntimeError, SemValue, ThunkBody},
};
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_syntax::{
    BuiltinValueRole, ComparisonOp, FloatIntrinsicOp, HostOp, IntegerIntrinsicOp, IntegerType,
    IntrinsicOp, PrimitiveError, PrimitiveOp, PrimitiveType, ScalarMemoryOp,
};

/// Typed access to host operations used to construct the Builtin package and intrinsic thunks.
pub struct BuiltinRuntime;

pub enum BuiltinFailure {
    Exit(i32),
    Runtime(RuntimeError),
}

impl BuiltinRuntime {
    pub fn package_value(role: BuiltinValueRole) -> SemValue {
        SemValue::Thunk(Rc::new(ThunkBody::Host(HostOp::Builtin(role))))
    }

    pub fn intrinsic_value(op: IntrinsicOp) -> SemValue {
        SemValue::Thunk(Rc::new(ThunkBody::Host(HostOp::Intrinsic(op))))
    }

    pub fn invoke(
        op: HostOp, args: Vec<SemValue>, input: &mut dyn BufRead, output: &mut dyn Write,
        error_output: &mut dyn Write, argv: &[String], host: &mut HostRuntime,
    ) -> Result<HostAction, BuiltinFailure> {
        use crate::impls::*;
        use crate::representation::ScalarMemory;
        use BuiltinValueRole as Role;

        if let Some(operation) = PrimitiveOp::from_host(op) {
            return Self::arithmetic(operation, args).map_err(BuiltinFailure::Runtime);
        }

        if let Some(operation) = ComparisonOp::from_host(op) {
            return Branch::scalar(operation, args).map_err(BuiltinFailure::Exit);
        }

        match op {
            | HostOp::Intrinsic(intrinsic) => match intrinsic {
                | IntrinsicOp::Integer(
                    _,
                    IntegerIntrinsicOp::Add
                    | IntegerIntrinsicOp::Sub
                    | IntegerIntrinsicOp::Mul
                    | IntegerIntrinsicOp::Div
                    | IntegerIntrinsicOp::Mod,
                )
                | IntrinsicOp::Float(
                    _,
                    FloatIntrinsicOp::Add
                    | FloatIntrinsicOp::Sub
                    | FloatIntrinsicOp::Mul
                    | FloatIntrinsicOp::Div,
                ) => {
                    unreachable!("arithmetic ops dispatch through PrimitiveOp")
                }
                | IntrinsicOp::Integer(
                    _,
                    IntegerIntrinsicOp::Eq | IntegerIntrinsicOp::Lt | IntegerIntrinsicOp::Gt,
                )
                | IntrinsicOp::Float(
                    _,
                    FloatIntrinsicOp::Eq | FloatIntrinsicOp::Lt | FloatIntrinsicOp::Gt,
                ) => {
                    unreachable!("comparisons dispatch through ComparisonOp")
                }
                | IntrinsicOp::Integer(integer, IntegerIntrinsicOp::ToString) => {
                    integer_to_string(integer, args)
                }
                | IntrinsicOp::Float(float, FloatIntrinsicOp::ToString) => {
                    float_to_string(float, args)
                }
                | IntrinsicOp::Int64FromInt => IntegerConversion::widen(IntegerType::Int64, args),
                | IntrinsicOp::UInt64FromUInt => {
                    IntegerConversion::widen(IntegerType::UInt64, args)
                }
                | IntrinsicOp::Int64ToInt => IntegerConversion::narrow(IntegerType::Int, args),
                | IntrinsicOp::UInt64ToUInt => IntegerConversion::narrow(IntegerType::UInt, args),
                | IntrinsicOp::CharToString => char_to_str(args),
                | IntrinsicOp::CharCodepoint => char_codepoint(args),
                | IntrinsicOp::CharFromCodepoint => char_from_codepoint_branch(args),
                | IntrinsicOp::StringLength => str_scalar_length(args),
                | IntrinsicOp::StringByteLength => str_byte_length(args),
                | IntrinsicOp::StringAppend => str_append(args),
                | IntrinsicOp::StringSplitOnce => str_split_once_branch(args),
                | IntrinsicOp::StringSplitAt => str_split_at_branch(args),
                | IntrinsicOp::StringEq => str_eq_branch(args),
                | IntrinsicOp::StringGet => str_get_branch(args),
                | IntrinsicOp::StringParseInt => str_parse_int_branch(args),
            },
            | HostOp::Builtin(role) => match role {
                | Role::Integer(integer, operation) => match operation {
                    | ScalarMemoryOp::StoreLe => ScalarMemory::store(args),
                    | ScalarMemoryOp::LoadLe => {
                        return ScalarMemory::load(PrimitiveType::Integer(integer), args)
                            .map_err(BuiltinFailure::Runtime);
                    }
                },
                | Role::Float(float, operation) => match operation {
                    | ScalarMemoryOp::StoreLe => ScalarMemory::store(args),
                    | ScalarMemoryOp::LoadLe => {
                        return ScalarMemory::load(PrimitiveType::Float(float), args)
                            .map_err(BuiltinFailure::Runtime);
                    }
                },
                | Role::MemoryCopy
                | Role::MemoryFill
                | Role::MemoryNull
                | Role::MemoryAllocate
                | Role::MemoryFree
                | Role::MemoryRetain
                | Role::MemoryFromString
                | Role::MemoryToString
                | Role::MemoryOffset
                | Role::MemoryLoadAddr
                | Role::MemoryStoreAddr => crate::memory::MemoryRuntime::invoke(role, args, host),
                | Role::Stdin => stdin(args),
                | Role::Stdout => stdout(args),
                | Role::Stderr => stderr(args),
                | Role::IoRead => io_read(args, input, host),
                | Role::IoReadLine => io_read_line(args, input, host),
                | Role::IoReadAll => io_read_all(args, input, host),
                | Role::IoWriteAll => io_write_all(args, output, error_output, host),
                | Role::IoFlush => io_flush(args, output, error_output, host),
                | Role::IoCloseReader => io_close_reader(args, host),
                | Role::IoCloseWriter => io_close_writer(args, output, error_output, host),
                | Role::FsOpenReader => fs_open_reader(args, host),
                | Role::FsCreateWriter => fs_create_writer(args, host),
                | Role::FsAppendWriter => fs_append_writer(args, host),
                | Role::WriteStr => write_str(args, output),
                | Role::WriteInt => write_int(args, output),
                | Role::WriteLine => write_line(args, output),
                | Role::ReadLine => read_line(args, input, host),
                | Role::ReadLineAsInt => read_line_as_int_branch(args, input, host),
                | Role::ReadTillEof => read_till_eof(args, input, host),
                | Role::ArgAt => arg_at(args, argv),
                | Role::RandomInt => random_int(args),
                | Role::Exit => exit(args),
            },
        }
        .map_err(BuiltinFailure::Exit)
    }

    fn arithmetic(operation: PrimitiveOp, args: Vec<SemValue>) -> Result<HostAction, RuntimeError> {
        let [SemValue::Literal(first), SemValue::Literal(second)] = args.as_slice() else {
            unreachable!("checked arithmetic receives two literal operands")
        };
        match operation.evaluate(&[first.clone(), second.clone()]) {
            | Ok(value) => Ok(HostAction::Return(SemValue::Literal(value))),
            | Err(PrimitiveError::DivisionByZero) => Err(RuntimeError::IntegerDivisionByZero),
            | Err(PrimitiveError::RemainderByZero) => Err(RuntimeError::IntegerRemainderByZero),
            | Err(PrimitiveError::OperandType) => {
                unreachable!("checked arithmetic receives operands of its declared type")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_syntax::{IntegerLiteral, IntegerType, Literal};

    #[test]
    fn arithmetic_reports_zero_divisors_without_host_effects() {
        for integer in [
            IntegerType::Int8,
            IntegerType::Int16,
            IntegerType::Int32,
            IntegerType::Int64,
            IntegerType::Int,
            IntegerType::UInt8,
            IntegerType::UInt16,
            IntegerType::UInt32,
            IntegerType::UInt64,
            IntegerType::UInt,
        ] {
            for operation in [IntegerIntrinsicOp::Div, IntegerIntrinsicOp::Mod] {
                for divisor in [0, 2] {
                    let args = [7, divisor]
                        .map(|value| {
                            SemValue::Literal(Literal::Integer(IntegerLiteral::from_value(
                                value, integer,
                            )))
                        })
                        .to_vec();
                    let mut input = std::io::Cursor::new(b"unread");
                    let mut output = Vec::new();
                    let mut error_output = Vec::new();
                    let result = BuiltinRuntime::invoke(
                        HostOp::Intrinsic(IntrinsicOp::Integer(integer, operation)),
                        args,
                        &mut input,
                        &mut output,
                        &mut error_output,
                        &[],
                        &mut HostRuntime::new(),
                    );
                    match (divisor, operation, result) {
                        | (
                            0,
                            IntegerIntrinsicOp::Div,
                            Err(BuiltinFailure::Runtime(RuntimeError::IntegerDivisionByZero)),
                        )
                        | (
                            0,
                            IntegerIntrinsicOp::Mod,
                            Err(BuiltinFailure::Runtime(RuntimeError::IntegerRemainderByZero)),
                        ) => {}
                        | (
                            2,
                            operation,
                            Ok(HostAction::Return(SemValue::Literal(Literal::Integer(value)))),
                        ) => {
                            let expected = if operation == IntegerIntrinsicOp::Div { 3 } else { 1 };
                            assert_eq!(value, IntegerLiteral::from_value(expected, integer));
                        }
                        | _ => panic!("arithmetic returned the wrong result or failure"),
                    }
                    assert_eq!(input.position(), 0);
                    assert!(output.is_empty());
                    assert!(error_output.is_empty());
                }
            }
        }
    }
}
