use crate::{
    host::HostRuntime,
    syntax::{Computation, Prim, RcValue, RuntimeError, SemValue, Thunk, Value},
};
use std::io::{BufRead, Write};
use zydeco_syntax::{
    BuiltinValueRole, FloatOperation, IntegerOperation, PrimitiveError, PrimitiveOp, PrimitiveType,
    Return,
};

/// Typed access to host operations used to construct the Builtin package.
pub struct BuiltinRuntime;

pub enum BuiltinFailure {
    Exit(i32),
    Runtime(RuntimeError),
}

impl BuiltinRuntime {
    pub fn package_value(role: BuiltinValueRole) -> RcValue {
        let primitive: Computation = Prim { role }.into();
        std::rc::Rc::new(Value::Thunk(Thunk(std::rc::Rc::new(primitive))))
    }

    pub fn invoke(
        role: BuiltinValueRole, args: Vec<SemValue>, input: &mut dyn BufRead,
        output: &mut dyn Write, error_output: &mut dyn Write, argv: &[String],
        host: &mut HostRuntime,
    ) -> Result<Computation, BuiltinFailure> {
        use crate::impls::*;
        use crate::representation::ScalarBytes;
        use BuiltinValueRole as Role;

        if let Some(operation) = PrimitiveOp::from_builtin(role) {
            return Self::arithmetic(operation, args).map_err(BuiltinFailure::Runtime);
        }

        match role {
            | Role::Integer(integer, operation) => match operation {
                | IntegerOperation::Add
                | IntegerOperation::Sub
                | IntegerOperation::Mul
                | IntegerOperation::Div
                | IntegerOperation::Mod => {
                    unreachable!("arithmetic roles dispatch through PrimitiveOp")
                }
                | IntegerOperation::Eq | IntegerOperation::Lt | IntegerOperation::Gt => {
                    integer_branch(integer, operation, args)
                }
                | IntegerOperation::ToString => integer_to_string(integer, args),
                | IntegerOperation::ToLeBytes => ScalarBytes::encode(args),
                | IntegerOperation::FromLeBytes => {
                    ScalarBytes::decode(PrimitiveType::Integer(integer), args)
                }
            },
            | Role::Float(float, operation) => match operation {
                | FloatOperation::Add
                | FloatOperation::Sub
                | FloatOperation::Mul
                | FloatOperation::Div => {
                    unreachable!("arithmetic roles dispatch through PrimitiveOp")
                }
                | FloatOperation::Eq | FloatOperation::Lt | FloatOperation::Gt => {
                    float_branch(float, operation, args)
                }
                | FloatOperation::ToString => float_to_string(float, args),
                | FloatOperation::ToLeBytes => ScalarBytes::encode(args),
                | FloatOperation::FromLeBytes => {
                    ScalarBytes::decode(PrimitiveType::Float(float), args)
                }
            },
            | Role::StrScalarLength => str_scalar_length(args, input, output, argv, host),
            | Role::StrByteLength => str_byte_length(args, input, output, argv, host),
            | Role::StrAppend => str_append(args, input, output, argv, host),
            | Role::StrSplitOnce => str_split_once_branch(args, input, output, argv, host),
            | Role::StrSplitAt => str_split_at_branch(args, input, output, argv, host),
            | Role::StrEq => str_eq_branch(args, input, output, argv, host),
            | Role::StrGet => str_get_branch(args, input, output, argv, host),
            | Role::CharToStr => char_to_str(args, input, output, argv, host),
            | Role::CharCodepoint => char_codepoint(args, input, output, argv, host),
            | Role::CharFromCodepoint => {
                char_from_codepoint_branch(args, input, output, argv, host)
            }
            | Role::StrParseInt => str_parse_int_branch(args, input, output, argv, host),
            | Role::BytesEmpty => bytes_empty(args, input, output, argv, host),
            | Role::BytesLength => bytes_length(args, input, output, argv, host),
            | Role::BytesAppend => bytes_append(args, input, output, argv, host),
            | Role::BytesFromStr => bytes_from_str(args, input, output, argv, host),
            | Role::BytesToStr => bytes_to_str_branch(args, input, output, argv, host),
            | Role::BytesGet => bytes_get_branch(args, input, output, argv, host),
            | Role::BytesSlice => bytes_slice_branch(args, input, output, argv, host),
            | Role::BytesAligned => ScalarBytes::aligned(args),
            | Role::BytesSingleton => bytes_singleton(args, input, output, argv, host),
            | Role::BytesEq => bytes_eq_branch(args, input, output, argv, host),
            | Role::BytesLt => bytes_lt_branch(args, input, output, argv, host),
            | Role::BufferAllocate
            | Role::BufferWrite
            | Role::BufferRead
            | Role::BufferFreeze
            | Role::BufferClose => crate::buffer::BufferRuntime::invoke(role, args, host),
            | Role::Stdin => stdin(args, input, output, argv, host),
            | Role::Stdout => stdout(args, input, output, argv, host),
            | Role::Stderr => stderr(args, input, output, argv, host),
            | Role::IoRead => io_read(args, input, output, argv, host),
            | Role::IoReadLine => io_read_line(args, input, output, argv, host),
            | Role::IoReadAll => io_read_all(args, input, output, argv, host),
            | Role::IoWriteAll => io_write_all(args, input, output, error_output, argv, host),
            | Role::IoFlush => io_flush(args, input, output, error_output, argv, host),
            | Role::IoCloseReader => io_close_reader(args, input, output, argv, host),
            | Role::IoCloseWriter => io_close_writer(args, input, output, error_output, argv, host),
            | Role::FsOpenReader => fs_open_reader(args, input, output, argv, host),
            | Role::FsCreateWriter => fs_create_writer(args, input, output, argv, host),
            | Role::FsAppendWriter => fs_append_writer(args, input, output, argv, host),
            | Role::WriteStr => write_str(args, input, output, argv, host),
            | Role::WriteInt => write_int(args, input, output, argv, host),
            | Role::WriteLine => write_line(args, input, output, argv, host),
            | Role::ReadLine => read_line(args, input, output, argv, host),
            | Role::ReadLineAsInt => read_line_as_int_branch(args, input, output, argv, host),
            | Role::ReadTillEof => read_till_eof(args, input, output, argv, host),
            | Role::ArgAt => arg_at(args, input, output, argv, host),
            | Role::RandomInt => random_int(args, input, output, argv, host),
            | Role::Exit => exit(args, input, output, argv, host),
        }
        .map_err(BuiltinFailure::Exit)
    }

    fn arithmetic(
        operation: PrimitiveOp, args: Vec<SemValue>,
    ) -> Result<Computation, RuntimeError> {
        let [SemValue::Literal(first), SemValue::Literal(second)] = args.as_slice() else {
            unreachable!("checked arithmetic receives two literal operands")
        };
        match operation.evaluate(&[first.clone(), second.clone()]) {
            | Ok(value) => Ok(Return(std::rc::Rc::new(Value::Lit(value))).into()),
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
            IntegerType::UInt8,
            IntegerType::UInt16,
            IntegerType::UInt32,
            IntegerType::UInt64,
        ] {
            for operation in [IntegerOperation::Div, IntegerOperation::Mod] {
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
                        BuiltinValueRole::Integer(integer, operation),
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
                            IntegerOperation::Div,
                            Err(BuiltinFailure::Runtime(RuntimeError::IntegerDivisionByZero)),
                        )
                        | (
                            0,
                            IntegerOperation::Mod,
                            Err(BuiltinFailure::Runtime(RuntimeError::IntegerRemainderByZero)),
                        ) => {}
                        | (2, operation, Ok(Computation::Ret(Return(value)))) => {
                            let Value::Lit(Literal::Integer(value)) = value.as_ref() else {
                                panic!("arithmetic must return an integer literal")
                            };
                            let expected = if operation == IntegerOperation::Div { 3 } else { 1 };
                            assert_eq!(*value, IntegerLiteral::from_value(expected, integer));
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
