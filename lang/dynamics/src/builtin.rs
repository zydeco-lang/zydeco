use crate::{
    host::HostRuntime,
    syntax::{Computation, Prim, RcValue, RuntimeError, SemValue, Thunk, Value},
};
use std::io::{BufRead, Write};
use zydeco_syntax::{
    BuiltinValueRole, FloatOperation, IntegerOperation, IntegerType, PrimitiveError, PrimitiveOp,
    PrimitiveType, Return,
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
        use crate::representation::ScalarMemory;
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
                | IntegerOperation::StoreLe => ScalarMemory::store(args),
                | IntegerOperation::LoadLe => {
                    return ScalarMemory::load(PrimitiveType::Integer(integer), args)
                        .map_err(BuiltinFailure::Runtime);
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
                | FloatOperation::StoreLe => ScalarMemory::store(args),
                | FloatOperation::LoadLe => {
                    return ScalarMemory::load(PrimitiveType::Float(float), args)
                        .map_err(BuiltinFailure::Runtime);
                }
            },
            | Role::Int64FromInt => IntegerConversion::widen(IntegerType::Int64, args),
            | Role::UInt64FromUInt => IntegerConversion::widen(IntegerType::UInt64, args),
            | Role::Int64ToInt => IntegerConversion::narrow(IntegerType::Int, args),
            | Role::UInt64ToUInt => IntegerConversion::narrow(IntegerType::UInt, args),
            | Role::StrScalarLength => str_scalar_length(args),
            | Role::StrByteLength => str_byte_length(args),
            | Role::StrAppend => str_append(args),
            | Role::StrSplitOnce => str_split_once_branch(args),
            | Role::StrSplitAt => str_split_at_branch(args),
            | Role::StrEq => str_eq_branch(args),
            | Role::StrGet => str_get_branch(args),
            | Role::CharToStr => char_to_str(args),
            | Role::CharCodepoint => char_codepoint(args),
            | Role::CharFromCodepoint => char_from_codepoint_branch(args),
            | Role::StrParseInt => str_parse_int_branch(args),
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
            IntegerType::Int,
            IntegerType::UInt8,
            IntegerType::UInt16,
            IntegerType::UInt32,
            IntegerType::UInt64,
            IntegerType::UInt,
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
