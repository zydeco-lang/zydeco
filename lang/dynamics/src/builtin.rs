use crate::{
    host::HostRuntime,
    syntax::{Computation, Prim, RcValue, SemValue, Thunk, Value},
};
use std::io::{BufRead, Write};
use zydeco_syntax::{BuiltinValueRole, FloatOperation, IntegerOperation};

/// Typed access to host operations used to construct the Builtin package.
pub struct BuiltinRuntime;

impl BuiltinRuntime {
    pub fn package_value(role: BuiltinValueRole) -> RcValue {
        let primitive: Computation = Prim { arity: role.arity() as u64, role }.into();
        std::rc::Rc::new(Value::Thunk(Thunk(std::rc::Rc::new(primitive))))
    }

    pub fn invoke(
        role: BuiltinValueRole, args: Vec<SemValue>, input: &mut dyn BufRead,
        output: &mut dyn Write, argv: &[String], host: &mut HostRuntime,
    ) -> Result<Computation, i32> {
        use crate::impls::*;
        use BuiltinValueRole as Role;

        match role {
            | Role::Integer(integer, operation) => match operation {
                | IntegerOperation::Add
                | IntegerOperation::Sub
                | IntegerOperation::Mul
                | IntegerOperation::Div
                | IntegerOperation::Mod => integer_arithmetic(integer, operation, args),
                | IntegerOperation::Eq | IntegerOperation::Lt | IntegerOperation::Gt => {
                    integer_branch(integer, operation, args)
                }
                | IntegerOperation::ToString => integer_to_string(integer, args),
            },
            | Role::Float(float, operation) => match operation {
                | FloatOperation::Add
                | FloatOperation::Sub
                | FloatOperation::Mul
                | FloatOperation::Div => float_arithmetic(float, operation, args),
                | FloatOperation::Eq | FloatOperation::Lt | FloatOperation::Gt => {
                    float_branch(float, operation, args)
                }
                | FloatOperation::ToString => float_to_string(float, args),
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
            | Role::BytesSingleton => bytes_singleton(args, input, output, argv, host),
            | Role::BytesEq => bytes_eq_branch(args, input, output, argv, host),
            | Role::BytesLt => bytes_lt_branch(args, input, output, argv, host),
            | Role::Stdin => stdin(args, input, output, argv, host),
            | Role::Stdout => stdout(args, input, output, argv, host),
            | Role::Stderr => stderr(args, input, output, argv, host),
            | Role::IoRead => io_read(args, input, output, argv, host),
            | Role::IoReadLine => io_read_line(args, input, output, argv, host),
            | Role::IoReadAll => io_read_all(args, input, output, argv, host),
            | Role::IoWriteAll => io_write_all(args, input, output, argv, host),
            | Role::IoFlush => io_flush(args, input, output, argv, host),
            | Role::IoCloseReader => io_close_reader(args, input, output, argv, host),
            | Role::IoCloseWriter => io_close_writer(args, input, output, argv, host),
            | Role::FsOpenReader => fs_open_reader(args, input, output, argv, host),
            | Role::FsCreateWriter => fs_create_writer(args, input, output, argv, host),
            | Role::FsAppendWriter => fs_append_writer(args, input, output, argv, host),
            | Role::WriteStr => write_str(args, input, output, argv, host),
            | Role::WriteInt => write_int(args, input, output, argv, host),
            | Role::WriteLine => write_line(args, input, output, argv, host),
            | Role::ReadLine => read_line(args, input, output, argv, host),
            | Role::ReadLineAsInt => read_line_as_int_branch(args, input, output, argv, host),
            | Role::ReadTillEof => read_till_eof(args, input, output, argv, host),
            | Role::ArgList => arg_fold(args, input, output, argv, host),
            | Role::RandomInt => random_int(args, input, output, argv, host),
            | Role::Exit => exit(args, input, output, argv, host),
        }
    }
}
