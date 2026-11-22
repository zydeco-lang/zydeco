use crate::{
    host::{HostIoErrorKind, HostRuntime, HostValue, ReaderHandle, WriterHandle},
    syntax::*,
};
use std::io::{self, BufRead, Write};
use zydeco_host::{io::LineInput, random::random_signed_int, string as host_str};

type ZValue = SemValue;

fn ret<E>(value: ZValue) -> Result<HostAction, E> {
    Ok(HostAction::Return(value))
}

fn resume<E>(continuation: &ZValue, arguments: impl Into<Vec<ZValue>>) -> Result<HostAction, E> {
    Ok(HostAction::Resume { continuation: continuation.clone(), arguments: arguments.into() })
}

pub(crate) struct Branch;

impl Branch {
    fn select(condition: bool, when_true: &ZValue, when_false: &ZValue) -> Result<HostAction, i32> {
        let selected = if condition { when_true } else { when_false };
        resume(selected, [])
    }

    /// Select one continuation using the same typed semantics as constant folding.
    pub(crate) fn scalar(operation: ComparisonOp, args: Vec<ZValue>) -> Result<HostAction, i32> {
        let [
            ZValue::Literal(first),
            ZValue::Literal(second),
            when_true @ ZValue::Thunk(_),
            when_false @ ZValue::Thunk(_),
        ] = args.as_slice()
        else {
            unreachable!("checked comparison arguments")
        };
        let condition = operation
            .evaluate(&[first.clone(), second.clone()])
            .expect("checked comparison operand types");
        Self::select(condition, when_true, when_false)
    }
}

pub fn integer_to_string(integer_type: IntegerType, args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Integer(value))]
            if value.integer_type() == Some(integer_type) =>
        {
            ret(Literal::String(value.to_string().into()).into())
        }
        | _ => unreachable!("type-checked integer rendering received a mismatched value"),
    }
}

/// Convert a floating-point literal to its shortest round-trippable decimal form.
pub fn float_to_string(float_type: FloatType, args: Vec<ZValue>) -> Result<HostAction, i32> {
    let rendered = match (float_type, args.as_slice()) {
        | (FloatType::Float32, [ZValue::Literal(Literal::Float(FloatLiteral::Float32(bits)))]) => {
            f32::from_bits(*bits).to_string()
        }
        | (FloatType::Float64, [ZValue::Literal(Literal::Float(FloatLiteral::Float64(bits)))]) => {
            f64::from_bits(*bits).to_string()
        }
        | _ => unreachable!("type-checked float rendering received a mismatched value"),
    };
    ret(Literal::String(rendered.into()).into())
}

// /* Strings */
/// Return the number of Unicode scalar values in a string.
pub fn str_scalar_length(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(a))] => {
            ret(Literal::Integer((a.scalar_len() as i64).into()).into())
        }
        | _ => unreachable!(""),
    }
}

/// Return the number of bytes in a string's UTF-8 encoding.
pub fn str_byte_length(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(string))] => {
            ret(Literal::Integer((string.byte_len() as i64).into()).into())
        }
        | _ => unreachable!(""),
    }
}

/// Concatenate two string literals.
pub fn str_append(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(a)), ZValue::Literal(Literal::String(b))] => {
            ret(Literal::String([a.as_str(), b.as_str()].concat().into()).into())
        }
        | _ => unreachable!(""),
    }
}

struct OptionalPairBranch;

impl OptionalPairBranch {
    fn select(
        pair: Option<(Utf8String, Utf8String)>, when_none: &ZValue, when_some: &ZValue,
    ) -> Result<HostAction, i32> {
        match pair {
            | None => resume(when_none, []),
            | Some((first, second)) => resume(
                when_some,
                vec![Literal::String(first).into(), Literal::String(second).into()],
            ),
        }
    }
}

/// Split once and select a computation without constructing a
/// library-defined optional pair.
pub fn str_split_once_branch(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            ZValue::Literal(Literal::Char(separator)),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let pair = string
                .as_str()
                .split_once(*separator)
                .map(|(first, second)| (first.into(), second.into()));
            OptionalPairBranch::select(pair, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

/// Split at an index and select a computation without constructing a
/// library-defined optional pair.
pub fn str_split_at_branch(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int(index))),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let pair = host_str::split_at_scalar_byte(string.as_str(), *index).map(|split| {
                let s = string.as_str();
                (s[..split].into(), s[split..].into())
            });
            OptionalPairBranch::select(pair, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

/// Select a computation according to string equality without constructing a
/// library-defined Boolean value.
pub fn str_eq_branch(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(a)),
            ZValue::Literal(Literal::String(b)),
            when_true @ ZValue::Thunk(_),
            when_false @ ZValue::Thunk(_),
        ] => Branch::select(a == b, when_true, when_false),
        | _ => unreachable!(""),
    }
}

struct OptionalValueBranch;

impl OptionalValueBranch {
    fn select(
        value: Option<ZValue>, when_none: &ZValue, when_some: &ZValue,
    ) -> Result<HostAction, i32> {
        match value {
            | None => resume(when_none, []),
            | Some(value) => resume(when_some, vec![value]),
        }
    }
}

/// Safely index a string by Unicode scalar position and select a continuation.
pub fn str_get_branch(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int(index))),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let character = host_str::scalar_at(string.as_str(), *index)
                .map(|character| Literal::Char(character).into());
            OptionalValueBranch::select(character, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

/// Convert a character literal to a single-character string.
pub fn char_to_str(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Char(a))] => ret(Literal::String((*a).into()).into()),
        | _ => unreachable!(""),
    }
}

/// Convert a character literal to its integer codepoint.
pub fn char_codepoint(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Char(a))] => {
            ret(Literal::Integer((*a as u32 as i64).into()).into())
        }
        | _ => unreachable!(""),
    }
}

/// Validate an integer as a Unicode scalar value and select a continuation.
pub fn char_from_codepoint_branch(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int(codepoint))),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let character = host_str::char_from_codepoint(*codepoint)
                .map(|character| Literal::Char(character).into());
            OptionalValueBranch::select(character, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

/// Parse a string as an integer and select a continuation without panicking.
pub fn str_parse_int_branch(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let integer = host_str::parse_signed_int(string.as_str())
                .and_then(|integer| IntegerLiteral::new(integer.into()).with_type(IntegerType::Int))
                .map(|integer| Literal::Integer(integer).into());
            OptionalValueBranch::select(integer, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

// /* IO */
struct HostContinuation;

impl HostContinuation {
    fn memory(
        host: &mut HostRuntime, bytes: Vec<u8>, error: &ZValue, success: &ZValue,
    ) -> Result<HostAction, i32> {
        match host.import_memory(&bytes) {
            | Ok(access) => Ok(crate::memory::MemoryRuntime::resume(
                success,
                [
                    HostValue::Address(access).into(),
                    crate::memory::MemoryRuntime::int_value(bytes.len() as i64),
                ],
            )),
            | Err(fault) => Self::io_error(error, fault),
        }
    }

    fn force(continuation: &ZValue) -> HostAction {
        HostAction::Resume { continuation: continuation.clone(), arguments: Vec::new() }
    }

    fn one(continuation: &ZValue, argument: ZValue) -> HostAction {
        HostAction::Resume { continuation: continuation.clone(), arguments: vec![argument] }
    }

    fn two(continuation: &ZValue, first: ZValue, second: ZValue) -> HostAction {
        HostAction::Resume { continuation: continuation.clone(), arguments: vec![first, second] }
    }

    fn io_error(continuation: &ZValue, error: io::Error) -> Result<HostAction, i32> {
        let kind = Literal::Integer((HostIoErrorKind::from_io_error(&error) as i64).into()).into();
        let message = Literal::String(error.to_string().into()).into();
        Ok(Self::two(continuation, kind, message))
    }
}

/// Return the interpreter's injected standard-input capability.
pub fn stdin(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [] => ret(HostValue::Reader(ReaderHandle::STDIN).into()),
        | _ => unreachable!(""),
    }
}

/// Return the interpreter's injected standard-output capability.
pub fn stdout(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [] => ret(HostValue::Writer(WriterHandle::STDOUT).into()),
        | _ => unreachable!(""),
    }
}

/// Return the interpreter's injected standard-error capability.
pub fn stderr(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [] => ret(HostValue::Writer(WriterHandle::STDERR).into()),
        | _ => unreachable!(""),
    }
}

/// Read at most the requested number of bytes from a capability.
pub fn io_read(
    args: Vec<ZValue>, input: &mut dyn BufRead, host: &mut HostRuntime,
) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Reader(reader)),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int(count))),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => {
            match host.io.read_with(*reader, input, |reader| LineInput::read_up_to(reader, *count))
            {
                | Ok(bytes) => HostContinuation::memory(host, bytes, when_error, when_success),
                | Err(error) => HostContinuation::io_error(when_error, error),
            }
        }
        | _ => unreachable!(""),
    }
}

/// Read one byte line, distinguishing EOF from failure.
pub fn io_read_line(
    args: Vec<ZValue>, input: &mut dyn BufRead, host: &mut HostRuntime,
) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Reader(reader)),
            when_error @ ZValue::Thunk(_),
            when_eof @ ZValue::Thunk(_),
            when_line @ ZValue::Thunk(_),
        ] => match host.io.read_with(*reader, input, LineInput::read_line_bytes) {
            | Ok((0, _)) => Ok(HostContinuation::force(when_eof)),
            | Ok((_, bytes)) => HostContinuation::memory(host, bytes, when_error, when_line),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

/// Read all remaining bytes from a capability.
pub fn io_read_all(
    args: Vec<ZValue>, input: &mut dyn BufRead, host: &mut HostRuntime,
) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Reader(reader)),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => match host.io.read_with(*reader, input, LineInput::read_all) {
            | Ok(bytes) => HostContinuation::memory(host, bytes, when_error, when_success),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

/// Write an entire byte buffer to a capability.
pub fn io_write_all(
    args: Vec<ZValue>, output: &mut dyn Write, stderr: &mut dyn Write, host: &mut HostRuntime,
) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Writer(writer)),
            ZValue::Host(HostValue::Address(address)),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int(length))),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => match host.write_memory(*writer, *address, *length, output, stderr) {
            | Ok(()) => Ok(HostContinuation::force(when_success)),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

/// Flush buffered data through a writable capability.
pub fn io_flush(
    args: Vec<ZValue>, output: &mut dyn Write, stderr: &mut dyn Write, host: &mut HostRuntime,
) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Writer(writer)),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => match host.io.write_with(*writer, output, stderr, |writer| writer.flush()) {
            | Ok(()) => Ok(HostContinuation::force(when_success)),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

/// Close a readable capability, preserving standard input as a process resource.
pub fn io_close_reader(args: Vec<ZValue>, host: &mut HostRuntime) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Reader(reader)),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => match host.close_reader(*reader) {
            | Ok(()) => Ok(HostContinuation::force(when_success)),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

/// Close a writable capability after flushing it.
pub fn io_close_writer(
    args: Vec<ZValue>, output: &mut dyn Write, stderr: &mut dyn Write, host: &mut HostRuntime,
) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Writer(writer)),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => match host.close_writer(*writer, output, stderr) {
            | Ok(()) => Ok(HostContinuation::force(when_success)),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

struct FileIo;

impl FileIo {
    fn open(
        args: &[ZValue], operation: impl FnOnce(&mut HostRuntime, &str) -> io::Result<ZValue>,
        host: &mut HostRuntime,
    ) -> Result<HostAction, i32> {
        match args {
            | [
                ZValue::Literal(Literal::String(path)),
                when_error @ ZValue::Thunk(_),
                when_success @ ZValue::Thunk(_),
            ] => match operation(host, path.as_str()) {
                | Ok(capability) => Ok(HostContinuation::one(when_success, capability)),
                | Err(error) => HostContinuation::io_error(when_error, error),
            },
            | _ => unreachable!(""),
        }
    }
}

/// Open an existing file for buffered reading.
pub fn fs_open_reader(args: Vec<ZValue>, host: &mut HostRuntime) -> Result<HostAction, i32> {
    FileIo::open(
        &args,
        |host, path| host.open_reader(path).map(|handle| HostValue::Reader(handle).into()),
        host,
    )
}

/// Create or truncate a file for writing.
pub fn fs_create_writer(args: Vec<ZValue>, host: &mut HostRuntime) -> Result<HostAction, i32> {
    FileIo::open(
        &args,
        |host, path| host.create_writer(path).map(|handle| HostValue::Writer(handle).into()),
        host,
    )
}

/// Create or open a file for appending.
pub fn fs_append_writer(args: Vec<ZValue>, host: &mut HostRuntime) -> Result<HostAction, i32> {
    FileIo::open(
        &args,
        |host, path| host.append_writer(path).map(|handle| HostValue::Writer(handle).into()),
        host,
    )
}

/// Write a string to output and then force the provided continuation.
pub fn write_str(args: Vec<ZValue>, output: &mut dyn Write) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(s)), e @ ZValue::Thunk(..)] => {
            output
                .write_all(s.as_str().as_bytes())
                .and_then(|_| output.flush())
                .expect("legacy standard-output write failed");
            resume(e, [])
        }
        | _ => unreachable!(""),
    }
}

/// Write an integer to output and then force the provided continuation.
pub fn write_int(args: Vec<ZValue>, output: &mut dyn Write) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Integer(IntegerLiteral::Int(i))), e @ ZValue::Thunk(..)] => {
            write!(output, "{i}")
                .and_then(|_| output.flush())
                .expect("legacy standard-output write failed");
            resume(e, [])
        }
        | _ => unreachable!(""),
    }
}

/// Write a string and newline to output, then force the continuation.
pub fn write_line(args: Vec<ZValue>, output: &mut dyn Write) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(line)), e @ ZValue::Thunk(..)] => {
            writeln!(output, "{line}")
                .and_then(|_| output.flush())
                .expect("legacy standard-output write failed");
            resume(e, [])
        }
        | _ => unreachable!(""),
    }
}

/// Read a line from input and pass it to the continuation.
pub fn read_line(
    args: Vec<ZValue>, input: &mut dyn BufRead, host: &mut HostRuntime,
) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [e @ ZValue::Thunk(_)] => {
            let line = host
                .io
                .read_with(ReaderHandle::STDIN, input, LineInput::read_line)
                .expect("legacy standard-input read failed");
            resume(e, [Literal::String(line.into()).into()])
        }
        | _ => unreachable!(""),
    }
}

/// Read a line and select either the failure continuation or the successful
/// integer continuation without constructing a library-defined option value.
pub fn read_line_as_int_branch(
    args: Vec<ZValue>, input: &mut dyn BufRead, host: &mut HostRuntime,
) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [failure @ ZValue::Thunk(_), success @ ZValue::Thunk(_)] => {
            let line = host
                .io
                .read_with(ReaderHandle::STDIN, input, LineInput::read_line)
                .expect("legacy standard-input read failed");
            match host_str::parse_signed_int(&line)
                .and_then(|integer| IntegerLiteral::new(integer.into()).with_type(IntegerType::Int))
            {
                | Some(integer) => resume(success, [Literal::Integer(integer).into()]),
                | None => resume(failure, []),
            }
        }
        | _ => unreachable!(""),
    }
}

/// Read all remaining input and pass it to the continuation.
pub fn read_till_eof(
    args: Vec<ZValue>, input: &mut dyn BufRead, host: &mut HostRuntime,
) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [e @ ZValue::Thunk(_)] => {
            let line = host
                .io
                .read_with(ReaderHandle::STDIN, input, LineInput::read_remaining)
                .expect("legacy standard-input read failed");
            resume(e, [Literal::String(line.into()).into()])
        }
        | _ => unreachable!(""),
    }
}

/// Lookup in the invocation's stable argument sequence; tails are ordinary library computations.
pub fn arg_at(args: Vec<ZValue>, argv: &[String]) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Integer(IntegerLiteral::Int(index))), when_none, when_some] => {
            let argument = usize::try_from(*index).ok().and_then(|index| argv.get(index));
            match argument {
                | Some(argument) => {
                    resume(when_some, [Literal::String(argument.as_str().into()).into()])
                }
                | None => resume(when_none, []),
            }
        }
        | _ => unreachable!("checked argument lookup classifier"),
    }
}

/// Produce a random integer literal and pass it to the continuation.
pub fn random_int(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [k] => {
            let integer = random_signed_int().expect("cannot read random source");
            let i = Literal::Integer(integer.into());
            resume(k, [i.into()])
        }
        | _ => unreachable!(""),
    }
}

/// Exit evaluation with the provided integer exit code.
pub fn exit(args: Vec<ZValue>) -> Result<HostAction, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Integer(IntegerLiteral::Int(a)))] => Err(*a as i32),
        | _ => unreachable!(""),
    }
}

/// Explicit conversion between machine integers and their exact-width counterparts.
pub(crate) struct IntegerConversion;

impl IntegerConversion {
    pub(crate) fn widen(target: IntegerType, args: Vec<ZValue>) -> Result<HostAction, i32> {
        let [ZValue::Literal(Literal::Integer(value))] = args.as_slice() else {
            unreachable!("checked integer widening receives one integer")
        };
        ret(Literal::Integer(value.with_type(target).expect("widening preserves the value")).into())
    }

    pub(crate) fn narrow(target: IntegerType, args: Vec<ZValue>) -> Result<HostAction, i32> {
        let [ZValue::Literal(Literal::Integer(value)), when_none, when_some] = args.as_slice()
        else {
            unreachable!("checked integer narrowing receives an integer and two continuations")
        };
        OptionalValueBranch::select(
            value.with_type(target).map(|value| Literal::Integer(value).into()),
            when_none,
            when_some,
        )
    }
}
