use crate::{
    host::{HostIoErrorKind, HostRuntime, HostValue, ReaderHandle, WriterHandle},
    syntax::*,
};
use std::{
    io::{self, BufRead, Read, Write},
    rc::Rc,
};

type ZValue = SemValue;
type ZCompute = Computation;

struct Input;

impl Input {
    fn line(reader: &mut dyn BufRead) -> io::Result<String> {
        let mut line = String::new();
        reader.read_line(&mut line)?;
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }
        Ok(line)
    }

    fn remaining(reader: &mut dyn BufRead) -> io::Result<String> {
        let mut input = String::new();
        reader.read_to_string(&mut input)?;
        Ok(input)
    }
}

#[inline]
/// Rc helper for constructing shared computations.
fn mk_rc<T>(t: T) -> Rc<T> {
    Rc::new(t)
}

// /* Function helpers */
/// Wrap a value in a return computation.
fn ret<E>(value: ZValue) -> Result<ZCompute, E> {
    Ok(Return(mk_rc(value.into())).into())
}
/// Apply a computation to an argument value.
fn app(body: Rc<ZCompute>, arg: ZValue) -> ZCompute {
    App(body, mk_rc(arg.into())).into()
}
#[allow(unused)]
/// Apply a destructor to a computation.
fn dtor(body: Rc<ZCompute>, dtor: &str) -> ZCompute {
    Dtor(body, DtorName(dtor.to_string())).into()
}

// /* Arithmetic */
macro_rules! integer_arithmetic_result {
    ($variant:ident, $first:expr, $second:expr, $operation:expr) => {{
        let result = match $operation {
            | IntegerOperation::Add => $first.wrapping_add(*$second),
            | IntegerOperation::Sub => $first.wrapping_sub(*$second),
            | IntegerOperation::Mul => $first.wrapping_mul(*$second),
            | IntegerOperation::Div => $first.wrapping_div(*$second),
            | IntegerOperation::Mod => $first.wrapping_rem(*$second),
            | IntegerOperation::Eq
            | IntegerOperation::Lt
            | IntegerOperation::Gt
            | IntegerOperation::ToString => unreachable!(),
        };
        Literal::Integer(IntegerLiteral::$variant(result)).into()
    }};
}

/// Evaluate width- and signedness-aware wrapping integer arithmetic.
pub fn integer_arithmetic(
    integer_type: IntegerType, operation: IntegerOperation, args: Vec<ZValue>,
) -> Result<ZCompute, i32> {
    let value = match (integer_type, args.as_slice()) {
        | (
            IntegerType::Int8,
            [
                ZValue::Literal(Literal::Integer(IntegerLiteral::Int8(first))),
                ZValue::Literal(Literal::Integer(IntegerLiteral::Int8(second))),
            ],
        ) => integer_arithmetic_result!(Int8, first, second, operation),
        | (
            IntegerType::Int16,
            [
                ZValue::Literal(Literal::Integer(IntegerLiteral::Int16(first))),
                ZValue::Literal(Literal::Integer(IntegerLiteral::Int16(second))),
            ],
        ) => integer_arithmetic_result!(Int16, first, second, operation),
        | (
            IntegerType::Int32,
            [
                ZValue::Literal(Literal::Integer(IntegerLiteral::Int32(first))),
                ZValue::Literal(Literal::Integer(IntegerLiteral::Int32(second))),
            ],
        ) => integer_arithmetic_result!(Int32, first, second, operation),
        | (
            IntegerType::Int64,
            [
                ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(first))),
                ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(second))),
            ],
        ) => integer_arithmetic_result!(Int64, first, second, operation),
        | (
            IntegerType::UInt8,
            [
                ZValue::Literal(Literal::Integer(IntegerLiteral::UInt8(first))),
                ZValue::Literal(Literal::Integer(IntegerLiteral::UInt8(second))),
            ],
        ) => integer_arithmetic_result!(UInt8, first, second, operation),
        | (
            IntegerType::UInt16,
            [
                ZValue::Literal(Literal::Integer(IntegerLiteral::UInt16(first))),
                ZValue::Literal(Literal::Integer(IntegerLiteral::UInt16(second))),
            ],
        ) => integer_arithmetic_result!(UInt16, first, second, operation),
        | (
            IntegerType::UInt32,
            [
                ZValue::Literal(Literal::Integer(IntegerLiteral::UInt32(first))),
                ZValue::Literal(Literal::Integer(IntegerLiteral::UInt32(second))),
            ],
        ) => integer_arithmetic_result!(UInt32, first, second, operation),
        | (
            IntegerType::UInt64,
            [
                ZValue::Literal(Literal::Integer(IntegerLiteral::UInt64(first))),
                ZValue::Literal(Literal::Integer(IntegerLiteral::UInt64(second))),
            ],
        ) => integer_arithmetic_result!(UInt64, first, second, operation),
        | _ => unreachable!("type-checked integer operation received mismatched values"),
    };
    ret(value)
}

struct Branch;

impl Branch {
    fn select(condition: bool, when_true: &ZValue, when_false: &ZValue) -> Result<ZCompute, i32> {
        let selected = if condition { when_true } else { when_false };
        Ok(Force(mk_rc(selected.clone().into())).into())
    }
}

fn integer_comparison<T: PartialEq + PartialOrd>(
    first: &T, second: &T, operation: IntegerOperation,
) -> bool {
    match operation {
        | IntegerOperation::Eq => first == second,
        | IntegerOperation::Lt => first < second,
        | IntegerOperation::Gt => first > second,
        | _ => unreachable!(),
    }
}

/// Select a continuation using comparison in the integer's Rust domain.
pub fn integer_branch(
    integer_type: IntegerType, operation: IntegerOperation, args: Vec<ZValue>,
) -> Result<ZCompute, i32> {
    let [first, second, when_true @ ZValue::Thunk(_), when_false @ ZValue::Thunk(_)] =
        args.as_slice()
    else {
        unreachable!("type-checked integer branch received malformed arguments")
    };
    let condition = match (integer_type, first, second) {
        | (
            IntegerType::Int8,
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int8(first))),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int8(second))),
        ) => integer_comparison(first, second, operation),
        | (
            IntegerType::Int16,
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int16(first))),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int16(second))),
        ) => integer_comparison(first, second, operation),
        | (
            IntegerType::Int32,
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int32(first))),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int32(second))),
        ) => integer_comparison(first, second, operation),
        | (
            IntegerType::Int64,
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(first))),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(second))),
        ) => integer_comparison(first, second, operation),
        | (
            IntegerType::UInt8,
            ZValue::Literal(Literal::Integer(IntegerLiteral::UInt8(first))),
            ZValue::Literal(Literal::Integer(IntegerLiteral::UInt8(second))),
        ) => integer_comparison(first, second, operation),
        | (
            IntegerType::UInt16,
            ZValue::Literal(Literal::Integer(IntegerLiteral::UInt16(first))),
            ZValue::Literal(Literal::Integer(IntegerLiteral::UInt16(second))),
        ) => integer_comparison(first, second, operation),
        | (
            IntegerType::UInt32,
            ZValue::Literal(Literal::Integer(IntegerLiteral::UInt32(first))),
            ZValue::Literal(Literal::Integer(IntegerLiteral::UInt32(second))),
        ) => integer_comparison(first, second, operation),
        | (
            IntegerType::UInt64,
            ZValue::Literal(Literal::Integer(IntegerLiteral::UInt64(first))),
            ZValue::Literal(Literal::Integer(IntegerLiteral::UInt64(second))),
        ) => integer_comparison(first, second, operation),
        | _ => unreachable!("type-checked integer branch received mismatched values"),
    };
    Branch::select(condition, when_true, when_false)
}

pub fn integer_to_string(integer_type: IntegerType, args: Vec<ZValue>) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Integer(value))]
            if value.integer_type() == Some(integer_type) =>
        {
            ret(Literal::String(value.to_string().into()).into())
        }
        | _ => unreachable!("type-checked integer rendering received a mismatched value"),
    }
}

macro_rules! float_arithmetic_result {
    ($constructor:ident, $type:ty, $first:expr, $second:expr, $operation:expr) => {{
        let first = <$type>::from_bits(*$first);
        let second = <$type>::from_bits(*$second);
        let result = match $operation {
            | FloatOperation::Add => first + second,
            | FloatOperation::Sub => first - second,
            | FloatOperation::Mul => first * second,
            | FloatOperation::Div => first / second,
            | _ => unreachable!(),
        };
        Literal::Float(FloatLiteral::$constructor(result.to_bits())).into()
    }};
}

pub fn float_arithmetic(
    float_type: FloatType, operation: FloatOperation, args: Vec<ZValue>,
) -> Result<ZCompute, i32> {
    let value = match (float_type, args.as_slice()) {
        | (
            FloatType::Float32,
            [
                ZValue::Literal(Literal::Float(FloatLiteral::Float32(first))),
                ZValue::Literal(Literal::Float(FloatLiteral::Float32(second))),
            ],
        ) => float_arithmetic_result!(Float32, f32, first, second, operation),
        | (
            FloatType::Float64,
            [
                ZValue::Literal(Literal::Float(FloatLiteral::Float64(first))),
                ZValue::Literal(Literal::Float(FloatLiteral::Float64(second))),
            ],
        ) => float_arithmetic_result!(Float64, f64, first, second, operation),
        | _ => unreachable!("type-checked float operation received mismatched values"),
    };
    ret(value)
}

fn float_comparison<T: PartialEq + PartialOrd>(
    first: T, second: T, operation: FloatOperation,
) -> bool {
    match operation {
        | FloatOperation::Eq => first == second,
        | FloatOperation::Lt => first < second,
        | FloatOperation::Gt => first > second,
        | _ => unreachable!(),
    }
}

pub fn float_branch(
    float_type: FloatType, operation: FloatOperation, args: Vec<ZValue>,
) -> Result<ZCompute, i32> {
    let [first, second, when_true @ ZValue::Thunk(_), when_false @ ZValue::Thunk(_)] =
        args.as_slice()
    else {
        unreachable!("type-checked float branch received malformed arguments")
    };
    let condition = match (float_type, first, second) {
        | (
            FloatType::Float32,
            ZValue::Literal(Literal::Float(FloatLiteral::Float32(first))),
            ZValue::Literal(Literal::Float(FloatLiteral::Float32(second))),
        ) => float_comparison(f32::from_bits(*first), f32::from_bits(*second), operation),
        | (
            FloatType::Float64,
            ZValue::Literal(Literal::Float(FloatLiteral::Float64(first))),
            ZValue::Literal(Literal::Float(FloatLiteral::Float64(second))),
        ) => float_comparison(f64::from_bits(*first), f64::from_bits(*second), operation),
        | _ => unreachable!("type-checked float branch received mismatched values"),
    };
    Branch::select(condition, when_true, when_false)
}

/// Convert a floating-point literal to its shortest round-trippable decimal form.
pub fn float_to_string(float_type: FloatType, args: Vec<ZValue>) -> Result<ZCompute, i32> {
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
pub fn str_scalar_length(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(a))] => {
            ret(Literal::Integer((a.scalar_len() as i64).into()).into())
        }
        | _ => unreachable!(""),
    }
}

/// Return the number of bytes in a string's UTF-8 encoding.
pub fn str_byte_length(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(string))] => {
            ret(Literal::Integer((string.byte_len() as i64).into()).into())
        }
        | _ => unreachable!(""),
    }
}

/// Concatenate two string literals.
pub fn str_append(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
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
    ) -> Result<ZCompute, i32> {
        match pair {
            | None => Ok(Force(mk_rc(when_none.clone().into())).into()),
            | Some((first, second)) => {
                let continuation = Force(mk_rc(when_some.clone().into())).into();
                let continuation = app(mk_rc(continuation), Literal::String(first).into());
                Ok(app(mk_rc(continuation), Literal::String(second).into()))
            }
        }
    }
}

/// Split once and select a computation without constructing a
/// library-defined optional pair.
pub fn str_split_once_branch(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
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
pub fn str_split_at_branch(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(index))),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let pair = usize::try_from(*index).ok().and_then(|index| string.split_at_scalar(index));
            OptionalPairBranch::select(pair, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

/// Select a computation according to string equality without constructing a
/// library-defined Boolean value.
pub fn str_eq_branch(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
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
    ) -> Result<ZCompute, i32> {
        match value {
            | None => Ok(Force(mk_rc(when_none.clone().into())).into()),
            | Some(value) => {
                let continuation = Force(mk_rc(when_some.clone().into())).into();
                Ok(app(mk_rc(continuation), value))
            }
        }
    }
}

/// Safely index a string by Unicode scalar position and select a continuation.
pub fn str_get_branch(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(index))),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let character = usize::try_from(*index)
                .ok()
                .and_then(|index| string.scalar(index))
                .map(|character| Literal::Char(character).into());
            OptionalValueBranch::select(character, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

/// Convert a character literal to a single-character string.
pub fn char_to_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Char(a))] => ret(Literal::String((*a).into()).into()),
        | _ => unreachable!(""),
    }
}

/// Convert a character literal to its integer codepoint.
pub fn char_codepoint(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Char(a))] => {
            ret(Literal::Integer((*a as u32 as i64).into()).into())
        }
        | _ => unreachable!(""),
    }
}

/// Validate an integer as a Unicode scalar value and select a continuation.
pub fn char_from_codepoint_branch(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(codepoint))),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let character = u32::try_from(*codepoint)
                .ok()
                .and_then(char::from_u32)
                .map(|character| Literal::Char(character).into());
            OptionalValueBranch::select(character, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

/// Parse a string as an integer and select a continuation without panicking.
pub fn str_parse_int_branch(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let integer = string
                .as_str()
                .parse::<i64>()
                .ok()
                .map(|integer| Literal::Integer(integer.into()).into());
            OptionalValueBranch::select(integer, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

// /* Bytes */
struct HostBytes;

impl HostBytes {
    fn value(bytes: impl Into<Rc<[u8]>>) -> ZValue {
        HostValue::Bytes(bytes.into()).into()
    }

    fn borrow(value: &ZValue) -> &[u8] {
        match value {
            | ZValue::Host(HostValue::Bytes(bytes)) => bytes,
            | _ => unreachable!("expected host byte buffer"),
        }
    }
}

/// Construct an empty immutable byte buffer.
pub fn bytes_empty(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [] => ret(HostBytes::value(Vec::<u8>::new())),
        | _ => unreachable!(""),
    }
}

/// Return the number of octets in a byte buffer.
pub fn bytes_length(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [bytes] => ret(Literal::Integer((HostBytes::borrow(bytes).len() as i64).into()).into()),
        | _ => unreachable!(""),
    }
}

/// Concatenate two immutable byte buffers.
pub fn bytes_append(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [first, second] => {
            let bytes = [HostBytes::borrow(first), HostBytes::borrow(second)].concat();
            ret(HostBytes::value(bytes))
        }
        | _ => unreachable!(""),
    }
}

/// Encode a UTF-8 string into bytes.
pub fn bytes_from_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(string))] => {
            ret(HostBytes::value(string.as_str().as_bytes().to_vec()))
        }
        | _ => unreachable!(""),
    }
}

/// Decode bytes as UTF-8 and select the valid or invalid continuation.
pub fn bytes_to_str_branch(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [bytes, when_invalid @ ZValue::Thunk(_), when_valid @ ZValue::Thunk(_)] => {
            let value = std::str::from_utf8(HostBytes::borrow(bytes))
                .ok()
                .map(|string| Literal::String(string.into()).into());
            OptionalValueBranch::select(value, when_invalid, when_valid)
        }
        | _ => unreachable!(""),
    }
}

// /* IO */
struct HostContinuation;

impl HostContinuation {
    fn force(continuation: &ZValue) -> ZCompute {
        Force(mk_rc(continuation.clone().into())).into()
    }

    fn one(continuation: &ZValue, argument: ZValue) -> ZCompute {
        app(mk_rc(Self::force(continuation)), argument)
    }

    fn two(continuation: &ZValue, first: ZValue, second: ZValue) -> ZCompute {
        let continuation = Self::one(continuation, first);
        app(mk_rc(continuation), second)
    }

    fn io_error(continuation: &ZValue, error: io::Error) -> Result<ZCompute, i32> {
        let kind = Literal::Integer((HostIoErrorKind::from_error(&error) as i64).into()).into();
        let message = Literal::String(error.to_string().into()).into();
        Ok(Self::two(continuation, kind, message))
    }
}

struct ReaderIo;

impl ReaderIo {
    fn run<T>(
        handle: ReaderHandle, input: &mut dyn BufRead, host: &mut HostRuntime,
        operation: impl FnOnce(&mut dyn BufRead) -> io::Result<T>,
    ) -> io::Result<T> {
        if handle == ReaderHandle::STDIN {
            operation(input)
        } else {
            operation(host.reader(handle)?)
        }
    }
}

struct WriterIo;

impl WriterIo {
    fn run<T>(
        handle: WriterHandle, output: &mut dyn Write, host: &mut HostRuntime,
        operation: impl FnOnce(&mut dyn Write) -> io::Result<T>,
    ) -> io::Result<T> {
        match handle {
            | WriterHandle::STDOUT | WriterHandle::STDERR => operation(output),
            | handle => operation(host.writer(handle)?),
        }
    }
}

/// Return the interpreter's injected standard-input capability.
pub fn stdin(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [] => ret(HostValue::Reader(ReaderHandle::STDIN).into()),
        | _ => unreachable!(""),
    }
}

/// Return the interpreter's injected standard-output capability.
pub fn stdout(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [] => ret(HostValue::Writer(WriterHandle::STDOUT).into()),
        | _ => unreachable!(""),
    }
}

/// Return the interpreter's injected standard-error capability.
pub fn stderr(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [] => ret(HostValue::Writer(WriterHandle::STDERR).into()),
        | _ => unreachable!(""),
    }
}

/// Read at most the requested number of bytes from a capability.
pub fn io_read(
    args: Vec<ZValue>, input: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Reader(reader)),
            ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(count))),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => {
            let count = match u64::try_from(*count) {
                | Ok(count) => count,
                | Err(_) => {
                    return HostContinuation::io_error(
                        when_error,
                        io::Error::new(
                            io::ErrorKind::InvalidInput,
                            "byte count cannot be negative",
                        ),
                    );
                }
            };
            match ReaderIo::run(*reader, input, host, |reader| {
                let mut bytes = Vec::new();
                reader.take(count).read_to_end(&mut bytes)?;
                Ok(bytes)
            }) {
                | Ok(bytes) => Ok(HostContinuation::one(when_success, HostBytes::value(bytes))),
                | Err(error) => HostContinuation::io_error(when_error, error),
            }
        }
        | _ => unreachable!(""),
    }
}

/// Read one byte line, distinguishing EOF from failure.
pub fn io_read_line(
    args: Vec<ZValue>, input: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Reader(reader)),
            when_error @ ZValue::Thunk(_),
            when_eof @ ZValue::Thunk(_),
            when_line @ ZValue::Thunk(_),
        ] => match ReaderIo::run(*reader, input, host, |reader| {
            let mut bytes = Vec::new();
            let read = reader.read_until(b'\n', &mut bytes)?;
            if bytes.last() == Some(&b'\n') {
                bytes.pop();
                if bytes.last() == Some(&b'\r') {
                    bytes.pop();
                }
            }
            Ok((read, bytes))
        }) {
            | Ok((0, _)) => Ok(HostContinuation::force(when_eof)),
            | Ok((_, bytes)) => Ok(HostContinuation::one(when_line, HostBytes::value(bytes))),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

/// Read all remaining bytes from a capability.
pub fn io_read_all(
    args: Vec<ZValue>, input: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Reader(reader)),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => match ReaderIo::run(*reader, input, host, |reader| {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes)?;
            Ok(bytes)
        }) {
            | Ok(bytes) => Ok(HostContinuation::one(when_success, HostBytes::value(bytes))),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

/// Write an entire byte buffer to a capability.
pub fn io_write_all(
    args: Vec<ZValue>, _: &mut dyn BufRead, output: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Writer(writer)),
            bytes,
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => match WriterIo::run(*writer, output, host, |writer| {
            writer.write_all(HostBytes::borrow(bytes))
        }) {
            | Ok(()) => Ok(HostContinuation::force(when_success)),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

/// Flush buffered data through a writable capability.
pub fn io_flush(
    args: Vec<ZValue>, _: &mut dyn BufRead, output: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Writer(writer)),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => match WriterIo::run(*writer, output, host, |writer| writer.flush()) {
            | Ok(()) => Ok(HostContinuation::force(when_success)),
            | Err(error) => HostContinuation::io_error(when_error, error),
        },
        | _ => unreachable!(""),
    }
}

/// Close a readable capability, preserving standard input as a process resource.
pub fn io_close_reader(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
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
    args: Vec<ZValue>, _: &mut dyn BufRead, output: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Host(HostValue::Writer(writer)),
            when_error @ ZValue::Thunk(_),
            when_success @ ZValue::Thunk(_),
        ] => {
            let result = if matches!(*writer, WriterHandle::STDOUT | WriterHandle::STDERR) {
                WriterIo::run(*writer, output, host, |writer| writer.flush())
            } else {
                host.close_writer(*writer)
            };
            match result {
                | Ok(()) => Ok(HostContinuation::force(when_success)),
                | Err(error) => HostContinuation::io_error(when_error, error),
            }
        }
        | _ => unreachable!(""),
    }
}

struct FileIo;

impl FileIo {
    fn open(
        args: &[ZValue], operation: impl FnOnce(&mut HostRuntime, &str) -> io::Result<ZValue>,
        host: &mut HostRuntime,
    ) -> Result<ZCompute, i32> {
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
pub fn fs_open_reader(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    FileIo::open(
        &args,
        |host, path| host.open_reader(path).map(|handle| HostValue::Reader(handle).into()),
        host,
    )
}

/// Create or truncate a file for writing.
pub fn fs_create_writer(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    FileIo::open(
        &args,
        |host, path| host.create_writer(path).map(|handle| HostValue::Writer(handle).into()),
        host,
    )
}

/// Create or open a file for appending.
pub fn fs_append_writer(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    FileIo::open(
        &args,
        |host, path| host.append_writer(path).map(|handle| HostValue::Writer(handle).into()),
        host,
    )
}

/// Write a string to output and then force the provided continuation.
pub fn write_str(
    args: Vec<ZValue>, _r: &mut dyn BufRead, output: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(s)), e @ ZValue::Thunk(..)] => {
            WriterIo::run(WriterHandle::STDOUT, output, host, |writer| {
                writer.write_all(s.as_str().as_bytes())?;
                writer.flush()
            })
            .expect("legacy standard-output write failed");
            Ok(Force(mk_rc(e.clone().into())).into())
        }
        | _ => unreachable!(""),
    }
}

/// Write an integer to output and then force the provided continuation.
pub fn write_int(
    args: Vec<ZValue>, _r: &mut dyn BufRead, output: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(i))), e @ ZValue::Thunk(..)] => {
            WriterIo::run(WriterHandle::STDOUT, output, host, |writer| {
                write!(writer, "{i}")?;
                writer.flush()
            })
            .expect("legacy standard-output write failed");
            Ok(Force(mk_rc(e.clone().into())).into())
        }
        | _ => unreachable!(""),
    }
}

/// Write a string and newline to output, then force the continuation.
pub fn write_line(
    args: Vec<ZValue>, _r: &mut dyn BufRead, output: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(line)), e @ ZValue::Thunk(..)] => {
            WriterIo::run(WriterHandle::STDOUT, output, host, |writer| {
                writeln!(writer, "{line}")?;
                writer.flush()
            })
            .expect("legacy standard-output write failed");
            Ok(Force(mk_rc(e.clone().into())).into())
        }
        | _ => unreachable!(""),
    }
}

/// Read a line from input and pass it to the continuation.
pub fn read_line(
    args: Vec<ZValue>, input: &mut dyn BufRead, _w: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [e @ ZValue::Thunk(_)] => {
            let line = ReaderIo::run(ReaderHandle::STDIN, input, host, Input::line)
                .expect("legacy standard-input read failed");
            Ok(app(
                mk_rc(Force(mk_rc(e.clone().into())).into()),
                Literal::String(line.into()).into(),
            ))
        }
        | _ => unreachable!(""),
    }
}

/// Read a line and select either the failure continuation or the successful
/// integer continuation without constructing a library-defined option value.
pub fn read_line_as_int_branch(
    args: Vec<ZValue>, input: &mut dyn BufRead, _w: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [failure @ ZValue::Thunk(_), success @ ZValue::Thunk(_)] => {
            let line = ReaderIo::run(ReaderHandle::STDIN, input, host, Input::line)
                .expect("legacy standard-input read failed");
            match line.parse::<i64>() {
                | Ok(integer) => Ok(app(
                    mk_rc(Force(mk_rc(success.clone().into())).into()),
                    Literal::Integer(integer.into()).into(),
                )),
                | Err(_) => Ok(Force(mk_rc(failure.clone().into())).into()),
            }
        }
        | _ => unreachable!(""),
    }
}

/// Read all remaining input and pass it to the continuation.
pub fn read_till_eof(
    args: Vec<ZValue>, input: &mut dyn BufRead, _w: &mut dyn Write, _: &[String],
    host: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [e @ ZValue::Thunk(_)] => {
            let line = ReaderIo::run(ReaderHandle::STDIN, input, host, Input::remaining)
                .expect("legacy standard-input read failed");
            Ok(app(
                mk_rc(Force(mk_rc(e.clone().into())).into()),
                Literal::String(line.into()).into(),
            ))
        }
        | _ => unreachable!(""),
    }
}

struct ArgumentFold;

impl ArgumentFold {
    fn tail(tail: ZCompute) -> RcValue {
        mk_rc(Value::Thunk(Thunk(mk_rc(tail))))
    }

    fn item(step: &ZValue, argument: &str, tail: ZCompute) -> ZCompute {
        let step: ZCompute = Force(mk_rc(step.clone().into())).into();
        let with_argument = app(mk_rc(step), Literal::String(argument.into()).into());
        App(mk_rc(with_argument), Self::tail(tail)).into()
    }

    fn build(argv: &[String], when_empty: &ZValue, when_item: &ZValue) -> ZCompute {
        let empty: ZCompute = Force(mk_rc(when_empty.clone().into())).into();
        argv.iter().rev().fold(empty, |tail, argument| Self::item(when_item, argument, tail))
    }
}

/// Fold over command-line arguments without constructing a library-defined
/// list. The item continuation receives the remaining fold as a thunk, so it
/// may preserve the ordinary lazy right-fold behavior.
pub fn arg_fold(
    args: Vec<ZValue>, _r: &mut dyn BufRead, _w: &mut dyn Write, argv: &[String],
    _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [when_empty @ ZValue::Thunk(_), when_item @ ZValue::Thunk(_)] => {
            Ok(ArgumentFold::build(argv, when_empty, when_item))
        }
        | _ => unreachable!(""),
    }
}

/// Produce a random integer literal and pass it to the continuation.
pub fn random_int(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    use rand::RngExt;
    match args.as_slice() {
        | [k] => {
            let mut rng = rand::rng();
            let i = Literal::Integer(rng.random_range(i64::MIN..=i64::MAX).into());
            Ok(app(mk_rc(Force(mk_rc(k.clone().into())).into()), i.into()))
        }
        | _ => unreachable!(""),
    }
}

/// Exit evaluation with the provided integer exit code.
pub fn exit(
    args: Vec<ZValue>, _r: &mut dyn BufRead, _w: &mut dyn Write, _: &[String], _: &mut HostRuntime,
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Integer(IntegerLiteral::Int64(a)))] => Err(*a as i32),
        | _ => unreachable!(""),
    }
}

#[cfg(test)]
mod tests {
    use super::Input;
    use std::io::Cursor;

    #[test]
    fn line_input_removes_one_line_ending_and_preserves_other_content() {
        for (source, expected) in
            [("text\n", "text"), ("text\r\n", "text"), ("text", "text"), ("text\r", "text\r")]
        {
            assert_eq!(Input::line(&mut Cursor::new(source)).unwrap(), expected);
        }
    }
}
