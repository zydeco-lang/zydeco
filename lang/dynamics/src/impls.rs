use crate::syntax::*;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};

type ZValue = SemValue;
type ZCompute = Computation;

struct Input;

impl Input {
    fn line(reader: &mut dyn BufRead) -> String {
        let mut line = String::new();
        reader.read_line(&mut line).unwrap();
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }
        line
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
/// Generate arithmetic primitives that operate on integer literals.
macro_rules! arith {
    ( $name:ident, $op:tt ) => {
        pub fn $name(
            args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
        ) -> Result<ZCompute, i32> {
            match args.as_slice() {
                [
                    ZValue::Literal(Literal::Int(a)),
                    ZValue::Literal(Literal::Int(b))
                ] => ret(Literal::Int(a $op b).into()),
                _ => unreachable!(""),
            }
        }
    };
}

arith!(add, +);
arith!(sub, -);
arith!(mul, *);
arith!(div, /);
arith!(modulo, %);

struct Branch;

impl Branch {
    fn select(condition: bool, when_true: &ZValue, when_false: &ZValue) -> Result<ZCompute, i32> {
        let selected = if condition { when_true } else { when_false };
        Ok(Force(mk_rc(selected.clone().into())).into())
    }
}

macro_rules! intcomp_branch {
    ( $name:ident, $op:tt ) => {
        pub fn $name(
            args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
        ) -> Result<ZCompute, i32> {
            match args.as_slice() {
                [
                    ZValue::Literal(Literal::Int(a)),
                    ZValue::Literal(Literal::Int(b)),
                    when_true @ ZValue::Thunk(_),
                    when_false @ ZValue::Thunk(_),
                ] => Branch::select(a $op b, when_true, when_false),
                _ => unreachable!(""),
            }
        }
    };
}

intcomp_branch!(int_eq_branch, ==);
intcomp_branch!(int_lt_branch, <);
intcomp_branch!(int_gt_branch, >);

// /* Strings */
/// Return the number of Unicode scalar values in a string.
pub fn str_scalar_length(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(a))] => ret(Literal::Int(a.scalar_len() as i64).into()),
        | _ => unreachable!(""),
    }
}

/// Return the number of bytes in a string's UTF-8 encoding.
pub fn str_byte_length(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(string))] => {
            ret(Literal::Int(string.byte_len() as i64).into())
        }
        | _ => unreachable!(""),
    }
}

/// Concatenate two string literals.
pub fn str_append(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
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
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
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
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            ZValue::Literal(Literal::Int(index)),
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
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
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
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            ZValue::Literal(Literal::Int(index)),
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

/// Convert an integer literal to its string representation.
pub fn int_to_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Int(a))] => ret(Literal::String(a.to_string().into()).into()),
        | _ => unreachable!(""),
    }
}

/// Convert a character literal to a single-character string.
pub fn char_to_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Char(a))] => ret(Literal::String((*a).into()).into()),
        | _ => unreachable!(""),
    }
}

/// Convert a character literal to its integer codepoint.
pub fn char_codepoint(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Char(a))] => ret(Literal::Int(*a as u32 as i64).into()),
        | _ => unreachable!(""),
    }
}

/// Validate an integer as a Unicode scalar value and select a continuation.
pub fn char_from_codepoint_branch(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::Int(codepoint)),
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
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let integer =
                string.as_str().parse::<i64>().ok().map(|integer| Literal::Int(integer).into());
            OptionalValueBranch::select(integer, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

// /* IO */
/// Write a string to output and then force the provided continuation.
pub fn write_str(
    args: Vec<ZValue>, _r: &mut dyn BufRead, w: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(s)), e @ ZValue::Thunk(..)] => {
            write!(w, "{s}").unwrap();
            w.flush().unwrap();
            Ok(Force(mk_rc(e.clone().into())).into())
        }
        | _ => unreachable!(""),
    }
}

/// Write an integer to output and then force the provided continuation.
pub fn write_int(
    args: Vec<ZValue>, _r: &mut dyn BufRead, w: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Int(i)), e @ ZValue::Thunk(..)] => {
            write!(w, "{i}").unwrap();
            w.flush().unwrap();
            Ok(Force(mk_rc(e.clone().into())).into())
        }
        | _ => unreachable!(""),
    }
}

/// Write a string and newline to output, then force the continuation.
pub fn write_line(
    args: Vec<ZValue>, _r: &mut dyn BufRead, w: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(line)), e @ ZValue::Thunk(..)] => {
            writeln!(w, "{line}").unwrap();
            w.flush().unwrap();
            Ok(Force(mk_rc(e.clone().into())).into())
        }
        | _ => unreachable!(""),
    }
}

/// Read a line from input and pass it to the continuation.
pub fn read_line(
    args: Vec<ZValue>, r: &mut dyn BufRead, _w: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [e @ ZValue::Thunk(_)] => {
            let line = Input::line(r);
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
    args: Vec<ZValue>, r: &mut dyn BufRead, _w: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [failure @ ZValue::Thunk(_), success @ ZValue::Thunk(_)] => {
            match Input::line(r).parse::<i64>() {
                | Ok(integer) => Ok(app(
                    mk_rc(Force(mk_rc(success.clone().into())).into()),
                    Literal::Int(integer).into(),
                )),
                | Err(_) => Ok(Force(mk_rc(failure.clone().into())).into()),
            }
        }
        | _ => unreachable!(""),
    }
}

/// Read all remaining input and pass it to the continuation.
pub fn read_till_eof(
    args: Vec<ZValue>, r: &mut dyn BufRead, _w: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [e @ ZValue::Thunk(_)] => {
            let mut line = String::new();
            r.read_to_string(&mut line).unwrap();
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
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    use rand::RngExt;
    match args.as_slice() {
        | [k] => {
            let mut rng = rand::rng();
            let i = Literal::Int(rng.random_range(i64::MIN..=i64::MAX));
            Ok(app(mk_rc(Force(mk_rc(k.clone().into())).into()), i.into()))
        }
        | _ => unreachable!(""),
    }
}

/// Exit evaluation with the provided integer exit code.
pub fn exit(
    args: Vec<ZValue>, _r: &mut dyn BufRead, _w: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Int(a))] => Err(*a as i32),
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
            assert_eq!(Input::line(&mut Cursor::new(source)), expected);
        }
    }
}
