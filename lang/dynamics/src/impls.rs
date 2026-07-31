use crate::syntax::*;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};

type ZValue = SemValue;
type ZCompute = Computation;

/// Box helper for constructing semantic values.
fn mk_box<T>(t: T) -> Box<T> {
    Box::new(t)
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
/// Construct a constructor value from a list of arguments.
fn ctor(ctor: &str, args: Vec<Rc<ZValue>>) -> ZValue {
    let args = match args.len() {
        | 0 => mk_box(Triv.into()),
        | 1 => mk_box(args[0].as_ref().to_owned()),
        | _ => {
            let args = args.into_iter().map(|arg| arg.as_ref().to_owned()).collect();
            let ConsN(items, tail) = ConsN::from_vec(args).expect("non-empty constructor payload");
            mk_box(ConsN(items, Box::new(tail)).into())
        }
    };
    Ctor(CtorName(ctor.to_string()), args).into()
}
#[allow(unused)]
/// Apply a destructor to a computation.
fn dtor(body: Rc<ZCompute>, dtor: &str) -> ZCompute {
    Dtor(body, DtorName(dtor.to_string())).into()
}

// /* Bool */
/// Encode a Rust boolean as a Zydeco boolean constructor.
fn bool(b: bool) -> ZValue {
    let b = match b {
        | true => "+True",
        | false => "+False",
    };
    ctor(b, vec![])
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

/// Generate integer comparison primitives.
macro_rules! intcomp {
    ( $name:ident, $op:tt ) => {
        pub fn $name(
            args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _:&[String],
        ) -> Result<ZCompute, i32> {
            match args.as_slice() {
                [
                    ZValue::Literal(Literal::Int(a)),
                    ZValue::Literal(Literal::Int(b))
                ] => ret(bool(a $op b)),
                _ => unreachable!(""),
            }
        }
    };
}

intcomp!(int_eq, ==);
intcomp!(int_lt, <);
intcomp!(int_gt, >);

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
/// Return the length of a string as an integer literal.
pub fn str_length(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(a))] => ret(Literal::Int(a.len() as i64).into()),
        | _ => unreachable!(""),
    }
}

/// Concatenate two string literals.
pub fn str_append(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(a)), ZValue::Literal(Literal::String(b))] => {
            let mut a = a.to_owned();
            a.extend(b);
            ret(Literal::String(a).into())
        }
        | _ => unreachable!(""),
    }
}

/// Split a string once on the given delimiter, returning an option pair.
pub fn str_split_once(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(s)), ZValue::Literal(Literal::Char(p))] => {
            match s.iter().collect::<String>().split_once(p.to_owned()) {
                | Some((a, b)) => ret(ctor(
                    "+Some",
                    vec![mk_rc(ctor(
                        "+Cons",
                        vec![
                            mk_rc(Literal::String(a.chars().collect()).into()),
                            mk_rc(Literal::String(b.chars().collect()).into()),
                        ],
                    ))],
                )),
                | None => ret(ctor("+None", vec![])),
            }
        }
        | _ => unreachable!(""),
    }
}

/// Split a string at index `n`, returning an option pair.
pub fn str_split_n(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(s)), ZValue::Literal(Literal::Int(n))] => {
            if n.is_negative() {
                return ret(ctor("+None", vec![]));
            }
            let (a, b) = s.split_at(*n as usize);
            ret(ctor(
                "+Some",
                vec![mk_rc(ctor(
                    "+Cons",
                    vec![
                        mk_rc(Literal::String(a.to_owned()).into()),
                        mk_rc(Literal::String(b.to_owned()).into()),
                    ],
                ))],
            ))
        }
        | _ => unreachable!(""),
    }
}

struct OptionalPairBranch;

impl OptionalPairBranch {
    fn select(
        pair: Option<(Vec<char>, Vec<char>)>, when_none: &ZValue, when_some: &ZValue,
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
            let pair =
                string.iter().collect::<String>().split_once(*separator).map(|(first, second)| {
                    (first.chars().collect::<Vec<_>>(), second.chars().collect::<Vec<_>>())
                });
            OptionalPairBranch::select(pair, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

/// Split at an index and select a computation without constructing a
/// library-defined optional pair.
pub fn str_split_n_branch(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [
            ZValue::Literal(Literal::String(string)),
            ZValue::Literal(Literal::Int(index)),
            when_none @ ZValue::Thunk(_),
            when_some @ ZValue::Thunk(_),
        ] => {
            let pair = usize::try_from(*index).ok().and_then(|index| {
                (index <= string.len()).then(|| {
                    let (first, second) = string.split_at(index);
                    (first.to_vec(), second.to_vec())
                })
            });
            OptionalPairBranch::select(pair, when_none, when_some)
        }
        | _ => unreachable!(""),
    }
}

/// Test two strings for equality.
pub fn str_eq(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(a)), ZValue::Literal(Literal::String(b))] => {
            ret(bool(a == b))
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

/// Index a string by position and return the character literal.
pub fn str_index(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(a)), ZValue::Literal(Literal::Int(b))] => {
            ret(Literal::Char(*a.iter().nth(*b as usize).unwrap()).into())
        }
        | _ => unreachable!(""),
    }
}

/// Convert an integer literal to its string representation.
pub fn int_to_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Int(a))] => {
            ret(Literal::String(a.to_string().chars().collect()).into())
        }
        | _ => unreachable!(""),
    }
}

/// Convert a character literal to a single-character string.
pub fn char_to_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Char(a))] => {
            ret(Literal::String(a.to_string().chars().collect()).into())
        }
        | _ => unreachable!(""),
    }
}

/// Convert a character literal to its integer codepoint.
pub fn char_to_int(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::Char(a))] => ret(Literal::Int((*a as u8) as i64).into()),
        | _ => unreachable!(""),
    }
}

/// Parse a string literal into an integer literal.
pub fn str_to_int(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [ZValue::Literal(Literal::String(s))] => {
            ret(Literal::Int(s.iter().collect::<String>().parse().unwrap()).into())
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
            write!(w, "{}", s.iter().collect::<String>()).unwrap();
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
            writeln!(w, "{}", line.iter().collect::<String>()).unwrap();
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
            let mut line = String::new();
            r.read_line(&mut line).unwrap();
            line.pop();
            Ok(app(
                mk_rc(Force(mk_rc(e.clone().into())).into()),
                Literal::String(line.chars().collect()).into(),
            ))
        }
        | _ => unreachable!(""),
    }
}

/// Read a line and attempt to parse it as an integer.
pub fn read_line_as_int(
    args: Vec<ZValue>, r: &mut dyn BufRead, _w: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [e @ ZValue::Thunk(_)] => {
            let mut line = String::new();
            r.read_line(&mut line).unwrap();
            line.pop();
            let i: Option<i64> = line.parse().ok();
            match i {
                | Some(i) => Ok(app(
                    mk_rc(Force(mk_rc(e.clone().into())).into()),
                    ctor("+Some", vec![mk_rc(Literal::Int(i).into())]),
                )),
                | None => {
                    Ok(app(mk_rc(Force(mk_rc(e.clone().into())).into()), ctor("+None", vec![])))
                }
            }
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
            let mut line = String::new();
            r.read_line(&mut line).unwrap();
            match line.trim_end_matches(['\r', '\n']).parse::<i64>() {
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
                Literal::String(line.chars().collect()).into(),
            ))
        }
        | _ => unreachable!(""),
    }
}

/// Build a Zydeco list of command-line arguments and pass it to the continuation.
pub fn arg_list(
    args: Vec<ZValue>, _r: &mut dyn BufRead, _w: &mut dyn Write, argv: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        | [k] => {
            let mut z_arg_list = ctor("+Nil", vec![]);
            for arg in argv.iter().rev() {
                z_arg_list = ctor(
                    "+Cons",
                    vec![mk_rc(Literal::String(arg.chars().collect()).into()), mk_rc(z_arg_list)],
                );
            }
            Ok(app(mk_rc(Force(mk_rc(k.clone().into())).into()), z_arg_list))
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
        let with_argument = app(mk_rc(step), Literal::String(argument.chars().collect()).into());
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
