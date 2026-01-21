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
            // re-expand from right to left, like:
            // (t1, (t2, (t3, t4)))
            let mut iter = args.into_iter().rev();
            let snd = iter.next().unwrap().as_ref().to_owned();
            let fst = iter.next().unwrap().as_ref().to_owned();
            let mut body: SemValue = Cons(mk_box(fst), mk_box(snd)).into();
            for term in iter {
                let term = term.as_ref().to_owned();
                let id = body;
                body = Cons(mk_box(term), mk_box(id)).into()
            }
            mk_box(body)
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

/// Produce a random integer literal and pass it to the continuation.
pub fn random_int(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    use rand::Rng;
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
