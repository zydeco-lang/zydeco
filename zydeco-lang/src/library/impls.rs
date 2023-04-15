use super::syntax::*;
use crate::{dynamics::syntax as ds, rc, utils::span::SpanInfo};
use std::{
    io::{BufRead, Write},
    rc::Rc,
};

type ZValue = ds::SemVal;
type ZCompute = ZComp;

// /* Function helpers */
fn ret<E>(value: ZValue) -> Result<ZCompute, E> {
    Ok(Ret(rc!(value.into())).into())
}

// /* Constructors and Destructors */
fn ctor(ctor: &str, args: Vec<Rc<ZValue>>) -> ZValue {
    Ctor { ctorv: CtorV::new(format!("{}", ctor), SpanInfo::dummy()), args }.into()
}
fn dtor(body: Rc<ZCompute>, dtor: &str, args: Vec<ZValue>) -> ZCompute {
    let args = args.into_iter().map(|a| rc!(a.into())).collect();
    Dtor { body, dtorv: DtorV::new(format!("{}", dtor), SpanInfo::dummy()), args }.into()
}

// /* Bool */
fn bool(b: bool) -> ZValue {
    let b = match b {
        true => "True",
        false => "False",
    };
    ctor(b, vec![])
}

// /* Arithmetic */
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
pub fn str_length(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::String(a))] => ret(Literal::Int(a.len() as i64).into()),
        _ => unreachable!(""),
    }
}

pub fn str_append(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::String(a)), ZValue::Literal(Literal::String(b))] => {
            ret(Literal::String(a.to_owned() + b.as_str()).into())
        }
        _ => unreachable!(""),
    }
}

pub fn str_eq(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::String(a)), ZValue::Literal(Literal::String(b))] => {
            ret(bool(a == b))
        }
        _ => unreachable!(""),
    }
}

pub fn str_index(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::String(a)), ZValue::Literal(Literal::Int(b))] => {
            ret(Literal::Char(a.chars().nth(*b as usize).unwrap()).into())
        }
        _ => unreachable!(""),
    }
}

pub fn int_to_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::Int(a))] => ret(Literal::String(a.to_string()).into()),
        _ => unreachable!(""),
    }
}

pub fn char_to_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::Char(a))] => ret(Literal::String(a.to_string()).into()),
        _ => unreachable!(""),
    }
}

pub fn char_to_int(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::Char(a))] => ret(Literal::Int((*a as u8) as i64).into()),
        _ => unreachable!(""),
    }
}

pub fn str_to_int(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::String(s))] => ret(Literal::Int(s.parse().unwrap()).into()),
        _ => unreachable!(""),
    }
}

// /* IO */
pub fn write_str(
    args: Vec<ZValue>, _r: &mut (dyn BufRead), w: &mut (dyn Write), _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::String(s)), e @ ZValue::Thunk(..)] => {
            write!(w, "{}", s).unwrap();
            Ok(Force(rc!(e.clone().into())).into())
        }
        _ => unreachable!(""),
    }
}

pub fn read_line(
    args: Vec<ZValue>, r: &mut (dyn BufRead), _w: &mut (dyn Write), _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [e @ ZValue::Thunk(_)] => {
            let mut line = String::new();
            r.read_line(&mut line).unwrap();
            line.pop();
            Ok(dtor(
                rc!(Force(rc!(e.clone().into())).into()),
                "arg",
                vec![Literal::String(line).into()],
            ))
        }
        _ => unreachable!(""),
    }
}

pub fn arg_list(
    args: Vec<ZValue>, _r: &mut (dyn BufRead), _w: &mut (dyn Write), argv: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [k] => {
            let mut z_arg_list = ctor("Nil", vec![]);
            for arg in argv.iter().rev() {
                z_arg_list =
                    ctor("Cons", vec![rc!(Literal::String(arg.clone()).into()), rc!(z_arg_list)]);
            }
            Ok(dtor(rc!(Force(rc!(k.clone().into())).into()), "arg", vec![z_arg_list]))
        }
        _ => unreachable!(""),
    }
}

pub fn exit(
    args: Vec<ZValue>, _r: &mut (dyn BufRead), _w: &mut (dyn Write), _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Literal(Literal::Int(a))] => Err(*a as i32),
        _ => unreachable!(""),
    }
}
