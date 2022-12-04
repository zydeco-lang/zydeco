use crate::dynamics::syntax::{ZCompute, ZValue};
use std::io::{BufRead, Write};
use std::rc::Rc;

// /* Function helpers */
fn ret<E>(value: ZValue) -> Result<ZCompute, E> {
    Ok(ZCompute::Return(Rc::new(value)))
}

// /* Bool */
fn bool(b: bool) -> ZValue {
    let b = match b {
        true => "True",
        false => "False",
    };
    ZValue::Ctor(format!("{}", b), vec![])
}

// /* Arithmetic */
macro_rules! arith {
    ( $name:ident, $op:tt ) => {
        pub fn $name(
            args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
        ) -> Result<ZCompute, i32> {
            match args.as_slice() {
                [ZValue::Int(a), ZValue::Int(b)] => ret(ZValue::Int(a $op b)),
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
                [ZValue::Int(a), ZValue::Int(b)] => ret(bool(a $op b)),
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
        [ZValue::String(a)] => ret(ZValue::Int(a.len() as i64)),
        _ => unreachable!(""),
    }
}

pub fn str_append(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::String(a), ZValue::String(b)] => {
            ret(ZValue::String(a.to_owned() + b.as_str()))
        }
        _ => unreachable!(""),
    }
}

pub fn str_eq(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::String(a), ZValue::String(b)] => ret(bool(a == b)),
        _ => unreachable!(""),
    }
}

pub fn str_index(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::String(a), ZValue::Int(b)] => {
            ret(ZValue::Char(a.chars().nth(*b as usize).unwrap()))
        }
        _ => unreachable!(""),
    }
}

pub fn int_to_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Int(a)] => ret(ZValue::String(a.to_string())),
        _ => unreachable!(""),
    }
}

pub fn char_to_str(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Char(a)] => ret(ZValue::String(a.to_string())),
        _ => unreachable!(""),
    }
}

pub fn char_to_int(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Char(a)] => ret(ZValue::Int((*a as u8) as i64)),
        _ => unreachable!(""),
    }
}

pub fn str_to_int(
    args: Vec<ZValue>, _: &mut dyn BufRead, _: &mut dyn Write, _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::String(s)] => ret(ZValue::Int(s.parse().unwrap())),
        _ => unreachable!(""),
    }
}

// /* IO */
pub fn write_line(
    args: Vec<ZValue>, _r: &mut (dyn BufRead), w: &mut (dyn Write),
    _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::String(s), e @ ZValue::Thunk(..)] => {
            writeln!(w, "{}", s).unwrap();
            Ok(ZCompute::Force(Rc::new(e.clone())))
        }
        _ => unreachable!(""),
    }
}

pub fn read_line(
    args: Vec<ZValue>, r: &mut (dyn BufRead), _w: &mut (dyn Write),
    _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [e @ ZValue::Thunk(_, _)] => {
            let mut line = String::new();
            r.read_line(&mut line).unwrap();
            line.pop();
            Ok(ZCompute::App(
                Rc::new(ZCompute::Force(Rc::new(e.clone()))),
                Rc::new(ZValue::String(line)),
            ))
        }
        _ => unreachable!(""),
    }
}

pub fn arg_list(
    args: Vec<ZValue>, _r: &mut (dyn BufRead), _w: &mut (dyn Write),
    argv: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [k] => {
            let mut z_arg_list =
                Rc::new(ZValue::Ctor("SLNil".to_string(), vec![]));
            for arg in argv.iter().rev() {
                z_arg_list = Rc::new(ZValue::Ctor(
                    "SLCons".to_string(),
                    vec![Rc::new(ZValue::String(arg.clone())), z_arg_list],
                ));
            }
            Ok(ZCompute::App(
                Rc::new(ZCompute::Force(Rc::new(k.clone()))),
                z_arg_list,
            ))
        }
        _ => unreachable!(""),
    }
}

pub fn exit(
    args: Vec<ZValue>, _r: &mut (dyn BufRead), _w: &mut (dyn Write),
    _: &[String],
) -> Result<ZCompute, i32> {
    match args.as_slice() {
        [ZValue::Int(a)] => Err(*a as i32),
        _ => unreachable!(""),
    }
}
