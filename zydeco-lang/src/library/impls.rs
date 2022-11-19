use crate::dynamics::syntax::{ZCompute, ZValue};
use crate::utils::ann::{AnnHolder, AnnT};
use std::rc::Rc;

/* Function helpers */

fn ret<Ann: AnnT>(value: ZValue<Ann>) -> ZCompute<Ann> {
    let ann = value.ann().clone();
    ZCompute::Return(Rc::new(value), ann)
}

/* Arithmetic */

pub fn add<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _), ZValue::Int(b, _)] => {
            ret(ZValue::Int(a + b, Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn sub<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _), ZValue::Int(b, _)] => {
            ret(ZValue::Int(a - b, Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn mul<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _), ZValue::Int(b, _)] => {
            ret(ZValue::Int(a * b, Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn modulo<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _), ZValue::Int(b, _)] => {
            ret(ZValue::Int(a % b, Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

/* Strings */
pub fn str_length<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::String(a, _)] => {
            ret(ZValue::Int(a.len() as i64, Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn str_append<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::String(a, _), ZValue::String(b, _)] => {
            ret(ZValue::String(a.to_owned() + b.as_str(), Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn str_eq<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::String(a, _), ZValue::String(b, _)] => {
            ret(ZValue::Bool(a == b, Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn str_index<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::String(a, _), ZValue::Int(b, _)] => ret(ZValue::Char(
            a.chars().nth(*b as usize).unwrap(),
            Ann::internal(""),
        )),
        _ => unreachable!(""),
    }
}

/* IO */

pub fn int_to_str<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _)] => {
            ret(ZValue::String(a.to_string(), Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn char_to_str<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::Char(a, _)] => {
            ret(ZValue::String(a.to_string(), Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn bool_to_str<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::Bool(a, _)] => {
            ret(ZValue::String(a.to_string(), Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn str_to_int<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::String(s, _)] => {
            ret(ZValue::Int(s.parse().unwrap(), Ann::internal("")))
        }
        _ => unreachable!(""),
    }
}

pub fn write_line<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::String(s, _), e @ ZValue::Thunk(_comp, _, _)] => {
            println!("{}", s);
            ZCompute::Force(Rc::new(e.clone()), Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn read_line<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [e @ ZValue::Thunk(_comp, _, _)] => {
            let mut line = String::new();
            std::io::stdin().read_line(&mut line).unwrap();
            line.pop();
            ZCompute::App(
                Rc::new(ZCompute::Force(Rc::new(e.clone()), Ann::internal(""))),
                Rc::new(ZValue::String(line, Ann::internal(""))),
                Ann::internal(""),
            )
        }
        _ => unreachable!(""),
    }
}

pub fn exit<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZCompute<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _)] => std::process::exit(*a as i32),
        _ => unreachable!(""),
    }
}
