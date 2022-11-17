use crate::dynamics::syntax::ZValue;
use crate::utils::ann::AnnT;

/* Arithmetics */

pub fn add<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _), ZValue::Int(b, _)] => {
            ZValue::Int(a + b, Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn sub<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _), ZValue::Int(b, _)] => {
            ZValue::Int(a - b, Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn mul<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _), ZValue::Int(b, _)] => {
            ZValue::Int(a * b, Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn modulo<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _), ZValue::Int(b, _)] => {
            ZValue::Int(a % b, Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

/* Strings */

pub fn str_append<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::String(a, _), ZValue::String(b, _)] => {
            ZValue::String(a.to_owned() + b.as_str(), Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn str_eq<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::String(a, _), ZValue::String(b, _)] => {
            ZValue::Bool(a == b, Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn str_index<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::String(a, _), ZValue::Int(b, _)] => {
            ZValue::Char(a.chars().nth(*b as usize).unwrap(), Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

/* IO */

pub fn int_to_str<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::Int(a, _)] => {
            ZValue::String(a.to_string(), Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn char_to_str<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::Char(a, _)] => {
            ZValue::String(a.to_string(), Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn bool_to_str<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::Bool(a, _)] => {
            ZValue::String(a.to_string(), Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn write_line<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::String(s, _)] => {
            println!("{}", s)
        }
        _ => unreachable!(""),
    }
    ZValue::Triv(Ann::internal(""))
}

pub fn str_to_int<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::String(s, _)] => {
            ZValue::Int(s.parse().unwrap(), Ann::internal(""))
        }
        _ => unreachable!(""),
    }
}

pub fn read_line<Ann: AnnT>(_args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    let mut line = String::new();
    std::io::stdin().read_line(&mut line).unwrap();
    line.pop();
    ZValue::String(line, Ann::internal(""))
}
