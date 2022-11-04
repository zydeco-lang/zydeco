use crate::dynamics::syntax::ZValue;
use crate::utils::ann::AnnT;

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

pub fn read_line<Ann: AnnT>(_args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    let mut line = String::new();
    std::io::stdin().read_line(&mut line).unwrap();
    ZValue::String(line, Ann::internal(""))
}

pub fn write_line<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    match args.as_slice() {
        [ZValue::String(s, _)] => {
            println!("{}", s)
        }
        _ => unreachable!(""),
    }
    ZValue::Unit(Ann::internal(""))
}