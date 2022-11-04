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
