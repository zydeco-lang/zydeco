use super::syntax::{ZCompute, ZValue};
use crate::{parse::syntax::VVar, utils::ann::AnnT};
use std::rc::Rc;

pub fn get_builtin<Ann: AnnT>(var: &VVar<Ann>) -> Option<ZValue<Ann>> {
    if var.name() == "add" {
        Some(ZValue::Thunk(Rc::new(wrap_prim(add, 2)), None, Ann::internal("")))
    } else {
        None
    }
}

fn wrap_prim<Ann: AnnT>(
    func: fn(Vec<ZValue<Ann>>) -> ZValue<Ann>, arity: u64,
) -> ZCompute<Ann> {
    ZCompute::Prim { arity, body: func, ann: Ann::internal("") }
}

fn add<Ann: AnnT>(args: Vec<ZValue<Ann>>) -> ZValue<Ann> {
    if let (ZValue::Int(a, _), ZValue::Int(b, _)) =
        (args[0].clone(), args[1].clone())
    {
        ZValue::Int(a + b, Ann::internal(""))
    } else {
        ZValue::Int(0, Ann::internal(""))
    }
}
