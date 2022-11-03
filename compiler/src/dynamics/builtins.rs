use std::rc::Rc;
use crate::parse::syntax::VVar;
use super::syntax::{ZValue, ZCompute};

pub fn get_builtin<Ann: Clone>(var: &VVar<Ann>) -> Option<ZValue<Ann>> {
    if var.name() == "add" {
        Some(ZValue::Thunk(Rc::new(ZCompute::Prim { arity: 2, body: add, ann: var.ann().clone() }), None, var.ann().clone()))
    }
    else {
        None
    }
}

fn add<Ann: Clone>(args: Vec<ZValue<Ann>>, ann: Ann) -> ZValue<Ann> {
    if let (ZValue::Int(a, _), ZValue::Int(b, _)) = (args[0].clone(), args[1].clone()) {
        ZValue::Int(a + b, ann)
    }
    else { ZValue::Int(0, ann) }
}