use std::collections::HashMap;
use crate::parse::syntax::*;

static ARITH_OPS: [&str; 3] = ["add", "sub", "mul"];

pub fn builtin_ctx<Ann>(ann: Ann) -> HashMap<VVar<Ann>, TValue<Ann>> 
where Ann: Clone
{
    let intbox = Box::new(TValue::Int(ann.clone()));
    let arith_type = TValue::Comp(Box::new(TCompute::Lam(intbox.clone(), Box::new(TCompute::Lam(intbox.clone(), Box::new(TCompute::Ret(intbox.clone(), ann.clone())), ann.clone())), ann.clone())), ann.clone());
    let mut ctx = HashMap::new();
    ARITH_OPS.map(|op| ctx.insert(VVar::new(op.to_string(), ann.clone()), arith_type.clone()));
    ctx
}