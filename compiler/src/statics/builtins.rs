use crate::parse::syntax::*;
use crate::parse::TValParser;
use super::ctx::Ctx;

static ARITH_OPS: [&str; 3] = ["add", "sub", "mul"];

pub fn builtin_ctx() -> Ctx<()>
{
    let arith_type = TValParser::new().parse("Comp(Int -> Int -> Ret(Int))").unwrap();
    let mut ctx = Ctx::new();
    ARITH_OPS.map(|op| ctx.push(VVar::new(op.to_string(), ()), arith_type.clone()));
    ctx
}
