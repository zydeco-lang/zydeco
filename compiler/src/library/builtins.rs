use super::impls::*;
use crate::dynamics::{
    eval::Runtime,
    syntax::{ZCompute, ZValue},
};
use crate::parse::{syntax::*, TValParser};
use crate::statics::ctx::Ctx;
use crate::utils::ann::AnnT;
use std::rc::Rc;

type ZPrim = fn(Vec<ZValue<()>>) -> ZValue<()>;

struct Builtin<'a> {
    name: &'a str,
    ztype: &'a str,
    arity: u64,
    body: ZPrim,
}

fn builtin<'a>(
    name: &'a str, ztype: &'a str, arity: u64, body: ZPrim,
) -> Builtin<'a> {
    Builtin { name, ztype, arity, body }
}

// Only need to change here (and provide impl) to add new builtin function
fn std_library() -> Vec<Builtin<'static>> {
    vec![
        builtin("add", "Comp(Int -> Int -> Ret(Int))", 2, add),
        builtin("sub", "Comp(Int -> Int -> Ret(Int))", 2, sub),
        builtin("mul", "Comp(Int -> Int -> Ret(Int))", 2, mul),
        builtin("str_append", "Comp(String -> String -> Ret(String))", 2, str_append),
        builtin("str_eq", "Comp(String -> String -> Ret(Bool))", 2, str_eq),
        builtin("read_line", "Comp(Ret(String))", 0, read_line),
        builtin("write_line", "Comp(String -> Ret(Unit))", 1, write_line),
    ]
}

pub fn builtin_ctx() -> Ctx<()> {
    let mut ctx = Ctx::new();
    let parser = TValParser::new();
    for builtin in std_library() {
        ctx.push(
            VVar::new(builtin.name.to_string(), ()),
            parser.parse(builtin.ztype).unwrap(),
        );
    }
    ctx
}

pub fn builtin_runtime() -> Runtime<()> {
    let mut runtime = Runtime::new();
    for builtin in std_library() {
        runtime.insert(
            VVar::new(builtin.name.to_string(), ()),
            Rc::new(ZValue::Thunk(
                Rc::new(wrap_prim(builtin.body, builtin.arity)),
                None,
                (),
            )),
        ).unwrap();
    }
    runtime
}

fn wrap_prim<Ann: AnnT>(
    func: fn(Vec<ZValue<Ann>>) -> ZValue<Ann>, arity: u64,
) -> ZCompute<Ann> {
    ZCompute::Prim { arity, body: func, ann: Ann::internal("") }
}
