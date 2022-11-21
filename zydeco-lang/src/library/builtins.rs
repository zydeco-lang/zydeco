use super::impls::*;
use crate::parse::{syntax::*, TValParser};
use crate::statics::ctx::Ctx;
use crate::{
    dynamics::{
        eval::Runtime,
        syntax::{PrimComp, ZCompute, ZValue},
    },
    lex::token::Tok,
};
use logos::Logos;
use std::io::{BufRead, Write};
use std::rc::Rc;

struct Builtin {
    name: &'static str,
    ztype: &'static str,
    arity: u64,
    behavior: Box<PrimComp>,
}

impl Builtin {
    pub fn new(
        name: &'static str, ztype: &'static str, arity: u64, behavior: PrimComp,
    ) -> Self {
        Builtin { name, ztype, arity, behavior: Box::new(behavior) }
    }
}

// Only need to change here (and provide impl) to add new builtin function
fn std_library() -> Vec<Builtin> {
    vec![
        Builtin::new("add", "Comp(Int -> Int -> Ret(Int))", 2, add),
        Builtin::new("sub", "Comp(Int -> Int -> Ret(Int))", 2, sub),
        Builtin::new("mul", "Comp(Int -> Int -> Ret(Int))", 2, mul),
        Builtin::new("mod", "Comp(Int -> Int -> Ret(Int))", 2, modulo),
        Builtin::new("str_length", "Comp(String -> Ret(Int))", 2, str_length),
        Builtin::new(
            "str_append",
            "Comp(String -> String -> Ret(String))",
            2,
            str_append,
        ),
        Builtin::new(
            "str_eq",
            "Comp(String -> String -> Ret(Bool))",
            2,
            str_eq,
        ),
        Builtin::new(
            "str_index",
            "Comp(String -> Int -> Ret(Char))",
            2,
            str_index,
        ),
        Builtin::new("int_to_str", "Comp(Int -> Ret(String))", 1, int_to_str),
        Builtin::new(
            "char_to_str",
            "Comp(Char -> Ret(String))",
            1,
            char_to_str,
        ),
        Builtin::new(
            "bool_to_str",
            "Comp(Bool -> Ret(String))",
            1,
            bool_to_str,
        ),
        Builtin::new("str_to_int", "Comp(String -> Ret(Int))", 1, str_to_int),
        Builtin::new(
            "write_line",
            "Thunk(String -> Thunk(OS) -> OS)",
            2,
            write_line,
        ),
        Builtin::new(
            "read_line",
            "Thunk(Thunk(String -> OS) -> OS)",
            1,
            read_line,
        ),
        Builtin::new("exit", "Thunk(Int -> OS)", 1, exit),
    ]
}

pub fn builtin_ctx() -> Ctx<()> {
    let mut ctx = Ctx::new();
    let parser = TValParser::new();
    for builtin in std_library() {
        let lexer = Tok::lexer(builtin.ztype)
            .spanned()
            .map(|(tok, range)| (range.start, tok, range.end));
        ctx.push(
            VVar::new(builtin.name.to_string(), ()),
            parser.parse(builtin.ztype, lexer).unwrap(),
        );
    }
    ctx
}

pub fn builtin_runtime<'rt>(
    input: &'rt mut (dyn BufRead), output: &'rt mut (dyn Write),
) -> Runtime<'rt> {
    let mut runtime = Runtime::new(input, output);
    for builtin in std_library() {
        runtime
            .insert(
                builtin.name.to_string(),
                Rc::new(ZValue::Thunk(
                    Rc::new(wrap_prim(*builtin.behavior, builtin.arity)),
                    None,
                )),
            )
            .unwrap();
    }
    runtime
}

fn wrap_prim(func: PrimComp, arity: u64) -> ZCompute {
    ZCompute::Prim { arity, body: func }
}
