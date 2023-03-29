use super::{impls::*, syntax::*};
use crate::{rc, utils::span::span};
use std::collections::HashMap;

struct Builtin {
    name: &'static str,
    arity: u64,
    behavior: Box<PrimComp>,
}

impl Builtin {
    pub fn new(name: &'static str, arity: u64, behavior: PrimComp) -> Self {
        Builtin { name, arity, behavior: Box::new(behavior) }
    }
    pub fn gen(self) -> (TermV, ZVal) {
        let Builtin { name, arity, behavior } = self;
        (
            TermV::new(name.to_string(), span(0, 0)),
            Thunk(rc!(Prim { arity, body: *behavior }.into())).into(),
        )
    }
}

// To add new builtin functions, provide impl and add declaration to std.zydeco
pub(super) fn std_library() -> HashMap<TermV, ZVal> {
    [
        Builtin::new("add", 2, add),
        Builtin::new("sub", 2, sub),
        Builtin::new("mul", 2, mul),
        Builtin::new("div", 2, div),
        Builtin::new("mod", 2, modulo),
        Builtin::new("int_eq", 2, int_eq),
        Builtin::new("int_lt", 2, int_lt),
        Builtin::new("int_gt", 2, int_gt),
        Builtin::new("str_length", 1, str_length),
        Builtin::new("str_append", 2, str_append),
        Builtin::new("str_eq", 2, str_eq),
        Builtin::new("str_index", 2, str_index),
        Builtin::new("int_to_str", 1, int_to_str),
        Builtin::new("char_to_str", 1, char_to_str),
        Builtin::new("char_to_int", 1, char_to_int),
        Builtin::new("str_to_int", 1, str_to_int),
        Builtin::new("write_str", 2, write_str),
        Builtin::new("read_line", 1, read_line),
        Builtin::new("exit", 1, exit),
        Builtin::new("arg_list", 1, arg_list),
    ]
    .into_iter()
    .map(Builtin::gen)
    .collect()
}
