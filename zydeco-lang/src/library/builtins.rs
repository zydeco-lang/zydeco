use super::impls::*;
use crate::dynamics::{
    env::Env,
    syntax::{PrimComp, ZCompute, ZValue},
};
use std::rc::Rc;

struct Builtin {
    name: &'static str,
    arity: u64,
    behavior: Box<PrimComp>,
}

impl Builtin {
    pub fn new(name: &'static str, arity: u64, behavior: PrimComp) -> Self {
        Builtin { name, arity, behavior: Box::new(behavior) }
    }
}

// Only need to change here (and provide impl) to add new builtin function
fn std_library() -> Vec<Builtin> {
    vec![
        Builtin::new("add", 2, add),
        Builtin::new("sub", 2, sub),
        Builtin::new("mul", 2, mul),
        Builtin::new("mod", 2, modulo),
        Builtin::new("int_eq", 2, int_eq),
        Builtin::new("int_lt", 2, int_lt),
        Builtin::new("int_gt", 2, int_gt),
        Builtin::new("str_length", 2, str_length),
        Builtin::new("str_append", 2, str_append),
        Builtin::new("str_eq", 2, str_eq),
        Builtin::new("str_index", 2, str_index),
        Builtin::new("int_to_str", 1, int_to_str),
        Builtin::new("char_to_str", 1, char_to_str),
        Builtin::new("str_to_int", 1, str_to_int),
        Builtin::new("write_line", 2, write_line),
        Builtin::new("read_line", 1, read_line),
        Builtin::new("exit", 1, exit),
        Builtin::new("arg_list", 1, arg_list),
    ]
}

pub fn link_builtin(env: &mut Env) {
    for builtin in std_library() {
        env.insert(
            builtin.name.to_string(),
            Rc::new(ZValue::Thunk(
                Rc::new({
                    let arity = builtin.arity;
                    let body = *builtin.behavior;
                    ZCompute::Prim { arity, body }
                }),
                Some(Env::new()),
            )),
        );
    }
}
