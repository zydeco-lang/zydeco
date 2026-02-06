#![allow(unused)]

use crate::sps::syntax::*;
use std::collections::HashMap;
use zydeco_statics::surface_syntax::ScopedArena;

pub type BuiltinMap = HashMap<&'static str, Builtin>;

#[derive(Clone, Debug)]
pub struct Builtin {
    pub name: &'static str,
    pub arity: usize,
    pub sort: BuiltinSort,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltinSort {
    Operator,
    Function,
}

impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.name, self.arity)
    }
}

impl Builtin {
    pub fn all() -> BuiltinMap {
        use BuiltinSort::*;
        [
            Builtin::new("add", 2, Operator),
            Builtin::new("sub", 2, Operator),
            Builtin::new("mul", 2, Operator),
            Builtin::new("int_eq", 2, Operator),
            Builtin::new("int_lt", 2, Operator),
            Builtin::new("int_gt", 2, Operator),
            // Builtin::new("str_length", 1, Function),
            // Builtin::new("str_append", 2, Function),
            // Builtin::new("str_split_once", 2, Function),
            // Builtin::new("str_split_n", 2, Function),
            // Builtin::new("str_eq", 2, Function),
            // Builtin::new("str_index", 2, Function),
            // Builtin::new("int_to_str", 1, Function),
            // Builtin::new("char_to_str", 1, Function),
            // Builtin::new("char_to_int", 1, Function),
            // Builtin::new("str_to_int", 1, Function),
            // Builtin::new("write_str", 2, Function),
            Builtin::new("write_line", 2, Function),
            Builtin::new("read_line", 1, Function),
            // Builtin::new("read_line_as_int", 1, Function),
            // Builtin::new("read_till_eof", 1, Function),
            // Builtin::new("arg_list", 1, Function),
            // Builtin::new("random_int", 1, Function),
            Builtin::new("exit", 1, Function),
        ]
        .into_iter()
        .map(Self::generate)
        .collect()
    }
    pub fn new(name: &'static str, arity: usize, sort: BuiltinSort) -> Self {
        Builtin { name, arity, sort }
    }
    fn generate<'a>(self) -> (&'a str, Self) {
        (self.name, self)
    }

    /// Turn a builtin operator definition into returning a complex CBPV value,
    /// pop parameters from stack (CBPV function), and finally wrap it with closure.
    pub fn make_operator<Arena>(&self, arena: &mut Arena) -> ValueId
    where
        Arena: AsMut<StackirArena> + AsMut<ScopedArena>,
    {
        let op = self.name;
        // make fresh variables as operands
        let params: Vec<_> = (0..self.arity)
            .map(|i| {
                let param = VarName::from(format!("param_{}", i));
                let scoped: &mut ScopedArena = arena.as_mut();
                scoped.defs.alloc(param)
            })
            .collect();
        let operands = params.iter().map(|def| def.build(arena, None)).collect();
        // construct the complex value
        let complex = Complex { operator: op, operands }.build(arena, None);
        // construct the computation of returning the complex value
        let stack = Bullet.build(arena, None);
        let mut tail = SReturn { stack, value: complex }.build(arena, None);
        // construct the let-argument (CBPV function) wrapping the return computation
        for def in params.into_iter().rev() {
            let vpat = def.build(arena, None);
            let binder = Cons(vpat, Bullet);
            let bindee = Bullet.build(arena, None);
            tail = Computation::LetArg(Let { binder, bindee, tail }).build(arena, None);
        }
        // construct the closure wrapping the whole computation
        Closure { capture: Context::new(), stack: Bullet, body: tail }.build(arena, None)
    }

    /// Wrap a builtin function definition with closure.
    pub fn make_function<Arena>(&self, arena: &mut Arena) -> ValueId
    where
        Arena: AsMut<StackirArena> + AsMut<ScopedArena>,
    {
        let function = self.name;
        let body = ExternCall { function, stack: Bullet }.build(arena, None);
        Closure { capture: Context::new(), stack: Bullet, body }.build(arena, None)
    }
}
