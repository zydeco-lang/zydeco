#![allow(unused)]

use crate::syntax::*;
use std::collections::HashMap;
use zydeco_statics::surface_syntax::ScopedArena;

pub type BuiltinMap = HashMap<&'static str, Builtin>;

#[derive(Clone, Debug)]
pub struct Builtin {
    pub name: &'static str,
    pub arity: usize,
    pub sort: BuiltinSort,
}

#[derive(Clone, Debug)]
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
            Builtin::new("div", 2, Operator),
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
    pub fn make_operator<Arena>(self, arena: &mut Arena) -> ValueId
    where
        Arena: AsMut<StackArena> + AsMut<ScopedArena>,
    {
        let op = serde_plain::from_str::<Operator>(self.name).unwrap();
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
    pub fn make_function<Arena>(self, arena: &mut Arena) -> ValueId
    where
        Arena: AsMut<StackArena> + AsMut<ScopedArena>,
    {
        let function = serde_plain::from_str::<Function>(self.name).unwrap();
        let body = ExternCall { function, arity: self.arity, stack: Bullet }.build(arena, None);
        Closure { capture: Context::new(), stack: Bullet, body }.build(arena, None)
    }
}

// // Alternative trait-based implementation

// pub trait BuiltinImpl<Arena> {
//     fn make(self, arena: &mut Arena) -> ValueId
//     where
//         Arena: AsMut<StackArena> + AsMut<ScopedArena>;
// }

// pub trait IntrinsicOperator: Into<Operator> {}
// impl<I, Arena> BuiltinImpl<Arena> for I
// where
//     I: IntrinsicOperator,
// {
//     fn make(self, arena: &mut Arena) -> ValueId
//     where
//         Arena: AsMut<StackArena> + AsMut<ScopedArena>,
//     {
//         let op = self.into();
//         // make fresh variables as operands
//         let params: Vec<_> = (0..2)
//             .map(|i| {
//                 let param = VarName::from(format!("param_{}", i));
//                 let scoped: &mut ScopedArena = arena.as_mut();
//                 scoped.defs.alloc(param)
//             })
//             .collect();
//         let operands = params.iter().map(|def| def.build(arena, None)).collect();
//         // construct the complex value
//         let complex = Complex { operator: op, operands }.build(arena, None);
//         // construct the computation of returning the complex value
//         let stack = Bullet.build(arena, None);
//         let mut tail = SReturn { stack, value: complex }.build(arena, None);
//         // construct the let-argument (CBPV function) wrapping the return computation
//         for def in params.into_iter().rev() {
//             let vpat = def.build(arena, None);
//             let binder = Cons(vpat, Bullet);
//             let bindee = Bullet.build(arena, None);
//             tail = Computation::LetArg(Let { binder, bindee, tail }).build(arena, None);
//         }
//         // construct the closure wrapping the whole computation
//         Closure { capture: Context::new(), stack: Bullet, body: tail }.build(arena, None)
//     }
// }

// pub struct IntrinsicAdd;
// impl Into<Operator> for IntrinsicAdd {
//     fn into(self) -> Operator {
//         Operator::Add
//     }
// }
// impl IntrinsicOperator for IntrinsicAdd {}
