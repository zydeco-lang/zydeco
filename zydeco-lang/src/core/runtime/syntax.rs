use crate::syntax::*;
pub use crate::{core::library::syntax as ls, syntax::Env, utils::fmt::FmtArgs};
use im::Vector;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_derive::{FmtArgs, IntoEnum};

#[derive(Clone)]
pub struct Thunk {
    pub body: Rc<ls::Command>,
    pub env: Env<TermV, Value>,
}

#[derive(IntoEnum, FmtArgs, Clone)]
pub enum Value {
    Thunk(Thunk),
    Ctor(Ctor<CtorV, TV>),
    Literal(Literal),
}
type TV = Rc<Value>;
impl ValueT for Value {}

#[derive(Clone)]
pub enum ProgKont {
    Ret(Value),
    ExitCode(i32),
}
impl ComputationT for ProgKont {}

/* --------------------------------- Runtime -------------------------------- */

#[derive(Clone)]
pub enum Stack {
    Kont(Rc<ls::Command>, Env<TermV, Value>, TermV),
    Dtor(DtorV, Vec<Rc<Value>>),
}

use std::fmt;
impl fmt::Debug for Stack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stack::Kont(comp, _, var) => {
                write!(f, "Kont({} -> {})", comp.as_ref().fmt(), var)
            }
            Stack::Dtor(dtor, args) => write!(
                f,
                "Dtor(.{}({}))",
                dtor,
                args.into_iter().map(|arg| arg.fmt()).collect::<Vec<_>>().join(", ")
            ),
        }
    }
}

pub struct Runtime<'rt> {
    pub input: &'rt mut (dyn BufRead),
    pub output: &'rt mut (dyn Write),
    pub args: &'rt [String],
    pub stack: Vector<Stack>,
    pub env: Env<TermV, Value>,
}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone)]
pub struct Module {
    pub name: Option<String>,
}

#[derive(Clone)]
pub struct Program {
    pub module: Module,
    pub entry: ProgKont,
}
