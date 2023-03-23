use crate::syntax::*;
pub use crate::{library::syntax as ls, syntax::env::Env};
use im::Vector;
use std::{
    fmt::Debug,
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_derive::EnumGenerator;

#[derive(Clone)]
pub struct Thunk {
    pub body: Rc<ls::TermComputation>,
    pub env: Env<TermV, TermValue>,
}

#[derive(EnumGenerator, Clone)]
pub enum TermValue {
    Thunk(Thunk),
    Ctor(Ctor<CtorV, TV>),
    Literal(Literal),
}
type TV = Rc<TermValue>;
impl ValueT for TermValue {}

#[derive(Clone)]
pub enum TermComputation {
    Ret(TermValue),
    ExitCode(i32),
}
impl ComputationT for TermComputation {}

/* --------------------------------- Runtime -------------------------------- */

#[derive(Clone)]
pub enum Frame {
    Kont(Rc<ls::TermComputation>, TermV),
    Dtor(DtorV, Vec<Rc<TermValue>>),
}

impl Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Frame::Kont(_, var) => write!(f, "Kont({})", var),
            Frame::Dtor(dtor, _) => write!(f, "Dtor({})", dtor),
        }
    }
}

pub struct Runtime<'rt> {
    pub input: &'rt mut (dyn BufRead),
    pub output: &'rt mut (dyn Write),
    pub args: &'rt [String],
    pub stack: Vector<Frame>,
    pub env: Env<TermV, TermValue>,
}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone)]
pub struct Module {
    pub name: Option<String>,
    pub entry: TermComputation,
}
