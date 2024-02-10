use crate::syntax::*;
pub use crate::{library::syntax as ls, syntax::Env, utils::fmt::FmtArgs};
use derive_more::From;
use im::Vector;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_derive::FmtArgs;

#[derive(Clone)]
pub struct Thunk {
    pub body: Rc<ls::SynComp>,
    pub env: Env<TermV, SemVal>,
}

#[derive(From, FmtArgs, Clone)]
pub enum SemVal {
    Thunk(Thunk),
    Ctor(Ctor<CtorV, TV>),
    Literal(Literal),
}
type TV = Rc<SemVal>;
impl ValueT for SemVal {}

#[derive(Clone)]
pub enum ProgKont {
    Ret(SemVal),
    ExitCode(i32),
}
impl ComputationT for ProgKont {}

/* --------------------------------- Runtime -------------------------------- */

#[derive(Clone)]
pub enum SemComp {
    Kont(Rc<ls::SynComp>, Env<TermV, SemVal>, TermV),
    App(Rc<SemVal>),
    Dtor(DtorV),
}

use std::fmt;
impl fmt::Debug for SemComp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemComp::Kont(comp, _, var) => {
                write!(f, "Kont({} -> {})", comp.as_ref().fmt(), var)
            }
            SemComp::App(val) => write!(f, "App({})", val.as_ref().fmt()),
            SemComp::Dtor(dtor) => write!(f, "Dtor(.{})", dtor,),
        }
    }
}

pub struct Runtime<'rt> {
    pub input: &'rt mut (dyn BufRead),
    pub output: &'rt mut (dyn Write),
    pub args: &'rt [String],
    pub stack: Vector<SemComp>,
    pub env: Env<TermV, SemVal>,
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
