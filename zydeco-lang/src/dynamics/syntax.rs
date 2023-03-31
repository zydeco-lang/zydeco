use crate::syntax::*;
pub use crate::{library::syntax as ls, syntax::env::Env, utils::fmt::FmtArgs};
use im::Vector;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_derive::{EnumGenerator, FmtArgs};

#[derive(Clone)]
pub struct Thunk {
    pub body: Rc<ls::ZComp>,
    pub env: Env<TermV, SemVal>,
}

#[derive(EnumGenerator, FmtArgs, Clone)]
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
    Kont(Rc<ls::ZComp>, Env<TermV, SemVal>, TermV),
    Dtor(DtorV, Vec<Rc<SemVal>>),
}

use std::fmt;
impl fmt::Debug for SemComp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemComp::Kont(comp, _, var) => {
                write!(f, "Kont({} -> {})", comp.as_ref().fmt(), var)
            }
            SemComp::Dtor(dtor, args) => write!(
                f,
                "Dtor(.{}({}))",
                dtor,
                args.into_iter()
                    .map(|arg| arg.fmt())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
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
