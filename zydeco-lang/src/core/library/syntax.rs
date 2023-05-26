use crate::core::runtime::syntax as ds;
pub use crate::syntax::*;
use derive_more::From;
use im::Vector;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_derive::FmtArgs;

#[derive(From, FmtArgs, Clone)]
pub enum Value {
    Var(TermV),
    Thunk(Thunk<RcCommand>),
    Ctor(Ctor<CtorV, RcValue>),
    Literal(Literal),
    SemValue(ds::Value),
}
type RcValue = Rc<Value>;
impl ValueT for Value {}

pub type PrimComp =
    fn(Vec<ds::Value>, &mut (dyn BufRead), &mut (dyn Write), &[String]) -> Result<Command, i32>;

#[derive(Clone)]
pub struct Prim {
    pub arity: u64,
    pub body: PrimComp,
}

#[derive(Clone)]
pub struct Seq(pub Vector<RcCommand>);

#[derive(Clone)]
pub struct Skip();

#[derive(Clone)]
pub struct Push(pub RcValue);

#[derive(Clone)]
pub struct Pop(pub TermV, pub RcCommand);

#[derive(From, FmtArgs, Clone)]
pub enum Command {
    Seq(Seq),
    Skip(Skip),
    // Thunk (closure call)
    Force(Force<RcValue>),
    // Ret and Do binding
    Push(Push),
    Pop(Pop),
    // Data
    Match(Match<CtorV, TermV, RcValue, RcCommand>),
    // Codata
    Dtor(Dtor<RcCommand, DtorV, RcValue>),
    Comatch(Comatch<DtorV, TermV, RcCommand>),
    // Prim
    Prim(Prim),
}
type RcCommand = Rc<Command>;
impl ComputationT for Command {}

/* --------------------------------- Module --------------------------------- */

#[derive(Clone)]
pub struct Module {
    pub name: Option<String>,
    pub define: Vector<(TermV, Value)>,
}

#[derive(Clone)]
pub struct Program {
    pub module: Module,
    pub entry: Command,
}
