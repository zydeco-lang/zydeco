pub use zydeco_syntax::*;

use crate::{
    env::EvalEnv,
    host::{HostRuntime, HostValue},
};
use derive_more::From;
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_ir::{CoreProgram, DtorIdx, syntax as ir};

#[cfg(test)]
mod tests;

/* -------------------------------- Semantics ------------------------------- */

/// Runtime target executed when forcing a closure value.
#[derive(Clone, Debug)]
pub enum ThunkBody {
    Core { body: ir::CompuId, env: EvalEnv<SemValue> },
    Fix { param: ir::VarId, body: ir::CompuId, env: EvalEnv<SemValue> },
    Builtin(BuiltinValueRole),
    Foreign(CheckedImport),
}

/// Semantic values produced and passed by the evaluator.
#[derive(From, Clone, Debug)]
pub enum SemValue {
    Thunk(Rc<ThunkBody>),
    Ctor(Ctor<CtorName, Rc<SemValue>>),
    Triv(Triv),
    VCons(Rc<[SemValue]>),
    Literal(Literal),
    Host(HostValue),
}

impl SemValue {
    fn drain_children(&mut self, work: &mut Vec<SemValue>) {
        match self {
            | Self::Ctor(Ctor(_, payload)) => {
                if let Some(inner) = Rc::get_mut(payload) {
                    let extracted = std::mem::replace(inner, Self::Triv(Triv));
                    if matches!(extracted, Self::Ctor(_) | Self::VCons(_) | Self::Thunk(_)) {
                        work.push(extracted);
                    }
                }
            }
            | Self::VCons(fields) => {
                if let Some(slice) = Rc::get_mut(fields) {
                    for item in slice.iter_mut() {
                        if matches!(item, Self::Ctor(_) | Self::VCons(_) | Self::Thunk(_)) {
                            work.push(std::mem::replace(item, Self::Triv(Triv)));
                        }
                    }
                }
            }
            | Self::Thunk(_) | Self::Triv(_) | Self::Literal(_) | Self::Host(_) => {}
        }
    }
}

impl Drop for SemValue {
    fn drop(&mut self) {
        if !matches!(self, Self::Ctor(_) | Self::VCons(_)) {
            return;
        }
        let mut work = Vec::new();
        self.drain_children(&mut work);
        while let Some(mut next) = work.pop() {
            next.drain_children(&mut work);
        }
    }
}

/// Runtime stack frames for computations.
#[derive(Clone, Debug)]
pub enum SemCompu {
    Kont { body: ir::CompuId, env: EvalEnv<SemValue>, binder: ir::VPatId },
    App(SemValue),
    Dtor(DtorIdx),
}

/// Result of one host builtin or foreign primitive invocation.
#[derive(Clone, Debug)]
pub enum HostAction {
    Return(SemValue),
    Resume { continuation: SemValue, arguments: Vec<SemValue> },
}

/// Mutable runtime state threaded through evaluation of a borrowed [`CoreProgram`].
pub struct Runtime<'prog, 'rt> {
    pub input: &'rt mut dyn BufRead,
    pub output: &'rt mut dyn Write,
    pub stderr: &'rt mut dyn Write,
    pub args: &'rt [String],
    pub(crate) host: HostRuntime,
    pub(crate) foreign: crate::foreign::ForeignRuntime,
    pub stack: Vec<SemCompu>,
    pub env: EvalEnv<SemValue>,
    pub(crate) packages: Vec<SemValue>,
    pub program: &'prog CoreProgram,
}

/// Program-level continuation produced by evaluation.
#[derive(Clone, Debug)]
pub enum ProgKont {
    Ret(SemValue),
    ExitCode(i32),
    Error(RuntimeError),
}

/// Failures reported to the host without unwinding the evaluator.
#[derive(Clone, Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("integer exceeds the tagged payload range")]
    IntegerRange,
    #[error("pattern match failed")]
    PatternMatch,
    #[error("integer division by zero")]
    IntegerDivisionByZero,
    #[error("integer remainder by zero")]
    IntegerRemainderByZero,
    #[error(transparent)]
    Foreign(#[from] crate::foreign::ForeignRuntimeError),
}
