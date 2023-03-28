use super::syntax::{self as ds, *};
use crate::utils::fmt::{Args, FmtArgs};

impl FmtArgs for ds::Thunk {
    fn fmt_args(&self, fargs: Args) -> String {
        format!("{{ {} }}", self.body.fmt_args(fargs))
    }
}

impl FmtArgs for TermValue {
    fn fmt_args(&self, fargs: Args) -> String {
        match self {
            TermValue::Thunk(v) => v.fmt_args(fargs),
            TermValue::Ctor(v) => v.fmt_args(fargs),
            TermValue::Literal(v) => v.fmt_args(fargs),
        }
    }
}

impl FmtArgs for TermComputation {
    fn fmt_args(&self, fargs: Args) -> String {
        match self {
            TermComputation::Ret(v) => v.fmt_args(fargs),
            TermComputation::ExitCode(c) => format!("exit({})", c),
        }
    }
}

impl FmtArgs for Module {
    fn fmt_args(&self, fargs: Args) -> String {
        self.entry.fmt_args(fargs)
    }
}
