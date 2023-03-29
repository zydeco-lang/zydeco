use super::syntax::{self as ds, *};
use crate::utils::fmt::{Args, FmtArgs};

impl FmtArgs for ds::Thunk {
    fn fmt_args(&self, fargs: Args) -> String {
        format!("{{ {} }}", self.body.fmt_args(fargs))
    }
}

impl FmtArgs for SemVal {
    fn fmt_args(&self, fargs: Args) -> String {
        match self {
            SemVal::Thunk(v) => v.fmt_args(fargs),
            SemVal::Ctor(v) => v.fmt_args(fargs),
            SemVal::Literal(v) => v.fmt_args(fargs),
        }
    }
}

impl FmtArgs for ProgKont {
    fn fmt_args(&self, fargs: Args) -> String {
        match self {
            ProgKont::Ret(v) => v.fmt_args(fargs),
            ProgKont::ExitCode(c) => format!("exit({})", c),
        }
    }
}

impl FmtArgs for Module {
    fn fmt_args(&self, fargs: Args) -> String {
        self.entry.fmt_args(fargs)
    }
}
