use super::syntax::{self as ds, *};
use crate::utils::fmt::{Args, FmtArgs};

impl FmtArgs for ds::Thunk {
    fn fmt_args(&self, fargs: Args) -> String {
        let ds::Thunk { body, env: _ } = self;
        let mut s = String::new();
        s += "{ ";
        let fargs = fargs.indent();
        s += &body.fmt_args(fargs);
        s += " }";
        s
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
    fn fmt_args(&self, _fargs: Args) -> String {
        String::new()
    }
}

impl FmtArgs for Program {
    fn fmt_args(&self, fargs: Args) -> String {
        let Program { module, entry } = self;
        let mut s = String::new();
        s += &module.fmt_args(fargs);
        s += &entry.fmt_args(fargs);
        s
    }
}
