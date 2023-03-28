use super::syntax as ls;
use crate::utils::fmt::{Args, FmtArgs};

impl FmtArgs for ls::TermValue {
    fn fmt_args(&self, fargs: Args) -> String {
        match self {
            ls::TermValue::Var(v) => v.fmt_args(fargs),
            ls::TermValue::Thunk(v) => v.fmt_args(fargs),
            ls::TermValue::Ctor(v) => v.fmt_args(fargs),
            ls::TermValue::Literal(v) => v.fmt_args(fargs),
            ls::TermValue::SemValue(v) => v.fmt_args(fargs),
        }
    }
}

impl FmtArgs for ls::TermComputation {
    fn fmt_args(&self, fargs: Args) -> String {
        match self {
            ls::TermComputation::Ret(c) => c.fmt_args(fargs),
            ls::TermComputation::Force(c) => c.fmt_args(fargs),
            ls::TermComputation::Let(c) => c.fmt_args(fargs),
            ls::TermComputation::Do(c) => c.fmt_args(fargs),
            ls::TermComputation::Rec(c) => c.fmt_args(fargs),
            ls::TermComputation::Match(c) => c.fmt_args(fargs),
            ls::TermComputation::CoMatch(c) => c.fmt_args(fargs),
            ls::TermComputation::Dtor(c) => c.fmt_args(fargs),
            ls::TermComputation::Prim(c) => format!("prim(..{}..)", c.arity),
        }
    }
}
