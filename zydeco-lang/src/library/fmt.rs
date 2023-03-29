use super::syntax as ls;
use crate::utils::fmt::{Args, FmtArgs};

impl FmtArgs for ls::ZVal {
    fn fmt_args(&self, fargs: Args) -> String {
        match self {
            ls::ZVal::Var(v) => v.fmt_args(fargs),
            ls::ZVal::Thunk(v) => v.fmt_args(fargs),
            ls::ZVal::Ctor(v) => v.fmt_args(fargs),
            ls::ZVal::Literal(v) => v.fmt_args(fargs),
            ls::ZVal::SemValue(v) => v.fmt_args(fargs),
        }
    }
}

impl FmtArgs for ls::ZComp {
    fn fmt_args(&self, fargs: Args) -> String {
        match self {
            ls::ZComp::Ret(c) => c.fmt_args(fargs),
            ls::ZComp::Force(c) => c.fmt_args(fargs),
            ls::ZComp::Let(c) => c.fmt_args(fargs),
            ls::ZComp::Do(c) => c.fmt_args(fargs),
            ls::ZComp::Rec(c) => c.fmt_args(fargs),
            ls::ZComp::Match(c) => c.fmt_args(fargs),
            ls::ZComp::CoMatch(c) => c.fmt_args(fargs),
            ls::ZComp::Dtor(c) => c.fmt_args(fargs),
            ls::ZComp::Prim(c) => format!("prim(..{}..)", c.arity),
        }
    }
}

impl FmtArgs for ls::Module {
    fn fmt_args(&self, fargs: Args) -> String {
        let mut s = String::new();
        if let Some(name) = &self.name {
            s += &format!("module {} where", name);
            s += &fargs.force_space();
        }
        for (var, def) in &self.define {
            s += &format!("def {} = {}", var, def.fmt_args(fargs));
            s += &fargs.force_space();
        }
        s += &fargs.force_space();
        s += &self.entry.fmt_args(fargs);
        s += &fargs.force_space();
        if let Some(_) = &self.name {
            s += "end";
            s += &fargs.force_space();
        }
        s
    }
}
