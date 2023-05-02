use super::syntax::*;
use crate::{prelude::*, utils::fmt::Args};

impl FmtArgs for Seq {
    fn fmt_args(&self, fargs: Args) -> String {
        let Seq(cs) = self;
        cs.into_iter().map(|c| c.fmt_args(fargs)).collect::<Vec<_>>().join("; ")
    }
}

impl FmtArgs for Skip {
    fn fmt_args(&self, _fargs: Args) -> String {
        format!("skip")
    }
}

impl FmtArgs for Push {
    fn fmt_args(&self, fargs: Args) -> String {
        let Push(c) = self;
        let mut s = String::new();
        s += "push ";
        s += &c.fmt_args(fargs);
        s
    }
}

impl FmtArgs for Pop {
    fn fmt_args(&self, _fargs: Args) -> String {
        let Pop(v, c) = self;
        let mut s = String::new();
        s += "pop ";
        s += &v.fmt_args(_fargs);
        s += " . ";
        s += &c.fmt_args(_fargs);
        s
    }
}

impl FmtArgs for Prim {
    fn fmt_args(&self, _fargs: Args) -> String {
        format!("prim(..{}..)", self.arity)
    }
}

impl FmtArgs for Module {
    fn fmt_args(&self, fargs: Args) -> String {
        let mut s = String::new();
        if let Some(name) = &self.name {
            s += &format!("module {} where", name);
            s += &fargs.br_indent();
        }
        for (var, def) in &self.define {
            s += &format!("def {} = {}", var, def.fmt_args(fargs));
            s += &fargs.br_indent();
        }
        s += &fargs.br_indent();
        if let Some(_) = &self.name {
            s += "end";
            s += &fargs.br_indent();
        }
        s
    }
}

impl FmtArgs for Program {
    fn fmt_args(&self, fargs: Args) -> String {
        let mut s = String::new();
        let Program { module, entry } = self;
        s += &module.fmt_args(fargs);
        s += &fargs.br_indent();
        s += &entry.fmt_args(fargs);
        s
    }
}
