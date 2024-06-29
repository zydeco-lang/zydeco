use super::syntax as ls;
use crate::utils::fmt::{Args, FmtArgs};

impl FmtArgs for ls::Prim {
    fn fmt_args(&self, _fargs: Args) -> String {
        format!("prim(..{}..)", self.arity)
    }
}

impl FmtArgs for ls::Module {
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

impl FmtArgs for ls::Program {
    fn fmt_args(&self, fargs: Args) -> String {
        let mut s = String::new();
        let ls::Program { module, entry } = self;
        s += &module.fmt_args(fargs);
        s += &fargs.br_indent();
        s += &entry.fmt_args(fargs);
        s
    }
}
