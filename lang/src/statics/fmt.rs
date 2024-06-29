use super::syntax::*;
use crate::{prelude::*, utils::fmt::*};

impl FmtArgs for AbstVar {
    fn fmt_args(&self, _fargs: Args) -> String {
        format!("${}", self.0)
    }
}

impl FmtArgs for Type {
    fn fmt_args(&self, fargs: Args) -> String {
        self.synty.fmt_args(fargs)
    }
}

impl FmtArgs for Abs<(TypeV, Option<Sp<Kind>>), RcComp> {
    fn fmt_args(&self, fargs: Args) -> String {
        let Abs { param, body } = self;
        format!("fn {} -> {}", param.fmt_args(fargs), body.fmt_args(fargs))
    }
}

impl FmtArgs for App<RcComp, RcType> {
    fn fmt_args(&self, fargs: Args) -> String {
        let App { body, arg } = self;
        format!("{} @({})", body.fmt_args(fargs), arg.fmt_args(fargs))
    }
}

impl FmtArgs for Module {
    fn fmt_args(&self, args: Args) -> String {
        let mut s = String::new();
        let Module { name, data, codata, alias, define, define_ext } = self;
        if let Some(name) = name {
            s += &format!("module {} where", name);
            s += &args.br_indent();
        }
        for d in data {
            s += &d.fmt_args(args);
            s += &args.br_indent();
        }
        for d in codata {
            s += &d.fmt_args(args);
            s += &args.br_indent();
        }
        for d in alias {
            s += &d.fmt_args(args);
            s += &args.br_indent();
        }
        for DeclSymbol { public, external: _, inner: Define { name: (var, ty), def: () } } in
            define_ext
        {
            if *public {
                s += &format!("pub ");
            }
            s += &format!("extern {} : {} end", var.fmt_args(args), ty.fmt_args(args));
            s += &args.br_indent();
        }
        for d in define {
            s += &d.fmt_args(args);
            s += &args.br_indent();
        }
        s
    }
}

impl FmtArgs for Program {
    fn fmt_args(&self, args: Args) -> String {
        let Program { module, entry } = self;
        let mut s = String::new();
        s += &module.fmt_args(args);
        s += &args.br_indent();
        s += &entry.fmt_args(args);
        s
    }
}
