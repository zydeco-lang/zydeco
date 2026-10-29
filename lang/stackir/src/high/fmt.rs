//! Printing for lexical SPS: its identifiers, its own forms, and its program.

use super::syntax::*;
use pretty::RcDoc;

pub use crate::fmt::Pretty;

pub type Formatter<'a> = crate::fmt::Formatter<'a, Sps>;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let name = f.admin.def_name(f.scoped, f.statics, self);
        RcDoc::text(format!("{}{}", name.ugly(&f.statics_formatter()), self.concise()))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VPatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        f.inner.vpats[self].pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match f.inner.values.get(self) {
            | Some(value) => value.pretty(f),
            | None => RcDoc::text(format!("<value:{}>", self.concise())),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for StackId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        f.inner.stacks[self].pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CompuId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match f.inner.compus.get(self) {
            | Some(compu) => compu.pretty(f),
            | None => RcDoc::text(format!("<compu:{}>", self.concise())),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueExt {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Self::Closure(Closure { stack, body }) => RcDoc::concat([
                RcDoc::text("{"),
                RcDoc::space(),
                stack.pretty(f),
                RcDoc::space(),
                RcDoc::text("->"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
                RcDoc::space(),
                RcDoc::text("}"),
            ])
            .group(),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for StackExt {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Self::Kont(Kont { binder, body }) => RcDoc::concat([
                RcDoc::text("(kont"),
                RcDoc::space(),
                binder.pretty(f),
                RcDoc::space(),
                RcDoc::text("->"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
                RcDoc::text(")"),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CompuExt {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Self::Fix(SFix { param, stack, body }) => RcDoc::concat([
                RcDoc::text("fix"),
                RcDoc::space(),
                param.pretty(f),
                RcDoc::space(),
                RcDoc::text("!"),
                RcDoc::space(),
                stack.pretty(f),
                RcDoc::space(),
                RcDoc::text("->"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Self::Force(SForce { thunk, stack }) => RcDoc::concat([
                thunk.pretty(f),
                RcDoc::space(),
                RcDoc::text("!"),
                RcDoc::space(),
                stack.pretty(f),
            ]),
            | Self::Ret(SReturn { stack, value }) => RcDoc::concat([
                stack.pretty(f),
                RcDoc::space(),
                RcDoc::text("@"),
                RcDoc::space(),
                value.pretty(f),
            ]),
            | Self::LetProc(LetProc { procedure, stack, body, tail }) => RcDoc::concat([
                RcDoc::text("letproc"),
                RcDoc::space(),
                procedure.pretty(f),
                RcDoc::space(),
                stack.pretty(f),
                RcDoc::space(),
                RcDoc::text("="),
                RcDoc::concat([
                    RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent),
                    RcDoc::line(),
                    RcDoc::text("in"),
                ])
                .group(),
                RcDoc::concat([RcDoc::line(), tail.pretty(f)]).group(),
            ]),
            | Self::CallProc(CallProc { procedure, stack }) => RcDoc::concat([
                procedure.pretty(f),
                RcDoc::space(),
                RcDoc::text("!"),
                RcDoc::space(),
                stack.pretty(f),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SpsProgram {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("[root]")
            .append(RcDoc::concat([RcDoc::line(), self.root().pretty(f)]).nest(f.indent))
            .append(RcDoc::line())
    }
}
