//! Printing for first-order SPSLow: its identifiers, its own forms, and its program.

use super::{check::SpsLowProgram, syntax::*};
use pretty::RcDoc;

pub use crate::fmt::Pretty;

pub type Formatter<'a> = crate::fmt::Formatter<'a, SpsLow>;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let name = f.admin.def_name(f.scoped, f.statics, self);
        RcDoc::text(format!("{}{}", name.plain(), self.concise()))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VPatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        f.inner.vpats[self].pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let value = f.inner.values[self].pretty(f);
        match f.inner.entry_protocols.get(self) {
            | Some(protocol) => RcDoc::text(format!("{protocol} ")).append(value),
            | None => value,
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
        f.inner.compus[self].pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueExt {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Self::Block(Block { label, entry, body }) => RcDoc::concat([
                RcDoc::text("block"),
                RcDoc::space(),
                RcDoc::text("["),
                label.pretty(f),
                RcDoc::text("]"),
                RcDoc::text("("),
                RcDoc::intersperse(
                    entry.words().map(|(role, pattern)| {
                        RcDoc::concat([RcDoc::text(format!("{role}: ")), pattern.pretty(f)])
                    }),
                    RcDoc::text(", "),
                ),
                RcDoc::text(")"),
                RcDoc::space(),
                RcDoc::text("•"),
                RcDoc::space(),
                RcDoc::text("->"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Self::ClosurePackage(ClosurePackage { environment, code }) => RcDoc::concat([
                RcDoc::text("pack-closure("),
                environment.pretty(f),
                RcDoc::text(", "),
                code.pretty(f),
                RcDoc::text(")"),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for StackExt {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Self::ContinuationPackage(ContinuationPackage { code, residual }) => RcDoc::concat([
                RcDoc::text("pack-continuation("),
                code.pretty(f),
                RcDoc::text(", "),
                residual.pretty(f),
                RcDoc::text(")"),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CompuExt {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Self::Jump(Jump { target, argument, stack }) => RcDoc::concat([
                RcDoc::text("jump "),
                target.pretty(f),
                RcDoc::text(format!("({}: ", argument.word().0)),
                argument.word().1.pretty(f),
                RcDoc::text(")"),
                RcDoc::text(" ! "),
                stack.pretty(f),
            ]),
            | Self::LetLabel(LetLabel { label, kind, body, tail }) => RcDoc::concat([
                RcDoc::text(match kind {
                    | LabelKind::Local => "label ",
                    | LabelKind::Closed => "closed label ",
                }),
                label.pretty(f),
                RcDoc::text(" • ->"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
                RcDoc::line(),
                RcDoc::text("in"),
                RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(f.indent).group(),
            ]),
            | Self::JumpLabel(JumpLabel { label, stack }) => RcDoc::concat([
                RcDoc::text("jump "),
                label.pretty(f),
                RcDoc::text(" ! "),
                stack.pretty(f),
            ]),
            | Self::OpenClosure(OpenClosure { package, environment, code, body }) => {
                RcDoc::concat([
                    RcDoc::text("open-closure "),
                    package.pretty(f),
                    RcDoc::text(" as ("),
                    environment.pretty(f),
                    RcDoc::text(", "),
                    code.pretty(f),
                    RcDoc::text(") in"),
                    RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
                ])
            }
            | Self::OpenContinuation(OpenContinuation { package, code, body }) => RcDoc::concat([
                RcDoc::text("open-continuation "),
                package.pretty(f),
                RcDoc::text(" as "),
                code.pretty(f),
                RcDoc::text(" :: • in"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SpsLowProgram {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat(f.inner.protocols.parameters().map(|(id, kind)| {
            RcDoc::text(format!("[parameter:{id}] {kind}")).append(RcDoc::line())
        }))
        .append(RcDoc::concat(f.inner.protocols.iter().map(|(id, definition)| {
            RcDoc::text(format!("[protocol:{id}] {definition}")).append(RcDoc::line())
        })))
        .append(RcDoc::text("[root]"))
        .append(RcDoc::concat([RcDoc::line(), self.root().pretty(f)]).nest(f.indent))
        .append(RcDoc::line())
    }
}
