//! Stack-safe debug formatting for [`CoreProgram`] and its dense nodes.

use crate::syntax::*;
use pretty::RcDoc;
pub use zydeco_syntax::{Pretty, Ugly};

/// Formatter borrowing the owning [`CoreArena`] for variable names and child lookups.
pub struct Formatter<'arena> {
    pub arena: &'arena CoreArena,
    pub indent: isize,
}

impl<'arena> Formatter<'arena> {
    pub fn new(arena: &'arena CoreArena) -> Self {
        Self { arena, indent: 2 }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VarId {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        let name = f.arena.vars[*self].as_str();
        RcDoc::text(format!("{name}#{}", self.index()))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VPatId {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        zydeco_foundation::stack::maybe_grow(|| f.arena.vpats[*self].pretty(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueId {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        zydeco_foundation::stack::maybe_grow(|| f.arena.values[*self].pretty(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CompuId {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        zydeco_foundation::stack::maybe_grow(|| f.arena.compus[*self].pretty(f))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoreRoot {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        match self {
            | CoreRoot::Value(id) => id.pretty(f),
            | CoreRoot::Compu(id) => id.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoreProgram {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        self.root().pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValuePattern {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        match self {
            | ValuePattern::Hole(Hole) => RcDoc::text("_"),
            | ValuePattern::Var(var) => var.pretty(f),
            | ValuePattern::Ctor { tag, payload, .. } => RcDoc::concat([
                RcDoc::text(tag.name.as_str()),
                RcDoc::text("("),
                payload.pretty(f),
                RcDoc::text(")"),
            ]),
            | ValuePattern::Lit(literal) => RcDoc::text(format!("{literal:?}")),
            | ValuePattern::Alias(Alias(patterns)) => RcDoc::concat([
                RcDoc::text("("),
                RcDoc::intersperse(patterns.iter().map(|pat| pat.pretty(f)), RcDoc::text("; ")),
                RcDoc::text(")"),
            ]),
            | ValuePattern::Triv(Triv) => RcDoc::text("()"),
            | ValuePattern::VCons(VCons { items, .. }) => RcDoc::concat([
                RcDoc::text("("),
                RcDoc::intersperse(items.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
                RcDoc::text(")"),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Value {
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        match self {
            | Value::Hole(Hole) => RcDoc::text("_"),
            | Value::Var(var) => var.pretty(f),
            | Value::Let(Let { binder, bindee, tail }) => RcDoc::concat([
                RcDoc::text("let "),
                binder.pretty(f),
                RcDoc::text(" = "),
                bindee.pretty(f),
                RcDoc::text(" in"),
                RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(f.indent).group(),
            ]),
            | Value::Thunk(Thunk(body)) => {
                RcDoc::concat([RcDoc::text("{ "), body.pretty(f), RcDoc::text(" }")])
            }
            | Value::Ctor { tag, payload } => RcDoc::concat([
                RcDoc::text(tag.name.as_str()),
                RcDoc::text("("),
                payload.pretty(f),
                RcDoc::text(")"),
            ]),
            | Value::Triv(Triv) => RcDoc::text("()"),
            | Value::VCons(VCons { items, .. }) => RcDoc::concat([
                RcDoc::text("("),
                RcDoc::intersperse(items.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
                RcDoc::text(")"),
            ]),
            | Value::Proj { head, steps } => steps.iter().fold(head.pretty(f), |doc, step| {
                RcDoc::concat([doc, RcDoc::text(format!(".{}", step.position))])
            }),
            | Value::Lit(literal) => RcDoc::text(format!("{literal:?}")),
            | Value::Foreign(import) => RcDoc::text(format!("ffi[{}]", import.target().symbol)),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Computation {
    #[allow(clippy::too_many_lines)]
    fn pretty(&self, f: &'a Formatter<'a>) -> RcDoc<'a> {
        match self {
            | Computation::Hole(Hole) => RcDoc::text("_"),
            | Computation::VAbs(Abs(param, body)) => RcDoc::concat([
                RcDoc::text("fn "),
                param.pretty(f),
                RcDoc::text(" =>"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Computation::VApp(App(function, argument)) => RcDoc::concat([
                RcDoc::text("("),
                function.pretty(f),
                RcDoc::text(" "),
                argument.pretty(f),
                RcDoc::text(")"),
            ]),
            | Computation::Fix { param, body } => RcDoc::concat([
                RcDoc::text("fix "),
                param.pretty(f),
                RcDoc::text(" =>"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Computation::Force(Force(thunk)) => {
                RcDoc::concat([RcDoc::text("! "), thunk.pretty(f)])
            }
            | Computation::Ret(Return(value)) => {
                RcDoc::concat([RcDoc::text("ret "), value.pretty(f)])
            }
            | Computation::Do(Bind { binder, bindee, tail }) => RcDoc::concat([
                RcDoc::text("do "),
                binder.pretty(f),
                RcDoc::text(" <- "),
                bindee.pretty(f),
                RcDoc::text(";"),
                RcDoc::line(),
                tail.pretty(f),
            ]),
            | Computation::Let(Let { binder, bindee, tail }) => RcDoc::concat([
                RcDoc::text("let "),
                binder.pretty(f),
                RcDoc::text(" = "),
                bindee.pretty(f),
                RcDoc::text(" in"),
                RcDoc::line(),
                tail.pretty(f),
            ]),
            | Computation::Match(Match { scrut, arms }) => RcDoc::concat([
                RcDoc::text("match "),
                scrut.pretty(f),
                RcDoc::concat(arms.iter().map(|Matcher { binder, tail }| {
                    RcDoc::concat([
                        RcDoc::line(),
                        RcDoc::text("| "),
                        binder.pretty(f),
                        RcDoc::text(" => "),
                        tail.pretty(f),
                    ])
                })),
                RcDoc::line(),
                RcDoc::text("end"),
            ]),
            | Computation::CoMatch { arms } => RcDoc::concat([
                RcDoc::text("comatch"),
                RcDoc::concat(arms.iter().map(|CoMatcher { dtor, tail }| {
                    RcDoc::concat([
                        RcDoc::line(),
                        RcDoc::text("| "),
                        RcDoc::text(dtor.name.as_str()),
                        RcDoc::text(" => "),
                        tail.pretty(f),
                    ])
                })),
                RcDoc::line(),
                RcDoc::text("end"),
            ]),
            | Computation::Dtor { body, tag } => RcDoc::concat([
                RcDoc::text("("),
                body.pretty(f),
                RcDoc::text(" "),
                RcDoc::text(tag.name.as_str()),
                RcDoc::text(")"),
            ]),
        }
    }
}
