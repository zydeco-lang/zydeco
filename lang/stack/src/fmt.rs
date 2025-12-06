//! Formatters for the Zydeco Intermediate Representation (ZIR).

use super::syntax::*;
use zydeco_statics::tyck::syntax as ss;
use zydeco_surface::scoped::syntax::ScopedArena;

/* -------------------------------- Formatter ------------------------------- */

pub use zydeco_syntax::{Pretty, Ugly};
pub struct Formatter<'arena> {
    arena: &'arena StackArena,
    scoped: &'arena ScopedArena,
    statics: &'arena ss::StaticsArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(
        arena: &'arena StackArena, scoped: &'arena ScopedArena, statics: &'arena ss::StaticsArena,
    ) -> Self {
        Formatter { arena, scoped, statics }
    }

    fn statics_fmt(&self) -> zydeco_statics::tyck::fmt::Formatter<'arena> {
        zydeco_statics::tyck::fmt::Formatter::new(self.scoped, self.statics)
    }
}

/* ---------------------------------- Ugly ---------------------------------- */

impl<'a> Ugly<'a, Formatter<'a>> for DefId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let name = &f.scoped.defs[self];
        let statics_fmt = f.statics_fmt();
        s += &name.ugly(&statics_fmt);
        s += &self.concise();
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VPatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let statics_fmt = f.statics_fmt();
        self.ugly(&statics_fmt)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ValueId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let value = &f.arena.values[self];
        value.ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Value {
    fn ugly(&self, f: &'a Formatter) -> String {
        let statics_fmt = f.statics_fmt();
        match self {
            | Value::Hole(Hole) => "_".to_string(),
            | Value::Var(def) => def.ugly(f),
            | Value::Clo(Clo { capture, stack, body }) => {
                let mut s = String::new();
                if !capture.is_empty() {
                    s += " [";
                    s += &capture.iter().map(|d| d.ugly(f)).collect::<Vec<_>>().join(", ");
                    s += "] ";
                }
                s += &format!("{{ {} -> {} }}", stack.ugly(f), body.ugly(f));
                s
            }
            | Value::Ctor(Ctor(name, val)) => {
                format!("{}({})", name.ugly(&statics_fmt), val.ugly(f))
            }
            | Value::Triv(Triv) => "()".to_string(),
            | Value::VCons(Cons(a, b)) => format!("({}, {})", a.ugly(f), b.ugly(f)),
            | Value::Lit(lit) => lit.ugly(&statics_fmt),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for StackId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let stack = &f.arena.stacks[self];
        stack.ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Stack {
    fn ugly(&self, f: &'a Formatter) -> String {
        let statics_fmt = f.statics_fmt();
        match self {
            | Stack::Kont(s) => format!("({})", s.ugly(f)),
            | Stack::Var(s) => s.ugly(f),
            | Stack::Arg(Cons(val, stack)) => format!("arg({}) :: {}", val.ugly(f), stack.ugly(f)),
            | Stack::Tag(Cons(dtor, stack)) => {
                format!("tag({}) :: {}", dtor.ugly(&statics_fmt), stack.ugly(f))
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Bullet {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "•".to_string()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Kont {
    fn ugly(&self, f: &'a Formatter) -> String {
        format!("kont {} -> {}", self.binder.ugly(f), self.body.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CompuId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let compu = &f.arena.compus[self];
        compu.ugly(f)
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Computation {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | Computation::Hole(Hole) => "_".to_string(),
            | Computation::Fix(Fix(param, body)) => {
                format!("fix {} -> {}", param.ugly(f), body.ugly(f))
            }
            | Computation::Force(SForce { thunk, stack }) => {
                format!("{} ! {}", thunk.ugly(f), stack.ugly(f))
            }
            | Computation::Ret(SReturn { stack, value }) => {
                format!("{} @ {}", stack.ugly(f), value.ugly(f))
            }
            | Computation::Case(Match { scrut, arms }) => {
                let mut s = String::new();
                s += &format!("case {}", scrut.ugly(f));
                for Matcher { binder, tail } in arms.iter() {
                    s += &format!(" | {} -> {}", binder.ugly(f), tail.ugly(f));
                }
                s += " end";
                s
            }
            | Computation::LetValue(Let { binder, bindee, tail }) => {
                format!("let {} = {} in {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
            }
            | Computation::LetStack(Let { binder, bindee, tail }) => {
                format!("let {} = {} in {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
            }
            | Computation::LetArg(Let { binder, bindee, tail }) => {
                let Cons(param, Bullet) = binder;
                format!(
                    "let arg({}) :: {} = {} in {}",
                    param.ugly(f),
                    Bullet.ugly(f),
                    bindee.ugly(f),
                    tail.ugly(f)
                )
            }
            | Computation::CoCase(CoMatch { arms }) => {
                let mut s = String::new();
                let statics_fmt = f.statics_fmt();
                s += "cocase";
                for CoMatcher { dtor, tail } in arms.iter() {
                    let Cons(dtor_name, Bullet) = dtor;
                    s += &format!(" | tag({}) -> {}", dtor_name.ugly(&statics_fmt), tail.ugly(f));
                }
                s += " end";
                s
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TermId {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | TermId::Value(v) => v.ugly(f),
            | TermId::Compu(c) => c.ugly(f),
            | TermId::Stack(s) => s.ugly(f),
        }
    }
}

/* --------------------------------- Pretty --------------------------------- */

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let name = &f.scoped.defs[self];
        let statics_fmt = zydeco_statics::tyck::fmt::Formatter::new(f.scoped, f.statics);
        RcDoc::text(format!("{}{}", name.ugly(&statics_fmt), self.concise()))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VPatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let statics_fmt = zydeco_statics::tyck::fmt::Formatter::new(f.scoped, f.statics);
        RcDoc::text(self.ugly(&statics_fmt))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let value = &f.arena.values[self];
        value.pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Value {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Value::Hole(Hole) => RcDoc::text("_"),
            | Value::Var(def) => def.pretty(f),
            | Value::Clo(Clo { capture, stack, body }) => {
                let mut doc = RcDoc::nil();
                if !capture.is_empty() {
                    let capture_doc = if capture.len() == 1 {
                        capture[0].pretty(f)
                    } else {
                        RcDoc::concat(
                            capture
                                .iter()
                                .map(|d| d.pretty(f))
                                .enumerate()
                                .flat_map(|(i, d)| {
                                    if i == 0 {
                                        vec![d]
                                    } else {
                                        vec![RcDoc::text(", "), d]
                                    }
                                })
                                .collect::<Vec<_>>(),
                        )
                    };
                    doc = doc
                        .append(RcDoc::text("["))
                        .append(capture_doc)
                        .append(RcDoc::text("] "))
                        .group();
                }
                doc.append(
                    RcDoc::concat([
                        RcDoc::text("{"),
                        RcDoc::space(),
                        stack.pretty(f),
                        RcDoc::space(),
                        RcDoc::text("->"),
                        RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(2).group(),
                        RcDoc::space(),
                        RcDoc::text("}"),
                    ])
                    .group(),
                )
            }
            | Value::Ctor(Ctor(name, val)) => {
                let statics_fmt = zydeco_statics::tyck::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::concat([
                    RcDoc::text(name.ugly(&statics_fmt)),
                    RcDoc::text("("),
                    val.pretty(f),
                    RcDoc::text(")"),
                ])
            }
            | Value::Triv(Triv) => RcDoc::text("()"),
            | Value::VCons(Cons(a, b)) => RcDoc::concat([
                RcDoc::text("("),
                a.pretty(f),
                RcDoc::text(","),
                RcDoc::space(),
                b.pretty(f),
                RcDoc::text(")"),
            ]),
            | Value::Lit(lit) => {
                let statics_fmt = zydeco_statics::tyck::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::text(lit.ugly(&statics_fmt))
            }
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for StackId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let stack = &f.arena.stacks[self];
        stack.pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Stack {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Stack::Kont(s) => RcDoc::concat([RcDoc::text("("), s.pretty(f), RcDoc::text(")")]),
            | Stack::Var(s) => s.pretty(f),
            | Stack::Arg(Cons(val, stack)) => RcDoc::concat([
                RcDoc::text("arg("),
                val.pretty(f),
                RcDoc::text(")"),
                RcDoc::space(),
                RcDoc::text("::"),
                RcDoc::space(),
                stack.pretty(f),
            ]),
            | Stack::Tag(Cons(dtor, stack)) => {
                let statics_fmt = zydeco_statics::tyck::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::concat([
                    RcDoc::text("tag("),
                    RcDoc::text(dtor.ugly(&statics_fmt)),
                    RcDoc::text(")"),
                    RcDoc::space(),
                    RcDoc::text("::"),
                    RcDoc::space(),
                    stack.pretty(f),
                ])
            }
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Bullet {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("•")
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Kont {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::concat([
            RcDoc::text("kont"),
            RcDoc::space(),
            self.binder.pretty(f),
            RcDoc::space(),
            RcDoc::text("->"),
            RcDoc::concat([RcDoc::line(), self.body.pretty(f)]).nest(2).group(),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CompuId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let compu = &f.arena.compus[self];
        compu.pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Computation {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Computation::Hole(Hole) => RcDoc::text("_"),
            | Computation::Fix(Fix(param, body)) => RcDoc::concat([
                RcDoc::text("fix"),
                RcDoc::space(),
                param.pretty(f),
                RcDoc::space(),
                RcDoc::text("->"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(2).group(),
            ]),
            | Computation::Force(SForce { thunk, stack }) => RcDoc::concat([
                thunk.pretty(f),
                RcDoc::space(),
                RcDoc::text("!"),
                RcDoc::space(),
                stack.pretty(f),
            ]),
            | Computation::Ret(SReturn { stack, value }) => RcDoc::concat([
                stack.pretty(f),
                RcDoc::space(),
                RcDoc::text("@"),
                RcDoc::space(),
                value.pretty(f),
            ]),
            | Computation::Case(Match { scrut, arms }) => RcDoc::concat([
                RcDoc::text("case"),
                RcDoc::space(),
                scrut.pretty(f),
                RcDoc::concat(arms.iter().map(|Matcher { binder, tail }| {
                    RcDoc::concat([
                        RcDoc::line(),
                        RcDoc::text("|"),
                        RcDoc::space(),
                        binder.pretty(f),
                        RcDoc::space(),
                        RcDoc::text("->"),
                        RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(2),
                    ])
                })),
                RcDoc::line(),
                RcDoc::text("end"),
            ]),
            | Computation::LetValue(Let { binder, bindee, tail }) => RcDoc::concat([
                RcDoc::text("let"),
                RcDoc::space(),
                binder.pretty(f),
                RcDoc::space(),
                RcDoc::text("="),
                RcDoc::concat([
                    RcDoc::concat([RcDoc::line(), bindee.pretty(f)]).nest(2),
                    RcDoc::line(),
                    RcDoc::text("in"),
                ])
                .group(),
                RcDoc::concat([RcDoc::line(), tail.pretty(f)]).group(),
            ]),
            | Computation::LetStack(Let { binder, bindee, tail }) => RcDoc::concat([
                RcDoc::text("let"),
                RcDoc::space(),
                binder.pretty(f),
                RcDoc::space(),
                RcDoc::text("="),
                RcDoc::concat([
                    RcDoc::concat([RcDoc::line(), bindee.pretty(f)]).nest(2),
                    RcDoc::line(),
                    RcDoc::text("in"),
                ])
                .group(),
                RcDoc::concat([RcDoc::line(), tail.pretty(f)]).group(),
            ]),
            | Computation::LetArg(Let { binder, bindee, tail }) => {
                let Cons(param, Bullet) = binder;
                RcDoc::concat([
                    RcDoc::text("let"),
                    RcDoc::space(),
                    RcDoc::text("arg("),
                    param.pretty(f),
                    RcDoc::text(")"),
                    RcDoc::space(),
                    RcDoc::text("::"),
                    RcDoc::space(),
                    Bullet.pretty(f),
                    RcDoc::space(),
                    RcDoc::text("="),
                    RcDoc::concat([
                        RcDoc::concat([RcDoc::line(), bindee.pretty(f)]).nest(2),
                        RcDoc::line(),
                        RcDoc::text("in"),
                    ])
                    .group(),
                    RcDoc::concat([RcDoc::line(), tail.pretty(f)]).group(),
                ])
            }
            | Computation::CoCase(CoMatch { arms }) => {
                let statics_fmt = zydeco_statics::tyck::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::concat([
                    RcDoc::text("cocase"),
                    RcDoc::concat(arms.iter().map(|CoMatcher { dtor, tail }| {
                        let Cons(dtor_name, Bullet) = dtor;
                        RcDoc::concat([
                            RcDoc::line(),
                            RcDoc::text("|"),
                            RcDoc::space(),
                            RcDoc::text("tag("),
                            RcDoc::text(dtor_name.ugly(&statics_fmt)),
                            RcDoc::text(")"),
                            RcDoc::space(),
                            RcDoc::text("->"),
                            RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(2),
                        ])
                    })),
                    RcDoc::line(),
                    RcDoc::text("end"),
                ])
            }
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for TermId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | TermId::Value(v) => v.pretty(f),
            | TermId::Compu(c) => c.pretty(f),
            | TermId::Stack(s) => s.pretty(f),
        }
    }
}
