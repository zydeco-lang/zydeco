//! Formatters for the Zydeco Intermediate Representation (ZIR).

use super::syntax::*;
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_surface::scoped::syntax::ScopedArena;

/* -------------------------------- Formatter ------------------------------- */

pub use zydeco_syntax::Pretty;
/// Formatter for stack IR using scoped and statics naming.
pub struct Formatter<'arena> {
    admin: &'arena AdminArena,
    inner: &'arena StackirInnerArena,
    scoped: &'arena ScopedArena,
    statics: &'arena StaticsArena,
    pub indent: isize,
}
impl<'arena> Formatter<'arena> {
    pub fn new(
        admin: &'arena AdminArena, inner: &'arena StackirInnerArena, scoped: &'arena ScopedArena,
        statics: &'arena StaticsArena,
    ) -> Self {
        Formatter { admin, inner, scoped, statics, indent: 2 }
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
        let vpat = &f.inner.vpats[self];
        use super::syntax::{Cons, Ctor, ValuePattern as VPat};
        match vpat {
            | VPat::Hole(_) => RcDoc::text("_"),
            | VPat::Var(def) => def.pretty(f),
            | VPat::Ctor(Ctor(ctor, tail)) => {
                use zydeco_syntax::CtorName;
                let CtorName(name_str) = &ctor.name;
                RcDoc::concat([
                    RcDoc::text(name_str.clone()),
                    RcDoc::text("("),
                    tail.pretty(f),
                    RcDoc::text(")"),
                ])
            }
            | VPat::Triv(_) => RcDoc::text("()"),
            | VPat::VCons(Cons(a, b)) => RcDoc::concat([
                RcDoc::text("("),
                a.pretty(f),
                RcDoc::text(","),
                RcDoc::space(),
                b.pretty(f),
                RcDoc::text(")"),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        if let Some(value) = f.inner.values.get(self) {
            value.pretty(f)
        } else {
            RcDoc::text(format!("<value:{}>", self.concise()))
        }
        // let value = &f.inner.values[self];
        // value.pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Value {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Value::Hole(Hole) => RcDoc::text("_"),
            | Value::Var(def) => def.pretty(f),
            | Value::Closure(Closure { stack, body }) => RcDoc::concat([
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
            | Value::Ctor(Ctor(ctor, val)) => {
                let statics_fmt = zydeco_statics::tyck::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::concat([
                    RcDoc::text(ctor.name.ugly(&statics_fmt)),
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
            | Value::Literal(lit) => {
                let statics_fmt = zydeco_statics::tyck::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::text(lit.ugly(&statics_fmt))
            }
            | Value::Complex(Complex { operator, operands }) => {
                let op_str = format!("<operator:{}>", operator);
                let ops_doc = RcDoc::concat(
                    operands
                        .iter()
                        .map(|op| op.pretty(f))
                        .enumerate()
                        .flat_map(
                            |(i, d)| {
                                if i == 0 { vec![d] } else { vec![RcDoc::text(", "), d] }
                            },
                        )
                        .collect::<Vec<_>>(),
                );
                RcDoc::concat([RcDoc::text(op_str), RcDoc::text("("), ops_doc, RcDoc::text(")")])
            }
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for StackId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let stack = &f.inner.stacks[self];
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
                    RcDoc::text(dtor.name.ugly(&statics_fmt)),
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
            RcDoc::concat([RcDoc::line(), self.body.pretty(f)]).nest(f.indent).group(),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CompuId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        if let Some(compu) = f.inner.compus.get(self) {
            compu.pretty(f)
        } else {
            RcDoc::text(format!("<compu:{}>", self.concise()))
        }
        // let compu = &f.inner.compus[self];
        // compu.pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Computation<LetJoin> {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Computation::Hole(Hole) => RcDoc::text("_"),
            | Computation::Fix(SFix { param, body }) => RcDoc::concat([
                RcDoc::text("fix"),
                RcDoc::space(),
                param.pretty(f),
                RcDoc::space(),
                RcDoc::text("->"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
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
                        RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(f.indent),
                    ])
                })),
                RcDoc::line(),
                RcDoc::text("end"),
            ]),
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => RcDoc::concat([
                RcDoc::text("let"),
                RcDoc::space(),
                binder.pretty(f),
                RcDoc::space(),
                RcDoc::text("="),
                RcDoc::concat([
                    RcDoc::concat([RcDoc::line(), bindee.pretty(f)]).nest(f.indent),
                    RcDoc::line(),
                    RcDoc::text("in"),
                ])
                .group(),
                RcDoc::concat([RcDoc::line(), tail.pretty(f)]).group(),
            ]),
            | Computation::Join(LetJoin::Stack(Let { binder, bindee, tail })) => RcDoc::concat([
                RcDoc::text("let"),
                RcDoc::space(),
                binder.pretty(f),
                RcDoc::space(),
                RcDoc::text("="),
                RcDoc::concat([
                    RcDoc::concat([RcDoc::line(), bindee.pretty(f)]).nest(f.indent),
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
                        RcDoc::concat([RcDoc::line(), bindee.pretty(f)]).nest(f.indent),
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
                        let Cons(dtor, Bullet) = dtor;
                        RcDoc::concat([
                            RcDoc::line(),
                            RcDoc::text("|"),
                            RcDoc::space(),
                            RcDoc::text("tag("),
                            RcDoc::text(dtor.name.ugly(&statics_fmt)),
                            RcDoc::text(")"),
                            RcDoc::space(),
                            RcDoc::text("->"),
                            RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(f.indent),
                        ])
                    })),
                    RcDoc::line(),
                    RcDoc::text("end"),
                ])
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                let arity = f.admin.builtins[function].arity;
                let fun_str = format!("<extern:{}/{}>", function, arity);
                RcDoc::concat([RcDoc::text(fun_str), RcDoc::space(), stack.pretty(f)])
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

impl<'a> Pretty<'a, Formatter<'a>> for StackirArena {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let mut doc = RcDoc::nil();
        let builtins = &f.admin.builtins;

        // Print all builtins
        for (name, builtin) in
            builtins.iter().filter(|(_, builtin)| builtin.sort == BuiltinSort::Operator)
        {
            doc = doc.append(RcDoc::text(format!("[operator:{}] {}", name, builtin)));
            doc = doc.append(RcDoc::line());
        }
        for (name, builtin) in
            builtins.iter().filter(|(_, builtin)| builtin.sort == BuiltinSort::Function)
        {
            doc = doc.append(RcDoc::text(format!("[function:{}] {}", name, builtin)));
            doc = doc.append(RcDoc::line());
        }

        // Print all entries (each entry compu is let g1 = v1 in ... in body)
        for (compu_id, _) in self.inner.entry.iter() {
            doc = doc
                .append(RcDoc::text("[entry]"))
                .append(RcDoc::concat([RcDoc::line(), compu_id.pretty(f)]).nest(f.indent))
                .append(RcDoc::line());
        }

        doc
    }
}
