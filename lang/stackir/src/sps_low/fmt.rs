//! Pretty-printing for first-order SPS.

use super::{check::SpsLowProgram, syntax::*};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::scoped::syntax::ScopedArena;

pub use zydeco_syntax::Pretty;

pub struct Formatter<'arena> {
    admin: &'arena SpsLowAdminArena,
    inner: &'arena SpsLowInnerArena,
    scoped: &'arena ScopedArena,
    statics: &'arena StaticsArena,
    pub indent: isize,
}

impl<'arena> Formatter<'arena> {
    pub fn new(
        admin: &'arena SpsLowAdminArena, inner: &'arena SpsLowInnerArena,
        scoped: &'arena ScopedArena, statics: &'arena StaticsArena,
    ) -> Self {
        Self { admin, inner, scoped, statics, indent: 2 }
    }
}

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text(format!("{}{}", f.scoped.defs[self].plain(), self.concise()))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for VPatId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match &f.inner.vpats[self] {
            | ValuePattern::Hole(_) => RcDoc::text("_"),
            | ValuePattern::Var(def) => def.pretty(f),
            | ValuePattern::Ctor(Ctor(ctor, body)) => RcDoc::concat([
                RcDoc::text(ctor.name.plain().to_string()),
                RcDoc::text("("),
                body.pretty(f),
                RcDoc::text(")"),
            ]),
            | ValuePattern::Alias(Alias(patterns)) => RcDoc::concat([
                RcDoc::text("("),
                RcDoc::intersperse(
                    patterns.iter().map(|pattern| pattern.pretty(f)),
                    RcDoc::text("; "),
                ),
                RcDoc::text(")"),
            ]),
            | ValuePattern::Triv(_) => RcDoc::text("()"),
            | ValuePattern::VCons(VCons { items, layout: _ }) => RcDoc::concat([
                RcDoc::text("("),
                RcDoc::intersperse(items.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
                RcDoc::text(")"),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for ValueId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        f.inner.values[self].pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Value {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Value::Hole(Hole) => RcDoc::text("_"),
            | Value::Var(def) => def.pretty(f),
            | Value::Block(Block { label, body }) => RcDoc::concat([
                RcDoc::text("block"),
                RcDoc::space(),
                RcDoc::text("["),
                label.pretty(f),
                RcDoc::text("]"),
                RcDoc::space(),
                RcDoc::text("•"),
                RcDoc::space(),
                RcDoc::text("->"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Value::ClosurePackage(ClosurePackage { environment, code }) => RcDoc::concat([
                RcDoc::text("pack-closure("),
                environment.pretty(f),
                RcDoc::text(", "),
                code.pretty(f),
                RcDoc::text(")"),
            ]),
            | Value::Ctor(Ctor(ctor, value)) => {
                let statics_fmt = zydeco_statics::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::concat([
                    RcDoc::text(ctor.name.ugly(&statics_fmt)),
                    RcDoc::text("("),
                    value.pretty(f),
                    RcDoc::text(")"),
                ])
            }
            | Value::Triv(Triv) => RcDoc::text("()"),
            | Value::VCons(VCons { items, layout: _ }) => RcDoc::concat([
                RcDoc::text("("),
                RcDoc::intersperse(items.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
                RcDoc::text(")"),
            ]),
            | Value::Literal(literal) => {
                let statics_fmt = zydeco_statics::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::text(literal.ugly(&statics_fmt))
            }
            | Value::Complex(Complex { operator, operands }) => RcDoc::concat([
                RcDoc::text(format!("<operator:{operator}>(")),
                RcDoc::intersperse(
                    operands.iter().map(|operand| operand.pretty(f)),
                    RcDoc::text(", "),
                ),
                RcDoc::text(")"),
            ]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for StackId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        f.inner.stacks[self].pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Stack {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Stack::Var(Bullet) => RcDoc::text("•"),
            | Stack::Arg(Cons(value, stack)) => RcDoc::concat([
                RcDoc::text("arg("),
                value.pretty(f),
                RcDoc::text(") :: "),
                stack.pretty(f),
            ]),
            | Stack::Tag(Cons(dtor, stack)) => {
                let statics_fmt = zydeco_statics::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::concat([
                    RcDoc::text("tag("),
                    RcDoc::text(dtor.name.ugly(&statics_fmt)),
                    RcDoc::text(") :: "),
                    stack.pretty(f),
                ])
            }
            | Stack::ContinuationPackage(ContinuationPackage { code, residual }) => {
                RcDoc::concat([
                    RcDoc::text("pack-continuation("),
                    code.pretty(f),
                    RcDoc::text(", "),
                    residual.pretty(f),
                    RcDoc::text(")"),
                ])
            }
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CompuId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        f.inner.compus[self].pretty(f)
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Computation {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Computation::Hole(SHole(stack)) => {
                RcDoc::concat([RcDoc::text("_ ! "), stack.pretty(f)])
            }
            | Computation::Jump(Jump { target, stack }) => RcDoc::concat([
                RcDoc::text("jump "),
                target.pretty(f),
                RcDoc::text(" ! "),
                stack.pretty(f),
            ]),
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => RcDoc::concat([
                RcDoc::text("match-product "),
                scrut.pretty(f),
                RcDoc::text(" as "),
                binder.pretty(f),
                RcDoc::text(" in"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => RcDoc::concat([
                RcDoc::text("case "),
                scrut.pretty(f),
                RcDoc::concat(arms.iter().map(|Matcher { binder, tail }| {
                    RcDoc::concat([
                        RcDoc::line(),
                        RcDoc::text("| "),
                        binder.pretty(f),
                        RcDoc::text(" ->"),
                        RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(f.indent),
                    ])
                })),
                RcDoc::line(),
                RcDoc::text("end"),
            ]),
            | Computation::LetValue(LetValue { binder, bindee, body }) => RcDoc::concat([
                RcDoc::text("let "),
                binder.pretty(f),
                RcDoc::text(" = "),
                bindee.pretty(f),
                RcDoc::text(" in"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Computation::LetStack(LetStack { bindee, body }) => RcDoc::concat([
                RcDoc::text("let • = "),
                bindee.pretty(f),
                RcDoc::text(" in"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Computation::LetArg(LetArg { binder, bindee, body }) => RcDoc::concat([
                RcDoc::text("let arg("),
                binder.pretty(f),
                RcDoc::text(") :: • = "),
                bindee.pretty(f),
                RcDoc::text(" in"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                let statics_fmt = zydeco_statics::fmt::Formatter::new(f.scoped, f.statics);
                RcDoc::concat([
                    RcDoc::text("cocase "),
                    scrut.pretty(f),
                    RcDoc::concat(arms.iter().map(|CoMatcher { dtor, tail }| {
                        RcDoc::concat([
                            RcDoc::line(),
                            RcDoc::text("| "),
                            RcDoc::text(dtor.0.name.ugly(&statics_fmt)),
                            RcDoc::text(" ->"),
                            RcDoc::concat([RcDoc::line(), tail.pretty(f)]).nest(f.indent),
                        ])
                    })),
                    RcDoc::line(),
                    RcDoc::text("end"),
                ])
            }
            | Computation::OpenClosure(OpenClosure { package, environment, code, body }) => {
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
            | Computation::OpenContinuation(OpenContinuation { package, code, body }) => {
                RcDoc::concat([
                    RcDoc::text("open-continuation "),
                    package.pretty(f),
                    RcDoc::text(" as "),
                    code.pretty(f),
                    RcDoc::text(" :: • in"),
                    RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
                ])
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                let arity = f.admin.builtins[function].arity;
                RcDoc::concat([
                    RcDoc::text(format!("<extern:{function}/{arity}> ")),
                    stack.pretty(f),
                ])
            }
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for TermId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | TermId::Value(value) => value.pretty(f),
            | TermId::Compu(compu) => compu.pretty(f),
            | TermId::Stack(stack) => stack.pretty(f),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for SpsLowProgram {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let builtins = &f.admin.builtins;
        let declarations = builtins
            .iter()
            .map(|(name, builtin)| {
                let sort =
                    if builtin.sort == BuiltinSort::Operator { "operator" } else { "function" };
                RcDoc::text(format!("[{sort}:{name}] {builtin}")).append(RcDoc::line())
            })
            .collect::<Vec<_>>();
        RcDoc::concat(declarations)
            .append(RcDoc::text("[root]"))
            .append(RcDoc::concat([RcDoc::line(), self.root().pretty(f)]).nest(f.indent))
            .append(RcDoc::line())
    }
}
