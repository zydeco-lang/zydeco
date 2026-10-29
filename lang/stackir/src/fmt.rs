//! Printing shared by the stack IR phases.
//!
//! The shared forms print once; each phase prints its identifiers, its own forms, and its program.

use crate::{arena::DefinitionNames, syntax::*};
use pretty::RcDoc;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::scoped::syntax::ScopedArena;

pub use zydeco_syntax::Pretty;

/// Formatter for one phase, naming definitions through scoped and statics.
pub struct Formatter<'arena, P: Phase> {
    pub(crate) admin: &'arena dyn DefinitionNames,
    pub(crate) inner: &'arena P::Nodes,
    pub(crate) scoped: &'arena ScopedArena,
    pub(crate) statics: &'arena StaticsArena,
    pub indent: isize,
}

impl<'arena, P: Phase> Formatter<'arena, P> {
    pub fn new(
        admin: &'arena dyn DefinitionNames, inner: &'arena P::Nodes, scoped: &'arena ScopedArena,
        statics: &'arena StaticsArena,
    ) -> Self {
        Self { admin, inner, scoped, statics, indent: 2 }
    }

    pub(crate) fn statics_formatter(&self) -> zydeco_statics::fmt::Formatter<'arena> {
        zydeco_statics::fmt::Formatter::new(self.scoped, self.statics)
    }
}

impl<'a, P: Phase> Pretty<'a, Formatter<'a, P>> for StackVar {
    fn pretty(&self, _f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        RcDoc::text("•")
    }
}

impl<'a, P: Phase> Pretty<'a, Formatter<'a, P>> for ValuePattern<P::VPatId>
where
    P::VPatId: Pretty<'a, Formatter<'a, P>>,
    DefId: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        match self {
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

impl<'a, P: Phase> Pretty<'a, Formatter<'a, P>> for Value<P>
where
    P::ValueId: Pretty<'a, Formatter<'a, P>>,
    P::ValueExt: Pretty<'a, Formatter<'a, P>>,
    DefId: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        match self {
            | Value::Hole(Hole) => RcDoc::text("_"),
            | Value::Var(def) => def.pretty(f),
            | Value::Ctor(Ctor(ctor, value)) => RcDoc::concat([
                RcDoc::text(ctor.name.ugly(&f.statics_formatter())),
                RcDoc::text("("),
                value.pretty(f),
                RcDoc::text(")"),
            ]),
            | Value::Triv(Triv) => RcDoc::text("()"),
            | Value::VCons(VCons { items, layout: _ }) => RcDoc::concat([
                RcDoc::text("("),
                RcDoc::intersperse(items.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
                RcDoc::text(")"),
            ]),
            | Value::Literal(literal) => RcDoc::text(literal.ugly(&f.statics_formatter())),
            | Value::AddrOffset(AddrOffset { base, displacement }) => RcDoc::concat([
                RcDoc::text("addr.offset("),
                base.pretty(f),
                RcDoc::text(", "),
                displacement.pretty(f),
                RcDoc::text(")"),
            ]),
            | Value::Primitive(Primitive { operation, operands }) => RcDoc::concat([
                RcDoc::text(format!("<primitive:{operation}>(")),
                RcDoc::intersperse(
                    operands.iter().map(|operand| operand.pretty(f)),
                    RcDoc::text(", "),
                ),
                RcDoc::text(")"),
            ]),
            | Value::Ext(form) => form.pretty(f),
        }
    }
}

impl<'a, P: Phase> Pretty<'a, Formatter<'a, P>> for Stack<P>
where
    P::ValueId: Pretty<'a, Formatter<'a, P>>,
    P::StackId: Pretty<'a, Formatter<'a, P>>,
    P::StackExt: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        match self {
            | Stack::Var(StackVar) => RcDoc::text("•"),
            | Stack::Arg(Cons(value, stack)) => RcDoc::concat([
                RcDoc::text("arg("),
                value.pretty(f),
                RcDoc::text(") :: "),
                stack.pretty(f),
            ]),
            | Stack::Tag(Cons(dtor, stack)) => RcDoc::concat([
                RcDoc::text("tag("),
                RcDoc::text(dtor.name.ugly(&f.statics_formatter())),
                RcDoc::text(") :: "),
                stack.pretty(f),
            ]),
            | Stack::Ext(form) => form.pretty(f),
        }
    }
}

impl<'a, P: Phase> Pretty<'a, Formatter<'a, P>> for Computation<P>
where
    P::VPatId: Pretty<'a, Formatter<'a, P>>,
    P::ValueId: Pretty<'a, Formatter<'a, P>>,
    P::StackId: Pretty<'a, Formatter<'a, P>>,
    P::CompuId: Pretty<'a, Formatter<'a, P>>,
    P::CompuExt: Pretty<'a, Formatter<'a, P>>,
{
    // One arm per computation form is the clearest shape for this printer.
    #[allow(clippy::too_many_lines)]
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        match self {
            | Computation::Hole(SHole(stack)) => {
                RcDoc::concat([RcDoc::text("_ ! "), stack.pretty(f)])
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => RcDoc::concat([
                RcDoc::text("match-product "),
                scrut.pretty(f),
                RcDoc::text(" as "),
                binder.pretty(f),
                RcDoc::text(" in"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Computation::Compare(CompareBranch {
                operation,
                operands,
                when_true,
                when_false,
            }) => RcDoc::concat([
                RcDoc::text(format!("compare {operation}(")),
                operands[0].pretty(f),
                RcDoc::text(", "),
                operands[1].pretty(f),
                RcDoc::text(")"),
                RcDoc::line(),
                RcDoc::text("| true ->"),
                RcDoc::concat([RcDoc::line(), when_true.pretty(f)]).nest(f.indent),
                RcDoc::line(),
                RcDoc::text("| false ->"),
                RcDoc::concat([RcDoc::line(), when_false.pretty(f)]).nest(f.indent),
                RcDoc::line(),
                RcDoc::text("end"),
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
            | Computation::LetValue(Let { binder, bindee, tail: body }) => RcDoc::concat([
                RcDoc::text("let "),
                binder.pretty(f),
                RcDoc::text(" = "),
                bindee.pretty(f),
                RcDoc::text(" in"),
                RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
            ]),
            | Computation::LetStack(Let { binder: StackVar, bindee, tail: body }) => {
                RcDoc::concat([
                    RcDoc::text("let • = "),
                    bindee.pretty(f),
                    RcDoc::text(" in"),
                    RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
                ])
            }
            | Computation::LetArg(Let { binder: Cons(binder, StackVar), bindee, tail: body }) => {
                RcDoc::concat([
                    RcDoc::text("let arg("),
                    binder.pretty(f),
                    RcDoc::text(") :: • = "),
                    bindee.pretty(f),
                    RcDoc::text(" in"),
                    RcDoc::concat([RcDoc::line(), body.pretty(f)]).nest(f.indent).group(),
                ])
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                let statics_fmt = f.statics_formatter();
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
            | Computation::Memory(MemoryStep::Load { scalar, address, result, next }) => {
                RcDoc::concat([
                    RcDoc::text(format!("load {scalar:?} ")),
                    address.pretty(f),
                    RcDoc::text(" as "),
                    result.pretty(f),
                    RcDoc::text(";"),
                    RcDoc::line(),
                    next.pretty(f),
                ])
            }
            | Computation::Memory(MemoryStep::Store { scalar, address, value, next }) => {
                RcDoc::concat([
                    RcDoc::text(format!("store {scalar:?} ")),
                    address.pretty(f),
                    RcDoc::text(", "),
                    value.pretty(f),
                    RcDoc::text(";"),
                    RcDoc::line(),
                    next.pretty(f),
                ])
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                let (name, arity) = match function {
                    | ExternalFunction::Host(function) => (function.host_name(), function.arity()),
                    | ExternalFunction::Foreign(import) => {
                        (import.target.symbol.to_string(), import.signature.parameters().len())
                    }
                    | ExternalFunction::Unit(import) => (import.target.symbol.to_string(), 0),
                };
                RcDoc::concat([RcDoc::text(format!("<extern:{name}/{arity}> ")), stack.pretty(f)])
            }
            | Computation::Ext(form) => form.pretty(f),
        }
    }
}

impl<'a, P: Phase> Pretty<'a, Formatter<'a, P>> for TermId<P>
where
    P::ValueId: Pretty<'a, Formatter<'a, P>>,
    P::StackId: Pretty<'a, Formatter<'a, P>>,
    P::CompuId: Pretty<'a, Formatter<'a, P>>,
{
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        match self {
            | TermId::Value(value) => value.pretty(f),
            | TermId::Compu(compu) => compu.pretty(f),
            | TermId::Stack(stack) => stack.pretty(f),
        }
    }
}
