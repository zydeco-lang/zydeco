//! Pretty-printing for first-order SPS.

use super::{
    check::SpsLowProgram,
    entry::{EntryKind, EntryProtocol},
    syntax::*,
    traverse::{Edge, Node, Occurrence, Traversal, Visitor},
};
pub use crate::arena::NameStyle;
use crate::arena::NameTable;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::scoped::syntax::ScopedArena;

pub use zydeco_syntax::Pretty;

/// The scope a local name is unique in: a block body, or `None` for the root and labels.
type Scope = Option<ValueId>;

pub struct Formatter<'arena> {
    admin: &'arena SpsLowAdminArena,
    inner: &'arena SpsLowInnerArena,
    scoped: &'arena ScopedArena,
    statics: &'arena StaticsArena,
    /// Readable spellings; empty for [`NameStyle::Identified`], where every name carries its id.
    names: NameTable<Scope>,
    pub indent: isize,
}

impl<'arena> Formatter<'arena> {
    /// A formatter spelling every definition with its arena id.
    pub fn new(
        admin: &'arena SpsLowAdminArena, inner: &'arena SpsLowInnerArena,
        scoped: &'arena ScopedArena, statics: &'arena StaticsArena,
    ) -> Self {
        Self { admin, inner, scoped, statics, names: NameTable::default(), indent: 2 }
    }

    /// Spell definitions in `style`. Readable spellings come from one traversal of `program`:
    /// labels and the root's locals share a scope, and each block body is a scope of its
    /// own, which suffices because blocks are closed.
    pub fn with_name_style(mut self, program: &SpsLowProgram, style: NameStyle) -> Self {
        let names = match style {
            | NameStyle::Identified => NameTable::default(),
            | NameStyle::Readable => {
                let mut namer = Namer {
                    admin: self.admin,
                    scoped: self.scoped,
                    statics: self.statics,
                    blocks: Vec::new(),
                    names: NameTable::default(),
                };
                Traversal { arena: self.inner }.run(program.root().into(), &mut namer);
                namer.names
            }
        };
        self.names = names;
        self
    }
}

/// Spell every binding occurrence in first-visit order: labels in the global scope, and
/// variable patterns in the scope of the innermost block whose body contains them.
struct Namer<'a> {
    admin: &'a SpsLowAdminArena,
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
    blocks: Vec<ValueId>,
    names: NameTable<Scope>,
}

impl Namer<'_> {
    fn assign(&mut self, scope: Scope, def: DefId) {
        let plain = self.admin.def_name(self.scoped, self.statics, &def).plain();
        self.names.assign(scope, def, &plain);
    }
}

impl Visitor for Namer<'_> {
    fn enter(&mut self, node: Node<'_>, _edge: Edge, occurrence: Occurrence) {
        if occurrence != Occurrence::First {
            return;
        }
        match node {
            | Node::Value(id, Value::Block(Block { label, .. })) => {
                self.assign(None, *label);
                self.blocks.push(id);
            }
            | Node::Pattern(_, ValuePattern::Var(def)) => {
                let scope = self.blocks.last().copied();
                self.assign(scope, *def);
            }
            | _ => {}
        }
    }

    fn exit(&mut self, node: Node<'_>) {
        if let Node::Value(id, Value::Block(_)) = node
            && self.blocks.last() == Some(&id)
        {
            self.blocks.pop();
        }
    }
}

use pretty::RcDoc;

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match f.names.get(self) {
            | Some(spelling) => RcDoc::text(spelling.to_owned()),
            | None => {
                let name = f.admin.def_name(f.scoped, f.statics, self);
                RcDoc::text(format!("{}{}", name.plain(), self.concise()))
            }
        }
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
        match &f.inner.values[self] {
            // Blocks are listed once at the top level; an occurrence names the label.
            | Value::Block(Block { label, .. }) => label.pretty(f),
            | value => {
                let value = value.pretty(f);
                match f.inner.entry_protocols.get(self) {
                    | Some(protocol) => RcDoc::text(format!("{protocol} ")).append(value),
                    | None => value,
                }
            }
        }
    }
}

/// Collect every block in first-visit order, so the listing follows the root.
struct Blocks(Vec<ValueId>);

impl Visitor for Blocks {
    fn enter(&mut self, node: Node<'_>, _edge: Edge, occurrence: Occurrence) {
        if let (Occurrence::First, Node::Value(id, Value::Block(_))) = (occurrence, node) {
            self.0.push(id);
        }
    }
}

/// `[block:label]` with a provenance comment, then the entry words popped as ordinary
/// arguments, then the body.
///
/// The comment records which record kind the block's code pointer lives in and the protocol
/// known for it: the residual stack after the entry pops for a closure, or the accepted result
/// for a continuation. The administrative words themselves carry no type at this level.
fn block_definition<'a>(id: ValueId, f: &'a Formatter) -> RcDoc<'a> {
    let Value::Block(Block { label, entry, body }) = &f.inner.values[&id] else {
        unreachable!("only blocks are listed")
    };
    let kind = match entry.kind() {
        | EntryKind::Closure => "closure",
        | EntryKind::Continuation => "continuation",
    };
    let protocol = match f.inner.entry_protocols.get(&id) {
        | Some(EntryProtocol::Closure(stack)) => {
            RcDoc::text(format!("  -- {kind} entry; • : {stack}"))
        }
        | Some(EntryProtocol::Continuation(result)) => {
            RcDoc::text(format!("  -- {kind} entry; result : {result}"))
        }
        | None => RcDoc::text(format!("  -- {kind} entry")),
    };
    let pops = RcDoc::concat(entry.words().map(|(_, pattern)| {
        RcDoc::concat([
            RcDoc::line(),
            RcDoc::text("let arg("),
            pattern.pretty(f),
            RcDoc::text(") :: • = • in"),
        ])
    }));
    RcDoc::concat([
        RcDoc::text("[block:"),
        label.pretty(f),
        RcDoc::text("]"),
        protocol,
        RcDoc::concat([pops, RcDoc::line(), body.pretty(f)]).nest(f.indent),
    ])
}

impl<'a> Pretty<'a, Formatter<'a>> for Value {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        match self {
            | Value::Hole(Hole) => RcDoc::text("_"),
            | Value::Var(def) => def.pretty(f),
            | Value::Block(Block { label, .. }) => label.pretty(f),
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
            | Computation::Jump(Jump { target, argument, stack }) => RcDoc::concat([
                RcDoc::text("jump "),
                target.pretty(f),
                RcDoc::text(" ! arg("),
                argument.word().1.pretty(f),
                RcDoc::text(") :: "),
                stack.pretty(f),
            ]),
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => RcDoc::concat([
                RcDoc::text("match-product "),
                scrut.pretty(f),
                RcDoc::text(" as "),
                binder.pretty(f),
                RcDoc::text(" in"),
                RcDoc::line(),
                body.pretty(f),
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
            | Computation::LetValue(LetValue { binder, bindee, tail: body }) => RcDoc::concat([
                RcDoc::text("let "),
                binder.pretty(f),
                RcDoc::text(" = "),
                bindee.pretty(f),
                RcDoc::text(" in"),
                RcDoc::line(),
                body.pretty(f),
            ]),
            | Computation::LetStack(LetStack { binder: Bullet, bindee, tail: body }) => {
                RcDoc::concat([
                    RcDoc::text("let • = "),
                    bindee.pretty(f),
                    RcDoc::text(" in"),
                    RcDoc::line(),
                    body.pretty(f),
                ])
            }
            | Computation::LetArg(LetArg { binder: Cons(binder, Bullet), bindee, tail: body }) => {
                RcDoc::concat([
                    RcDoc::text("let arg("),
                    binder.pretty(f),
                    RcDoc::text(") :: • = "),
                    bindee.pretty(f),
                    RcDoc::text(" in"),
                    RcDoc::line(),
                    body.pretty(f),
                ])
            }
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
                    RcDoc::line(),
                    body.pretty(f),
                ])
            }
            | Computation::OpenContinuation(OpenContinuation { package, code, body }) => {
                RcDoc::concat([
                    RcDoc::text("open-continuation "),
                    package.pretty(f),
                    RcDoc::text(" as "),
                    code.pretty(f),
                    RcDoc::text(" :: • in"),
                    RcDoc::line(),
                    body.pretty(f),
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
        let mut blocks = Blocks(Vec::new());
        Traversal { arena: f.inner }.run(self.root().into(), &mut blocks);
        RcDoc::concat(f.inner.protocols.parameters().map(|(id, kind)| {
            RcDoc::text(format!("[parameter:{id}] {kind}")).append(RcDoc::line())
        }))
        .append(RcDoc::concat(f.inner.protocols.iter().map(|(id, definition)| {
            RcDoc::text(format!("[protocol:{id}] {definition}")).append(RcDoc::line())
        })))
        .append(RcDoc::text("[root]"))
        .append(RcDoc::concat([RcDoc::line(), self.root().pretty(f)]).nest(f.indent))
        .append(RcDoc::line())
        .append(RcDoc::concat(
            blocks
                .0
                .into_iter()
                .map(|id| RcDoc::concat([RcDoc::line(), block_definition(id, f), RcDoc::line()])),
        ))
    }
}
