//! Bound- and free-variable summaries for first-order SPS.

use super::syntax::*;
use super::traverse::{Edge, EntityId, Node, Occurrence, Traversal, Visitor};
use rustc_hash::FxHashMap;
use zydeco_utils::context::{CoContext, Context};

/// Bottom-up facts for one immutable arena. A cycle invalidates all exposed summaries.
#[derive(Default)]
pub struct Variables {
    patterns: FxHashMap<VPatId, Context<DefId>>,
    values: FxHashMap<ValueId, CoContext<DefId>>,
    stacks: FxHashMap<StackId, CoContext<DefId>>,
    computations: FxHashMap<CompuId, CoContext<DefId>>,
    cyclic: bool,
}

impl Variables {
    pub fn free_variables(&self, term: TermId) -> Option<&CoContext<DefId>> {
        if self.cyclic {
            return None;
        }
        match term {
            | TermId::Value(id) => self.values.get(&id),
            | TermId::Stack(id) => self.stacks.get(&id),
            | TermId::Compu(id) => self.computations.get(&id),
        }
    }

    pub fn bound_variables(&self, pattern: VPatId) -> Option<&Context<DefId>> {
        if self.cyclic { None } else { self.patterns.get(&pattern) }
    }

    fn analyze(arena: &SpsLowInnerArena, root: EntityId) -> Self {
        let mut analysis = Self::default();
        Traversal { arena }.run(root, &mut analysis);
        assert!(!analysis.cyclic, "variable analysis requires acyclic SPSLow syntax");
        analysis
    }

    fn pattern(&self, node: ValuePattern) -> Context<DefId> {
        match node {
            | ValuePattern::Hole(Hole) | ValuePattern::Triv(Triv) => Context::new(),
            | ValuePattern::Var(def) => Context::singleton(def),
            | ValuePattern::Ctor(Ctor(_, body)) => body.summary(self),
            | ValuePattern::Alias(Alias(patterns)) => patterns
                .into_iter()
                .map(|pattern| pattern.summary(self))
                .fold(Context::new(), |vars, pattern| vars + pattern),
            | ValuePattern::VCons(VCons { items, layout: _ }) => items
                .into_iter()
                .map(|item| item.summary(self))
                .fold(Context::new(), |vars, item| vars + item),
        }
    }
    fn value(&self, node: Value) -> CoContext<DefId> {
        match node {
            | Value::Var(def) => CoContext::singleton(def),
            | Value::Block(Block { label, entry, body }) => entry
                .words()
                .fold(body.summary(self) - Context::singleton(label), |free, (_, pattern)| {
                    free - pattern.summary(self)
                }),
            | Value::ClosurePackage(ClosurePackage { environment, code }) => {
                environment.summary(self) + code.summary(self)
            }
            | Value::Ctor(Ctor(_, body)) => body.summary(self),
            | Value::VCons(VCons { items, layout: _ }) => items
                .into_iter()
                .map(|item| item.summary(self))
                .fold(CoContext::new(), |vars, item| vars + item),
            | Value::Primitive(Primitive { operation: _, operands }) => operands
                .into_iter()
                .map(|operand| operand.summary(self))
                .fold(CoContext::new(), |vars, operand| vars + operand),
            | Value::Hole(Hole) | Value::Triv(Triv) | Value::Literal(_) => CoContext::new(),
            | Value::AddrOffset(AddrOffset { base, displacement }) => {
                base.summary(self) + displacement.summary(self)
            }
        }
    }
    fn stack(&self, node: Stack) -> CoContext<DefId> {
        match node {
            | Stack::Var(Bullet) => CoContext::new(),
            | Stack::Arg(Cons(value, stack)) => value.summary(self) + stack.summary(self),
            | Stack::Tag(Cons(_, stack)) => stack.summary(self),
            | Stack::ContinuationPackage(ContinuationPackage { code, residual }) => {
                code.summary(self) + residual.summary(self)
            }
        }
    }
    fn computation(&self, node: Computation) -> CoContext<DefId> {
        match node {
            | Computation::Hole(SHole(stack)) => stack.summary(self),
            | Computation::Jump(Jump { target, argument, stack }) => {
                target.summary(self) + argument.word().1.summary(self) + stack.summary(self)
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                scrut.summary(self) + (body.summary(self) - binder.summary(self))
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                scrut.summary(self)
                    + arms
                        .into_iter()
                        .map(|Matcher { binder, tail }| tail.summary(self) - binder.summary(self))
                        .fold(CoContext::new(), |vars, arm| vars + arm)
            }
            | Computation::LetValue(LetValue { binder, bindee, tail: body }) => {
                bindee.summary(self) + (body.summary(self) - binder.summary(self))
            }
            | Computation::LetStack(LetStack { binder: Bullet, bindee, tail: body }) => {
                bindee.summary(self) + body.summary(self)
            }
            | Computation::LetArg(LetArg { binder: Cons(binder, Bullet), bindee, tail: body }) => {
                bindee.summary(self) + (body.summary(self) - binder.summary(self))
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                scrut.summary(self)
                    + arms
                        .into_iter()
                        .map(|CoMatcher { dtor: _, tail }| tail.summary(self))
                        .fold(CoContext::new(), |vars, arm| vars + arm)
            }
            | Computation::OpenClosure(OpenClosure { package, environment, code, body }) => {
                package.summary(self)
                    + (body.summary(self) - environment.summary(self) - code.summary(self))
            }
            | Computation::OpenContinuation(OpenContinuation { package, code, body }) => {
                package.summary(self) + (body.summary(self) - code.summary(self))
            }
            | Computation::ExternCall(ExternCall { function: _, stack }) => stack.summary(self),
        }
    }
}

impl Visitor for Variables {
    fn enter(&mut self, _node: Node<'_>, _edge: Edge, occurrence: Occurrence) {
        self.cyclic |= occurrence == Occurrence::Cyclic;
    }

    fn exit(&mut self, node: Node<'_>) {
        if self.cyclic {
            return;
        }
        match node {
            | Node::Pattern(id, node) => {
                self.patterns.insert(id, self.pattern(node.clone()));
            }
            | Node::Value(id, node) => {
                self.values.insert(id, self.value(node.clone()));
            }
            | Node::Stack(id, node) => {
                self.stacks.insert(id, self.stack(node.clone()));
            }
            | Node::Computation(id, node) => {
                self.computations.insert(id, self.computation(node.clone()));
            }
        }
    }
}

trait Summary {
    type Output;
    fn summary(self, analysis: &Variables) -> Self::Output;
}

macro_rules! Summary {
    ($id:ty, $field:ident, $output:ty) => {
        impl Summary for $id {
            type Output = $output;
            fn summary(self, analysis: &Variables) -> Self::Output {
                analysis.$field[&self].clone()
            }
        }
    };
}
Summary!(VPatId, patterns, Context<DefId>);
Summary!(ValueId, values, CoContext<DefId>);
Summary!(StackId, stacks, CoContext<DefId>);
Summary!(CompuId, computations, CoContext<DefId>);

pub trait Vars {
    fn vars(self, arena: &SpsLowInnerArena) -> Context<DefId>;
}
impl Vars for VPatId {
    fn vars(self, arena: &SpsLowInnerArena) -> Context<DefId> {
        self.summary(&Variables::analyze(arena, self.into()))
    }
}

pub trait FreeVars {
    fn free_vars(self, arena: &SpsLowInnerArena) -> CoContext<DefId>;
}
macro_rules! FreeVars {
    ($id:ty) => {
        impl FreeVars for $id {
            fn free_vars(self, arena: &SpsLowInnerArena) -> CoContext<DefId> {
                self.summary(&Variables::analyze(arena, self.into()))
            }
        }
    };
}
FreeVars!(ValueId);
FreeVars!(StackId);
FreeVars!(CompuId);
