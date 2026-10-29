//! Bound- and free-variable summaries shared by the stack IR phases.

use crate::{
    syntax::*,
    traverse::{Edge, EntityId, Node, Occurrence, Traversal, Visitor},
};
use rustc_hash::FxHashMap;

/// Bottom-up facts for one immutable arena. A cycle invalidates all exposed summaries.
pub struct Variables<P: Phase> {
    patterns: FxHashMap<P::VPatId, Context<DefId>>,
    values: FxHashMap<P::ValueId, CoContext<DefId>>,
    stacks: FxHashMap<P::StackId, CoContext<DefId>>,
    computations: FxHashMap<P::CompuId, CoContext<DefId>>,
    cyclic: bool,
}

/// The free variables of a phase's own forms, from the summaries of their children.
pub trait FreeVariables<P: Phase> {
    fn free_variables(&self, facts: &Variables<P>) -> CoContext<DefId>;
}

impl<P: Phase> Default for Variables<P> {
    fn default() -> Self {
        Self {
            patterns: FxHashMap::default(),
            values: FxHashMap::default(),
            stacks: FxHashMap::default(),
            computations: FxHashMap::default(),
            cyclic: false,
        }
    }
}

impl<P: Phase> Variables<P> {
    pub fn free_variables(&self, term: TermId<P>) -> Option<&CoContext<DefId>> {
        if self.cyclic {
            return None;
        }
        match term {
            | TermId::Value(id) => self.values.get(&id),
            | TermId::Stack(id) => self.stacks.get(&id),
            | TermId::Compu(id) => self.computations.get(&id),
        }
    }

    pub fn bound_variables(&self, pattern: P::VPatId) -> Option<&Context<DefId>> {
        if self.cyclic { None } else { self.patterns.get(&pattern) }
    }

    /// Analyze every node under `root`, which must be acyclic.
    pub fn analyze(nodes: &P::Nodes, root: EntityId<P>) -> Self {
        let mut analysis = Self::default();
        Traversal { arena: nodes }.run(root, &mut analysis);
        assert!(!analysis.cyclic, "variable analysis requires acyclic stack IR syntax");
        analysis
    }

    /// The bound variables of a completed pattern child.
    pub fn pattern_summary(&self, pattern: P::VPatId) -> Context<DefId> {
        self.patterns[&pattern].clone()
    }

    /// The free variables of a completed value child.
    pub fn value_summary(&self, value: P::ValueId) -> CoContext<DefId> {
        self.values[&value].clone()
    }

    /// The free variables of a completed stack child.
    pub fn stack_summary(&self, stack: P::StackId) -> CoContext<DefId> {
        self.stacks[&stack].clone()
    }

    /// The free variables of a completed computation child.
    pub fn compu_summary(&self, computation: P::CompuId) -> CoContext<DefId> {
        self.computations[&computation].clone()
    }

    fn pattern(&self, node: &ValuePattern<P::VPatId>) -> Context<DefId> {
        match node {
            | ValuePattern::Hole(Hole) | ValuePattern::Triv(Triv) => Context::new(),
            | ValuePattern::Var(def) => Context::singleton(*def),
            | ValuePattern::Ctor(Ctor(_, body)) => self.pattern_summary(*body),
            | ValuePattern::Alias(Alias(patterns)) => patterns
                .iter()
                .map(|pattern| self.pattern_summary(*pattern))
                .fold(Context::new(), |vars, pattern| vars + pattern),
            | ValuePattern::VCons(VCons { items, layout: _ }) => items
                .iter()
                .map(|item| self.pattern_summary(*item))
                .fold(Context::new(), |vars, item| vars + item),
        }
    }

    fn value(&self, node: &Value<P>) -> CoContext<DefId> {
        match node {
            | Value::Var(def) => CoContext::singleton(*def),
            | Value::Ctor(Ctor(_, body)) => self.value_summary(*body),
            | Value::VCons(VCons { items, layout: _ }) => items
                .iter()
                .map(|item| self.value_summary(*item))
                .fold(CoContext::new(), |vars, item| vars + item),
            | Value::Primitive(Primitive { operation: _, operands }) => operands
                .iter()
                .map(|operand| self.value_summary(*operand))
                .fold(CoContext::new(), |vars, operand| vars + operand),
            | Value::Hole(Hole) | Value::Triv(Triv) | Value::Literal(_) => CoContext::new(),
            | Value::AddrOffset(AddrOffset { base, displacement }) => {
                self.value_summary(*base) + self.value_summary(*displacement)
            }
            | Value::Ext(form) => form.free_variables(self),
        }
    }

    fn stack(&self, node: &Stack<P>) -> CoContext<DefId> {
        match node {
            | Stack::Var(StackVar) => CoContext::new(),
            | Stack::Arg(Cons(value, stack)) => {
                self.value_summary(*value) + self.stack_summary(*stack)
            }
            | Stack::Tag(Cons(_, stack)) => self.stack_summary(*stack),
            | Stack::Ext(form) => form.free_variables(self),
        }
    }

    fn computation(&self, node: &Computation<P>) -> CoContext<DefId> {
        match node {
            | Computation::Hole(SHole(stack)) => self.stack_summary(*stack),
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                self.value_summary(*scrut)
                    + (self.compu_summary(*body) - self.pattern_summary(*binder))
            }
            | Computation::Compare(CompareBranch { operands, when_true, when_false, .. }) => {
                self.value_summary(operands[0])
                    + self.value_summary(operands[1])
                    + self.compu_summary(*when_true)
                    + self.compu_summary(*when_false)
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                self.value_summary(*scrut)
                    + arms
                        .iter()
                        .map(|Matcher { binder, tail }| {
                            self.compu_summary(*tail) - self.pattern_summary(*binder)
                        })
                        .fold(CoContext::new(), |vars, arm| vars + arm)
            }
            | Computation::LetValue(Let { binder, bindee, tail }) => {
                self.value_summary(*bindee)
                    + (self.compu_summary(*tail) - self.pattern_summary(*binder))
            }
            | Computation::LetStack(Let { binder: StackVar, bindee, tail }) => {
                self.stack_summary(*bindee) + self.compu_summary(*tail)
            }
            | Computation::LetArg(Let { binder: Cons(binder, StackVar), bindee, tail }) => {
                self.stack_summary(*bindee)
                    + (self.compu_summary(*tail) - self.pattern_summary(*binder))
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                self.stack_summary(*scrut)
                    + arms
                        .iter()
                        .map(|CoMatcher { dtor: _, tail }| self.compu_summary(*tail))
                        .fold(CoContext::new(), |vars, arm| vars + arm)
            }
            | Computation::ExternCall(ExternCall { function: _, stack }) => {
                self.stack_summary(*stack)
            }
            | Computation::Memory(MemoryStep::Load { address, result, next, .. }) => {
                self.value_summary(*address)
                    + (self.compu_summary(*next) - self.pattern_summary(*result))
            }
            | Computation::Memory(MemoryStep::Store { address, value, next, .. }) => {
                self.value_summary(*address)
                    + self.value_summary(*value)
                    + self.compu_summary(*next)
            }
            | Computation::Ext(form) => form.free_variables(self),
        }
    }
}

impl<P: Phase> Visitor<P> for Variables<P> {
    fn enter(&mut self, _node: Node<'_, P>, _edge: Edge<P>, occurrence: Occurrence) {
        self.cyclic |= occurrence == Occurrence::Cyclic;
    }

    fn exit(&mut self, node: Node<'_, P>) {
        if self.cyclic {
            return;
        }
        match node {
            | Node::Pattern(id, node) => {
                self.patterns.insert(id, self.pattern(node));
            }
            | Node::Value(id, node) => {
                self.values.insert(id, self.value(node));
            }
            | Node::Stack(id, node) => {
                self.stacks.insert(id, self.stack(node));
            }
            | Node::Computation(id, node) => {
                self.computations.insert(id, self.computation(node));
            }
        }
    }
}

/// The bound variables of one pattern, from a fresh analysis of its arena.
pub trait Vars<P: Phase> {
    fn vars(self, arena: &impl AsRef<P::Nodes>) -> Context<DefId>;
}

/// The free variables of one term, from a fresh analysis of its arena.
pub trait FreeVars<P: Phase> {
    fn free_vars(self, arena: &impl AsRef<P::Nodes>) -> CoContext<DefId>;
}
