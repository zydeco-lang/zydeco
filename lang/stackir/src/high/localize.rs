//! Localization turns a value-bound closure into a second-class procedure when every use of its
//! binder calls it outside any closure.
//!
//! The rewrite reads the stack-passing-style paper's localize equation,
//! `let x = λ_c •. N in M = letproc x • = N in M`, from left to right: each call `x ! S` in `M`
//! becomes a procedure call. Closure conversion lowers a procedure to a label that calls reach by
//! direct jumps, so its binding builds no closure package. Normalization runs first in the default
//! plan, so that a closure called once is inlined rather than localized.
//!
//! An analyzer decides the bindings bottom-up, and a second traversal finds the calls of each
//! localized binder within its tail. The rewrite then replaces both kinds of node in place.

use super::{
    check::FragmentProgram,
    syntax::*,
    traverse::{Edge, EntityId, Node, Occurrence, Traversal, Visitor},
    variables::Variables,
};
use crate::protocol::ValueProtocol;
use std::{collections::HashMap, convert::Infallible};
use zydeco_utils::{
    context::{CoContext, Context},
    pass::CompilerPass,
};

/// Localize every eligible closure binding.
pub struct Localizer;

impl CompilerPass<FragmentProgram> for Localizer {
    type Output = FragmentProgram;
    type Error = Infallible;

    fn run(&mut self, program: FragmentProgram) -> Result<Self::Output, Self::Error> {
        let scheme = program.scheme();
        let (mut arena, root) = program.into_program().into_parts();
        let bindings = Escapes::analyze(&arena.inner, root);
        let calls = Calls::collect(&arena.inner, root, &bindings);
        for (binding, procedure) in bindings {
            let Computation::Join(LetJoin::Value(Let { bindee, tail, .. })) =
                arena.inner.compus[&binding]
            else {
                unreachable!("an eligible binding is a value let")
            };
            let Value::Closure(Closure { body, .. }) = arena.inner.values[&bindee] else {
                unreachable!("an eligible binding binds a closure")
            };
            // The procedure's body expects the stack its closure did.
            if let Some(ValueProtocol::Thunk(stack)) = arena.inner.value_protocols.get(&bindee) {
                let stack = (**stack).clone();
                arena.inner.compu_protocols.insert_new(binding, stack);
            }
            arena.inner.compus.replace_existing(
                binding,
                LetProc { procedure, stack: StackVar, body, tail }.into(),
            );
        }
        for Call { call, procedure, stack } in calls {
            arena.inner.compus.replace_existing(call, CallProc { procedure, stack }.into());
        }
        Ok(FragmentProgram::try_new(SpsProgram::new(arena, root), scheme)
            .expect("localization preserves lexical ownership and its scheme's fragment"))
    }
}

/// Bottom-up escaping occurrences: the variables that occur in a node other than as the head of a
/// call outside every closure and fixed-point body. A closure binding is eligible when its binder is
/// called in its tail and escapes nowhere there. An eligible binding's closure body is no longer
/// under a closure, so the binding contributes the body's escaping occurrences rather than all of
/// its free variables, and the bindings around it are decided with that in mind.
struct Escapes<'a> {
    arena: &'a SpsInnerArena,
    variables: Variables,
    summaries: HashMap<EntityId, CoContext<DefId>>,
    /// The escaping occurrences of each closure's body, used if a binding localizes the closure.
    bodies: HashMap<ValueId, CoContext<DefId>>,
    /// Eligible bindings and their binders, innermost first.
    eligible: Vec<(CompuId, DefId)>,
}

impl<'a> Escapes<'a> {
    fn analyze(arena: &'a SpsInnerArena, root: CompuId) -> Vec<(CompuId, DefId)> {
        let mut variables = Variables::default();
        Traversal { arena }.run(root.into(), &mut variables);
        let mut escapes = Self {
            arena,
            variables,
            summaries: HashMap::new(),
            bodies: HashMap::new(),
            eligible: Vec::new(),
        };
        Traversal { arena }.run(root.into(), &mut escapes);
        escapes.eligible
    }

    fn take(&mut self, child: impl Into<EntityId>) -> CoContext<DefId> {
        let child = child.into();
        if let EntityId::Value(value) = child {
            self.bodies.remove(&value);
        }
        self.summaries.remove(&child).expect("a child exits before its parent")
    }

    fn free(&self, term: impl Into<TermId>) -> CoContext<DefId> {
        self.variables.free_variables(term.into()).expect("acyclic SPS").clone()
    }

    fn bound(&self, pattern: VPatId) -> Context<DefId> {
        self.variables.bound_variables(pattern).expect("acyclic SPS").clone()
    }

    fn value(&mut self, id: ValueId, value: &Value) -> CoContext<DefId> {
        match value {
            | Value::Var(def) => CoContext::singleton(*def),
            | Value::Closure(Closure { body, .. }) => {
                let body_escapes = self.take(*body);
                self.bodies.insert(id, body_escapes);
                self.free(*body)
            }
            | Value::Ctor(Ctor(_, body)) => self.take(*body),
            | Value::VCons(VCons { items, .. }) => {
                items.iter().fold(CoContext::new(), |acc, item| acc + self.take(*item))
            }
            | Value::Primitive(Primitive { operands, .. }) => {
                operands.iter().fold(CoContext::new(), |acc, operand| acc + self.take(*operand))
            }
            | Value::AddrOffset(AddrOffset { base, displacement }) => {
                self.take(*base) + self.take(*displacement)
            }
            | Value::Hole(_) | Value::Triv(_) | Value::Literal(_) => CoContext::new(),
        }
    }

    fn stack(&mut self, stack: &Stack) -> CoContext<DefId> {
        match stack {
            | Stack::Kont(Kont { binder, body }) => self.take(*body) - self.bound(*binder),
            | Stack::Var(_) => CoContext::new(),
            | Stack::Arg(Cons(value, rest)) => self.take(*value) + self.take(*rest),
            | Stack::Tag(Cons(_, rest)) => self.take(*rest),
        }
    }

    fn computation(&mut self, id: CompuId, computation: &Computation<LetJoin>) -> CoContext<DefId> {
        match computation {
            | Computation::Hole(SHole(stack))
            | Computation::ExternCall(ExternCall { stack, .. })
            | Computation::CallProc(CallProc { stack, .. }) => self.take(*stack),
            | Computation::Force(SForce { thunk, stack }) => {
                let thunk_escapes = self.take(*thunk);
                let stack_escapes = self.take(*stack);
                if matches!(self.arena.values[thunk], Value::Var(_)) {
                    stack_escapes
                } else {
                    thunk_escapes + stack_escapes
                }
            }
            | Computation::Ret(SReturn { stack, value }) => self.take(*stack) + self.take(*value),
            | Computation::Fix(SFix { param, stack, body }) => {
                self.take(*body);
                self.take(*stack) + (self.free(*body) - param)
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                self.take(*scrut) + (self.take(*body) - self.bound(*binder))
            }
            | Computation::Compare(CompareBranch { operands, when_true, when_false, .. }) => {
                self.take(operands[0])
                    + self.take(operands[1])
                    + self.take(*when_true)
                    + self.take(*when_false)
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                arms.iter().fold(self.take(*scrut), |acc, Matcher { binder, tail }| {
                    acc + (self.take(*tail) - self.bound(*binder))
                })
            }
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                let body = self.bodies.remove(bindee);
                let bindee_escapes = self.take(*bindee);
                let tail_escapes = self.take(*tail);
                let binder_var = match self.arena.vpats[binder] {
                    | ValuePattern::Var(def) => Some(def),
                    | _ => None,
                };
                let eligible = binder_var.filter(|def| {
                    body.is_some()
                        && self.arena.builtin_functions.get(bindee).is_none()
                        && self.free(*tail).iter().any(|free| free == def)
                        && !tail_escapes.iter().any(|escaping| escaping == def)
                });
                let rest = tail_escapes - self.bound(*binder);
                match (eligible, body) {
                    | (Some(procedure), Some(body)) => {
                        self.eligible.push((id, procedure));
                        body + rest
                    }
                    | _ => bindee_escapes + rest,
                }
            }
            | Computation::Join(LetJoin::Stack(Let { bindee, tail, .. })) => {
                self.take(*bindee) + self.take(*tail)
            }
            | Computation::LetProc(LetProc { body, tail, .. }) => {
                self.take(*body) + self.take(*tail)
            }
            | Computation::LetArg(Let { binder: Cons(binder, _), bindee, tail }) => {
                self.take(*bindee) + (self.take(*tail) - self.bound(*binder))
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => arms
                .iter()
                .fold(self.take(*scrut), |acc, CoMatcher { tail, .. }| acc + self.take(*tail)),
            | Computation::Memory(MemoryStep::Load { address, result, next, .. }) => {
                self.take(*address) + (self.take(*next) - self.bound(*result))
            }
            | Computation::Memory(MemoryStep::Store { address, value, next, .. }) => {
                self.take(*address) + self.take(*value) + self.take(*next)
            }
        }
    }
}

impl Visitor for Escapes<'_> {
    fn exit(&mut self, node: Node<'_>) {
        let summary = match node {
            | Node::Pattern(_, _) => return,
            | Node::Value(id, value) => self.value(id, value),
            | Node::Stack(_, stack) => self.stack(stack),
            | Node::Computation(id, computation) => self.computation(id, computation),
        };
        self.summaries.insert(node.id(), summary);
    }
}

/// A call of a localized binder, which becomes a procedure call.
struct Call {
    call: CompuId,
    procedure: DefId,
    stack: StackId,
}

/// The calls of each localized binder within its binding's tail.
struct Calls<'a> {
    arena: &'a SpsInnerArena,
    bindings: HashMap<CompuId, DefId>,
    /// Tails that bring a localized binder into scope when the traversal reaches them.
    openings: HashMap<EntityId, DefId>,
    /// Localized binders in scope, each with the tail that opened it.
    scopes: Vec<(EntityId, DefId)>,
    calls: Vec<Call>,
}

impl<'a> Calls<'a> {
    fn collect(
        arena: &'a SpsInnerArena, root: CompuId, bindings: &[(CompuId, DefId)],
    ) -> Vec<Call> {
        let mut calls = Self {
            arena,
            bindings: bindings.iter().copied().collect(),
            openings: HashMap::new(),
            scopes: Vec::new(),
            calls: Vec::new(),
        };
        Traversal { arena }.run(root.into(), &mut calls);
        calls.calls
    }
}

impl Visitor for Calls<'_> {
    fn enter(&mut self, node: Node<'_>, _: Edge, occurrence: Occurrence) {
        if occurrence != Occurrence::First {
            return;
        }
        if let Some(procedure) = self.openings.remove(&node.id()) {
            self.scopes.push((node.id(), procedure));
        }
        match node {
            | Node::Computation(id, Computation::Join(LetJoin::Value(Let { tail, .. }))) => {
                if let Some(procedure) = self.bindings.get(&id) {
                    self.openings.insert((*tail).into(), *procedure);
                }
            }
            | Node::Computation(call, Computation::Force(SForce { thunk, stack })) => {
                if let Value::Var(def) = self.arena.values[thunk]
                    && self.scopes.iter().any(|(_, procedure)| *procedure == def)
                {
                    self.calls.push(Call { call, procedure: def, stack: *stack });
                }
            }
            | _ => {}
        }
    }

    fn exit(&mut self, node: Node<'_>) {
        while self.scopes.last().is_some_and(|(opener, _)| *opener == node.id()) {
            self.scopes.pop();
        }
    }
}

#[cfg(test)]
mod tests;
