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
    traverse::{Edge, Node, Occurrence, Scopes, Traversal, Visitor},
    variables::Variables,
};
use crate::protocol::ValueProtocol;
use std::convert::Infallible;
use zydeco_foundation::{
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
        let eligible = Escapes::analyze(&arena.inner, root);
        let calls = Calls::collect(&arena.inner, root, &eligible);
        for (binding, procedure) in eligible.lets {
            let Computation::LetValue(Let { bindee, tail, .. }) = arena.inner.compus[&binding]
            else {
                unreachable!("an eligible binding is a value let")
            };
            let Value::Ext(ValueExt::Closure(Closure { body, .. })) = arena.inner.values[&bindee]
            else {
                unreachable!("an eligible binding binds a closure")
            };
            // The procedure's body expects the stack its closure did.
            if let Some(ValueProtocol::Thunk(stack)) = arena.inner.value_protocols.get(bindee) {
                let stack = (**stack).clone();
                arena.inner.compu_protocols.insert_new(binding, stack);
            }
            arena.inner.compus.replace_existing(
                binding,
                LetProc { procedure, recursive: false, stack: StackVar, body, tail }.into(),
            );
        }
        for (fix_id, procedure, recursive) in eligible.fixes {
            let Computation::Ext(CompuExt::Fix(SFix { stack, body, .. })) =
                arena.inner.compus[&fix_id]
            else {
                unreachable!("an eligible fix is an SFix")
            };
            let site = arena.admin.term_site(fix_id);
            let tail = CallProc { procedure, stack }.build(&mut arena, site);
            arena.inner.compus.replace_existing(
                fix_id,
                LetProc { procedure, recursive, stack: StackVar, body, tail }.into(),
            );
        }
        for Call { call, procedure, stack } in calls {
            arena.inner.compus.replace_existing(call, CallProc { procedure, stack }.into());
        }
        Ok(FragmentProgram::from_pass(SpsProgram::new(arena, root), scheme))
    }
}

struct Eligible {
    lets: Vec<(CompuId, DefId)>,
    fixes: Vec<(CompuId, DefId, bool)>,
}

/// Bottom-up escaping occurrences: the variables that occur in a node other than as the head of a
/// call outside every closure and fixed-point body. A closure binding or `SFix` is eligible when
/// its binder escapes nowhere in its scope. An eligible binding's body is no longer under a
/// closure, so the binding contributes the body's escaping occurrences rather than all of its free
/// variables, and the bindings around it are decided with that in mind.
struct Escapes<'a> {
    arena: &'a SpsInnerArena,
    variables: Variables,
    value_summaries: SecondaryMap<ValueId, CoContext<DefId>>,
    stack_summaries: SecondaryMap<StackId, CoContext<DefId>>,
    compu_summaries: SecondaryMap<CompuId, CoContext<DefId>>,
    /// The escaping occurrences of each closure's body, used if a binding localizes the closure.
    bodies: SecondaryMap<ValueId, CoContext<DefId>>,
    /// Eligible closure bindings and their binders, innermost first.
    eligible_lets: Vec<(CompuId, DefId)>,
    /// Eligible `SFix` nodes with their parameter and whether they are recursive.
    eligible_fixes: Vec<(CompuId, DefId, bool)>,
}

impl<'a> Escapes<'a> {
    fn analyze(arena: &'a SpsInnerArena, root: CompuId) -> Eligible {
        let mut escapes = Self {
            arena,
            variables: Variables::default(),
            value_summaries: SecondaryMap::new(),
            stack_summaries: SecondaryMap::new(),
            compu_summaries: SecondaryMap::new(),
            bodies: SecondaryMap::new(),
            eligible_lets: Vec::new(),
            eligible_fixes: Vec::new(),
        };
        Traversal { arena }.run(root.into(), &mut escapes);
        Eligible { lets: escapes.eligible_lets, fixes: escapes.eligible_fixes }
    }

    fn take_value(&mut self, id: ValueId) -> CoContext<DefId> {
        self.bodies.remove(id);
        self.value_summaries.remove(id).expect("a child exits before its parent")
    }

    fn take_stack(&mut self, id: StackId) -> CoContext<DefId> {
        self.stack_summaries.remove(id).expect("a child exits before its parent")
    }

    fn take_compu(&mut self, id: CompuId) -> CoContext<DefId> {
        self.compu_summaries.remove(id).expect("a child exits before its parent")
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
            | Value::Ext(ValueExt::Closure(Closure { body, .. })) => {
                let body_escapes = self.take_compu(*body);
                self.bodies.insert_new(id, body_escapes);
                self.free(*body)
            }
            | Value::Ctor(Ctor(_, body)) => self.take_value(*body),
            | Value::VCons(VCons { items, .. }) => {
                items.iter().fold(CoContext::new(), |acc, item| acc + self.take_value(*item))
            }
            | Value::Primitive(Primitive { operands, .. }) => operands
                .iter()
                .fold(CoContext::new(), |acc, operand| acc + self.take_value(*operand)),
            | Value::AddrOffset(AddrOffset { base, displacement }) => {
                self.take_value(*base) + self.take_value(*displacement)
            }
            | Value::Hole(_) | Value::Triv(_) | Value::Literal(_) => CoContext::new(),
        }
    }

    fn stack(&mut self, stack: &Stack) -> CoContext<DefId> {
        match stack {
            | Stack::Ext(StackExt::Kont(Kont { binder, body })) => {
                self.take_compu(*body) - self.bound(*binder)
            }
            | Stack::Var(_) => CoContext::new(),
            | Stack::Arg(Cons(value, rest)) => self.take_value(*value) + self.take_stack(*rest),
            | Stack::Tag(Cons(_, rest)) => self.take_stack(*rest),
        }
    }

    fn computation(&mut self, id: CompuId, computation: &Computation) -> CoContext<DefId> {
        match computation {
            | Computation::Hole(SHole(stack))
            | Computation::ExternCall(ExternCall { stack, .. })
            | Computation::Ext(CompuExt::CallProc(CallProc { stack, .. })) => {
                self.take_stack(*stack)
            }
            | Computation::Ext(CompuExt::Force(SForce { thunk, stack })) => {
                let thunk_escapes = self.take_value(*thunk);
                let stack_escapes = self.take_stack(*stack);
                if matches!(self.arena.values[thunk], Value::Var(_)) {
                    stack_escapes
                } else {
                    thunk_escapes + stack_escapes
                }
            }
            | Computation::Ext(CompuExt::Ret(SReturn { stack, value })) => {
                self.take_stack(*stack) + self.take_value(*value)
            }
            | Computation::Ext(CompuExt::Fix(SFix { param, stack, body })) => {
                let body_escapes = self.take_compu(*body);
                let stack_escapes = self.take_stack(*stack);
                if !body_escapes.iter().any(|escaping| escaping == param) {
                    let recursive = self.free(*body).iter().any(|free| free == param);
                    self.eligible_fixes.push((id, *param, recursive));
                    stack_escapes + (body_escapes - param)
                } else {
                    stack_escapes + (self.free(*body) - param)
                }
            }
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                self.take_value(*scrut) + (self.take_compu(*body) - self.bound(*binder))
            }
            | Computation::Compare(CompareBranch { operands, when_true, when_false, .. }) => {
                self.take_value(operands[0])
                    + self.take_value(operands[1])
                    + self.take_compu(*when_true)
                    + self.take_compu(*when_false)
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                arms.iter().fold(self.take_value(*scrut), |acc, Matcher { binder, tail }| {
                    acc + (self.take_compu(*tail) - self.bound(*binder))
                })
            }
            | Computation::LetValue(Let { binder, bindee, tail }) => {
                let body = self.bodies.remove(bindee);
                let bindee_escapes = self.take_value(*bindee);
                let tail_escapes = self.take_compu(*tail);
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
                        self.eligible_lets.push((id, procedure));
                        body + rest
                    }
                    | _ => bindee_escapes + rest,
                }
            }
            | Computation::LetStack(Let { bindee, tail, .. }) => {
                self.take_stack(*bindee) + self.take_compu(*tail)
            }
            | Computation::Ext(CompuExt::LetProc(LetProc { body, tail, .. })) => {
                self.take_compu(*body) + self.take_compu(*tail)
            }
            | Computation::LetArg(Let { binder: Cons(binder, _), bindee, tail }) => {
                self.take_stack(*bindee) + (self.take_compu(*tail) - self.bound(*binder))
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                arms.iter().fold(self.take_stack(*scrut), |acc, CoMatcher { tail, .. }| {
                    acc + self.take_compu(*tail)
                })
            }
            | Computation::Memory(MemoryStep::Load { address, result, next, .. }) => {
                self.take_value(*address) + (self.take_compu(*next) - self.bound(*result))
            }
            | Computation::Memory(MemoryStep::Store { address, value, next, .. }) => {
                self.take_value(*address) + self.take_value(*value) + self.take_compu(*next)
            }
        }
    }
}

impl Visitor<Sps> for Escapes<'_> {
    fn enter(&mut self, node: Node<'_>, edge: Edge, occurrence: Occurrence) {
        self.variables.enter(node, edge, occurrence);
    }

    fn exit(&mut self, node: Node<'_>) {
        self.variables.exit(node);
        match node {
            | Node::Pattern(_, _) => {}
            | Node::Value(id, value) => {
                let summary = self.value(id, value);
                self.value_summaries.insert_new(id, summary);
            }
            | Node::Stack(id, stack) => {
                let summary = self.stack(stack);
                self.stack_summaries.insert_new(id, summary);
            }
            | Node::Computation(id, computation) => {
                let summary = self.computation(id, computation);
                self.compu_summaries.insert_new(id, summary);
            }
        }
    }
}

/// A call of a localized binder, which becomes a procedure call.
struct Call {
    call: CompuId,
    procedure: DefId,
    stack: StackId,
}

/// The calls of each localized binder within its binding's tail or contified fixed-point body.
struct Calls<'a> {
    arena: &'a SpsInnerArena,
    lets: SecondaryMap<CompuId, DefId>,
    fixes: SecondaryMap<CompuId, DefId>,
    /// Localized binders in scope, each opened by the tail or body it scopes over.
    scopes: Scopes<DefId>,
    calls: Vec<Call>,
}

impl<'a> Calls<'a> {
    fn collect(arena: &'a SpsInnerArena, root: CompuId, eligible: &Eligible) -> Vec<Call> {
        if eligible.lets.is_empty() && eligible.fixes.is_empty() {
            return Vec::new();
        }
        let mut lets = SecondaryMap::new();
        for &(compu, def) in &eligible.lets {
            lets.insert_new(compu, def);
        }
        let mut fixes = SecondaryMap::new();
        for &(compu, def, _) in &eligible.fixes {
            fixes.insert_new(compu, def);
        }
        let mut calls = Self { arena, lets, fixes, scopes: Scopes::default(), calls: Vec::new() };
        Traversal { arena }.run(root.into(), &mut calls);
        calls.calls
    }
}

impl Visitor<Sps> for Calls<'_> {
    fn enter(&mut self, node: Node<'_>, _: Edge, occurrence: Occurrence) {
        if occurrence != Occurrence::First {
            return;
        }
        self.scopes.enter(node.id());
        match node {
            | Node::Computation(id, Computation::LetValue(Let { tail, .. })) => {
                if let Some(procedure) = self.lets.get(id) {
                    self.scopes.open_at((*tail).into(), *procedure);
                }
            }
            | Node::Computation(id, Computation::Ext(CompuExt::Fix(SFix { body, .. }))) => {
                if let Some(procedure) = self.fixes.get(id) {
                    self.scopes.open_at((*body).into(), *procedure);
                }
            }
            | Node::Computation(
                call,
                Computation::Ext(CompuExt::Force(SForce { thunk, stack })),
            ) => {
                if let Value::Var(def) = self.arena.values[thunk]
                    && self.scopes.innermost_first().any(|procedure| *procedure == def)
                {
                    self.calls.push(Call { call, procedure: def, stack: *stack });
                }
            }
            | _ => {}
        }
    }

    fn exit(&mut self, node: Node<'_>) {
        self.scopes.exit(node.id());
    }
}

#[cfg(test)]
mod tests;
