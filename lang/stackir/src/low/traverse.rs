//! Shared structural traversal for first-order SPS analyses.

use super::syntax::*;
use rustc_hash::FxHashMap;
use zydeco_utils::fold::{Driver, Explicit, Folder, Step};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, derive_more::From)]
pub enum EntityId {
    Pattern(VPatId),
    Value(ValueId),
    Stack(StackId),
    Computation(CompuId),
}

#[derive(Clone, Copy, Debug)]
pub enum Node<'a> {
    Pattern(VPatId, &'a ValuePattern),
    Value(ValueId, &'a Value),
    Stack(StackId, &'a Stack),
    Computation(CompuId, &'a Computation),
}

impl EntityId {
    pub fn node(self, arena: &SpsLowInnerArena) -> Node<'_> {
        match self {
            | Self::Pattern(id) => Node::Pattern(id, &arena.vpats[&id]),
            | Self::Value(id) => Node::Value(id, &arena.values[&id]),
            | Self::Stack(id) => Node::Stack(id, &arena.stacks[&id]),
            | Self::Computation(id) => Node::Computation(id, &arena.compus[&id]),
        }
    }
}

impl Node<'_> {
    pub fn id(self) -> EntityId {
        match self {
            | Self::Pattern(id, _) => id.into(),
            | Self::Value(id, _) => id.into(),
            | Self::Stack(id, _) => id.into(),
            | Self::Computation(id, _) => id.into(),
        }
    }

    /// Select one child in constant time, without retaining a node's child vector in a frame.
    /// Provenance and continuation metadata describe syntax; they add no executable edges.
    pub fn child(self, position: usize) -> Option<(EntityId, Edge)> {
        let child = match self {
            | Self::Pattern(_, pattern) => match pattern {
                | ValuePattern::Hole(_) | ValuePattern::Var(_) | ValuePattern::Triv(_) => None,
                | ValuePattern::Ctor(Ctor(_, body)) => [(*body).into()].get(position).copied(),
                | ValuePattern::Alias(Alias(ConsN(head, tail))) => {
                    if position == head.len() { Some(tail) } else { head.get(position) }
                        .map(|id| (*id).into())
                }
                | ValuePattern::VCons(VCons { items, .. }) => {
                    items.get(position).map(|id| (*id).into())
                }
            },
            | Self::Value(_, value) => match value {
                | Value::Hole(_) | Value::Var(_) | Value::Triv(_) | Value::Literal(_) => None,
                | Value::Block(Block { entry, body, .. }) => {
                    if let Some((_, pattern)) = entry.words().nth(position) {
                        Some(pattern.into())
                    } else if position == entry.words().count() {
                        Some((*body).into())
                    } else {
                        None
                    }
                }
                | Value::ClosurePackage(ClosurePackage { environment, code }) => {
                    [(*environment).into(), (*code).into()].get(position).copied()
                }
                | Value::Ctor(Ctor(_, body)) => [(*body).into()].get(position).copied(),
                | Value::VCons(VCons { items, .. }) => items.get(position).map(|id| (*id).into()),
                | Value::Primitive(Primitive { operands, .. }) => {
                    operands.get(position).map(|id| (*id).into())
                }
            },
            | Self::Stack(_, stack) => match stack {
                | Stack::Var(_) => None,
                | Stack::Arg(Cons(value, stack)) => {
                    [(*value).into(), (*stack).into()].get(position).copied()
                }
                | Stack::Tag(Cons(_, stack)) => [(*stack).into()].get(position).copied(),
                | Stack::ContinuationPackage(ContinuationPackage { code, residual }) => {
                    [(*code).into(), (*residual).into()].get(position).copied()
                }
            },
            | Self::Computation(id, computation) => match computation {
                | Computation::Hole(SHole(stack))
                | Computation::ExternCall(ExternCall { stack, .. }) => {
                    [(*stack).into()].get(position).copied()
                }
                | Computation::Jump(Jump { target, argument, stack }) => {
                    [(*target).into(), argument.word().1.into(), (*stack).into()]
                        .get(position)
                        .copied()
                }
                | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                    [(*scrut).into(), (*binder).into(), (*body).into()].get(position).copied()
                }
                | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                    if position == 0 {
                        Some((*scrut).into())
                    } else {
                        arms.get((position - 1) / 2).map(|arm| {
                            if position % 2 == 1 { arm.binder.into() } else { arm.tail.into() }
                        })
                    }
                }
                | Computation::LetValue(LetValue { binder, bindee, tail }) => {
                    [(*bindee).into(), (*binder).into(), (*tail).into()].get(position).copied()
                }
                | Computation::LetStack(LetStack { bindee, tail, .. }) => {
                    return match position {
                        | 0 => Some(((*bindee).into(), Edge::Child)),
                        | 1 => Some(((*tail).into(), Edge::BranchJoin(id))),
                        | _ => None,
                    };
                }
                | Computation::LetArg(LetArg { binder: Cons(binder, _), bindee, tail }) => {
                    [(*bindee).into(), (*binder).into(), (*tail).into()].get(position).copied()
                }
                | Computation::CoCase(SCoMatch { scrut, arms }) => {
                    if position == 0 {
                        Some((*scrut).into())
                    } else {
                        arms.get(position - 1).map(|arm| arm.tail.into())
                    }
                }
                | Computation::OpenClosure(OpenClosure { package, environment, code, body }) => {
                    [(*package).into(), (*environment).into(), (*code).into(), (*body).into()]
                        .get(position)
                        .copied()
                }
                | Computation::OpenContinuation(OpenContinuation { package, code, body }) => {
                    [(*package).into(), (*code).into(), (*body).into()].get(position).copied()
                }
            },
        };
        child.map(|id| (id, Edge::Child))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Edge {
    Root,
    Child,
    BranchJoin(CompuId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Occurrence {
    First,
    Shared,
    Cyclic,
}

/// Observe every incoming edge, expanding each node and delivering its exit only once.
/// Repeated or cyclic edges do not expand children or deliver another exit.
pub trait Visitor {
    fn enter(&mut self, _node: Node<'_>, _edge: Edge, _occurrence: Occurrence) {}
    fn exit(&mut self, _node: Node<'_>) {}
}

pub struct Together<A, B> {
    pub first: A,
    pub second: B,
}

impl<A: Visitor, B: Visitor> Visitor for Together<A, B> {
    fn enter(&mut self, node: Node<'_>, edge: Edge, occurrence: Occurrence) {
        self.first.enter(node, edge, occurrence);
        self.second.enter(node, edge, occurrence);
    }
    fn exit(&mut self, node: Node<'_>) {
        self.first.exit(node);
        self.second.exit(node);
    }
}

pub struct Traversal<'a> {
    pub arena: &'a SpsLowInnerArena,
}

impl Traversal<'_> {
    pub fn run(&self, root: EntityId, visitor: &mut impl Visitor) {
        self.run_with_driver::<Explicit>(root, visitor);
    }

    pub fn run_with_driver<D: Driver>(&self, root: EntityId, visitor: &mut impl Visitor) {
        D::run(
            &mut AnalysisFolder { arena: self.arena, visitor, states: FxHashMap::default() },
            (root, Edge::Root),
        );
    }
}

enum VisitState {
    Active,
    Complete,
}

struct AnalysisFolder<'a, 'visitor, V> {
    arena: &'a SpsLowInnerArena,
    visitor: &'visitor mut V,
    states: FxHashMap<EntityId, VisitState>,
}

impl<'a, V: Visitor> Folder for AnalysisFolder<'a, '_, V> {
    type Input = (EntityId, Edge);
    type Output = ();
    type Frame = (Node<'a>, usize);

    fn enter(&mut self, (id, edge): Self::Input) -> Step<Self> {
        let occurrence = match self.states.get(&id) {
            | None => Occurrence::First,
            | Some(VisitState::Active) => Occurrence::Cyclic,
            | Some(VisitState::Complete) => Occurrence::Shared,
        };
        let node = id.node(self.arena);
        self.visitor.enter(node, edge, occurrence);
        if occurrence != Occurrence::First {
            return Step::Return(());
        }
        self.states.insert(id, VisitState::Active);
        self.resume((node, 0), ())
    }

    fn resume(&mut self, (node, position): Self::Frame, (): ()) -> Step<Self> {
        if let Some(input) = node.child(position) {
            Step::Call { input, frame: (node, position + 1) }
        } else {
            self.states.insert(node.id(), VisitState::Complete);
            self.visitor.exit(node);
            Step::Return(())
        }
    }
}

#[cfg(test)]
mod tests;
