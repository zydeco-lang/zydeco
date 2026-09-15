//! Shared structural traversal for lexical SPS analyses.

use super::syntax::*;
use rustc_hash::FxHashMap;

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
    Computation(CompuId, &'a Computation<LetJoin>),
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

    fn children(self, mut child: impl FnMut(EntityId, Edge)) {
        let mut ordinary = |id| child(id, Edge::Child);
        match self {
            | Self::Pattern(_, pattern) => match pattern {
                | ValuePattern::Hole(_) | ValuePattern::Var(_) | ValuePattern::Triv(_) => {}
                | ValuePattern::Ctor(Ctor(_, body)) => ordinary((*body).into()),
                | ValuePattern::Alias(Alias(items)) => {
                    items.iter().for_each(|item| ordinary((*item).into()));
                }
                | ValuePattern::VCons(VCons { items, .. }) => {
                    items.iter().for_each(|item| ordinary((*item).into()));
                }
            },
            | Self::Value(_, value) => match value {
                | Value::Hole(_) | Value::Var(_) | Value::Triv(_) | Value::Literal(_) => {}
                | Value::Closure(Closure { body, .. }) => ordinary((*body).into()),
                | Value::Ctor(Ctor(_, body)) => ordinary((*body).into()),
                | Value::VCons(VCons { items, .. }) => {
                    items.iter().for_each(|item| ordinary((*item).into()));
                }
                | Value::Primitive(Primitive { operands: items, .. }) => {
                    items.iter().for_each(|item| ordinary((*item).into()));
                }
                | Value::AddrOffset(AddrOffset { base, displacement }) => {
                    ordinary((*base).into());
                    ordinary((*displacement).into());
                }
            },
            | Self::Stack(_, stack) => match stack {
                | Stack::Var(_) => {}
                | Stack::Kont(Kont { binder, body }) => {
                    ordinary((*binder).into());
                    ordinary((*body).into());
                }
                | Stack::Arg(Cons(value, stack)) => {
                    ordinary((*value).into());
                    ordinary((*stack).into());
                }
                | Stack::Tag(Cons(_, stack)) => ordinary((*stack).into()),
            },
            | Self::Computation(id, computation) => match computation {
                | Computation::Memory(MemoryStep::Load { address, result, next, .. }) => {
                    ordinary((*address).into());
                    ordinary((*result).into());
                    ordinary((*next).into());
                }
                | Computation::Memory(MemoryStep::Store { address, value, next, .. }) => {
                    ordinary((*address).into());
                    ordinary((*value).into());
                    ordinary((*next).into());
                }
                | Computation::Hole(SHole(stack))
                | Computation::ExternCall(ExternCall { stack, .. }) => ordinary((*stack).into()),
                | Computation::Force(SForce { thunk, stack }) => {
                    ordinary((*thunk).into());
                    ordinary((*stack).into());
                }
                | Computation::Ret(SReturn { stack, value }) => {
                    ordinary((*stack).into());
                    ordinary((*value).into());
                }
                | Computation::Fix(SFix { stack, body, .. }) => {
                    ordinary((*stack).into());
                    ordinary((*body).into());
                }
                | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                    ordinary((*scrut).into());
                    ordinary((*binder).into());
                    ordinary((*body).into());
                }
                | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                    ordinary((*scrut).into());
                    arms.iter().for_each(|Matcher { binder, tail }| {
                        ordinary((*binder).into());
                        ordinary((*tail).into());
                    });
                }
                | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                    ordinary((*bindee).into());
                    ordinary((*binder).into());
                    ordinary((*tail).into());
                }
                | Computation::Join(LetJoin::Stack(Let { bindee, tail, .. })) => {
                    ordinary((*bindee).into());
                    child((*tail).into(), Edge::BranchJoin(id));
                }
                | Computation::LetArg(Let { binder: Cons(binder, _), bindee, tail }) => {
                    ordinary((*bindee).into());
                    ordinary((*binder).into());
                    ordinary((*tail).into());
                }
                | Computation::CoCase(SCoMatch { scrut, arms }) => {
                    ordinary((*scrut).into());
                    arms.iter().for_each(|CoMatcher { tail, .. }| ordinary((*tail).into()));
                }
            },
        }
    }
}

/// The immediate structural edge, including the parent owning a branch join.
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

/// Every incoming edge has an entry event, including shared and cyclic edges.
/// Exit is delivered once per node, after its children. Repeated descendants are not expanded.
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
    pub arena: &'a StackirInnerArena,
}

enum VisitState {
    Active,
    Complete,
}

enum Step<'a> {
    Enter(EntityId, Edge),
    Exit(Node<'a>),
}

impl Traversal<'_> {
    pub fn run(&self, root: EntityId, visitor: &mut impl Visitor) {
        let mut states = FxHashMap::default();
        let mut pending = vec![Step::Enter(root, Edge::Root)];
        while let Some(step) = pending.pop() {
            match step {
                | Step::Enter(id, edge) => {
                    let occurrence = match states.get(&id) {
                        | None => Occurrence::First,
                        | Some(VisitState::Active) => Occurrence::Cyclic,
                        | Some(VisitState::Complete) => Occurrence::Shared,
                    };
                    let node = match id {
                        | EntityId::Pattern(id) => Node::Pattern(id, &self.arena.vpats[&id]),
                        | EntityId::Value(id) => Node::Value(id, &self.arena.values[&id]),
                        | EntityId::Stack(id) => Node::Stack(id, &self.arena.stacks[&id]),
                        | EntityId::Computation(id) => {
                            Node::Computation(id, &self.arena.compus[&id])
                        }
                    };
                    visitor.enter(node, edge, occurrence);
                    if occurrence != Occurrence::First {
                        continue;
                    }
                    states.insert(id, VisitState::Active);
                    pending.push(Step::Exit(node));
                    let start = pending.len();
                    node.children(|child, edge| pending.push(Step::Enter(child, edge)));
                    pending[start..].reverse();
                }
                | Step::Exit(node) => {
                    states.insert(node.id(), VisitState::Complete);
                    visitor.exit(node);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
