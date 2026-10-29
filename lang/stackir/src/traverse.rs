//! Structural traversal shared by the stack IR phases.
//!
//! The traversal enumerates the children of the shared forms once and reaches the children of
//! a phase's own forms through [`Children`]. Every incoming edge is observed; children are
//! expanded and the exit is delivered once per node, so repeated and cyclic edges expand nothing.

use crate::syntax::*;
use rustc_hash::FxHashMap;
use zydeco_utils::fold::{Driver, Explicit, Folder, Step};

/// A node of one phase, by sort.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum EntityId<P: Phase> {
    Pattern(P::VPatId),
    Value(P::ValueId),
    Stack(P::StackId),
    Computation(P::CompuId),
}

/// A borrowed node of one phase with its identity.
#[derive(Clone, Copy, Debug)]
pub enum Node<'a, P: Phase> {
    Pattern(P::VPatId, &'a ValuePattern<P::VPatId>),
    Value(P::ValueId, &'a Value<P>),
    Stack(P::StackId, &'a Stack<P>),
    Computation(P::CompuId, &'a Computation<P>),
}

/// The children of a phase's own forms, one position at a time, in the order the traversal
/// visits them.
pub trait Children<P: Phase> {
    fn child(&self, position: usize) -> Option<EntityId<P>>;
}

/// The immediate structural edge, including the stack let-binding that owns a tail.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Edge<P: Phase> {
    Root,
    Child,
    StackLetTail(P::CompuId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Occurrence {
    First,
    Shared,
    Cyclic,
}

impl<P: Phase> EntityId<P> {
    pub fn node(self, nodes: &P::Nodes) -> Node<'_, P> {
        match self {
            | Self::Pattern(id) => Node::Pattern(id, &nodes.vpats()[&id]),
            | Self::Value(id) => Node::Value(id, &nodes.values()[&id]),
            | Self::Stack(id) => Node::Stack(id, &nodes.stacks()[&id]),
            | Self::Computation(id) => Node::Computation(id, &nodes.compus()[&id]),
        }
    }
}

impl<P: Phase> Node<'_, P> {
    pub fn id(self) -> EntityId<P> {
        match self {
            | Self::Pattern(id, _) => EntityId::Pattern(id),
            | Self::Value(id, _) => EntityId::Value(id),
            | Self::Stack(id, _) => EntityId::Stack(id),
            | Self::Computation(id, _) => EntityId::Computation(id),
        }
    }

    /// Select one child in constant time, without retaining a node's child vector in a frame.
    /// Provenance and continuation metadata describe syntax; they add no executable edges.
    pub fn child(self, position: usize) -> Option<(EntityId<P>, Edge<P>)> {
        use EntityId as E;
        let child = match self {
            | Self::Pattern(_, pattern) => match pattern {
                | ValuePattern::Hole(_) | ValuePattern::Var(_) | ValuePattern::Triv(_) => None,
                | ValuePattern::Ctor(Ctor(_, body)) => (position == 0).then_some(E::Pattern(*body)),
                | ValuePattern::Alias(Alias(ConsN(head, tail))) => {
                    if position == head.len() { Some(tail) } else { head.get(position) }
                        .map(|id| E::Pattern(*id))
                }
                | ValuePattern::VCons(VCons { items, .. }) => {
                    items.get(position).map(|id| E::Pattern(*id))
                }
            },
            | Self::Value(_, value) => match value {
                | Value::Hole(_) | Value::Var(_) | Value::Triv(_) | Value::Literal(_) => None,
                | Value::Ctor(Ctor(_, body)) => (position == 0).then_some(E::Value(*body)),
                | Value::VCons(VCons { items, .. }) => items.get(position).map(|id| E::Value(*id)),
                | Value::Primitive(Primitive { operands, .. }) => {
                    operands.get(position).map(|id| E::Value(*id))
                }
                | Value::AddrOffset(AddrOffset { base, displacement }) => {
                    [E::Value(*base), E::Value(*displacement)].get(position).copied()
                }
                | Value::Ext(form) => form.child(position),
            },
            | Self::Stack(_, stack) => match stack {
                | Stack::Var(_) => None,
                | Stack::Arg(Cons(value, stack)) => {
                    [E::Value(*value), E::Stack(*stack)].get(position).copied()
                }
                | Stack::Tag(Cons(_, stack)) => (position == 0).then_some(E::Stack(*stack)),
                | Stack::Ext(form) => form.child(position),
            },
            | Self::Computation(id, computation) => match computation {
                | Computation::Memory(MemoryStep::Load { address, result, next, .. }) => {
                    [E::Value(*address), E::Pattern(*result), E::Computation(*next)]
                        .get(position)
                        .copied()
                }
                | Computation::Memory(MemoryStep::Store { address, value, next, .. }) => {
                    [E::Value(*address), E::Value(*value), E::Computation(*next)]
                        .get(position)
                        .copied()
                }
                | Computation::Hole(SHole(stack))
                | Computation::ExternCall(ExternCall { stack, .. }) => {
                    (position == 0).then_some(E::Stack(*stack))
                }
                | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => {
                    [E::Value(*scrut), E::Pattern(*binder), E::Computation(*body)]
                        .get(position)
                        .copied()
                }
                | Computation::Compare(CompareBranch {
                    operands, when_true, when_false, ..
                }) => [
                    E::Value(operands[0]),
                    E::Value(operands[1]),
                    E::Computation(*when_true),
                    E::Computation(*when_false),
                ]
                .get(position)
                .copied(),
                | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => {
                    if position == 0 {
                        Some(E::Value(*scrut))
                    } else {
                        arms.get((position - 1) / 2).map(|arm| {
                            if position % 2 == 1 {
                                E::Pattern(arm.binder)
                            } else {
                                E::Computation(arm.tail)
                            }
                        })
                    }
                }
                | Computation::LetValue(Let { binder, bindee, tail }) => {
                    [E::Value(*bindee), E::Pattern(*binder), E::Computation(*tail)]
                        .get(position)
                        .copied()
                }
                | Computation::LetStack(Let { bindee, tail, .. }) => {
                    return match position {
                        | 0 => Some((E::Stack(*bindee), Edge::Child)),
                        | 1 => Some((E::Computation(*tail), Edge::StackLetTail(id))),
                        | _ => None,
                    };
                }
                | Computation::LetArg(Let { binder: Cons(binder, _), bindee, tail }) => {
                    [E::Stack(*bindee), E::Pattern(*binder), E::Computation(*tail)]
                        .get(position)
                        .copied()
                }
                | Computation::CoCase(SCoMatch { scrut, arms }) => {
                    if position == 0 {
                        Some(E::Stack(*scrut))
                    } else {
                        arms.get(position - 1).map(|arm| E::Computation(arm.tail))
                    }
                }
                | Computation::Ext(form) => form.child(position),
            },
        };
        child.map(|id| (id, Edge::Child))
    }
}

/// Observe every incoming edge, expanding each node and delivering its exit only once.
/// Repeated or cyclic edges do not expand children or deliver another exit.
pub trait Visitor<P: Phase> {
    fn enter(&mut self, _node: Node<'_, P>, _edge: Edge<P>, _occurrence: Occurrence) {}
    fn exit(&mut self, _node: Node<'_, P>) {}
}

/// Two independent analyses receiving the same events.
pub struct Together<A, B> {
    pub first: A,
    pub second: B,
}

impl<P: Phase, A: Visitor<P>, B: Visitor<P>> Visitor<P> for Together<A, B> {
    fn enter(&mut self, node: Node<'_, P>, edge: Edge<P>, occurrence: Occurrence) {
        self.first.enter(node, edge, occurrence);
        self.second.enter(node, edge, occurrence);
    }
    fn exit(&mut self, node: Node<'_, P>) {
        self.first.exit(node);
        self.second.exit(node);
    }
}

/// A traversal of the nodes of one phase's arena.
pub struct Traversal<'a, N: Nodes> {
    pub arena: &'a N,
}

impl<N: Nodes> Traversal<'_, N> {
    pub fn run(&self, root: EntityId<N::Phase>, visitor: &mut impl Visitor<N::Phase>) {
        self.run_with_driver::<Explicit>(root, visitor);
    }

    pub fn run_with_driver<D: Driver>(
        &self, root: EntityId<N::Phase>, visitor: &mut impl Visitor<N::Phase>,
    ) {
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

struct AnalysisFolder<'a, 'visitor, N: Nodes, V> {
    arena: &'a N,
    visitor: &'visitor mut V,
    states: FxHashMap<EntityId<N::Phase>, VisitState>,
}

impl<'a, N: Nodes, V: Visitor<N::Phase>> Folder for AnalysisFolder<'a, '_, N, V> {
    type Input = (EntityId<N::Phase>, Edge<N::Phase>);
    type Output = ();
    type Frame = (Node<'a, N::Phase>, usize);

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

/// Lexical scopes that open at nodes of one phase, for analyses that resolve a name against
/// the regions around its occurrence. A scope registered for a node opens when the traversal
/// enters that node and closes when the node exits.
pub struct Scopes<P: Phase, S> {
    /// Scopes that open when the traversal reaches the keyed node.
    openings: FxHashMap<EntityId<P>, S>,
    /// The open scopes around the current node, innermost last, each with the node that opened it.
    open: Vec<(EntityId<P>, S)>,
}

impl<P: Phase, S> Default for Scopes<P, S> {
    fn default() -> Self {
        Self { openings: FxHashMap::default(), open: Vec::new() }
    }
}

impl<P: Phase, S> Scopes<P, S> {
    /// Open `scope` when the traversal enters `node`.
    pub fn open_at(&mut self, node: EntityId<P>, scope: S) {
        self.openings.insert(node, scope);
    }

    /// Open the scope registered for the node being entered, if any.
    pub fn enter(&mut self, node: EntityId<P>) {
        if let Some(scope) = self.openings.remove(&node) {
            self.open.push((node, scope));
        }
    }

    /// Open `scope` at the node being entered.
    pub fn push(&mut self, node: EntityId<P>, scope: S) {
        self.open.push((node, scope));
    }

    /// Close the scopes that the exiting `node` opened.
    pub fn exit(&mut self, node: EntityId<P>) {
        while self.open.last().is_some_and(|(opener, _)| *opener == node) {
            self.open.pop();
        }
    }

    /// The open scopes, innermost first.
    pub fn innermost_first(&self) -> impl Iterator<Item = &S> {
        self.open.iter().rev().map(|(_, scope)| scope)
    }
}
