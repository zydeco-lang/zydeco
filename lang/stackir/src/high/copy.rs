//! Deep copies of SPS subtrees into fresh nodes of the same arena.
//!
//! The duplicating scheme compiles every arm of a branch against its own copy of the consumer stack,
//! and lexical ownership requires each copy to own its nodes. A copy keeps definition identities,
//! which stay lexically scoped because copies land in disjoint arms, and carries each node's source
//! site, protocol evidence, and builtin provenance.

use super::{syntax::*, traverse::EntityId};
use zydeco_utils::fold::{Driver, Explicit, Folder, Step};

impl SpsArena {
    /// Copy a stack, including the continuations and closures it contains, into fresh nodes.
    pub(crate) fn copy_stack(&mut self, stack: StackId) -> StackId {
        let EntityId::Stack(copy) = Explicit::run(&mut SubtreeCopy { arena: self }, stack.into())
        else {
            unreachable!("a copied stack is a stack")
        };
        copy
    }
}

struct SubtreeCopy<'a> {
    arena: &'a mut SpsArena,
}

/// One owned layer of a node under copy, with the position of its next child.
struct CopyFrame {
    source: EntityId,
    layer: Layer,
    position: usize,
}

enum Layer {
    Pattern(ValuePattern),
    Value(Value),
    Stack(Stack),
    Computation(Computation<LetJoin>),
}

/// A child slot of an owned layer, read before its subtree is copied and overwritten after.
enum Slot<'a> {
    Pattern(&'a mut VPatId),
    Value(&'a mut ValueId),
    Stack(&'a mut StackId),
    Computation(&'a mut CompuId),
}

impl Slot<'_> {
    fn source(&self) -> EntityId {
        match self {
            | Self::Pattern(id) => (**id).into(),
            | Self::Value(id) => (**id).into(),
            | Self::Stack(id) => (**id).into(),
            | Self::Computation(id) => (**id).into(),
        }
    }

    fn fill(self, copy: EntityId) {
        match (self, copy) {
            | (Self::Pattern(slot), EntityId::Pattern(id)) => *slot = id,
            | (Self::Value(slot), EntityId::Value(id)) => *slot = id,
            | (Self::Stack(slot), EntityId::Stack(id)) => *slot = id,
            | (Self::Computation(slot), EntityId::Computation(id)) => *slot = id,
            | _ => unreachable!("a copied child keeps its sort"),
        }
    }
}

impl Layer {
    /// The child at `position`, in the order the shared traversal enumerates children.
    fn slot(&mut self, position: usize) -> Option<Slot<'_>> {
        use Slot as S;
        match self {
            | Self::Pattern(pattern) => match pattern {
                | ValuePattern::Hole(_) | ValuePattern::Var(_) | ValuePattern::Triv(_) => None,
                | ValuePattern::Ctor(Ctor(_, body)) => (position == 0).then_some(S::Pattern(body)),
                | ValuePattern::Alias(Alias(ConsN(head, tail))) => {
                    if position == head.len() {
                        Some(S::Pattern(tail))
                    } else {
                        head.get_mut(position).map(S::Pattern)
                    }
                }
                | ValuePattern::VCons(VCons { items, .. }) => {
                    items.get_mut(position).map(S::Pattern)
                }
            },
            | Self::Value(value) => match value {
                | Value::Hole(_) | Value::Var(_) | Value::Triv(_) | Value::Literal(_) => None,
                | Value::Closure(Closure { body, .. }) => {
                    (position == 0).then_some(S::Computation(body))
                }
                | Value::Ctor(Ctor(_, body)) => (position == 0).then_some(S::Value(body)),
                | Value::VCons(VCons { items, .. }) => items.get_mut(position).map(S::Value),
                | Value::Primitive(Primitive { operands, .. }) => {
                    operands.get_mut(position).map(S::Value)
                }
                | Value::AddrOffset(AddrOffset { base, displacement }) => match position {
                    | 0 => Some(S::Value(base)),
                    | 1 => Some(S::Value(displacement)),
                    | _ => None,
                },
            },
            | Self::Stack(stack) => match stack {
                | Stack::Var(_) => None,
                | Stack::Kont(Kont { binder, body }) => match position {
                    | 0 => Some(S::Pattern(binder)),
                    | 1 => Some(S::Computation(body)),
                    | _ => None,
                },
                | Stack::Arg(Cons(value, rest)) => match position {
                    | 0 => Some(S::Value(value)),
                    | 1 => Some(S::Stack(rest)),
                    | _ => None,
                },
                | Stack::Tag(Cons(_, rest)) => (position == 0).then_some(S::Stack(rest)),
            },
            | Self::Computation(computation) => Self::computation_slot(computation, position),
        }
    }

    fn computation_slot(
        computation: &mut Computation<LetJoin>, position: usize,
    ) -> Option<Slot<'_>> {
        use Slot as S;
        match computation {
            | Computation::Memory(MemoryStep::Load { address, result, next, .. }) => match position
            {
                | 0 => Some(S::Value(address)),
                | 1 => Some(S::Pattern(result)),
                | 2 => Some(S::Computation(next)),
                | _ => None,
            },
            | Computation::Memory(MemoryStep::Store { address, value, next, .. }) => match position
            {
                | 0 => Some(S::Value(address)),
                | 1 => Some(S::Value(value)),
                | 2 => Some(S::Computation(next)),
                | _ => None,
            },
            | Computation::Hole(SHole(stack))
            | Computation::ExternCall(ExternCall { stack, .. }) => {
                (position == 0).then_some(S::Stack(stack))
            }
            | Computation::Force(SForce { thunk, stack }) => match position {
                | 0 => Some(S::Value(thunk)),
                | 1 => Some(S::Stack(stack)),
                | _ => None,
            },
            | Computation::Ret(SReturn { stack, value }) => match position {
                | 0 => Some(S::Stack(stack)),
                | 1 => Some(S::Value(value)),
                | _ => None,
            },
            | Computation::Fix(SFix { stack, body, .. }) => match position {
                | 0 => Some(S::Stack(stack)),
                | 1 => Some(S::Computation(body)),
                | _ => None,
            },
            | Computation::ProductMatch(SProductMatch { scrut, binder, body }) => match position {
                | 0 => Some(S::Value(scrut)),
                | 1 => Some(S::Pattern(binder)),
                | 2 => Some(S::Computation(body)),
                | _ => None,
            },
            | Computation::Compare(CompareBranch { operands, when_true, when_false, .. }) => {
                match position {
                    | 0 | 1 => Some(S::Value(&mut operands[position])),
                    | 2 => Some(S::Computation(when_true)),
                    | 3 => Some(S::Computation(when_false)),
                    | _ => None,
                }
            }
            | Computation::CoprodMatch(SCoprodMatch { scrut, arms }) => match position {
                | 0 => Some(S::Value(scrut)),
                | position => arms.get_mut((position - 1) / 2).map(|Matcher { binder, tail }| {
                    if position % 2 == 1 { S::Pattern(binder) } else { S::Computation(tail) }
                }),
            },
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => match position {
                | 0 => Some(S::Value(bindee)),
                | 1 => Some(S::Pattern(binder)),
                | 2 => Some(S::Computation(tail)),
                | _ => None,
            },
            | Computation::LetArg(Let { binder: Cons(binder, _), bindee, tail }) => {
                match position {
                    | 0 => Some(S::Stack(bindee)),
                    | 1 => Some(S::Pattern(binder)),
                    | 2 => Some(S::Computation(tail)),
                    | _ => None,
                }
            }
            | Computation::Join(LetJoin::Stack(Let { bindee, tail, .. })) => match position {
                | 0 => Some(S::Stack(bindee)),
                | 1 => Some(S::Computation(tail)),
                | _ => None,
            },
            | Computation::LetProc(LetProc { body, tail, .. }) => match position {
                | 0 => Some(S::Computation(body)),
                | 1 => Some(S::Computation(tail)),
                | _ => None,
            },
            | Computation::CallProc(CallProc { stack, .. }) => {
                (position == 0).then_some(S::Stack(stack))
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => match position {
                | 0 => Some(S::Stack(scrut)),
                | position => arms.get_mut(position - 1).map(|arm| S::Computation(&mut arm.tail)),
            },
        }
    }
}

impl Folder for SubtreeCopy<'_> {
    type Input = EntityId;
    type Output = EntityId;
    type Frame = CopyFrame;

    fn enter(&mut self, source: EntityId) -> Step<Self> {
        let inner = &self.arena.inner;
        let layer = match source {
            | EntityId::Pattern(id) => Layer::Pattern(inner.vpats[&id].clone()),
            | EntityId::Value(id) => Layer::Value(inner.values[&id].clone()),
            | EntityId::Stack(id) => Layer::Stack(inner.stacks[&id].clone()),
            | EntityId::Computation(id) => Layer::Computation(inner.compus[&id].clone()),
        };
        self.advance(CopyFrame { source, layer, position: 0 })
    }

    fn resume(&mut self, mut frame: CopyFrame, child: EntityId) -> Step<Self> {
        frame.layer.slot(frame.position).expect("a suspended layer has a child slot").fill(child);
        frame.position += 1;
        self.advance(frame)
    }
}

impl SubtreeCopy<'_> {
    fn advance(&mut self, mut frame: CopyFrame) -> Step<Self> {
        if let Some(slot) = frame.layer.slot(frame.position) {
            let input = slot.source();
            return Step::Call { input, frame };
        }
        Step::Return(self.build(frame.source, frame.layer))
    }

    /// Allocate the copied layer at its source site, then carry the source node's evidence.
    fn build(&mut self, source: EntityId, layer: Layer) -> EntityId {
        let arena = &mut *self.arena;
        match (source, layer) {
            | (EntityId::Pattern(source), Layer::Pattern(pattern)) => {
                let site = arena.admin.pattern_site(source);
                let copy = pattern.build(arena, site);
                if let Some(protocol) = arena.inner.pattern_protocols.get(&source).cloned() {
                    arena.inner.pattern_protocols.insert_new(copy, protocol);
                }
                copy.into()
            }
            | (EntityId::Value(source), Layer::Value(value)) => {
                let site = arena.admin.term_site(source);
                let copy = value.build(arena, site);
                if let Some(protocol) = arena.inner.value_protocols.get(&source).cloned() {
                    arena.inner.value_protocols.insert_new(copy, protocol);
                }
                if let Some(&role) = arena.inner.builtin_functions.get(&source) {
                    arena.inner.builtin_functions.insert_new(copy, role);
                }
                copy.into()
            }
            | (EntityId::Stack(source), Layer::Stack(stack)) => {
                let site = arena.admin.term_site(source);
                stack.build(arena, site).into()
            }
            | (EntityId::Computation(source), Layer::Computation(computation)) => {
                let site = arena.admin.term_site(source);
                let copy = computation.build(arena, site);
                if let Some(protocol) = arena.inner.compu_protocols.get(&source).cloned() {
                    arena.inner.compu_protocols.insert_new(copy, protocol);
                }
                copy.into()
            }
            | _ => unreachable!("a layer keeps the sort of its source"),
        }
    }
}

#[cfg(test)]
mod tests;
