//! Lexical SPS instances of the shared structural traversal.

use super::syntax::*;
pub use crate::traverse::{Children, Occurrence, Together, Visitor};

pub type EntityId = crate::traverse::EntityId<Sps>;
pub type Node<'a> = crate::traverse::Node<'a, Sps>;
pub type Edge = crate::traverse::Edge<Sps>;
pub type Traversal<'a> = crate::traverse::Traversal<'a, SpsInnerArena>;

impl Children<Sps> for ValueExt {
    fn child(&self, position: usize) -> Option<EntityId> {
        match self {
            | Self::Closure(Closure { body, .. }) => (position == 0).then_some((*body).into()),
        }
    }
}

impl Children<Sps> for StackExt {
    fn child(&self, position: usize) -> Option<EntityId> {
        match self {
            | Self::Kont(Kont { binder, body }) => {
                [(*binder).into(), (*body).into()].get(position).copied()
            }
        }
    }
}

impl Children<Sps> for CompuExt {
    fn child(&self, position: usize) -> Option<EntityId> {
        match self {
            | Self::Force(SForce { thunk, stack }) => {
                [(*thunk).into(), (*stack).into()].get(position).copied()
            }
            | Self::Ret(SReturn { stack, value }) => {
                [(*stack).into(), (*value).into()].get(position).copied()
            }
            | Self::Fix(SFix { stack, body, .. }) => {
                [(*stack).into(), (*body).into()].get(position).copied()
            }
            | Self::LetProc(LetProc { body, tail, .. }) => {
                [(*body).into(), (*tail).into()].get(position).copied()
            }
            | Self::CallProc(CallProc { stack, .. }) => (position == 0).then_some((*stack).into()),
        }
    }
}

#[cfg(test)]
mod tests;
