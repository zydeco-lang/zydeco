//! First-order SPSLow instances of the shared structural traversal.

use super::syntax::*;
pub use crate::traverse::{Children, Occurrence, Together, Visitor};

pub type EntityId = crate::traverse::EntityId<SpsLow>;
pub type Node<'a> = crate::traverse::Node<'a, SpsLow>;
pub type Edge = crate::traverse::Edge<SpsLow>;
pub type Traversal<'a> = crate::traverse::Traversal<'a, SpsLowInnerArena>;

impl Children<SpsLow> for ValueExt {
    fn child(&self, position: usize) -> Option<EntityId> {
        match self {
            | Self::Block(Block { entry, body, .. }) => {
                if let Some((_, pattern)) = entry.words().nth(position) {
                    Some(pattern.into())
                } else if position == entry.words().count() {
                    Some((*body).into())
                } else {
                    None
                }
            }
            | Self::ClosurePackage(ClosurePackage { environment, code }) => {
                [(*environment).into(), (*code).into()].get(position).copied()
            }
        }
    }
}

impl Children<SpsLow> for StackExt {
    fn child(&self, position: usize) -> Option<EntityId> {
        match self {
            | Self::ContinuationPackage(ContinuationPackage { code, residual }) => {
                [(*code).into(), (*residual).into()].get(position).copied()
            }
        }
    }
}

impl Children<SpsLow> for CompuExt {
    fn child(&self, position: usize) -> Option<EntityId> {
        match self {
            | Self::Jump(Jump { target, argument, stack }) => {
                [(*target).into(), argument.word().1.into(), (*stack).into()].get(position).copied()
            }
            | Self::LetLabel(LetLabel { body, tail, .. }) => {
                [(*body).into(), (*tail).into()].get(position).copied()
            }
            | Self::JumpLabel(JumpLabel { stack, .. }) => {
                (position == 0).then_some((*stack).into())
            }
            | Self::OpenClosure(OpenClosure { package, environment, code, body }) => {
                [(*package).into(), (*environment).into(), (*code).into(), (*body).into()]
                    .get(position)
                    .copied()
            }
            | Self::OpenContinuation(OpenContinuation { package, code, body }) => {
                [(*package).into(), (*code).into(), (*body).into()].get(position).copied()
            }
        }
    }
}

#[cfg(test)]
mod tests;
