//! Where closure conversion places each procedure's label.
//!
//! A local label runs in the frame of the block that binds it, so every jump to it must lie in that
//! block. Each continuation becomes a block of its own, so a call inside a continuation reaches the
//! label from another frame, and so does a call inside the body of a closed procedure. A procedure
//! with such a call becomes a closed label, whose free variables each call pushes. Inner procedures
//! are placed first, because a closed procedure moves the calls in its body to another frame.

use super::{high, low};
use crate::high::traverse::{EntityId, Node, Traversal, Visitor};
use std::collections::HashMap;
use zydeco_utils::context::CoContext;

/// The calls a subtree makes to procedures bound outside it, split by whether they stay in the
/// subtree's frame.
#[derive(Default)]
struct Calls {
    local: CoContext<high::DefId>,
    crossing: CoContext<high::DefId>,
}

impl Calls {
    fn join(self, other: Self) -> Self {
        Self { local: self.local + other.local, crossing: self.crossing + other.crossing }
    }

    /// The calls of a subtree that runs in a frame of its own all cross into it.
    fn enclosed(self) -> Self {
        Self { local: CoContext::new(), crossing: self.local + self.crossing }
    }

    fn without(self, procedure: &high::DefId) -> Self {
        Self { local: self.local - procedure, crossing: self.crossing - procedure }
    }

    fn reach(&self, procedure: &high::DefId) -> (bool, bool) {
        let contains = |calls: &CoContext<high::DefId>| calls.iter().any(|call| call == procedure);
        (contains(&self.local), contains(&self.crossing))
    }
}

/// Bottom-up placement of every procedure binding. `None` marks a procedure that no call reaches.
pub(super) struct Placement {
    summaries: HashMap<EntityId, Calls>,
    kinds: HashMap<high::CompuId, Option<low::LabelKind>>,
}

impl Placement {
    pub(super) fn analyze(
        arena: &high::SpsInnerArena, root: high::CompuId,
    ) -> HashMap<high::CompuId, Option<low::LabelKind>> {
        let mut placement = Self { summaries: HashMap::new(), kinds: HashMap::new() };
        Traversal { arena }.run(root.into(), &mut placement);
        placement.kinds
    }

    fn take(&mut self, child: impl Into<EntityId>) -> Calls {
        self.summaries.remove(&child.into()).unwrap_or_default()
    }

    fn children(&mut self, node: Node<'_>) -> Calls {
        (0..)
            .map_while(|position| node.child(position))
            .fold(Calls::default(), |calls, (child, _)| calls.join(self.take(child)))
    }
}

impl Visitor<high::Sps> for Placement {
    fn exit(&mut self, node: Node<'_>) {
        use high::{Computation as Compu, Value};
        let calls = match node {
            | Node::Pattern(..) => return,
            // Continuation, closure, and fixed-point bodies run in frames of their own.
            | Node::Stack(_, high::Stack::Ext(high::StackExt::Kont(_)))
            | Node::Value(_, Value::Ext(high::ValueExt::Closure(_))) => {
                self.children(node).enclosed()
            }
            | Node::Computation(
                _,
                Compu::Ext(high::CompuExt::Fix(high::SFix { stack, body, .. })),
            ) => self.take(*stack).join(self.take(*body).enclosed()),
            | Node::Computation(
                _,
                Compu::Ext(high::CompuExt::CallProc(high::CallProc { procedure, stack })),
            ) => {
                let call = Calls { local: CoContext::singleton(*procedure), ..Calls::default() };
                self.take(*stack).join(call)
            }
            | Node::Computation(
                id,
                Compu::Ext(high::CompuExt::LetProc(high::LetProc {
                    procedure, body, tail, ..
                })),
            ) => {
                let tail = self.take(*tail);
                let body = self.take(*body);
                let kind = match tail.reach(procedure) {
                    | (_, true) => Some(low::LabelKind::Closed),
                    | (true, false) => Some(low::LabelKind::Local),
                    | (false, false) => None,
                };
                self.kinds.insert(id, kind);
                let tail = tail.without(procedure);
                let body =
                    if kind == Some(low::LabelKind::Closed) { body.enclosed() } else { body };
                tail.join(body)
            }
            | _ => self.children(node),
        };
        self.summaries.insert(node.id(), calls);
    }
}
