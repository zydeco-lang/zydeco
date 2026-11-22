//! Explicit traversal of residual runtime graphs.

use crate::syntax::*;
use rustc_hash::FxHashSet;

/// One borrowed runtime node visited by [`RuntimeGraph`].
#[derive(Clone, Copy)]
pub enum RuntimeNode<'a> {
    Value(ValueId, &'a Value),
    Computation(CompuId, &'a Computation),
    /// A checked foreign import supplied externally.
    Foreign(ValueId),
}

impl RuntimeNode<'_> {
    pub fn id(self) -> TermId {
        match self {
            | Self::Value(id, _) | Self::Foreign(id) => id.into(),
            | Self::Computation(id, _) => id.into(),
        }
    }
}

/// Borrowed residual runtime graph view over a [`CoreArena`].
pub struct RuntimeGraph<'a> {
    pub core: &'a CoreArena,
}

impl<'a> RuntimeGraph<'a> {
    /// Traverse nodes reachable from `root` and any retained dead-binding runtime holes.
    pub fn nodes(&self, root: CoreRoot) -> RuntimeNodes<'a> {
        let mut pending = Vec::with_capacity(1 + self.core.retained_holes.len());
        pending.extend(self.core.retained_holes.iter().copied().rev());
        pending.push(root.into());
        RuntimeNodes { core: self.core, pending, visited: FxHashSet::default() }
    }
}

/// Depth-first iterator visiting each reachable [`TermId`] once.
pub struct RuntimeNodes<'a> {
    core: &'a CoreArena,
    pending: Vec<TermId>,
    visited: FxHashSet<TermId>,
}

impl<'a> Iterator for RuntimeNodes<'a> {
    type Item = RuntimeNode<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(term) = self.pending.pop() {
            if !self.visited.insert(term) {
                continue;
            }
            match term {
                | TermId::Value(id) => {
                    let value = &self.core.values[id];
                    match value {
                        | Value::Foreign(_) => return Some(RuntimeNode::Foreign(id)),
                        | Value::Let(Let { bindee, tail, .. }) => {
                            self.pending.extend([TermId::from(*bindee), TermId::from(*tail)]);
                        }
                        | Value::Thunk(Thunk(body)) => self.pending.push((*body).into()),
                        | Value::Ctor { payload, .. } => self.pending.push((*payload).into()),
                        | Value::Proj { head, .. } => self.pending.push((*head).into()),
                        | Value::VCons(VCons { items, .. }) => {
                            self.pending.extend(items.iter().copied().map(TermId::Value));
                        }
                        | Value::Hole(_) | Value::Var(_) | Value::Triv(_) | Value::Lit(_) => {}
                    }
                    return Some(RuntimeNode::Value(id, value));
                }
                | TermId::Compu(id) => {
                    let compu = &self.core.compus[id];
                    match compu {
                        | Computation::Hole(_) => {}
                        | Computation::VAbs(Abs(_, body))
                        | Computation::Fix { body, .. }
                        | Computation::Dtor { body, .. } => self.pending.push((*body).into()),
                        | Computation::VApp(App(function, argument)) => {
                            self.pending.extend([TermId::from(*function), TermId::from(*argument)]);
                        }
                        | Computation::Force(Force(value)) | Computation::Ret(Return(value)) => {
                            self.pending.push((*value).into());
                        }
                        | Computation::Let(Let { bindee, tail, .. }) => {
                            self.pending.extend([TermId::from(*bindee), TermId::from(*tail)]);
                        }
                        | Computation::Do(Bind { bindee, tail, .. }) => {
                            self.pending.extend([TermId::from(*bindee), TermId::from(*tail)]);
                        }
                        | Computation::Match(Match { scrut, arms }) => {
                            self.pending.push((*scrut).into());
                            self.pending.extend(arms.iter().map(|arm| TermId::Compu(arm.tail)));
                        }
                        | Computation::CoMatch { arms } => {
                            self.pending.extend(arms.iter().map(|arm| TermId::Compu(arm.tail)));
                        }
                    }
                    return Some(RuntimeNode::Computation(id, compu));
                }
            }
        }
        None
    }
}
