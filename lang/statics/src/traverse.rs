//! Borrowed view of the residual runtime graph, excluding classifier and erased witness edges.

use crate::{arena::StaticsArena, syntax::*};
use rustc_hash::FxHashSet;
use zydeco_utils::arena::ArenaAccess;

#[derive(Clone, Copy)]
pub enum RuntimeNode<'a> {
    Value(ValueId, &'a Value),
    Computation(CompuId, &'a Computation),
    /// Its checked foreign implementation supplies the value, including an authored hole payload.
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

pub struct RuntimeGraph<'a> {
    pub statics: &'a StaticsArena,
}

impl<'a> RuntimeGraph<'a> {
    /// Each call traverses one stable residual graph, visiting shared nodes once.
    pub fn nodes(&self, root: TermAnnId) -> RuntimeNodes<'a> {
        let root = match root {
            | TermAnnId::Value(value, _) => Some(self.statics.execution_value(value).into()),
            | TermAnnId::Compu(compu, _) => Some(self.statics.execution_compu(compu).into()),
            | _ => None,
        };
        RuntimeNodes {
            statics: self.statics,
            pending: root.into_iter().collect(),
            visited: FxHashSet::default(),
        }
    }
}

pub struct RuntimeNodes<'a> {
    statics: &'a StaticsArena,
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
                // A checked foreign import supplies this value's implementation.
                | TermId::Value(value) if self.statics.foreign_imports.get(&value).is_some() => {}
                | TermId::Value(value) => match &self.statics.values[&value] {
                    | Value::Hole(_) => {}
                    | Value::Named(Named(_, body))
                    | Value::Ctor(Ctor(_, body))
                    | Value::SCons(ConsN(_, body))
                    | Value::Proj(Proj(body, _))
                    | Value::ValAbs(Abs(_, body)) => self.pending.push((*body).into()),
                    | Value::Match(Match { scrut, arms }) => {
                        self.pending.push((*scrut).into());
                        self.pending.extend(arms.iter().map(|arm| TermId::Value(arm.tail)));
                    }
                    | Value::Int64Op(Int64ValueOp { operands, .. }) => {
                        self.pending.extend(operands.iter().map(|operand| TermId::Value(*operand)));
                    }
                    | Value::Let(Let { bindee, tail, .. }) => {
                        self.pending.extend([TermId::from(*bindee), TermId::from(*tail)])
                    }
                    | Value::ValApp(App(function, argument)) => {
                        self.pending.push(TermId::from(*function));
                        if let ValArgument::Value(argument) = argument {
                            self.pending.push(TermId::from(*argument));
                        }
                    }
                    | Value::Thunk(Thunk(body)) => self.pending.push((*body).into()),
                    | Value::VCons(fields) => {
                        self.pending.extend(fields.iter().copied().map(TermId::Value))
                    }
                    | Value::Var(_) | Value::Triv(_) | Value::Lit(_) => {}
                },
                | TermId::Compu(compu) => match &self.statics.compus[&compu] {
                    | Computation::Hole(_) => {}
                    | Computation::VAbs(Abs(_, body))
                    | Computation::TAbs(Abs(_, body))
                    | Computation::Fix(Fix(_, body))
                    | Computation::TApp(App(body, _))
                    | Computation::Dtor(Dtor(body, _)) => self.pending.push((*body).into()),
                    | Computation::VApp(App(function, argument)) => {
                        self.pending.extend([TermId::from(*function), TermId::from(*argument)])
                    }
                    | Computation::Force(Force(value)) | Computation::Ret(Return(value)) => {
                        self.pending.push((*value).into())
                    }
                    | Computation::Let(Let { bindee, tail, .. }) => {
                        self.pending.extend([TermId::from(*bindee), TermId::from(*tail)])
                    }
                    | Computation::Do(Bind { bindee, tail, .. }) => {
                        self.pending.extend([TermId::from(*bindee), TermId::from(*tail)])
                    }
                    | Computation::Match(Match { scrut, arms }) => {
                        self.pending.push((*scrut).into());
                        self.pending.extend(arms.iter().map(|arm| TermId::Compu(arm.tail)));
                    }
                    | Computation::CoMatch(CoMatch { arms }) => {
                        self.pending.extend(arms.iter().map(|arm| TermId::Compu(arm.tail)))
                    }
                },
                | _ => {}
            }
            return Some(match term {
                | TermId::Value(value) if self.statics.foreign_imports.get(&value).is_some() => {
                    RuntimeNode::Foreign(value)
                }
                | TermId::Value(value) => RuntimeNode::Value(value, &self.statics.values[&value]),
                | TermId::Compu(compu) => {
                    RuntimeNode::Computation(compu, &self.statics.compus[&compu])
                }
                | _ => continue,
            });
        }
        None
    }
}
