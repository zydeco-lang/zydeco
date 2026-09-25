//! Structural child views for checked terms and the residual runtime graph.

use crate::{arena::StaticsArena, syntax::*};
use rustc_hash::FxHashSet;
use zydeco_utils::arena::ArenaAccess;

/// Raw checked terms, including classifiers, static arguments, and patterns.
/// Type nodes are leaves here: classifier clients use `TypeFolder` to select
/// binder and nominal-definition policies. Definition references are leaves;
/// following a closed definition is a client-owned semantic edge.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CheckedNode {
    Type(TypeId),
    Value(ValueId),
    Computation(CompuId),
    Pattern(VPatId),
}

impl CheckedNode {
    pub fn children(self, statics: &StaticsArena, mut child: impl FnMut(Self)) {
        match self {
            | Self::Type(_) => {}
            | Self::Pattern(pattern) => {
                child(Self::Type(statics.annotations_vpat[&pattern]));
                match &statics.vpats[&pattern] {
                    | ValuePattern::Hole(_)
                    | ValuePattern::Var(_)
                    | ValuePattern::Triv(_)
                    | ValuePattern::Lit(_) => {}
                    | ValuePattern::Named(Named(_, inner)) | ValuePattern::Ctor(Ctor(_, inner)) => {
                        child(Self::Pattern(*inner))
                    }
                    | ValuePattern::Alias(Alias(ConsN(aliases, base))) => {
                        aliases.iter().for_each(|alias| child(Self::Pattern(*alias)));
                        child(Self::Pattern(*base));
                    }
                    | ValuePattern::VCons(fields) => {
                        fields.iter().for_each(|field| child(Self::Pattern(*field)))
                    }
                    | ValuePattern::SCons(ConsN(_, body)) => child(Self::Pattern(*body)),
                    | ValuePattern::View(view) => {
                        child(Self::Value(view.function));
                        child(Self::Pattern(view.pattern));
                    }
                }
            }
            | Self::Value(value) => {
                child(Self::Type(statics.annotations_value[&value]));
                match &statics.values[&value] {
                    | Value::Hole(_) | Value::Var(_) | Value::Triv(_) | Value::Lit(_) => {}
                    | Value::Named(Named(_, body))
                    | Value::Ctor(Ctor(_, body))
                    | Value::Proj(Proj(body, _)) => child(Self::Value(*body)),
                    | Value::Thunk(Thunk(body)) => child(Self::Computation(*body)),
                    | Value::VCons(fields) => {
                        fields.iter().for_each(|field| child(Self::Value(*field)))
                    }
                    | Value::SCons(ConsN(statics, body)) => {
                        for argument in statics {
                            if let StaticTermId::Type(ty) = argument {
                                child(Self::Type(*ty));
                            }
                        }
                        child(Self::Value(*body));
                    }
                    | Value::ValAbs(Abs(binder, body)) => {
                        if let ValBinder::Value(pattern) = binder {
                            child(Self::Pattern(*pattern));
                        }
                        child(Self::Value(*body));
                    }
                    | Value::ValApp(App(function, argument)) => {
                        child(Self::Value(*function));
                        match argument {
                            | ValArgument::Value(value) => child(Self::Value(*value)),
                            | ValArgument::Type(ty) => child(Self::Type(*ty)),
                        }
                    }
                    | Value::Let(Let { binder, bindee, tail }) => {
                        child(Self::Value(*bindee));
                        child(Self::Pattern(*binder));
                        child(Self::Value(*tail));
                    }
                    | Value::Match(Match { scrut, arms }) => {
                        child(Self::Value(*scrut));
                        for Matcher { binder, tail } in arms {
                            child(Self::Pattern(*binder));
                            child(Self::Value(*tail));
                        }
                    }
                    | Value::IntOp(IntValueOp { operands, .. }) => {
                        operands.iter().for_each(|operand| child(Self::Value(*operand)))
                    }
                }
            }
            | Self::Computation(compu) => {
                child(Self::Type(statics.annotations_compu[&compu]));
                match &statics.compus[&compu] {
                    | Computation::Hole(_) => {}
                    | Computation::VAbs(Abs(binder, body))
                    | Computation::Fix(Fix(binder, body)) => {
                        child(Self::Pattern(*binder));
                        child(Self::Computation(*body));
                    }
                    | Computation::TAbs(Abs(_, body)) | Computation::Dtor(Dtor(body, _)) => {
                        child(Self::Computation(*body))
                    }
                    | Computation::TApp(App(body, argument)) => {
                        child(Self::Computation(*body));
                        child(Self::Type(*argument));
                    }
                    | Computation::VApp(App(function, argument)) => {
                        child(Self::Computation(*function));
                        child(Self::Value(*argument));
                    }
                    | Computation::Force(Force(value)) | Computation::Ret(Return(value)) => {
                        child(Self::Value(*value))
                    }
                    | Computation::Let(Let { binder, bindee, tail }) => {
                        child(Self::Value(*bindee));
                        child(Self::Pattern(*binder));
                        child(Self::Computation(*tail));
                    }
                    | Computation::Do(Bind { binder, bindee, tail }) => {
                        child(Self::Computation(*bindee));
                        child(Self::Pattern(*binder));
                        child(Self::Computation(*tail));
                    }
                    | Computation::Match(Match { scrut, arms }) => {
                        child(Self::Value(*scrut));
                        for Matcher { binder, tail } in arms {
                            child(Self::Pattern(*binder));
                            child(Self::Computation(*tail));
                        }
                    }
                    | Computation::CoMatch(CoMatch { arms }) => {
                        arms.iter().for_each(|arm| child(Self::Computation(arm.tail)))
                    }
                }
            }
        }
    }
}

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
                    | Value::IntOp(IntValueOp { operands, .. }) => {
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
