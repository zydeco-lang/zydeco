//! Execution readiness of the shared residual typed program.

use crate::{arena::StaticsArena, syntax::*};
use std::collections::HashSet;

/// A hole left in code or data that can reach execution.
#[derive(Clone, Debug, thiserror::Error)]
#[error(
    "cannot execute an unfilled value or computation hole; fill the hole before running or building the program"
)]
pub struct ExecutableHole {
    pub term: TermId,
}

pub struct ExecutionReadiness;

impl ExecutionReadiness {
    /// Inspect only the residual runtime graph. Type annotations and eliminated
    /// static definitions may still contain holes for interactive inspection.
    pub fn check(statics: &StaticsArena, root: TermAnnId) -> Result<(), ExecutableHole> {
        let root: TermId = match root {
            | TermAnnId::Value(value, _) => statics.execution_value(value).into(),
            | TermAnnId::Compu(compu, _) => statics.execution_compu(compu).into(),
            | _ => return Ok(()),
        };
        let mut pending = vec![root];
        let mut visited = HashSet::new();
        while let Some(term) = pending.pop() {
            if !visited.insert(term) {
                continue;
            }
            match term {
                | TermId::Value(value) => match &statics.values[&value] {
                    | Value::Hole(_) => return Err(ExecutableHole { term }),
                    | Value::Named(Named(_, body))
                    | Value::Ctor(Ctor(_, body))
                    | Value::SCons(ConsN(_, body))
                    | Value::Proj(Proj(body, _))
                    | Value::ValAbs(Abs(_, body)) => pending.push((*body).into()),
                    | Value::Let(Let { bindee, tail, .. }) => {
                        pending.extend([TermId::from(*bindee), TermId::from(*tail)])
                    }
                    | Value::ValApp(App(function, argument)) => {
                        pending.push(TermId::from(*function));
                        if let ValArgument::Value(argument) = argument {
                            pending.push(TermId::from(*argument));
                        }
                    }
                    | Value::Thunk(Thunk(body)) => pending.push((*body).into()),
                    | Value::VCons(fields) => {
                        pending.extend(fields.iter().copied().map(TermId::Value))
                    }
                    | Value::Var(_) | Value::Triv(_) | Value::Lit(_) => {}
                },
                | TermId::Compu(compu) => match &statics.compus[&compu] {
                    | Computation::Hole(_) => return Err(ExecutableHole { term }),
                    | Computation::VAbs(Abs(_, body))
                    | Computation::TAbs(Abs(_, body))
                    | Computation::Fix(Fix(_, body))
                    | Computation::TApp(App(body, _))
                    | Computation::Dtor(Dtor(body, _)) => pending.push((*body).into()),
                    | Computation::VApp(App(function, argument)) => {
                        pending.extend([TermId::from(*function), TermId::from(*argument)])
                    }
                    | Computation::Force(Force(value)) | Computation::Ret(Return(value)) => {
                        pending.push((*value).into())
                    }
                    | Computation::Let(Let { bindee, tail, .. }) => {
                        pending.extend([TermId::from(*bindee), TermId::from(*tail)])
                    }
                    | Computation::Do(Bind { bindee, tail, .. }) => {
                        pending.extend([TermId::from(*bindee), TermId::from(*tail)])
                    }
                    | Computation::Match(Match { scrut, arms }) => {
                        pending.push((*scrut).into());
                        pending.extend(arms.iter().map(|arm| TermId::Compu(arm.tail)));
                    }
                    | Computation::CoMatch(CoMatch { arms }) => {
                        pending.extend(arms.iter().map(|arm| TermId::Compu(arm.tail)))
                    }
                },
                | _ => {}
            }
        }
        Ok(())
    }
}
