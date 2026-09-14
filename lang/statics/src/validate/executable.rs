//! Execution readiness of the shared residual typed program.

use crate::{
    arena::StaticsArena,
    syntax::*,
    traverse::{RuntimeGraph, RuntimeNode},
};
use zydeco_surface::diagnostic::Diagnostics;

/// A hole left in code or data that can reach execution.
#[derive(Clone, Debug, thiserror::Error)]
#[error(
    "cannot execute an unfilled value or computation hole; fill the hole before running or building the program"
)]
pub struct ExecutableHole {
    pub term: TermId,
}

pub type ExecutableHoles = Diagnostics<ExecutableHole>;

pub struct ExecutionReadiness;

impl ExecutionReadiness {
    /// Collect every distinct reachable hole, preserving the residual runtime boundary.
    pub fn check(statics: &StaticsArena, root: TermAnnId) -> Result<(), ExecutableHoles> {
        let holes = (RuntimeGraph { statics })
            .nodes(root)
            .filter_map(|node| match node {
                | RuntimeNode::Value(_, Value::Hole(_))
                | RuntimeNode::Computation(_, Computation::Hole(_)) => {
                    Some(ExecutableHole { term: node.id() })
                }
                | _ => None,
            })
            .collect();
        match Diagnostics::with_errors(holes) {
            | Some(errors) => Err(errors),
            | None => Ok(()),
        }
    }
}
