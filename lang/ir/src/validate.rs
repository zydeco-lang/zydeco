//! Execution-readiness validation for residual [`CoreProgram`]s.

use crate::{
    syntax::*,
    traverse::{RuntimeGraph, RuntimeNode},
};
use zydeco_foundation::diagnostic::Diagnostics;

/// A hole left in code or data that can reach execution.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error(
    "cannot execute an unfilled value or computation hole; fill the hole before running or building the program"
)]
pub struct ExecutableHole {
    pub term: TermId,
}

pub type ExecutableHoles = Diagnostics<ExecutableHole>;

/// Validator ensuring a [`CoreProgram`] contains no executable holes.
pub struct ExecutionReadiness;

impl ExecutionReadiness {
    /// Collect every distinct reachable hole in the residual runtime graph.
    pub fn check(program: &CoreProgram) -> Result<(), ExecutableHoles> {
        let holes = (RuntimeGraph { core: program.arena() })
            .nodes(program.root())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deep_residual_program_validates_on_a_small_stack() {
        std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(|| {
                let mut arena = CoreArena::default();
                let triv = arena.values.alloc(Value::Triv(Triv));
                let mut compu = arena.compus.alloc(Computation::Ret(Return(triv)));
                for _ in 0..8192 {
                    let binder = arena.vpats.alloc(ValuePattern::Hole(Hole));
                    compu = arena.compus.alloc(Computation::Let(Let {
                        binder,
                        bindee: triv,
                        tail: compu,
                    }));
                }
                let program = CoreProgram::new(arena, CoreRoot::Compu(compu));
                assert!(ExecutionReadiness::check(&program).is_ok());
            })
            .unwrap()
            .join()
            .unwrap();
    }

    #[test]
    fn duplicate_and_retained_holes_are_collected_once_per_identity() {
        let mut arena = CoreArena::default();
        let hole_val = arena.values.alloc(Value::Hole(Hole));
        let pair = arena
            .values
            .alloc(Value::VCons(VCons::new(vec![hole_val, hole_val], ProductLayout { arity: 2 })));
        let hole_comp = arena.compus.alloc(Computation::Hole(Hole));
        arena.retained_holes.push(TermId::Compu(hole_comp));
        let compu = arena.compus.alloc(Computation::Ret(Return(pair)));
        let program = CoreProgram::new(arena, CoreRoot::Compu(compu));
        let errors = ExecutionReadiness::check(&program).unwrap_err();
        assert_eq!(errors.len(), 2);
    }
}
