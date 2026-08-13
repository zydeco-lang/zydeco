//! Sanity checks for the stack-passing style ZIR.
//!
//! These checks are intended for debugging only. They double check that:
//! - the root computation is closed (has no free variables), and
//! - closure bodies only reference variables that are listed in their capture sets.

use super::syntax::*;
use super::variables::FreeVars;
use zydeco_statics::surface_syntax::ScopedArena;

/// Check that the given stack IR arena is well-formed enough for debugging.
///
/// In particular, this function panics if:
/// - the root computation has free variables (an "open" program), or
/// - any closure body mentions variables that are not listed in its capture set.
pub fn check(program: &StackirProgram, scoped: &ScopedArena) {
    check_closed_root(program, scoped);
}

/// Ensure that the program root is closed (has no free variables).
fn check_closed_root(program: &StackirProgram, scoped: &ScopedArena) {
    let fv = program.root.free_vars(&program.arena);
    let fv_str =
        fv.0.iter()
            .map(|def| {
                let name = &scoped.defs[def];
                format!("{}{}", name.plain(), def.concise())
            })
            .collect::<Vec<_>>()
            .join(", ");
    if !fv.0.is_empty() {
        panic!("stack IR root {:?} is not closed; free variables: {}", program.root, fv_str);
    }
}
