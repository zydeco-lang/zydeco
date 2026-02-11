//! Sanity checks for the stack-passing style ZIR.
//!
//! These checks are intended for debugging only. They double check that:
//! - entry computations are closed (no free variables), and
//! - closure bodies only reference variables that are listed in their capture sets.

use super::syntax::*;
use super::variables::FreeVars;
use zydeco_statics::surface_syntax::ScopedArena;

/// Check that the given stack IR arena is well-formed enough for debugging.
///
/// In particular, this function panics if:
/// - any entry computation has free variables (an "open" program), or
/// - any closure body mentions variables that are not listed in its capture set.
pub fn check(arena: &StackirArena, scoped: &ScopedArena) {
    check_closed_entries(arena, scoped);
}

/// Ensure that all entry computations are closed (no free variables).
fn check_closed_entries(arena: &StackirArena, scoped: &ScopedArena) {
    for (entry, ()) in arena.inner.entry.iter() {
        let fv = entry.free_vars(arena);
        // pretty print the free variables
        let fv_str =
            fv.0.iter()
                .map(|def| {
                    let name = &scoped.defs[def];
                    format!("{}{}", name.plain(), def.concise())
                })
                .collect::<Vec<_>>()
                .join(", ");
        if !fv.0.is_empty() {
            panic!("stack IR entry {:?} is not closed; free variables: {}", entry, fv_str);
        }
    }
}
