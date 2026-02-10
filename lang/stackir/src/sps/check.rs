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
    check_closure_captures(arena);
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

/// Ensure that closure bodies only reference variables from their capture sets.
fn check_closure_captures(arena: &StackirArena) {
    for (value_id, value) in arena.inner.values.iter() {
        if let Value::Closure(Closure { capture, stack: _, body }) = value {
            let fv = body.free_vars(arena);

            // Any free variable of the body must appear in the capture list.
            let mut missing = Vec::new();
            'fv: for def in fv.iter() {
                for cap in capture.iter() {
                    if cap == def {
                        continue 'fv;
                    }
                }
                missing.push(*def);
            }

            if !missing.is_empty() {
                panic!(
                    "closure value {:?} has body {:?} with free variables {:?} \
                     not listed in its capture set {:?}",
                    value_id, body, missing, capture
                );
            }
        }
    }
}
