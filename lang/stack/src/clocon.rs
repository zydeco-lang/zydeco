//! Closure conversion.
//!
//! After this pass, there will be no implicit captures.

use super::arena::*;
use super::subst::SubstitutionInPlace;
use super::syntax::*;
use std::collections::HashMap;
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_statics::tyck::syntax as ss;
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_syntax::VarName;
use zydeco_utils::context::Context;
use zydeco_utils::prelude::CoContext;

/// Perform closure conversion on the stack arena.
pub struct ClosureConverter<'a> {
    arena: &'a mut StackArena,
    scoped: &'a mut ScopedArena,
    _statics: &'a StaticsArena,
}
impl AsRef<StackArena> for ClosureConverter<'_> {
    fn as_ref(&self) -> &StackArena {
        &self.arena
    }
}
impl AsMut<StackArena> for ClosureConverter<'_> {
    fn as_mut(&mut self) -> &mut StackArena {
        &mut self.arena
    }
}

impl<'a> ClosureConverter<'a> {
    pub fn new(
        arena: &'a mut StackArena, scoped: &'a mut ScopedArena, statics: &'a StaticsArena,
    ) -> Self {
        Self { arena, scoped, _statics: statics }
    }

    pub fn convert(&mut self) {
        // Transform Fix computations
        let fixes: Vec<_> = (self.arena.compus.iter())
            .filter_map(|(id, compu)| match compu {
                | Computation::Fix(fix) => Some((*id, fix.clone())),
                | _ => None,
            })
            .collect();
        for (compu_id, fix) in fixes {
            self.convert_fix(compu_id, &fix);
        }

        // Transform Clo values (thunks)
        let clos: Vec<_> = (self.arena.values.iter())
            .filter_map(|(id, value)| match value {
                | Value::Clo(clo) => Some((*id, clo.clone())),
                | _ => None,
            })
            .collect();
        for (value_id, clo) in clos {
            self.convert_clo(value_id, &clo);
        }

        // Update Force operations to handle converted closures
        // Find all Force operations and update them to unpack the closure pair
        let forces: Vec<_> = (self.arena.compus.iter())
            .filter_map(|(id, compu)| match compu {
                | Computation::Force(force) => Some((*id, force.clone())),
                | _ => None,
            })
            .collect();
        for (compu_id, force) in forces {
            self.convert_force(compu_id, &force);
        }
    }

    /// Get the ss::TermId site for a CompuId, if it exists.
    fn get_compu_site(&self, compu_id: CompuId) -> Option<ss::TermId> {
        self.arena.terms.back(&TermId::Compu(compu_id)).copied()
    }

    /// Get the ss::TermId site for a ValueId, if it exists.
    fn get_value_site(&self, value_id: ValueId) -> Option<ss::TermId> {
        self.arena.terms.back(&TermId::Value(value_id)).copied()
    }

    /// Compute free variables in a computation using cocontext from scoped.
    fn free_vars_compu(&self, compu_id: CompuId) -> CoContext<DefId> {
        use super::free::FreeVars;
        compu_id.free_vars(&self)
    }

    /// Convert a Fix computation to explicit closure form.
    fn convert_fix(&mut self, old_compu_id: CompuId, fix: &SFix) {
        let site = self.get_compu_site(old_compu_id);

        // 1. Compute capture list for body (excluding param)
        let free_vars: Vec<DefId> = self.free_vars_compu(old_compu_id).into_iter().collect();
        let mut free_var_renames = HashMap::new();
        let mut renamed_captures = Vec::with_capacity(free_vars.len());
        for &capture in free_vars.iter() {
            let VarName(original_name) = self.scoped.defs[&capture].clone();
            let new_def = self.scoped.defs.alloc(VarName(format!("{original_name}#cap")));
            free_var_renames.insert(capture, new_def);
            renamed_captures.push(new_def);
        }

        // 2. Substitute free variables in the body to use freshly bound capture vars,
        //    then replace all occurrences of param with param applied to captures.
        //    In stack style: when param is used, push captures on stack, then use param.
        // Convert HashMap<DefId, DefId> to HashMap<DefId, ValueId> for substitution
        let mut subst_map = HashMap::new();
        for (&old_def, &new_def) in free_var_renames.iter() {
            let new_value_id = new_def.build(self, None);
            subst_map.insert(old_def, new_value_id);
        }
        fix.body.substitute_in_place(self, &subst_map);

        // 3. Wrap body in a let arg to retrieve captures from stack
        // Create a nested value pattern to destructure all captures from a nested pair
        // Build nested Cons pattern: Cons(capture1, Cons(capture2, ... Cons(captureN, Triv)))
        let mut capture_pattern = Triv.build(self, None);
        for &capture in free_vars.iter().rev() {
            let capture_vpat = (*free_var_renames.get(&capture).unwrap()).build(self, None);
            capture_pattern = Cons(capture_vpat, capture_pattern).build(self, None);
        }
        // Use a single LetArg to extract all captures from the stack
        let capture_stack = Bullet.build(self, site);
        let transformed_body =
            Let { binder: Cons(capture_pattern, Bullet), bindee: capture_stack, tail: fix.body }
                .build(self, site);

        // 4. Push the capture list onto the stack first, then run the fix.
        // Build the capture pair value from free_vars
        let mut capture_pair: ValueId = Triv.build(self, site);
        for &capture in free_vars.iter().rev() {
            let capture_val = capture.build(self, site);
            capture_pair = Cons(capture_val, capture_pair).build(self, site);
        }
        // Push the capture pair onto the stack
        let bullet_stack = Bullet.build(self, site);
        let capture_stack = Cons(capture_pair, bullet_stack).build(self, site);
        // Create the Fix computation
        let fix_compu = SFix { capture: Context::new(), param: fix.param, body: transformed_body }
            .build(self, site);
        // Wrap the Fix in a LetStack that pushes captures, then runs the Fix
        // Update the Fix in place with the wrapped computation
        self.arena
            .compus
            .replace(old_compu_id, Let { binder: Bullet, bindee: capture_stack, tail: fix_compu });
    }

    /// Convert a Clo (thunk) to explicit closure form.
    fn convert_clo(&mut self, old_value_id: ValueId, clo: &Clo) {
        // Preserve the site from the original value
        let site = self.get_value_site(old_value_id);

        // 1. Capture the environment (free variables in body)
        let free_vars: Vec<DefId> = self.free_vars_compu(clo.body).into_iter().collect();
        let mut free_var_renames = HashMap::new();

        // 2. Make the closure a pair of (capture list, body function)
        // Build the capture pair value from free_vars
        let mut capture_pair = Triv.build(self, site);
        let mut capture_pattern = Triv.build(self, None);
        for &capture in free_vars.iter().rev() {
            let VarName(original_name) = self.scoped.defs[&capture].clone();
            let new_def = self.scoped.defs.alloc(VarName(format!("{original_name}#cap")));
            free_var_renames.insert(capture, new_def);

            let capture_val = capture.build(self, site);
            capture_pair = Cons(capture_val, capture_pair).build(self, site);

            let capture_vpat = new_def.build(self, None);
            capture_pattern = Cons(capture_vpat, capture_pattern).build(self, None);
        }

        // Substitute free variables in the closure body to refer to the freshly
        // bound capture variables.
        // Convert HashMap<DefId, DefId> to HashMap<DefId, ValueId> for substitution
        let mut subst_map = HashMap::new();
        for (&old_def, &new_def) in free_var_renames.iter() {
            let new_value_id = new_def.build(self, None);
            subst_map.insert(old_def, new_value_id);
        }
        clo.body.substitute_in_place(self, &subst_map);

        // Use a single LetArg to extract all captures from the stack
        let capture_stack = Bullet.build(self, site);
        let transformed_body =
            Let { binder: Cons(capture_pattern, Bullet), bindee: capture_stack, tail: clo.body }
                .build(self, site);

        // The body is already a computation that can be wrapped in a closure
        // We'll store it as a closure that takes the captures as argument
        // The pair will be: (capture_values, body_closure)
        // where body_closure is a closure whose body is the original body
        let body_closure = Clo {
            capture: Context(vec![]), // Body closure doesn't need additional captures
            stack: Bullet,
            body: transformed_body,
        }
        .build(self, site);

        // Update the value in place with the pair: (captures, body_closure)
        self.arena.values.replace(old_value_id, Cons(capture_pair, body_closure));
    }

    /// Convert a Force computation to handle converted closures.
    fn convert_force(&mut self, compu_id: CompuId, force: &SForce) {
        // Always destructure the thunk as a pair at runtime using LetValue
        // The thunk should be a pair (capture_pair, body_closure) from converted closures
        let site = self.get_compu_site(compu_id);

        // Create fresh DefIds for the pattern binders
        let capture_pair_def = self.scoped.defs.alloc(VarName("__env__".into()));
        let body_closure_def = self.scoped.defs.alloc(VarName("__code__".into()));

        // Create Var patterns to bind the destructured values
        let capture_pair_vpat = capture_pair_def.build(self, None);
        let body_closure_vpat = body_closure_def.build(self, None);
        let pair_pattern = Cons(capture_pair_vpat, body_closure_vpat).build(self, None);

        // After destructuring with LetValue, we need to:
        // 1. Push capture_pair onto the stack
        // 2. Force body_closure
        // Reference the pattern-bound values using Value::Var
        let capture_pair_val: ValueId = capture_pair_def.build(self, site);
        let body_closure_val = body_closure_def.build(self, site);

        let capture_pair_stack = Cons(capture_pair_val, force.stack).build(self, site);
        let force_body =
            SForce { thunk: body_closure_val, stack: capture_pair_stack }.build(self, site);

        // LetValue to destructure: let Cons(capture_pair, body_closure) = thunk in ...
        // This will destructure the pair at runtime.
        // Replace the original Force with the transformed computation
        self.arena
            .compus
            .replace(compu_id, Let { binder: pair_pattern, bindee: force.thunk, tail: force_body });
    }
}
