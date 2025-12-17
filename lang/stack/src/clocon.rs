//! Closure conversion.
//!
//! After this pass, there will be no implicit captures.

use super::arena::*;
use super::subst::SubstitutionInPlace;
use super::syntax::*;
use std::collections::{HashMap, HashSet};
use zydeco_statics::tyck::arena::StaticsArena;
use zydeco_statics::tyck::syntax as ss;
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_syntax::VarName;
use zydeco_utils::context::Context;

/// Perform closure conversion on the stack arena.
pub struct ClosureConverter<'a> {
    arena: &'a mut StackArena,
    scoped: &'a mut ScopedArena,
    statics: &'a StaticsArena,
    fix_force_visited: HashSet<CompuId>,
}
impl AsMut<StackArena> for ClosureConverter<'_> {
    fn as_mut(&mut self) -> &mut StackArena {
        self.arena
    }
}

impl<'a> ClosureConverter<'a> {
    pub fn new(
        arena: &'a mut StackArena, scoped: &'a mut ScopedArena, statics: &'a StaticsArena,
    ) -> Self {
        Self { arena, scoped, statics, fix_force_visited: HashSet::new() }
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
            self.convert_clo_force(compu_id, &force);
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

    /// Get the ss::TermId site for a StackId, if it exists.
    fn get_stack_site(&self, stack_id: StackId) -> Option<ss::TermId> {
        self.arena.terms.back(&TermId::Stack(stack_id)).copied()
    }

    /// Compute free variables in a computation using cocontext from scoped.
    fn free_vars_compu(&self, compu_id: CompuId) -> HashSet<DefId> {
        // Map from CompuId to ss::TermId via arena.terms
        let ss_term_id = match self.get_compu_site(compu_id) {
            | Some(ss_term_id) => ss_term_id,
            | None => {
                // If no mapping exists, fall back to empty set
                // This can happen for terms created during closure conversion
                log::warn!(
                    "No site mapping found for CompuId {:?}, falling back to empty free vars set",
                    compu_id
                );
                return HashSet::new();
            }
        };

        // Map from ss::TermId to su::TermId via statics.terms
        let su_term_id = match self.statics.terms.back(&ss_term_id) {
            | Some(su_term_id) => *su_term_id,
            | None => {
                // If no mapping exists, fall back to empty set
                log::warn!(
                    "No scoped TermId mapping found for ss::TermId {:?}, falling back to empty free vars set",
                    ss_term_id
                );
                return HashSet::new();
            }
        };

        // Get cocontext (free variables) from scoped arena
        let coctx = match self.scoped.coctxs_term_local.get(&su_term_id) {
            | Some(coctx) => coctx.clone(),
            | None => {
                // If no cocontext exists, fall back to empty set
                log::warn!(
                    "No cocontext found for scoped TermId {:?}, falling back to empty free vars set",
                    su_term_id
                );
                return HashSet::new();
            }
        };

        // Convert CoContext to HashSet<DefId>, filtering out type and kind identifiers
        // Only keep term-level (value/computation) identifiers, and exclude bound variables
        coctx
            .iter()
            .cloned()
            .filter(|def_id| {
                // Check if this is a term-level definition (value/computation)
                // by checking if it has a Type annotation (not Kind or Set)
                self.statics
                    .annotations_var
                    .get(def_id)
                    .map(|ann| matches!(ann, ss::AnnId::Type(_)))
                    .unwrap_or(false)
            })
            .collect()
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
            let new_value_id = {
                let this = &mut *self.arena;
                new_def.build(this, None)
            };
            subst_map.insert(old_def, new_value_id);
        }
        fix.body.substitute_in_place(&mut *self.arena, &subst_map);
        let transformed_body_inner = self.convert_fix_force(fix.body, fix.param, &renamed_captures);

        // 3. Wrap body in a let arg to retrieve captures from stack
        // Create a nested value pattern to destructure all captures from a nested pair
        // Build nested Cons pattern: Cons(capture1, Cons(capture2, ... Cons(captureN, Triv)))
        let mut capture_pattern = Triv.build(self, None);
        for &capture in free_vars.iter().rev() {
            let capture_vpat = {
                let vpat = *free_var_renames.get(&capture).unwrap();
                vpat.build(self, None)
            };
            capture_pattern = Cons(capture_vpat, capture_pattern).build(self, None);
        }
        // Use a single LetArg to extract all captures from the stack
        let capture_stack = Bullet.build(self, site);
        let transformed_body = Let {
            binder: Cons(capture_pattern, Bullet),
            bindee: capture_stack,
            tail: transformed_body_inner,
        }
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

    /// Replace all occurrences of param in body with application of param to captures.
    fn convert_fix_force(&mut self, body: CompuId, param: DefId, captures: &[DefId]) -> CompuId {
        // Traverse body and replace Value::Var(param) with application
        // Application in stack style: push captures onto stack, then call param
        self.convert_fix_force_rec(body, param, captures, &mut HashSet::new())
    }

    fn convert_fix_force_rec(
        &mut self, compu_id: CompuId, param: DefId, captures: &[DefId],
        visited: &mut HashSet<CompuId>,
    ) -> CompuId {
        if !visited.insert(compu_id) {
            return compu_id;
        }

        // Preserve the site from the original computation
        let site = self.get_compu_site(compu_id);
        let compu = self.arena.compus[&compu_id].clone();
        match compu {
            | Computation::Hole(h) => {
                let this = &mut *self.arena;
                h.build(this, site)
            }
            | Computation::Force(SForce { thunk, stack }) => {
                let new_thunk =
                    self.convert_fix_force_in_value(thunk, param, captures, &mut HashSet::new());
                let new_stack =
                    self.convert_fix_force_in_stack(stack, param, captures, &mut HashSet::new());
                SForce { thunk: new_thunk, stack: new_stack }.build(self, site)
            }
            | Computation::Ret(SReturn { stack, value }) => {
                let new_stack =
                    self.convert_fix_force_in_stack(stack, param, captures, &mut HashSet::new());
                let new_value =
                    self.convert_fix_force_in_value(value, param, captures, &mut HashSet::new());
                {
                    let this = &mut *self.arena;
                    let compu = SReturn { stack: new_stack, value: new_value };
                    compu.build(this, site)
                }
            }
            | Computation::Fix(fix) => {
                // Don't replace param if it's the same as the fix param (shadowing)
                if fix.param == param {
                    {
                        let this = &mut *self.arena;
                        fix.build(this, site)
                    }
                } else {
                    let new_body = self.convert_fix_force_rec(fix.body, param, captures, visited);
                    {
                        let this = &mut *self.arena;
                        let compu = SFix { capture: fix.capture, param: fix.param, body: new_body };
                        compu.build(this, site)
                    }
                }
            }
            | Computation::Case(Match { scrut, arms }) => {
                let new_scrut =
                    self.convert_fix_force_in_value(scrut, param, captures, &mut HashSet::new());
                let new_arms = arms
                    .into_iter()
                    .map(|arm| Matcher {
                        binder: arm.binder,
                        tail: self.convert_fix_force_rec(arm.tail, param, captures, visited),
                    })
                    .collect();
                {
                    let this = &mut *self.arena;
                    let compu = Match { scrut: new_scrut, arms: new_arms };
                    compu.build(this, site)
                }
            }
            | Computation::LetValue(Let { binder, bindee, tail }) => {
                let new_bindee =
                    self.convert_fix_force_in_value(bindee, param, captures, &mut HashSet::new());
                let new_tail = self.convert_fix_force_rec(tail, param, captures, visited);
                {
                    let this = &mut *self.arena;
                    let compu = Let { binder, bindee: new_bindee, tail: new_tail };
                    compu.build(this, site)
                }
            }
            | Computation::LetStack(Let { binder, bindee, tail }) => {
                let new_bindee =
                    self.convert_fix_force_in_stack(bindee, param, captures, &mut HashSet::new());
                let new_tail = self.convert_fix_force_rec(tail, param, captures, visited);
                {
                    let this = &mut *self.arena;
                    let compu = Let { binder, bindee: new_bindee, tail: new_tail };
                    compu.build(this, site)
                }
            }
            | Computation::LetArg(Let { binder, bindee, tail }) => {
                let new_bindee =
                    self.convert_fix_force_in_stack(bindee, param, captures, &mut HashSet::new());
                let new_tail = self.convert_fix_force_rec(tail, param, captures, visited);
                {
                    let this = &mut *self.arena;
                    let compu = Let { binder, bindee: new_bindee, tail: new_tail };
                    compu.build(this, site)
                }
            }
            | Computation::CoCase(CoMatch { arms }) => {
                let new_arms = arms
                    .into_iter()
                    .map(|arm| CoMatcher {
                        dtor: arm.dtor,
                        tail: self.convert_fix_force_rec(arm.tail, param, captures, visited),
                    })
                    .collect();
                {
                    let this = &mut *self.arena;
                    let compu = CoMatch { arms: new_arms };
                    compu.build(this, site)
                }
            }
        }
    }

    fn convert_fix_force_in_value(
        &mut self, value_id: ValueId, param: DefId, captures: &[DefId],
        visited: &mut HashSet<ValueId>,
    ) -> ValueId {
        if !visited.insert(value_id) {
            return value_id;
        }

        // Preserve the site from the original value
        let site = self.get_value_site(value_id);
        let value = self.arena.values[&value_id].clone();
        match value {
            | Value::Var(def) if def == param => {
                // When param is used, we need to apply it to captures
                // Param is a thunk that expects captures on the stack (via LetArg)
                // In stack style: push captures on stack, then force param
                // Since we're in a value context, we create a thunk that does this application
                // The thunk, when forced, will push captures and then force param
                // Create a nested pair value from all captures
                let mut capture_pair: ValueId = {
                    let this = &mut *self.arena;
                    Triv.build(this, site)
                };
                for &capture in captures.iter().rev() {
                    let capture_val = {
                        let this = &mut *self.arena;
                        capture.build(this, site)
                    };
                    capture_pair = {
                        let this = &mut *self.arena;
                        let value = Cons(capture_val, capture_pair);
                        value.build(this, site)
                    };
                }

                // Push the capture pair onto the stack, then force param
                let bullet_stack = {
                    let this = &mut *self.arena;
                    Bullet.build(this, site)
                };
                let stack_with_captures = {
                    let this = &mut *self.arena;
                    let stack = Cons(capture_pair, bullet_stack);
                    stack.build(this, site)
                };
                let param_val = {
                    let this = &mut *self.arena;
                    param.build(this, site)
                };

                // Force param with captures on the stack
                let force_param_compu = {
                    let this = &mut *self.arena;
                    let compu = SForce { thunk: param_val, stack: stack_with_captures };
                    compu.build(this, site)
                };

                // Track that we're forcing the fix param
                self.fix_force_visited.insert(force_param_compu);

                // Wrap in a thunk/closure that will do the application when forced
                {
                    let this = &mut *self.arena;
                    let value = Clo {
                        capture: Context(vec![]), // No additional captures needed
                        stack: Bullet,
                        body: force_param_compu,
                    };
                    value.build(this, site)
                }
            }
            | Value::Clo(Clo { capture, stack: _, body }) => {
                let new_body =
                    self.convert_fix_force_rec(body, param, captures, &mut HashSet::new());
                {
                    let this = &mut *self.arena;
                    let value = Clo { capture, stack: Bullet, body: new_body };
                    value.build(this, site)
                }
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let new_body = self.convert_fix_force_in_value(body, param, captures, visited);
                {
                    let this = &mut *self.arena;
                    let value = Ctor(ctor, new_body);
                    value.build(this, site)
                }
            }
            | Value::VCons(Cons(a, b)) => {
                let new_a = self.convert_fix_force_in_value(a, param, captures, visited);
                let new_b = self.convert_fix_force_in_value(b, param, captures, visited);
                {
                    let this = &mut *self.arena;
                    let value = Cons(new_a, new_b);
                    value.build(this, site)
                }
            }
            | other => {
                let this = &mut *self.arena;
                other.build(this, site)
            }
        }
    }

    fn convert_fix_force_in_stack(
        &mut self, stack_id: StackId, param: DefId, captures: &[DefId],
        visited: &mut HashSet<StackId>,
    ) -> StackId {
        if !visited.insert(stack_id) {
            return stack_id;
        }

        // Preserve the site from the original stack
        let site = self.get_stack_site(stack_id);
        let stack = self.arena.stacks[&stack_id].clone();
        match stack {
            | Stack::Kont(Kont { binder, body }) => {
                let new_body =
                    self.convert_fix_force_rec(body, param, captures, &mut HashSet::new());
                {
                    let this = &mut *self.arena;
                    let stack = Kont { binder, body: new_body };
                    stack.build(this, site)
                }
            }
            | Stack::Arg(Cons(val, stack)) => {
                let mut val_visited = HashSet::new();
                let new_val =
                    self.convert_fix_force_in_value(val, param, captures, &mut val_visited);
                let new_stack = self.convert_fix_force_in_stack(stack, param, captures, visited);
                {
                    let this = &mut *self.arena;
                    let stack = Cons(new_val, new_stack);
                    stack.build(this, site)
                }
            }
            | Stack::Tag(Cons(dtor, stack)) => {
                let new_stack = self.convert_fix_force_in_stack(stack, param, captures, visited);
                {
                    let this = &mut *self.arena;
                    let stack = Cons(dtor, new_stack);
                    stack.build(this, site)
                }
            }
            | Stack::Var(_) => {
                let this = &mut *self.arena;
                Bullet.build(this, site)
            }
        }
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
        let mut capture_pair = {
            let this = &mut *self.arena;
            Triv.build(this, site)
        };
        let mut capture_pattern = {
            let this = &mut *self.arena;
            Triv.build(this, None)
        };
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
        {
            clo.body.substitute_in_place(&mut *self.arena, &subst_map);
        }

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
    fn convert_clo_force(&mut self, compu_id: CompuId, force: &SForce) {
        // Skip forces that are visited by fix (these are forces of fix params with captures)
        if self.fix_force_visited.contains(&compu_id) {
            return;
        }

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
