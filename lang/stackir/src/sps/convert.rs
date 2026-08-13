//! Closure conversion.
//!
//! After this pass, there will be no implicit captures.

use super::{arena::*, substitute::*, syntax::*};
use derive_more::{AsMut, AsRef};
use std::{collections::HashMap, convert::Infallible};
use {
    zydeco_statics::{arena::StaticsArena, syntax as ss},
    zydeco_surface::scoped::arena::ScopedArena,
    zydeco_syntax::VarName,
    zydeco_utils::prelude::{CoContext, CompilerPass},
};

/// Perform closure conversion on the stack arena.
#[derive(AsRef, AsMut)]
pub struct ClosureConverter<'a> {
    #[as_ref(StackirArena)]
    #[as_mut(StackirArena)]
    arena: &'a mut StackirArena,
    #[as_ref(ScopedArena)]
    #[as_mut(ScopedArena)]
    scoped: &'a mut ScopedArena,
    _statics: &'a StaticsArena,
}

impl<'a> CompilerPass for ClosureConverter<'a> {
    type Arena = StackirArena;
    type Out = ();
    type Error = Infallible;
    fn run(self) -> Result<Self::Out, Self::Error> {
        self.convert();
        Ok(())
    }
}

impl<'a> ClosureConverter<'a> {
    pub fn new(
        program: &'a mut StackirProgram, scoped: &'a mut ScopedArena, statics: &'a StaticsArena,
    ) -> Self {
        let StackirProgram { arena, root: _ } = program;
        Self { arena, scoped, _statics: statics }
    }

    pub fn convert(mut self) {
        // Transform Fix computations
        let fixes: Vec<_> = (self.arena.inner.compus.iter())
            .filter_map(|(id, compu)| match compu {
                | Computation::Fix(fix) => Some((*id, fix.clone())),
                | _ => None,
            })
            .collect();
        for (compu_id, fix) in fixes {
            self.convert_fix(compu_id, &fix);
        }

        // Transform Clo values (thunks)
        let clos: Vec<_> = (self.arena.inner.values.iter())
            .filter_map(|(id, value)| match value {
                | Value::Closure(clo) => Some((*id, clo.clone())),
                | _ => None,
            })
            .collect();
        for (value_id, clo) in clos {
            self.convert_clo(value_id, &clo);
        }

        // Update Force operations to handle converted closures
        // Find all Force operations and update them to unpack the closure pair
        let forces: Vec<_> = (self.arena.inner.compus.iter())
            .filter_map(|(id, compu)| match compu {
                | Computation::Force(force) => Some((*id, force.clone())),
                | _ => None,
            })
            .collect();
        for (compu_id, force) in forces {
            self.convert_force(compu_id, &force);
        }
    }

    fn alloc_def(&mut self, name: VarName) -> DefId {
        let id = self.arena.admin.fresh();
        self.scoped.insert_def(id, name);
        id
    }

    /// Get the ss::TermId site for a CompuId, if it exists.
    fn get_compu_site(&self, compu_id: CompuId) -> Option<ss::TermId> {
        self.arena.admin.terms.back(&TermId::Compu(compu_id)).copied()
    }

    /// Get the ss::TermId site for a ValueId, if it exists.
    fn get_value_site(&self, value_id: ValueId) -> Option<ss::TermId> {
        self.arena.admin.terms.back(&TermId::Value(value_id)).copied()
    }

    /// Compute free variables in a computation using cocontext from scoped.
    fn free_vars_compu(&self, compu_id: CompuId) -> CoContext<DefId> {
        use super::variables::FreeVars;
        compu_id.free_vars(&self)
    }

    fn build_product_pattern(&mut self, items: Vec<VPatId>) -> VPatId {
        let arity = items.len();
        match ConsN::from_vec(items) {
            | Some(items) => VCons::new(items, ProductLayout { arity }).build(self, None),
            | None => Triv.build(self, None),
        }
    }

    fn build_product_value(&mut self, items: Vec<ValueId>, site: Option<ss::TermId>) -> ValueId {
        let arity = items.len();
        match ConsN::from_vec(items) {
            | Some(items) => VCons::new(items, ProductLayout { arity }).build(self, site),
            | None => Triv.build(self, site),
        }
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
            let new_def = self.alloc_def(VarName(format!("{original_name}#cap")));
            free_var_renames.insert(capture, new_def);
            renamed_captures.push(new_def);
        }

        // 2. Substitute free variables in the body to use freshly bound capture vars,
        //    then replace all occurrences of param with param applied to captures.
        //    In stack style: when param is used, push captures on stack, then use param.
        // Convert HashMap<DefId, DefId> to HashMap<DefId, ValueId> for substitution
        let mut subst_map = SubstVarMap::new();
        for (&old_def, &new_def) in free_var_renames.iter() {
            let new_value_id = new_def.build(self, None);
            subst_map.insert(old_def, new_value_id);
        }
        fix.body.subst_var_in_place(self, &mut subst_map);

        // 3. Wrap body in a let arg to retrieve the flat capture product from the stack.
        let mut capture_patterns = Vec::with_capacity(free_vars.len());
        let mut capture_values = Vec::with_capacity(free_vars.len());
        for &capture in &free_vars {
            let capture_var = *free_var_renames.get(&capture).unwrap();
            capture_patterns.push(capture_var.build(self, None));
            capture_values.push(capture_var.build(self, None));
        }
        let capture_pattern = self.build_product_pattern(capture_patterns);
        let capture_pack = self.build_product_value(capture_values, None);

        // Add a variable that re-packs the captures and the param into a thunk pair
        let param_value: ValueId = fix.param.build(self, site);
        let closure_pair =
            VCons::new(ConsN(vec![capture_pack], param_value), ProductLayout { arity: 2 })
                .build(self, site);
        let closure_def = {
            let original_name = self.scoped.defs[&fix.param].clone();
            self.alloc_def(VarName(format!("{original_name}#clo")))
        };
        let closure_vpat = closure_def.build(self, None);
        let transformed_body = {
            // Substitute the closure def into the transformed body to replace fix.param
            let closure_def_value: ValueId = closure_def.build(self, site);
            let mut subst_map = SubstVarMap::new();
            subst_map.insert(fix.param, closure_def_value);
            fix.body.subst_var_in_place(self, &mut subst_map);
            fix.body
        };
        // LetValue the closure pair to a closure definition
        let transformed_let_body =
            Let { binder: closure_vpat, bindee: closure_pair, tail: transformed_body }
                .build(self, site);
        // Use a single LetArg to extract all captures from the stack
        let capture_stack = Bullet.build(self, site);
        let transformed_arg_body = Let {
            binder: Cons(capture_pattern, Bullet),
            bindee: capture_stack,
            tail: transformed_let_body,
        }
        .build(self, site);

        // 4. Push the capture list onto the stack first, then run the fix.
        let capture_values: Vec<ValueId> = free_vars
            .iter()
            .map(|&capture| {
                let value: ValueId = capture.build(self, site);
                value
            })
            .collect();
        let capture_pair = self.build_product_value(capture_values, site);
        // Push the capture pair onto the stack
        let bullet_stack = Bullet.build(self, site);
        let capture_stack: StackId = Cons(capture_pair, bullet_stack).build(self, site);
        // Create the Fix computation
        let fix_compu = SFix { param: fix.param, body: transformed_arg_body }.build(self, site);
        // Wrap the Fix in a LetStack that pushes captures, then runs the Fix
        // Update the Fix in place with the wrapped computation
        self.arena.inner.compus.replace_existing(
            old_compu_id,
            Computation::Join(LetJoin::Stack(Let {
                binder: Bullet,
                bindee: capture_stack,
                tail: fix_compu,
            })),
        );
    }

    /// Convert a Clo (thunk) to explicit closure form.
    fn convert_clo(&mut self, old_value_id: ValueId, clo: &Closure) {
        // Preserve the site from the original value
        let site = self.get_value_site(old_value_id);

        // 1. Capture the environment (free variables in body)
        let free_vars: Vec<DefId> = self.free_vars_compu(clo.body).into_iter().collect();
        let mut free_var_renames = HashMap::new();

        // 2. Make the closure a pair of (capture list, body function).
        let mut capture_values = Vec::with_capacity(free_vars.len());
        let mut capture_patterns = Vec::with_capacity(free_vars.len());
        for &capture in &free_vars {
            let VarName(original_name) = self.scoped.defs[&capture].clone();
            let new_def = self.alloc_def(VarName(format!("{original_name}#cap")));
            free_var_renames.insert(capture, new_def);

            capture_values.push(capture.build(self, site));
            capture_patterns.push(new_def.build(self, None));
        }
        let capture_pair = self.build_product_value(capture_values, site);
        let capture_pattern = self.build_product_pattern(capture_patterns);

        // Substitute free variables in the closure body to refer to the freshly
        // bound capture variables.
        // Convert HashMap<DefId, DefId> to HashMap<DefId, ValueId> for substitution
        let mut subst_map = SubstVarMap::new();
        for (&old_def, &new_def) in free_var_renames.iter() {
            let new_value_id = new_def.build(self, None);
            subst_map.insert(old_def, new_value_id);
        }
        clo.body.subst_var_in_place(self, &mut subst_map);

        // Use a single LetArg to extract all captures from the stack
        let capture_stack = Bullet.build(self, site);
        let transformed_body =
            Let { binder: Cons(capture_pattern, Bullet), bindee: capture_stack, tail: clo.body }
                .build(self, site);

        // The body is already a computation that can be wrapped in a closure
        // We'll store it as a closure that takes the captures as argument
        // The pair will be: (capture_values, body_closure)
        // where body_closure is a closure whose body is the original body
        let body_closure = Closure { stack: Bullet, body: transformed_body }.build(self, site);

        // Update the value in place with the pair: (captures, body_closure)
        self.arena.inner.values.replace_existing_with(
            old_value_id,
            VCons::new(ConsN(vec![capture_pair], body_closure), ProductLayout { arity: 2 }),
        );
    }

    /// Convert a Force computation to handle converted closures.
    fn convert_force(&mut self, compu_id: CompuId, force: &SForce) {
        // Always destructure the thunk as a pair at runtime using LetValue
        // The thunk should be a pair (capture_pair, body_closure) from converted closures
        let site = self.get_compu_site(compu_id);

        // Create fresh DefIds for the pattern binders
        let capture_pair_def = self.alloc_def(VarName("__env__".into()));
        let body_closure_def = self.alloc_def(VarName("__code__".into()));

        // Create Var patterns to bind the destructured values
        let capture_pair_vpat = capture_pair_def.build(self, None);
        let body_closure_vpat = body_closure_def.build(self, None);
        let pair_pattern = VCons::new(
            ConsN(vec![capture_pair_vpat], body_closure_vpat),
            ProductLayout { arity: 2 },
        )
        .build(self, None);

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
        self.arena.inner.compus.replace_existing(
            compu_id,
            Computation::Join(LetJoin::Value(Let {
                binder: pair_pattern,
                bindee: force.thunk,
                tail: force_body,
            })),
        );
    }
}
