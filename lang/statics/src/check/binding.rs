//! Declaration bindings, recursive groups, and assignment of typed patterns.

use super::*;
use crate::check::judgment::{Action, Tyck};
use crate::check::pattern::{PatternAction, ValuePatternShape};

pub struct Assign<Br, Be>(pub Br, pub Be);
pub struct FixPoint<T>(pub T);

/// Type check one acyclic context binding.
impl<'a> Tyck<'a> for TyEnvT<su::Binding> {
    type Out = TyEnvT<()>;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<TyEnvT<()>> {
        let mut env = self.mk(());
        let su::BindingForm::Definition(su::Definition { binder, bindee }) = self.inner.inner
        else {
            unreachable!()
        };
        let surface_bindee = bindee;
        let (bindee, is_sealed) = match bindee.syntactically_sealed(tycker) {
            | Some(bindee) => (bindee, true),
            | None => (bindee, false),
        };
        // synthesize the bindee
        let out_ann = env.mk(bindee).tyck_k(tycker, Action::syn())?;
        let env = match out_ann {
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) => unreachable!(),
            | TermAnnId::Type(ty, kd) => {
                let bindee = ty;
                let binder = env.mk(binder).tyck_k(tycker, PatternAction::ana(kd.into()))?;
                let (binder, _kd) = binder.as_type();

                if let (Some(def), _) = binder.try_destruct_def(tycker) {
                    let _ = tycker.statics.type_definitions.upsert(def, bindee);
                }

                // seal the type if needed
                let bindee = if is_sealed {
                    let abst: AbstId = tycker.fresh();
                    tycker.statics.absts.insert_new(abst, ());
                    if let (Some(def), _kd) = binder.try_destruct_def(tycker) {
                        tycker.statics.abst_hints.insert_new(abst, def);
                    }
                    tycker.record_seal(abst, ty);
                    Alloc::alloc(tycker, abst, kd, &env.info)
                } else {
                    bindee
                };

                // add the type into the environment
                let TyEnvT { info: new_env, inner: () } =
                    env.mk(Assign(binder, bindee)).tyck_k(tycker, ())?;
                env.info = new_env;
                // should also be added to global if it only depends on global definitions
                match binder.try_destruct_def(tycker) {
                    | (Some(def), _) => {
                        // coctx defines what the bindee is using that is not local
                        if tycker
                            .source_free_variables(&surface_bindee)
                            .iter()
                            .all(|id| tycker.statics.global_defs.get(id).is_some())
                        {
                            tycker.statics.global_defs.ensure(def);
                        }
                    }
                    | (None, _) => {}
                }
                env
            }
            | TermAnnId::Value(bindee, ty) => {
                let binder_elaboration =
                    env.mk(binder).tyck_k(tycker, PatternAction::ana(ty.into()))?;
                ValuePatternShape::require_binding_k(
                    tycker,
                    binder,
                    binder_elaboration.as_value().0,
                )?;
                let (binder, _) = binder_elaboration.as_value();
                // Existential package patterns introduce abstract types whose
                // scope extends over the following term.
                env.info = binder_elaboration.info;
                // should also be added to global if it only depends on global definitions
                match binder.try_destruct_def(tycker) {
                    | (Some(def), _) => {
                        let _ = tycker.statics.value_aliases.upsert(def, bindee);
                        // coctx defines what the bindee is using that is not local
                        if tycker
                            .source_free_variables(&surface_bindee)
                            .iter()
                            .all(|id| tycker.statics.global_defs.get(id).is_some())
                        {
                            tycker.statics.global_defs.ensure(def);
                            // consider adding it to the inlinables as well
                            let _ = tycker.statics.inlinables.upsert(def, bindee);
                        }
                    }
                    | (None, _) => {}
                }
                env
            }
            | TermAnnId::Compu(_, _) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };

        Ok(env)
    }
}

/// Type check a self-recursive or mutually recursive context node.
impl<'a> Tyck<'a> for FixPoint<TyEnvT<Vec<su::Binding>>> {
    type Out = TyEnvT<()>;
    type Action = ();

    fn tyck_inner_k<'f>(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let FixPoint(group_under_env) = self;
        let bindings = &group_under_env.inner;
        let mut env = group_under_env.mk(());

        use std::collections::HashMap;

        let mut binder_map = HashMap::new();
        let mut abst_map = HashMap::new();
        for (binding_index, binding) in bindings.iter().enumerate() {
            let id = binding.id;
            let su::BindingForm::Definition(su::Definition { binder, bindee }) = binding.inner
            else {
                unreachable!("recursive groups contain definitions")
            };
            // the bindee must be sealed
            let Some(bindee) = bindee.syntactically_sealed(tycker) else {
                tycker.err_k(TyckError::MissingSeal, std::panic::Location::caller())?
            };
            // the type definition is self referencing, need to get the annotation
            let Some(syn_ann) = bindee.syntactically_annotated(tycker) else {
                tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
            };
            // try synthesizing the kind
            let ann = env.mk(syn_ann).tyck_k(tycker, Action::syn())?;
            // the binder should be a type; register it before analyzing the bindee
            let kd =
                ann.try_as_kind(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
            let binder = env.mk(binder).tyck_k(tycker, PatternAction::ana(kd.into()))?;
            let (binder, _kd) = binder.as_type();
            binder_map.insert(id, binder);
            // register the def with abstract type
            let (def, kd) = binder.try_destruct_def(tycker);
            if let Some(def) = def {
                let index =
                    crate::query::InternedBindingIndex::new(tycker.db, binding_index as u32);
                let Some((abst, abst_ty, next_abst_ty)) =
                    crate::query::rec_group_abst_judgment_at(tycker.db, tycker.query_site(), index)
                else {
                    unreachable!("recursive-group identities are query-produced")
                };
                tycker.statics.absts.insert_new(abst, ());
                tycker.statics.abst_hints.insert_new(abst, def);
                tycker.statics.types_pre.insert_new(
                    abst_ty,
                    ss::Fillable::Done(ss::Type::Abst(abst)),
                    kd,
                );
                tycker.store_env(abst_ty, &env.info);
                env.info += [(def, abst_ty.into())];
                abst_map.insert(id, (abst, next_abst_ty, kd));
            }
        }
        for binding in bindings {
            let id = binding.id;
            let su::BindingForm::Definition(su::Definition { binder: _, bindee }) = binding.inner
            else {
                unreachable!("recursive groups contain definitions")
            };
            let binder = binder_map[&id];
            // should not be added to global because they are mutually recursive
            // match binder.try_destruct_def(tycker) {
            //     | (Some(def), _) => {
            //         tycker.statics.global_defs.insert(def, ());
            //     }
            //     | (None, _) => {}
            // }
            // remove seal
            let Some(bindee) = bindee.syntactically_sealed(tycker) else { unreachable!() };
            let bindee = env.mk(bindee).tyck_k(tycker, Action::syn())?;
            let (bindee, _kd) = bindee.try_as_type(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            // subst vars in bindee
            let bindee_subst = bindee.subst_env_k(tycker, &env.info)?;
            if let (Some(def), _) = binder.try_destruct_def(tycker) {
                let _ = tycker.statics.type_definitions.upsert(def, bindee_subst);
            }
            // add the types to the seal arena
            let (abst, abst_ty, kd) = abst_map[&id];
            tycker.record_seal(abst, bindee_subst);
            tycker.statics.types_pre.insert_new(
                abst_ty,
                ss::Fillable::Done(ss::Type::Abst(abst)),
                kd,
            );
            tycker.store_env(abst_ty, &env.info);
            // add the type into the environment
            let TyEnvT { info: new_env, inner: () } =
                env.mk(Assign(binder, abst_ty)).tyck_k(tycker, ())?;
            env.info = new_env;
        }
        Ok(env)
    }
}

impl<'a> Tyck<'a> for TyEnvT<Assign<ss::KPatId, ss::KindId>> {
    type Out = TyEnvT<()>;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let Assign(assigner, assignee) = self.inner;
        match tycker.statics.kpats[&assigner] {
            | ss::KindPattern::Hole(_) => Ok(self.mk(())),
            | ss::KindPattern::Var(definition) => {
                let mut env = self.info.clone();
                env += [(definition, assignee.into())];
                Ok(TyEnvT::new(env, ()))
            }
        }
    }
}

impl<'a> Tyck<'a> for TyEnvT<Assign<ss::TPatId, ss::TypeId>> {
    type Out = TyEnvT<()>;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        use ss::TypePattern as TPat;
        let Assign(assigner, assignee) = self.inner;
        let pat = tycker.statics.tpats[&assigner].to_owned();
        match pat {
            | TPat::Hole(_) => Ok(self.mk(())),
            | TPat::Var(def) => {
                // defensive programming: def should be in ctx and should be a kind;
                let def_kd = {
                    let ann = tycker.statics.annotations_var[&def];
                    ann.as_kind()
                };
                // def_kd should correctly be the type of assignee
                let assignee_kd = { tycker.statics.type_kind(assignee) };
                Lub::lub_k(def_kd, assignee_kd, tycker)?;
                let mut env = self.info.clone();
                env += [(def, assignee.into())];
                Ok(TyEnvT { info: env, inner: () })
            }
            | TPat::Named(ss::Named(name, inner)) => {
                let payload_kind = tycker.statics.annotations_tpat[&inner];
                let payload = assignee.project_named(tycker, &name, payload_kind);
                let payload = tycker.err_p_to_k(payload)?;
                self.mk(Assign(inner, payload)).tyck_k(tycker, ())
            }
        }
    }
}
