//! Term checking dispatch, expected-type preparation, and source fact recording.

use super::*;
use crate::check::binding::{Assign, FixPoint};
use crate::check::functions::{
    ComputationPiFormation, PackPiElimination, PackPiIntroduction, PackageSignature,
    PackageWitnessProjectionBuilder, ValuePiFormation, ValuePiInstantiation, ValuePiPatternSkolems,
};
use crate::check::intrinsics::{BuiltinAttachment, ForeignAttachment, InternalTerm};
use crate::check::judgment::{Action, Switch, Tyck, TyckTask};
use crate::check::monadic::MonadicBlockElaboration;
use crate::check::pattern::CheckedPatternExt;
use crate::check::pattern::{PatternAction, PatternSkolems, ValuePatternShape};
use crate::check::projection::{
    DeferredTelescopeExistsMode, DeferredTelescopeType, DeferredTelescopeView,
    FieldProjectionResolver,
};
use crate::check::source::{InferenceRegion, TyckObservation};

mod abstraction;
mod application;
mod atomic;
mod boundary;
mod classifiers;
mod computation;
mod data;
mod package;
mod structure;

#[derive(Deref)]
struct TermChecker<'site> {
    site: &'site TyEnvT<su::TermId>,
}

impl<'a> Tyck<'a> for TyEnvT<su::TermId> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        let switch = action.switch;
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back_mut(TyckTask::Term(self.inner, switch));
            let entity = su::EntityId::Term(self.inner);
            let occurrence = tycker.check_counts.get(&entity).copied().unwrap_or(0);
            let _ = tycker.check_counts.upsert(entity, occurrence + 1);
            tycker.allocator.enter(
                self.inner.key_space().as_u64(),
                self.inner.raw().into_u32(),
                occurrence,
            );
            let result = self.tyck_inner_k(tycker, action);
            tycker.allocator.exit();
            result
        })
    }

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        let Action { mut switch, mut prepared_environment } = action;
        // check if we're analyzing against an unfilled type
        match switch {
            | Switch::Syn => {}
            | Switch::Ana(ana) => match ana {
                | AnnId::Set => {}
                | AnnId::Kind(kd) => match tycker.statics.kinds_pre[&kd].to_owned() {
                    | Fillable::Fill(fill) => {
                        tycker.observe_completion(self.inner, switch);
                        match self.tyck_k(tycker, Action::syn())? {
                            | TermAnnId::Type(ty, kd) => {
                                let kd = fill.fill_k(tycker, kd.into())?.as_kind();
                                return Ok(TermAnnId::Type(ty, kd));
                            }
                            | TermAnnId::Hole(_)
                            | TermAnnId::Kind(_)
                            | TermAnnId::Value(_, _)
                            | TermAnnId::Compu(_, _) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                        }
                    }
                    | _ => {}
                },
                | AnnId::Type(ty) => match tycker.statics.types_pre[&ty].to_owned() {
                    | Fillable::Fill(fill) => {
                        tycker.observe_completion(self.inner, switch);
                        match self.tyck_k(tycker, Action::syn())? {
                            | TermAnnId::Value(v, ty) => {
                                let ty = fill.fill_k(tycker, ty.into())?.as_type();
                                return Ok(TermAnnId::Value(v, ty));
                            }
                            | TermAnnId::Compu(c, ty) => {
                                let ty = fill.fill_k(tycker, ty.into())?.as_type();
                                return Ok(TermAnnId::Compu(c, ty));
                            }
                            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            }
                        }
                    }
                    | _ => {
                        let preparation_is_valid = prepared_environment
                            .as_ref()
                            .is_some_and(|prepared| self.info.is_extension_of(prepared));
                        if !preparation_is_valid {
                            let kd = tycker.statics.type_kind(ty);
                            switch = Switch::Ana(
                                ty.subst_env_k(tycker, &self.info)?.normalize_k(tycker, kd)?.into(),
                            );
                            prepared_environment = Some(self.info.clone());
                        }
                    }
                },
            },
        }
        // the switch should contain no unfilled from here on

        tycker.observe_completion(self.inner, switch);

        use su::Term as Tm;
        let checker = TermChecker { site: self };
        let out_ann = match tycker.scoped.terms[&self.inner].to_owned() {
            | Tm::TypeOf(su::TypeOf(operand)) => {
                checker.check_type_of_k(tycker, operand, switch)?
            }
            | Tm::Meta(term) => {
                checker.check_meta_k(tycker, *term, switch, prepared_environment)?
            }
            | Tm::SourceBoundary(su::SourceBoundary(term)) => {
                checker.check_source_boundary_k(tycker, term, switch)?
            }
            | Tm::SignatureBoundary(su::SignatureBoundary(term)) => {
                checker.check_signature_boundary_k(tycker, term, switch)?
            }
            | Tm::Internal(internal) => {
                InternalTerm(internal, self.inner).tyck_k(tycker, &self.info, switch)?
            }
            | Tm::Sealed(_) => unreachable!(),
            | Tm::Ann(term) => {
                let su::Ann { tm, ty } = term;
                // if the ty is a hole, we should stay in current switch
                match tycker.scoped.terms[&ty] {
                    | Tm::Hole(su::Hole) => {
                        let res = self.mk(tm).tyck_k(
                            tycker,
                            Action::forward(switch, prepared_environment.as_ref()),
                        )?;
                        return Ok(res);
                    }
                    | _ => {}
                }
                let ty_out_ann = self.mk(ty).tyck_k(tycker, Action::syn())?;
                let ty_ann = match ty_out_ann {
                    | TermAnnId::Kind(kd) => kd.into(),
                    | TermAnnId::Type(ty, _kd) => ty.into(),
                    | TermAnnId::Hole(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let ann = match switch {
                    | Switch::Syn => ty_ann,
                    | Switch::Ana(ty_ana) => Lub::lub_k(ty_ann, ty_ana, tycker)?,
                };

                self.mk(tm).tyck_k(tycker, Action::ana_prepared(ann, &self.info))?
            }
            | Tm::Hole(term) => checker.check_hole_k(tycker, term, switch)?,
            | Tm::Var(def) => checker.check_var_k(tycker, def, switch)?,
            | Tm::Named(term) => checker.check_named_k(tycker, term, switch)?,
            | Tm::Label(term) => checker.check_label_k(tycker, term, switch)?,
            | Tm::Triv(su::Triv) => checker.check_triv_k(tycker, switch)?,
            | Tm::Cons(term) => checker.check_cons_k(tycker, term, switch)?,
            | Tm::ValAbs(term) => checker.check_val_abs_k(tycker, term, switch)?,
            | Tm::Abs(term) => checker.check_abs_k(tycker, term, switch)?,
            | Tm::App(term) => checker.check_app_k(tycker, term, switch, prepared_environment)?,
            | Tm::Fix(term) => checker.check_fix_k(tycker, term, switch)?,
            | Tm::ValPi(term) => {
                checker.check_val_pi_k(tycker, term, switch, prepared_environment)?
            }
            | Tm::Pi(term) => checker.check_pi_k(tycker, term, switch)?,
            | Tm::Sigma(term) => checker.check_sigma_k(tycker, term, switch)?,
            | Tm::ManifestExists(term) => checker.check_manifest_exists_k(tycker, *term, switch)?,
            | Tm::Pack(term) => checker.check_pack_k(tycker, *term, switch)?,
            | Tm::Thunk(term) => checker.check_thunk_k(tycker, term, switch)?,
            | Tm::Force(term) => checker.check_force_k(tycker, term, switch)?,
            | Tm::Ret(term) => checker.check_ret_k(tycker, term, switch)?,
            | Tm::Do(term) => checker.check_do_k(tycker, *term, switch, prepared_environment)?,
            | Tm::Let(term) => checker.check_let_k(tycker, *term, switch, prepared_environment)?,
            | Tm::MobileParam(_) | Tm::MobileBind(_) => {
                unreachable!("mobile syntax must be eliminated during name resolution")
            }
            | Tm::Residual(term) => {
                checker.check_residual_k(tycker, term, switch, prepared_environment)?
            }
            | Tm::Block(term) => {
                checker.check_block_k(tycker, term, switch, prepared_environment)?
            }
            | Tm::RecGroup(term) => {
                checker.check_rec_group_k(tycker, term, switch, prepared_environment)?
            }
            | Tm::MoBlock(term) => checker.check_mo_block_k(tycker, *term, switch)?,
            | Tm::Data(term) => checker.check_data_k(tycker, term, switch)?,
            | Tm::CoData(term) => checker.check_co_data_k(tycker, term, switch)?,
            | Tm::Ctor(term) => checker.check_ctor_k(tycker, term, switch)?,
            | Tm::Match(term) => checker.check_match_k(tycker, term, switch)?,
            | Tm::CoMatchClauses(term) => checker.check_co_match_clauses_k(tycker, term, switch)?,
            | Tm::CoMatch(term) => checker.check_co_match_k(tycker, term, switch)?,
            | Tm::Dtor(term) => checker.check_dtor_k(tycker, term, switch)?,
            | Tm::Proj(term) => checker.check_proj_k(tycker, term, switch)?,
            | Tm::Lit(lit) => checker.check_lit_k(tycker, lit, switch)?,
        };

        let member_classifier = match (&tycker.scoped.terms[&self.inner], out_ann) {
            | (Tm::Label(_), TermAnnId::Kind(kind)) => Some(AnnId::Kind(kind)),
            | (Tm::Label(_), TermAnnId::Type(ty, _)) => Some(AnnId::Type(ty)),
            | (Tm::Named(_), TermAnnId::Type(_, kind)) if matches!(switch, Switch::Syn) => {
                Some(AnnId::Kind(kind))
            }
            | (Tm::Named(_), TermAnnId::Value(_, ty)) if matches!(switch, Switch::Syn) => {
                Some(AnnId::Type(ty))
            }
            | _ => None,
        };
        if let Some(classifier) = member_classifier {
            tycker.statics.member_provenance.record(classifier, self.inner);
        }

        if let Some(out) = out_ann.as_term() {
            // Maintain one canonical back mapping for the materialized term.
            // Import boundaries are reference sites, so they retain their own
            // facts below without replacing the provider's source location.
            if !matches!(tycker.scoped.terms[&self.inner], Tm::SourceBoundary(_)) {
                tycker.statics.terms.record(self.inner, out);
            }
            // record the final annotation for editor facts; fixpoint re-checks
            // overwrite the earlier occurrence's entry
            tycker.statics.record_term_annotation(self.inner, out_ann);

            // check if the term is global
            let global = tycker
                .source_free_variables(&self.inner)
                .iter()
                .all(|def| tycker.statics.global_defs.get(def).is_some());

            if global {
                tycker.statics.global_terms.ensure(out);
            }
        }

        Ok(out_ann)
    }
}

#[cfg(test)]
mod tests;
