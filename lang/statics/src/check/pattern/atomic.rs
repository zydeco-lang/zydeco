//! Pattern annotations, leaves, units, and statically checked views.

use super::*;

impl PatternChecker<'_> {
    pub(super) fn check_ann_k<'db>(
        &self, tycker: &mut Tycker<'db>, pat: su::Ann<su::PatId, su::TermId>,
        switch: Switch<AnnId>, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        Ok({
            let su::Ann { tm, ty } = pat;
            let ty_out_ann = self.mk(ty).tyck_k(tycker, Action::syn())?;
            let ty_tm: AnnId = match ty_out_ann {
                | TermAnnId::Kind(kd) => kd.into(),
                | TermAnnId::Type(ty, _) => ty.into(),
                | TermAnnId::Hole(_) => {
                    // Fixme: I forgor
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
                | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            };
            match switch {
                | Switch::Syn => self
                    .mk(tm)
                    .tyck_k(tycker, PatternAction::ana(ty_tm).with_skolems(skolems.clone()))?,
                | Switch::Ana(ty_ana) => {
                    let ty = Lub::lub_k(ty_tm, ty_ana, tycker)?;

                    self.mk(tm)
                        .tyck_k(tycker, PatternAction::ana(ty).with_skolems(skolems.clone()))?
                }
            }
        })
    }

    pub(super) fn check_hole_k<'db>(
        &self, tycker: &mut Tycker<'db>, pat: su::Hole, switch: Switch<AnnId>,
    ) -> ResultKont<CheckedPattern> {
        Ok({
            let su::Hole = pat;
            match switch {
                | Switch::Syn => {
                    tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                }
                | Switch::Ana(ann) => {
                    let ann = PatternLeaf::Hole.materialize(tycker, &self.info, ann);
                    self.mk(PatternCheck::new(ann))
                }
            }
        })
    }

    pub(super) fn check_var_k<'db>(
        &self, tycker: &mut Tycker<'db>, def: su::DefId, switch: Switch<AnnId>,
    ) -> ResultKont<CheckedPattern> {
        Ok({
            let ann = match switch {
                | Switch::Syn => match tycker.statics.annotations_var.get(&def) {
                    | Some(ann) => ann.to_owned(),
                    | None => PatternVariableStandIn::materialize(tycker, &self.info, self.inner),
                },
                | Switch::Ana(ann) => ann,
            };
            let ann = match ann {
                | AnnId::Set => AnnId::Set,
                | AnnId::Kind(kd) => kd.into(),
                | AnnId::Type(ty) => {
                    let vtype = ss::VType.build(tycker, &self.info);
                    let kd = tycker.statics.type_kind(ty);
                    Lub::lub_k(vtype, kd, tycker)?;
                    ty.into()
                }
            };
            if let Some(ann_) = tycker.statics.annotations_var.insert_or_get(def, ann) {
                let ann = Lub::lub_k(ann_, ann, tycker)?;
                tycker.statics.annotations_var.replace_existing(def, ann);
            }

            let ann = PatternLeaf::Variable(def).materialize(tycker, &self.info, ann);
            self.mk(PatternCheck::new(ann))
        })
    }

    pub(super) fn check_view_k<'db>(
        &self, tycker: &mut Tycker<'db>, function: su::TermId, pattern: su::PatId,
        switch: Switch<AnnId>, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        Ok({
            let function = self.mk(function).tyck_k(tycker, Action::syn())?;
            let (function, function_ty) = function.try_as_value(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            let function_kind = tycker.statics.type_kind(function_ty);
            let function_ty = function_ty.normalize_k(tycker, function_kind)?;
            let function_view = function_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
            let ss::Type::ValPi(pi) = tycker.type_filled_k(&function_view)?.to_owned() else {
                tycker.err_k(
                    TyckError::TypeExpected {
                        expected: "a value function with a runtime parameter".to_string(),
                        found: function_view,
                    },
                    std::panic::Location::caller(),
                )?
            };
            let ss::ValPi { binder, codomain } = *pi;
            let ss::ValPiBinder::Value(parameter) = binder else {
                tycker.err_k(
                    TyckError::TypeExpected {
                        expected: "a value function with a runtime parameter".to_string(),
                        found: function_view,
                    },
                    std::panic::Location::caller(),
                )?
            };
            let domain = match switch {
                | Switch::Syn => parameter.domain,
                | Switch::Ana(AnnId::Type(expected)) => {
                    Lub::lub_k(parameter.domain, expected, tycker)?
                }
                | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            };
            let (pattern_env, codomain, mut opened) = match parameter.witnesses {
                | None => (self.info.clone(), codomain, Vec::new()),
                | Some(witnesses) => {
                    PackageSignature { domain: parameter.domain, witnesses, codomain }
                        .open_codomain_k(tycker, &self.info)?
                }
            };
            let nested = TyEnvT::new(pattern_env, pattern).tyck_k(
                tycker,
                PatternAction::ana(codomain.into()).with_skolems(skolems.clone()),
            )?;
            let (pattern, _) = nested.annotation.try_as_value(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            opened.extend(nested.inner.opened.iter().copied());
            let view_pattern = Alloc::alloc(
                tycker,
                Box::new(ss::ViewPattern { function, pattern }),
                domain,
                &self.info,
            );
            TyEnvT::new(
                nested.info,
                PatternCheck::with_opened(PatAnnId::Value(view_pattern, domain), opened),
            )
        })
    }

    pub(super) fn check_triv_k<'db>(
        &self, tycker: &mut Tycker<'db>, switch: Switch<AnnId>,
    ) -> ResultKont<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => {
                let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                let Some(outcome) = crate::query::pat_triv_syn_judgment(
                    tycker.db,
                    tycker.data,
                    pat,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("trivial pattern judgments are query-produced")
                };
                let crate::query::PatTrivSynOutcome { id, value, ty } = outcome;
                tycker.statics.vpats.insert_new(id, value);
                tycker.statics.annotations_vpat.insert_new(id, ty);
                tycker.statics.env_vpat.insert_new(id, self.info.clone());
                self.mk(PatternCheck::new(PatAnnId::Value(id, ty)))
            }
            | Switch::Ana(AnnId::Type(ana)) => {
                let unit = ss::UnitTy.build(tycker, &self.info);
                let ann = Lub::lub_k(unit, ana, tycker)?;
                let triv = Alloc::alloc(tycker, ss::Triv, ann, &self.info);
                self.mk(PatternCheck::new(PatAnnId::Value(triv, ann)))
            }
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        })
    }
}
