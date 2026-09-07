//! Named patterns and selective field projections.

use super::*;

impl PatternChecker<'_> {
    pub(super) fn check_named_k<'db>(
        &self, tycker: &mut Tycker<'db>, pat: su::Named<FieldName, su::PatId>,
        switch: Switch<AnnId>, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        Ok({
            let su::Named(name, inner) = pat;
            match switch {
                | Switch::Syn => {
                    let checked = self
                        .mk(inner)
                        .tyck_k(tycker, PatternAction::syn().with_skolems(skolems.clone()))?;
                    match checked.annotation {
                        | inner_out @ (PatAnnId::Kind(_) | PatAnnId::Type(_, _)) => {
                            let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                            let inner_interned =
                                crate::query::InternedPatAnn::new(tycker.db, inner_out);
                            let Some(outcome) = crate::query::pat_named_syn_judgment(
                                tycker.db,
                                tycker.data,
                                pat,
                                inner_interned,
                                tycker.site_occurrence(),
                            ) else {
                                unreachable!(
                                    "the type and rejection arms of named pattern judgments are query-produced"
                                )
                            };
                            match outcome {
                                | crate::query::PatNamedSynOutcome::Type {
                                    kind_id,
                                    kind,
                                    named_id,
                                    named,
                                } => {
                                    tycker
                                        .statics
                                        .kinds_pre
                                        .insert_new(kind_id, ss::Fillable::Done(kind));
                                    tycker.statics.tpats.insert_new(named_id, named);
                                    tycker.statics.annotations_tpat.insert_new(named_id, kind_id);
                                    tycker.statics.env_tpat.insert_new(named_id, self.info.clone());
                                    checked.with_annotation(PatAnnId::Type(named_id, kind_id))
                                }
                                | crate::query::PatNamedSynOutcome::Error(error) => {
                                    tycker.err_k(error, std::panic::Location::caller())?
                                }
                            }
                        }
                        | PatAnnId::Value(inner, inner_ty) => {
                            let inner_kind = tycker.statics.type_kind(inner_ty);
                            let vtype = ss::VType.build(tycker, &self.info);
                            Lub::lub_k(vtype, inner_kind, tycker)?;
                            let named_ty = Alloc::alloc(
                                tycker,
                                ss::Label(name.clone(), inner_ty),
                                vtype,
                                &self.info,
                            );
                            let named =
                                Alloc::alloc(tycker, ss::Named(name, inner), named_ty, &self.info);
                            checked.with_annotation(PatAnnId::Value(named, named_ty))
                        }
                    }
                }
                | Switch::Ana(AnnId::Kind(expected)) => {
                    let ss::Kind::Label(ss::Label(expected_name, inner_kind)) =
                        tycker.kind_filled_k(&expected)?.to_owned()
                    else {
                        tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
                    };
                    if name != expected_name {
                        tycker.err_k(
                            TyckError::NamedLabelMismatch {
                                expected: expected_name,
                                found: name.clone(),
                            },
                            std::panic::Location::caller(),
                        )?
                    }
                    let checked = self.mk(inner).tyck_k(
                        tycker,
                        PatternAction::ana(inner_kind.into()).with_skolems(skolems.clone()),
                    )?;
                    let (inner, _) = checked.try_as_type(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    let pat_i = crate::query::InternedPat::new(tycker.db, self.inner);
                    let input = crate::query::InternedPatNamedAna::new(
                        tycker.db,
                        crate::query::PatNamedAnaArm::Kind { name, inner, expected },
                    );
                    let Some(crate::query::PatNamedAnaOutcome::Type { id, pat, kd }) =
                        crate::query::pat_named_ana_judgment(
                            tycker.db,
                            tycker.data,
                            pat_i,
                            input,
                            tycker.site_occurrence(),
                        )
                    else {
                        unreachable!("the kind arm of named pattern judgments is query-produced")
                    };
                    tycker.statics.tpats.insert_new(id, pat);
                    tycker.statics.annotations_tpat.insert_new(id, kd);
                    tycker.statics.env_tpat.insert_new(id, self.info.clone());
                    checked.with_annotation(PatAnnId::Type(id, kd))
                }
                | Switch::Ana(AnnId::Type(expected)) => {
                    let expected_view =
                        expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                    let ss::Type::Label(ss::Label(expected_name, inner_ty)) =
                        tycker.type_filled_k(&expected_view)?.to_owned()
                    else {
                        tycker.err_k(
                            TyckError::TypeExpected {
                                expected: "a named value type".to_string(),
                                found: expected,
                            },
                            std::panic::Location::caller(),
                        )?
                    };
                    if name != expected_name {
                        tycker.err_k(
                            TyckError::NamedLabelMismatch {
                                expected: expected_name,
                                found: name.clone(),
                            },
                            std::panic::Location::caller(),
                        )?
                    }
                    let checked = self.mk(inner).tyck_k(
                        tycker,
                        PatternAction::ana(inner_ty.into()).with_skolems(skolems.clone()),
                    )?;
                    let (inner, _) = checked.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    let pat_i = crate::query::InternedPat::new(tycker.db, self.inner);
                    let input = crate::query::InternedPatNamedAna::new(
                        tycker.db,
                        crate::query::PatNamedAnaArm::Type { name, inner, expected },
                    );
                    let Some(crate::query::PatNamedAnaOutcome::Value { id, pat, ty }) =
                        crate::query::pat_named_ana_judgment(
                            tycker.db,
                            tycker.data,
                            pat_i,
                            input,
                            tycker.site_occurrence(),
                        )
                    else {
                        unreachable!("the type arm of named pattern judgments is query-produced")
                    };
                    tycker.statics.vpats.insert_new(id, pat);
                    tycker.statics.annotations_vpat.insert_new(id, ty);
                    tycker.statics.env_vpat.insert_new(id, self.info.clone());
                    checked.with_annotation(PatAnnId::Value(id, ty))
                }
                | Switch::Ana(AnnId::Set) => {
                    let pat_i = crate::query::InternedPat::new(tycker.db, self.inner);
                    let input = crate::query::InternedPatNamedAna::new(
                        tycker.db,
                        crate::query::PatNamedAnaArm::SortMismatch,
                    );
                    let Some(crate::query::PatNamedAnaOutcome::Error(error)) =
                        crate::query::pat_named_ana_judgment(
                            tycker.db,
                            tycker.data,
                            pat_i,
                            input,
                            tycker.site_occurrence(),
                        )
                    else {
                        unreachable!("the set arm of named pattern judgments is query-produced")
                    };
                    tycker.err_k(error, std::panic::Location::caller())?
                }
            }
        })
    }

    pub(super) fn check_project_k<'db>(
        &self, tycker: &mut Tycker<'db>, field: FieldName, inner: su::PatId, switch: Switch<AnnId>,
        skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => {
                let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                let Some(error) =
                    crate::query::pat_project_syn_judgment(tycker.db, tycker.data, pat)
                else {
                    unreachable!("projection pattern judgments are query-produced")
                };
                tycker.err_k(error, std::panic::Location::caller())?
            }
            | Switch::Ana(AnnId::Kind(expected)) => {
                let candidate = FieldProjectionResolver::r#type(tycker, expected, &field)?;
                FieldProjectionResolver::record_type_origin(tycker, self.inner.into(), &candidate);
                let checked = self.mk(inner).tyck_k(
                    tycker,
                    PatternAction::ana(candidate.projected.into()).with_skolems(skolems.clone()),
                )?;
                let (payload, _) = checked.try_as_type(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let pattern = FieldProjectionResolver::type_pattern(
                    tycker, &self.info, expected, candidate, payload,
                );
                checked.with_annotation(PatAnnId::Type(pattern, expected))
            }
            | Switch::Ana(AnnId::Type(expected)) => {
                let members =
                    ExistentialProjectionPattern::members(tycker, std::iter::once(self.inner))
                        .unwrap();
                ExistentialProjectionPattern::check_k(
                    tycker,
                    &self.info,
                    expected,
                    members,
                    skolems.clone(),
                )?
            }
            | Switch::Ana(AnnId::Set) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        })
    }
}
