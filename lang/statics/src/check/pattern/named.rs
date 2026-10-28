//! Named patterns and selective field projections.

use super::*;

impl PatternChecker<'_> {
    pub(super) fn check_named_k<'db>(
        &self, tycker: &mut Tycker<'db>, pat: su::Named<FieldName, su::PatId>,
        switch: Switch<AnnId>, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let su::Named(name, inner) = pat;
        match switch {
            | Switch::Syn => self.check_named_syn_k(tycker, name, inner, skolems),
            | Switch::Ana(AnnId::Kind(expected)) => {
                self.check_named_ana_kind_k(tycker, name, inner, expected, skolems)
            }
            | Switch::Ana(AnnId::Type(expected)) => {
                self.check_named_ana_type_k(tycker, name, inner, expected, skolems)
            }
            | Switch::Ana(AnnId::Set) => {
                let input = crate::query::InternedPatNamedAna::new(
                    tycker.db,
                    crate::query::PatNamedAnaArm::SortMismatch,
                );
                let outcome = tycker.judge(self.inner, input, crate::query::pat_named_ana_judgment);
                let crate::query::PatNamedAnaOutcome::Error(error) = outcome else {
                    unreachable!("the set arm of named pattern judgments is query-produced")
                };
                tycker.err_k(error)
            }
        }
    }

    /// Synthesize a named pattern: a static inner pattern names a type
    /// pattern through the judgment, a value inner pattern a labeled value.
    fn check_named_syn_k(
        &self, tycker: &mut Tycker<'_>, name: FieldName, inner: su::PatId, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let checked =
            self.mk(inner).tyck_k(tycker, PatternAction::syn().with_skolems(skolems.clone()))?;
        match checked.annotation {
            | inner_out @ (PatAnnId::Kind(_) | PatAnnId::Type(_, _)) => {
                let inner_interned = crate::query::InternedPatAnn::new(tycker.db, inner_out);
                match tycker.judge(self.inner, inner_interned, crate::query::pat_named_syn_judgment)
                {
                    | crate::query::PatNamedSynOutcome::Type { kind_id, kind, named_id, named } => {
                        tycker.statics.kinds_pre.insert_new(kind_id, ss::Fillable::Done(kind));
                        tycker.statics.tpats.insert_new(named_id, named);
                        tycker.statics.annotations_tpat.insert_new(named_id, kind_id);
                        tycker.statics.env_tpat.insert_new(named_id, self.info.clone());
                        Ok(checked.with_annotation(PatAnnId::Type(named_id, kind_id)))
                    }
                    | crate::query::PatNamedSynOutcome::Error(error) => tycker.err_k(error),
                }
            }
            | PatAnnId::Value(inner, inner_ty) => {
                let inner_kind = tycker.statics.type_kind(inner_ty);
                let vtype = ss::VType.build(tycker, &self.info);
                Lub::lub_k(vtype, inner_kind, tycker)?;
                let named_ty =
                    Alloc::alloc(tycker, ss::Label(name.clone(), inner_ty), vtype, &self.info);
                let named = Alloc::alloc(tycker, ss::Named(name, inner), named_ty, &self.info);
                Ok(checked.with_annotation(PatAnnId::Value(named, named_ty)))
            }
        }
    }

    /// Check a named type pattern against a label kind.
    fn check_named_ana_kind_k(
        &self, tycker: &mut Tycker<'_>, name: FieldName, inner: su::PatId, expected: ss::KindId,
        skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let ss::Kind::Label(ss::Label(expected_name, inner_kind)) =
            tycker.kind_filled_k(&expected)?.to_owned()
        else {
            tycker.err_k(TyckError::KindMismatch)?
        };
        if name != expected_name {
            tycker.err_k(TyckError::NamedLabelMismatch {
                expected: expected_name,
                found: name.clone(),
            })?
        }
        let checked = self
            .mk(inner)
            .tyck_k(tycker, PatternAction::ana(inner_kind.into()).with_skolems(skolems.clone()))?;
        let (inner, _) = checked.try_as_type(tycker, TyckError::SortMismatch)?;
        let input = crate::query::InternedPatNamedAna::new(
            tycker.db,
            crate::query::PatNamedAnaArm::Kind { name, inner, expected },
        );
        let outcome = tycker.judge(self.inner, input, crate::query::pat_named_ana_judgment);
        let crate::query::PatNamedAnaOutcome::Type { id, pat, kd } = outcome else {
            unreachable!("the kind arm of named pattern judgments is query-produced")
        };
        tycker.statics.tpats.insert_new(id, pat);
        tycker.statics.annotations_tpat.insert_new(id, kd);
        tycker.statics.env_tpat.insert_new(id, self.info.clone());
        Ok(checked.with_annotation(PatAnnId::Type(id, kd)))
    }

    /// Check a named value pattern against a named value type.
    fn check_named_ana_type_k(
        &self, tycker: &mut Tycker<'_>, name: FieldName, inner: su::PatId, expected: ss::TypeId,
        skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        let expected_view = expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
        let ss::Type::Label(ss::Label(expected_name, inner_ty)) =
            tycker.type_filled_k(&expected_view)?.to_owned()
        else {
            tycker.err_k(TyckError::TypeExpected {
                expected: "a named value type".to_string(),
                found: expected,
            })?
        };
        if name != expected_name {
            tycker.err_k(TyckError::NamedLabelMismatch {
                expected: expected_name,
                found: name.clone(),
            })?
        }
        let checked = self
            .mk(inner)
            .tyck_k(tycker, PatternAction::ana(inner_ty.into()).with_skolems(skolems.clone()))?;
        let (inner, _) = checked.try_as_value(tycker, TyckError::SortMismatch)?;
        let input = crate::query::InternedPatNamedAna::new(
            tycker.db,
            crate::query::PatNamedAnaArm::Type { name, inner, expected },
        );
        let outcome = tycker.judge(self.inner, input, crate::query::pat_named_ana_judgment);
        let crate::query::PatNamedAnaOutcome::Value { id, pat, ty } = outcome else {
            unreachable!("the type arm of named pattern judgments is query-produced")
        };
        tycker.statics.vpats.insert_new(id, pat);
        tycker.statics.annotations_vpat.insert_new(id, ty);
        tycker.statics.env_vpat.insert_new(id, self.info.clone());
        Ok(checked.with_annotation(PatAnnId::Value(id, ty)))
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
                tycker.err_k(error)?
            }
            | Switch::Ana(AnnId::Kind(expected)) => {
                let candidate = FieldProjectionResolver::r#type(tycker, expected, &field)?;
                let checked = self.mk(inner).tyck_k(
                    tycker,
                    PatternAction::ana(candidate.projected.into()).with_skolems(skolems.clone()),
                )?;
                let (payload, _) = checked.try_as_type(tycker, TyckError::SortMismatch)?;
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
            | Switch::Ana(AnnId::Set) => tycker.err_k(TyckError::SortMismatch)?,
        })
    }
}
