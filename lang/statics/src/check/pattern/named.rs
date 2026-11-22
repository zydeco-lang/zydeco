//! Named patterns and selective field projections.

use super::*;

impl PatternChecker<'_> {
    pub(super) fn check_named<'db>(
        &self, tycker: &mut Tycker<'db>, pat: su::Named<FieldName, su::PatId>,
        switch: Switch<AnnId>, skolems: PatternSkolems,
    ) -> Result<CheckedPattern> {
        let su::Named(name, inner) = pat;
        match switch {
            | Switch::Syn => self.check_named_syn(tycker, name, inner, skolems),
            | Switch::Ana(AnnId::Kind(expected)) => {
                self.check_named_ana_kind(tycker, name, inner, expected, skolems)
            }
            | Switch::Ana(AnnId::Type(expected)) => {
                self.check_named_ana_type(tycker, name, inner, expected, skolems)
            }
            | Switch::Ana(AnnId::Set) => tycker.err(TyckError::SortMismatch),
        }
    }

    /// Synthesize a named pattern: a static inner pattern names a type
    /// pattern, a value inner pattern a labeled value.
    fn check_named_syn(
        &self, tycker: &mut Tycker<'_>, name: FieldName, inner: su::PatId, skolems: PatternSkolems,
    ) -> Result<CheckedPattern> {
        let checked =
            self.mk(inner).tyck(tycker, PatternAction::syn().with_skolems(skolems.clone()))?;
        match checked.annotation {
            | PatAnnId::Type(inner, inner_kind) => {
                let kind_id = Alloc::alloc(tycker, ss::Label(name, inner_kind), (), &());
                let named_id = Alloc::alloc(tycker, ss::Named(name, inner), kind_id, &());
                Ok(checked.with_annotation(PatAnnId::Type(named_id, kind_id)))
            }
            | PatAnnId::Kind(_) => {
                tycker.err(TyckError::Expressivity("named kind components are not supported"))
            }
            | PatAnnId::Value(inner, inner_ty) => {
                let inner_kind = tycker.statics.type_kind(inner_ty);
                let vtype = ss::VType.build(tycker, &self.info);
                Lub::lub(vtype, inner_kind, tycker)?;
                let named_ty = Alloc::alloc(tycker, ss::Label(name, inner_ty), vtype, &());
                let named = Alloc::alloc(tycker, ss::Named(name, inner), named_ty, &());
                Ok(checked.with_annotation(PatAnnId::Value(named, named_ty)))
            }
        }
    }

    /// Check a named type pattern against a label kind.
    fn check_named_ana_kind(
        &self, tycker: &mut Tycker<'_>, name: FieldName, inner: su::PatId, expected: ss::KindId,
        skolems: PatternSkolems,
    ) -> Result<CheckedPattern> {
        let ss::Kind::Label(ss::Label(expected_name, inner_kind)) =
            tycker.kind_filled(&expected)?.to_owned()
        else {
            tycker.err(TyckError::KindMismatch)?
        };
        if name != expected_name {
            tycker.err(TyckError::NamedLabelMismatch { expected: expected_name, found: name })?
        }
        let checked = self
            .mk(inner)
            .tyck(tycker, PatternAction::ana(inner_kind.into()).with_skolems(skolems.clone()))?;
        let (inner, _) = checked.try_as_type(tycker, TyckError::SortMismatch)?;
        let id = Alloc::alloc(tycker, ss::Named(name, inner), expected, &());
        Ok(checked.with_annotation(PatAnnId::Type(id, expected)))
    }

    /// Check a named value pattern against a named value type.
    fn check_named_ana_type(
        &self, tycker: &mut Tycker<'_>, name: FieldName, inner: su::PatId, expected: ss::TypeId,
        skolems: PatternSkolems,
    ) -> Result<CheckedPattern> {
        let expected_view = expected.unroll(tycker)?.subst_env(tycker, &self.info)?;
        let ss::Type::Label(ss::Label(expected_name, inner_ty)) =
            tycker.type_filled(&expected_view)?.to_owned()
        else {
            tycker.err(TyckError::TypeExpected {
                expected: "a named value type".to_string(),
                found: expected,
            })?
        };
        if name != expected_name {
            tycker.err(TyckError::NamedLabelMismatch { expected: expected_name, found: name })?
        }
        let checked = self
            .mk(inner)
            .tyck(tycker, PatternAction::ana(inner_ty.into()).with_skolems(skolems.clone()))?;
        let (inner, _) = checked.try_as_value(tycker, TyckError::SortMismatch)?;
        let id = Alloc::alloc(tycker, ss::Named(name, inner), expected, &());
        Ok(checked.with_annotation(PatAnnId::Value(id, expected)))
    }

    pub(super) fn check_project<'db>(
        &self, tycker: &mut Tycker<'db>, field: FieldName, inner: su::PatId, switch: Switch<AnnId>,
        skolems: PatternSkolems,
    ) -> Result<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => tycker.err(TyckError::MissingAnnotation)?,
            | Switch::Ana(AnnId::Kind(expected)) => {
                let candidate = FieldProjectionResolver::r#type(tycker, expected, &field)?;
                let checked = self.mk(inner).tyck(
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
                ExistentialProjectionPattern::check(
                    tycker,
                    &self.info,
                    expected,
                    members,
                    skolems.clone(),
                )?
            }
            | Switch::Ana(AnnId::Set) => tycker.err(TyckError::SortMismatch)?,
        })
    }
}
