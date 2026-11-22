//! Constructor, literal, and conjunctive alias patterns.

use super::*;

impl PatternChecker<'_> {
    pub(super) fn check_ctor<'db>(
        &self, tycker: &mut Tycker<'db>, pat: su::Ctor<CtorName, su::PatId>, switch: Switch<AnnId>,
        skolems: PatternSkolems,
    ) -> Result<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => tycker.err(TyckError::MissingAnnotation)?,
            | Switch::Ana(ann) => {
                let AnnId::Type(ann_ty) = ann else { tycker.err(TyckError::SortMismatch)? };
                let ann_ty_unroll = ann_ty.unroll(tycker)?.subst_env(tycker, &self.info)?;
                let &ss::Type::Data(data_id) = &tycker.type_filled(&ann_ty_unroll)? else {
                    tycker.err(TyckError::TypeExpected {
                        expected: "data type definition".to_string(),
                        found: ann_ty_unroll,
                    })?
                };
                let su::Ctor(ctor, args) = pat;
                let Some(arm_ty) = tycker.statics.datas[data_id].get(&ctor) else {
                    tycker.err(TyckError::UnknownDataConstructor(ctor))?
                };
                let args_out_ann = self.mk(args).tyck(
                    tycker,
                    PatternAction::ana(arm_ty.into()).with_skolems(skolems.clone()),
                )?;
                let (args, _) = args_out_ann.as_value();
                let id = Alloc::alloc(tycker, ss::Ctor(ctor, args), ann_ty, &());
                tycker.statics.data_pat_hints.insert_new(id, data_id);
                args_out_ann.with_annotation(PatAnnId::Value(id, ann_ty))
            }
        })
    }

    pub(super) fn check_lit<'db>(
        &self, tycker: &mut Tycker<'db>, literal: su::Literal, switch: Switch<AnnId>,
    ) -> Result<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => tycker.err(TyckError::MissingAnnotation)?,
            | Switch::Ana(ann) => {
                let AnnId::Type(expected) = ann else { tycker.err(TyckError::SortMismatch)? };
                let expected_unroll = expected.unroll(tycker)?.subst_env(tycker, &self.info)?;
                match tycker.primitive_type_of(expected_unroll) {
                    | Some(ss::PrimitiveType::Integer(integer_type)) => {
                        use zydeco_syntax::Literal;
                        let Literal::Integer(i) = literal else {
                            tycker.err(TyckError::Expressivity(
                                "literal patterns support integer literals only",
                            ))?
                        };
                        let value = i.value();
                        let Some(i) = i.with_type(integer_type) else {
                            tycker
                                .err(TyckError::IntegerLiteralOutOfRange { value, integer_type })?
                        };
                        let lit = Alloc::alloc(
                            tycker,
                            ss::ValuePattern::Lit(Literal::Integer(i)),
                            expected,
                            &(),
                        );
                        self.mk(PatternCheck::new(PatAnnId::Value(lit, expected)))
                    }
                    | Some(_) | None => tycker.err(TyckError::TypeExpected {
                        expected: "an integer primitive type".to_string(),
                        found: expected_unroll,
                    })?,
                }
            }
        })
    }

    pub(super) fn check_alias<'db>(
        &self, tycker: &mut Tycker<'db>, patterns: su::ConsN<su::PatId, su::PatId>,
        switch: Switch<AnnId>, skolems: PatternSkolems,
    ) -> Result<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => tycker.err(TyckError::MissingAnnotation)?,
            | Switch::Ana(AnnId::Type(expected)) => {
                let members =
                    ExistentialProjectionPattern::members(tycker, patterns.iter().copied());
                if let Some(members) = members
                    && ExistentialProjectionPattern::applies(tycker, &self.info, expected)?
                {
                    ExistentialProjectionPattern::check(
                        tycker,
                        &self.info,
                        expected,
                        members,
                        skolems.clone(),
                    )?
                } else {
                    let initial = (self.info.clone(), Vec::new(), Vec::new());
                    let (pattern_env, output, opened) = patterns.into_iter().try_fold(
                        initial,
                        |(pattern_env, mut output, mut opened), pattern| -> Result<_> {
                            let checked = TyEnvT::new(pattern_env, pattern).tyck(
                                tycker,
                                PatternAction::ana(expected.into()).with_skolems(skolems.clone()),
                            )?;
                            let TyEnvT { info, inner } = checked;
                            let (pattern, _) =
                                inner.annotation.try_as_value(tycker, TyckError::SortMismatch)?;
                            if !ValuePatternShape::is_irrefutable(tycker, pattern) {
                                tycker.err(TyckError::RefutablePatternAlias)?
                            }
                            output.push(pattern);
                            opened.extend(inner.opened);
                            Ok((info, output, opened))
                        },
                    )?;
                    let alias = ss::Alias(
                        ss::ConsN::from_vec(output).expect("an alias pattern is non-empty"),
                    );
                    let id = Alloc::alloc(tycker, alias, expected, &());
                    TyEnvT::new(
                        pattern_env,
                        PatternCheck::with_opened(PatAnnId::Value(id, expected), opened),
                    )
                }
            }
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err(TyckError::PatternAliasRequiresValue)?
            }
        })
    }
}
