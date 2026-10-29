//! Constructor, literal, and conjunctive alias patterns.

use super::*;

impl PatternChecker<'_> {
    pub(super) fn check_ctor_k<'db>(
        &self, tycker: &mut Tycker<'db>, pat: su::Ctor<CtorName, su::PatId>, switch: Switch<AnnId>,
        skolems: PatternSkolems,
    ) -> Result<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => {
                let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                let Some(error) = crate::query::pat_ctor_syn_judgment(tycker.db, tycker.data, pat)
                else {
                    unreachable!("constructor pattern judgments are query-produced")
                };
                tycker.err(error)?
            }
            | Switch::Ana(ann) => {
                let AnnId::Type(ann_ty) = ann else { tycker.err(TyckError::SortMismatch)? };
                let ann_ty_unroll = ann_ty.unroll(tycker)?.subst_env(tycker, &self.info)?;
                let ss::Type::Data(data_id) = &tycker.type_filled(&ann_ty_unroll)? else {
                    tycker.err(TyckError::TypeExpected {
                        expected: "data type definition".to_string(),
                        found: ann_ty_unroll,
                    })?
                };
                let su::Ctor(ctor, args) = pat;
                use std::collections::HashMap;
                let arm_ty = match tycker.statics.datas[data_id]
                    .clone()
                    .into_iter()
                    .collect::<HashMap<_, _>>()
                    .get(&ctor)
                    .cloned()
                {
                    | Some(ty) => ty,
                    | None => tycker.err(TyckError::UnknownDataConstructor(ctor.clone()))?,
                };
                let args_out_ann = self.mk(args).tyck_k(
                    tycker,
                    PatternAction::ana(arm_ty.to_owned().into()).with_skolems(skolems.clone()),
                )?;
                let (args, _) = args_out_ann.as_value();
                let input = crate::query::InternedPatCtorInput::new(
                    tycker.db,
                    ctor.to_owned(),
                    args,
                    ann_ty,
                    data_id.to_owned(),
                );
                let outcome = tycker.judge(self.inner, input, crate::query::pat_ctor_ana_judgment);
                tycker.statics.vpats.insert_new(outcome.id, outcome.pat);
                tycker.statics.annotations_vpat.insert_new(outcome.id, outcome.ann);
                tycker.statics.env_vpat.insert_new(outcome.id, self.info.clone());
                tycker.statics.data_pat_hints.insert_new(outcome.id, data_id.to_owned());
                args_out_ann.with_annotation(PatAnnId::Value(outcome.id, outcome.ann))
            }
        })
    }

    pub(super) fn check_lit_k<'db>(
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
                            &self.info,
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

    pub(super) fn check_alias_k<'db>(
        &self, tycker: &mut Tycker<'db>, patterns: su::ConsN<su::PatId, su::PatId>,
        switch: Switch<AnnId>, skolems: PatternSkolems,
    ) -> Result<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => {
                let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                let Some(error) = crate::query::pat_alias_syn_judgment(tycker.db, tycker.data, pat)
                else {
                    unreachable!("alias pattern judgments are query-produced")
                };
                tycker.err(error)?
            }
            | Switch::Ana(AnnId::Type(expected)) => {
                let members =
                    ExistentialProjectionPattern::members(tycker, patterns.iter().copied());
                if let Some(members) = members
                    && ExistentialProjectionPattern::applies_k(tycker, &self.info, expected)?
                {
                    ExistentialProjectionPattern::check_k(
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
                            let checked = TyEnvT::new(pattern_env, pattern).tyck_k(
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
                    let input =
                        crate::query::InternedPatAliasInput::new(tycker.db, output, expected);
                    let outcome =
                        tycker.judge(self.inner, input, crate::query::pat_alias_ana_judgment);
                    tycker.statics.vpats.insert_new(outcome.id, outcome.pat);
                    tycker.statics.annotations_vpat.insert_new(outcome.id, outcome.ann);
                    tycker.statics.env_vpat.insert_new(outcome.id, self.info.clone());
                    TyEnvT::new(
                        pattern_env,
                        PatternCheck::with_opened(PatAnnId::Value(outcome.id, outcome.ann), opened),
                    )
                }
            }
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err(TyckError::PatternAliasRequiresValue)?
            }
        })
    }
}
