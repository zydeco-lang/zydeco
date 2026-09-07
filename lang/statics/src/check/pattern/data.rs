//! Constructor, literal, and conjunctive alias patterns.

use super::*;

impl PatternChecker<'_> {
    pub(super) fn check_ctor_k<'db>(
        &self, tycker: &mut Tycker<'db>, pat: su::Ctor<CtorName, su::PatId>, switch: Switch<AnnId>,
        skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => {
                let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                let Some(error) = crate::query::pat_ctor_syn_judgment(tycker.db, tycker.data, pat)
                else {
                    unreachable!("constructor pattern judgments are query-produced")
                };
                tycker.err_k(error, std::panic::Location::caller())?
            }
            | Switch::Ana(ann) => {
                let AnnId::Type(ann_ty) = ann else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ann_ty_unroll = ann_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                let ss::Type::Data(data_id) = &tycker.type_filled_k(&ann_ty_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "data type definition".to_string(),
                            found: ann_ty_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
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
                    | None => tycker.err_k(
                        TyckError::UnknownDataConstructor(ctor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                let args_out_ann = self.mk(args).tyck_k(
                    tycker,
                    PatternAction::ana(arm_ty.to_owned().into()).with_skolems(skolems.clone()),
                )?;
                let (args, _) = args_out_ann.as_value();
                let pat_i = crate::query::InternedPat::new(tycker.db, self.inner);
                let input = crate::query::InternedPatCtorInput::new(
                    tycker.db,
                    ctor.to_owned(),
                    args,
                    ann_ty,
                    data_id.to_owned(),
                );
                let Some(outcome) = crate::query::pat_ctor_ana_judgment(
                    tycker.db,
                    tycker.data,
                    pat_i,
                    input,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("constructor pattern judgments are query-produced")
                };
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
    ) -> ResultKont<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => {
                tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
            }
            | Switch::Ana(ann) => {
                let AnnId::Type(expected) = ann else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let expected_unroll = expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                match tycker.primitive_type_of(expected_unroll) {
                    | Some(ss::PrimitiveType::Integer(integer_type)) => {
                        use zydeco_syntax::Literal;
                        let Literal::Integer(i) = literal else {
                            tycker.err_k(
                                TyckError::Expressivity(
                                    "literal patterns support integer literals only",
                                ),
                                std::panic::Location::caller(),
                            )?
                        };
                        let value = i.value();
                        let Some(i) = i.with_type(integer_type) else {
                            tycker.err_k(
                                TyckError::IntegerLiteralOutOfRange { value, integer_type },
                                std::panic::Location::caller(),
                            )?
                        };
                        let lit = Alloc::alloc(
                            tycker,
                            ss::ValuePattern::Lit(Literal::Integer(i)),
                            expected,
                            &self.info,
                        );
                        self.mk(PatternCheck::new(PatAnnId::Value(lit, expected)))
                    }
                    | Some(_) | None => tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "an integer primitive type".to_string(),
                            found: expected_unroll,
                        },
                        std::panic::Location::caller(),
                    )?,
                }
            }
        })
    }

    pub(super) fn check_alias_k<'db>(
        &self, tycker: &mut Tycker<'db>, patterns: su::ConsN<su::PatId, su::PatId>,
        switch: Switch<AnnId>, skolems: PatternSkolems,
    ) -> ResultKont<CheckedPattern> {
        Ok(match switch {
            | Switch::Syn => {
                let pat = crate::query::InternedPat::new(tycker.db, self.inner);
                let Some(error) = crate::query::pat_alias_syn_judgment(tycker.db, tycker.data, pat)
                else {
                    unreachable!("alias pattern judgments are query-produced")
                };
                tycker.err_k(error, std::panic::Location::caller())?
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
                        |(pattern_env, mut output, mut opened), pattern| -> ResultKont<_> {
                            let checked = TyEnvT::new(pattern_env, pattern).tyck_k(
                                tycker,
                                PatternAction::ana(expected.into()).with_skolems(skolems.clone()),
                            )?;
                            let TyEnvT { info, inner } = checked;
                            let (pattern, _) = inner.annotation.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            if !ValuePatternShape::is_irrefutable(tycker, pattern) {
                                tycker.err_k(
                                    TyckError::RefutablePatternAlias,
                                    std::panic::Location::caller(),
                                )?
                            }
                            output.push(pattern);
                            opened.extend(inner.opened);
                            Ok((info, output, opened))
                        },
                    )?;
                    let pat_i = crate::query::InternedPat::new(tycker.db, self.inner);
                    let input =
                        crate::query::InternedPatAliasInput::new(tycker.db, output, expected);
                    let Some(outcome) = crate::query::pat_alias_ana_judgment(
                        tycker.db,
                        tycker.data,
                        pat_i,
                        input,
                        tycker.site_occurrence(),
                    ) else {
                        unreachable!("alias pattern judgments are query-produced")
                    };
                    tycker.statics.vpats.insert_new(outcome.id, outcome.pat);
                    tycker.statics.annotations_vpat.insert_new(outcome.id, outcome.ann);
                    tycker.statics.env_vpat.insert_new(outcome.id, self.info.clone());
                    TyEnvT::new(
                        pattern_env,
                        PatternCheck::with_opened(PatAnnId::Value(outcome.id, outcome.ann), opened),
                    )
                }
            }
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => tycker
                .err_k(TyckError::PatternAliasRequiresValue, std::panic::Location::caller())?,
        })
    }
}
