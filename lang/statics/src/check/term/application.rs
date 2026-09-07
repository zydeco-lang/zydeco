//! Value and computation application and recursive computation introduction.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_app_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::App<su::TermId, su::TermId>,
        switch: Switch<AnnId>, prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::App(f, a) = term;
            let f_out_ann = self.mk(f).tyck_k(tycker, Action::syn())?;
            match f_out_ann {
                | TermAnnId::Hole(_) | TermAnnId::Kind(_) => {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                }
                | TermAnnId::Type(f_ty, f_kd) => {
                    // type application in f omega
                    // f_kd should be a kind arrow
                    let ss::Kind::Arrow(kd_arr) = tycker.kind_filled_k(&f_kd)?.to_owned() else {
                        tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
                    };
                    let ss::Arrow(a_kd, kd_out) = kd_arr;
                    let a_out_ann = self.mk(a).tyck_k(tycker, Action::ana(a_kd.into()))?;
                    let (a_ty, _a_kd) = a_out_ann.try_as_type(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    // check kd_out is the same as the analyzed kind
                    let kd_out = {
                        match switch {
                            | Switch::Syn => kd_out,
                            | Switch::Ana(ana) => match ana {
                                | AnnId::Kind(kd_ana) => Lub::lub_k(kd_out, kd_ana, tycker)?,
                                | AnnId::Set | AnnId::Type(_) => tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?,
                            },
                        }
                    };
                    // Preserve function-kinded prefixes and normalize once saturated.
                    let body_ty_norm = f_ty.apply_type_argument_k(tycker, a_ty, kd_out)?;
                    TermAnnId::Type(body_ty_norm, kd_out)
                }
                | TermAnnId::Value(function, function_ty) => {
                    let function_kind = tycker.statics.type_kind(function_ty);
                    let function_ty = function_ty.normalize_k(tycker, function_kind)?;
                    let function_view =
                        function_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                    let ss::Type::ValPi(pi) = tycker.type_filled_k(&function_view)?.to_owned()
                    else {
                        tycker.err_k(
                            TyckError::TypeExpected {
                                expected: "a value-function classifier".to_string(),
                                found: function_view,
                            },
                            std::panic::Location::caller(),
                        )?
                    };
                    let ss::ValPi { binder, codomain } = *pi;
                    let (argument, result) = match binder {
                        | ss::ValPiBinder::Type(binder) => {
                            let argument = self
                                .mk(a)
                                .tyck_k(tycker, Action::ana(binder.domain_kind(tycker).into()))?;
                            let (argument, _) = argument.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let payload = binder.pattern.bind_argument_k(tycker, argument)?;
                            let result =
                                codomain.subst_abst_k(tycker, (binder.witness, payload))?;
                            (ss::ValArgument::Type(argument), result)
                        }
                        | ss::ValPiBinder::Value(parameter) => {
                            let argument =
                                self.mk(a).tyck_k(tycker, Action::ana(parameter.domain.into()))?;
                            let (argument, _) = argument.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let result = match parameter.witnesses {
                                | None => codomain,
                                | Some(witnesses) => self
                                    .mk(ValuePiInstantiation {
                                        signature: PackageSignature {
                                            domain: parameter.domain,
                                            witnesses,
                                            codomain,
                                        },
                                        projection: parameter.witness_projection,
                                        argument,
                                    })
                                    .tyck_k(tycker, ())?,
                            };
                            (ss::ValArgument::Value(argument), result)
                        }
                    };
                    result.constrain_to_scope_k(tycker, self.info.skolem_scope())?;
                    let result = match switch {
                        | Switch::Syn => result,
                        | Switch::Ana(AnnId::Type(expected)) => {
                            Lub::lub_k(result, expected, tycker)?
                        }
                        | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    };
                    let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                    let input = crate::query::InternedAppInput::new(
                        tycker.db,
                        crate::query::AppKind::Value { function, argument },
                        result,
                        result,
                    );
                    let Some(crate::query::AppSynOutcome::Value { id, value, ann, reported }) =
                        crate::query::app_judgment(
                            tycker.db,
                            tycker.data,
                            term,
                            input,
                            tycker.site_occurrence(),
                        )
                    else {
                        unreachable!("value applications are query-produced")
                    };
                    tycker.statics.values.insert_new(id, value);
                    tycker.statics.annotations_value.insert_new(id, ann);
                    tycker.statics.env_value.insert_new(id, self.info.clone());
                    TermAnnId::Value(id, reported)
                }
                | TermAnnId::Compu(f_out, f_ty) => {
                    let f_kd = tycker.statics.type_kind(f_ty);
                    let f_ty = f_ty.normalize_k(tycker, f_kd)?;
                    // either a term-term application or a type-polymorphic term application
                    match f_ty.reveal_or_refine_arrow_k(tycker, &self.info)? {
                        | ss::Type::Arrow(ty) => {
                            // a term-term application
                            let ss::Arrow(ty_arg, ty_out) = ty;
                            let a_out_ann =
                                self.mk(a).tyck_k(tycker, Action::ana(ty_arg.into()))?;
                            let (a_out, _a_ty) = a_out_ann.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            // check ty_out is the same as the analyzed type
                            let ty_out = {
                                match switch {
                                    | Switch::Syn => ty_out,
                                    | Switch::Ana(ana) => match ana {
                                        | AnnId::Type(ty_ana) => {
                                            Lub::lub_k(ty_out, ty_ana, tycker)?
                                        }
                                        | AnnId::Set | AnnId::Kind(_) => tycker.err_k(
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?,
                                    },
                                }
                            };
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedAppInput::new(
                                tycker.db,
                                crate::query::AppKind::CompuValue {
                                    function: f_out,
                                    argument: a_out,
                                },
                                ty_out,
                                ty_out,
                            );
                            let Some(crate::query::AppSynOutcome::Compu {
                                id,
                                compu,
                                ann,
                                reported,
                            }) = crate::query::app_judgment(
                                tycker.db,
                                tycker.data,
                                term,
                                input,
                                tycker.site_occurrence(),
                            )
                            else {
                                unreachable!("computation applications are query-produced")
                            };
                            tycker.statics.compus.insert_new(id, compu);
                            tycker.statics.annotations_compu.insert_new(id, ann);
                            tycker.statics.env_compu.insert_new(id, self.info.clone());
                            TermAnnId::Compu(id, reported)
                        }
                        | ss::Type::Forall(ty) => {
                            // a type-polymorphic term application
                            let ss::Forall(binder, ty_body) = ty;
                            let domain_kind = binder.domain_kind(tycker);
                            let a_out_ann =
                                self.mk(a).tyck_k(tycker, Action::ana(domain_kind.into()))?;
                            let (a_ty, _a_kd) = a_out_ann.try_as_type(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let payload = binder.pattern.bind_argument_k(tycker, a_ty)?;
                            let body_ty_subst =
                                ty_body.subst_abst_k(tycker, (binder.witness, payload))?;
                            let ty_out = {
                                match switch {
                                    | Switch::Syn => body_ty_subst,
                                    | Switch::Ana(ana) => match ana {
                                        | AnnId::Type(ty_ana) => {
                                            Lub::lub_k(body_ty_subst, ty_ana, tycker)?
                                        }
                                        | AnnId::Set | AnnId::Kind(_) => tycker.err_k(
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?,
                                    },
                                }
                            };
                            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
                            let input = crate::query::InternedAppInput::new(
                                tycker.db,
                                crate::query::AppKind::CompuType {
                                    function: f_out,
                                    argument: a_ty,
                                },
                                ty_out,
                                body_ty_subst,
                            );
                            let Some(crate::query::AppSynOutcome::Compu {
                                id,
                                compu,
                                ann,
                                reported,
                            }) = crate::query::app_judgment(
                                tycker.db,
                                tycker.data,
                                term,
                                input,
                                tycker.site_occurrence(),
                            )
                            else {
                                unreachable!(
                                    "polymorphic computation applications are query-produced"
                                )
                            };
                            tycker.statics.compus.insert_new(id, compu);
                            tycker.statics.annotations_compu.insert_new(id, ann);
                            tycker.statics.env_compu.insert_new(id, self.info.clone());
                            TermAnnId::Compu(id, reported)
                        }
                        | ss::Type::PackPi(signature) => self
                            .mk(PackPiElimination {
                                function: f_out,
                                argument: a,
                                signature: *signature,
                            })
                            .tyck_k(
                                tycker,
                                Action::forward(switch, prepared_environment.as_ref()),
                            )?,
                        | _ => tycker.err_k(
                            TyckError::TypeExpected {
                                expected: "one of `_ -> _`, a package-dependent arrow, or \
                                       `forall _ . _`"
                                    .to_string(),
                                found: f_ty,
                            },
                            std::panic::Location::caller(),
                        )?,
                    }
                }
            }
        })
    }

    pub(super) fn check_fix_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::Fix<su::PatId, su::TermId>,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        Ok({
            let su::Fix(pat, body) = term;
            let binder_elaboration = {
                let switch = {
                    match switch {
                        | Switch::Ana(AnnId::Type(ty)) => {
                            let thunk_app_ty: ss::TypeId = cs::Thk(ty).build(tycker, &self.info);
                            Switch::Ana(thunk_app_ty.into())
                        }
                        | _ => switch,
                    }
                };
                self.mk(pat).tyck_k(tycker, PatternAction::switch(switch))?
            };
            let (binder, binder_ty) = binder_elaboration.try_as_value(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            let (binder, binder_ty) = {
                let ss::Type::App(ret_app_body_ty) = tycker.type_filled_k(&binder_ty)? else {
                    unreachable!()
                };
                let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
                (binder, body_ty)
            };
            let body_out_ann = TyEnvT::new(binder_elaboration.info.clone(), body)
                .tyck_k(tycker, Action::ana(binder_ty.into()))?;
            let (body_out, fix_ty) = body_out_ann.try_as_compu(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            binder_elaboration.close_scope_k(tycker, fix_ty)?;
            let term = crate::query::InternedTerm::new(tycker.db, self.inner);
            let input = crate::query::InternedFixInput::new(tycker.db, binder, body_out, fix_ty);
            let Some(outcome) = crate::query::fix_judgment(
                tycker.db,
                tycker.data,
                term,
                input,
                tycker.site_occurrence(),
            ) else {
                unreachable!("fixpoint judgments are query-produced")
            };
            tycker.statics.compus.insert_new(outcome.id, outcome.compu);
            tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
            tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
            TermAnnId::Compu(outcome.id, outcome.ann)
        })
    }
}
