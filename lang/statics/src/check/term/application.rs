//! Value and computation application and recursive computation introduction.

use super::*;

impl TermChecker<'_> {
    pub(super) fn check_app_k<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::App<su::TermId, su::TermId>,
        switch: Switch<AnnId>, prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
        let su::App(f, a) = term;
        let f_out_ann = self.mk(f).tyck_k(tycker, Action::syn())?;
        match f_out_ann {
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())
            }
            | TermAnnId::Type(f_ty, f_kd) => self.check_type_app_k(tycker, f_ty, f_kd, a, switch),
            | TermAnnId::Value(function, function_ty) => {
                self.check_val_app_k(tycker, function, function_ty, a, switch)
            }
            | TermAnnId::Compu(f_out, f_ty) => {
                self.check_compu_app_k(tycker, f_out, f_ty, a, switch, prepared_environment)
            }
        }
    }

    /// Type application in F omega: the function's kind must be a kind arrow.
    fn check_type_app_k(
        &self, tycker: &mut Tycker<'_>, f_ty: ss::TypeId, f_kd: ss::KindId, a: su::TermId,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
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
        let kd_out = match switch {
            | Switch::Syn => kd_out,
            | Switch::Ana(AnnId::Kind(kd_ana)) => Lub::lub_k(kd_ana, kd_out, tycker)?,
            | Switch::Ana(AnnId::Set | AnnId::Type(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };
        // Preserve function-kinded prefixes and normalize once saturated.
        let body_ty_norm = f_ty.apply_type_argument_k(tycker, a_ty, kd_out)?;
        Ok(TermAnnId::Type(body_ty_norm, kd_out))
    }

    /// Apply a value function, whose classifier's binder decides how the
    /// argument is checked.
    fn check_val_app_k(
        &self, tycker: &mut Tycker<'_>, function: ss::ValueId, function_ty: ss::TypeId,
        a: su::TermId, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        let function_kind = tycker.statics.type_kind(function_ty);
        let function_ty = function_ty.normalize_k(tycker, function_kind)?;
        let function_view = function_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
        let ss::Type::ValPi(pi) = tycker.type_filled_k(&function_view)?.to_owned() else {
            tycker.err_k(
                TyckError::TypeExpected {
                    expected: "a value-function classifier".to_string(),
                    found: function_view,
                },
                std::panic::Location::caller(),
            )?
        };
        let ss::ValPi { binder, codomain } = *pi;
        let (argument, result) = self.check_val_argument_k(tycker, a, binder, codomain)?;
        result.constrain_to_scope_k(tycker, self.info.skolem_scope())?;
        let result = match switch {
            | Switch::Syn => result,
            | Switch::Ana(AnnId::Type(expected)) => Lub::lub_k(expected, result, tycker)?,
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };
        let input = crate::query::InternedAppInput::new(
            tycker.db,
            crate::query::AppShape::Value { function, argument },
            result,
            result,
        );
        let outcome = tycker.judge(self.inner, input, crate::query::app_judgment);
        let crate::query::AppSynOutcome::Value { id, value, ann, reported } = outcome else {
            unreachable!("value applications are query-produced")
        };
        tycker.statics.values.insert_new(id, value);
        tycker.statics.annotations_value.insert_new(id, ann);
        tycker.statics.env_value.insert_new(id, self.info.clone());
        Ok(TermAnnId::Value(id, reported))
    }

    /// Check a value-function argument against the classifier's binder and
    /// return it with the instantiated codomain.
    fn check_val_argument_k(
        &self, tycker: &mut Tycker<'_>, a: su::TermId, binder: ss::ValPiBinder,
        codomain: ss::TypeId,
    ) -> ResultKont<(ss::ValArgument, ss::TypeId)> {
        match binder {
            | ss::ValPiBinder::Type(binder) => {
                let argument =
                    self.mk(a).tyck_k(tycker, Action::ana(binder.domain_kind(tycker).into()))?;
                let (argument, _) = argument.try_as_type(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let payload = binder.pattern.bind_argument_k(tycker, argument)?;
                let result = codomain.subst_abst_k(tycker, (binder.witness, payload))?;
                Ok((ss::ValArgument::Type(argument), result))
            }
            | ss::ValPiBinder::Value(parameter) => {
                let argument = self.mk(a).tyck_k(tycker, Action::ana(parameter.domain.into()))?;
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
                Ok((ss::ValArgument::Value(argument), result))
            }
        }
    }

    /// Apply a computation: a term-term application, a type-polymorphic
    /// application, or a package-dependent application.
    fn check_compu_app_k(
        &self, tycker: &mut Tycker<'_>, f_out: ss::CompuId, f_ty: ss::TypeId, a: su::TermId,
        switch: Switch<AnnId>, prepared_environment: Option<ss::TyEnv>,
    ) -> ResultKont<TermAnnId> {
        let f_kd = tycker.statics.type_kind(f_ty);
        let f_ty = f_ty.normalize_k(tycker, f_kd)?;
        match f_ty.reveal_or_refine_arrow_k(tycker, &self.info)? {
            | ss::Type::Arrow(arrow) => self.check_arrow_app_k(tycker, f_out, arrow, a, switch),
            | ss::Type::Forall(forall) => self.check_forall_app_k(tycker, f_out, forall, a, switch),
            | ss::Type::PackPi(signature) => self
                .mk(PackPiElimination { function: f_out, argument: a, signature: *signature })
                .tyck_k(tycker, Action::forward(switch, prepared_environment.as_ref())),
            | _ => tycker.err_k(
                TyckError::TypeExpected {
                    expected: "one of `_ -> _`, a package-dependent arrow, or \
                               `forall _ . _`"
                        .to_string(),
                    found: f_ty,
                },
                std::panic::Location::caller(),
            ),
        }
    }

    /// A term-term application.
    fn check_arrow_app_k(
        &self, tycker: &mut Tycker<'_>, f_out: ss::CompuId,
        arrow: ss::Arrow<ss::TypeId, ss::TypeId>, a: su::TermId, switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        let ss::Arrow(ty_arg, ty_out) = arrow;
        let a_out_ann = self.mk(a).tyck_k(tycker, Action::ana(ty_arg.into()))?;
        let (a_out, _a_ty) = a_out_ann.try_as_value(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        // check ty_out is the same as the analyzed type
        let ty_out = match switch {
            | Switch::Syn => ty_out,
            | Switch::Ana(AnnId::Type(ty_ana)) => Lub::lub_k(ty_ana, ty_out, tycker)?,
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };
        let shape = crate::query::AppShape::CompuValue { function: f_out, argument: a_out };
        Ok(self.record_compu_app(tycker, shape, ty_out, ty_out))
    }

    /// A type-polymorphic term application.
    fn check_forall_app_k(
        &self, tycker: &mut Tycker<'_>, f_out: ss::CompuId, forall: ss::Forall, a: su::TermId,
        switch: Switch<AnnId>,
    ) -> ResultKont<TermAnnId> {
        let ss::Forall(binder, ty_body) = forall;
        let domain_kind = binder.domain_kind(tycker);
        let a_out_ann = self.mk(a).tyck_k(tycker, Action::ana(domain_kind.into()))?;
        let (a_ty, _a_kd) = a_out_ann.try_as_type(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        let payload = binder.pattern.bind_argument_k(tycker, a_ty)?;
        let body_ty_subst = ty_body.subst_abst_k(tycker, (binder.witness, payload))?;
        let ty_out = match switch {
            | Switch::Syn => body_ty_subst,
            | Switch::Ana(AnnId::Type(ty_ana)) => Lub::lub_k(ty_ana, body_ty_subst, tycker)?,
            | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };
        let shape = crate::query::AppShape::CompuType { function: f_out, argument: a_ty };
        Ok(self.record_compu_app(tycker, shape, ty_out, body_ty_subst))
    }

    /// Record a computation application through the application judgment.
    fn record_compu_app(
        &self, tycker: &mut Tycker<'_>, shape: crate::query::AppShape, ann: ss::TypeId,
        reported: ss::TypeId,
    ) -> TermAnnId {
        let input = crate::query::InternedAppInput::new(tycker.db, shape, ann, reported);
        let outcome = tycker.judge(self.inner, input, crate::query::app_judgment);
        let crate::query::AppSynOutcome::Compu { id, compu, ann, reported } = outcome else {
            unreachable!("computation applications are query-produced")
        };
        tycker.statics.compus.insert_new(id, compu);
        tycker.statics.annotations_compu.insert_new(id, ann);
        tycker.statics.env_compu.insert_new(id, self.info.clone());
        TermAnnId::Compu(id, reported)
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
            ValuePatternShape::require_binding_k(tycker, pat, binder)?;
            let (binder, binder_ty) = {
                let thunk_hole = tycker.thk_hole(&self.info, self.inner);
                let thunk_ty = Lub::lub_k(thunk_hole, binder_ty, tycker)?;
                let ss::Type::App(thunk_app_body_ty) = tycker.type_filled_k(&thunk_ty)? else {
                    unreachable!("a fixpoint binder was checked against a thunk type")
                };
                let ss::App(_thunk_ty, body_ty) = thunk_app_body_ty;
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
            let input = crate::query::InternedFixInput::new(tycker.db, binder, body_out, fix_ty);
            let outcome = tycker.judge(self.inner, input, crate::query::fix_judgment);
            tycker.statics.compus.insert_new(outcome.id, outcome.compu);
            tycker.statics.annotations_compu.insert_new(outcome.id, outcome.ann);
            tycker.statics.env_compu.insert_new(outcome.id, self.info.clone());
            TermAnnId::Compu(outcome.id, outcome.ann)
        })
    }
}
