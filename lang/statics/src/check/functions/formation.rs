//! Form value and computation function classifiers from checked binder scopes.

use super::*;

impl<'a> Tyck<'a> for TyEnvT<ValuePiFormation> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_inner_k(
        &self, tycker: &mut Tycker<'a>, Action { switch, .. }: Self::Action,
    ) -> ResultKont<Self::Out> {
        let ValuePiFormation { binder, codomain } = &self.inner;
        let vtype = ss::VType.build(tycker, &self.info);
        match switch {
            | Switch::Syn => {}
            | Switch::Ana(AnnId::Kind(expected)) => {
                Lub::lub_k(vtype, expected, tycker)?;
            }
            | Switch::Ana(AnnId::Set | AnnId::Type(_)) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        }

        let pi = match binder.inner.annotation {
            | PatAnnId::Kind(_) => tycker.err_k(
                TyckError::Expressivity("kind quantification is not supported"),
                std::panic::Location::caller(),
            )?,
            | PatAnnId::Type(pattern, _) => {
                let witness = Alloc::alloc(tycker, pattern, (), &());
                let payload_kind = tycker.statics.annotations_abst[&witness];
                let payload = Alloc::alloc(tycker, witness, payload_kind, &self.info);
                let argument = pattern.introduce_payload(tycker, payload);
                let argument = tycker.err_p_to_k(argument)?;
                let body_env = TyEnvT::new(binder.info.clone(), Assign(pattern, argument))
                    .tyck_k(tycker, ())?
                    .info;
                let codomain =
                    TyEnvT::new(body_env, *codomain).tyck_k(tycker, Action::ana(vtype.into()))?;
                let (codomain, _) = codomain.try_as_type(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                ss::ValPi {
                    binder: ss::ValPiBinder::Type(ss::TypeBinder { pattern, witness }),
                    codomain,
                }
            }
            | PatAnnId::Value(pattern, domain) => {
                if !ValuePatternShape::is_irrefutable(tycker, pattern) {
                    tycker.err_k(
                        TyckError::Expressivity(
                            "value-function parameters must be irrefutable patterns",
                        ),
                        std::panic::Location::caller(),
                    )?
                }
                let domain_kind = tycker.statics.type_kind(domain);
                Lub::lub_k(vtype, domain_kind, tycker)?;
                let codomain = TyEnvT::new(binder.info.clone(), *codomain)
                    .tyck_k(tycker, Action::ana(vtype.into()))?;
                let (codomain, _) = codomain.try_as_type(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let witnesses = binder.package_telescope_k(tycker)?;
                let witness_projection = PackageWitnessProjectionBuilder::build(pattern, tycker);
                let projected_arity =
                    PackageWitnessProjectionBuilder::abstract_arity(&witness_projection);
                let witness_arity = witnesses.as_ref().map_or(0, ss::PackTelescope::len);
                if projected_arity != witness_arity {
                    tycker.err_k(
                        TyckError::PackageWitnessArityMismatch {
                            expected: witness_arity,
                            found: projected_arity,
                        },
                        std::panic::Location::caller(),
                    )?
                }
                if witnesses.is_none() {
                    binder.close_scope_k(tycker, codomain)?;
                }
                if let Some(witnesses) = &witnesses {
                    tycker.validate_builtin_signature_k(&ss::PackPi {
                        domain,
                        witnesses: witnesses.clone(),
                        codomain,
                    })?;
                }
                ss::ValPi {
                    binder: ss::ValPiBinder::Value(ss::ValueParameter {
                        domain,
                        witnesses,
                        witness_projection,
                    }),
                    codomain,
                }
            }
        };
        let pi = Alloc::alloc(tycker, pi, vtype, &self.info);
        Ok(TermAnnId::Type(pi, vtype))
    }
}

impl<'a> Tyck<'a> for TyEnvT<ComputationPiFormation> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        let ComputationPiFormation { binder, codomain } = &self.inner;
        let (_, domain) =
            binder.try_as_value(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;

        let domain_kind = tycker.statics.type_kind(domain);
        let vtype = ss::VType.build(tycker, &self.info);
        Lub::lub_k(vtype, domain_kind, tycker)?;

        // Elaboration may inspect a value through `typeof`. Only the resulting
        // static type participates in the arrow; it cannot retain value terms.
        let codomain = TyEnvT::new(binder.info.clone(), *codomain).tyck_k(tycker, action)?;
        let (codomain, codomain_kind) = codomain.try_as_type(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;
        let pi = match tycker.kind_filled_k(&codomain_kind)?.to_owned() {
            | ss::Kind::VType(_) => tycker.err_k(
                TyckError::Expressivity(
                    "value-returning computation arrows are not supported; use `val pi`",
                ),
                std::panic::Location::caller(),
            )?,
            | ss::Kind::CType(_) => match binder.package_telescope_k(tycker)? {
                | None => {
                    binder.close_scope_k(tycker, codomain)?;
                    Alloc::alloc(tycker, ss::Arrow(domain, codomain), codomain_kind, &self.info)
                }
                | Some(witnesses) => {
                    let signature = ss::PackPi { domain, witnesses, codomain };
                    tycker.validate_builtin_signature_k(&signature)?;
                    Alloc::alloc(tycker, signature, codomain_kind, &self.info)
                }
            },
            | ss::Kind::Arrow(_) | ss::Kind::Label(_) => {
                tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
            }
        };
        Ok(TermAnnId::Type(pi, codomain_kind))
    }
}
