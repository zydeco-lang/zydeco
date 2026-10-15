//! Checking monadic bases and translating checked monadic blocks.

use super::*;
use crate::check::judgment::Action;
use crate::check::source::CheckedTerm;
use crate::traverse::CheckedNode;

struct MonadicBasisElaboration<'a> {
    syntax: &'a su::MonadicBasis,
    env: &'a TyEnv,
}

impl<'a> MonadicBasisElaboration<'a> {
    fn new(syntax: &'a su::MonadicBasis, env: &'a TyEnv) -> Self {
        Self { syntax, env }
    }

    fn check_k(&self, tycker: &mut Tycker<'_>) -> ResultKont<MonadicTypeBasis> {
        let monad = self.definition_k(tycker, self.syntax.monad)?;
        let algebra = self.definition_k(tycker, self.syntax.algebra)?;
        let vtype = ss::VType.build(tycker, self.env);
        let ctype = ss::CType.build(tycker, self.env);
        let monad_constructor = ss::Arrow(vtype, ctype).build(tycker, self.env);
        let monad_kind = ss::Arrow(monad_constructor, ctype).build(tycker, self.env);
        let carrier_constructor = ss::Arrow(ctype, ctype).build(tycker, self.env);
        let algebra_kind =
            ss::Arrow(monad_constructor, carrier_constructor).build(tycker, self.env);
        self.expect_kind_k(tycker, monad, monad_kind)?;
        self.expect_kind_k(tycker, algebra, algebra_kind)?;
        Ok(MonadicTypeBasis { monad, algebra })
    }

    fn definition_k(
        &self, tycker: &mut Tycker<'_>, definition: su::TermId,
    ) -> ResultKont<ss::TypeId> {
        let checked =
            TyEnvT { info: self.env.clone(), inner: definition }.tyck_k(tycker, Action::syn())?;
        let (definition, _) =
            checked.try_as_type(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
        Ok(definition)
    }

    fn expect_kind_k(
        &self, tycker: &mut Tycker<'_>, ty: ss::TypeId, expected: ss::KindId,
    ) -> ResultKont<()> {
        let actual = tycker.statics.type_kind(ty);
        Lub::lub_k(expected, actual, tycker)?;
        Ok(())
    }
}

/// Type-check a monadic payload once and hand its immutable checked handle to
/// the algebra translation.
pub(super) struct MonadicBlockElaboration<'a> {
    pub(super) syntax: &'a su::MoBlock,
    pub(super) environment: &'a TyEnv,
}

impl<'a> MonadicBlockElaboration<'a> {
    pub(super) fn new(syntax: &'a su::MoBlock, environment: &'a TyEnv) -> Self {
        Self { syntax, environment }
    }

    pub(super) fn check_k(&self, tycker: &mut Tycker<'_>) -> ResultKont<TermAnnId> {
        let basis =
            MonadicBasisElaboration::new(&self.syntax.basis, self.environment).check_k(tycker)?;
        let body_environment = TyEnv::monadic_new(tycker, self.environment);
        let body = tycker.synthesize_once_k(self.syntax.body, &body_environment, |tycker| {
            TyEnvT { info: body_environment.clone(), inner: self.syntax.body }
                .tyck_k(tycker, Action::syn())
        })?;
        AlgebraicTranslation::new(body, self.syntax.body, body_environment, basis, self.environment)
            .build_k(tycker)
    }
}

/// Sort-preserving algebra translation of one checked root. Types abstract over
/// the carrier; values and computations also abstract over its dictionary.
/// The checked input is shared; only the translated output is materialized.
struct AlgebraicTranslation<'a> {
    body: CheckedTerm,
    source: su::TermId,
    body_environment: TyEnv,
    basis: MonadicTypeBasis,
    environment: &'a TyEnv,
}

impl<'a> AlgebraicTranslation<'a> {
    fn new(
        body: CheckedTerm, source: su::TermId, body_environment: TyEnv, basis: MonadicTypeBasis,
        environment: &'a TyEnv,
    ) -> Self {
        Self { body, source, body_environment, basis, environment }
    }

    fn build_k(self, tycker: &mut Tycker<'_>) -> ResultKont<TermAnnId> {
        let Self { body, source, body_environment, basis, environment } = self;
        let body = match body.root() {
            | TermAnnId::Type(body, _) => CheckedNode::Type(body),
            | TermAnnId::Value(body, _) => CheckedNode::Value(body),
            | TermAnnId::Compu(body, _) => CheckedNode::Computation(body),
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) => {
                return tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller());
            }
        };

        let monad_ty_kd: ss::KindId = ss::Arrow(ss::VType, ss::CType).build(tycker, environment);
        let monad_ty_var =
            Alloc::alloc(tycker, ss::VarName("M".to_string()), monad_ty_kd.into(), &());
        let abst: ss::AbstId = Alloc::alloc(tycker, monad_ty_var, monad_ty_kd, &());
        let monad_ty = cs::Type(cs::Ann(abst, monad_ty_kd)).build(tycker, environment);
        let ctype = ss::CType.build(tycker, environment);
        let monad_application =
            Alloc::alloc(tycker, ss::App(basis.monad, monad_ty), ctype, environment);
        let monad_impl_ty = cs::Thk(cs::Type(monad_application)).build(tycker, environment);
        let monad_impl_var =
            Alloc::alloc(tycker, ss::VarName("mo".to_string()), monad_impl_ty.into(), &());
        let monad_impl = cs::Value(monad_impl_var).build(tycker, environment);

        use crate::environment::*;
        let menv = MonEnv {
            ty: body_environment,
            subst: SubstEnv::new(),
            subst_abst: SubstAbstEnv::new_sync(),
            structure: StrEnv::new(),
            basis,
            monad_ty,
            monad_impl,
            nominals: Default::default(),
            nominal_dictionary: None,
        };
        use crate::elaborate::monadic::nominal::NominalTranslation;
        let prepared = NominalTranslation::prepare(tycker, menv, source, body);
        let menv = tycker.err_p_to_k(prepared)?;
        let monad_ty_tpat: ss::TPatId =
            Alloc::alloc(tycker, monad_ty_var, monad_ty_kd, environment);
        let monad_binder = ss::TypeBinder { pattern: monad_ty_tpat, witness: abst };
        if let CheckedNode::Type(body) = body {
            // Type translation needs the carrier and nominal families, not runtime algebras.
            let (_, body) = cs::TypeLift { ty: body }.mbuild_k(tycker, menv)?;
            let kind =
                ss::Arrow(monad_ty_kd, tycker.statics.type_kind(body)).build(tycker, environment);
            let body = Alloc::alloc(
                tycker,
                ss::TypeAbstraction { binder: monad_binder, body },
                kind,
                environment,
            );
            return Ok(TermAnnId::Type(body, kind));
        }

        let dictionary = NominalTranslation::dictionary(tycker, menv);
        let (menv, dictionary) = tycker.err_p_to_k(dictionary)?;
        let monad_impl_vpat: ss::VPatId =
            Alloc::alloc(tycker, monad_impl_var, monad_impl_ty, environment);
        let body = match body {
            | CheckedNode::Value(body) => {
                let (_, mut body) = cs::TermLift { tm: body }.mbuild_k(tycker, menv)?;
                let body_ty = tycker.statics.annotations_value[&body];
                if let Some((binder, bindee)) = dictionary {
                    body = Alloc::alloc(
                        tycker,
                        ss::Let { binder, bindee, tail: body },
                        body_ty,
                        environment,
                    );
                }
                let vtype = ss::VType.build(tycker, environment);
                let inner_ty = Alloc::alloc(
                    tycker,
                    ss::ValPi {
                        binder: ss::ValPiBinder::Value(ss::ValueParameter {
                            domain: monad_impl_ty,
                            witnesses: None,
                            witness_projection: ss::PackageWitnessProjection::Ignore,
                        }),
                        codomain: body_ty,
                    },
                    vtype,
                    environment,
                );
                let inner = Alloc::alloc(
                    tycker,
                    ss::Abs(ss::ValBinder::Value(monad_impl_vpat), body),
                    inner_ty,
                    environment,
                );
                let ty = Alloc::alloc(
                    tycker,
                    ss::ValPi { binder: ss::ValPiBinder::Type(monad_binder), codomain: inner_ty },
                    vtype,
                    environment,
                );
                let body = Alloc::alloc(
                    tycker,
                    ss::Abs(ss::ValBinder::Type(monad_ty_tpat), inner),
                    ty,
                    environment,
                );
                return Ok(TermAnnId::Value(body, ty));
            }
            | CheckedNode::Computation(body) => body,
            | CheckedNode::Type(_) | CheckedNode::Pattern(_) => {
                unreachable!("type roots returned above; patterns are not monadic operands")
            }
        };
        let (_, mut body_lift) = cs::TermLift { tm: body }.mbuild_k(tycker, menv)?;
        if let Some((binder, bindee)) = dictionary {
            let ty = tycker.statics.annotations_compu[&body_lift];
            body_lift =
                Alloc::alloc(tycker, ss::Let { binder, bindee, tail: body_lift }, ty, environment);
        }
        let body_lift_ty = cs::TypeOf(body_lift).build(tycker, environment);

        // <monad_impl_to_body_lift> = fn (mo: Thk (Monad M)) => Lift(body)
        let monad_impl_to_body_lift_ty =
            Alloc::alloc(tycker, ss::Arrow(monad_impl_ty, body_lift_ty), ctype, environment);
        let monad_impl_to_body_lift = Alloc::alloc(
            tycker,
            ss::Abs(monad_impl_vpat, body_lift),
            monad_impl_to_body_lift_ty,
            environment,
        );

        // fn (M : VType -> CType) => <monad_impl_to_body_lift>
        let res_body_ty = Alloc::alloc(
            tycker,
            ss::Forall(monad_binder, monad_impl_to_body_lift_ty),
            ctype,
            environment,
        );
        let res_body = Alloc::alloc(
            tycker,
            ss::Abs(monad_ty_tpat, monad_impl_to_body_lift),
            res_body_ty,
            environment,
        );

        Ok(TermAnnId::Compu(res_body, res_body_ty))
    }
}
