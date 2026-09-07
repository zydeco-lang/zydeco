//! Checking monadic bases and translating checked monadic blocks.

use super::*;
use crate::check::judgment::Action;
use crate::check::source::CheckedTerm;

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
        AlgebraicTranslation::new(body, body_environment, basis, self.environment).build_k(tycker)
    }
}

/// Algebra translation from one checked computation root to its monad-polymorphic
/// checked computation. The input handle is shared; only the translated output
/// is newly materialized.
struct AlgebraicTranslation<'a> {
    body: CheckedTerm,
    body_environment: TyEnv,
    basis: MonadicTypeBasis,
    environment: &'a TyEnv,
}

impl<'a> AlgebraicTranslation<'a> {
    fn new(
        body: CheckedTerm, body_environment: TyEnv, basis: MonadicTypeBasis, environment: &'a TyEnv,
    ) -> Self {
        Self { body, body_environment, basis, environment }
    }

    fn build_k(self, tycker: &mut Tycker<'_>) -> ResultKont<TermAnnId> {
        let Self { body, body_environment, basis, environment } = self;
        let (body, _body_ty) = body.root().try_as_compu(
            tycker,
            TyckError::SortMismatch,
            std::panic::Location::caller(),
        )?;

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
        let (_menv, body_lift) = cs::TermLift { tm: body }.mbuild_k(
            tycker,
            MonEnv {
                ty: body_environment,
                subst: SubstEnv::new(),
                subst_abst: SubstAbstEnv::new_sync(),
                structure: StrEnv::new(),
                basis,
                monad_ty,
                monad_impl,
            },
        )?;
        let body_lift_ty = cs::TypeOf(body_lift).build(tycker, environment);

        // <monad_impl_to_body_lift> = fn (mo: Thk (Monad M)) => Lift(body)
        let monad_impl_vpat: ss::VPatId =
            Alloc::alloc(tycker, monad_impl_var, monad_impl_ty, environment);
        let monad_impl_to_body_lift_ty =
            Alloc::alloc(tycker, ss::Arrow(monad_impl_ty, body_lift_ty), ctype, environment);
        let monad_impl_to_body_lift = Alloc::alloc(
            tycker,
            ss::Abs(monad_impl_vpat, body_lift),
            monad_impl_to_body_lift_ty,
            environment,
        );

        // fn (M : VType -> CType) => <monad_impl_to_body_lift>
        let monad_ty_tpat: ss::TPatId =
            Alloc::alloc(tycker, monad_ty_var, monad_ty_kd, environment);
        let res_body_ty = Alloc::alloc(
            tycker,
            ss::Forall(
                ss::TypeBinder { pattern: monad_ty_tpat, witness: abst },
                monad_impl_to_body_lift_ty,
            ),
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
