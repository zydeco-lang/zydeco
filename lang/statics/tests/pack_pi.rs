mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, Tycker, environment::*, syntax::*};
use zydeco_utils::prelude::IdAllocator;

impl TestFixture {
    fn package_domain(tycker: &mut Tycker<'_>, vtype: KindId, ctype: KindId) -> TypeId {
        let env = TyEnv::new();
        let source_witness: AbstId = Alloc::alloc(tycker, None::<DefId>, ctype, &());
        let payload = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let binder = TypeBinder::with_witness(tycker, source_witness, &env);
        Alloc::alloc(tycker, Exists::new(binder, payload), vtype, &env)
    }

    fn witness(tycker: &mut Tycker<'_>, ctype: KindId) -> (AbstId, TypeId) {
        let witness: AbstId = Alloc::alloc(tycker, None::<DefId>, ctype, &());
        tycker.statics.existential_skolems.ensure(witness);
        let body_env = TyEnv::new().with_skolem(witness);
        let codomain = Alloc::alloc(tycker, witness, ctype, &body_env);
        (witness, codomain)
    }

    fn pack_pi(
        tycker: &mut Tycker<'_>, domain: TypeId, witness: AbstId, codomain: TypeId, ctype: KindId,
    ) -> TypeId {
        Alloc::alloc(
            tycker,
            PackPi { domain, witnesses: PackTelescope::singleton(witness), codomain },
            ctype,
            &TyEnv::new(),
        )
    }

    fn val_pi(
        tycker: &mut Tycker<'_>, domain: TypeId, witness: AbstId, codomain: TypeId, vtype: KindId,
    ) -> TypeId {
        Alloc::alloc(
            tycker,
            ValPi {
                binder: ValPiBinder::Value(ValueParameter {
                    domain,
                    witnesses: Some(PackTelescope::singleton(witness)),
                    witness_projection: PackageWitnessProjection::Package { abstracts: 1 },
                }),
                codomain,
            },
            vtype,
            &TyEnv::new(),
        )
    }
}

#[test]
fn pack_pi_binds_its_opened_witness() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let domain = TestFixture::package_domain(tycker, vtype, ctype);
        let (witness, codomain) = TestFixture::witness(tycker, ctype);
        let pack_pi = TestFixture::pack_pi(tycker, domain, witness, codomain, ctype);
        let outer = SkolemScope::default();

        assert!(pack_pi.constrain_to_scope(tycker, &outer).is_ok());
        assert!(codomain.constrain_to_scope(tycker, &outer).is_err());
    });
}

#[test]
fn pack_pi_witnesses_are_alpha_equivalent() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let domain = TestFixture::package_domain(tycker, vtype, ctype);
        let (lhs_witness, lhs_codomain) = TestFixture::witness(tycker, ctype);
        let (rhs_witness, rhs_codomain) = TestFixture::witness(tycker, ctype);
        let lhs = TestFixture::pack_pi(tycker, domain, lhs_witness, lhs_codomain, ctype);
        let rhs = TestFixture::pack_pi(tycker, domain, rhs_witness, rhs_codomain, ctype);

        let Ok(joined) = lhs.lub(rhs, tycker) else {
            panic!("alpha-equivalent package arrows did not unify")
        };
        assert_eq!(joined, lhs);
    });
}

#[test]
fn holes_in_pack_pi_codomain_retain_the_bound_witness_scope() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let domain = TestFixture::package_domain(tycker, vtype, ctype);
        let (witness, codomain) = TestFixture::witness(tycker, ctype);
        let mut source_ids = IdAllocator::<zydeco_surface::bitter::arena::BitterScope>::new();
        let site: zydeco_statics::surface_syntax::TermId = source_ids.alloc();
        let fill: zydeco_statics::syntax::FillId =
            Alloc::alloc(tycker, zydeco_statics::syntax::InferenceSite::Term(site), (), &());
        let body_env = TyEnv::new().with_skolem(witness);
        let hole = Alloc::alloc(tycker, fill, ctype, &body_env);
        let pack_pi = TestFixture::pack_pi(tycker, domain, witness, hole, ctype);

        assert!(pack_pi.constrain_to_scope(tycker, &SkolemScope::default()).is_ok());
        assert!(fill.fill(tycker, codomain.into()).is_ok());
    });
}

#[test]
fn abstract_substitution_stops_at_pack_pi_witnesses() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let domain = TestFixture::package_domain(tycker, vtype, ctype);
        let (bound, codomain) = TestFixture::witness(tycker, ctype);
        let (_, replacement) = TestFixture::witness(tycker, ctype);
        let pack_pi = TestFixture::pack_pi(tycker, domain, bound, codomain, ctype);

        let Ok(substituted) = pack_pi.subst_abst(tycker, (bound, replacement)) else {
            panic!("substitution through a package arrow failed")
        };
        assert_eq!(substituted, pack_pi);
    });
}

#[test]
fn abstract_substitution_sequences_shadow_bound_pack_pi_witnesses() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let domain = TestFixture::package_domain(tycker, vtype, ctype);
        let (bound, bound_ty) = TestFixture::witness(tycker, ctype);
        let (free, free_ty) = TestFixture::witness(tycker, ctype);
        let (_, bound_replacement) = TestFixture::witness(tycker, ctype);
        let codomain = Alloc::alloc(tycker, Prod(vec![bound_ty, free_ty]), ctype, &TyEnv::new());
        let pack_pi = TestFixture::pack_pi(tycker, domain, bound, codomain, ctype);
        let assignments = [(free, bound_ty), (bound, bound_replacement)];

        let Ok(substituted) = pack_pi.subst_absts(tycker, &assignments) else {
            panic!("substitution sequence through a package arrow failed")
        };
        let Ok(Type::PackPi(substituted)) = tycker.type_filled(&substituted) else {
            panic!("substitution changed the package-arrow shape")
        };
        let Ok(Type::Prod(Prod(components))) = tycker.type_filled(&substituted.codomain) else {
            panic!("substitution changed the codomain product shape")
        };

        assert_eq!(components.as_slice(), [bound_ty, bound_ty]);
    });
}

#[test]
fn val_pi_binds_and_alpha_renames_its_opened_witness() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let domain = TestFixture::package_domain(tycker, vtype, vtype);
        let (lhs_witness, lhs_codomain) = TestFixture::witness(tycker, vtype);
        let (rhs_witness, rhs_codomain) = TestFixture::witness(tycker, vtype);
        let lhs = TestFixture::val_pi(tycker, domain, lhs_witness, lhs_codomain, vtype);
        let rhs = TestFixture::val_pi(tycker, domain, rhs_witness, rhs_codomain, vtype);
        let outer = SkolemScope::default();

        assert!(lhs.constrain_to_scope(tycker, &outer).is_ok());
        assert!(lhs_codomain.constrain_to_scope(tycker, &outer).is_err());
        let Ok(joined) = lhs.lub(rhs, tycker) else {
            panic!("alpha-equivalent ValPi package binders did not unify")
        };
        assert_eq!(joined, lhs);
    });
}

#[test]
fn abstract_substitution_stops_at_val_pi_witnesses() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let domain = TestFixture::package_domain(tycker, vtype, vtype);
        let (bound, codomain) = TestFixture::witness(tycker, vtype);
        let (_, replacement) = TestFixture::witness(tycker, vtype);
        let val_pi = TestFixture::val_pi(tycker, domain, bound, codomain, vtype);

        let Ok(substituted) = val_pi.subst_abst(tycker, (bound, replacement)) else {
            panic!("substitution through ValPi failed")
        };
        assert_eq!(substituted, val_pi);
    });
}
