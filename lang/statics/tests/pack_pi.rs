mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, Tycker, tyck::syntax::*};
use zydeco_utils::prelude::IdAllocator;

impl TestFixture {
    fn package_domain(tycker: &mut Tycker<'_>, vtype: KindId, ctype: KindId) -> TypeId {
        let env = TyEnv::new();
        let source_witness: AbstId = Alloc::alloc(tycker, None::<DefId>, ctype, &());
        let payload = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let binder = TypeBinder::with_witness(tycker, source_witness, &env);
        Alloc::alloc(tycker, Exists(binder, payload), vtype, &env)
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
        let fill = tycker.statics.fills.alloc(site);
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
