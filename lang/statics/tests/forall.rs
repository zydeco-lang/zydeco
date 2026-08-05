mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, Tycker, environment::*, syntax::*};

impl TestFixture {
    fn abst(tycker: &mut Tycker<'_>, kind: KindId) -> (AbstId, TypeId) {
        let env = TyEnv::new();
        let abst = Alloc::alloc(tycker, None::<DefId>, kind, &());
        let ty = Alloc::alloc(tycker, abst, kind, &env);
        (abst, ty)
    }
}

#[test]
fn alpha_equivalent_forall_types_unify() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let (lhs_binder, lhs_body) = TestFixture::abst(tycker, vtype);
        let (rhs_binder, rhs_body) = TestFixture::abst(tycker, vtype);
        let lhs_binder = TypeBinder::with_witness(tycker, lhs_binder, &env);
        let rhs_binder = TypeBinder::with_witness(tycker, rhs_binder, &env);
        let lhs = Alloc::alloc(tycker, Forall(lhs_binder, lhs_body), ctype, &env);
        let rhs = Alloc::alloc(tycker, Forall(rhs_binder, rhs_body), ctype, &env);

        let Ok(joined) = lhs.lub(rhs, tycker) else {
            panic!("alpha-equivalent universal types did not unify")
        };
        assert_eq!(joined, lhs);
    });
}

#[test]
fn alpha_equivalent_value_forall_types_unify() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let (lhs_binder, lhs_body) = TestFixture::abst(tycker, vtype);
        let (rhs_binder, rhs_body) = TestFixture::abst(tycker, vtype);
        let lhs_binder = TypeBinder::with_witness(tycker, lhs_binder, &env);
        let rhs_binder = TypeBinder::with_witness(tycker, rhs_binder, &env);
        let lhs = Alloc::alloc(tycker, ValueForall(lhs_binder, lhs_body), vtype, &env);
        let rhs = Alloc::alloc(tycker, ValueForall(rhs_binder, rhs_body), vtype, &env);

        let Ok(joined) = lhs.lub(rhs, tycker) else {
            panic!("alpha-equivalent pure universal types did not unify")
        };
        assert_eq!(joined, lhs);
    });
}
