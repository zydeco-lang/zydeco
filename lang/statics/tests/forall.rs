mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, Tycker, tyck::syntax::*};

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
        let lhs = Alloc::alloc(tycker, Forall(lhs_binder, lhs_body), ctype, &env);
        let rhs = Alloc::alloc(tycker, Forall(rhs_binder, rhs_body), ctype, &env);

        let Ok(joined) = lhs.lub(rhs, tycker) else {
            panic!("alpha-equivalent universal types did not unify")
        };
        assert_eq!(joined, lhs);
    });
}
