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
fn alpha_equivalent_existential_types_unify() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let (lhs_binder, lhs_body) = TestFixture::abst(tycker, vtype);
        let (rhs_binder, rhs_body) = TestFixture::abst(tycker, vtype);
        let lhs_binder = TypeBinder::with_witness(tycker, lhs_binder, &env);
        let rhs_binder = TypeBinder::with_witness(tycker, rhs_binder, &env);
        let lhs = Alloc::alloc(tycker, Exists(lhs_binder, lhs_body), vtype, &env);
        let rhs = Alloc::alloc(tycker, Exists(rhs_binder, rhs_body), vtype, &env);

        let Ok(joined) = lhs.lub(rhs, tycker) else {
            panic!("alpha-equivalent existential types did not unify")
        };
        assert_eq!(joined, lhs);
    });
}
