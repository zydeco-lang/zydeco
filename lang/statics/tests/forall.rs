mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, Tycker, syntax::*};

impl TestFixture {
    fn abst(tycker: &mut Tycker<'_>, kind: KindId) -> (AbstId, TypeId) {
        let abst = Alloc::alloc(tycker, None::<DefId>, kind, &());
        let ty = Alloc::alloc(tycker, abst, kind, &());
        (abst, ty)
    }
}

#[test]
fn alpha_equivalent_forall_types_unify() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let (lhs_binder, lhs_body) = TestFixture::abst(tycker, vtype);
        let (rhs_binder, rhs_body) = TestFixture::abst(tycker, vtype);
        let lhs_binder = TypeBinder::with_witness(tycker, lhs_binder);
        let rhs_binder = TypeBinder::with_witness(tycker, rhs_binder);
        let lhs = Alloc::alloc(tycker, Forall(lhs_binder, lhs_body), ctype, &());
        let rhs = Alloc::alloc(tycker, Forall(rhs_binder, rhs_body), ctype, &());

        let Ok(joined) = lhs.lub(rhs, tycker) else {
            panic!("alpha-equivalent universal types did not unify")
        };
        assert_eq!(joined, lhs);
    });
}

#[test]
fn alpha_equivalent_val_pi_type_binders_unify() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let (lhs_binder, lhs_body) = TestFixture::abst(tycker, vtype);
        let (rhs_binder, rhs_body) = TestFixture::abst(tycker, vtype);
        let lhs_binder = TypeBinder::with_witness(tycker, lhs_binder);
        let rhs_binder = TypeBinder::with_witness(tycker, rhs_binder);
        let lhs = Alloc::alloc(
            tycker,
            ValPi { binder: ValPiBinder::Type(lhs_binder), codomain: lhs_body },
            vtype,
            &(),
        );
        let rhs = Alloc::alloc(
            tycker,
            ValPi { binder: ValPiBinder::Type(rhs_binder), codomain: rhs_body },
            vtype,
            &(),
        );

        let Ok(joined) = lhs.lub(rhs, tycker) else {
            panic!("alpha-equivalent ValPi type binders did not unify")
        };
        assert_eq!(joined, lhs);
    });
}

#[test]
fn alpha_equivalent_type_abstractions_unify() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let (lhs_witness, lhs_body) = TestFixture::abst(tycker, vtype);
        let (rhs_witness, rhs_body) = TestFixture::abst(tycker, vtype);
        let lhs_binder = TypeBinder::with_witness(tycker, lhs_witness);
        let rhs_binder = TypeBinder::with_witness(tycker, rhs_witness);
        let function_kind = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let lhs = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: lhs_binder, body: lhs_body },
            function_kind,
            &(),
        );
        let rhs = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: rhs_binder, body: rhs_body },
            function_kind,
            &(),
        );

        let Ok(joined) = lhs.lub(rhs, tycker) else {
            panic!("alpha-equivalent type abstractions did not unify")
        };
        assert_eq!(joined, lhs);
    });
}
