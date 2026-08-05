mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Tycker, environment::*, syntax::*};

impl TestFixture {
    fn abst_type(tycker: &mut Tycker<'_>, kind: KindId) -> (AbstId, TypeId) {
        let env = TyEnv::new();
        let abst = Alloc::alloc(tycker, None::<DefId>, kind, &());
        let ty = Alloc::alloc(tycker, abst, kind, &env);
        (abst, ty)
    }
}

#[test]
fn abstract_substitution_rewrites_product_components() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let (variable, variable_ty) = TestFixture::abst_type(tycker, vtype);
        let product = Alloc::alloc(tycker, Prod(variable_ty, variable_ty), vtype, &env);
        let replacement = Alloc::alloc(tycker, UnitTy, vtype, &env);

        let Ok(substituted) = product.subst_abst(tycker, (variable, replacement)) else {
            panic!("abstract substitution through a product failed")
        };
        let Ok(Type::Prod(Prod(head, tail))) = tycker.type_filled(&substituted) else {
            panic!("substitution changed the product shape")
        };
        assert_eq!((head, tail), (replacement, replacement));
    });
}
