mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, tyck::syntax::*};

#[test]
fn structurally_different_primitive_types_do_not_unify() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let integer = Alloc::alloc(tycker, IntTy, vtype, &env);

        assert!(unit.lub(integer, tycker).is_err());
    });
}
