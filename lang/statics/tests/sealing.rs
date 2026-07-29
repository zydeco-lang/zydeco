mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, tyck::syntax::*};

#[test]
fn sealed_abstract_types_unroll_to_their_representations() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let representation = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let sealed: AbstId = Alloc::alloc(tycker, None::<DefId>, vtype, &());
        tycker.statics.seals.insert_new(sealed, representation);
        let abstract_ty = Alloc::alloc(tycker, sealed, vtype, &env);

        let Ok(unrolled) = abstract_ty.unroll(tycker) else {
            panic!("sealed abstract type failed to unroll")
        };
        assert_eq!(unrolled, representation);
    });
}
