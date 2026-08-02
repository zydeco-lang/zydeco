mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, tyck::syntax::*};
use zydeco_utils::prelude::IdAllocator;

#[test]
fn closing_a_hole_scope_rejects_an_existential_witness_solution() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let witness: AbstId = Alloc::alloc(tycker, None::<DefId>, vtype, &());
        tycker.statics.existential_skolems.ensure(witness);
        let body_env = TyEnv::new().with_skolem(witness);
        let witness_ty = Alloc::alloc(tycker, witness, vtype, &body_env);
        let mut source_ids = IdAllocator::<zydeco_surface::bitter::arena::BitterScope>::new();
        let site: zydeco_statics::surface_syntax::TermId = source_ids.alloc();
        let fill = tycker.statics.fills.alloc(site.into());
        let hole: TypeId = Alloc::alloc(tycker, fill, vtype, &body_env);

        assert!(hole.constrain_to_scope(tycker, &SkolemScope::default()).is_ok());
        assert!(fill.fill(tycker, witness_ty.into()).is_err());
    });
}
