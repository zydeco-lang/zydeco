mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, syntax::*};

#[test]
fn readiness_collects_distinct_holes_and_ignores_erased_static_material() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        let result = Alloc::alloc(tycker, OSTy, ctype, &());
        let first: ValueId = Alloc::alloc(tycker, Hole, unit, &());
        let second: CompuId = Alloc::alloc(tycker, Hole, result, &());
        let thunk_kind = Alloc::alloc(tycker, Arrow(ctype, vtype), (), &());
        let thunk_type = Alloc::alloc(tycker, ThkTy, thunk_kind, &());
        let thunk_type = Alloc::alloc(tycker, App(thunk_type, result), vtype, &());
        let thunk = Alloc::alloc(tycker, Thunk(second), thunk_type, &());
        let product = Alloc::alloc(tycker, Prod(vec![unit, unit, thunk_type]), vtype, &());
        let value = Alloc::alloc(tycker, Value::VCons(vec![first, first, thunk]), product, &());
        let source = TermAnnId::Value(value, product);

        tycker.elaborate_static_root(source).unwrap();
        let core = tycker.statics.core.as_ref().expect("runtime value produces CoreProgram");
        let errors = zydeco_ir::validate::ExecutionReadiness::check(core).unwrap_err();
        assert_eq!(errors.len(), 3);
        assert!(
            (zydeco_ir::traverse::RuntimeGraph { core: core.arena() }).nodes(core.root()).count()
                >= 4
        );

        // The original graph and its authored holes remain available to tooling after erasure.
        assert!(matches!(tycker.statics.values[&first], Value::Hole(_)));
        assert!(matches!(tycker.statics.compus[&second], Computation::Hole(_)));

        let residual = Alloc::alloc(tycker, Triv, unit, &());
        let residual_root = TermAnnId::Value(residual, unit);
        tycker.elaborate_static_root(residual_root).unwrap();
        let residual_core = tycker.statics.core.as_ref().expect("unit value produces CoreProgram");
        assert!(zydeco_ir::validate::ExecutionReadiness::check(residual_core).is_ok());
        assert_eq!(
            (zydeco_ir::traverse::RuntimeGraph { core: residual_core.arena() })
                .nodes(residual_core.root())
                .count(),
            1
        );
    });
}
