mod common;

use common::TestFixture;
use zydeco_statics::{
    Alloc, elaborate::static_values::StaticElaboration, environment::TyEnv, syntax::*,
    traverse::RuntimeGraph, validate::ExecutionReadiness,
};

#[test]
fn readiness_collects_distinct_holes_and_ignores_erased_static_material() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let result = Alloc::alloc(tycker, OSTy, ctype, &env);
        let first: ValueId = Alloc::alloc(tycker, Hole, unit, &env);
        let second: CompuId = Alloc::alloc(tycker, Hole, result, &env);
        let thunk_kind = Alloc::alloc(tycker, Arrow(ctype, vtype), (), &());
        let thunk_type = Alloc::alloc(tycker, ThkTy, thunk_kind, &env);
        let thunk_type = Alloc::alloc(tycker, App(thunk_type, result), vtype, &env);
        let thunk = Alloc::alloc(tycker, Thunk(second), thunk_type, &env);
        let product = Alloc::alloc(tycker, Prod(vec![unit, unit, thunk_type]), vtype, &env);
        let value = Alloc::alloc(tycker, Value::VCons(vec![first, first, thunk]), product, &env);
        let source = TermAnnId::Value(value, product);
        let errors = ExecutionReadiness::check(&tycker.statics, source).unwrap_err();
        assert_eq!(errors.len(), 2);
        assert!(errors.iter().any(|error| error.term == first.into()));
        assert!(errors.iter().any(|error| error.term == second.into()));
        assert_eq!((RuntimeGraph { statics: &tycker.statics }).nodes(source).count(), 4);

        // The original graph and its authored holes remain available to tooling after erasure.
        let residual = Alloc::alloc(tycker, Triv, unit, &env);
        tycker.statics.static_elaboration =
            Some(StaticElaboration { source, residual: Some(TermAnnId::Value(residual, unit)) });
        assert!(ExecutionReadiness::check(&tycker.statics, source).is_ok());
        let nodes = (RuntimeGraph { statics: &tycker.statics })
            .nodes(source)
            .map(|node| node.id())
            .collect::<Vec<_>>();
        assert_eq!(nodes, [residual.into()]);
    });
}
