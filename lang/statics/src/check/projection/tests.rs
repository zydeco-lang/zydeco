use super::*;
use crate::check::tests::with_empty_tycker;

#[test]
fn field_substitution_is_independent_of_label_depth() {
    with_empty_tycker(|tycker| {
        let empty = TyEnv::default();
        let vtype = ss::VType.build(tycker, &empty);
        let unit = ss::UnitTy.build(tycker, &empty);
        let source_def: ss::DefId = tycker.fresh();
        let target_def: ss::DefId = tycker.fresh();
        let source: ss::TypeId = Alloc::alloc(tycker, source_def, vtype, &empty);
        let target: ss::TypeId = Alloc::alloc(tycker, target_def, vtype, &empty);
        let environment =
            TyEnv::from_iter([(source_def, target.into()), (target_def, unit.into())]);

        let field = FieldName("selected".to_owned());
        let wrapper = FieldName("wrapper".to_owned());
        let shallow = Alloc::alloc(tycker, ss::Label(field.clone(), source), vtype, &empty);
        let nested = Alloc::alloc(tycker, ss::Label(field.clone(), source), vtype, &empty);
        let deep = Alloc::alloc(tycker, ss::Label(wrapper, nested), vtype, &empty);

        let shallow =
            FieldProjectionResolver::value_term_k(tycker, &environment, shallow, &field).unwrap();
        let deep =
            FieldProjectionResolver::value_term_k(tycker, &environment, deep, &field).unwrap();

        assert_eq!(shallow.projected, target);
        assert_eq!(deep.projected, target);
        assert_ne!(deep.projected, unit);
        assert!(tycker.errors.is_empty());
    });
}

#[test]
fn field_search_reenters_the_environment_after_unrolling_a_seal() {
    with_empty_tycker(|tycker| {
        let environment = TyEnv::default();
        let vtype = ss::VType.build(tycker, &environment);
        let unit = ss::UnitTy.build(tycker, &environment);
        let field = FieldName("selected".to_owned());
        let wrapper = FieldName("wrapper".to_owned());
        let definition = Alloc::alloc(tycker, ss::Label(field.clone(), unit), vtype, &environment);
        let witness: ss::AbstId = Alloc::alloc(tycker, None::<ss::DefId>, vtype, &());
        tycker.record_seal(witness, definition);
        let sealed = Alloc::alloc(tycker, witness, vtype, &environment);
        let root = Alloc::alloc(tycker, ss::Label(wrapper.clone(), sealed), vtype, &environment);

        let direct =
            FieldProjectionResolver::value_term_k(tycker, &environment, root, &wrapper).unwrap();
        let nested =
            FieldProjectionResolver::value_term_k(tycker, &environment, root, &field).unwrap();

        assert_eq!(direct.projected, sealed);
        assert_eq!(nested.projected, unit);
        assert!(tycker.errors.is_empty());
    });
}

#[test]
fn repeated_field_search_reuses_materialization_until_inference_changes() {
    with_empty_tycker(|tycker| {
        let empty = TyEnv::default();
        let vtype = ss::VType.build(tycker, &empty);
        let unit = ss::UnitTy.build(tycker, &empty);
        let source_def: ss::DefId = tycker.fresh();
        let source = Alloc::alloc(tycker, source_def, vtype, &empty);
        let environment = TyEnv::from_iter([(source_def, unit.into())]);
        let field = FieldName("selected".to_owned());
        let definition = Alloc::alloc(tycker, ss::Label(field.clone(), source), vtype, &empty);
        let witness: ss::AbstId = Alloc::alloc(tycker, None::<ss::DefId>, vtype, &());
        tycker.record_seal(witness, definition);
        let sealed = Alloc::alloc(tycker, witness, vtype, &empty);

        let before = tycker.statics.types_pre.len();
        let first =
            FieldProjectionResolver::value_term_k(tycker, &environment, sealed, &field).unwrap();
        let after_first = tycker.statics.types_pre.len();
        let second =
            FieldProjectionResolver::value_term_k(tycker, &environment, sealed, &field).unwrap();
        let after_second = tycker.statics.types_pre.len();

        assert_eq!(first.projected, unit);
        assert_eq!(second.projected, unit);
        assert!(after_first > before);
        assert_eq!(after_second, after_first);

        let site = tycker.data.root(tycker.db);
        let fill: ss::FillId = Alloc::alloc(tycker, ss::InferenceSite::Term(site), (), &());
        assert!(fill.fill(tycker, unit.into()).is_ok());
        let third =
            FieldProjectionResolver::value_term_k(tycker, &environment, sealed, &field).unwrap();

        assert_eq!(third.projected, unit);
        assert!(tycker.statics.types_pre.len() > after_second);
        assert!(tycker.errors.is_empty());
    });
}

#[test]
fn adding_a_seal_invalidates_cached_field_materialization() {
    with_empty_tycker(|tycker| {
        let empty = TyEnv::default();
        let vtype = ss::VType.build(tycker, &empty);
        let unit = ss::UnitTy.build(tycker, &empty);
        let witness: ss::AbstId = Alloc::alloc(tycker, None::<ss::DefId>, vtype, &());
        let sealed = Alloc::alloc(tycker, witness, vtype, &empty);
        let deferred = DeferredEnvType::with_environment(sealed, &empty);

        let before = deferred.materialize_k(tycker).unwrap();
        tycker.record_seal(witness, unit);
        let after = deferred.materialize_k(tycker).unwrap();

        assert_eq!(before, sealed);
        assert_eq!(after, unit);
        assert!(tycker.errors.is_empty());
    });
}

#[test]
fn deferred_telescope_uses_the_final_environment_once() {
    with_empty_tycker(|tycker| {
        let empty = TyEnv::default();
        let vtype = ss::VType.build(tycker, &empty);
        let unit = ss::UnitTy.build(tycker, &empty);
        let source_def: ss::DefId = tycker.fresh();
        let target_def: ss::DefId = tycker.fresh();
        let source: ss::TypeId = Alloc::alloc(tycker, source_def, vtype, &empty);
        let target: ss::TypeId = Alloc::alloc(tycker, target_def, vtype, &empty);
        let first = TyEnv::from_iter([(source_def, target.into())]);
        let final_environment =
            TyEnv::from_iter([(source_def, target.into()), (target_def, unit.into())]);

        let deferred = DeferredTelescopeType::new(source)
            .with_environment(&first)
            .descend(source)
            .with_environment(&final_environment);
        let materialized = deferred.materialize_k(tycker).unwrap();

        assert_eq!(materialized, target);
        assert_ne!(materialized, unit);
        assert!(tycker.errors.is_empty());
    });
}

#[test]
fn deferred_telescope_composes_abstract_assignments_in_order() {
    with_empty_tycker(|tycker| {
        let environment = TyEnv::default();
        let vtype = ss::VType.build(tycker, &environment);
        let unit = ss::UnitTy.build(tycker, &environment);
        let first: ss::AbstId = Alloc::alloc(tycker, None::<ss::DefId>, vtype, &());
        let second: ss::AbstId = Alloc::alloc(tycker, None::<ss::DefId>, vtype, &());
        let first_ty = Alloc::alloc(tycker, first, vtype, &environment);
        let second_ty = Alloc::alloc(tycker, second, vtype, &environment);

        let materialized = DeferredTelescopeType::new(first_ty)
            .with_abstract(first, second_ty)
            .with_abstract(second, unit)
            .materialize_k(tycker)
            .unwrap();

        assert_eq!(materialized, unit);
        assert!(tycker.errors.is_empty());
    });
}
