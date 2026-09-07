use super::*;
use crate::check::tests::{anonymous_type_binder, with_empty_tycker, with_tycker};

#[test]
fn prepared_products_substitute_once_and_reenter_after_unrolling() {
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

        let product = Alloc::alloc(tycker, ss::Prod(vec![source, source]), vtype, &empty);
        let prepared = product.subst_env_k(tycker, &environment).unwrap();
        let ss::Prod(components) = prepared.view_prepared_product_k(tycker, &environment).unwrap();
        assert_eq!(components.as_slice(), [target, target]);

        let definition = Alloc::alloc(tycker, ss::Prod(vec![source, unit]), vtype, &empty);
        let witness: ss::AbstId = Alloc::alloc(tycker, None::<ss::DefId>, vtype, &());
        tycker.record_seal(witness, definition);
        let sealed = Alloc::alloc(tycker, witness, vtype, &empty);
        let ss::Prod(components) = sealed.view_prepared_product_k(tycker, &environment).unwrap();
        assert_eq!(components.as_slice(), [target, unit]);
        assert!(tycker.errors.is_empty());
    });
}

#[test]
fn prepared_annotations_survive_lexical_environment_extensions() {
    with_empty_tycker(|tycker| {
        let empty = TyEnv::default();
        let vtype = ss::VType.build(tycker, &empty);
        let unit = ss::UnitTy.build(tycker, &empty);
        let source_def: ss::DefId = tycker.fresh();
        let target_def: ss::DefId = tycker.fresh();
        let inner_def: ss::DefId = tycker.fresh();
        let source: ss::TypeId = Alloc::alloc(tycker, source_def, vtype, &empty);
        let target: ss::TypeId = Alloc::alloc(tycker, target_def, vtype, &empty);
        let prepared_environment =
            TyEnv::from_iter([(source_def, target.into()), (target_def, unit.into())]);
        let prepared = source.subst_env_k(tycker, &prepared_environment).unwrap();
        assert_eq!(prepared, target);

        let extended_environment = prepared_environment.clone() + [(inner_def, unit.into())];
        let hole = tycker.data.root(tycker.db);
        let checked = TyEnvT::new(extended_environment, hole)
            .tyck_k(tycker, Action::ana_prepared(prepared.into(), &prepared_environment))
            .unwrap();

        assert!(matches!(checked, TermAnnId::Value(_, annotation) if annotation == target));
        assert_ne!(target, unit);
        assert!(tycker.errors.is_empty());
    });
}

#[test]
fn forall_bodies_preserve_prepared_expected_types() {
    with_tycker(
        |allocator, scoped| {
            let binder_def: su::DefId = allocator.alloc();
            scoped.insert_def(binder_def, su::VarName("A".to_owned()));

            let binder: su::PatId = allocator.alloc();
            let body: su::TermId = allocator.alloc();
            let root: su::TermId = allocator.alloc();
            scoped.pats.insert_new(binder, su::Pattern::Var(binder_def));
            scoped.terms.insert_new(body, su::Hole.into());
            scoped.terms.insert_new(root, su::Abs(binder, body).into());
            (root, binder_def)
        },
        |tycker, binder_def| {
            let empty = TyEnv::default();
            let vtype = ss::VType.build(tycker, &empty);
            let ctype = ss::CType.build(tycker, &empty);
            let target_def: ss::DefId = tycker.fresh();
            let target: ss::TypeId = Alloc::alloc(tycker, target_def, ctype, &empty);
            let terminal_data: ss::CoDataId = tycker.fresh();
            tycker.statics.codatas.insert_new(terminal_data, ss::CoData::new([]));
            let terminal = Alloc::alloc(tycker, terminal_data, ctype, &empty);
            let expected_pattern: ss::TPatId = Alloc::alloc(tycker, binder_def, vtype, &empty);
            let expected_witness: ss::AbstId = Alloc::alloc(tycker, expected_pattern, (), &());
            let expected = Alloc::alloc(
                tycker,
                ss::Forall(
                    ss::TypeBinder { pattern: expected_pattern, witness: expected_witness },
                    target,
                ),
                ctype,
                &empty,
            );
            let environment = TyEnv::from_iter([(target_def, terminal.into())]);
            let root = tycker.data.root(tycker.db);

            let checked = TyEnvT::new(environment.clone(), root)
                .tyck_k(tycker, Action::ana_prepared(expected.into(), &environment))
                .unwrap();

            let TermAnnId::Compu(_, annotation) = checked else {
                panic!("a computation forall should check a computation abstraction")
            };
            let ss::Type::Forall(ss::Forall(_, body)) =
                tycker.type_filled_k(&annotation).unwrap().to_owned()
            else {
                panic!("the checked abstraction should retain a forall annotation")
            };
            assert_eq!(body, target);
            assert_ne!(body, terminal);
            assert!(tycker.errors.is_empty());
        },
    );
}

#[test]
fn checked_type_applications_suspend_function_kinded_prefixes() {
    with_empty_tycker(|tycker| {
        let environment = TyEnv::default();
        let vtype = ss::VType.build(tycker, &environment);
        let result_kind = Alloc::alloc(tycker, ss::Arrow(vtype, vtype), (), &());
        let function_kind = Alloc::alloc(tycker, ss::Arrow(vtype, result_kind), (), &());
        let first = anonymous_type_binder(tycker, vtype, &environment);
        let second = anonymous_type_binder(tycker, vtype, &environment);
        let first_type = Alloc::alloc(tycker, first.witness, vtype, &environment);
        let second_type = Alloc::alloc(tycker, second.witness, vtype, &environment);
        let body =
            Alloc::alloc(tycker, ss::Prod(vec![first_type, second_type]), vtype, &environment);
        let inner = Alloc::alloc(
            tycker,
            ss::TypeAbstraction { binder: second, body },
            result_kind,
            &environment,
        );
        let function = Alloc::alloc(
            tycker,
            ss::TypeAbstraction { binder: first, body: inner },
            function_kind,
            &environment,
        );
        let first_argument = Alloc::alloc(tycker, ss::UnitTy, vtype, &environment);
        let second_argument = Alloc::alloc(tycker, ss::OpaqueTy, vtype, &environment);

        let partial = function.apply_type_argument_k(tycker, first_argument, result_kind).unwrap();
        assert!(matches!(
            tycker.type_filled_k(&partial).unwrap(),
            ss::Type::App(ss::App(found, argument))
                if found == function && argument == first_argument
        ));

        let mut normalizer = crate::normalize::FilledNormalizer::default();
        normalizer.normalize_type_k(partial, tycker).unwrap();
        assert!(matches!(tycker.statics.normalized_at(partial), Some(ss::Type::App(_))));

        let saturated = partial.apply_type_argument_k(tycker, second_argument, vtype).unwrap();
        let ss::Type::Prod(ss::Prod(found_components)) =
            tycker.type_filled_k(&saturated).unwrap().to_owned()
        else {
            panic!("saturating the application should materialize its product body")
        };
        assert_eq!(found_components.as_slice(), [first_argument, second_argument]);
        assert!(tycker.errors.is_empty());
    });
}

#[test]
fn synthesized_ascriptions_preserve_current_annotations() {
    with_tycker(
        |allocator, scoped| {
            let source_def = allocator.alloc();
            let target_def = allocator.alloc();
            scoped.insert_def(source_def, su::VarName("Source".to_owned()));
            scoped.insert_def(target_def, su::VarName("Target".to_owned()));

            let hole = allocator.alloc();
            let annotation = allocator.alloc();
            let root = allocator.alloc();
            scoped.terms.insert_new(hole, su::Hole.into());
            scoped.terms.insert_new(annotation, su::Term::Var(source_def));
            scoped.terms.insert_new(root, su::Ann { tm: hole, ty: annotation }.into());
            (root, (source_def, target_def))
        },
        |tycker, (source_def, target_def)| {
            let empty = TyEnv::default();
            let vtype = ss::VType.build(tycker, &empty);
            let unit = ss::UnitTy.build(tycker, &empty);
            let target = Alloc::alloc(tycker, target_def, vtype, &empty);
            let field = FieldName("field".to_owned());
            let annotation: ss::TypeId =
                Alloc::alloc(tycker, ss::Label(field, target), vtype, &empty);
            tycker.statics.annotations_var.insert_new(source_def, vtype.into());
            let environment =
                TyEnv::from_iter([(source_def, annotation.into()), (target_def, unit.into())]);
            let root = tycker.data.root(tycker.db);

            let checked = TyEnvT::new(environment, root).tyck_k(tycker, Action::syn()).unwrap();

            assert!(matches!(
                checked,
                TermAnnId::Value(_, checked_annotation) if checked_annotation == annotation
            ));
            assert!(tycker.errors.is_empty());
        },
    );
}
