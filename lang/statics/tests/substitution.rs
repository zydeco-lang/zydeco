mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, StaticsAlloc, Tycker, environment::*, syntax::*};

impl TestFixture {
    fn abst_type(tycker: &mut Tycker<'_>, kind: KindId) -> (AbstId, TypeId) {
        let abst = Alloc::alloc(tycker, None::<DefId>, kind, &());
        let ty = Alloc::alloc(tycker, abst, kind, &());
        (abst, ty)
    }
}

#[test]
fn lexical_substitution_specializes_a_solved_hole_without_mutating_its_solution() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let source_def: DefId = tycker.fresh_def();
        let source = Alloc::alloc(tycker, source_def, vtype, &());
        let solution = Alloc::alloc(tycker, Prod(vec![source, source]), vtype, &());
        let site = tycker.data.root(tycker.db);
        let fill: FillId = Alloc::alloc(tycker, InferenceSite::Term(site), (), &());
        let hole: TypeId = Alloc::alloc(tycker, fill, vtype, &Default::default());
        assert!(fill.fill(tycker, solution.into()).is_ok());
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        let opaque = Alloc::alloc(tycker, OpaqueTy, vtype, &());

        for replacement in [unit, opaque] {
            let substitution = TyEnv::from_iter([(source_def, replacement.into())]);
            let Ok(specialized) = hole.subst_env(tycker, &substitution) else {
                panic!("substitution must follow a solved hole")
            };
            let Ok(Type::Prod(Prod(components))) = tycker.type_filled(&specialized) else {
                panic!("specialization must retain the solved product shape")
            };
            assert_eq!(components.as_slice(), [replacement, replacement]);
            assert_eq!(tycker.statics.solus.get(fill), Some(&solution.into()));
        }
    });
}

#[test]
fn abstract_substitution_rewrites_product_components() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let (variable, variable_ty) = TestFixture::abst_type(tycker, vtype);
        let product = Alloc::alloc(tycker, Prod(vec![variable_ty, variable_ty]), vtype, &());
        let replacement = Alloc::alloc(tycker, UnitTy, vtype, &());

        let Ok(substituted) = product.subst_abst(tycker, (variable, replacement)) else {
            panic!("abstract substitution through a product failed")
        };
        let Ok(Type::Prod(Prod(components))) = tycker.type_filled(&substituted) else {
            panic!("substitution changed the product shape")
        };
        assert_eq!(components.as_slice(), [replacement, replacement]);
    });
}

#[test]
fn nominal_substitution_reuses_unchanged_definitions() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let empty = TyEnv::new();
        let source_def: DefId = tycker.fresh_def();
        let source = Alloc::alloc(tycker, source_def, vtype, &());
        let replacement = Alloc::alloc(tycker, UnitTy, vtype, &());
        let substitution = TyEnv::from_iter([(source_def, replacement.into())]);

        let data_id: DataId =
            tycker.statics.datas.alloc(Data::new([(CtorName::new("make"), source)]));
        let data = Alloc::alloc(tycker, data_id, vtype, &());

        let Ok(unchanged_data) = data.subst_env(tycker, &empty) else {
            panic!("no-op data substitution failed")
        };
        assert_eq!(unchanged_data, data);
        let Ok(specialized_data) = data.subst_env(tycker, &substitution) else {
            panic!("data specialization failed")
        };
        let Ok(Type::Data(specialized_data_id)) = tycker.type_filled(&specialized_data) else {
            panic!("substitution changed the data type shape")
        };
        assert_ne!(specialized_data_id, data_id);
        assert_eq!(
            tycker.statics.datas[&specialized_data_id].get(&CtorName::new("make")),
            Some(replacement)
        );

        let codata_id: CoDataId =
            tycker.statics.codatas.alloc(CoData::new([(DtorName::new("open"), source)]));
        let codata = Alloc::alloc(tycker, codata_id, vtype, &());

        let Ok(unchanged_codata) = codata.subst_env(tycker, &empty) else {
            panic!("no-op codata substitution failed")
        };
        assert_eq!(unchanged_codata, codata);
        let Ok(specialized_codata) = codata.subst_env(tycker, &substitution) else {
            panic!("codata specialization failed")
        };
        let Ok(Type::CoData(specialized_codata_id)) = tycker.type_filled(&specialized_codata)
        else {
            panic!("substitution changed the codata type shape")
        };
        assert_ne!(specialized_codata_id, codata_id);
        assert_eq!(
            tycker.statics.codatas[&specialized_codata_id].get(&DtorName::new("open")),
            Some(replacement),
        );
    });
}

#[test]
fn abstract_substitution_stops_at_type_abstraction_binders() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let (witness, body) = TestFixture::abst_type(tycker, vtype);
        let binder = TypeBinder::with_witness(tycker, witness);
        let function_kind = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let abstraction =
            Alloc::alloc(tycker, TypeAbstraction { binder, body }, function_kind, &());
        let replacement = Alloc::alloc(tycker, UnitTy, vtype, &());

        let Ok(substituted) = abstraction.subst_abst(tycker, (witness, replacement)) else {
            panic!("substitution through a type abstraction failed")
        };
        assert_eq!(substituted, abstraction);
    });
}

#[test]
fn abstract_substitution_respects_nested_quantifiers() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        let (witness, witness_ty) = TestFixture::abst_type(tycker, vtype);
        let binder = TypeBinder::with_witness(tycker, witness);
        let ret_kind = Alloc::alloc(tycker, Arrow(vtype, ctype), (), &());
        let ret = Alloc::alloc(tycker, RetTy, ret_kind, &());
        let returns = Alloc::alloc(tycker, App(ret, witness_ty), ctype, &());
        let universal = Alloc::alloc(tycker, Forall(binder.clone(), returns), ctype, &());
        let existential = Alloc::alloc(
            tycker,
            Exists { binder: binder.clone(), mode: ExistsMode::Abstract, body: witness_ty },
            vtype,
            &(),
        );
        // A shared complete type retains its binder; only an opened body
        // receives the instantiation of that binder's witness.
        assert_eq!(
            universal
                .subst_abst(tycker, (witness, unit))
                .unwrap_or_else(|_| panic!("substitution or comparison must succeed")),
            universal
        );
        assert_eq!(
            existential
                .subst_abst(tycker, (witness, unit))
                .unwrap_or_else(|_| panic!("substitution or comparison must succeed")),
            existential
        );
        let opened = returns
            .subst_abst(tycker, (witness, unit))
            .unwrap_or_else(|_| panic!("substitution or comparison must succeed"));
        let expected = Alloc::alloc(tycker, App(ret, unit), ctype, &());
        Lub::lub(expected, opened, tycker)
            .unwrap_or_else(|_| panic!("substitution or comparison must succeed"));

        let (parameter, parameter_ty) = TestFixture::abst_type(tycker, ctype);
        let instantiated = parameter_ty
            .subst_absts(tycker, &[(parameter, universal), (witness, unit)])
            .unwrap_or_else(|_| panic!("substitution or comparison must succeed"));
        assert_eq!(
            instantiated, universal,
            "later assignments must not capture a replacement's binder"
        );
        let wrong = Alloc::alloc(tycker, Forall(binder, expected), ctype, &());
        assert!(
            Lub::lub(universal, wrong, tycker).is_err(),
            "a constant result is not the bound result"
        );
    });
}
