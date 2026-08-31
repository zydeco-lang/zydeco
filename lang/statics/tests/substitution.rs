mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, StaticsAlloc, Tycker, environment::*, syntax::*};

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
        let product = Alloc::alloc(tycker, Prod(vec![variable_ty, variable_ty]), vtype, &env);
        let replacement = Alloc::alloc(tycker, UnitTy, vtype, &env);

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
        let source_def: DefId = tycker.fresh();
        let source = Alloc::alloc(tycker, source_def, vtype, &empty);
        let replacement = Alloc::alloc(tycker, UnitTy, vtype, &empty);
        let substitution = TyEnv::from_iter([(source_def, replacement.into())]);

        let data_id: DataId = tycker.fresh();
        tycker
            .statics
            .datas
            .insert_new(data_id, Data::new([(CtorName("make".to_owned()), source)]));
        let data = Alloc::alloc(tycker, data_id, vtype, &empty);

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
            tycker.statics.datas[&specialized_data_id].get(&CtorName("make".to_owned())),
            Some(replacement)
        );

        let codata_id: CoDataId = tycker.fresh();
        tycker
            .statics
            .codatas
            .insert_new(codata_id, CoData::new([(DtorName("open".to_owned()), source)]));
        let codata = Alloc::alloc(tycker, codata_id, vtype, &empty);

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
            tycker.statics.codatas[&specialized_codata_id].get(&DtorName("open".to_owned())),
            Some(replacement),
        );
    });
}

#[test]
fn abstract_substitution_stops_at_type_abstraction_binders() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let (witness, body) = TestFixture::abst_type(tycker, vtype);
        let binder = TypeBinder::with_witness(tycker, witness, &env);
        let function_kind = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let abstraction =
            Alloc::alloc(tycker, TypeAbstraction { binder, body }, function_kind, &env);
        let replacement = Alloc::alloc(tycker, UnitTy, vtype, &env);

        let Ok(substituted) = abstraction.subst_abst(tycker, (witness, replacement)) else {
            panic!("substitution through a type abstraction failed")
        };
        assert_eq!(substituted, abstraction);
    });
}
