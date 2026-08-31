mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, StaticsAlloc, Tycker, environment::*, syntax::*};

struct TypeBinderFixture;

impl TypeBinderFixture {
    fn named(tycker: &mut Tycker<'_>, env: &TyEnv, kind: KindId, name: &str) -> TypeBinder {
        let definition = Alloc::alloc(tycker, VarName(name.to_owned()), AnnId::Kind(kind), &());
        let pattern = Alloc::alloc(tycker, definition, kind, env);
        let witness = Alloc::alloc(tycker, pattern, (), &());
        TypeBinder { pattern, witness }
    }
}

#[test]
fn type_application_beta_reduces() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let variable = Alloc::alloc(tycker, VarName("X".to_string()), AnnId::Kind(vtype), &());
        let binder: TPatId = Alloc::alloc(tycker, variable, vtype, &env);
        let witness: AbstId = Alloc::alloc(tycker, binder, (), &());
        let body: TypeId = Alloc::alloc(tycker, witness, vtype, &env);
        let function_kind: KindId = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let function = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: TypeBinder { pattern: binder, witness }, body },
            function_kind,
            &env,
        );
        let argument = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let application = Alloc::alloc(tycker, App(function, argument), vtype, &env);

        let Ok(normalized) = application.normalize(tycker, vtype) else {
            panic!("type application failed to normalize")
        };
        assert_eq!(normalized, argument);
    });
}

#[test]
fn saturated_type_application_fuses_nested_abstractions() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let first = TypeBinderFixture::named(tycker, &env, vtype, "A");
        let second = TypeBinderFixture::named(tycker, &env, vtype, "B");
        let third = TypeBinderFixture::named(tycker, &env, vtype, "C");
        let first_type = Alloc::alloc(tycker, first.witness, vtype, &env);
        let second_type = Alloc::alloc(tycker, second.witness, vtype, &env);
        let third_type = Alloc::alloc(tycker, third.witness, vtype, &env);
        let tail = Alloc::alloc(tycker, Prod(vec![second_type, third_type]), vtype, &env);
        let body = Alloc::alloc(tycker, Prod(vec![first_type, tail]), vtype, &env);
        let one_argument = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let two_arguments = Alloc::alloc(tycker, Arrow(vtype, one_argument), (), &());
        let three_arguments = Alloc::alloc(tycker, Arrow(vtype, two_arguments), (), &());
        let third_abstraction =
            Alloc::alloc(tycker, TypeAbstraction { binder: third, body }, one_argument, &env);
        let second_abstraction = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: second, body: third_abstraction },
            two_arguments,
            &env,
        );
        let function = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: first, body: second_abstraction },
            three_arguments,
            &env,
        );
        let first_argument = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let second_argument = Alloc::alloc(tycker, OpaqueTy, vtype, &env);
        let third_data: DataId = tycker.fresh();
        tycker.statics.datas.insert_new(third_data, Data::new([]));
        let third_argument = Alloc::alloc(tycker, third_data, vtype, &env);
        let first_application =
            Alloc::alloc(tycker, App(function, first_argument), two_arguments, &env);
        let second_application =
            Alloc::alloc(tycker, App(first_application, second_argument), one_argument, &env);
        let application =
            Alloc::alloc(tycker, App(second_application, third_argument), vtype, &env);

        let before = tycker.statics.types_pre.len();
        let Ok(normalized) = application.normalize(tycker, vtype) else {
            panic!("the saturated type application should normalize")
        };
        let allocated = tycker.statics.types_pre.len() - before;

        let Ok(normalized_type) = tycker.type_filled(&normalized) else {
            panic!("the saturated result should be filled")
        };
        let Type::Prod(Prod(found_components)) = normalized_type.to_owned() else {
            panic!("the saturated application should expose the result product")
        };
        let [found_first, found_tail] = found_components.as_slice() else {
            panic!("the saturated application should expose two result components")
        };
        let Ok(tail_type) = tycker.type_filled(found_tail) else {
            panic!("the nested result should be filled")
        };
        let Type::Prod(Prod(found_tail_components)) = tail_type.to_owned() else {
            panic!("the saturated application should preserve the nested result product")
        };
        assert_eq!(*found_first, first_argument);
        assert_eq!(found_tail_components.as_slice(), [second_argument, third_argument]);
        assert_eq!(allocated, 2, "the result body should be rewritten only once");
    });
}

#[test]
fn partial_type_applications_unify_with_their_beta_normal_form() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let env = TyEnv::new();
        let outer = TypeBinderFixture::named(tycker, &env, vtype, "A");
        let inner = TypeBinderFixture::named(tycker, &env, vtype, "B");
        let expected_inner = TypeBinderFixture::named(tycker, &env, vtype, "C");
        let outer_type = Alloc::alloc(tycker, outer.witness, vtype, &env);
        let result_kind = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let function_kind = Alloc::alloc(tycker, Arrow(vtype, result_kind), (), &());
        let body = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: inner, body: outer_type },
            result_kind,
            &env,
        );
        let function =
            Alloc::alloc(tycker, TypeAbstraction { binder: outer, body }, function_kind, &env);
        let argument = Alloc::alloc(tycker, UnitTy, vtype, &env);
        let partial = Alloc::alloc(tycker, App(function, argument), result_kind, &env);
        let expected = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: expected_inner, body: argument },
            result_kind,
            &env,
        );

        let Ok(joined) = Lub::lub(partial, expected, tycker) else {
            panic!("a partial application should unify with its beta normal form")
        };
        let Ok(joined_type) = tycker.type_filled(&joined) else {
            panic!("the joined type should be filled")
        };

        assert!(matches!(
            joined_type,
            Type::Abs(TypeAbstraction { body, .. }) if body == argument
        ));
    });
}
