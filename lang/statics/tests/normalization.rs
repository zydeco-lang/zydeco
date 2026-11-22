mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, Lub, Tycker, syntax::*};

struct TypeBinderFixture;

impl TypeBinderFixture {
    fn named(tycker: &mut Tycker<'_>, kind: KindId, name: &str) -> TypeBinder {
        let definition = Alloc::alloc(tycker, VarName::new(name), AnnId::Kind(kind), &());
        let pattern = Alloc::alloc(tycker, definition, kind, &());
        let witness = Alloc::alloc(tycker, pattern, (), &());
        TypeBinder { pattern, witness }
    }
}

#[test]
fn type_application_beta_reduces() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let variable = Alloc::alloc(tycker, VarName::new("X"), AnnId::Kind(vtype), &());
        let binder: TPatId = Alloc::alloc(tycker, variable, vtype, &());
        let witness: AbstId = Alloc::alloc(tycker, binder, (), &());
        let body: TypeId = Alloc::alloc(tycker, witness, vtype, &());
        let function_kind: KindId = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let function = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: TypeBinder { pattern: binder, witness }, body },
            function_kind,
            &(),
        );
        let argument = Alloc::alloc(tycker, UnitTy, vtype, &());
        let application = Alloc::alloc(tycker, App(function, argument), vtype, &());

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
        let first = TypeBinderFixture::named(tycker, vtype, "A");
        let second = TypeBinderFixture::named(tycker, vtype, "B");
        let third = TypeBinderFixture::named(tycker, vtype, "C");
        let first_type = Alloc::alloc(tycker, first.witness, vtype, &());
        let second_type = Alloc::alloc(tycker, second.witness, vtype, &());
        let third_type = Alloc::alloc(tycker, third.witness, vtype, &());
        let tail = Alloc::alloc(tycker, Prod(vec![second_type, third_type]), vtype, &());
        let body = Alloc::alloc(tycker, Prod(vec![first_type, tail]), vtype, &());
        let one_argument = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let two_arguments = Alloc::alloc(tycker, Arrow(vtype, one_argument), (), &());
        let three_arguments = Alloc::alloc(tycker, Arrow(vtype, two_arguments), (), &());
        let third_abstraction =
            Alloc::alloc(tycker, TypeAbstraction { binder: third, body }, one_argument, &());
        let second_abstraction = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: second, body: third_abstraction },
            two_arguments,
            &(),
        );
        let function = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: first, body: second_abstraction },
            three_arguments,
            &(),
        );
        let first_argument = Alloc::alloc(tycker, UnitTy, vtype, &());
        let second_argument = Alloc::alloc(tycker, OpaqueTy, vtype, &());
        let third_data: DataId = tycker.statics.datas.alloc(Data::new([]));
        let third_argument = Alloc::alloc(tycker, third_data, vtype, &());
        let first_application =
            Alloc::alloc(tycker, App(function, first_argument), two_arguments, &());
        let second_application =
            Alloc::alloc(tycker, App(first_application, second_argument), one_argument, &());
        let application = Alloc::alloc(tycker, App(second_application, third_argument), vtype, &());

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
        let outer = TypeBinderFixture::named(tycker, vtype, "A");
        let inner = TypeBinderFixture::named(tycker, vtype, "B");
        let expected_inner = TypeBinderFixture::named(tycker, vtype, "C");
        let outer_type = Alloc::alloc(tycker, outer.witness, vtype, &());
        let result_kind = Alloc::alloc(tycker, Arrow(vtype, vtype), (), &());
        let function_kind = Alloc::alloc(tycker, Arrow(vtype, result_kind), (), &());
        let body = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: inner, body: outer_type },
            result_kind,
            &(),
        );
        let function =
            Alloc::alloc(tycker, TypeAbstraction { binder: outer, body }, function_kind, &());
        let argument = Alloc::alloc(tycker, UnitTy, vtype, &());
        let partial = Alloc::alloc(tycker, App(function, argument), result_kind, &());
        let expected = Alloc::alloc(
            tycker,
            TypeAbstraction { binder: expected_inner, body: argument },
            result_kind,
            &(),
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
