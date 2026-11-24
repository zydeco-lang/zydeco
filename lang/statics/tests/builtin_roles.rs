mod common;

use common::TestFixture;
use zydeco_statics::{
    Alloc, BuiltinClassifierError, BuiltinSignatureValidator, DuplicateBuiltinRole, Tycker,
    syntax::*,
};

impl TestFixture {
    fn abstract_type(tycker: &mut Tycker<'_>, kind: KindId) -> AbstId {
        Alloc::alloc(tycker, None::<DefId>, kind, &())
    }

    fn builtin_type(
        tycker: &mut Tycker<'_>, kind: KindId, role: BuiltinTypeRole,
    ) -> (AbstId, TypeId) {
        let witness = Self::abstract_type(tycker, kind);
        tycker.statics.builtin_roles.attach_type(witness, role).unwrap();
        let ty = Alloc::alloc(tycker, witness, kind, &());
        (witness, ty)
    }

    fn primitive_type(tycker: &mut Tycker<'_>, kind: KindId, primitive: PrimitiveType) -> TypeId {
        Alloc::alloc(tycker, PrimitiveTy(primitive), kind, &())
    }

    fn add_classifier(
        tycker: &mut Tycker<'_>, vtype: KindId, ctype: KindId, int: TypeId, parameter_count: usize,
    ) -> TypeId {
        let thk = Alloc::alloc(tycker, ThkTy, vtype, &());
        let ret = Alloc::alloc(tycker, RetTy, ctype, &());
        let result = Alloc::alloc(tycker, App(ret, int), ctype, &());
        let body = (0..parameter_count)
            .fold(result, |body, _| Alloc::alloc(tycker, Arrow(int, body), ctype, &()));
        Alloc::alloc(tycker, App(thk, body), vtype, &())
    }
}

#[test]
fn builtin_type_roles_follow_fresh_existential_witnesses() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let signature_witness = TestFixture::abstract_type(tycker, vtype);
        let opened_witness = TestFixture::abstract_type(tycker, vtype);

        tycker
            .statics
            .builtin_roles
            .attach_type(signature_witness, BuiltinTypeRole::Reader)
            .unwrap();
        tycker.statics.builtin_roles.transfer_witness(signature_witness, opened_witness).unwrap();

        assert_eq!(
            tycker.statics.builtin_roles.witness(opened_witness),
            Some(BuiltinRole::Type(BuiltinTypeRole::Reader))
        );
    });
}

#[test]
fn one_existential_witness_cannot_carry_conflicting_builtin_roles() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let witness = TestFixture::abstract_type(tycker, vtype);

        tycker.statics.builtin_roles.attach_type(witness, BuiltinTypeRole::Reader).unwrap();

        assert_eq!(
            tycker.statics.builtin_roles.attach_type(witness, BuiltinTypeRole::Writer),
            Err(BuiltinRole::Type(BuiltinTypeRole::Reader))
        );
    });
}

#[test]
fn one_package_signature_rejects_duplicate_type_roles() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let first = TestFixture::abstract_type(tycker, vtype);
        let second = TestFixture::abstract_type(tycker, vtype);
        let domain = Alloc::alloc(tycker, UnitTy, vtype, &());
        tycker.statics.builtin_roles.attach_type(first, BuiltinTypeRole::Reader).unwrap();
        tycker.statics.builtin_roles.attach_type(second, BuiltinTypeRole::Reader).unwrap();
        let signature =
            PackPi { domain, witnesses: PackTelescope::new(first, [second]), codomain: domain };

        let error =
            BuiltinSignatureValidator::new(&tycker.statics).validate(&signature).unwrap_err();

        assert!(matches!(
            error.duplicates.as_slice(),
            [DuplicateBuiltinRole::Type {
                role: BuiltinTypeRole::Reader,
                witnesses,
            }] if witnesses == &vec![first, second]
        ));
    });
}

#[test]
fn one_package_signature_rejects_duplicate_operation_roles() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let witness = TestFixture::abstract_type(tycker, vtype);
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        let first = Alloc::alloc(tycker, Label(FieldName::from("first"), unit), vtype, &());
        let second = Alloc::alloc(tycker, Label(FieldName::from("second"), unit), vtype, &());
        let operations = Alloc::alloc(tycker, Prod(vec![first, second]), vtype, &());
        let domain = Alloc::alloc(tycker, Label(FieldName::from("int"), operations), vtype, &());
        tycker.statics.builtin_roles.attach_value(first, BuiltinValueRole::MemoryOffset).unwrap();
        tycker.statics.builtin_roles.attach_value(second, BuiltinValueRole::MemoryOffset).unwrap();
        let signature =
            PackPi { domain, witnesses: PackTelescope::singleton(witness), codomain: unit };

        let error =
            BuiltinSignatureValidator::new(&tycker.statics).validate(&signature).unwrap_err();

        assert!(matches!(
            error.duplicates.as_slice(),
            [DuplicateBuiltinRole::Value {
                role: BuiltinValueRole::MemoryOffset,
                entries,
            }] if entries == &vec![first, second]
        ));
    });
}

#[test]
fn the_same_role_may_appear_once_in_distinct_package_signatures() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let first = TestFixture::abstract_type(tycker, vtype);
        let second = TestFixture::abstract_type(tycker, vtype);
        let unrelated = TestFixture::abstract_type(tycker, vtype);
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &());
        tycker.statics.builtin_roles.attach_type(first, BuiltinTypeRole::Reader).unwrap();
        tycker.statics.builtin_roles.attach_type(second, BuiltinTypeRole::Reader).unwrap();
        let first_signature =
            PackPi { domain: unit, witnesses: PackTelescope::singleton(first), codomain: unit };
        // The second signature is structurally distinct: the shared role sits
        // behind a role-less witness, so validating it is not a replay of the
        // first validation.
        let second_signature = PackPi {
            domain: unit,
            witnesses: PackTelescope::new(unrelated, [second]),
            codomain: unit,
        };

        assert!(BuiltinSignatureValidator::new(&tycker.statics).validate(&first_signature).is_ok());
        assert!(
            BuiltinSignatureValidator::new(&tycker.statics).validate(&second_signature).is_ok()
        );
    });
}

#[test]
fn foundational_operation_roles_require_their_exact_classifier() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let int =
            TestFixture::primitive_type(tycker, vtype, PrimitiveType::Integer(IntegerType::Int));
        let (os_witness, os) = TestFixture::builtin_type(tycker, ctype, BuiltinTypeRole::OS);
        let thk = Alloc::alloc(tycker, ThkTy, vtype, &());
        let valid_body = Alloc::alloc(tycker, Arrow(int, os), ctype, &());
        let valid_classifier = Alloc::alloc(tycker, App(thk, valid_body), vtype, &());
        let valid_entry =
            Alloc::alloc(tycker, Label(FieldName::from("exit"), valid_classifier), vtype, &());
        tycker.statics.builtin_roles.attach_value(valid_entry, BuiltinValueRole::Exit).unwrap();
        let valid_signature = PackPi {
            domain: valid_entry,
            witnesses: PackTelescope::singleton(os_witness),
            codomain: os,
        };
        assert!(BuiltinSignatureValidator::new(&tycker.statics).validate(&valid_signature).is_ok());

        let invalid_body = Alloc::alloc(tycker, Arrow(int, valid_body), ctype, &());
        let classifier = Alloc::alloc(tycker, App(thk, invalid_body), vtype, &());
        let entry = Alloc::alloc(tycker, Label(FieldName::from("exit"), classifier), vtype, &());
        tycker.statics.builtin_roles.attach_value(entry, BuiltinValueRole::Exit).unwrap();
        let signature =
            PackPi { domain: entry, witnesses: PackTelescope::singleton(os_witness), codomain: os };

        let error =
            BuiltinSignatureValidator::new(&tycker.statics).validate(&signature).unwrap_err();

        assert!(matches!(
            error.classifiers.as_slice(),
            [BuiltinClassifierError::Mismatch {
                role: BuiltinValueRole::Exit,
                entry: found,
                ..
            }] if *found == entry
        ));
    });
}

#[test]
fn read_line_as_int_rejects_a_non_branch_classifier() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let int =
            TestFixture::primitive_type(tycker, vtype, PrimitiveType::Integer(IntegerType::Int));
        let (os_witness, os) = TestFixture::builtin_type(tycker, ctype, BuiltinTypeRole::OS);
        let classifier = TestFixture::add_classifier(tycker, vtype, ctype, int, 2);
        let entry = Alloc::alloc(tycker, Label(FieldName::from("int_eq"), classifier), vtype, &());
        tycker.statics.builtin_roles.attach_value(entry, BuiltinValueRole::ReadLineAsInt).unwrap();
        let signature =
            PackPi { domain: entry, witnesses: PackTelescope::singleton(os_witness), codomain: os };

        let error =
            BuiltinSignatureValidator::new(&tycker.statics).validate(&signature).unwrap_err();

        assert!(matches!(
            error.classifiers.as_slice(),
            [BuiltinClassifierError::Mismatch {
                role: BuiltinValueRole::ReadLineAsInt,
                entry: found,
                ..
            }] if *found == entry
        ));
    });
}
