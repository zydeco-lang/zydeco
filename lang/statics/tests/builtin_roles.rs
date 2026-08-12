mod common;

use common::TestFixture;
use zydeco_statics::{
    Alloc, BuiltinClassifierError, BuiltinSignatureValidator, DuplicateBuiltinRole, Tycker,
    environment::*, syntax::*,
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
        let ty = Alloc::alloc(tycker, witness, kind, &TyEnv::new());
        (witness, ty)
    }

    fn add_classifier(
        tycker: &mut Tycker<'_>, vtype: KindId, ctype: KindId, int: TypeId, parameter_count: usize,
    ) -> TypeId {
        let thk = Alloc::alloc(tycker, ThkTy, vtype, &TyEnv::new());
        let ret = Alloc::alloc(tycker, RetTy, ctype, &TyEnv::new());
        let result = Alloc::alloc(tycker, App(ret, int), ctype, &TyEnv::new());
        let body = (0..parameter_count)
            .fold(result, |body, _| Alloc::alloc(tycker, Arrow(int, body), ctype, &TyEnv::new()));
        Alloc::alloc(tycker, App(thk, body), vtype, &TyEnv::new())
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
            .attach_type(signature_witness, BuiltinTypeRole::Int64)
            .unwrap();
        tycker.statics.builtin_roles.transfer_witness(signature_witness, opened_witness).unwrap();

        assert_eq!(
            tycker.statics.builtin_roles.witness(opened_witness),
            Some(BuiltinRole::Type(BuiltinTypeRole::Int64))
        );
    });
}

#[test]
fn one_existential_witness_cannot_carry_conflicting_builtin_roles() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let witness = TestFixture::abstract_type(tycker, vtype);

        tycker.statics.builtin_roles.attach_type(witness, BuiltinTypeRole::Int64).unwrap();

        assert_eq!(
            tycker.statics.builtin_roles.attach_type(witness, BuiltinTypeRole::Char),
            Err(BuiltinRole::Type(BuiltinTypeRole::Int64))
        );
    });
}

#[test]
fn one_package_signature_rejects_duplicate_type_roles() {
    TestFixture::run(|tycker| {
        let (vtype, _) = TestFixture::kinds(tycker);
        let first = TestFixture::abstract_type(tycker, vtype);
        let second = TestFixture::abstract_type(tycker, vtype);
        let domain = Alloc::alloc(tycker, UnitTy, vtype, &TyEnv::new());
        tycker.statics.builtin_roles.attach_type(first, BuiltinTypeRole::Int64).unwrap();
        tycker.statics.builtin_roles.attach_type(second, BuiltinTypeRole::Int64).unwrap();
        let signature =
            PackPi { domain, witnesses: PackTelescope::new(first, [second]), codomain: domain };

        let error =
            BuiltinSignatureValidator::new(&tycker.statics).validate(&signature).unwrap_err();

        assert!(matches!(
            error.duplicates.as_slice(),
            [DuplicateBuiltinRole::Type {
                role: BuiltinTypeRole::Int64,
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
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &TyEnv::new());
        let first =
            Alloc::alloc(tycker, Label(FieldName::from("first"), unit), vtype, &TyEnv::new());
        let second =
            Alloc::alloc(tycker, Label(FieldName::from("second"), unit), vtype, &TyEnv::new());
        let operations = Alloc::alloc(tycker, Prod(first, second), vtype, &TyEnv::new());
        let domain =
            Alloc::alloc(tycker, Label(FieldName::from("int"), operations), vtype, &TyEnv::new());
        tycker
            .statics
            .builtin_roles
            .attach_value(
                first,
                BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add),
            )
            .unwrap();
        tycker
            .statics
            .builtin_roles
            .attach_value(
                second,
                BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add),
            )
            .unwrap();
        let signature =
            PackPi { domain, witnesses: PackTelescope::singleton(witness), codomain: unit };

        let error =
            BuiltinSignatureValidator::new(&tycker.statics).validate(&signature).unwrap_err();

        assert!(matches!(
            error.duplicates.as_slice(),
            [DuplicateBuiltinRole::Value {
                role: BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add),
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
        let unit = Alloc::alloc(tycker, UnitTy, vtype, &TyEnv::new());
        tycker.statics.builtin_roles.attach_type(first, BuiltinTypeRole::Int64).unwrap();
        tycker.statics.builtin_roles.attach_type(second, BuiltinTypeRole::Int64).unwrap();
        let first_signature =
            PackPi { domain: unit, witnesses: PackTelescope::singleton(first), codomain: unit };
        let second_signature =
            PackPi { domain: unit, witnesses: PackTelescope::singleton(second), codomain: unit };

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
        let (int_witness, int) = TestFixture::builtin_type(tycker, vtype, BuiltinTypeRole::Int64);
        let (os_witness, os) = TestFixture::builtin_type(tycker, ctype, BuiltinTypeRole::OS);
        let valid_classifier = TestFixture::add_classifier(tycker, vtype, ctype, int, 2);
        let valid_entry = Alloc::alloc(
            tycker,
            Label(FieldName::from("add"), valid_classifier),
            vtype,
            &TyEnv::new(),
        );
        tycker
            .statics
            .builtin_roles
            .attach_value(
                valid_entry,
                BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add),
            )
            .unwrap();
        let valid_signature = PackPi {
            domain: valid_entry,
            witnesses: PackTelescope::new(int_witness, [os_witness]),
            codomain: os,
        };
        assert!(BuiltinSignatureValidator::new(&tycker.statics).validate(&valid_signature).is_ok());

        let classifier = TestFixture::add_classifier(tycker, vtype, ctype, int, 1);
        let entry =
            Alloc::alloc(tycker, Label(FieldName::from("add"), classifier), vtype, &TyEnv::new());
        tycker
            .statics
            .builtin_roles
            .attach_value(
                entry,
                BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add),
            )
            .unwrap();
        let signature = PackPi {
            domain: entry,
            witnesses: PackTelescope::new(int_witness, [os_witness]),
            codomain: os,
        };

        let error =
            BuiltinSignatureValidator::new(&tycker.statics).validate(&signature).unwrap_err();

        assert!(matches!(
            error.classifiers.as_slice(),
            [BuiltinClassifierError::Mismatch {
                role: BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add),
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
        let (int_witness, int) = TestFixture::builtin_type(tycker, vtype, BuiltinTypeRole::Int64);
        let (os_witness, os) = TestFixture::builtin_type(tycker, ctype, BuiltinTypeRole::OS);
        let classifier = TestFixture::add_classifier(tycker, vtype, ctype, int, 2);
        let entry = Alloc::alloc(
            tycker,
            Label(FieldName::from("int_eq"), classifier),
            vtype,
            &TyEnv::new(),
        );
        tycker.statics.builtin_roles.attach_value(entry, BuiltinValueRole::ReadLineAsInt).unwrap();
        let signature = PackPi {
            domain: entry,
            witnesses: PackTelescope::new(int_witness, [os_witness]),
            codomain: os,
        };

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
