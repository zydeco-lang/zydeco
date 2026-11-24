//! Shared declarative ABI, builtin catalog, FFI protocol, and runtime failure codes.
//!
//! `zydeco-abi` is `#![no_std]` (using `alloc`) with no procedural-macro or external dependencies by
//! default so frontends, backends, and embedded native/WASI runtimes share one contract.

#![no_std]

extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

pub mod builtin;
pub mod ffi;
pub mod host;
pub mod unit;

pub use builtin::*;
pub use ffi::*;
pub use host::*;
pub use unit::*;
pub use zydeco_machine::word::ScalarRepresentation;

#[cfg(feature = "bundle")]
pub mod bundle {
    /// One source file in a self-contained copy of this crate.
    pub struct SourceFile {
        pub path: &'static str,
        pub contents: &'static str,
    }

    include!(concat!(env!("OUT_DIR"), "/bundle.rs"));
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{boxed::Box, collections::BTreeSet, vec};

    fn walk_classifier(classifier: &BuiltinValueClassifier) -> (usize, HostCallMode) {
        let BuiltinValueClassifier::Thunk(compu) = classifier else {
            panic!("builtin operation classifier must be a Thk");
        };
        let mut compu = compu.as_ref();
        while let BuiltinComputationClassifier::ForallCType(body) = compu {
            compu = body.as_ref();
        }
        let mut arity = 0;
        while let BuiltinComputationClassifier::Arrow(_, tail) = compu {
            arity += 1;
            compu = tail.as_ref();
        }
        let mode = match compu {
            | BuiltinComputationClassifier::Return(_) => HostCallMode::Returning,
            | BuiltinComputationClassifier::OS | BuiltinComputationClassifier::Bound(_) => {
                HostCallMode::Control
            }
            | other => panic!("unexpected terminal classifier `{other}`"),
        };
        (arity, mode)
    }

    #[test]
    fn builtin_catalog_is_complete_unique_and_consistent() {
        let all: alloc::vec::Vec<_> = BuiltinValueRole::all().collect();
        assert_eq!(all.len(), 10 * 11 + 2 * 10 + 48);

        let mut source_names = BTreeSet::new();
        let mut host_names = BTreeSet::new();
        for role in all {
            let src = role.source_name();
            assert!(source_names.insert(src.clone()), "duplicate source name {src}");
            assert_eq!(BuiltinValueRole::from_source_name(&src), Some(role));
            assert_eq!(BuiltinRole::from_source_name(&src), Some(BuiltinRole::Value(role)));

            let host = role.host_name();
            assert!(host_names.insert(host.clone()), "duplicate host name {host}");

            let abi = BuiltinOperationAbi::for_role(role).into_classifier();
            let (abi_arity, abi_mode) = walk_classifier(&abi);
            assert_eq!(role.arity(), abi_arity, "arity mismatch for {role}");
            assert_eq!(role.call_mode(), abi_mode, "call_mode mismatch for {role}");

            if role.may_allocate_gc() {
                assert_eq!(
                    role.spare_box(),
                    None,
                    "GC-allocating host op {role} cannot take a spare box"
                );
                assert!(role.arity() <= 5, "GC-allocating host op {role} exceeds 5 args");
            }
        }
    }

    #[test]
    fn intrinsic_catalog_is_complete_unique_and_consistent() {
        let ops: alloc::vec::Vec<_> = IntrinsicOp::all().collect();
        assert_eq!(ops.len(), 10 * 9 + 2 * 8 + 15);

        let mut source_names = BTreeSet::new();
        let mut host_names = BTreeSet::new();
        for op in ops {
            let src = op.source_name();
            assert!(source_names.insert(src.clone()), "duplicate intrinsic source name {src}");
            assert_eq!(IntrinsicOp::from_source_name(&src), Some(op));
            assert_eq!(IntrinsicRole::from_source_name(&src), Some(IntrinsicRole::Operation(op)));

            let host = op.host_name();
            assert!(host_names.insert(host.clone()), "duplicate intrinsic host name {host}");

            let abi = BuiltinOperationAbi::for_intrinsic(op).into_classifier();
            let (abi_arity, abi_mode) = walk_classifier(&abi);
            assert_eq!(op.arity(), abi_arity, "arity mismatch for {op}");
            assert_eq!(op.call_mode(), abi_mode, "call_mode mismatch for {op}");

            if op.may_allocate_gc() {
                assert_eq!(
                    op.spare_box(),
                    None,
                    "GC-allocating intrinsic op {op} cannot take a spare box"
                );
                assert!(op.arity() <= 5, "GC-allocating intrinsic op {op} exceeds 5 args");
            }
        }

        let all_roles: alloc::vec::Vec<_> = IntrinsicRole::all().collect();
        let mut role_names = BTreeSet::new();
        for role in all_roles {
            let src = role.source_name();
            assert!(role_names.insert(src.clone()), "duplicate intrinsic role name {src}");
            assert_eq!(IntrinsicRole::from_source_name(&src), Some(role));
        }
        assert_eq!(
            IntrinsicRole::from_source_name("val/int/add"),
            Some(IntrinsicRole::ValueInt(ValueIntOp::Add))
        );
        assert_eq!(IntrinsicRole::from_source_name("int_add"), None);
    }

    #[test]
    fn runtime_failure_and_abi_boundaries_validate() {
        for code in 1..=4 {
            let failure = RuntimeFailure::from_code(code).unwrap();
            assert_eq!(failure.code(), code);
            assert!(!failure.message().is_empty());
        }
        assert_eq!(RuntimeFailure::from_code(0), None);
        assert_eq!(RuntimeFailure::from_code(5), None);

        assert_eq!(HostIoErrorKind::NotFound as i64, 0);
        assert_eq!(HostIoErrorKind::Closed as i64, 6);
        assert_eq!(HostIoErrorKind::Other as i64, 7);

        let ok = ForeignSignature::new(
            vec![ForeignParameter::Integer(IntegerType::Int); ForeignSignature::MAX_C_ARGUMENTS],
            ForeignResult::Unit,
        );
        assert!(ok.is_ok());

        let too_many = ForeignSignature::new(
            vec![
                ForeignParameter::Integer(IntegerType::Int);
                ForeignSignature::MAX_C_ARGUMENTS + 1
            ],
            ForeignResult::Integer(IntegerType::Int),
        );
        assert_eq!(
            too_many,
            Err(ForeignSignatureError::TooManyArguments {
                found: 7,
                maximum: ForeignSignature::MAX_C_ARGUMENTS,
            })
        );

        let valid_unit = UnitValueType::Product(vec![
            UnitValueType::Named {
                name: "x".into(),
                value: Box::new(UnitValueType::Integer(IntegerType::Int)),
            },
            UnitValueType::String,
        ]);
        assert!(valid_unit.validate().is_ok());
        assert_eq!(UnitValueType::Product(vec![]).validate(), Err(UnitInterfaceError));
        assert_eq!(
            UnitValueType::Product(vec![UnitValueType::Unit]).validate(),
            Err(UnitInterfaceError)
        );
        assert_eq!(
            UnitValueType::Named { name: "1bad".into(), value: Box::new(UnitValueType::Unit) }
                .validate(),
            Err(UnitInterfaceError)
        );
    }
}
