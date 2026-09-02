mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, ForeignClassifier, ForeignClassifierError, TyEnv, Tycker, syntax::*};

struct ForeignFixture;

impl ForeignFixture {
    fn primitive(tycker: &mut Tycker<'_>, vtype: KindId, primitive: PrimitiveType) -> TypeId {
        Alloc::alloc(tycker, PrimitiveTy(primitive), vtype, &TyEnv::new())
    }

    fn classifier(
        tycker: &mut Tycker<'_>, vtype: KindId, ctype: KindId, first: PrimitiveType,
    ) -> TypeId {
        let environment = TyEnv::new();
        let thk = Alloc::alloc(tycker, ThkTy, vtype, &environment);
        let ret = Alloc::alloc(tycker, RetTy, ctype, &environment);
        let bytes = Self::primitive(tycker, vtype, first);
        let uint64 = Self::primitive(tycker, vtype, PrimitiveType::Integer(IntegerType::UInt64));
        let result = Alloc::alloc(tycker, App(ret, uint64), ctype, &environment);
        let seed = Alloc::alloc(tycker, Arrow(uint64, result), ctype, &environment);
        let body = Alloc::alloc(tycker, Arrow(bytes, seed), ctype, &environment);
        Alloc::alloc(tycker, App(thk, body), vtype, &environment)
    }

    fn target() -> ForeignTarget {
        ForeignTarget {
            abi: ForeignAbi::C,
            library: ForeignLibraryName::parse("xxhash").unwrap(),
            symbol: ForeignSymbolName::parse("XXH64").unwrap(),
        }
    }
}

#[test]
fn derives_the_xxh64_marshalling_protocol_from_its_cbpv_classifier() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let classifier = ForeignFixture::classifier(tycker, vtype, ctype, PrimitiveType::Bytes);

        let import = ForeignClassifier::new(&tycker.statics)
            .validate(ForeignFixture::target(), classifier)
            .unwrap();

        assert_eq!(
            import.signature.parameters,
            vec![ForeignParameter::BorrowedBytes, ForeignParameter::UInt64]
        );
        assert_eq!(import.signature.result, ForeignResult::UInt64);
    });
}

#[test]
fn rejects_a_foreign_classifier_outside_the_safe_profile() {
    TestFixture::run(|tycker| {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let classifier = ForeignFixture::classifier(
            tycker,
            vtype,
            ctype,
            PrimitiveType::Integer(IntegerType::UInt64),
        );

        let error = ForeignClassifier::new(&tycker.statics)
            .validate(ForeignFixture::target(), classifier)
            .unwrap_err();

        assert!(matches!(error, ForeignClassifierError::Unsupported { .. }));
    });
}
