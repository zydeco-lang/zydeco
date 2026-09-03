mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, ForeignClassifier, ForeignClassifierError, TyEnv, Tycker, syntax::*};

struct ForeignFixture;

impl ForeignFixture {
    fn classifier(
        tycker: &mut Tycker<'_>, parameters: &[PrimitiveType], result: PrimitiveType,
    ) -> TypeId {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let environment = TyEnv::new();
        let thk_kind = Alloc::alloc(tycker, Arrow(ctype, vtype), (), &());
        let ret_kind = Alloc::alloc(tycker, Arrow(vtype, ctype), (), &());
        let thk = Alloc::alloc(tycker, ThkTy, thk_kind, &environment);
        let ret = Alloc::alloc(tycker, RetTy, ret_kind, &environment);
        let result = Alloc::alloc(tycker, PrimitiveTy(result), vtype, &environment);
        let result = Alloc::alloc(tycker, App(ret, result), ctype, &environment);
        let body = parameters.iter().rev().fold(result, |body, &primitive| {
            let parameter = Alloc::alloc(tycker, PrimitiveTy(primitive), vtype, &environment);
            Alloc::alloc(tycker, Arrow(parameter, body), ctype, &environment)
        });
        Alloc::alloc(tycker, App(thk, body), vtype, &environment)
    }

    fn target() -> ForeignTarget {
        ForeignTarget {
            abi: ForeignAbi::C,
            library: ForeignLibraryName::parse("fixture").unwrap(),
            symbol: ForeignSymbolName::parse("call").unwrap(),
        }
    }
}

const U64: PrimitiveType = PrimitiveType::Integer(IntegerType::UInt64);
const U32: PrimitiveType = PrimitiveType::Integer(IntegerType::UInt32);
const BYTES: PrimitiveType = PrimitiveType::Bytes;

#[test]
fn derives_signatures_compositionally_in_source_order() {
    for parameters in [
        vec![],
        vec![U64],
        vec![BYTES],
        vec![BYTES, U64],
        vec![U64, BYTES, BYTES, U64],
        vec![BYTES, BYTES, BYTES],
        vec![U64; 6],
    ] {
        TestFixture::run(|tycker| {
            let classifier = ForeignFixture::classifier(tycker, &parameters, U64);
            let import = ForeignClassifier::new(&tycker.statics)
                .validate(ForeignFixture::target(), classifier)
                .unwrap();
            let expected = parameters
                .iter()
                .map(|parameter| match *parameter {
                    | BYTES => ForeignParameter::BorrowedBytes,
                    | U64 => ForeignParameter::UInt64,
                    | _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            assert_eq!(import.signature.parameters(), expected);
            assert_eq!(import.signature.result(), ForeignResult::UInt64);
        });
    }
}

#[test]
fn call_plan_expands_bytes_at_their_source_positions() {
    let signature = ForeignSignature::new(
        vec![ForeignParameter::UInt64, ForeignParameter::BorrowedBytes, ForeignParameter::UInt64],
        ForeignResult::UInt64,
    )
    .unwrap();
    assert_eq!(
        signature.arguments().collect::<Vec<_>>(),
        [
            ForeignArgument { parameter: 0, component: ForeignComponent::UInt64 },
            ForeignArgument { parameter: 1, component: ForeignComponent::BytesPointer },
            ForeignArgument { parameter: 1, component: ForeignComponent::BytesLength },
            ForeignArgument { parameter: 2, component: ForeignComponent::UInt64 },
        ]
    );
}

#[test]
fn rejects_unsupported_parameters_and_results() {
    TestFixture::run(|tycker| {
        let classifier = ForeignFixture::classifier(tycker, &[BYTES, U32], U64);
        assert!(matches!(
            ForeignClassifier::new(&tycker.statics).validate(ForeignFixture::target(), classifier),
            Err(ForeignClassifierError::UnsupportedParameter { index: 2, .. })
        ));
        let classifier = ForeignFixture::classifier(tycker, &[BYTES, U64], U32);
        assert!(matches!(
            ForeignClassifier::new(&tycker.statics).validate(ForeignFixture::target(), classifier),
            Err(ForeignClassifierError::UnsupportedResult { .. })
        ));
    });
}

#[test]
fn argument_limit_counts_flattened_c_arguments_not_source_parameters() {
    TestFixture::run(|tycker| {
        let valid = ForeignFixture::classifier(tycker, &[BYTES, BYTES, BYTES], U64);
        assert!(
            ForeignClassifier::new(&tycker.statics)
                .validate(ForeignFixture::target(), valid)
                .is_ok()
        );
        let invalid = ForeignFixture::classifier(tycker, &[BYTES, BYTES, BYTES, U64], U64);
        assert!(matches!(
            ForeignClassifier::new(&tycker.statics).validate(ForeignFixture::target(), invalid),
            Err(ForeignClassifierError::Signature(ForeignSignatureError::TooManyArguments {
                found: 7,
                maximum: 6,
            }))
        ));
    });
}
