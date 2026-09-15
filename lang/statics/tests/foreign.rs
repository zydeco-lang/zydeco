mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, ForeignClassifier, ForeignClassifierError, TyEnv, Tycker, syntax::*};

#[derive(Clone, Copy)]
enum Parameter {
    Scalar(PrimitiveType),
    Address,
    UntrustedAddress,
    AddressPair,
}

impl Parameter {
    fn classifier(self, tycker: &mut Tycker<'_>, vtype: KindId) -> TypeId {
        let environment = TyEnv::new();
        if let Self::Scalar(primitive) = self {
            return Alloc::alloc(tycker, PrimitiveTy(primitive), vtype, &environment);
        }
        let witness: AbstId = Alloc::alloc(tycker, None::<DefId>, vtype, &());
        if !matches!(self, Self::UntrustedAddress) {
            tycker.statics.builtin_roles.attach_type(witness, BuiltinTypeRole::Addr).unwrap();
        }
        let address = Alloc::alloc(tycker, witness, vtype, &environment);
        if matches!(self, Self::AddressPair) {
            Alloc::alloc(tycker, Prod(vec![address, address]), vtype, &environment)
        } else {
            address
        }
    }
}

struct ForeignFixture;

impl ForeignFixture {
    fn classifier(
        tycker: &mut Tycker<'_>, parameters: &[Parameter], result: PrimitiveType,
    ) -> TypeId {
        let (vtype, ctype) = TestFixture::kinds(tycker);
        let environment = TyEnv::new();
        let thk_kind = Alloc::alloc(tycker, Arrow(ctype, vtype), (), &());
        let ret_kind = Alloc::alloc(tycker, Arrow(vtype, ctype), (), &());
        let thk = Alloc::alloc(tycker, ThkTy, thk_kind, &environment);
        let ret = Alloc::alloc(tycker, RetTy, ret_kind, &environment);
        let result = Alloc::alloc(tycker, PrimitiveTy(result), vtype, &environment);
        let result = Alloc::alloc(tycker, App(ret, result), ctype, &environment);
        let body = parameters.iter().rev().fold(result, |body, &parameter| {
            let parameter = parameter.classifier(tycker, vtype);
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

const U64: PrimitiveType = PrimitiveType::Integer(IntegerType::UInt);
const F32: PrimitiveType = PrimitiveType::Float(FloatType::Float32);
const U: Parameter = Parameter::Scalar(U64);
const W: Parameter = Parameter::Address;

#[test]
fn derives_signatures_compositionally_in_source_order() {
    for parameters in
        [vec![], vec![U], vec![W], vec![W, U], vec![U, W, U, W, U, U], vec![W; 6], vec![U; 6]]
    {
        TestFixture::run(|tycker| {
            let classifier = ForeignFixture::classifier(tycker, &parameters, U64);
            let import = ForeignClassifier::new(&tycker.statics)
                .validate(ForeignFixture::target(), classifier)
                .unwrap();
            let expected = parameters
                .iter()
                .map(|parameter| match parameter {
                    | Parameter::Address => ForeignParameter::Address,
                    | Parameter::Scalar(U64) => ForeignParameter::Integer(IntegerType::UInt),
                    | _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            assert_eq!(import.signature.parameters(), expected);
            assert_eq!(import.signature.result(), ForeignResult::Integer(IntegerType::UInt));
        });
    }
}

#[test]
fn an_address_supplies_one_pointer_and_length_remains_an_explicit_argument() {
    let signature = ForeignSignature::new(
        vec![
            ForeignParameter::Integer(IntegerType::UInt),
            ForeignParameter::Address,
            ForeignParameter::Integer(IntegerType::UInt),
        ],
        ForeignResult::Integer(IntegerType::UInt),
    )
    .unwrap();
    assert_eq!(
        signature.arguments().collect::<Vec<_>>(),
        [
            ForeignArgument {
                parameter: 0,
                component: ForeignComponent::Integer(IntegerType::UInt)
            },
            ForeignArgument { parameter: 1, component: ForeignComponent::MemoryPointer },
            ForeignArgument {
                parameter: 2,
                component: ForeignComponent::Integer(IntegerType::UInt)
            },
        ]
    );
}

#[test]
fn rejects_unsupported_parameters_results_and_forged_address_shapes() {
    TestFixture::run(|tycker| {
        for parameter in
            [Parameter::Scalar(F32), Parameter::UntrustedAddress, Parameter::AddressPair]
        {
            let classifier = ForeignFixture::classifier(tycker, &[W, parameter], U64);
            assert!(matches!(
                ForeignClassifier::new(&tycker.statics)
                    .validate(ForeignFixture::target(), classifier),
                Err(ForeignClassifierError::UnsupportedParameter { index: 2, .. })
            ));
        }
        let classifier = ForeignFixture::classifier(tycker, &[W, U], F32);
        assert!(matches!(
            ForeignClassifier::new(&tycker.statics).validate(ForeignFixture::target(), classifier),
            Err(ForeignClassifierError::UnsupportedResult { .. })
        ));
    });
}

#[test]
fn the_register_limit_counts_each_address_once() {
    TestFixture::run(|tycker| {
        let valid = ForeignFixture::classifier(tycker, &[W; 6], U64);
        assert!(
            ForeignClassifier::new(&tycker.statics)
                .validate(ForeignFixture::target(), valid)
                .is_ok()
        );
        let invalid = ForeignFixture::classifier(tycker, &[W; 7], U64);
        assert!(matches!(
            ForeignClassifier::new(&tycker.statics).validate(ForeignFixture::target(), invalid),
            Err(ForeignClassifierError::Signature(ForeignSignatureError::TooManyArguments {
                found: 7,
                maximum: 6
            }))
        ));
    });
}
