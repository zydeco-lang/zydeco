mod common;

use common::TestFixture;
use zydeco_statics::{Alloc, ForeignClassifier, ForeignClassifierError, TyEnv, Tycker, syntax::*};

#[derive(Clone, Copy)]
enum Parameter {
    Scalar(PrimitiveType),
    Window,
    UntrustedWindow,
    SwappedWindow,
    UnsignedLength,
}

impl Parameter {
    fn classifier(self, tycker: &mut Tycker<'_>, vtype: KindId) -> TypeId {
        let environment = TyEnv::new();
        if let Self::Scalar(primitive) = self {
            return Alloc::alloc(tycker, PrimitiveTy(primitive), vtype, &environment);
        }
        let fields = [BuiltinTypeRole::Access, BuiltinTypeRole::Addr].map(|role| {
            let witness: AbstId = Alloc::alloc(tycker, None::<DefId>, vtype, &());
            if !matches!(self, Self::UntrustedWindow) {
                tycker.statics.builtin_roles.attach_type(witness, role).unwrap();
            }
            Alloc::alloc(tycker, witness, vtype, &environment)
        });
        let [access, address] =
            if matches!(self, Self::SwappedWindow) { [fields[1], fields[0]] } else { fields };
        let length = if matches!(self, Self::UnsignedLength) {
            IntegerType::UInt64
        } else {
            IntegerType::Int64
        };
        let length =
            Alloc::alloc(tycker, PrimitiveTy(PrimitiveType::Integer(length)), vtype, &environment);
        Alloc::alloc(tycker, Prod(vec![access, address, length]), vtype, &environment)
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

const U64: PrimitiveType = PrimitiveType::Integer(IntegerType::UInt64);
const F32: PrimitiveType = PrimitiveType::Float(FloatType::Float32);
const U: Parameter = Parameter::Scalar(U64);
const W: Parameter = Parameter::Window;

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
                    | Parameter::Window => ForeignParameter::BorrowedMemory,
                    | Parameter::Scalar(U64) => ForeignParameter::Integer(IntegerType::UInt64),
                    | _ => unreachable!(),
                })
                .collect::<Vec<_>>();
            assert_eq!(import.signature.parameters(), expected);
            assert_eq!(import.signature.result(), ForeignResult::Integer(IntegerType::UInt64));
        });
    }
}

#[test]
fn a_window_supplies_one_pointer_and_length_remains_an_explicit_argument() {
    let signature = ForeignSignature::new(
        vec![
            ForeignParameter::Integer(IntegerType::UInt64),
            ForeignParameter::BorrowedMemory,
            ForeignParameter::Integer(IntegerType::UInt64),
        ],
        ForeignResult::Integer(IntegerType::UInt64),
    )
    .unwrap();
    assert_eq!(
        signature.arguments().collect::<Vec<_>>(),
        [
            ForeignArgument {
                parameter: 0,
                component: ForeignComponent::Integer(IntegerType::UInt64)
            },
            ForeignArgument { parameter: 1, component: ForeignComponent::MemoryPointer },
            ForeignArgument {
                parameter: 2,
                component: ForeignComponent::Integer(IntegerType::UInt64)
            },
        ]
    );
}

#[test]
fn rejects_unsupported_parameters_results_and_forged_window_shapes() {
    TestFixture::run(|tycker| {
        for parameter in [
            Parameter::Scalar(F32),
            Parameter::UntrustedWindow,
            Parameter::SwappedWindow,
            Parameter::UnsignedLength,
        ] {
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
fn the_register_limit_counts_each_window_once() {
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
