use super::*;
use crate::{IntegerArithmetic, IntegerLiteral};

const I64: ScalarType = ScalarType::Integer(IntegerType::Int64);

fn addition() -> PrimitiveOp {
    PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Add)
}

#[test]
fn exact_bits_survive_checked_round_trips() {
    for ty in [IntegerType::Int64, IntegerType::UInt64] {
        let scalar = ScalarType::Integer(ty);
        let program = ScalarRegion {
            inputs: vec![scalar],
            output: scalar,
            steps: vec![
                ScalarStep::Decode { ty: scalar, value: ScalarId(0) },
                ScalarStep::Encode { ty: scalar, raw: ScalarId(1) },
            ],
            result: ScalarId(2),
        }
        .verify()
        .unwrap();
        for bits in [0, 1, 0x1000, i64::MAX as u64, i64::MIN as u64, u64::MAX] {
            let literal = Literal::Integer(match ty {
                | IntegerType::Int64 => IntegerLiteral::Int64(bits as i64),
                | IntegerType::UInt64 => IntegerLiteral::UInt64(bits),
                | _ => unreachable!(),
            });
            assert_eq!(program.evaluate(std::slice::from_ref(&literal)).unwrap(), literal);
        }
        assert_eq!(program.allocation_count(), 1);
    }
}

#[test]
fn verifier_rejects_wrong_representation_type_and_definition_order() {
    let baseline = ScalarProgram::primitive(addition());
    let mut bad = baseline.region().clone();
    bad.steps[2] =
        ScalarStep::Arithmetic { operation: addition(), operands: [ScalarId(0), ScalarId(3)] };
    assert!(matches!(
        bad.verify(),
        Err(ScalarError::Representation {
            at: ScalarId(4),
            value: ScalarId(0),
            expected: ScalarRepr::Raw(I64),
            found: ScalarRepr::Value(I64),
        })
    ));
    let mut bad = baseline.region().clone();
    bad.steps[0] =
        ScalarStep::Decode { ty: ScalarType::Integer(IntegerType::UInt64), value: ScalarId(0) };
    assert!(matches!(bad.verify(), Err(ScalarError::Representation { at: ScalarId(2), .. })));
    let mut bad = baseline.region().clone();
    bad.steps[0] = ScalarStep::Decode { ty: I64, value: ScalarId(5) };
    assert!(matches!(
        bad.verify(),
        Err(ScalarError::Unavailable { at: ScalarId(2), value: ScalarId(5) })
    ));
}

#[test]
fn ordinary_exit_requires_an_encoded_value() {
    let baseline = ScalarProgram::primitive(addition());
    assert_eq!(baseline.result_type(), I64);
    let mut bad = baseline.region().clone();
    bad.result = ScalarId(4);
    assert!(matches!(
        bad.verify(),
        Err(ScalarError::RawResult { value: ScalarId(4), found: ScalarRepr::Raw(I64) })
    ));
    let mut bad = baseline.region().clone();
    bad.output = ScalarType::Integer(IntegerType::UInt64);
    assert!(matches!(bad.verify(), Err(ScalarError::Representation { value: ScalarId(5), .. })));
}

#[test]
fn box_elimination_preserves_errors_even_when_the_result_is_unused() {
    let program =
        ScalarProgram::primitive(PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Div));
    let mut region = program.region().clone();
    region.result = ScalarId(0);
    let baseline = region.verify().unwrap();
    let optimized = baseline.eliminate_boxes().unwrap();
    assert_eq!(optimized.allocation_count(), 0);
    let inputs = [7, 0].map(|n| Literal::Integer(IntegerLiteral::Int64(n)));
    assert_eq!(baseline.evaluate(&inputs), Err(PrimitiveError::DivisionByZero));
    assert_eq!(optimized.evaluate(&inputs), Err(PrimitiveError::DivisionByZero));
}

#[test]
fn float_boxing_preserves_nan_payloads_and_signed_zero() {
    let ty = ScalarType::Float(FloatType::Float64);
    let program = ScalarRegion {
        inputs: vec![ty],
        output: ty,
        steps: vec![
            ScalarStep::Decode { ty, value: ScalarId(0) },
            ScalarStep::Encode { ty, raw: ScalarId(1) },
        ],
        result: ScalarId(2),
    }
    .verify()
    .unwrap();
    for bits in [0, 1 << 63, 0x7ff0_0000_0000_0000, 0x7ff8_0000_0000_1234, 0xfff8_0000_0000_5678] {
        let value = Literal::Float(crate::FloatLiteral::Float64(bits));
        assert_eq!(program.evaluate(std::slice::from_ref(&value)).unwrap(), value);
    }
}

#[test]
fn native_storage_checks_root_classes_and_distinct_homes() {
    let program = ScalarProgram::primitive(addition());
    let storage = ScalarStorage::for_program(&program);
    assert_eq!((storage.value_words(), storage.raw_words()), (3, 3));
    let mut bad = storage.slots.clone();
    bad.swap(0, 1);
    assert!(matches!(
        ScalarStorage::verify(&program, bad),
        Err(ScalarError::InputStorage { input: ScalarId(0), .. })
    ));
    let mut bad = storage.slots.clone();
    bad[2] = ScalarSlot::Value(2);
    assert!(matches!(
        ScalarStorage::verify(&program, bad),
        Err(ScalarError::StorageClass { value: ScalarId(2), .. })
    ));
    let mut bad = storage.slots.clone();
    bad[5] = ScalarSlot::Raw(0);
    assert!(matches!(
        ScalarStorage::verify(&program, bad),
        Err(ScalarError::StorageClass { value: ScalarId(5), .. })
    ));
    let mut bad = storage.slots.clone();
    bad[3] = ScalarSlot::Raw(0);
    assert!(matches!(
        ScalarStorage::verify(&program, bad),
        Err(ScalarError::StorageSlot { slot: ScalarSlot::Raw(0) })
    ));
}
