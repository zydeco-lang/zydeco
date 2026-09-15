use super::*;

#[derive(Default)]
struct Fixture {
    arena: SpsLowArena,
}

impl Fixture {
    fn definition(&mut self, name: &str) -> DefId {
        let def = self.arena.admin.fresh_def();
        self.arena.admin.insert_def(def, name.into());
        def
    }

    fn literal(&mut self, value: i64) -> ValueId {
        Literal::Integer(IntegerLiteral::Int64(value)).build(&mut self.arena, None)
    }

    fn chain(shared: bool, separator: bool) -> (SpsLowProgram, CompuId, ValueId, ValueId) {
        let mut fixture = Self::default();
        let sum = fixture.definition("sum");
        let product = fixture.definition("product");
        let maximum = fixture.literal(i64::MAX);
        let one = fixture.literal(1);
        let first = Primitive {
            operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Add),
            operands: [maximum, one],
        }
        .build(&mut fixture.arena, None);
        let sum_use = sum.build(&mut fixture.arena, None);
        let two = fixture.literal(2);
        let second = Primitive {
            operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Mul),
            operands: [sum_use, two],
        }
        .build(&mut fixture.arena, None);
        let mut stack = Bullet.build(&mut fixture.arena, None);
        if shared {
            let value: ValueId = sum.build(&mut fixture.arena, None);
            stack = Cons(value, stack).build(&mut fixture.arena, None);
        }
        let value: ValueId = product.build(&mut fixture.arena, None);
        let stack = Cons(value, stack).build(&mut fixture.arena, None);
        let tail = SHole(stack).build(&mut fixture.arena, None);
        let binder = product.build(&mut fixture.arena, None);
        let mut tail = LetValue { binder, bindee: second, tail }.build(&mut fixture.arena, None);
        if separator {
            let binder = Hole.build(&mut fixture.arena, None);
            let bindee = Triv.build(&mut fixture.arena, None);
            tail = LetValue { binder, bindee, tail }.build(&mut fixture.arena, None);
        }
        let binder = sum.build(&mut fixture.arena, None);
        let root = LetValue { binder, bindee: first, tail }.build(&mut fixture.arena, None);
        (SpsLowProgram::try_new(fixture.arena, root).unwrap(), root, first, second)
    }
}

#[test]
fn adjacent_single_use_bindings_remove_one_box_and_preserve_wrapping() {
    let (program, first_binding, first, second) = Fixture::chain(false, false);
    let keep = ScalarPlans::new(&program, ScalarBoxing::Keep);
    let fused = ScalarPlans::new(&program, ScalarBoxing::Eliminate);
    assert!(!keep.elided(first_binding));
    assert_eq!(
        keep.call(first).program.allocation_count() + keep.call(second).program.allocation_count(),
        2
    );
    assert!(fused.elided(first_binding));
    let call = fused.call(second);
    assert_eq!(call.program.allocation_count(), 1);
    let inputs = call
        .inputs
        .iter()
        .map(|id| {
            let Value::Literal(literal) = &program.arena().inner.values[id] else {
                panic!("fused inputs are original literals")
            };
            literal.clone()
        })
        .collect::<Vec<_>>();
    assert_eq!(call.program.evaluate(&inputs).unwrap(), Literal::Integer(IntegerLiteral::Int64(0)));
}

#[test]
fn shared_bindings_and_intervening_constructions_keep_value_boundaries() {
    for (shared, separator) in [(true, false), (false, true)] {
        let (program, first_binding, _, second) = Fixture::chain(shared, separator);
        let plans = ScalarPlans::new(&program, ScalarBoxing::Eliminate);
        assert!(!plans.elided(first_binding));
        assert_eq!(plans.call(second).program.region().inputs.len(), 2);
    }
}

#[test]
fn long_chains_split_at_the_region_bound() {
    let mut fixture = Fixture::default();
    let definitions =
        (0..40).map(|index| fixture.definition(&format!("x{index}"))).collect::<Vec<_>>();
    let mut values = Vec::new();
    for (index, _) in definitions.iter().enumerate() {
        let left = if index == 0 {
            fixture.literal(0)
        } else {
            definitions[index - 1].build(&mut fixture.arena, None)
        };
        let right = fixture.literal(1);
        values.push(
            Primitive {
                operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Add),
                operands: [left, right],
            }
            .build(&mut fixture.arena, None),
        );
    }
    let result: ValueId = definitions[39].build(&mut fixture.arena, None);
    let ambient = Bullet.build(&mut fixture.arena, None);
    let stack = Cons(result, ambient).build(&mut fixture.arena, None);
    let mut root = SHole(stack).build(&mut fixture.arena, None);
    for (&definition, &bindee) in definitions.iter().zip(&values).rev() {
        let binder = definition.build(&mut fixture.arena, None);
        root = LetValue { binder, bindee, tail: root }.build(&mut fixture.arena, None);
    }
    let program = SpsLowProgram::try_new(fixture.arena, root).unwrap();
    let plans = ScalarPlans::new(&program, ScalarBoxing::Eliminate);
    assert_eq!(plans.elided.len(), 38);
    for (value, operations) in [(values[31], 32), (values[39], 8)] {
        let region = &plans.call(value).program;
        assert_eq!(region.allocation_count(), 1);
        assert_eq!(
            region
                .region()
                .steps
                .iter()
                .filter(|step| matches!(step, ScalarStep::Arithmetic { .. }))
                .count(),
            operations
        );
    }
}
