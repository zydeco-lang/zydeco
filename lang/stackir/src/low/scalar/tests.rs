use super::*;
use crate::SpsScheme;

#[derive(Default)]
struct Fixture {
    arena: SpsLowArena,
}

impl Fixture {
    fn definition(&mut self, name: &str) -> DefId {
        self.arena.admin.alloc_def(name.into())
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
        let mut stack = StackVar.build(&mut fixture.arena, None);
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
        (
            SpsLowProgram::try_new(fixture.arena, root, SpsScheme::BranchJoin).unwrap(),
            root,
            first,
            second,
        )
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
    let ambient = StackVar.build(&mut fixture.arena, None);
    let stack = Cons(result, ambient).build(&mut fixture.arena, None);
    let mut root = SHole(stack).build(&mut fixture.arena, None);
    for (&definition, &bindee) in definitions.iter().zip(&values).rev() {
        let binder = definition.build(&mut fixture.arena, None);
        root = LetValue { binder, bindee, tail: root }.build(&mut fixture.arena, None);
    }
    let program = SpsLowProgram::try_new(fixture.arena, root, SpsScheme::BranchJoin).unwrap();
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

#[test]
fn multi_use_binding_in_chain_fuses_with_one_box() {
    // let y = 7 + 1 in let z = y * y in _ ! z
    let mut fixture = Fixture::default();
    let y = fixture.definition("y");
    let z = fixture.definition("z");
    let seven = fixture.literal(7);
    let one = fixture.literal(1);
    let first = Primitive {
        operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Add),
        operands: [seven, one],
    }
    .build(&mut fixture.arena, None);
    let y_use_left = y.build(&mut fixture.arena, None);
    let y_use_right = y.build(&mut fixture.arena, None);
    let second = Primitive {
        operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Mul),
        operands: [y_use_left, y_use_right],
    }
    .build(&mut fixture.arena, None);
    let z_use: ValueId = z.build(&mut fixture.arena, None);
    let ambient = StackVar.build(&mut fixture.arena, None);
    let stack = Cons(z_use, ambient).build(&mut fixture.arena, None);
    let hole = SHole(stack).build(&mut fixture.arena, None);
    let z_binder = z.build(&mut fixture.arena, None);
    let inner =
        LetValue { binder: z_binder, bindee: second, tail: hole }.build(&mut fixture.arena, None);
    let y_binder = y.build(&mut fixture.arena, None);
    let root =
        LetValue { binder: y_binder, bindee: first, tail: inner }.build(&mut fixture.arena, None);
    let program = SpsLowProgram::try_new(fixture.arena, root, SpsScheme::BranchJoin).unwrap();
    let plans = ScalarPlans::new(&program, ScalarBoxing::Eliminate);
    assert!(plans.elided(root));
    let call = plans.call(second);
    assert_eq!(call.program.allocation_count(), 1);
    let inputs = call
        .inputs
        .iter()
        .map(|id| {
            let Value::Literal(literal) = &program.arena().inner.values[id] else { unreachable!() };
            literal.clone()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        call.program.evaluate(&inputs).unwrap(),
        Literal::Integer(IntegerLiteral::Int64(64))
    );
}

#[test]
fn multi_use_transitive_chain_elides_all_intermediate_boxes() {
    // let y = 3 + 1 in let t = y + 2 in let r = t * y in _ ! r
    let mut fixture = Fixture::default();
    let y = fixture.definition("y");
    let t = fixture.definition("t");
    let r = fixture.definition("r");
    let three = fixture.literal(3);
    let one = fixture.literal(1);
    let e1 = Primitive {
        operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Add),
        operands: [three, one],
    }
    .build(&mut fixture.arena, None);
    let y_u1 = y.build(&mut fixture.arena, None);
    let two = fixture.literal(2);
    let e2 = Primitive {
        operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Add),
        operands: [y_u1, two],
    }
    .build(&mut fixture.arena, None);
    let t_u1 = t.build(&mut fixture.arena, None);
    let y_u2 = y.build(&mut fixture.arena, None);
    let e3 = Primitive {
        operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Mul),
        operands: [t_u1, y_u2],
    }
    .build(&mut fixture.arena, None);
    let r_u: ValueId = r.build(&mut fixture.arena, None);
    let ambient = StackVar.build(&mut fixture.arena, None);
    let stack = Cons(r_u, ambient).build(&mut fixture.arena, None);
    let hole = SHole(stack).build(&mut fixture.arena, None);
    let r_pat = r.build(&mut fixture.arena, None);
    let c3 = LetValue { binder: r_pat, bindee: e3, tail: hole }.build(&mut fixture.arena, None);
    let t_pat = t.build(&mut fixture.arena, None);
    let c2 = LetValue { binder: t_pat, bindee: e2, tail: c3 }.build(&mut fixture.arena, None);
    let y_pat = y.build(&mut fixture.arena, None);
    let root = LetValue { binder: y_pat, bindee: e1, tail: c2 }.build(&mut fixture.arena, None);
    let program = SpsLowProgram::try_new(fixture.arena, root, SpsScheme::BranchJoin).unwrap();
    let plans = ScalarPlans::new(&program, ScalarBoxing::Eliminate);
    assert!(plans.elided(root));
    assert!(plans.elided(c2));
    assert_eq!(plans.call(e3).program.allocation_count(), 1);
}

#[test]
fn escaping_multi_use_binding_rolls_back_without_eliding() {
    // let y = 7 + 1 in let z = y * y in _ ! (y, z)
    let mut fixture = Fixture::default();
    let y = fixture.definition("y");
    let z = fixture.definition("z");
    let seven = fixture.literal(7);
    let one = fixture.literal(1);
    let first = Primitive {
        operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Add),
        operands: [seven, one],
    }
    .build(&mut fixture.arena, None);
    let y1 = y.build(&mut fixture.arena, None);
    let y2 = y.build(&mut fixture.arena, None);
    let second = Primitive {
        operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Mul),
        operands: [y1, y2],
    }
    .build(&mut fixture.arena, None);
    let y3: ValueId = y.build(&mut fixture.arena, None);
    let z1: ValueId = z.build(&mut fixture.arena, None);
    let pair = VCons::new(vec![y3, z1], ProductLayout { arity: 2 }).build(&mut fixture.arena, None);
    let ambient = StackVar.build(&mut fixture.arena, None);
    let stack = Cons(pair, ambient).build(&mut fixture.arena, None);
    let hole = SHole(stack).build(&mut fixture.arena, None);
    let z_pat = z.build(&mut fixture.arena, None);
    let inner =
        LetValue { binder: z_pat, bindee: second, tail: hole }.build(&mut fixture.arena, None);
    let y_pat = y.build(&mut fixture.arena, None);
    let root =
        LetValue { binder: y_pat, bindee: first, tail: inner }.build(&mut fixture.arena, None);
    let program = SpsLowProgram::try_new(fixture.arena, root, SpsScheme::BranchJoin).unwrap();
    let plans = ScalarPlans::new(&program, ScalarBoxing::Eliminate);
    assert!(!plans.elided(root));
}

#[test]
fn multi_use_memory_kernel_fuses_load_square_store_without_boxing() {
    let mut fixture = Fixture::default();
    let addr = fixture.definition("addr");
    let x = fixture.definition("x");
    let load_addr = addr.build(&mut fixture.arena, None);
    let store_addr = addr.build(&mut fixture.arena, None);
    let x1 = x.build(&mut fixture.arena, None);
    let x2 = x.build(&mut fixture.arena, None);
    let squared = Primitive {
        operation: PrimitiveOp::Integer(IntegerType::Int64, IntegerArithmetic::Mul),
        operands: [x1, x2],
    }
    .build(&mut fixture.arena, None);
    let ambient = StackVar.build(&mut fixture.arena, None);
    let done = SHole(ambient).build(&mut fixture.arena, None);
    let store = MemoryStep::Store {
        scalar: crate::syntax::memory::MemoryScalar::Integer(IntegerType::Int64),
        address: store_addr,
        value: squared,
        next: done,
    }
    .build(&mut fixture.arena, None);
    let x_pat = x.build(&mut fixture.arena, None);
    let load = MemoryStep::Load {
        scalar: crate::syntax::memory::MemoryScalar::Integer(IntegerType::Int64),
        address: load_addr,
        result: x_pat,
        next: store,
    }
    .build(&mut fixture.arena, None);
    let addr_pat = addr.build(&mut fixture.arena, None);
    let addr_stack = StackVar.build(&mut fixture.arena, None);
    let root = LetArg { binder: Cons(addr_pat, StackVar), bindee: addr_stack, tail: load }
        .build(&mut fixture.arena, None);
    let program = SpsLowProgram::try_new(fixture.arena, root, SpsScheme::BranchJoin).unwrap();
    let plans = ScalarPlans::new(&program, ScalarBoxing::Eliminate);
    assert!(plans.memory(load).is_some());
}
