use super::*;
use zydeco_statics::arena::StaticsScope;
use zydeco_utils::prelude::IdAllocator;

struct Fixture {
    allocator: IdAllocator<StaticsScope>,
    statics: StaticsArena,
}

impl Fixture {
    fn new() -> Self {
        Self { allocator: IdAllocator::new(), statics: StaticsArena::default() }
    }

    fn value(&mut self, value: impl Into<ss::Value>) -> ss::ValueId {
        let id = self.allocator.alloc();
        self.statics.values.insert_new(id, value.into());
        id
    }

    fn pattern(&mut self, pattern: impl Into<ss::ValuePattern>) -> ss::VPatId {
        let id = self.allocator.alloc();
        self.statics.vpats.insert_new(id, pattern.into());
        id
    }

    fn computation(&mut self, computation: impl Into<ss::Computation>) -> ss::CompuId {
        let id = self.allocator.alloc();
        self.statics.compus.insert_new(id, computation.into());
        id
    }

    fn ty(&mut self, ty: ss::Type) -> ss::TypeId {
        let id = self.allocator.alloc();
        let kind = self.allocator.alloc();
        self.statics.types_pre.insert_new(id, ss::Fillable::Done(ty), kind);
        id
    }

    fn lower(&self, root: ss::CompuId) -> BranchJoinProgram {
        let spans = SpanArena::default();
        let scoped = ScopedArena::default();
        RootLowerer { spans: &spans, scoped: &scoped, statics: &self.statics }.run(root).unwrap()
    }

    // Construct and destroy the fixtures on the same small stack. This isolates
    // residual lowering from parsing, checking, and later optimization passes.
    fn small_stack(test: impl FnOnce() + Send + 'static) {
        std::thread::Builder::new().stack_size(512 * 1024).spawn(test).unwrap().join().unwrap();
    }
}

#[test]
fn deeply_nested_computations_and_thunks_lower_on_a_small_stack() {
    Fixture::small_stack(|| {
        let mut fixture = Fixture::new();
        let unit = fixture.value(Triv);
        let hole = fixture.pattern(Hole);
        let returned = fixture.computation(Return(unit));
        let mut root = returned;
        for _ in 0..4096 {
            root = fixture.computation(Let { binder: hole, bindee: unit, tail: root });
            root = fixture.computation(Bind { binder: hole, bindee: returned, tail: root });
            root = fixture.computation(Abs(hole, root));
            root = fixture.computation(App(root, unit));
            let thunk = fixture.value(Thunk(root));
            root = fixture.computation(Force(thunk));
            root = fixture.computation(Match {
                scrut: unit,
                arms: vec![Matcher { binder: hole, tail: root }],
            });
        }
        let program = fixture.lower(root);
        let arena = &program.as_program().arena().inner;
        assert_eq!(arena.compus.len(), 4096 * 5 + 1);
        assert_eq!(arena.stacks.len(), 4096 * 4 + 1);
        assert_eq!(arena.vpats.len(), 4096 * 4);
        assert_eq!(arena.values.len(), 4096 * 5 + 1);
    });
}

#[test]
fn deeply_nested_values_and_structural_patterns_lower_on_a_small_stack() {
    Fixture::small_stack(|| {
        let mut fixture = Fixture::new();
        let unit = fixture.value(Triv);
        let hole = fixture.pattern(Hole);
        let mut value = unit;
        let mut pattern = hole;
        for _ in 0..8192 {
            value = fixture.value(Let { binder: hole, bindee: unit, tail: value });
            pattern = fixture.pattern(Alias(ConsN::from_vec(vec![pattern, hole]).unwrap()));
        }
        let tail = fixture.computation(Return(unit));
        let root = fixture.computation(Let { binder: pattern, bindee: value, tail });
        let program = fixture.lower(root);
        let arena = &program.as_program().arena().inner;
        assert_eq!(arena.compus.len(), 8192 + 2);
        assert_eq!(arena.vpats.len(), 8192 * 3 + 1);
        assert_eq!(arena.values.len(), 8192 + 2);
    });
}

#[test]
fn long_literal_fallthrough_chains_preserve_row_order_on_a_small_stack() {
    Fixture::small_stack(|| {
        let mut fixture = Fixture::new();
        let scrut = fixture.value(ss::Literal::Integer(IntegerLiteral::Int64(-1)));
        let unit = fixture.value(Triv);
        let tail = fixture.computation(Return(unit));
        let mut arms = (0..4096)
            .map(|index| {
                let binder = fixture.pattern(ss::Literal::Integer(IntegerLiteral::Int64(index)));
                Matcher { binder, tail }
            })
            .collect::<Vec<_>>();
        arms.push(Matcher { binder: fixture.pattern(Hole), tail });
        let root = fixture.computation(Match { scrut, arms });
        let program = fixture.lower(root);
        let program = program.as_program();
        let arena = &program.arena().inner;
        let Computation::Join(LetJoin::Value(Let { tail: mut current, .. })) =
            arena.compus[&program.root()]
        else {
            panic!("match scrutinee binding")
        };
        for index in 0..4096 {
            let Computation::Force(SForce { mut stack, .. }) = arena.compus[&current] else {
                panic!("literal comparison")
            };
            let arguments = (0..4)
                .map(|_| {
                    let Stack::Arg(Cons(value, tail)) = arena.stacks[&stack] else {
                        panic!("comparison argument")
                    };
                    stack = tail;
                    value
                })
                .collect::<Vec<_>>();
            assert!(
                matches!(arena.values[&arguments[1]], Value::Literal(ss::Literal::Integer(IntegerLiteral::Int64(actual))) if actual == index)
            );
            let Value::Closure(Closure { body, .. }) = arena.values[&arguments[2]] else {
                panic!("success closure")
            };
            assert!(matches!(arena.compus[&body], Computation::Ret(_)));
            let Value::Closure(Closure { body, .. }) = arena.values[&arguments[3]] else {
                panic!("fallthrough closure")
            };
            current = body;
        }
        assert!(matches!(arena.compus[&current], Computation::Ret(_)), "final catch-all row");
    });
}

#[test]
fn shared_typed_children_have_fresh_output_ownership_and_source_protocols() {
    let mut fixture = Fixture::new();
    let unit_ty = fixture.ty(ss::Type::Unit(ss::UnitTy));
    let pair_ty = fixture.ty(ss::Type::Prod(Prod(vec![unit_ty, unit_ty])));
    let unit = fixture.value(Triv);
    fixture.statics.annotations_value.insert_new(unit, unit_ty);
    let pair = fixture.value(ss::Value::VCons(vec![unit, unit]));
    fixture.statics.annotations_value.insert_new(pair, pair_ty);
    let root = fixture.computation(Return(pair));
    let program = fixture.lower(root);
    let program = program.as_program();
    let arena = program.arena();
    let Computation::Ret(SReturn { value, .. }) = arena.inner.compus[&program.root()] else {
        panic!("return")
    };
    let Value::VCons(VCons { items, .. }) = &arena.inner.values[&value] else { panic!("product") };
    assert_ne!(items[0], items[1]);
    for item in items {
        assert_eq!(arena.admin.terms.back(&(*item).into()), Some(&unit.into()));
        assert_eq!(arena.inner.value_protocols[item], crate::protocol::ValueProtocol::Unit);
    }
    assert_eq!(arena.admin.terms.back(&value.into()), Some(&pair.into()));
}

#[test]
fn independent_invalid_values_are_collected_without_publishing_a_program() {
    let mut fixture = Fixture::new();
    let unit = fixture.value(Triv);
    let hole = fixture.pattern(Hole);
    let first = fixture.value(Abs(ss::ValBinder::Value(hole), unit));
    let second = fixture.value(App(first, ss::ValArgument::Value(unit)));
    let tail = fixture.computation(Return(second));
    let root = fixture.computation(Let { binder: hole, bindee: first, tail });
    let spans = SpanArena::default();
    let scoped = ScopedArena::default();
    let errors = RootLowerer { spans: &spans, scoped: &scoped, statics: &fixture.statics }
        .run(root)
        .unwrap_err();
    assert!(
        matches!(errors.as_slice(), [SpsLowerError::ResidualStaticValue { value: a }, SpsLowerError::ResidualStaticValue { value: b }] if *a == first && *b == second)
    );
    fixture.statics.values[&first] = ss::Value::Triv(Triv);
    fixture.statics.values[&second] = ss::Value::Triv(Triv);
    fixture.lower(root);
}

#[test]
fn deeply_nested_builtin_products_lower_on_a_small_stack() {
    Fixture::small_stack(|| {
        let value = (0..16384)
            .fold(BuiltinPackageValue::Unit, |value, _| BuiltinPackageValue::Product(vec![value]));
        let spans = SpanArena::default();
        let scoped = ScopedArena::default();
        let statics = StaticsArena::default();
        let mut lowerer = Lowerer::new(&spans, &scoped, &statics);
        let value = BuiltinPackageLowering::lower(value, &mut lowerer);
        let stack = Bullet.build(&mut lowerer, None);
        let root = SReturn { stack, value }.build(&mut lowerer, None);
        let program = lowerer.finish(root).unwrap();
        assert_eq!(program.as_program().arena().inner.values.len(), 16385);
    });
}

#[test]
fn computation_roots_lower_as_single_program_roots() {
    let mut allocator = IdAllocator::<StaticsScope>::new();
    let value = allocator.alloc();
    let root = allocator.alloc();
    let mut statics = StaticsArena::default();
    statics.values.insert_new(value, ss::Triv.into());
    statics.compus.insert_new(root, ss::Return(value).into());
    let spans = SpanArena::default();
    let scoped = ScopedArena::default();

    let stackir =
        RootLowerer { spans: &spans, scoped: &scoped, statics: &statics }.run(root).unwrap();
    let stackir = stackir.as_program();

    assert!(stackir.arena().inner.compus.get(&stackir.root()).is_some());
    super::super::check::check(stackir, &scoped, &statics);
}

#[test]
fn residual_value_functions_report_an_internal_invariant_failure() {
    let mut allocator = IdAllocator::<StaticsScope>::new();
    let unit = allocator.alloc();
    let pattern = allocator.alloc();
    let abstraction = allocator.alloc();
    let root = allocator.alloc();
    let valid_root = allocator.alloc();
    let mut statics = StaticsArena::default();
    statics.values.insert_new(unit, ss::Triv.into());
    statics.vpats.insert_new(pattern, ss::ValuePattern::Triv(ss::Triv));
    statics.values.insert_new(abstraction, ss::Abs(ss::ValBinder::Value(pattern), unit).into());
    statics.compus.insert_new(root, ss::Return(abstraction).into());
    statics.compus.insert_new(valid_root, ss::Return(unit).into());
    let spans = SpanArena::default();
    let scoped = ScopedArena::default();

    let mut passes = zydeco_utils::pipeline![
        RootLowerer { spans: &spans, scoped: &scoped, statics: &statics },
        crate::SpsLowPipeline { scoped: &scoped, statics: &statics }.with_error(),
    ];
    let before = passes.run(valid_root).expect("a complete root reaches SPSLow");
    let errors = passes.run(root).expect_err("unelaborated static syntax cannot lower");
    assert!(
        matches!(errors.as_slice(), [SpsLowerError::ResidualStaticValue { value }] if *value == abstraction)
    );
    // Internal fixtures need a useful report even without source spans.
    let _ = errors[0].to_report(&spans, &scoped, &statics);
    let after = passes.run(valid_root).expect("failed lowering leaves no state in the next run");
    assert_eq!(before.arena().inner.compus.len(), after.arena().inner.compus.len());
}
