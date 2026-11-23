use super::*;
use crate::{
    ir::{BuiltinPackageValue, CoreArena, CoreProgram, CoreRoot, syntax as ir},
    protocol::ValueProtocol,
};
use zydeco_foundation::dense::DenseId;

struct Fixture {
    core: CoreArena,
}

impl Fixture {
    fn new() -> Self {
        Self { core: CoreArena::default() }
    }

    fn value(&mut self, value: impl Into<ir::Value>) -> ir::ValueId {
        self.core.values.alloc(value.into())
    }

    fn pattern(&mut self, pattern: ir::ValuePattern) -> ir::VPatId {
        self.core.vpats.alloc(pattern)
    }

    fn computation(&mut self, computation: ir::Computation) -> ir::CompuId {
        self.core.compus.alloc(computation)
    }

    fn dummy_program() -> CoreProgram {
        let mut core = CoreArena::default();
        let hole = core.compus.alloc(ir::Computation::Hole(Hole));
        CoreProgram::new(core, CoreRoot::Compu(hole))
    }

    fn into_program(self, root: ir::CompuId) -> CoreProgram {
        CoreProgram::new(self.core, CoreRoot::Compu(root))
    }

    fn lower(self, root: ir::CompuId) -> FragmentProgram {
        let program = self.into_program(root);
        RootLowerer { scheme: SpsScheme::BranchJoin }.run_infallible(&program)
    }

    // Construct and destroy the fixtures on the same small stack. This isolates
    // residual lowering from parsing, checking, and later optimization passes.
    fn small_stack(test: impl FnOnce() + Send + 'static) {
        std::thread::Builder::new().stack_size(512 * 1024).spawn(test).unwrap().join().unwrap();
    }
}

#[test]
fn only_structural_payloads_use_direct_constructor_arms() {
    let mut fixture = Fixture::new();
    let data = fixture.core.datas.alloc(vec![CtorName::new("+Outer")]);
    let tag = CtorIdx { idx: 0, name: CtorName::new("+Outer") };
    let hole = fixture.pattern(ir::ValuePattern::Hole(Hole));
    let product = fixture
        .pattern(ir::ValuePattern::VCons(VCons::new(vec![hole, hole], ProductLayout { arity: 2 })));
    let direct =
        fixture.pattern(ir::ValuePattern::Ctor { data, tag: tag.clone(), payload: product });
    let nested =
        fixture.pattern(ir::ValuePattern::Ctor { data, tag: tag.clone(), payload: direct });
    let nested_product = fixture.pattern(ir::ValuePattern::VCons(VCons::new(
        vec![hole, direct],
        ProductLayout { arity: 2 },
    )));
    let alias = fixture
        .pattern(ir::ValuePattern::Alias(Alias(ConsN::from_vec(vec![direct, hole]).unwrap())));
    let literal =
        fixture.pattern(ir::ValuePattern::Lit(ir::Literal::Integer(IntegerLiteral::Int(0))));
    let literal_payload = fixture.pattern(ir::ValuePattern::Ctor { data, tag, payload: literal });
    let program = fixture.into_program(ir::CompuId::from_index(0));
    let lowerer = Lowerer::new(&program, SpsScheme::BranchJoin);

    for pattern in [hole, product] {
        assert!(!lowerer.pattern_needs_match_plan(pattern));
        assert!(!lowerer.match_arm_needs_match_plan(pattern));
    }
    assert!(lowerer.pattern_needs_match_plan(direct));
    assert!(!lowerer.match_arm_needs_match_plan(direct));
    for pattern in [nested, nested_product, alias, literal, literal_payload] {
        assert!(lowerer.pattern_needs_match_plan(pattern));
        assert!(lowerer.match_arm_needs_match_plan(pattern));
    }
}

#[test]
fn mixed_reconstruction_and_ordered_match_plans_lower_to_a_valid_fragment() {
    let mut fixture = Fixture::new();
    let unit = fixture.value(ir::Value::Triv(Triv));
    let hole = fixture.pattern(ir::ValuePattern::Hole(Hole));
    let bindee = fixture.value(ir::Value::Let(Let { binder: hole, bindee: unit, tail: unit }));
    let returned = fixture.computation(ir::Computation::Ret(ir::Return(unit)));
    let scrut = fixture.value(ir::Value::Lit(ir::Literal::Integer(IntegerLiteral::Int(0))));
    let mut root = returned;
    for index in 0..12 {
        root = fixture.computation(ir::Computation::Let(Let { binder: hole, bindee, tail: root }));
        root = fixture.computation(ir::Computation::Do(ir::Bind {
            binder: hole,
            bindee: returned,
            tail: root,
        }));
        root = fixture.computation(ir::Computation::VAbs(Abs(hole, root)));
        root = fixture.computation(ir::Computation::VApp(App(root, unit)));
        let thunk = fixture.value(ir::Value::Thunk(Thunk(root)));
        root = fixture.computation(ir::Computation::Force(ir::Force(thunk)));
        let literal = fixture
            .pattern(ir::ValuePattern::Lit(ir::Literal::Integer(IntegerLiteral::Int(index))));
        let binder = fixture
            .pattern(ir::ValuePattern::Alias(Alias(ConsN::from_vec(vec![literal, hole]).unwrap())));
        root = fixture.computation(ir::Computation::Match(ir::Match {
            scrut,
            arms: vec![Matcher { binder, tail: root }, Matcher { binder: hole, tail: returned }],
        }));
    }
    fixture.lower(root);
}

#[test]
fn deeply_nested_computations_and_thunks_lower_on_a_small_stack() {
    Fixture::small_stack(|| {
        let mut fixture = Fixture::new();
        let unit = fixture.value(ir::Value::Triv(Triv));
        let hole = fixture.pattern(ir::ValuePattern::Hole(Hole));
        let returned = fixture.computation(ir::Computation::Ret(ir::Return(unit)));
        let mut root = returned;
        for _ in 0..4096 {
            root = fixture.computation(ir::Computation::Let(Let {
                binder: hole,
                bindee: unit,
                tail: root,
            }));
            root = fixture.computation(ir::Computation::Do(ir::Bind {
                binder: hole,
                bindee: returned,
                tail: root,
            }));
            root = fixture.computation(ir::Computation::VAbs(Abs(hole, root)));
            root = fixture.computation(ir::Computation::VApp(App(root, unit)));
            let thunk = fixture.value(ir::Value::Thunk(Thunk(root)));
            root = fixture.computation(ir::Computation::Force(ir::Force(thunk)));
            root = fixture.computation(ir::Computation::Match(ir::Match {
                scrut: unit,
                arms: vec![Matcher { binder: hole, tail: root }],
            }));
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
        let unit = fixture.value(ir::Value::Triv(Triv));
        let hole = fixture.pattern(ir::ValuePattern::Hole(Hole));
        let mut value = unit;
        let mut pattern = hole;
        for _ in 0..8192 {
            value = fixture.value(ir::Value::Let(Let { binder: hole, bindee: unit, tail: value }));
            pattern = fixture.pattern(ir::ValuePattern::Alias(Alias(
                ConsN::from_vec(vec![pattern, hole]).unwrap(),
            )));
        }
        let tail = fixture.computation(ir::Computation::Ret(ir::Return(unit)));
        let root =
            fixture.computation(ir::Computation::Let(Let { binder: pattern, bindee: value, tail }));
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
        let scrut = fixture.value(ir::Value::Lit(ir::Literal::Integer(IntegerLiteral::Int(-1))));
        let unit = fixture.value(ir::Value::Triv(Triv));
        let tail = fixture.computation(ir::Computation::Ret(ir::Return(unit)));
        let mut arms = (0..4096)
            .map(|index| {
                let binder = fixture.pattern(ir::ValuePattern::Lit(ir::Literal::Integer(
                    IntegerLiteral::Int(index),
                )));
                Matcher { binder, tail }
            })
            .collect::<Vec<_>>();
        arms.push(Matcher { binder: fixture.pattern(ir::ValuePattern::Hole(Hole)), tail });
        let root = fixture.computation(ir::Computation::Match(ir::Match { scrut, arms }));
        let program = fixture.lower(root);
        let program = program.as_program();
        let arena = &program.arena().inner;
        let Computation::LetValue(Let { tail: mut current, .. }) = arena.compus[&program.root()]
        else {
            panic!("match scrutinee binding")
        };
        for index in 0..4096 {
            let Computation::Ext(CompuExt::Force(SForce { mut stack, .. })) =
                arena.compus[&current]
            else {
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
                matches!(arena.values[&arguments[1]], Value::Literal(ir::Literal::Integer(IntegerLiteral::Int(actual))) if actual == index)
            );
            let Value::Ext(ValueExt::Closure(Closure { body, .. })) = arena.values[&arguments[2]]
            else {
                panic!("success closure")
            };
            assert!(matches!(arena.compus[&body], Computation::Ext(CompuExt::Ret(_))));
            let Value::Ext(ValueExt::Closure(Closure { body, .. })) = arena.values[&arguments[3]]
            else {
                panic!("fallthrough closure")
            };
            current = body;
        }
        assert!(
            matches!(arena.compus[&current], Computation::Ext(CompuExt::Ret(_))),
            "final catch-all row"
        );
    });
}

#[test]
fn shared_typed_children_have_fresh_output_ownership_and_source_protocols() {
    let mut fixture = Fixture::new();
    let unit = fixture.value(ir::Value::Triv(Triv));
    fixture.core.value_protocols.insert_new(unit, ValueProtocol::Unit);
    let pair =
        fixture.value(ir::Value::VCons(VCons::new(vec![unit, unit], ProductLayout { arity: 2 })));
    fixture
        .core
        .value_protocols
        .insert_new(pair, ValueProtocol::Product(vec![ValueProtocol::Unit, ValueProtocol::Unit]));
    let root = fixture.computation(ir::Computation::Ret(ir::Return(pair)));
    let program = fixture.lower(root);
    let program = program.as_program();
    let arena = program.arena();
    let Computation::Ext(CompuExt::Ret(SReturn { value, .. })) =
        arena.inner.compus[&program.root()]
    else {
        panic!("return")
    };
    let Value::VCons(VCons { items, .. }) = &arena.inner.values[&value] else { panic!("product") };
    assert_ne!(items[0], items[1]);
    for item in items {
        assert_eq!(arena.admin.term_site(TermId::Value(*item)), Some(ir::TermId::Value(unit)));
        assert_eq!(arena.inner.value_protocols[item], crate::protocol::ValueProtocol::Unit);
    }
    assert_eq!(arena.admin.term_site(TermId::Value(value)), Some(ir::TermId::Value(pair)));
}

#[test]
fn deeply_nested_builtin_products_lower_on_a_small_stack() {
    Fixture::small_stack(|| {
        let value = (0..16384)
            .fold(BuiltinPackageValue::Unit, |value, _| BuiltinPackageValue::Product(vec![value]));
        let core = Fixture::dummy_program();
        let mut lowerer = Lowerer::new(&core, SpsScheme::BranchJoin);
        let value = Explicit::run(&mut BuiltinPackageFolder { lowerer: &mut lowerer }, value);
        let stack = StackVar.build(&mut lowerer, None);
        let root = SReturn { stack, value }.build(&mut lowerer, None);
        let program = lowerer.finish(root);
        assert_eq!(program.as_program().arena().inner.values.len(), 16385);
    });
}

#[test]
fn builtin_folder_preserves_product_order_and_materialization() {
    use zydeco_syntax::{BuiltinValueRole, IntegerOperation, IntegerType};
    let core = Fixture::dummy_program();
    let mut lowerer = Lowerer::new(&core, SpsScheme::BranchJoin);
    let input = BuiltinPackageValue::Product(vec![
        BuiltinPackageValue::Unit,
        BuiltinPackageValue::Operation(BuiltinValueRole::Integer(
            IntegerType::Int,
            IntegerOperation::Eq,
        )),
        BuiltinPackageValue::Product(vec![BuiltinPackageValue::Unit, BuiltinPackageValue::Unit]),
    ]);
    let value = Explicit::run(&mut BuiltinPackageFolder { lowerer: &mut lowerer }, input);
    let arena = &lowerer.arena;
    let Value::VCons(VCons { items, layout }) = &arena.inner.values[&value] else {
        panic!("package product")
    };
    assert_eq!(layout.arity, 3);
    assert!(matches!(&arena.inner.values[&items[0]], Value::Triv(_)));
    assert!(matches!(&arena.inner.values[&items[1]], Value::Ext(ValueExt::Closure(_))));
    assert!(
        matches!(&arena.inner.values[&items[2]], Value::VCons(VCons { items, .. }) if items.len() == 2 && items[0] != items[1])
    );
}

#[test]
fn builtin_folder_rejects_empty_product_plans() {
    let rejected = std::panic::catch_unwind(|| {
        let core = Fixture::dummy_program();
        let mut lowerer = Lowerer::new(&core, SpsScheme::BranchJoin);
        Explicit::run(
            &mut BuiltinPackageFolder { lowerer: &mut lowerer },
            BuiltinPackageValue::Product(vec![]),
        );
    });
    let error = rejected.expect_err("an empty product plan violates the IR invariant");
    let message = error
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| error.downcast_ref::<&str>().copied());
    assert_eq!(message, Some("assertion failed: layout.arity > 0"));
}

#[test]
fn computation_roots_lower_as_single_program_roots() {
    let mut fixture = Fixture::new();
    let value = fixture.value(ir::Value::Triv(Triv));
    let root = fixture.computation(ir::Computation::Ret(ir::Return(value)));
    let stackir = fixture.lower(root);
    let stackir = stackir.as_program();

    assert!(stackir.arena().inner.compus.get(stackir.root()).is_some());
    super::super::check::check(stackir);
}
