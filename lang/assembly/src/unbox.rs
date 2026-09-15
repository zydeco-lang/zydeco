//! Local representation analysis for first-order SPSLow.
//!
//! The analysis marks values and patterns that can avoid a region-allocated cell. A value
//! that is constructed and consumed by a single projection-like use can be
//! represented by its fields directly. A variable bound to such a value can also
//! be expanded into several field slots when every use is a projection or closure opening.
//!
//! SPSLow's single-occurrence invariant keeps the analysis local: a value node is
//! consumed exactly once, and sharing is explicit through variables.

use crate::representation::{Local, RepresentationPolicy, UnboxingOpportunity, UnboxingReason};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use zydeco_stackir::low::traverse::{Edge, EntityId, Node, Occurrence, Traversal, Visitor};
use zydeco_utils::fold::{Driver, Explicit, Folder, Step};

use zydeco_stackir::{SpsLowProgram, low::syntax as sk};

/// Values, patterns, and variables that the assembly lowerer may represent
/// without a region-allocated cell.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct LocalUnboxing {
    pub values: HashSet<sk::ValueId>,
    pub patterns: HashSet<sk::VPatId>,
    pub unboxed_vars: HashMap<sk::DefId, usize>,
}

impl LocalUnboxing {
    pub fn collect(program: &SpsLowProgram) -> Self {
        Self::with_policy(program, &Local)
    }

    pub fn with_policy<P: RepresentationPolicy + ?Sized>(
        program: &SpsLowProgram, policy: &P,
    ) -> Self {
        Self::with_policy_and_driver::<Explicit>(program, policy)
    }

    pub fn with_policy_and_driver<D: Driver>(
        program: &SpsLowProgram, policy: &(impl RepresentationPolicy + ?Sized),
    ) -> Self {
        let arena = program.arena();
        let mut collector =
            Collector::<_, D> { arena, policy, unboxing: Self::default(), driver: PhantomData };
        Traversal { arena: &arena.inner }
            .run_with_driver::<D>(program.root().into(), &mut collector);
        collector.unboxing
    }
}

struct Collector<'a, P: ?Sized, D> {
    arena: &'a sk::SpsLowArena,
    policy: &'a P,
    unboxing: LocalUnboxing,
    driver: PhantomData<D>,
}

impl<P: RepresentationPolicy + ?Sized, D: Driver> Visitor for Collector<'_, P, D> {
    fn enter(&mut self, node: Node<'_>, _edge: Edge, occurrence: Occurrence) {
        debug_assert_eq!(occurrence, Occurrence::First, "validated SPSLow ownership");
        match node {
            | Node::Computation(
                _,
                sk::Computation::ProductMatch(sk::SProductMatch { scrut, binder, .. }),
            ) => {
                self.mark_product_pair(*scrut, *binder);
            }
            | Node::Computation(
                _,
                sk::Computation::LetValue(sk::LetValue { binder, bindee, tail }),
            ) => {
                self.mark_product_pair(*bindee, *binder);
                self.try_unbox_variable(*binder, *bindee, *tail);
            }
            | Node::Computation(
                _,
                sk::Computation::OpenClosure(sk::OpenClosure { package, environment, .. }),
            ) => {
                self.mark_closure(*package, *environment);
            }
            | _ => {}
        }
    }
}

impl<P: RepresentationPolicy + ?Sized, D: Driver> Collector<'_, P, D> {
    fn mark_product_pair(&mut self, value: sk::ValueId, pattern: sk::VPatId) {
        let Some((value_items, value_arity)) = self.vcons_shape(value) else { return };
        let Some((pattern_items, pattern_arity)) = self.vpat_shape(pattern) else { return };
        if value_items == pattern_items
            && value_arity == pattern_arity
            && self.policy.unbox(UnboxingOpportunity {
                reason: UnboxingReason::DirectProduct,
                fields: value_items,
            })
        {
            self.unboxing.values.insert(value);
            self.unboxing.patterns.insert(pattern);
        }
    }

    fn mark_closure(&mut self, package: sk::ValueId, environment: sk::VPatId) {
        let sk::Value::ClosurePackage(sk::ClosurePackage { environment: env_value, code: _ }) =
            &self.arena.inner.values[&package]
        else {
            return;
        };
        if !self.policy.unbox(UnboxingOpportunity {
            reason: UnboxingReason::DirectClosure,
            fields: zydeco_machine::closure::Closure::<u64>::WORDS,
        }) {
            return;
        }
        self.unboxing.values.insert(package);
        let Some((env_items, env_arity)) = self.vcons_shape(*env_value) else { return };
        let Some((pattern_items, pattern_arity)) = self.vpat_shape(environment) else { return };
        if env_items == pattern_items
            && env_arity == pattern_arity
            && self.policy.unbox(UnboxingOpportunity {
                reason: UnboxingReason::DirectProduct,
                fields: env_items,
            })
        {
            self.unboxing.values.insert(*env_value);
            self.unboxing.patterns.insert(environment);
        }
    }

    fn try_unbox_variable(&mut self, binder: sk::VPatId, bindee: sk::ValueId, body: sk::CompuId) {
        let sk::ValuePattern::Var(def) = &self.arena.inner.vpats[&binder] else { return };
        let (shape, fields, reason) = match &self.arena.inner.values[&bindee] {
            | sk::Value::VCons(sk::VCons { items, layout }) => (
                VariableShape::Product { elements: items.len(), arity: layout.arity },
                items.len(),
                UnboxingReason::ProjectedVariable,
            ),
            | sk::Value::ClosurePackage(_) => (
                VariableShape::Closure,
                zydeco_machine::closure::Closure::<u64>::WORDS,
                UnboxingReason::SharedClosure,
            ),
            | _ => return,
        };
        let info = VarUse::classify_with_driver::<D>(&self.arena.inner, body, *def, shape);
        if info.all_eliminations
            && !info.escapes
            && self.policy.unbox(UnboxingOpportunity { reason, fields })
        {
            self.unboxing.values.insert(bindee);
            self.unboxing.unboxed_vars.insert(*def, fields);
            self.unboxing.patterns.extend(info.projections);
            self.unboxing.values.extend(info.closures);
        }
    }

    fn vcons_shape(&self, value: sk::ValueId) -> Option<(usize, usize)> {
        match &self.arena.inner.values[&value] {
            | sk::Value::VCons(sk::VCons { items, layout }) => Some((items.len(), layout.arity)),
            | _ => None,
        }
    }

    fn vpat_shape(&self, pattern: sk::VPatId) -> Option<(usize, usize)> {
        match &self.arena.inner.vpats[&pattern] {
            | sk::ValuePattern::VCons(sk::VCons { items, layout }) => {
                Some((items.len(), layout.arity))
            }
            | _ => None,
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
struct VarUse {
    all_eliminations: bool,
    escapes: bool,
    projections: Vec<sk::VPatId>,
    closures: Vec<sk::ValueId>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VariableShape {
    Product { elements: usize, arity: usize },
    Closure,
}

impl VarUse {
    fn classify_with_driver<D: Driver>(
        arena: &sk::SpsLowInnerArena, root: sk::CompuId, def: sk::DefId, shape: VariableShape,
    ) -> Self {
        let mut folder = VariableUsesFolder {
            arena,
            def,
            shape,
            info: Self { all_eliminations: true, ..Self::default() },
        };
        D::run(&mut folder, Use::Node(root.into()));
        folder.info
    }
}

/// This view follows executable uses of one variable. Closed blocks and binding patterns
/// are boundaries; projections and closure openings give their immediate value child a role.
struct VariableUsesFolder<'a> {
    arena: &'a sk::SpsLowInnerArena,
    def: sk::DefId,
    shape: VariableShape,
    info: VarUse,
}

enum Use {
    Node(EntityId),
    Projection { value: sk::ValueId, pattern: sk::VPatId },
    ClosureOpening(sk::ValueId),
}

impl VariableUsesFolder<'_> {
    fn escape(&mut self) {
        self.info.all_eliminations = false;
        self.info.escapes = true;
    }

    fn vpat_shape_matches(&self, pattern: sk::VPatId) -> bool {
        match &self.arena.vpats[&pattern] {
            | sk::ValuePattern::VCons(sk::VCons { items, layout }) => {
                self.shape == VariableShape::Product { elements: items.len(), arity: layout.arity }
            }
            | _ => false,
        }
    }
}

impl<'a> Folder for VariableUsesFolder<'a> {
    type Input = Use;
    type Output = ();
    type Frame = (Node<'a>, usize);

    fn enter(&mut self, usage: Use) -> Step<Self> {
        match usage {
            | Use::Node(id) => {
                let node = id.node(self.arena);
                match node {
                    | Node::Pattern(_, _) | Node::Value(_, sk::Value::Block(_)) => {}
                    | Node::Value(_, sk::Value::Var(def)) if *def == self.def => self.escape(),
                    | _ => return self.resume((node, 0), ()),
                }
            }
            | Use::Projection { value, pattern } => {
                if matches!(self.arena.values[&value], sk::Value::Var(def) if def == self.def) {
                    if self.vpat_shape_matches(pattern) {
                        self.info.projections.push(pattern);
                    } else {
                        self.escape();
                    }
                } else {
                    return Step::TailCall(Use::Node(value.into()));
                }
            }
            | Use::ClosureOpening(value) => {
                if self.shape == VariableShape::Closure
                    && matches!(self.arena.values[&value], sk::Value::Var(def) if def == self.def)
                {
                    self.info.closures.push(value);
                } else {
                    return Step::TailCall(Use::Node(value.into()));
                }
            }
        }
        Step::Return(())
    }

    fn resume(&mut self, (node, position): Self::Frame, (): ()) -> Step<Self> {
        let Some((child, _)) = node.child(position) else {
            return Step::Return(());
        };
        let input = match (position, node) {
            | (
                0,
                Node::Computation(
                    _,
                    sk::Computation::ProductMatch(sk::SProductMatch { scrut, binder, .. }),
                ),
            ) => Use::Projection { value: *scrut, pattern: *binder },
            | (
                0,
                Node::Computation(
                    _,
                    sk::Computation::LetValue(sk::LetValue { bindee, binder, .. }),
                ),
            ) => Use::Projection { value: *bindee, pattern: *binder },
            | (
                0,
                Node::Computation(_, sk::Computation::OpenClosure(sk::OpenClosure { package, .. })),
            ) => Use::ClosureOpening(*package),
            | _ => Use::Node(child),
        };
        Step::Call { input, frame: (node, position + 1) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::representation::{Boxed, Direct, RepresentationStrategy, Shared};
    use zydeco_stackir::arena::Construct as _;
    use zydeco_statics::arena::StaticsScope;
    use zydeco_utils::arena::IdAllocator;
    use zydeco_utils::fold::Recursive;

    struct Fixture;

    #[derive(Clone, Copy, Debug)]
    enum ClosureUse {
        Open,
        Argument,
        EntryEnvironment,
        EntryResult,
        Capture,
        Continuation,
        Alias,
        Constructor,
    }

    impl Fixture {
        fn select(
            program: &SpsLowProgram, policy: &(impl RepresentationPolicy + ?Sized),
        ) -> LocalUnboxing {
            let explicit = LocalUnboxing::with_policy(program, policy);
            let recursive = LocalUnboxing::with_policy_and_driver::<Recursive>(program, policy);
            assert_eq!(explicit, recursive);
            explicit
        }

        fn definition(arena: &mut sk::SpsLowArena) -> sk::DefId {
            let id = IdAllocator::<StaticsScope>::new().alloc();
            arena.admin.defs.insert_new(id, "fixture".into());
            id
        }

        fn product(arena: &mut sk::SpsLowArena) -> sk::ValueId {
            let items = (0..2).map(|_| sk::Triv.build(arena, None)).collect();
            sk::VCons::new(items, sk::ProductLayout { arity: 2 }).build(arena, None)
        }

        fn pattern(arena: &mut sk::SpsLowArena) -> sk::VPatId {
            let items = (0..2).map(|_| sk::Hole.build(arena, None)).collect();
            sk::VCons::new(items, sk::ProductLayout { arity: 2 }).build(arena, None)
        }

        fn terminal(arena: &mut sk::SpsLowArena) -> sk::CompuId {
            let stack = sk::Bullet.build(arena, None);
            sk::SHole(stack).build(arena, None)
        }

        fn projection(escape: bool) -> (SpsLowProgram, sk::ValueId, sk::DefId) {
            let mut arena = sk::SpsLowArena::default();
            let variable = Self::definition(&mut arena);
            let bindee = Self::product(&mut arena);
            let scrut = variable.build(&mut arena, None);
            let tail = if escape {
                let ambient = sk::Bullet.build(&mut arena, None);
                let stack = sk::Cons(scrut, ambient).build(&mut arena, None);
                sk::SHole(stack).build(&mut arena, None)
            } else {
                let binder = Self::pattern(&mut arena);
                let body = Self::terminal(&mut arena);
                sk::SProductMatch { scrut, binder, body }.build(&mut arena, None)
            };
            let binder = variable.build(&mut arena, None);
            let root = sk::LetValue { binder, bindee, tail }.build(&mut arena, None);
            (SpsLowProgram::try_new(arena, root).unwrap(), bindee, variable)
        }

        fn block(arena: &mut sk::SpsLowArena, kind: sk::EntryKind) -> sk::ValueId {
            let label = Self::definition(arena);
            let body = Self::terminal(arena);
            let environment = sk::Hole.build(arena, None);
            let entry = match kind {
                | sk::EntryKind::Closure => sk::EntryParameters::Closure { environment },
                | sk::EntryKind::Continuation => sk::EntryParameters::Continuation {
                    result: sk::Hole.build(arena, None),
                    environment,
                },
            };
            sk::Block { label, entry, body }.build(arena, None)
        }

        fn closure(usage: ClosureUse) -> (SpsLowProgram, sk::ValueId, sk::DefId) {
            let mut arena = sk::SpsLowArena::default();
            let variable = Self::definition(&mut arena);
            let environment = Self::product(&mut arena);
            let code = Self::block(&mut arena, sk::EntryKind::Closure);
            let bindee = sk::ClosurePackage { environment, code }.build(&mut arena, None);
            let value = variable.build(&mut arena, None);
            let mut tail = match usage {
                | ClosureUse::Open => {
                    let environment = Self::pattern(&mut arena);
                    let code = sk::Hole.build(&mut arena, None);
                    let body = Self::terminal(&mut arena);
                    sk::OpenClosure { package: value, environment, code, body }
                        .build(&mut arena, None)
                }
                | ClosureUse::EntryEnvironment | ClosureUse::EntryResult => {
                    let stack = sk::Bullet.build(&mut arena, None);
                    let (kind, argument, stack) = match usage {
                        | ClosureUse::EntryEnvironment => (
                            sk::EntryKind::Closure,
                            sk::EntryArgument::Closure { environment: value },
                            stack,
                        ),
                        | _ => {
                            let environment: sk::ValueId = sk::Triv.build(&mut arena, None);
                            let stack = sk::Cons(environment, stack).build(&mut arena, None);
                            (
                                sk::EntryKind::Continuation,
                                sk::EntryArgument::Continuation { result: value },
                                stack,
                            )
                        }
                    };
                    let target = Self::block(&mut arena, kind);
                    sk::Jump { target, argument, stack }.build(&mut arena, None)
                }
                | ClosureUse::Argument | ClosureUse::Continuation => {
                    let stack = sk::Bullet.build(&mut arena, None);
                    let residual = sk::Cons(value, stack).build(&mut arena, None);
                    let stack = if matches!(usage, ClosureUse::Continuation) {
                        let code = Self::block(&mut arena, sk::EntryKind::Continuation);
                        sk::ContinuationPackage { code, residual }.build(&mut arena, None)
                    } else {
                        residual
                    };
                    sk::SHole(stack).build(&mut arena, None)
                }
                | ClosureUse::Capture | ClosureUse::Alias | ClosureUse::Constructor => {
                    let bindee = match usage {
                        | ClosureUse::Capture => {
                            let code = Self::block(&mut arena, sk::EntryKind::Closure);
                            sk::ClosurePackage { environment: value, code }.build(&mut arena, None)
                        }
                        | ClosureUse::Constructor => {
                            sk::Ctor(sk::CtorIdx { name: "Stored".into(), idx: 0 }, value)
                                .build(&mut arena, None)
                        }
                        | _ => value,
                    };
                    let binder = sk::Hole.build(&mut arena, None);
                    let tail = Self::terminal(&mut arena);
                    sk::LetValue { binder, bindee, tail }.build(&mut arena, None)
                }
            };
            // An unrelated closed code block must not count as an escaping occurrence.
            let unrelated = Self::block(&mut arena, sk::EntryKind::Closure);
            let ignored = sk::Hole.build(&mut arena, None);
            tail =
                sk::LetValue { binder: ignored, bindee: unrelated, tail }.build(&mut arena, None);
            let operands = [0, 1].map(|value| {
                sk::Literal::Integer(sk::IntegerLiteral::Int(value)).build(&mut arena, None)
            });
            let unrelated = sk::Primitive {
                operation: sk::PrimitiveOp::Integer(
                    sk::IntegerType::Int,
                    sk::IntegerArithmetic::Add,
                ),
                operands,
            }
            .build(&mut arena, None);
            let ignored = sk::Hole.build(&mut arena, None);
            tail =
                sk::LetValue { binder: ignored, bindee: unrelated, tail }.build(&mut arena, None);
            // A second opening must agree with the first, even on an escaping path.
            let package = variable.build(&mut arena, None);
            let environment = Self::pattern(&mut arena);
            let code = sk::Hole.build(&mut arena, None);
            tail =
                sk::OpenClosure { package, environment, code, body: tail }.build(&mut arena, None);
            let binder = variable.build(&mut arena, None);
            let root = sk::LetValue { binder, bindee, tail }.build(&mut arena, None);
            (SpsLowProgram::try_new(arena, root).unwrap(), bindee, variable)
        }
    }

    #[test]
    fn shared_closures_expand_only_when_every_use_opens_the_package() {
        for usage in [
            ClosureUse::Open,
            ClosureUse::Argument,
            ClosureUse::EntryEnvironment,
            ClosureUse::EntryResult,
            ClosureUse::Capture,
            ClosureUse::Continuation,
            ClosureUse::Alias,
            ClosureUse::Constructor,
        ] {
            let (program, value, variable) = Fixture::closure(usage);
            for &strategy in RepresentationStrategy::ALL {
                let selected = Fixture::select(&program, &strategy);
                let expected =
                    strategy == RepresentationStrategy::Shared && matches!(usage, ClosureUse::Open);
                assert_eq!(selected.values.contains(&value), expected, "{strategy}: {usage:?}");
                assert_eq!(selected.unboxed_vars.get(&variable), expected.then_some(&2));
                // Shared closure environments still cross an ordinary single-word boundary.
                assert!(selected.patterns.is_empty());
                assert_eq!(selected.values.len(), if expected { 3 } else { 0 });
            }
        }
    }

    #[test]
    fn policy_changes_the_emitted_layout_without_changing_stack_validity() {
        use crate::{
            LoweringPipeline,
            syntax::{Instruction, Program},
        };
        use zydeco_statics::arena::StaticsArena;
        use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
        use zydeco_utils::pass::CompilerPass;

        for (program, reason) in [
            (Fixture::projection(false).0, UnboxingReason::ProjectedVariable),
            (Fixture::closure(ClosureUse::Open).0, UnboxingReason::SharedClosure),
        ] {
            for &strategy in RepresentationStrategy::ALL {
                let assembly = LoweringPipeline::new(
                    &SpanArena::default(),
                    &ScopedArena::default(),
                    &StaticsArena::default(),
                )
                .with_representation(strategy)
                .run_infallible(&program);
                let allocations = assembly
                    .arena()
                    .programs
                    .iter()
                    .filter(|(_, node)| {
                        matches!(node, Program::Instruction(Instruction::PackProduct(_), _))
                    })
                    .count();
                let boxed = if reason == UnboxingReason::SharedClosure { 2 } else { 1 };
                let removed =
                    usize::from(strategy.unbox(UnboxingOpportunity { reason, fields: 2 }));
                assert_eq!(allocations, boxed - removed, "{strategy}: {reason:?}");
            }
        }
    }

    #[test]
    fn policies_select_only_justified_variable_expansion() {
        for escape in [false, true] {
            let (program, value, variable) = Fixture::projection(escape);
            for &strategy in RepresentationStrategy::ALL {
                let selected = Fixture::select(&program, &strategy);
                let expected = !escape
                    && matches!(
                        strategy,
                        RepresentationStrategy::Local | RepresentationStrategy::Shared
                    );
                assert_eq!(selected.values.contains(&value), expected);
                assert_eq!(selected.unboxed_vars.contains_key(&variable), expected);
                assert_eq!(selected.patterns.len(), usize::from(expected));
            }
        }
    }

    #[test]
    fn static_and_dynamic_policies_produce_the_same_plan() {
        let (program, _, _) = Fixture::projection(false);
        for (strategy, policy) in [
            (RepresentationStrategy::Boxed, &Boxed as &dyn RepresentationPolicy),
            (RepresentationStrategy::Direct, &Direct as &dyn RepresentationPolicy),
            (RepresentationStrategy::Local, &Local as &dyn RepresentationPolicy),
            (RepresentationStrategy::Shared, &Shared as &dyn RepresentationPolicy),
        ] {
            assert_eq!(Fixture::select(&program, &strategy), Fixture::select(&program, policy));
        }
        assert_eq!(LocalUnboxing::collect(&program), Fixture::select(&program, &Local));
    }

    #[test]
    fn boxed_closure_keeps_its_environment_boxed_under_a_custom_policy() {
        struct KeepClosures;
        impl RepresentationPolicy for KeepClosures {
            fn unbox(&self, opportunity: UnboxingOpportunity) -> bool {
                opportunity.reason != UnboxingReason::DirectClosure
            }
        }
        let mut arena = sk::SpsLowArena::default();
        let env_value = Fixture::product(&mut arena);
        let block_body = Fixture::terminal(&mut arena);
        let label = IdAllocator::<StaticsScope>::new().alloc();
        let entry = sk::EntryParameters::Closure { environment: sk::Hole.build(&mut arena, None) };
        let block = sk::Block { label, entry, body: block_body }.build(&mut arena, None);
        let package =
            sk::ClosurePackage { environment: env_value, code: block }.build(&mut arena, None);
        let environment = Fixture::pattern(&mut arena);
        let code = sk::Hole.build(&mut arena, None);
        let body = Fixture::terminal(&mut arena);
        let root = sk::OpenClosure { package, environment, code, body }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root).unwrap();
        let selected = Fixture::select(&program, &KeepClosures);
        assert!(selected.values.is_empty());
        assert!(selected.patterns.is_empty());
        let selected = Fixture::select(&program, &Direct);
        assert!(selected.values.contains(&package));
        assert!(selected.values.contains(&env_value));
        assert!(selected.patterns.contains(&environment));
    }

    #[test]
    fn local_vcons_pair_is_unboxed() {
        let mut arena = sk::SpsLowArena::default();
        let field_a: sk::ValueId = sk::Triv.build(&mut arena, None);
        let field_b: sk::ValueId = sk::Triv.build(&mut arena, None);
        let items = vec![field_a, field_b];
        let layout = sk::ProductLayout { arity: 2 };
        let value: sk::ValueId = sk::VCons::new(items, layout).build(&mut arena, None);

        let pattern_a: sk::VPatId = sk::Hole.build(&mut arena, None);
        let pattern_b: sk::VPatId = sk::Hole.build(&mut arena, None);
        let pattern: sk::VPatId =
            sk::VCons::new(vec![pattern_a, pattern_b], layout).build(&mut arena, None);

        let stack: sk::StackId = sk::Bullet.build(&mut arena, None);
        let body: sk::CompuId = sk::SHole(stack).build(&mut arena, None);
        let root: sk::CompuId =
            sk::LetValue { binder: pattern, bindee: value, tail: body }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root).unwrap();

        let unboxing = LocalUnboxing::collect(&program);
        assert!(unboxing.values.contains(&value));
        assert!(unboxing.patterns.contains(&pattern));
        assert_eq!(unboxing, Fixture::select(&program, &Direct));
        assert_eq!(Fixture::select(&program, &Boxed), LocalUnboxing::default());
    }

    #[test]
    fn indirect_vcons_pair_is_not_unboxed() {
        let mut arena = sk::SpsLowArena::default();
        let field_a: sk::ValueId = sk::Triv.build(&mut arena, None);
        let field_b: sk::ValueId = sk::Triv.build(&mut arena, None);
        let items = vec![field_a, field_b];
        let layout = sk::ProductLayout { arity: 2 };
        let value: sk::ValueId = sk::VCons::new(items, layout).build(&mut arena, None);

        let binder: sk::VPatId = sk::Hole.build(&mut arena, None);
        let stack: sk::StackId = sk::Bullet.build(&mut arena, None);
        let body: sk::CompuId = sk::SHole(stack).build(&mut arena, None);
        let root: sk::CompuId =
            sk::LetValue { binder, bindee: value, tail: body }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root).unwrap();

        let unboxing = LocalUnboxing::collect(&program);
        assert!(!unboxing.values.contains(&value));
    }

    #[test]
    fn drivers_preserve_policy_call_order_for_empty_and_wide_products() {
        #[derive(Default)]
        struct Alternating(std::cell::RefCell<Vec<(UnboxingReason, usize)>>);
        impl RepresentationPolicy for Alternating {
            fn unbox(&self, opportunity: UnboxingOpportunity) -> bool {
                let mut calls = self.0.borrow_mut();
                calls.push((opportunity.reason, opportunity.fields));
                calls.len() % 2 == 1
            }
        }
        let mut arena = sk::SpsLowArena::default();
        let mut root = Fixture::terminal(&mut arena);
        for count in [0usize, 1, 128].into_iter().rev() {
            let layout = sk::ProductLayout { arity: count.max(1) };
            let fields = (0..count).map(|_| sk::Triv.build(&mut arena, None)).collect();
            let bindee = sk::VCons::new(fields, layout).build(&mut arena, None);
            let patterns = (0..count).map(|_| sk::Hole.build(&mut arena, None)).collect();
            let binder = sk::VCons::new(patterns, layout).build(&mut arena, None);
            root = sk::LetValue { binder, bindee, tail: root }.build(&mut arena, None);
        }
        let program = SpsLowProgram::try_new(arena, root).unwrap();
        let explicit = Alternating::default();
        let recursive = Alternating::default();
        assert_eq!(
            LocalUnboxing::with_policy_and_driver::<Explicit>(&program, &explicit),
            LocalUnboxing::with_policy_and_driver::<Recursive>(&program, &recursive),
        );
        assert_eq!(
            explicit.0.borrow().as_slice(),
            &[
                (UnboxingReason::DirectProduct, 0),
                (UnboxingReason::DirectProduct, 1),
                (UnboxingReason::DirectProduct, 128),
            ]
        );
        assert_eq!(explicit.0.into_inner(), recursive.0.into_inner());
    }

    #[test]
    fn variable_use_classification_stops_at_closed_blocks() {
        let mut arena = sk::SpsLowArena::default();
        let label = Fixture::definition(&mut arena);
        let target = label.build(&mut arena, None);
        let environment = sk::Triv.build(&mut arena, None);
        let stack = sk::Bullet.build(&mut arena, None);
        let body = sk::Jump { target, argument: sk::EntryArgument::Closure { environment }, stack }
            .build(&mut arena, None);
        let entry = sk::EntryParameters::Closure { environment: sk::Hole.build(&mut arena, None) };
        let bindee = sk::Block { label, entry, body }.build(&mut arena, None);
        let binder = sk::Hole.build(&mut arena, None);
        let tail = Fixture::terminal(&mut arena);
        let root = sk::LetValue { binder, bindee, tail }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root).unwrap();
        let shape = VariableShape::Closure;
        let explicit =
            VarUse::classify_with_driver::<Explicit>(&program.arena().inner, root, label, shape);
        let recursive =
            VarUse::classify_with_driver::<Recursive>(&program.arena().inner, root, label, shape);
        assert_eq!(explicit, recursive);
        assert_eq!(explicit, VarUse { all_eliminations: true, ..VarUse::default() });
    }

    #[test]
    fn deep_collection_and_variable_uses_run_and_drop_on_a_small_stack() {
        std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(|| {
                let mut arena = sk::SpsLowArena::default();
                let def = Fixture::definition(&mut arena);
                let scrut = def.build(&mut arena, None);
                let projection = Fixture::pattern(&mut arena);
                let body = Fixture::terminal(&mut arena);
                let mut root =
                    sk::SProductMatch { scrut, binder: projection, body }.build(&mut arena, None);
                for _ in 0..16_384 {
                    let binder = sk::Hole.build(&mut arena, None);
                    let bindee = sk::Triv.build(&mut arena, None);
                    root = sk::LetValue { binder, bindee, tail: root }.build(&mut arena, None);
                }
                let binder = def.build(&mut arena, None);
                let bindee = Fixture::product(&mut arena);
                root = sk::LetValue { binder, bindee, tail: root }.build(&mut arena, None);
                let program = SpsLowProgram::try_new(arena, root).unwrap();
                let plan = LocalUnboxing::collect(&program);
                assert_eq!(plan.unboxed_vars.get(&def), Some(&2));
                assert!(plan.values.contains(&bindee));
                assert!(plan.patterns.contains(&projection));
            })
            .unwrap()
            .join()
            .unwrap();
    }

    #[test]
    fn deep_nested_escape_classification_does_not_use_native_recursion() {
        std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(|| {
                let mut arena = sk::SpsLowArena::default();
                let def = Fixture::definition(&mut arena);
                let mut value: sk::ValueId = def.build(&mut arena, None);
                for _ in 0..16_384 {
                    value = sk::Ctor(
                        sk::CtorIdx { idx: 0, name: sk::CtorName("nested".into()) },
                        value,
                    )
                    .build(&mut arena, None);
                }
                let ambient = sk::Bullet.build(&mut arena, None);
                let stack = sk::Cons(value, ambient).build(&mut arena, None);
                let root = sk::SHole(stack).build(&mut arena, None);
                // Isolate this semantic view from the separate entry and protocol validators.
                let usage = VarUse::classify_with_driver::<Explicit>(
                    &arena.inner,
                    root,
                    def,
                    VariableShape::Closure,
                );
                assert!(usage.escapes);
                assert!(!usage.all_eliminations);
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
