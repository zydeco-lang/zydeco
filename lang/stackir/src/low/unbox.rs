//! Local representation analysis for first-order SPSLow.
//!
//! The analysis marks values and patterns that can avoid a region-allocated cell. A value
//! that is constructed and consumed by a single projection-like use can be
//! represented by its fields directly. A variable bound to such a value can also
//! be expanded into several field slots when every use is a projection or closure opening.
//!
//! SPSLow's single-occurrence invariant keeps the analysis linear (`O(N)`): one pass
//! collects per-definition uses across the tree, and one pass selects unboxing opportunities
//! in lexical pre-order.

use super::{
    SpsLowProgram,
    representation::{Local, RepresentationPolicy, UnboxingOpportunity, UnboxingReason},
    syntax::{self as sk, IdBitSet, SecondaryMap},
    traverse::{Edge, EntityId, Node, Occurrence, Traversal, Visitor},
};
use zydeco_foundation::fold::{Explicit, Folder, Step};

/// Values, patterns, and variables that a lowering backend may represent
/// without a heap-allocated cell.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct LocalUnboxing {
    pub values: IdBitSet<sk::ValueId>,
    pub patterns: IdBitSet<sk::VPatId>,
    pub unboxed_vars: SecondaryMap<sk::DefId, usize>,
}

impl LocalUnboxing {
    pub fn collect(program: &SpsLowProgram) -> Self {
        Self::with_policy(program, &Local)
    }

    pub fn with_policy<P: RepresentationPolicy + ?Sized>(
        program: &SpsLowProgram, policy: &P,
    ) -> Self {
        let arena = program.arena();
        let var_uses = VarUses::collect(&arena.inner, program.root());
        let mut collector = Collector { arena, policy, var_uses, unboxing: Self::default() };
        Traversal { arena: &arena.inner }.run(program.root().into(), &mut collector);
        collector.unboxing
    }
}

struct Collector<'a, P: ?Sized> {
    arena: &'a sk::SpsLowArena,
    policy: &'a P,
    var_uses: VarUses,
    unboxing: LocalUnboxing,
}

impl<P: RepresentationPolicy + ?Sized> Visitor<sk::SpsLow> for Collector<'_, P> {
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
                sk::Computation::LetValue(sk::LetValue { binder, bindee, .. }),
            ) => {
                self.mark_product_pair(*bindee, *binder);
                self.try_unbox_variable(*binder, *bindee);
            }
            | Node::Computation(
                _,
                sk::Computation::Ext(sk::CompuExt::OpenClosure(sk::OpenClosure {
                    package,
                    environment,
                    ..
                })),
            ) => {
                self.mark_closure(*package, *environment);
            }
            | _ => {}
        }
    }
}

impl<P: RepresentationPolicy + ?Sized> Collector<'_, P> {
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
        let sk::Value::Ext(sk::ValueExt::ClosurePackage(sk::ClosurePackage {
            environment: env_value,
            code: _,
        })) = &self.arena.inner.values[&package]
        else {
            return;
        };
        if !self.policy.unbox(UnboxingOpportunity {
            reason: UnboxingReason::DirectClosure,
            fields: sk::ClosurePackage::FIELDS,
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

    fn try_unbox_variable(&mut self, binder: sk::VPatId, bindee: sk::ValueId) {
        let sk::ValuePattern::Var(def) = &self.arena.inner.vpats[&binder] else { return };
        let (shape, fields, reason) = match &self.arena.inner.values[&bindee] {
            | sk::Value::VCons(sk::VCons { items, layout }) => (
                VariableShape::Product { elements: items.len(), arity: layout.arity },
                items.len(),
                UnboxingReason::ProjectedVariable,
            ),
            | sk::Value::Ext(sk::ValueExt::ClosurePackage(_)) => {
                (VariableShape::Closure, sk::ClosurePackage::FIELDS, UnboxingReason::SharedClosure)
            }
            | _ => return,
        };
        let Some(summary) = self.var_uses.get(*def) else {
            if self.policy.unbox(UnboxingOpportunity { reason, fields }) {
                self.unboxing.values.insert(bindee);
                self.unboxing.unboxed_vars.insert_new(*def, fields);
            }
            return;
        };
        if summary.escapes {
            return;
        }
        let compatible = match shape {
            | VariableShape::Product { elements, arity } => {
                summary.closures.is_empty()
                    && summary
                        .projections
                        .iter()
                        .all(|&pattern| self.vpat_shape(pattern) == Some((elements, arity)))
            }
            | VariableShape::Closure => summary.projections.is_empty(),
        };
        if compatible && self.policy.unbox(UnboxingOpportunity { reason, fields }) {
            self.unboxing.values.insert(bindee);
            self.unboxing.unboxed_vars.insert_new(*def, fields);
            self.unboxing.patterns.extend(summary.projections.iter().copied());
            self.unboxing.values.extend(summary.closures.iter().copied());
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VariableShape {
    Product { elements: usize, arity: usize },
    Closure,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct VarUseSummary {
    escapes: bool,
    projections: Vec<sk::VPatId>,
    closures: Vec<sk::ValueId>,
}

#[derive(Debug, Default)]
struct VarUses {
    defs: SecondaryMap<sk::DefId, VarUseSummary>,
}

impl VarUses {
    fn collect(arena: &sk::SpsLowInnerArena, root: sk::CompuId) -> Self {
        let mut collector =
            VarUseCollector { arena, uses: Self::default(), active_block_labels: Vec::new() };
        Explicit::run(&mut collector, EntityId::Computation(root));
        collector.uses
    }

    fn get(&self, def: sk::DefId) -> Option<&VarUseSummary> {
        self.defs.get(def)
    }

    fn entry(&mut self, def: sk::DefId) -> &mut VarUseSummary {
        if !self.defs.contains_key(def) {
            self.defs.insert_new(def, VarUseSummary::default());
        }
        &mut self.defs[def]
    }
}

struct VarUseCollector<'a> {
    arena: &'a sk::SpsLowInnerArena,
    uses: VarUses,
    active_block_labels: Vec<sk::DefId>,
}

struct Frame<'a> {
    node: Node<'a>,
    position: usize,
    pop_label: bool,
}

impl<'a> Folder for VarUseCollector<'a> {
    type Input = EntityId;
    type Output = ();
    type Frame = Frame<'a>;

    fn enter(&mut self, id: EntityId) -> Step<Self> {
        let node = id.node(self.arena);
        let mut pop_label = false;
        match node {
            | Node::Pattern(_, _) => return Step::Return(()),
            | Node::Value(_, sk::Value::Var(def)) => {
                if !self.active_block_labels.contains(def) {
                    self.uses.entry(*def).escapes = true;
                }
                return Step::Return(());
            }
            | Node::Value(_, sk::Value::Ext(sk::ValueExt::Block(sk::Block { label, .. }))) => {
                self.active_block_labels.push(*label);
                pop_label = true;
            }
            | _ => {}
        }
        self.resume(Frame { node, position: 0, pop_label }, ())
    }

    fn resume(&mut self, mut frame: Self::Frame, (): ()) -> Step<Self> {
        let node = frame.node;
        loop {
            let position = frame.position;
            let Some((child, _)) = node.child(position) else {
                if frame.pop_label {
                    self.active_block_labels.pop();
                }
                return Step::Return(());
            };
            frame.position = position + 1;
            match node {
                | Node::Computation(
                    _,
                    sk::Computation::ProductMatch(sk::SProductMatch { scrut, binder, .. }),
                ) if child == EntityId::Value(*scrut) => {
                    if let sk::Value::Var(def) = self.arena.values[*scrut] {
                        self.uses.entry(def).projections.push(*binder);
                        continue;
                    }
                }
                | Node::Computation(
                    _,
                    sk::Computation::LetValue(sk::LetValue { bindee, binder, .. }),
                ) if child == EntityId::Value(*bindee) => {
                    if let sk::Value::Var(def) = self.arena.values[*bindee] {
                        self.uses.entry(def).projections.push(*binder);
                        continue;
                    }
                }
                | Node::Computation(
                    _,
                    sk::Computation::Ext(sk::CompuExt::OpenClosure(sk::OpenClosure {
                        package,
                        ..
                    })),
                ) if child == EntityId::Value(*package) => {
                    if let sk::Value::Var(def) = self.arena.values[*package] {
                        self.uses.entry(def).closures.push(*package);
                        continue;
                    }
                }
                | _ => {}
            }
            return Step::Call { input: child, frame };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::low::representation::{Boxed, Direct, RepresentationStrategy, Shared};
    use crate::{SpsScheme, arena::Construct as _};

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
            LocalUnboxing::with_policy(program, policy)
        }

        fn definition(arena: &mut sk::SpsLowArena) -> sk::DefId {
            arena.admin.alloc_def("fixture".into())
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
            let stack = sk::StackVar.build(arena, None);
            sk::SHole(stack).build(arena, None)
        }

        fn projection(escape: bool) -> (SpsLowProgram, sk::ValueId, sk::DefId) {
            let mut arena = sk::SpsLowArena::default();
            let variable = Self::definition(&mut arena);
            let bindee = Self::product(&mut arena);
            let scrut = variable.build(&mut arena, None);
            let tail = if escape {
                let ambient = sk::StackVar.build(&mut arena, None);
                let stack = sk::Cons(scrut, ambient).build(&mut arena, None);
                sk::SHole(stack).build(&mut arena, None)
            } else {
                let binder = Self::pattern(&mut arena);
                let body = Self::terminal(&mut arena);
                sk::SProductMatch { scrut, binder, body }.build(&mut arena, None)
            };
            let binder = variable.build(&mut arena, None);
            let root = sk::LetValue { binder, bindee, tail }.build(&mut arena, None);
            (SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap(), bindee, variable)
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
                    let stack = sk::StackVar.build(&mut arena, None);
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
                    let stack = sk::StackVar.build(&mut arena, None);
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
            (SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap(), bindee, variable)
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
                assert_eq!(selected.values.contains(value), expected, "{strategy}: {usage:?}");
                assert_eq!(selected.unboxed_vars.get(variable), expected.then_some(&2));
                // Shared closure environments still cross an ordinary single-word boundary.
                assert!(selected.patterns.is_empty());
                assert_eq!(selected.values.len(), if expected { 3 } else { 0 });
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
                assert_eq!(selected.values.contains(value), expected);
                assert_eq!(selected.unboxed_vars.contains_key(variable), expected);
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
        let label = arena.admin.fresh_def();
        let entry = sk::EntryParameters::Closure { environment: sk::Hole.build(&mut arena, None) };
        let block = sk::Block { label, entry, body: block_body }.build(&mut arena, None);
        let package =
            sk::ClosurePackage { environment: env_value, code: block }.build(&mut arena, None);
        let environment = Fixture::pattern(&mut arena);
        let code = sk::Hole.build(&mut arena, None);
        let body = Fixture::terminal(&mut arena);
        let root = sk::OpenClosure { package, environment, code, body }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap();
        let selected = Fixture::select(&program, &KeepClosures);
        assert!(selected.values.is_empty());
        assert!(selected.patterns.is_empty());
        let selected = Fixture::select(&program, &Direct);
        assert!(selected.values.contains(package));
        assert!(selected.values.contains(env_value));
        assert!(selected.patterns.contains(environment));
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

        let stack: sk::StackId = sk::StackVar.build(&mut arena, None);
        let body: sk::CompuId = sk::SHole(stack).build(&mut arena, None);
        let root: sk::CompuId =
            sk::LetValue { binder: pattern, bindee: value, tail: body }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap();

        let unboxing = LocalUnboxing::collect(&program);
        assert!(unboxing.values.contains(value));
        assert!(unboxing.patterns.contains(pattern));
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
        let stack: sk::StackId = sk::StackVar.build(&mut arena, None);
        let body: sk::CompuId = sk::SHole(stack).build(&mut arena, None);
        let root: sk::CompuId =
            sk::LetValue { binder, bindee: value, tail: body }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap();

        let unboxing = LocalUnboxing::collect(&program);
        assert!(!unboxing.values.contains(value));
    }

    #[test]
    fn policy_calls_follow_source_order_for_empty_and_wide_products() {
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
        let program = SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap();
        let policy = Alternating::default();
        LocalUnboxing::with_policy(&program, &policy);
        assert_eq!(
            policy.0.into_inner(),
            [
                (UnboxingReason::DirectProduct, 0),
                (UnboxingReason::DirectProduct, 1),
                (UnboxingReason::DirectProduct, 128),
            ]
        );
    }

    #[test]
    fn variable_use_classification_ignores_block_self_jumps() {
        let mut arena = sk::SpsLowArena::default();
        let label = Fixture::definition(&mut arena);
        let target = label.build(&mut arena, None);
        let environment = sk::Triv.build(&mut arena, None);
        let stack = sk::StackVar.build(&mut arena, None);
        let body = sk::Jump { target, argument: sk::EntryArgument::Closure { environment }, stack }
            .build(&mut arena, None);
        let entry = sk::EntryParameters::Closure { environment: sk::Hole.build(&mut arena, None) };
        let bindee = sk::Block { label, entry, body }.build(&mut arena, None);
        let binder = sk::Hole.build(&mut arena, None);
        let tail = Fixture::terminal(&mut arena);
        let root = sk::LetValue { binder, bindee, tail }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap();
        let uses = VarUses::collect(&program.arena().inner, root);
        assert_eq!(uses.get(label), None);
    }

    #[test]
    fn local_variables_inside_closed_blocks_are_unboxed_in_single_pass() {
        let mut arena = sk::SpsLowArena::default();
        let label = Fixture::definition(&mut arena);
        let def = Fixture::definition(&mut arena);
        let scrut = def.build(&mut arena, None);
        let projection = Fixture::pattern(&mut arena);
        let match_body = Fixture::terminal(&mut arena);
        let inner_tail = sk::SProductMatch { scrut, binder: projection, body: match_body }
            .build(&mut arena, None);
        let inner_binder = def.build(&mut arena, None);
        let inner_bindee = Fixture::product(&mut arena);
        let block_body =
            sk::LetValue { binder: inner_binder, bindee: inner_bindee, tail: inner_tail }
                .build(&mut arena, None);
        let entry = sk::EntryParameters::Closure { environment: sk::Hole.build(&mut arena, None) };
        let block = sk::Block { label, entry, body: block_body }.build(&mut arena, None);
        let root = sk::LetValue {
            binder: sk::Hole.build(&mut arena, None),
            bindee: block,
            tail: Fixture::terminal(&mut arena),
        }
        .build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap();
        let plan = LocalUnboxing::collect(&program);
        assert_eq!(plan.unboxed_vars.get(def), Some(&2));
        assert!(plan.values.contains(inner_bindee));
        assert!(plan.patterns.contains(projection));
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
                let program = SpsLowProgram::try_new(arena, root, SpsScheme::BranchJoin).unwrap();
                let plan = LocalUnboxing::collect(&program);
                assert_eq!(plan.unboxed_vars.get(def), Some(&2));
                assert!(plan.values.contains(bindee));
                assert!(plan.patterns.contains(projection));
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
                    value =
                        sk::Ctor(sk::CtorIdx { idx: 0, name: sk::CtorName::new("nested") }, value)
                            .build(&mut arena, None);
                }
                let ambient = sk::StackVar.build(&mut arena, None);
                let stack = sk::Cons(value, ambient).build(&mut arena, None);
                let root = sk::SHole(stack).build(&mut arena, None);
                // Isolate this semantic view from the separate entry and protocol validators.
                let uses = VarUses::collect(&arena.inner, root);
                let usage = uses.get(def).expect("def escape recorded");
                assert!(usage.escapes);
                assert!(usage.projections.is_empty());
                assert!(usage.closures.is_empty());
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
