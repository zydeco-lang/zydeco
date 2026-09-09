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
        let arena = program.arena();
        let mut collector = Collector { arena, policy, unboxing: Self::default() };
        collector.compu(program.root());
        collector.unboxing
    }
}

struct Collector<'a, P: ?Sized> {
    arena: &'a sk::SpsLowArena,
    policy: &'a P,
    unboxing: LocalUnboxing,
}

impl<P: RepresentationPolicy + ?Sized> Collector<'_, P> {
    fn compu(&mut self, id: sk::CompuId) {
        match self.arena.inner.compus[&id].clone() {
            | sk::Computation::Hole(sk::SHole(stack)) => self.stack(stack),
            | sk::Computation::Jump(sk::Jump { target, stack }) => {
                self.value(target);
                self.stack(stack);
            }
            | sk::Computation::ProductMatch(sk::SProductMatch { scrut, binder, body }) => {
                self.mark_product_pair(scrut, binder);
                self.value(scrut);
                self.pattern(binder);
                self.compu(body);
            }
            | sk::Computation::CoprodMatch(sk::SCoprodMatch { scrut, arms }) => {
                self.value(scrut);
                for sk::Matcher { binder, tail } in arms {
                    self.pattern(binder);
                    self.compu(tail);
                }
            }
            | sk::Computation::LetValue(sk::LetValue { binder, bindee, tail: body }) => {
                self.mark_product_pair(bindee, binder);
                self.try_unbox_variable(binder, bindee, body);
                self.value(bindee);
                self.pattern(binder);
                self.compu(body);
            }
            | sk::Computation::LetStack(sk::LetStack {
                binder: sk::Bullet,
                bindee,
                tail: body,
            }) => {
                self.stack(bindee);
                self.compu(body);
            }
            | sk::Computation::LetArg(sk::LetArg {
                binder: sk::Cons(binder, sk::Bullet),
                bindee,
                tail: body,
            }) => {
                self.stack(bindee);
                self.pattern(binder);
                self.compu(body);
            }
            | sk::Computation::CoCase(sk::SCoMatch { scrut, arms }) => {
                self.stack(scrut);
                for sk::CoMatcher { dtor: _, tail } in arms {
                    self.compu(tail);
                }
            }
            | sk::Computation::OpenClosure(sk::OpenClosure {
                package,
                environment,
                code,
                body,
            }) => {
                self.mark_closure(package, environment);
                self.value(package);
                self.pattern(environment);
                self.pattern(code);
                self.compu(body);
            }
            | sk::Computation::OpenContinuation(sk::OpenContinuation { package, code, body }) => {
                self.stack(package);
                self.pattern(code);
                self.compu(body);
            }
            | sk::Computation::ExternCall(sk::ExternCall { function: _, stack }) => {
                self.stack(stack);
            }
        }
    }

    fn value(&mut self, id: sk::ValueId) {
        match self.arena.inner.values[&id].clone() {
            | sk::Value::Hole(_)
            | sk::Value::Var(_)
            | sk::Value::Triv(_)
            | sk::Value::Literal(_) => {}
            | sk::Value::Block(sk::Block { label: _, body }) => self.compu(body),
            | sk::Value::ClosurePackage(sk::ClosurePackage { environment, code }) => {
                self.value(environment);
                self.value(code);
            }
            | sk::Value::Ctor(sk::Ctor(_, value)) => self.value(value),
            | sk::Value::VCons(sk::VCons { items, layout: _ }) => {
                for item in items {
                    self.value(item);
                }
            }
            | sk::Value::Primitive(sk::Primitive { operation: _, operands }) => {
                for operand in operands {
                    self.value(operand);
                }
            }
        }
    }

    fn pattern(&mut self, id: sk::VPatId) {
        match self.arena.inner.vpats[&id].clone() {
            | sk::ValuePattern::Hole(_) | sk::ValuePattern::Var(_) | sk::ValuePattern::Triv(_) => {}
            | sk::ValuePattern::Ctor(sk::Ctor(_, pattern)) => self.pattern(pattern),
            | sk::ValuePattern::Alias(sk::Alias(patterns)) => {
                for pattern in patterns {
                    self.pattern(pattern);
                }
            }
            | sk::ValuePattern::VCons(sk::VCons { items, layout: _ }) => {
                for item in items {
                    self.pattern(item);
                }
            }
        }
    }

    fn stack(&mut self, id: sk::StackId) {
        match self.arena.inner.stacks[&id].clone() {
            | sk::Stack::Var(sk::Bullet) => {}
            | sk::Stack::Arg(sk::Cons(value, stack)) => {
                self.value(value);
                self.stack(stack);
            }
            | sk::Stack::Tag(sk::Cons(_, stack)) => self.stack(stack),
            | sk::Stack::ContinuationPackage(sk::ContinuationPackage { code, residual }) => {
                self.value(code);
                self.stack(residual);
            }
        }
    }

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
        let info = classify_var(&self.arena.inner, body, *def, shape);
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

#[derive(Debug, Default)]
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

fn classify_var(
    arena: &sk::SpsLowInnerArena, root: sk::CompuId, def: sk::DefId, shape: VariableShape,
) -> VarUse {
    let mut info = VarUse { all_eliminations: true, ..VarUse::default() };
    let mut visitor = VarVisitor { arena, def, shape, info: &mut info };
    visitor.compu(root);
    info
}

struct VarVisitor<'a> {
    arena: &'a sk::SpsLowInnerArena,
    def: sk::DefId,
    shape: VariableShape,
    info: &'a mut VarUse,
}

impl VarVisitor<'_> {
    fn compu(&mut self, id: sk::CompuId) {
        match self.arena.compus[&id].clone() {
            | sk::Computation::Hole(sk::SHole(stack)) => self.stack(stack),
            | sk::Computation::Jump(sk::Jump { target, stack }) => {
                self.value_escape(target);
                self.stack(stack);
            }
            | sk::Computation::ProductMatch(sk::SProductMatch { scrut, binder, body }) => {
                self.value_in_projection(scrut, Some(binder));
                self.compu(body);
            }
            | sk::Computation::CoprodMatch(sk::SCoprodMatch { scrut, arms }) => {
                self.value_escape(scrut);
                for sk::Matcher { binder: _, tail } in arms {
                    self.compu(tail);
                }
            }
            | sk::Computation::LetValue(sk::LetValue { binder, bindee, tail: body }) => {
                self.value_in_projection(bindee, Some(binder));
                self.compu(body);
            }
            | sk::Computation::LetStack(sk::LetStack {
                binder: sk::Bullet,
                bindee,
                tail: body,
            }) => {
                self.stack(bindee);
                self.compu(body);
            }
            | sk::Computation::LetArg(sk::LetArg {
                binder: sk::Cons(_, sk::Bullet),
                bindee,
                tail: body,
            }) => {
                self.stack(bindee);
                self.compu(body);
            }
            | sk::Computation::CoCase(sk::SCoMatch { scrut, arms }) => {
                self.stack(scrut);
                for sk::CoMatcher { dtor: _, tail } in arms {
                    self.compu(tail);
                }
            }
            | sk::Computation::OpenClosure(sk::OpenClosure {
                package,
                environment: _,
                code: _,
                body,
            }) => {
                if self.shape == VariableShape::Closure
                    && matches!(self.arena.values[&package], sk::Value::Var(def) if def == self.def)
                {
                    self.info.closures.push(package);
                } else {
                    self.value_escape(package);
                }
                self.compu(body);
            }
            | sk::Computation::OpenContinuation(sk::OpenContinuation {
                package,
                code: _,
                body,
            }) => {
                self.stack(package);
                self.compu(body);
            }
            | sk::Computation::ExternCall(sk::ExternCall { function: _, stack }) => {
                self.stack(stack);
            }
        }
    }

    fn value_in_projection(&mut self, value: sk::ValueId, pattern: Option<sk::VPatId>) {
        if let sk::Value::Var(def) = &self.arena.values[&value]
            && *def == self.def
        {
            match pattern {
                | Some(pattern) if self.vpat_shape_matches(pattern) => {
                    self.info.projections.push(pattern);
                }
                | _ => {
                    self.info.all_eliminations = false;
                    self.info.escapes = true;
                }
            }
        } else {
            self.value_escape(value);
        }
    }

    fn value_escape(&mut self, value: sk::ValueId) {
        match self.arena.values[&value].clone() {
            | sk::Value::Var(def) if def == self.def => {
                self.info.all_eliminations = false;
                self.info.escapes = true;
            }
            | sk::Value::Var(_) => {}
            | sk::Value::VCons(sk::VCons { items, layout: _ }) => {
                for item in items {
                    self.value_escape(item);
                }
            }
            | sk::Value::ClosurePackage(sk::ClosurePackage { environment, code }) => {
                self.value_escape(environment);
                self.value_escape(code);
            }
            | sk::Value::Ctor(sk::Ctor(_, payload)) => self.value_escape(payload),
            | sk::Value::Primitive(sk::Primitive { operands, .. }) => {
                for operand in operands {
                    self.value_escape(operand);
                }
            }
            // SpsLowProgram validates that blocks are closed. Captures occur in package
            // environments and continuation residuals, which are visited separately.
            | sk::Value::Block(_) => {}
            | sk::Value::Hole(_) | sk::Value::Triv(_) | sk::Value::Literal(_) => {}
        }
    }

    fn stack(&mut self, stack: sk::StackId) {
        match self.arena.stacks[&stack].clone() {
            | sk::Stack::Var(sk::Bullet) => {}
            | sk::Stack::Arg(sk::Cons(value, stack)) => {
                self.value_escape(value);
                self.stack(stack);
            }
            | sk::Stack::Tag(sk::Cons(_, stack)) => self.stack(stack),
            | sk::Stack::ContinuationPackage(sk::ContinuationPackage { code, residual }) => {
                self.value_escape(code);
                self.stack(residual);
            }
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::representation::{Boxed, Direct, RepresentationStrategy, Shared};
    use zydeco_stackir::arena::Construct as _;
    use zydeco_statics::arena::StaticsScope;
    use zydeco_utils::arena::IdAllocator;

    struct Fixture;

    #[derive(Clone, Copy, Debug)]
    enum ClosureUse {
        Open,
        Argument,
        Capture,
        Continuation,
        Alias,
        Constructor,
    }

    impl Fixture {
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

        fn block(arena: &mut sk::SpsLowArena) -> sk::ValueId {
            let label = Self::definition(arena);
            let body = Self::terminal(arena);
            sk::Block { label, body }.build(arena, None)
        }

        fn closure(usage: ClosureUse) -> (SpsLowProgram, sk::ValueId, sk::DefId) {
            let mut arena = sk::SpsLowArena::default();
            let variable = Self::definition(&mut arena);
            let environment = Self::product(&mut arena);
            let code = Self::block(&mut arena);
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
                | ClosureUse::Argument | ClosureUse::Continuation => {
                    let stack = sk::Bullet.build(&mut arena, None);
                    let residual = sk::Cons(value, stack).build(&mut arena, None);
                    let stack = if matches!(usage, ClosureUse::Continuation) {
                        let code = Self::block(&mut arena);
                        sk::ContinuationPackage { code, residual }.build(&mut arena, None)
                    } else {
                        residual
                    };
                    sk::SHole(stack).build(&mut arena, None)
                }
                | ClosureUse::Capture | ClosureUse::Alias | ClosureUse::Constructor => {
                    let bindee = match usage {
                        | ClosureUse::Capture => {
                            let code = Self::block(&mut arena);
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
            let unrelated = Self::block(&mut arena);
            let ignored = sk::Hole.build(&mut arena, None);
            tail =
                sk::LetValue { binder: ignored, bindee: unrelated, tail }.build(&mut arena, None);
            let operands = [0, 1].map(|value| {
                sk::Literal::Integer(sk::IntegerLiteral::Int64(value)).build(&mut arena, None)
            });
            let unrelated = sk::Primitive {
                operation: sk::PrimitiveOp::Integer(
                    sk::IntegerType::Int64,
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
            ClosureUse::Capture,
            ClosureUse::Continuation,
            ClosureUse::Alias,
            ClosureUse::Constructor,
        ] {
            let (program, value, variable) = Fixture::closure(usage);
            for &strategy in RepresentationStrategy::ALL {
                let selected = LocalUnboxing::with_policy(&program, &strategy);
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

        for (program, reason) in [
            (Fixture::projection(false).0, UnboxingReason::ProjectedVariable),
            (Fixture::closure(ClosureUse::Open).0, UnboxingReason::SharedClosure),
        ] {
            for &strategy in RepresentationStrategy::ALL {
                let assembly = LoweringPipeline::new(
                    &SpanArena::default(),
                    &ScopedArena::default(),
                    &StaticsArena::default(),
                    &program,
                )
                .with_representation(strategy)
                .run();
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
                let selected = LocalUnboxing::with_policy(&program, &strategy);
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
            assert_eq!(
                LocalUnboxing::with_policy(&program, &strategy),
                LocalUnboxing::with_policy(&program, policy)
            );
        }
        assert_eq!(LocalUnboxing::collect(&program), LocalUnboxing::with_policy(&program, &Local));
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
        let block = sk::Block { label, body: block_body }.build(&mut arena, None);
        let package =
            sk::ClosurePackage { environment: env_value, code: block }.build(&mut arena, None);
        let environment = Fixture::pattern(&mut arena);
        let code = sk::Hole.build(&mut arena, None);
        let body = Fixture::terminal(&mut arena);
        let root = sk::OpenClosure { package, environment, code, body }.build(&mut arena, None);
        let program = SpsLowProgram::try_new(arena, root).unwrap();
        let selected = LocalUnboxing::with_policy(&program, &KeepClosures);
        assert!(selected.values.is_empty());
        assert!(selected.patterns.is_empty());
        let selected = LocalUnboxing::with_policy(&program, &Direct);
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
        assert_eq!(unboxing, LocalUnboxing::with_policy(&program, &Direct));
        assert_eq!(LocalUnboxing::with_policy(&program, &Boxed), LocalUnboxing::default());
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
}
