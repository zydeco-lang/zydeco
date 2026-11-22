//! Occurrence-based reconstruction of residual core terms with resumable continuations.

use super::*;
use crate::ir::syntax as ir;
use zydeco_foundation::fold::{Explicit, Folder, Step};

/// Match decisions share plans, never generated SPS nodes. Flat storage also makes
/// cloning and dropping a long fallthrough chain independent of the Rust stack.
#[derive(Clone, Copy)]
pub(super) struct MatchPlanId(usize);

#[derive(Clone, Copy)]
enum MatchPlan {
    Fail,
    Tail(ir::CompuId),
    Continue(DefId),
    Pattern { scrutinee: DefId, pattern: ir::VPatId, success: MatchPlanId, failure: MatchPlanId },
}

#[derive(Clone, Copy)]
pub(super) enum MatchKind {
    Planned,
    Coproduct,
    Product,
}

/// The kind of an unfinished pattern and the source fields it keeps; its children arrive as
/// completed patterns.
pub(super) enum PatternParent {
    Hole,
    Var(DefId),
    Triv,
    Ctor(CtorIdx),
    Alias { count: usize },
    VCons { count: usize, layout: ProductLayout },
}

/// The kind of an unfinished value and the source fields it keeps; its children arrive as
/// completed value plans.
pub(super) enum ValueParent {
    Hole,
    Var(DefId),
    Triv,
    Lit(ir::Literal),
    /// A value let, whose bindee, tail, and binder arrive in that order.
    Let,
    Thunk,
    Ctor(CtorIdx),
    VCons {
        count: usize,
        layout: ProductLayout,
    },
    Proj(Vec<ProductProjection>),
}

/// Each finish frame consumes the completed children of exactly one occurrence.
/// Intermediate frames retain the original interleaving of allocation and descent.
/// Frames carry the source fields their resumption needs, so completing a child never
/// re-establishes the source node's variant. Frames that walk a source vector carry its id
/// and the next position instead of the vector.
pub(super) enum Work {
    Pattern(ir::VPatId),
    /// Lower the children of an alias or product pattern in order.
    PatternChildren(ir::VPatId, usize),
    FinishPattern {
        source: ir::VPatId,
        parent: PatternParent,
    },
    Value(ir::ValueId),
    /// Lower the items of a product value in order.
    ValueChildren(ir::ValueId, usize),
    ValueLetTail {
        source: ir::ValueId,
        binder: ir::VPatId,
        tail: ir::ValueId,
    },
    ValueLetPattern {
        source: ir::ValueId,
        binder: ir::VPatId,
    },
    FinishValue {
        source: ir::ValueId,
        parent: ValueParent,
    },
    Computation(ir::CompuId, StackId),
    BinderBody {
        binder: ir::VPatId,
        body: ir::CompuId,
        stack: StackId,
        site: Option<ir::TermId>,
        role: &'static str,
    },
    LetBody {
        binder: ir::VPatId,
        bindee: ValueId,
        tail: ir::CompuId,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    ValueSteps(Vec<ValueStep>),
    /// Name a pushed frame around the lowered body, binding the frame's value steps outside it.
    NameStack {
        frame: StackId,
        steps: Vec<ValueStep>,
        site: Option<ir::TermId>,
    },
    Bind(ValueBinding),
    Abstraction {
        stack: StackId,
        site: Option<ir::TermId>,
    },
    Application {
        body: ir::CompuId,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    Fix {
        source: ir::CompuId,
        param: DefId,
        stack: StackId,
    },
    Force {
        stack: StackId,
        site: Option<ir::TermId>,
    },
    Return {
        stack: StackId,
        site: Option<ir::TermId>,
    },
    Do {
        bindee: ir::CompuId,
        site: Option<ir::TermId>,
    },
    LetBindee {
        binder: ir::VPatId,
        tail: ir::CompuId,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    Let {
        bindee: ValueId,
        site: Option<ir::TermId>,
    },
    MatchScrutinee {
        arms: Vec<Matcher<ir::VPatId, ir::CompuId>>,
        kind: MatchKind,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    MatchBody {
        scrut: ValueId,
        arms: Vec<Matcher<ir::VPatId, ir::CompuId>>,
        kind: MatchKind,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    ProductMatchTail {
        scrut: ValueId,
        tail: ir::CompuId,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    CoproductArms {
        remaining: std::vec::IntoIter<Matcher<ir::VPatId, ir::CompuId>>,
        scrut: ValueId,
        count: usize,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    ProductMatch {
        scrut: ValueId,
        site: Option<ir::TermId>,
    },
    MatchArm {
        binder: ir::VPatId,
        tail: ir::CompuId,
        stack: StackId,
    },
    MatchArmBody {
        tail: ir::CompuId,
        stack: StackId,
    },
    CoproductMatch {
        scrut: ValueId,
        count: usize,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    CoMatchArm {
        tag: DtorIdx,
        tail: ir::CompuId,
        site: Option<ir::TermId>,
    },
    CoMatch {
        source: ir::CompuId,
        count: usize,
        stack: StackId,
    },
    CoMatchArms {
        remaining: std::vec::IntoIter<CoMatcher<DtorIdx, ir::CompuId>>,
        source: ir::CompuId,
        count: usize,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    MatchPlan {
        plan: MatchPlanId,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    PlannedProduct {
        scrutinee: DefId,
        binder: VPatId,
        site: Option<ir::TermId>,
    },
    /// Lower a constructor pattern's arms once its fallback rows are complete.
    ConstructorFallback {
        case: ConstructorCase,
        data: ir::DataId,
        tag: CtorIdx,
        argument: ir::VPatId,
        success: MatchPlanId,
    },
    ConstructorArm {
        tag: CtorIdx,
        payload: Option<(ir::VPatId, MatchPlanId)>,
        failure: MatchPlanId,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    ConstructorArms {
        case: ConstructorCase,
        remaining: std::vec::IntoIter<ConstructorBranch>,
        fallback: ValueId,
        failure: MatchPlanId,
        count: usize,
    },
    PlannedConstructor {
        case: ConstructorCase,
        fallback: ValueId,
        count: usize,
    },
    LiteralSuccess {
        scrutinee: DefId,
        integer: IntegerLiteral,
        operator: ValueId,
        failure: MatchPlanId,
        stack: StackId,
        site: Option<ir::TermId>,
    },
    LiteralFailure {
        scrutinee: DefId,
        integer: IntegerLiteral,
        operator: ValueId,
        then: ValueId,
        stack: StackId,
        site: Option<ir::TermId>,
    },
}

/// The fields a constructor case keeps across its fallback rows and arms: the scrutinee, the
/// continuation that names the fallback rows, and the stack and site the arms join.
pub(super) struct ConstructorCase {
    scrutinee: DefId,
    continuation: DefId,
    stack: StackId,
    site: Option<ir::TermId>,
}

pub(super) struct ConstructorBranch {
    tag: CtorIdx,
    payload: Option<(ir::VPatId, MatchPlanId)>,
}

pub(super) struct LoweringFolder<'lo, 'source> {
    lo: &'lo mut Lowerer<'source>,
    patterns: Vec<VPatId>,
    values: Vec<ValuePlan<ValueId>>,
    computations: Vec<CompuId>,
    tags: Vec<DtorIdx>,
    plans: Vec<MatchPlan>,
}

impl<'lo, 'source> LoweringFolder<'lo, 'source> {
    pub(super) fn new(lo: &'lo mut Lowerer<'source>) -> Self {
        Self {
            lo,
            patterns: Vec::new(),
            values: Vec::new(),
            computations: Vec::new(),
            tags: Vec::new(),
            plans: Vec::new(),
        }
    }

    pub(super) fn lower(mut self, root: ir::CompuId, stack: StackId) -> CompuId {
        Explicit::run(&mut self, Work::Computation(root, stack));
        assert!(self.patterns.is_empty() && self.values.is_empty() && self.tags.is_empty());
        let root = self.computation();
        assert!(self.computations.is_empty());
        root
    }

    fn pattern(&mut self) -> VPatId {
        self.patterns.pop().expect("completed pattern child")
    }

    fn value(&mut self) -> ValuePlan<ValueId> {
        self.values.pop().expect("completed value child")
    }

    fn computation(&mut self) -> CompuId {
        self.computations.pop().expect("completed computation child")
    }

    fn plan(&mut self, plan: MatchPlan) -> MatchPlanId {
        let id = MatchPlanId(self.plans.len());
        self.plans.push(plan);
        id
    }

    fn binder_body(
        &mut self, binder: ir::VPatId, body: ir::CompuId, stack: StackId, site: Option<ir::TermId>,
        role: &'static str,
    ) -> Step<Self> {
        if self.lo.pattern_needs_match_plan(binder) {
            let scrutinee = self.lo.alloc_admin_def(role);
            self.patterns.push(scrutinee.build(self.lo, None));
            let success = self.plan(MatchPlan::Tail(body));
            let failure = self.plan(MatchPlan::Fail);
            let plan =
                self.plan(MatchPlan::Pattern { scrutinee, pattern: binder, success, failure });
            Step::TailCall(Work::MatchPlan { plan, stack, site })
        } else {
            Step::Call { input: Work::Pattern(binder), frame: Work::Computation(body, stack) }
        }
    }

    fn visit_pattern(&mut self, source: ir::VPatId) -> Step<Self> {
        let parent = match &self.lo.core.vpats[source] {
            | ir::ValuePattern::Hole(_) => PatternParent::Hole,
            | ir::ValuePattern::Var(var) => PatternParent::Var(self.lo.def(*var)),
            | ir::ValuePattern::Triv(_) => PatternParent::Triv,
            | ir::ValuePattern::Ctor { tag, payload, .. } => {
                return Step::Call {
                    input: Work::Pattern(*payload),
                    frame: Work::FinishPattern { source, parent: PatternParent::Ctor(tag.clone()) },
                };
            }
            | ir::ValuePattern::Alias(_) | ir::ValuePattern::VCons(_) => {
                return Step::TailCall(Work::PatternChildren(source, 0));
            }
            | ir::ValuePattern::Lit(_) => unreachable!("literal patterns require a match plan"),
        };
        Step::TailCall(Work::FinishPattern { source, parent })
    }

    fn visit_value(&mut self, source: ir::ValueId) -> Step<Self> {
        let site = Some(ir::TermId::Value(source));
        let (child, parent) = match &self.lo.core.values[source] {
            | ir::Value::Foreign(import) => {
                let stack = StackVar.build(self.lo, site);
                let function = match import.clone() {
                    | ir::CheckedImport::C(import) => ExternalFunction::Foreign(import),
                    | ir::CheckedImport::Zydeco(import) => ExternalFunction::Unit(import),
                };
                let body = ExternCall { function, stack }.build(self.lo, site);
                let value = Closure { stack: StackVar, body }.build(self.lo, site);
                if let Some(protocol) = self.lo.core.value_protocols.get(source) {
                    self.lo.arena.inner.value_protocols.insert_new(value, protocol.clone());
                }
                self.values.push(ValuePlan::pure(value));
                return Step::Return(());
            }
            | ir::Value::Ctor { tag, payload } => (Some(*payload), ValueParent::Ctor(tag.clone())),
            | ir::Value::Proj { head, steps } => (Some(*head), ValueParent::Proj(steps.clone())),
            | ir::Value::Let(Let { binder, bindee, tail }) => {
                return Step::Call {
                    input: Work::Value(*bindee),
                    frame: Work::ValueLetTail { source, binder: *binder, tail: *tail },
                };
            }
            | ir::Value::Thunk(Thunk(body)) => {
                let stack = StackVar.build(self.lo, site);
                return Step::Call {
                    input: Work::Computation(*body, stack),
                    frame: Work::FinishValue { source, parent: ValueParent::Thunk },
                };
            }
            | ir::Value::VCons(_) => return Step::TailCall(Work::ValueChildren(source, 0)),
            | ir::Value::Hole(_) => (None, ValueParent::Hole),
            | ir::Value::Var(var) => (None, ValueParent::Var(self.lo.def(*var))),
            | ir::Value::Triv(_) => (None, ValueParent::Triv),
            | ir::Value::Lit(literal) => (None, ValueParent::Lit(literal.clone())),
        };
        let frame = Work::FinishValue { source, parent };
        match child {
            | Some(child) => Step::Call { input: Work::Value(child), frame },
            | None => Step::TailCall(frame),
        }
    }

    fn visit_computation(&mut self, source: ir::CompuId, stack: StackId) -> Step<Self> {
        let site = Some(ir::TermId::Compu(source));
        match self.lo.core.compus[source].clone() {
            | ir::Computation::Hole(_) => self.computations.push(SHole(stack).build(self.lo, site)),
            | ir::Computation::VAbs(Abs(param, body)) => {
                let body_stack = StackVar.build(self.lo, site);
                return Step::Call {
                    input: Work::BinderBody {
                        binder: param,
                        body,
                        stack: body_stack,
                        site,
                        role: "__view_argument__",
                    },
                    frame: Work::Abstraction { stack, site },
                };
            }
            | ir::Computation::VApp(App(body, arg)) => {
                return Step::Call {
                    input: Work::Value(arg),
                    frame: Work::Application { body, stack, site },
                };
            }
            | ir::Computation::Fix { param, body } => {
                let param = self.lo.def(param);
                let body_stack = StackVar.build(self.lo, site);
                return Step::Call {
                    input: Work::Computation(body, body_stack),
                    frame: Work::Fix { source, param, stack },
                };
            }
            | ir::Computation::Force(Force(body)) => {
                return Step::Call { input: Work::Value(body), frame: Work::Force { stack, site } };
            }
            | ir::Computation::Ret(Return(body)) => {
                return Step::Call {
                    input: Work::Value(body),
                    frame: Work::Return { stack, site },
                };
            }
            | ir::Computation::Do(Bind { binder, bindee, tail }) => {
                return Step::Call {
                    input: Work::BinderBody {
                        binder,
                        body: tail,
                        stack,
                        site,
                        role: "__view_returned__",
                    },
                    frame: Work::Do { bindee, site },
                };
            }
            | ir::Computation::Let(Let { binder, bindee, tail }) => {
                return Step::Call {
                    input: Work::Value(bindee),
                    frame: Work::LetBindee { binder, tail, stack, site },
                };
            }
            | ir::Computation::Match(Match { scrut, arms }) => {
                let mixed = arms.iter().any(|arm| self.lo.is_coprod_pattern(arm.binder))
                    && arms.iter().any(|arm| !self.lo.is_coprod_pattern(arm.binder));
                let kind = if mixed
                    || arms.iter().any(|arm| self.lo.match_arm_needs_match_plan(arm.binder))
                {
                    MatchKind::Planned
                } else if self.lo.is_coprod_match(&arms) {
                    MatchKind::Coproduct
                } else {
                    MatchKind::Product
                };
                return Step::Call {
                    input: Work::Value(scrut),
                    frame: Work::MatchScrutinee { arms, kind, stack, site },
                };
            }
            | ir::Computation::CoMatch { arms } => {
                return Step::TailCall(Work::CoMatchArms {
                    source,
                    count: arms.len(),
                    stack,
                    site,
                    remaining: arms.into_iter(),
                });
            }
            | ir::Computation::Dtor { body, tag } => {
                let frame = Cons(tag, stack).build(self.lo, site);
                return match self.lo.push(frame, site) {
                    | Pushed::Extended(stack) => Step::TailCall(Work::Computation(body, stack)),
                    | Pushed::Named { frame, ambient } => Step::Call {
                        input: Work::Computation(body, ambient),
                        frame: Work::NameStack { frame, steps: Vec::new(), site },
                    },
                };
            }
        }
        Step::Return(())
    }

    fn visit_match_plan(
        &mut self, plan: MatchPlanId, stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        let (scrutinee, pattern, success, failure) = match self.plans[plan.0] {
            | MatchPlan::Fail => {
                self.computations.push(SHole(stack).build(self.lo, site));
                return Step::Return(());
            }
            | MatchPlan::Tail(tail) => {
                return Step::TailCall(Work::Computation(tail, stack));
            }
            | MatchPlan::Continue(continuation) => {
                let thunk = continuation.build(self.lo, site);
                self.computations.push(SForce { thunk, stack }.build(self.lo, site));
                return Step::Return(());
            }
            | MatchPlan::Pattern { scrutinee, pattern, success, failure } => {
                (scrutinee, pattern, success, failure)
            }
        };
        match self.lo.core.vpats[pattern].clone() {
            | ir::ValuePattern::Hole(_) | ir::ValuePattern::Triv(_) => {
                Step::TailCall(Work::MatchPlan { plan: success, stack, site })
            }
            | ir::ValuePattern::Var(var) => {
                let bindee = scrutinee.build(self.lo, site);
                let binder = self.lo.def(var).build(self.lo, None);
                Step::Call {
                    input: Work::MatchPlan { plan: success, stack, site },
                    frame: Work::Bind(ValueBinding { binder, bindee, site }),
                }
            }
            | ir::ValuePattern::Alias(Alias(patterns)) => {
                let plan = patterns.into_iter().rev().fold(success, |success, pattern| {
                    self.plan(MatchPlan::Pattern { scrutinee, pattern, success, failure })
                });
                Step::TailCall(Work::MatchPlan { plan, stack, site })
            }
            | ir::ValuePattern::VCons(ir::VCons { items, layout }) => {
                let components = items
                    .iter()
                    .map(|_| self.lo.alloc_admin_def("__match_component__"))
                    .collect::<Vec<_>>();
                let fields = components.iter().map(|def| def.build(self.lo, None)).collect();
                let binder = VCons::new(fields, layout).build(self.lo, None);
                let plan = items.into_iter().zip(components).rev().fold(
                    success,
                    |success, (pattern, scrutinee)| {
                        self.plan(MatchPlan::Pattern { scrutinee, pattern, success, failure })
                    },
                );
                Step::Call {
                    input: Work::MatchPlan { plan, stack, site },
                    frame: Work::PlannedProduct { scrutinee, binder, site },
                }
            }
            | ir::ValuePattern::Ctor { data, tag, payload } => {
                // Bind remaining rows once; unmatched tags and rejected payloads
                // resume that closure with their own ambient branch stack.
                let continuation = self.lo.alloc_admin_def("__match_fallback__");
                let failure_stack = StackVar.build(self.lo, site);
                Step::Call {
                    input: Work::MatchPlan { plan: failure, stack: failure_stack, site },
                    frame: Work::ConstructorFallback {
                        case: ConstructorCase { scrutinee, continuation, stack, site },
                        data,
                        tag,
                        argument: payload,
                        success,
                    },
                }
            }
            | ir::ValuePattern::Lit(literal) => {
                let ir::Literal::Integer(integer) = literal else {
                    unreachable!("a checked literal pattern carries an integer literal")
                };
                let integer_type = integer
                    .integer_type()
                    .expect("a checked integer literal carries its integer type");
                let role = zydeco_syntax::BuiltinValueRole::Integer(
                    integer_type,
                    zydeco_syntax::IntegerOperation::Eq,
                );
                let operator = ExternalFunction::Host(role).make_function(self.lo.scheme, self.lo);
                let success_stack = StackVar.build(self.lo, site);
                Step::Call {
                    input: Work::MatchPlan { plan: success, stack: success_stack, site },
                    frame: Work::LiteralSuccess {
                        scrutinee,
                        integer,
                        operator,
                        failure,
                        stack,
                        site,
                    },
                }
            }
        }
    }

    fn match_arms(&mut self, count: usize) -> Vec<Matcher<VPatId, CompuId>> {
        let patterns = self.patterns.split_off(self.patterns.len() - count);
        let tails = self.computations.split_off(self.computations.len() - count);
        patterns.into_iter().zip(tails).map(|(binder, tail)| Matcher { binder, tail }).collect()
    }

    // Patterns

    fn resume_pattern_children(&mut self, source: ir::VPatId, position: usize) -> Step<Self> {
        // The next child to lower, or the finished parent once every child is complete.
        let next = match &self.lo.core.vpats[source] {
            | ir::ValuePattern::Alias(Alias(children)) => {
                let ConsN(head, tail) = children;
                let child =
                    if position == head.len() { Some(*tail) } else { head.get(position).copied() };
                child.ok_or(PatternParent::Alias { count: children.len() })
            }
            | ir::ValuePattern::VCons(VCons { items, layout }) => items
                .get(position)
                .copied()
                .ok_or(PatternParent::VCons { count: items.len(), layout: *layout }),
            | _ => {
                unreachable!(
                    "a pattern-children frame is pushed only for an alias or product pattern"
                )
            }
        };
        match next {
            | Ok(child) => Step::Call {
                input: Work::Pattern(child),
                frame: Work::PatternChildren(source, position + 1),
            },
            | Err(parent) => Step::TailCall(Work::FinishPattern { source, parent }),
        }
    }

    fn resume_finish_pattern(&mut self, source: ir::VPatId, parent: PatternParent) -> Step<Self> {
        let pattern: ValuePattern = match parent {
            | PatternParent::Hole => Hole.into(),
            | PatternParent::Var(def) => def.into(),
            | PatternParent::Triv => Triv.into(),
            | PatternParent::Ctor(tag) => {
                let child = self.pattern();
                Ctor(tag, child).into()
            }
            | PatternParent::Alias { count } => {
                let children = self.patterns.split_off(self.patterns.len() - count);
                Alias(ConsN::from_vec(children).unwrap()).into()
            }
            | PatternParent::VCons { count, layout } => {
                let children = self.patterns.split_off(self.patterns.len() - count);
                VCons::new(children, layout).into()
            }
        };
        let pattern = pattern.build(self.lo, Some(source));
        if let Some(protocol) = self.lo.core.pattern_protocols.get(source) {
            self.lo.arena.inner.pattern_protocols.insert_new(pattern, protocol.clone());
        }
        self.patterns.push(pattern);
        Step::Return(())
    }

    // Values

    fn resume_value_children(&mut self, source: ir::ValueId, position: usize) -> Step<Self> {
        let ir::Value::VCons(VCons { items, layout }) = &self.lo.core.values[source] else {
            unreachable!("a value-children frame is pushed only for a product value")
        };
        if let Some(&child) = items.get(position) {
            return Step::Call {
                input: Work::Value(child),
                frame: Work::ValueChildren(source, position + 1),
            };
        }
        let parent = ValueParent::VCons { count: items.len(), layout: *layout };
        Step::TailCall(Work::FinishValue { source, parent })
    }

    fn resume_value_let_tail(
        &mut self, source: ir::ValueId, binder: ir::VPatId, tail: ir::ValueId,
    ) -> Step<Self> {
        Step::Call { input: Work::Value(tail), frame: Work::ValueLetPattern { source, binder } }
    }

    fn resume_value_let_pattern(&mut self, source: ir::ValueId, binder: ir::VPatId) -> Step<Self> {
        Step::Call {
            input: Work::Pattern(binder),
            frame: Work::FinishValue { source, parent: ValueParent::Let },
        }
    }

    fn resume_finish_value(&mut self, source: ir::ValueId, parent: ValueParent) -> Step<Self> {
        let site = Some(ir::TermId::Value(source));
        let plan = match parent {
            | ValueParent::Hole => ValuePlan::pure(Hole.build(self.lo, site)),
            | ValueParent::Var(def) => ValuePlan::pure(def.build(self.lo, site)),
            | ValueParent::Let => {
                let tail = self.value();
                let bindee = self.value();
                let binder = self.pattern();
                let binding = ValueStep::Bind(ValueBinding { binder, bindee: bindee.value, site });
                ValuePlan {
                    steps: bindee.steps.into_iter().chain([binding]).chain(tail.steps).collect(),
                    value: tail.value,
                }
            }
            | ValueParent::Thunk => {
                let body = self.computation();
                ValuePlan::pure(Closure { stack: StackVar, body }.build(self.lo, site))
            }
            | ValueParent::Ctor(tag) => {
                self.value().map(|body| Ctor(tag, body).build(self.lo, site))
            }
            | ValueParent::Triv => ValuePlan::pure(Triv.build(self.lo, site)),
            | ValueParent::VCons { count, layout } => {
                let children = self.values.split_off(self.values.len() - count);
                ValuePlan::sequence(children)
                    .map(|children| VCons::new(children, layout).build(self.lo, site))
            }
            | ValueParent::Proj(steps) => {
                steps.into_iter().fold(self.value(), |head, projection| {
                    let (binding, projected) = self.lo.projection_binding(
                        head.value,
                        projection.position,
                        projection.layout,
                        site,
                    );
                    head.with_binding(binding, projected)
                })
            }
            | ValueParent::Lit(literal) => ValuePlan::pure(literal.build(self.lo, site)),
        };
        if let Some(protocol) = self.lo.core.value_protocols.get(source) {
            let _ = self.lo.arena.inner.value_protocols.upsert(plan.value, protocol.clone());
        }
        self.values.push(plan);
        Step::Return(())
    }

    // Computations

    fn resume_value_steps(&mut self, steps: Vec<ValueStep>) -> Step<Self> {
        let tail = self.computation();
        self.computations.push(ValuePlan { steps, value: () }.bind(self.lo, tail));
        Step::Return(())
    }

    fn resume_name_stack(
        &mut self, frame: StackId, steps: Vec<ValueStep>, site: Option<ir::TermId>,
    ) -> Step<Self> {
        let tail = self.computation();
        let named = self.lo.arena.name_frames(vec![frame], tail, site);
        self.computations.push(ValuePlan { steps, value: () }.bind(self.lo, named));
        Step::Return(())
    }

    fn resume_bind(&mut self, binding: ValueBinding) -> Step<Self> {
        let tail = self.computation();
        self.computations.push(binding.bind(self.lo, tail));
        Step::Return(())
    }

    fn resume_abstraction(&mut self, stack: StackId, site: Option<ir::TermId>) -> Step<Self> {
        let binder = Cons(self.pattern(), StackVar);
        let tail = self.computation();
        self.computations.push(Let { binder, bindee: stack, tail }.build(self.lo, site));
        Step::Return(())
    }

    fn resume_application(
        &mut self, body: ir::CompuId, stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        let arg = self.value();
        let frame = Cons(arg.value, stack).build(self.lo, site);
        match self.lo.push(frame, site) {
            | Pushed::Extended(stack) => Step::Call {
                input: Work::Computation(body, stack),
                frame: Work::ValueSteps(arg.steps),
            },
            | Pushed::Named { frame, ambient } => Step::Call {
                input: Work::Computation(body, ambient),
                frame: Work::NameStack { frame, steps: arg.steps, site },
            },
        }
    }

    fn resume_fix(&mut self, source: ir::CompuId, param: DefId, stack: StackId) -> Step<Self> {
        let body = self.computation();
        let fix = SFix { param, stack, body }.build(self.lo, Some(source.into()));
        if let Some(protocol) = self.lo.core.compu_protocols.get(source) {
            self.lo.arena.inner.compu_protocols.insert_new(fix, protocol.clone());
        }
        self.computations.push(fix);
        Step::Return(())
    }

    fn resume_force(&mut self, stack: StackId, site: Option<ir::TermId>) -> Step<Self> {
        let body = self.value();
        let tail = SForce { thunk: body.value, stack }.build(self.lo, site);
        self.computations.push(body.bind(self.lo, tail));
        Step::Return(())
    }

    fn resume_return(&mut self, stack: StackId, site: Option<ir::TermId>) -> Step<Self> {
        let body = self.value();
        let tail = SReturn { stack, value: body.value }.build(self.lo, site);
        self.computations.push(body.bind(self.lo, tail));
        Step::Return(())
    }

    fn resume_do(&mut self, bindee: ir::CompuId, site: Option<ir::TermId>) -> Step<Self> {
        let binder = self.pattern();
        let body = self.computation();
        let frame = Kont { binder, body }.build(self.lo, site);
        match self.lo.push(frame, site) {
            | Pushed::Extended(stack) => Step::TailCall(Work::Computation(bindee, stack)),
            | Pushed::Named { frame, ambient } => Step::Call {
                input: Work::Computation(bindee, ambient),
                frame: Work::NameStack { frame, steps: Vec::new(), site },
            },
        }
    }

    fn resume_let_bindee(
        &mut self, binder: ir::VPatId, tail: ir::CompuId, stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        let bindee = self.value();
        Step::Call {
            input: Work::LetBody { binder, bindee: bindee.value, tail, stack, site },
            frame: Work::ValueSteps(bindee.steps),
        }
    }

    fn resume_let_body(
        &mut self, binder: ir::VPatId, bindee: ValueId, tail: ir::CompuId, stack: StackId,
        site: Option<ir::TermId>,
    ) -> Step<Self> {
        Step::Call {
            input: Work::BinderBody { binder, body: tail, stack, site, role: "__view_let__" },
            frame: Work::Let { bindee, site },
        }
    }

    fn resume_let(&mut self, bindee: ValueId, site: Option<ir::TermId>) -> Step<Self> {
        let binder = self.pattern();
        let tail = self.computation();
        self.computations.push(ValueBinding { binder, bindee, site }.bind(self.lo, tail));
        Step::Return(())
    }

    // Matches

    fn resume_match_scrutinee(
        &mut self, arms: Vec<Matcher<ir::VPatId, ir::CompuId>>, kind: MatchKind, stack: StackId,
        site: Option<ir::TermId>,
    ) -> Step<Self> {
        let scrut = self.value();
        Step::Call {
            input: Work::MatchBody { scrut: scrut.value, arms, kind, stack, site },
            frame: Work::ValueSteps(scrut.steps),
        }
    }

    fn resume_match_body(
        &mut self, scrut: ValueId, arms: Vec<Matcher<ir::VPatId, ir::CompuId>>, kind: MatchKind,
        stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        match kind {
            | MatchKind::Planned => {
                let scrutinee = self.lo.alloc_admin_def("__match_scrutinee__");
                let binder = scrutinee.build(self.lo, None);
                let fail = self.plan(MatchPlan::Fail);
                let plan = arms.into_iter().rev().fold(fail, |failure, arm| {
                    let success = self.plan(MatchPlan::Tail(arm.tail));
                    self.plan(MatchPlan::Pattern {
                        scrutinee,
                        pattern: arm.binder,
                        success,
                        failure,
                    })
                });
                Step::Call {
                    input: Work::MatchPlan { plan, stack, site },
                    frame: Work::Bind(ValueBinding { binder, bindee: scrut, site }),
                }
            }
            | MatchKind::Coproduct => Step::TailCall(Work::CoproductArms {
                scrut,
                count: arms.len(),
                stack,
                site,
                remaining: arms.into_iter(),
            }),
            | MatchKind::Product => {
                let [Matcher { binder, tail }] = arms.as_slice() else {
                    unreachable!("an irrefutable match has exactly one arm")
                };
                Step::Call {
                    input: Work::Pattern(*binder),
                    frame: Work::ProductMatchTail { scrut, tail: *tail, stack, site },
                }
            }
        }
    }

    fn resume_product_match_tail(
        &mut self, scrut: ValueId, tail: ir::CompuId, stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        Step::Call {
            input: Work::Computation(tail, stack),
            frame: Work::ProductMatch { scrut, site },
        }
    }

    fn resume_product_match(&mut self, scrut: ValueId, site: Option<ir::TermId>) -> Step<Self> {
        let binder = self.pattern();
        let body = self.computation();
        self.computations.push(SProductMatch { scrut, binder, body }.build(self.lo, site));
        Step::Return(())
    }

    fn resume_coproduct_arms(
        &mut self, mut remaining: std::vec::IntoIter<Matcher<ir::VPatId, ir::CompuId>>,
        scrut: ValueId, count: usize, stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        match remaining.next() {
            | Some(Matcher { binder, tail }) => {
                let arm = self.lo.arm_stack(stack, remaining.len() == 0, site);
                Step::Call {
                    input: Work::MatchArm { binder, tail, stack: arm },
                    frame: Work::CoproductArms { remaining, scrut, count, stack, site },
                }
            }
            | None => Step::TailCall(Work::CoproductMatch { scrut, count, stack, site }),
        }
    }

    fn resume_match_arm(
        &mut self, binder: ir::VPatId, tail: ir::CompuId, stack: StackId,
    ) -> Step<Self> {
        Step::Call { input: Work::Pattern(binder), frame: Work::MatchArmBody { tail, stack } }
    }

    fn resume_match_arm_body(&mut self, tail: ir::CompuId, stack: StackId) -> Step<Self> {
        Step::TailCall(Work::Computation(tail, stack))
    }

    fn resume_coproduct_match(
        &mut self, scrut: ValueId, count: usize, stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        let arms = self.match_arms(count);
        let branch = SCoprodMatch { scrut, arms }.build(self.lo, site);
        self.computations.push(self.lo.join(Some(stack), branch, site));
        Step::Return(())
    }

    // Comatches

    fn resume_co_match_arms(
        &mut self, mut remaining: std::vec::IntoIter<CoMatcher<DtorIdx, ir::CompuId>>,
        source: ir::CompuId, count: usize, stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        match remaining.next() {
            | Some(CoMatcher { dtor: tag, tail }) => Step::Call {
                input: Work::CoMatchArm { tag, tail, site },
                frame: Work::CoMatchArms { remaining, source, count, stack, site },
            },
            | None => Step::TailCall(Work::CoMatch { source, count, stack }),
        }
    }

    fn resume_co_match_arm(
        &mut self, tag: DtorIdx, tail: ir::CompuId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        self.tags.push(tag);
        let stack = StackVar.build(self.lo, site);
        Step::TailCall(Work::Computation(tail, stack))
    }

    fn resume_co_match(&mut self, source: ir::CompuId, count: usize, stack: StackId) -> Step<Self> {
        let tags = self.tags.split_off(self.tags.len() - count);
        let tails = self.computations.split_off(self.computations.len() - count);
        let arms = tags
            .into_iter()
            .zip(tails)
            .map(|(tag, tail)| CoMatcher { dtor: Cons(tag, StackVar), tail })
            .collect();
        let case = SCoMatch { scrut: stack, arms }.build(self.lo, Some(source.into()));
        if let Some(protocol) = self.lo.core.compu_protocols.get(source) {
            self.lo.arena.inner.compu_protocols.insert_new(case, protocol.clone());
        }
        self.computations.push(case);
        Step::Return(())
    }

    // Match plans

    fn resume_planned_product(
        &mut self, scrutinee: DefId, binder: VPatId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        let body = self.computation();
        let scrut = scrutinee.build(self.lo, site);
        self.computations.push(SProductMatch { scrut, binder, body }.build(self.lo, site));
        Step::Return(())
    }

    fn resume_constructor_fallback(
        &mut self, case: ConstructorCase, data: ir::DataId, tag: CtorIdx, argument: ir::VPatId,
        success: MatchPlanId,
    ) -> Step<Self> {
        let body = self.computation();
        let fallback = Closure { stack: StackVar, body }.build(self.lo, case.site);
        let failure = self.plan(MatchPlan::Continue(case.continuation));
        let constructors = &self.lo.core.datas[data];
        debug_assert_eq!(constructors[tag.idx], tag.name);
        let remaining = constructors
            .iter()
            .enumerate()
            .map(|(idx, &candidate)| ConstructorBranch {
                tag: CtorIdx { idx, name: candidate },
                payload: (idx == tag.idx).then_some((argument, success)),
            })
            .collect::<Vec<_>>()
            .into_iter();
        let count = constructors.len();
        Step::TailCall(Work::ConstructorArms { case, remaining, fallback, failure, count })
    }

    fn resume_constructor_arm(
        &mut self, tag: CtorIdx, payload: Option<(ir::VPatId, MatchPlanId)>, failure: MatchPlanId,
        stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        let (binder, plan) = if let Some((pattern, success)) = payload {
            let scrutinee = self.lo.alloc_admin_def("__match_payload__");
            let binder = scrutinee.build(self.lo, None);
            let plan = self.plan(MatchPlan::Pattern { scrutinee, pattern, success, failure });
            (binder, plan)
        } else {
            (Hole.build(self.lo, None), failure)
        };
        self.patterns.push(Ctor(tag, binder).build(self.lo, None));
        Step::TailCall(Work::MatchPlan { plan, stack, site })
    }

    fn resume_constructor_arms(
        &mut self, case: ConstructorCase, mut remaining: std::vec::IntoIter<ConstructorBranch>,
        fallback: ValueId, failure: MatchPlanId, count: usize,
    ) -> Step<Self> {
        match remaining.next() {
            | Some(ConstructorBranch { tag, payload }) => {
                let ConstructorCase { stack, site, .. } = case;
                let arm = self.lo.arm_stack(stack, remaining.len() == 0, site);
                Step::Call {
                    input: Work::ConstructorArm { tag, payload, failure, stack: arm, site },
                    frame: Work::ConstructorArms { case, remaining, fallback, failure, count },
                }
            }
            | None => Step::TailCall(Work::PlannedConstructor { case, fallback, count }),
        }
    }

    fn resume_planned_constructor(
        &mut self, case: ConstructorCase, fallback: ValueId, count: usize,
    ) -> Step<Self> {
        let ConstructorCase { scrutinee, continuation, stack, site } = case;
        let arms = self.match_arms(count);
        let scrut = scrutinee.build(self.lo, site);
        let body = SCoprodMatch { scrut, arms }.build(self.lo, site);
        let tail = self.lo.join(Some(stack), body, site);
        let binder = continuation.build(self.lo, None);
        self.computations.push(Let { binder, bindee: fallback, tail }.build(self.lo, site));
        Step::Return(())
    }

    fn resume_literal_success(
        &mut self, scrutinee: DefId, integer: IntegerLiteral, operator: ValueId,
        failure: MatchPlanId, stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        let body = self.computation();
        let then = Closure { stack: StackVar, body }.build(self.lo, site);
        let failure_stack = StackVar.build(self.lo, site);
        Step::Call {
            input: Work::MatchPlan { plan: failure, stack: failure_stack, site },
            frame: Work::LiteralFailure { scrutinee, integer, operator, then, stack, site },
        }
    }

    fn resume_literal_failure(
        &mut self, scrutinee: DefId, integer: IntegerLiteral, operator: ValueId, then: ValueId,
        stack: StackId, site: Option<ir::TermId>,
    ) -> Step<Self> {
        let body = self.computation();
        let otherwise = Closure { stack: StackVar, body }.build(self.lo, site);
        let scrut: ValueId = scrutinee.build(self.lo, site);
        let literal: ValueId = zydeco_syntax::Literal::Integer(integer).build(self.lo, site);
        let mut named = Vec::new();
        let stack = [otherwise, then, literal, scrut].into_iter().fold(stack, |stack, value| {
            let frame = Cons(value, stack).build(self.lo, site);
            self.lo.push_onto(frame, &mut named, site)
        });
        let call = SForce { thunk: operator, stack }.build(self.lo, site);
        self.computations.push(self.lo.arena.name_frames(named, call, site));
        Step::Return(())
    }
}

impl Folder for LoweringFolder<'_, '_> {
    type Input = Work;
    type Frame = Work;
    type Output = ();

    fn resume(&mut self, frame: Work, (): ()) -> Step<Self> {
        self.enter(frame)
    }

    fn enter(&mut self, work: Work) -> Step<Self> {
        match work {
            | Work::Pattern(source) => self.visit_pattern(source),
            | Work::PatternChildren(source, position) => {
                self.resume_pattern_children(source, position)
            }
            | Work::FinishPattern { source, parent } => self.resume_finish_pattern(source, parent),
            | Work::Value(source) => self.visit_value(source),
            | Work::ValueChildren(source, position) => self.resume_value_children(source, position),
            | Work::ValueLetTail { source, binder, tail } => {
                self.resume_value_let_tail(source, binder, tail)
            }
            | Work::ValueLetPattern { source, binder } => {
                self.resume_value_let_pattern(source, binder)
            }
            | Work::FinishValue { source, parent } => self.resume_finish_value(source, parent),
            | Work::Computation(source, stack) => self.visit_computation(source, stack),
            | Work::BinderBody { binder, body, stack, site, role } => {
                self.binder_body(binder, body, stack, site, role)
            }
            | Work::LetBody { binder, bindee, tail, stack, site } => {
                self.resume_let_body(binder, bindee, tail, stack, site)
            }
            | Work::ValueSteps(steps) => self.resume_value_steps(steps),
            | Work::NameStack { frame, steps, site } => self.resume_name_stack(frame, steps, site),
            | Work::Bind(binding) => self.resume_bind(binding),
            | Work::Abstraction { stack, site } => self.resume_abstraction(stack, site),
            | Work::Application { body, stack, site } => self.resume_application(body, stack, site),
            | Work::Fix { source, param, stack } => self.resume_fix(source, param, stack),
            | Work::Force { stack, site } => self.resume_force(stack, site),
            | Work::Return { stack, site } => self.resume_return(stack, site),
            | Work::Do { bindee, site } => self.resume_do(bindee, site),
            | Work::LetBindee { binder, tail, stack, site } => {
                self.resume_let_bindee(binder, tail, stack, site)
            }
            | Work::Let { bindee, site } => self.resume_let(bindee, site),
            | Work::MatchScrutinee { arms, kind, stack, site } => {
                self.resume_match_scrutinee(arms, kind, stack, site)
            }
            | Work::MatchBody { scrut, arms, kind, stack, site } => {
                self.resume_match_body(scrut, arms, kind, stack, site)
            }
            | Work::ProductMatchTail { scrut, tail, stack, site } => {
                self.resume_product_match_tail(scrut, tail, stack, site)
            }
            | Work::CoproductArms { remaining, scrut, count, stack, site } => {
                self.resume_coproduct_arms(remaining, scrut, count, stack, site)
            }
            | Work::ProductMatch { scrut, site } => self.resume_product_match(scrut, site),
            | Work::MatchArm { binder, tail, stack } => self.resume_match_arm(binder, tail, stack),
            | Work::MatchArmBody { tail, stack } => self.resume_match_arm_body(tail, stack),
            | Work::CoproductMatch { scrut, count, stack, site } => {
                self.resume_coproduct_match(scrut, count, stack, site)
            }
            | Work::CoMatchArm { tag, tail, site } => self.resume_co_match_arm(tag, tail, site),
            | Work::CoMatch { source, count, stack } => self.resume_co_match(source, count, stack),
            | Work::CoMatchArms { remaining, source, count, stack, site } => {
                self.resume_co_match_arms(remaining, source, count, stack, site)
            }
            | Work::MatchPlan { plan, stack, site } => self.visit_match_plan(plan, stack, site),
            | Work::PlannedProduct { scrutinee, binder, site } => {
                self.resume_planned_product(scrutinee, binder, site)
            }
            | Work::ConstructorFallback { case, data, tag, argument, success } => {
                self.resume_constructor_fallback(case, data, tag, argument, success)
            }
            | Work::ConstructorArm { tag, payload, failure, stack, site } => {
                self.resume_constructor_arm(tag, payload, failure, stack, site)
            }
            | Work::ConstructorArms { case, remaining, fallback, failure, count } => {
                self.resume_constructor_arms(case, remaining, fallback, failure, count)
            }
            | Work::PlannedConstructor { case, fallback, count } => {
                self.resume_planned_constructor(case, fallback, count)
            }
            | Work::LiteralSuccess { scrutinee, integer, operator, failure, stack, site } => {
                self.resume_literal_success(scrutinee, integer, operator, failure, stack, site)
            }
            | Work::LiteralFailure { scrutinee, integer, operator, then, stack, site } => {
                self.resume_literal_failure(scrutinee, integer, operator, then, stack, site)
            }
        }
    }
}
