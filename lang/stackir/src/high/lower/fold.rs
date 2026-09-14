//! Occurrence-based reconstruction of residual typed terms with heap-backed continuations.

use super::*;

/// Match decisions share plans, never generated SPS nodes. Flat storage also makes
/// cloning and dropping a long fallthrough chain independent of the Rust stack.
#[derive(Clone, Copy)]
struct MatchPlanId(usize);

#[derive(Clone, Copy)]
enum MatchPlan {
    Fail,
    Tail(ss::CompuId),
    Continue(DefId),
    Pattern { scrutinee: DefId, pattern: ss::VPatId, success: MatchPlanId, failure: MatchPlanId },
}

#[derive(Clone, Copy)]
enum MatchKind {
    Planned,
    Coproduct,
    Product,
}

/// Each finish frame consumes the completed children of exactly one occurrence.
/// Intermediate frames retain the original interleaving of allocation and descent.
enum Work {
    Pattern(ss::VPatId),
    FinishPattern(ss::VPatId),
    Value(ss::ValueId),
    FinishValue(ss::ValueId),
    Computation(ss::CompuId, StackId),
    ValueSteps(Vec<ValueStep>),
    Bind(ValueBinding),
    Abstraction {
        stack: StackId,
        site: Option<ss::TermId>,
    },
    Application {
        body: ss::CompuId,
        stack: StackId,
        site: Option<ss::TermId>,
    },
    Fix {
        source: ss::CompuId,
        param: DefId,
        stack: StackId,
    },
    Force {
        stack: StackId,
        site: Option<ss::TermId>,
    },
    Return {
        stack: StackId,
        site: Option<ss::TermId>,
    },
    Do {
        bindee: ss::CompuId,
        site: Option<ss::TermId>,
    },
    LetBindee {
        binder: ss::VPatId,
        tail: ss::CompuId,
        stack: StackId,
        site: Option<ss::TermId>,
    },
    Let {
        bindee: ValueId,
        site: Option<ss::TermId>,
    },
    MatchScrutinee {
        arms: Vec<Matcher<ss::VPatId, ss::CompuId>>,
        kind: MatchKind,
        stack: StackId,
        site: Option<ss::TermId>,
    },
    ProductMatch {
        scrut: ValueId,
        site: Option<ss::TermId>,
    },
    MatchArm {
        binder: ss::VPatId,
        tail: ss::CompuId,
        site: Option<ss::TermId>,
    },
    MatchArmBody {
        tail: ss::CompuId,
        site: Option<ss::TermId>,
    },
    CoproductMatch {
        scrut: ValueId,
        count: usize,
        stack: StackId,
        site: Option<ss::TermId>,
    },
    CoMatchArm {
        codata: ss::CoDataId,
        name: DtorName,
        tail: ss::CompuId,
        site: Option<ss::TermId>,
    },
    CoMatch {
        source: ss::CompuId,
        count: usize,
        stack: StackId,
    },
    MatchPlan {
        plan: MatchPlanId,
        stack: StackId,
        site: Option<ss::TermId>,
    },
    PlannedProduct {
        scrutinee: DefId,
        binder: VPatId,
        site: Option<ss::TermId>,
    },
    ConstructorFallback {
        scrutinee: DefId,
        pattern: ss::VPatId,
        success: MatchPlanId,
        continuation: DefId,
        stack: StackId,
        site: Option<ss::TermId>,
    },
    ConstructorArm {
        tag: CtorIdx,
        payload: Option<(ss::VPatId, MatchPlanId)>,
        failure: MatchPlanId,
        site: Option<ss::TermId>,
    },
    PlannedConstructor {
        scrutinee: DefId,
        continuation: DefId,
        fallback: ValueId,
        count: usize,
        stack: StackId,
        site: Option<ss::TermId>,
    },
    LiteralSuccess {
        scrutinee: DefId,
        integer: IntegerLiteral,
        operator: ValueId,
        failure: MatchPlanId,
        stack: StackId,
        site: Option<ss::TermId>,
    },
    LiteralFailure {
        scrutinee: DefId,
        integer: IntegerLiteral,
        operator: ValueId,
        then: ValueId,
        stack: StackId,
        site: Option<ss::TermId>,
    },
}

pub(super) struct LoweringFolder<'lo, 'source> {
    lo: &'lo mut Lowerer<'source>,
    work: Vec<Work>,
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
            work: Vec::new(),
            patterns: Vec::new(),
            values: Vec::new(),
            computations: Vec::new(),
            tags: Vec::new(),
            plans: Vec::new(),
        }
    }

    pub(super) fn lower(mut self, root: ss::CompuId, stack: StackId) -> CompuId {
        self.work.push(Work::Computation(root, stack));
        while let Some(work) = self.work.pop() {
            self.resume(work);
        }
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
        &mut self, binder: ss::VPatId, body: ss::CompuId, stack: StackId, site: Option<ss::TermId>,
        role: &'static str,
    ) {
        if self.lo.pattern_needs_match_plan(binder) {
            let scrutinee = self.lo.alloc_admin_def(role);
            self.patterns.push(scrutinee.build(self.lo, None));
            let success = self.plan(MatchPlan::Tail(body));
            let failure = self.plan(MatchPlan::Fail);
            let plan =
                self.plan(MatchPlan::Pattern { scrutinee, pattern: binder, success, failure });
            self.work.push(Work::MatchPlan { plan, stack, site });
        } else {
            self.work.push(Work::Computation(body, stack));
            self.work.push(Work::Pattern(binder));
        }
    }

    fn visit_pattern(&mut self, source: ss::VPatId) {
        self.work.push(Work::FinishPattern(source));
        match &self.lo.statics.vpats[&source] {
            | ss::ValuePattern::Named(Named(_, child))
            | ss::ValuePattern::Ctor(Ctor(_, child))
            | ss::ValuePattern::SCons(ss::ConsN(_, child)) => {
                self.work.push(Work::Pattern(*child));
            }
            | ss::ValuePattern::Alias(Alias(children)) => {
                self.work.extend(children.iter().rev().copied().map(Work::Pattern));
            }
            | ss::ValuePattern::VCons(children) => {
                self.work.extend(children.iter().rev().copied().map(Work::Pattern));
            }
            | ss::ValuePattern::Hole(_) | ss::ValuePattern::Var(_) | ss::ValuePattern::Triv(_) => {}
            | ss::ValuePattern::View(_) => unreachable!("static elaboration eliminates views"),
            | ss::ValuePattern::Lit(_) => unreachable!("literal patterns require a match plan"),
        }
    }

    fn finish_pattern(&mut self, source: ss::VPatId) {
        let pattern: ValuePattern = match self.lo.statics.vpats[&source].clone() {
            | ss::ValuePattern::Hole(hole) => hole.into(),
            | ss::ValuePattern::Var(def) => def.into(),
            | ss::ValuePattern::Triv(triv) => triv.into(),
            | ss::ValuePattern::Named(Named(_, _)) | ss::ValuePattern::SCons(_) => {
                let child = self.pattern();
                self.lo.arena.inner.vpats[&child].clone()
            }
            | ss::ValuePattern::Ctor(Ctor(name, _)) => {
                let child = self.pattern();
                let data = self.lo.statics.data_pat_hints[&source];
                let idx = self.lo.statics.datas[&data]
                    .iter()
                    .position(|(tag, _)| tag == &name)
                    .expect("Constructor tag not found");
                Ctor(CtorIdx { idx, name }, child).into()
            }
            | ss::ValuePattern::Alias(Alias(children)) => {
                let children = self.patterns.split_off(self.patterns.len() - children.len());
                Alias(ConsN::from_vec(children).unwrap()).into()
            }
            | ss::ValuePattern::VCons(children) => {
                let children = self.patterns.split_off(self.patterns.len() - children.len());
                let layout = self.lo.product_layout(self.lo.statics.annotations_vpat[&source]);
                VCons::new(children, layout).into()
            }
            | ss::ValuePattern::View(_) | ss::ValuePattern::Lit(_) => {
                unreachable!("structural pattern")
            }
        };
        let pattern = pattern.build(self.lo, Some(ss::PatId::Value(source)));
        if let Some(&ty) = self.lo.statics.annotations_vpat.get(&source) {
            self.lo.arena.inner.pattern_protocols.insert_new(pattern, self.lo.protocols.value(ty));
        }
        self.patterns.push(pattern);
    }

    fn visit_value(&mut self, source: ss::ValueId) {
        let site = Some(ss::TermId::Value(source));
        if let Some(import) = self.lo.statics.foreign_imports.get(&source).cloned() {
            let stack = Bullet.build(self.lo, site);
            let body = ExternCall { function: ExternalFunction::Foreign(import), stack }
                .build(self.lo, site);
            let value = Closure { stack: Bullet, body }.build(self.lo, site);
            let protocol = self.lo.protocols.value(self.lo.statics.annotations_value[&source]);
            self.lo.arena.inner.value_protocols.insert_new(value, protocol);
            self.values.push(ValuePlan::pure(value));
            return;
        }
        self.work.push(Work::FinishValue(source));
        match &self.lo.statics.values[&source] {
            | ss::Value::Named(Named(_, child))
            | ss::Value::Ctor(Ctor(_, child))
            | ss::Value::SCons(ss::ConsN(_, child))
            | ss::Value::Proj(Proj(child, _)) => self.work.push(Work::Value(*child)),
            | ss::Value::Let(Let { binder, bindee, tail }) => {
                self.work.extend([
                    Work::Pattern(*binder),
                    Work::Value(*tail),
                    Work::Value(*bindee),
                ]);
            }
            | ss::Value::Thunk(Thunk(body)) => {
                let stack = Bullet.build(self.lo, site);
                self.work.push(Work::Computation(*body, stack));
            }
            | ss::Value::VCons(children) => {
                self.work.extend(children.iter().rev().copied().map(Work::Value));
            }
            | ss::Value::Hole(_)
            | ss::Value::Var(_)
            | ss::Value::Triv(_)
            | ss::Value::Lit(_)
            | ss::Value::ValAbs(_)
            | ss::Value::ValApp(_)
            | ss::Value::Match(_)
            | ss::Value::Int64Op(_) => {}
        }
    }

    fn finish_value(&mut self, source: ss::ValueId) {
        let site = Some(ss::TermId::Value(source));
        let plan = match self.lo.statics.values[&source].clone() {
            | ss::Value::Hole(_) => ValuePlan::pure(Hole.build(self.lo, site)),
            | ss::Value::Var(def) => ValuePlan::pure(def.build(self.lo, site)),
            | ss::Value::Named(_) | ss::Value::SCons(_) => self.value(),
            | ss::Value::Let(_) => {
                let tail = self.value();
                let bindee = self.value();
                let binder = self.pattern();
                let binding = ValueStep::Bind(ValueBinding { binder, bindee: bindee.value, site });
                ValuePlan {
                    steps: bindee.steps.into_iter().chain([binding]).chain(tail.steps).collect(),
                    value: tail.value,
                }
            }
            | ss::Value::ValAbs(_)
            | ss::Value::ValApp(_)
            | ss::Value::Match(_)
            | ss::Value::Int64Op(_) => {
                self.lo.lower_errors.push(SpsLowerError::ResidualStaticValue { value: source });
                ValuePlan::pure(Hole.build(self.lo, site))
            }
            | ss::Value::Thunk(_) => {
                let body = self.computation();
                ValuePlan::pure(Closure { stack: Bullet, body }.build(self.lo, site))
            }
            | ss::Value::Ctor(Ctor(name, _)) => {
                let data = self.lo.statics.data_hints[&source];
                let idx = self.lo.statics.datas[&data]
                    .iter()
                    .position(|(tag, _)| tag == &name)
                    .expect("Constructor tag not found");
                self.value().map(|body| Ctor(CtorIdx { idx, name }, body).build(self.lo, site))
            }
            | ss::Value::Triv(triv) => ValuePlan::pure(triv.build(self.lo, site)),
            | ss::Value::VCons(children) => {
                let layout = self.lo.product_layout(self.lo.statics.annotations_value[&source]);
                let children = self.values.split_off(self.values.len() - children.len());
                ValuePlan::sequence(children)
                    .map(|children| VCons::new(children, layout).build(self.lo, site))
            }
            | ss::Value::Proj(Proj(_, field)) => {
                field.target.products.into_iter().fold(self.value(), |head, projection| {
                    let layout = self.lo.product_layout(projection.product);
                    let (binding, projected) =
                        self.lo.projection_binding(head.value, projection.position, layout, site);
                    head.with_binding(binding, projected)
                })
            }
            | ss::Value::Lit(literal) => ValuePlan::pure(literal.build(self.lo, site)),
        };
        if let Some(&ty) = self.lo.statics.annotations_value.get(&source) {
            let _ =
                self.lo.arena.inner.value_protocols.upsert(plan.value, self.lo.protocols.value(ty));
        }
        self.values.push(plan);
    }

    fn visit_computation(&mut self, source: ss::CompuId, stack: StackId) {
        let site = Some(ss::TermId::Compu(source));
        match self.lo.statics.compus[&source].clone() {
            | ss::Computation::Hole(_) => self.computations.push(SHole(stack).build(self.lo, site)),
            | ss::Computation::VAbs(Abs(param, body)) => {
                let body_stack = Bullet.build(self.lo, site);
                self.work.push(Work::Abstraction { stack, site });
                self.binder_body(param, body, body_stack, site, "__view_argument__");
            }
            | ss::Computation::VApp(App(body, arg)) => {
                self.work.extend([Work::Application { body, stack, site }, Work::Value(arg)]);
            }
            | ss::Computation::TAbs(Abs(_, body)) | ss::Computation::TApp(App(body, _)) => {
                self.work.push(Work::Computation(body, stack));
            }
            | ss::Computation::Fix(Fix(param, body)) => {
                let param = match &self.lo.statics.vpats[&param] {
                    | ss::ValuePattern::Var(def) => *def,
                    | _ => {
                        let fmt =
                            zydeco_statics::fmt::Formatter::new(self.lo.scoped, self.lo.statics);
                        panic!("Fix param must be a variable, found:\n{}", param.ugly(&fmt));
                    }
                };
                let body_stack = Bullet.build(self.lo, site);
                self.work.extend([
                    Work::Fix { source, param, stack },
                    Work::Computation(body, body_stack),
                ]);
            }
            | ss::Computation::Force(Force(body)) => {
                self.work.extend([Work::Force { stack, site }, Work::Value(body)]);
            }
            | ss::Computation::Ret(Return(body)) => {
                self.work.extend([Work::Return { stack, site }, Work::Value(body)]);
            }
            | ss::Computation::Do(Bind { binder, bindee, tail }) => {
                self.work.push(Work::Do { bindee, site });
                self.binder_body(binder, tail, stack, site, "__view_returned__");
            }
            | ss::Computation::Let(Let { binder, bindee, tail }) => {
                self.work
                    .extend([Work::LetBindee { binder, tail, stack, site }, Work::Value(bindee)]);
            }
            | ss::Computation::Match(Match { scrut, arms }) => {
                let mixed = arms.iter().any(|arm| self.lo.is_coprod_pattern(arm.binder))
                    && arms.iter().any(|arm| !self.lo.is_coprod_pattern(arm.binder));
                let kind = if mixed
                    || arms.iter().any(|arm| self.lo.pattern_needs_match_plan(arm.binder))
                {
                    MatchKind::Planned
                } else if self.lo.is_coprod_match(&arms) {
                    MatchKind::Coproduct
                } else {
                    MatchKind::Product
                };
                self.work
                    .extend([Work::MatchScrutinee { arms, kind, stack, site }, Work::Value(scrut)]);
            }
            | ss::Computation::CoMatch(CoMatch { arms }) => {
                let codata = self.lo.statics.codata_hints[&source];
                self.work.push(Work::CoMatch { source, count: arms.len(), stack });
                self.work.extend(arms.into_iter().rev().map(|CoMatcher { dtor: name, tail }| {
                    Work::CoMatchArm { codata, name, tail, site }
                }));
            }
            | ss::Computation::Dtor(Dtor(body, name)) => {
                let codata = self.lo.statics.codata_hints[&body];
                let tag = self.lo.protocols.tag(codata, name);
                let stack = Cons(tag, stack).build(self.lo, site);
                self.work.push(Work::Computation(body, stack));
            }
        }
    }

    fn visit_match_plan(&mut self, plan: MatchPlanId, stack: StackId, site: Option<ss::TermId>) {
        let (scrutinee, pattern, success, failure) = match self.plans[plan.0] {
            | MatchPlan::Fail => {
                self.computations.push(SHole(stack).build(self.lo, site));
                return;
            }
            | MatchPlan::Tail(tail) => {
                self.work.push(Work::Computation(tail, stack));
                return;
            }
            | MatchPlan::Continue(continuation) => {
                let thunk = continuation.build(self.lo, site);
                self.computations.push(SForce { thunk, stack }.build(self.lo, site));
                return;
            }
            | MatchPlan::Pattern { scrutinee, pattern, success, failure } => {
                (scrutinee, pattern, success, failure)
            }
        };
        match self.lo.statics.vpats[&pattern].clone() {
            | ss::ValuePattern::Hole(_) | ss::ValuePattern::Triv(_) => {
                self.work.push(Work::MatchPlan { plan: success, stack, site });
            }
            | ss::ValuePattern::Var(definition) => {
                let bindee = scrutinee.build(self.lo, site);
                let binder = definition.build(self.lo, None);
                self.work.extend([
                    Work::Bind(ValueBinding { binder, bindee, site }),
                    Work::MatchPlan { plan: success, stack, site },
                ]);
            }
            | ss::ValuePattern::Named(Named(_, inner))
            | ss::ValuePattern::SCons(ss::ConsN(_, inner)) => {
                let plan =
                    self.plan(MatchPlan::Pattern { scrutinee, pattern: inner, success, failure });
                self.work.push(Work::MatchPlan { plan, stack, site });
            }
            | ss::ValuePattern::Alias(Alias(patterns)) => {
                let plan = patterns.into_iter().rev().fold(success, |success, pattern| {
                    self.plan(MatchPlan::Pattern { scrutinee, pattern, success, failure })
                });
                self.work.push(Work::MatchPlan { plan, stack, site });
            }
            | ss::ValuePattern::VCons(patterns) => {
                let layout = self.lo.product_layout(self.lo.statics.annotations_vpat[&pattern]);
                let components = patterns
                    .iter()
                    .map(|_| self.lo.alloc_admin_def("__match_component__"))
                    .collect::<Vec<_>>();
                let fields = components.iter().map(|def| def.build(self.lo, None)).collect();
                let binder = VCons::new(fields, layout).build(self.lo, None);
                let plan = patterns.into_iter().zip(components).rev().fold(
                    success,
                    |success, (pattern, scrutinee)| {
                        self.plan(MatchPlan::Pattern { scrutinee, pattern, success, failure })
                    },
                );
                self.work.extend([
                    Work::PlannedProduct { scrutinee, binder, site },
                    Work::MatchPlan { plan, stack, site },
                ]);
            }
            | ss::ValuePattern::Ctor(_) => {
                // Bind remaining rows once; unmatched tags and rejected payloads
                // resume that closure with their own ambient branch stack.
                let continuation = self.lo.alloc_admin_def("__match_fallback__");
                let failure_stack = Bullet.build(self.lo, site);
                self.work.extend([
                    Work::ConstructorFallback {
                        scrutinee,
                        pattern,
                        success,
                        continuation,
                        stack,
                        site,
                    },
                    Work::MatchPlan { plan: failure, stack: failure_stack, site },
                ]);
            }
            | ss::ValuePattern::View(_) => unreachable!("static elaboration eliminates views"),
            | ss::ValuePattern::Lit(literal) => {
                let ss::Literal::Integer(integer) = literal else {
                    unreachable!("a checked literal pattern carries an integer literal")
                };
                let integer_type = integer
                    .integer_type()
                    .expect("a checked integer literal carries its integer type");
                let role = zydeco_syntax::BuiltinValueRole::Integer(
                    integer_type,
                    zydeco_syntax::IntegerOperation::Eq,
                );
                let operator = ExternalFunction::Host(role).make_function(self.lo);
                let success_stack = Bullet.build(self.lo, site);
                self.work.extend([
                    Work::LiteralSuccess { scrutinee, integer, operator, failure, stack, site },
                    Work::MatchPlan { plan: success, stack: success_stack, site },
                ]);
            }
        }
    }

    fn match_arms(&mut self, count: usize) -> Vec<Matcher<VPatId, CompuId>> {
        let patterns = self.patterns.split_off(self.patterns.len() - count);
        let tails = self.computations.split_off(self.computations.len() - count);
        patterns.into_iter().zip(tails).map(|(binder, tail)| Matcher { binder, tail }).collect()
    }

    fn resume(&mut self, work: Work) {
        match work {
            | Work::Pattern(source) => self.visit_pattern(source),
            | Work::FinishPattern(source) => self.finish_pattern(source),
            | Work::Value(source) => self.visit_value(source),
            | Work::FinishValue(source) => self.finish_value(source),
            | Work::Computation(source, stack) => self.visit_computation(source, stack),
            | Work::MatchPlan { plan, stack, site } => self.visit_match_plan(plan, stack, site),
            | Work::ValueSteps(steps) => {
                let tail = self.computation();
                self.computations.push(ValuePlan { steps, value: () }.bind(self.lo, tail));
            }
            | Work::Bind(ValueBinding { binder, bindee, site }) => {
                let tail = self.computation();
                self.computations.push(Let { binder, bindee, tail }.build(self.lo, site));
            }
            | Work::Abstraction { stack, site } => {
                let binder = Cons(self.pattern(), Bullet);
                let tail = self.computation();
                self.computations.push(Let { binder, bindee: stack, tail }.build(self.lo, site));
            }
            | Work::Application { body, stack, site } => {
                let arg = self.value();
                let stack = Cons(arg.value, stack).build(self.lo, site);
                self.work.extend([Work::ValueSteps(arg.steps), Work::Computation(body, stack)]);
            }
            | Work::Fix { source, param, stack } => {
                let body = self.computation();
                let fix = SFix { param, stack, body }.build(self.lo, Some(source.into()));
                if let Some(&ty) = self.lo.statics.annotations_compu.get(&source) {
                    self.lo
                        .arena
                        .inner
                        .compu_protocols
                        .insert_new(fix, self.lo.protocols.stack(ty));
                }
                self.computations.push(fix);
            }
            | Work::Force { stack, site } => {
                let body = self.value();
                let tail = SForce { thunk: body.value, stack }.build(self.lo, site);
                self.computations.push(body.bind(self.lo, tail));
            }
            | Work::Return { stack, site } => {
                let body = self.value();
                let tail = SReturn { stack, value: body.value }.build(self.lo, site);
                self.computations.push(body.bind(self.lo, tail));
            }
            | Work::Do { bindee, site } => {
                let binder = self.pattern();
                let body = self.computation();
                let stack = Kont { binder, body }.build(self.lo, site);
                self.work.push(Work::Computation(bindee, stack));
            }
            | Work::LetBindee { binder, tail, stack, site } => {
                let bindee = self.value();
                self.work.extend([
                    Work::ValueSteps(bindee.steps),
                    Work::Let { bindee: bindee.value, site },
                ]);
                self.binder_body(binder, tail, stack, site, "__view_let__");
            }
            | Work::Let { bindee, site } => {
                let binder = self.pattern();
                let tail = self.computation();
                self.computations.push(Let { binder, bindee, tail }.build(self.lo, site));
            }
            | Work::MatchScrutinee { arms, kind, stack, site } => {
                let scrut = self.value();
                self.work.push(Work::ValueSteps(scrut.steps));
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
                        self.work.extend([
                            Work::Bind(ValueBinding { binder, bindee: scrut.value, site }),
                            Work::MatchPlan { plan, stack, site },
                        ]);
                    }
                    | MatchKind::Coproduct => {
                        self.work.push(Work::CoproductMatch {
                            scrut: scrut.value,
                            count: arms.len(),
                            stack,
                            site,
                        });
                        self.work.extend(
                            arms.into_iter().rev().map(|Matcher { binder, tail }| Work::MatchArm {
                                binder,
                                tail,
                                site,
                            }),
                        );
                    }
                    | MatchKind::Product => {
                        let [Matcher { binder, tail }] = arms.as_slice() else {
                            unreachable!("an irrefutable match has exactly one arm")
                        };
                        self.work.extend([
                            Work::ProductMatch { scrut: scrut.value, site },
                            Work::Computation(*tail, stack),
                            Work::Pattern(*binder),
                        ]);
                    }
                }
            }
            | Work::ProductMatch { scrut, site } => {
                let binder = self.pattern();
                let body = self.computation();
                self.computations.push(SProductMatch { scrut, binder, body }.build(self.lo, site));
            }
            | Work::MatchArm { binder, tail, site } => {
                self.work.extend([Work::MatchArmBody { tail, site }, Work::Pattern(binder)]);
            }
            | Work::MatchArmBody { tail, site } => {
                let stack = Bullet.build(self.lo, site);
                self.work.push(Work::Computation(tail, stack));
            }
            | Work::CoproductMatch { scrut, count, stack, site } => {
                let arms = self.match_arms(count);
                let tail = SCoprodMatch { scrut, arms }.build(self.lo, site);
                self.computations
                    .push(Let { binder: Bullet, bindee: stack, tail }.build(self.lo, site));
            }
            | Work::CoMatchArm { codata, name, tail, site } => {
                self.tags.push(self.lo.protocols.tag(codata, name));
                let stack = Bullet.build(self.lo, site);
                self.work.push(Work::Computation(tail, stack));
            }
            | Work::CoMatch { source, count, stack } => {
                let tags = self.tags.split_off(self.tags.len() - count);
                let tails = self.computations.split_off(self.computations.len() - count);
                let arms = tags
                    .into_iter()
                    .zip(tails)
                    .map(|(tag, tail)| CoMatcher { dtor: Cons(tag, Bullet), tail })
                    .collect();
                let case = SCoMatch { scrut: stack, arms }.build(self.lo, Some(source.into()));
                let protocol = self.lo.protocols.codata(self.lo.statics.codata_hints[&source]);
                self.lo.arena.inner.compu_protocols.insert_new(case, protocol);
                self.computations.push(case);
            }
            | Work::PlannedProduct { scrutinee, binder, site } => {
                let body = self.computation();
                let scrut = scrutinee.build(self.lo, site);
                self.computations.push(SProductMatch { scrut, binder, body }.build(self.lo, site));
            }
            | Work::ConstructorFallback {
                scrutinee,
                pattern,
                success,
                continuation,
                stack,
                site,
            } => {
                let body = self.computation();
                let fallback = Closure { stack: Bullet, body }.build(self.lo, site);
                let failure = self.plan(MatchPlan::Continue(continuation));
                let ss::ValuePattern::Ctor(Ctor(name, argument)) =
                    self.lo.statics.vpats[&pattern].clone()
                else {
                    unreachable!("constructor fallback")
                };
                let data = self.lo.statics.data_pat_hints[&pattern];
                let constructors = &self.lo.statics.datas[&data];
                self.work.push(Work::PlannedConstructor {
                    scrutinee,
                    continuation,
                    fallback,
                    count: constructors.len(),
                    stack,
                    site,
                });
                self.work.extend(
                    constructors.iter().enumerate().collect::<Vec<_>>().into_iter().rev().map(
                        |(idx, (candidate, _))| Work::ConstructorArm {
                            tag: CtorIdx { idx, name: candidate.clone() },
                            payload: (candidate == &name).then_some((argument, success)),
                            failure,
                            site,
                        },
                    ),
                );
            }
            | Work::ConstructorArm { tag, payload, failure, site } => {
                let stack = Bullet.build(self.lo, site);
                let (binder, plan) = if let Some((pattern, success)) = payload {
                    let scrutinee = self.lo.alloc_admin_def("__match_payload__");
                    let binder = scrutinee.build(self.lo, None);
                    let plan =
                        self.plan(MatchPlan::Pattern { scrutinee, pattern, success, failure });
                    (binder, plan)
                } else {
                    (Hole.build(self.lo, None), failure)
                };
                self.patterns.push(Ctor(tag, binder).build(self.lo, None));
                self.work.push(Work::MatchPlan { plan, stack, site });
            }
            | Work::PlannedConstructor {
                scrutinee,
                continuation,
                fallback,
                count,
                stack,
                site,
            } => {
                let arms = self.match_arms(count);
                let scrut = scrutinee.build(self.lo, site);
                let body = SCoprodMatch { scrut, arms }.build(self.lo, site);
                let tail = Let { binder: Bullet, bindee: stack, tail: body }.build(self.lo, site);
                let binder = continuation.build(self.lo, None);
                self.computations.push(Let { binder, bindee: fallback, tail }.build(self.lo, site));
            }
            | Work::LiteralSuccess { scrutinee, integer, operator, failure, stack, site } => {
                let body = self.computation();
                let then = Closure { stack: Bullet, body }.build(self.lo, site);
                let failure_stack = Bullet.build(self.lo, site);
                self.work.extend([
                    Work::LiteralFailure { scrutinee, integer, operator, then, stack, site },
                    Work::MatchPlan { plan: failure, stack: failure_stack, site },
                ]);
            }
            | Work::LiteralFailure { scrutinee, integer, operator, then, stack, site } => {
                let body = self.computation();
                let otherwise = Closure { stack: Bullet, body }.build(self.lo, site);
                let scrut: ValueId = scrutinee.build(self.lo, site);
                let literal: ValueId =
                    zydeco_syntax::Literal::Integer(integer).build(self.lo, site);
                let stack = Cons(otherwise, stack).build(self.lo, site);
                let stack = Cons(then, stack).build(self.lo, site);
                let stack = Cons(literal, stack).build(self.lo, site);
                let stack = Cons(scrut, stack).build(self.lo, site);
                self.computations.push(SForce { thunk: operator, stack }.build(self.lo, site));
            }
        }
    }
}
