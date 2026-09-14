//! Reconstruction frames preserve forward facts and backward consumer demands.

use super::*;

enum Work {
    Value(ValueId, EnvId, Demand),
    FinishValue(ValueId, bool),
    Stack(ScopedStack),
    FinishStack(StackId),
    ContinuationBody {
        source: StackId,
        binder: VPatId,
    },
    Computation(CompuId, Scope),
    FinishComputation(CompuId),
    FixBody {
        source: CompuId,
        param: DefId,
        body: CompuId,
        scope: Scope,
    },
    FinishFix {
        source: CompuId,
        param: DefId,
    },
    ArgumentBody {
        source: CompuId,
        binder: VPatId,
        stack: ScopedStack,
    },
    Binding {
        binder: VPatId,
        bindee: ScopedValue,
        tail: CompuId,
        scope: Scope,
        site: Option<ss::TermId>,
    },
    ResidualBinding {
        binder: VPatId,
        bindee: ScopedValue,
        site: Option<ss::TermId>,
    },
    FinishBinding {
        binder: VPatId,
        site: Option<ss::TermId>,
    },
    BranchArm {
        binder: VPatId,
        tail: CompuId,
        env: EnvId,
        known: Rc<KnownValue>,
    },
    FinishBranchArm(VPatId),
    BranchScrutinee {
        scrut: ValueId,
        count: usize,
        tail: CompuId,
        bindee: ScopedStack,
        site: Option<ss::TermId>,
    },
    BranchTail {
        count: usize,
        tail: CompuId,
        bindee: ScopedStack,
        demands: Demands,
        site: Option<ss::TermId>,
    },
    BranchStack {
        tail: CompuId,
        demands: Demands,
        site: Option<ss::TermId>,
    },
    PrimitiveValue {
        operation: PrimitiveOp,
        operands: [ScopedValue; 2],
        site: Option<ss::TermId>,
    },
    FinishPrimitiveValue {
        operation: PrimitiveOp,
        site: Option<ss::TermId>,
    },
    PrimitiveContinuation {
        binder: VPatId,
        operation: PrimitiveOp,
        operands: [ScopedValue; 2],
        folded: bool,
        site: Option<ss::TermId>,
    },
    PrimitiveReturn {
        site: Option<ss::TermId>,
    },
    PrimitiveStack {
        operation: PrimitiveOp,
        site: Option<ss::TermId>,
    },
    ExternalStack {
        function: ExternalFunction,
        site: Option<ss::TermId>,
    },
}

pub(super) struct NormalizationFolder<'a> {
    norm: &'a mut Normalization,
    work: Vec<Work>,
    values: Vec<Residual<ValueId>>,
    stacks: Vec<Residual<StackId>>,
    computations: Vec<Residual<CompuId>>,
    arms: Vec<(Matcher<VPatId, CompuId>, Demands, Demand)>,
}

impl<'a> NormalizationFolder<'a> {
    pub(super) fn new(norm: &'a mut Normalization) -> Self {
        Self {
            norm,
            work: Vec::new(),
            values: Vec::new(),
            stacks: Vec::new(),
            computations: Vec::new(),
            arms: Vec::new(),
        }
    }

    pub(super) fn run(mut self, root: CompuId) -> CompuId {
        self.work.push(Work::Computation(root, Scope { values: EnvId(0), stack: None }));
        while let Some(work) = self.work.pop() {
            self.resume(work);
        }
        assert!(self.values.is_empty() && self.stacks.is_empty() && self.arms.is_empty());
        let root = self.computation().node;
        assert!(self.computations.is_empty());
        root
    }

    fn value(&mut self) -> Residual<ValueId> {
        self.values.pop().expect("completed value child")
    }

    fn stack(&mut self) -> Residual<StackId> {
        self.stacks.pop().expect("completed stack child")
    }

    fn computation(&mut self) -> Residual<CompuId> {
        self.computations.pop().expect("completed computation child")
    }

    fn finish_computation(
        &mut self, source: CompuId, compu: impl Into<Computation<LetJoin>>, demands: Demands,
    ) {
        let site = self.norm.source.admin.terms.back(&TermId::Compu(source)).copied();
        let node = compu.into().build(self.norm, site);
        if let Some(protocol) = self.norm.source.inner.compu_protocols.get(&source) {
            self.norm.arena.inner.compu_protocols.insert_new(node, protocol.clone());
        }
        self.computations.push(Residual { node, demands });
    }

    fn visit_value(&mut self, id: ValueId, env: EnvId, demand: Demand) {
        let site = self.norm.source.admin.terms.back(&TermId::Value(id)).copied();
        if demand.is_absent() && self.norm.discardable(id) {
            self.values
                .push(Residual { node: Triv.build(self.norm, site), demands: Demands::default() });
            return;
        }
        let protocol = matches!(demand, Demand::Used);
        match self.norm.source.inner.values[&id].clone() {
            | Value::Var(def) => {
                let (value, demands): (Value, _) = match self.norm.lookup(env, def).as_ref() {
                    | KnownValue::Alias(alias) => {
                        ((*alias).into(), Demands::singleton(*alias, demand))
                    }
                    | KnownValue::Triv => (Triv.into(), Demands::default()),
                    | KnownValue::Literal(literal) if self.norm.occurrences.get(&def) == Some(&1) => {
                        (literal.clone().into(), Demands::default())
                    }
                    | _ => (def.into(), Demands::singleton(def, demand)),
                };
                self.build_value(id, value, demands, protocol);
            }
            | Value::Closure(Closure { body, .. }) => {
                if let Computation::Force(SForce { thunk, stack }) =
                    self.norm.source.inner.compus[&body]
                    && matches!(self.norm.source.inner.stacks[&stack], Stack::Var(Bullet))
                    && matches!(self.norm.source.inner.values[&thunk], Value::Var(_))
                {
                    self.work.push(Work::Value(thunk, env, demand));
                    return;
                }
                self.work.extend([
                    Work::FinishValue(id, protocol),
                    Work::Computation(body, Scope { values: env, stack: None }),
                ]);
            }
            | Value::Ctor(Ctor(_, body)) => {
                let demand = if demand.is_absent() { Demand::Absent } else { Demand::Used };
                self.work.extend([Work::FinishValue(id, protocol), Work::Value(body, env, demand)]);
            }
            | Value::VCons(VCons { items, layout }) => {
                let demand =
                    if demand.is_absent() { Demand::Fields(Default::default()) } else { demand };
                let count = items.len();
                self.work.push(Work::FinishValue(id, protocol));
                self.work.extend(items.into_iter().enumerate().rev().map(|(position, item)| {
                    Work::Value(item, env, demand.item(position, count, layout))
                }));
            }
            | Value::Primitive(Primitive { operation, operands }) => {
                self.work.push(Work::PrimitiveValue {
                    operation,
                    operands: operands.map(|node| ScopedValue { node, env }),
                    site,
                })
            }
            | value => self.build_value(id, value, Demands::default(), protocol),
        }
    }

    fn build_value(
        &mut self, source: ValueId, value: impl Into<Value>, demands: Demands, protocol: bool,
    ) {
        let site = self.norm.source.admin.terms.back(&TermId::Value(source)).copied();
        let node = value.into().build(self.norm, site);
        if protocol && let Some(protocol) = self.norm.source.inner.value_protocols.get(&source) {
            self.norm.arena.inner.value_protocols.insert_new(node, protocol.clone());
        }
        self.values.push(Residual { node, demands });
    }

    fn visit_stack(&mut self, stack: ScopedStack) {
        let ScopedStack { node: id, scope } = self.norm.resolve_stack(stack);
        match self.norm.source.inner.stacks[&id].clone() {
            | Stack::Var(bullet) => {
                let site = self.norm.source.admin.terms.back(&TermId::Stack(id)).copied();
                self.stacks.push(Residual {
                    node: bullet.build(self.norm, site),
                    demands: Demands::default(),
                });
            }
            | Stack::Arg(Cons(value, rest)) => {
                self.work.extend([
                    Work::FinishStack(id),
                    Work::Stack(ScopedStack { node: rest, scope: scope.clone() }),
                    Work::Value(value, scope.values, Demand::Used),
                ]);
            }
            | Stack::Tag(Cons(_, rest)) => self
                .work
                .extend([Work::FinishStack(id), Work::Stack(ScopedStack { node: rest, scope })]),
            | Stack::Kont(Kont { binder, body }) => {
                if let ValuePattern::Var(def) = self.norm.source.inner.vpats[&binder]
                    && let Computation::Ret(SReturn { value, stack }) =
                        self.norm.source.inner.compus[&body]
                    && matches!(self.norm.source.inner.values[&value], Value::Var(returned) if returned == def)
                    && matches!(self.norm.source.inner.stacks[&stack], Stack::Var(Bullet))
                {
                    self.work.push(Work::Stack(ScopedStack { node: stack, scope }));
                    return;
                }
                let values = self.norm.bind(scope.values, binder, Rc::default());
                self.work.extend([
                    Work::ContinuationBody { source: id, binder },
                    Work::Computation(body, Scope { values, ..scope }),
                ]);
            }
        }
    }

    fn visit_computation(&mut self, id: CompuId, scope: Scope) {
        let site = self.norm.source.admin.terms.back(&TermId::Compu(id)).copied();
        match self.norm.source.inner.compus[&id].clone() {
            | Computation::Hole(SHole(stack)) => self.work.extend([
                Work::FinishComputation(id),
                Work::Stack(ScopedStack { node: stack, scope }),
            ]),
            | Computation::Force(SForce { thunk, stack }) => {
                let known = self.norm.known(thunk, scope.values);
                let stack = ScopedStack { node: stack, scope: scope.clone() };
                if let KnownValue::External(function) = known.as_ref() {
                    self.external_call(function.clone(), stack, site);
                } else if self.norm.movable_stack(stack.node)
                    && let Value::Closure(Closure { body, .. }) =
                        self.norm.source.inner.values[&thunk]
                {
                    let stack = Some(self.norm.delay_stack(stack));
                    self.work.push(Work::Computation(body, Scope { values: scope.values, stack }));
                } else if self.norm.movable_stack(stack.node)
                    && let KnownValue::Closure { body, env } = known.as_ref()
                {
                    let stack = Some(self.norm.delay_stack(stack));
                    self.work.push(Work::Computation(*body, Scope { values: *env, stack }));
                } else {
                    self.work.extend([
                        Work::FinishComputation(id),
                        Work::Value(thunk, scope.values, Demand::Used),
                        Work::Stack(stack),
                    ]);
                }
            }
            | Computation::Ret(SReturn { stack, value }) => {
                let stack =
                    self.norm.resolve_stack(ScopedStack { node: stack, scope: scope.clone() });
                if let Stack::Kont(Kont { binder, body }) =
                    self.norm.source.inner.stacks[&stack.node]
                {
                    self.work.push(Work::Binding {
                        binder,
                        bindee: ScopedValue { node: value, env: scope.values },
                        tail: body,
                        scope: stack.scope,
                        site,
                    });
                } else {
                    self.work.extend([
                        Work::FinishComputation(id),
                        Work::Value(value, scope.values, Demand::Used),
                        Work::Stack(stack),
                    ]);
                }
            }
            | Computation::Fix(SFix { param, stack, body }) => {
                self.work.extend([
                    Work::FixBody { source: id, param, body, scope: scope.clone() },
                    Work::Stack(ScopedStack { node: stack, scope }),
                ]);
            }
            | Computation::ProductMatch(SProductMatch { scrut: bindee, binder, body: tail })
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                self.work.push(Work::Binding {
                    binder,
                    bindee: ScopedValue { node: bindee, env: scope.values },
                    tail,
                    scope,
                    site,
                })
            }
            | Computation::Join(LetJoin::Stack(Let { bindee, tail, .. })) => {
                self.branch(bindee, tail, scope, site)
            }
            | Computation::CoprodMatch(_) => {
                unreachable!("coproduct matches are handled with their stack join")
            }
            | Computation::LetArg(Let { binder: Cons(binder, Bullet), bindee, tail }) => {
                let stack =
                    self.norm.resolve_stack(ScopedStack { node: bindee, scope: scope.clone() });
                if let Stack::Arg(Cons(value, rest)) = self.norm.source.inner.stacks[&stack.node]
                    && self.norm.movable_stack(rest)
                {
                    let value = ScopedValue { node: value, env: stack.scope.values };
                    let rest =
                        Some(self.norm.delay_stack(ScopedStack { node: rest, scope: stack.scope }));
                    self.work.push(Work::Binding {
                        binder,
                        bindee: value,
                        tail,
                        scope: Scope { stack: rest, ..scope },
                        site,
                    });
                } else {
                    let values = self.norm.bind(scope.values, binder, Rc::default());
                    self.work.extend([
                        Work::ArgumentBody { source: id, binder, stack },
                        Work::Computation(tail, Scope { values, stack: None }),
                    ]);
                }
            }
            | Computation::CoCase(SCoMatch { scrut, arms }) => {
                let stack =
                    self.norm.resolve_stack(ScopedStack { node: scrut, scope: scope.clone() });
                if let Stack::Tag(Cons(tag, rest)) = &self.norm.source.inner.stacks[&stack.node]
                    && self.norm.movable_stack(*rest)
                    && let Some(CoMatcher { tail, .. }) = arms.iter().find(|arm| arm.dtor.0 == *tag)
                {
                    let rest = Some(
                        self.norm.delay_stack(ScopedStack { node: *rest, scope: stack.scope }),
                    );
                    self.work.push(Work::Computation(*tail, Scope { stack: rest, ..scope }));
                } else {
                    self.work.push(Work::FinishComputation(id));
                    self.work.extend(arms.into_iter().rev().map(|arm| {
                        Work::Computation(arm.tail, Scope { values: scope.values, stack: None })
                    }));
                    self.work.push(Work::Stack(stack));
                }
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                self.external_call(function, ScopedStack { node: stack, scope }, site)
            }
        }
    }

    fn branch(&mut self, bindee: StackId, tail: CompuId, scope: Scope, site: Option<ss::TermId>) {
        let Computation::CoprodMatch(SCoprodMatch { scrut, arms }) =
            self.norm.source.inner.compus[&tail].clone()
        else {
            unreachable!("branch-join input guards exactly a coproduct match")
        };
        let known = self.norm.shared(scrut, scope.values);
        if self.norm.movable_stack(bindee) {
            for arm in &arms {
                match self.norm.matches(arm.binder, &known) {
                    | Some(true) => {
                        let value = ScopedValue { node: scrut, env: scope.values };
                        let stack = Some(
                            self.norm
                                .delay_stack(ScopedStack { node: bindee, scope: scope.clone() }),
                        );
                        self.work.push(Work::Binding {
                            binder: arm.binder,
                            bindee: value,
                            tail: arm.tail,
                            scope: Scope { stack, ..scope },
                            site,
                        });
                        return;
                    }
                    | Some(false) => {}
                    | None => break,
                }
            }
        }
        self.work.push(Work::BranchScrutinee {
            scrut,
            count: arms.len(),
            tail,
            bindee: ScopedStack { node: bindee, scope: scope.clone() },
            site,
        });
        self.work.extend(arms.into_iter().rev().map(|arm| Work::BranchArm {
            binder: arm.binder,
            tail: arm.tail,
            env: scope.values,
            known: known.clone(),
        }));
    }

    fn external_call(
        &mut self, function: ExternalFunction, stack: ScopedStack, site: Option<ss::TermId>,
    ) {
        if let ExternalFunction::Host(role) = &function
            && let Some(operation) = PrimitiveOp::from_builtin(*role)
        {
            if let Some((operands, rest)) = self.norm.primitive_arguments(stack.clone()) {
                if let Stack::Kont(Kont { binder, body }) =
                    self.norm.source.inner.stacks[&rest.node]
                {
                    let folded = self.norm.fold_primitive(operation, operands);
                    let known = folded
                        .clone()
                        .map_or_else(Rc::default, |value| Rc::new(KnownValue::Literal(value)));
                    let values = self.norm.bind(rest.scope.values, binder, known);
                    self.work.extend([
                        Work::PrimitiveContinuation {
                            binder,
                            operation,
                            operands,
                            folded: folded.is_some(),
                            site,
                        },
                        Work::Computation(body, Scope { values, ..rest.scope }),
                    ]);
                } else {
                    self.work.extend([
                        Work::PrimitiveReturn { site },
                        Work::PrimitiveValue { operation, operands, site },
                        Work::Stack(rest),
                    ]);
                }
            } else {
                self.work.extend([Work::PrimitiveStack { operation, site }, Work::Stack(stack)]);
            }
        } else {
            self.work.extend([Work::ExternalStack { function, site }, Work::Stack(stack)]);
        }
    }

    fn resume(&mut self, work: Work) {
        match work {
            | Work::Value(id, env, demand) => self.visit_value(id, env, demand),
            | Work::Stack(stack) => self.visit_stack(stack),
            | Work::Computation(id, scope) => self.visit_computation(id, scope),
            | Work::FinishValue(id, protocol) => match self.norm.source.inner.values[&id].clone() {
                | Value::Closure(Closure { stack, .. }) => {
                    let body = self.computation();
                    self.build_value(
                        id,
                        Closure { stack, body: body.node },
                        body.demands,
                        protocol,
                    );
                }
                | Value::Ctor(Ctor(tag, _)) => {
                    let body = self.value();
                    self.build_value(id, Ctor(tag, body.node), body.demands, protocol);
                }
                | Value::VCons(VCons { items, layout }) => {
                    let children = self.values.split_off(self.values.len() - items.len());
                    let (items, demands): (Vec<_>, Vec<_>) =
                        children.into_iter().map(|child| (child.node, child.demands)).unzip();
                    self.build_value(
                        id,
                        VCons::new(items, layout),
                        demands.into_iter().fold(Demands::default(), Demands::join),
                        protocol,
                    );
                }
                | _ => unreachable!("value with scheduled children"),
            },
            | Work::FinishStack(id) => {
                let site = self.norm.source.admin.terms.back(&TermId::Stack(id)).copied();
                let tail = self.stack();
                let (stack, demands): (Stack, _) = match self.norm.source.inner.stacks[&id].clone()
                {
                    | Stack::Arg(_) => {
                        let value = self.value();
                        (Cons(value.node, tail.node).into(), value.demands.join(tail.demands))
                    }
                    | Stack::Tag(Cons(tag, _)) => (Cons(tag, tail.node).into(), tail.demands),
                    | _ => unreachable!("stack with scheduled tail"),
                };
                self.stacks.push(Residual { node: stack.build(self.norm, site), demands });
            }
            | Work::ContinuationBody { source, binder } => {
                let mut body = self.computation();
                for def in binder.vars(&self.norm.source) {
                    body.demands.remove(&def);
                }
                let binder = self.norm.pattern(binder);
                let site = self.norm.source.admin.terms.back(&TermId::Stack(source)).copied();
                let node = Kont { binder, body: body.node }.build(self.norm, site);
                self.stacks.push(Residual { node, demands: body.demands });
            }
            | Work::FinishComputation(id) => match self.norm.source.inner.compus[&id].clone() {
                | Computation::Hole(_) => {
                    let stack = self.stack();
                    self.finish_computation(id, SHole(stack.node), stack.demands);
                }
                | Computation::Force(_) => {
                    let thunk = self.value();
                    let stack = self.stack();
                    self.finish_computation(
                        id,
                        SForce { thunk: thunk.node, stack: stack.node },
                        thunk.demands.join(stack.demands),
                    );
                }
                | Computation::Ret(_) => {
                    let value = self.value();
                    let stack = self.stack();
                    self.finish_computation(
                        id,
                        SReturn { stack: stack.node, value: value.node },
                        stack.demands.join(value.demands),
                    );
                }
                | Computation::LetArg(Let { binder: Cons(binder, Bullet), .. }) => {
                    let tail = self.computation();
                    let bindee = self.stack();
                    let binder = Cons(self.norm.pattern(binder), Bullet);
                    self.finish_computation(
                        id,
                        Let { binder, bindee: bindee.node, tail: tail.node },
                        bindee.demands.join(tail.demands),
                    );
                }
                | Computation::CoCase(SCoMatch { arms, .. }) => {
                    let tails = self.computations.split_off(self.computations.len() - arms.len());
                    let (arms, demands): (Vec<_>, Vec<_>) = arms
                        .into_iter()
                        .zip(tails)
                        .map(|(arm, tail)| {
                            (CoMatcher { dtor: arm.dtor, tail: tail.node }, tail.demands)
                        })
                        .unzip();
                    let scrut = self.stack();
                    self.finish_computation(
                        id,
                        SCoMatch { scrut: scrut.node, arms },
                        demands.into_iter().fold(scrut.demands, Demands::join),
                    );
                }
                | _ => unreachable!("computation with ordinary reconstruction"),
            },
            | Work::FixBody { source, param, body, scope } => {
                let values = self.norm.extend(scope.values, [(param, Rc::default())]);
                self.work.extend([
                    Work::FinishFix { source, param },
                    Work::Computation(body, Scope { values, stack: None }),
                ]);
            }
            | Work::FinishFix { source, param } => {
                let mut body = self.computation();
                body.demands.remove(&param);
                let stack = self.stack();
                self.finish_computation(
                    source,
                    SFix { param, stack: stack.node, body: body.node },
                    stack.demands.join(body.demands),
                );
            }
            | Work::ArgumentBody { source, binder, stack } => {
                let tail = self.computations.last_mut().expect("argument consumer body");
                for def in binder.vars(&self.norm.source) {
                    tail.demands.remove(&def);
                }
                self.work.extend([Work::FinishComputation(source), Work::Stack(stack)]);
            }
            | Work::Binding { binder, bindee, tail, scope, site } => {
                let values =
                    self.norm.extend(scope.values, self.norm.binding_facts(binder, bindee));
                self.work.extend([
                    Work::ResidualBinding { binder, bindee, site },
                    Work::Computation(tail, Scope { values, ..scope }),
                ]);
            }
            | Work::ResidualBinding { binder, bindee, site } => {
                let bound = binder.vars(&self.norm.source);
                let mut tail = self.computation();
                if !bound.iter().any(|def| tail.demands.contains(def))
                    && self.norm.discardable(bindee.node)
                {
                    self.computations.push(tail);
                    return;
                }
                if let Some(components) = self.norm.components(binder, bindee) {
                    self.computations.push(tail);
                    // Last component consumes tail demands first; emitted bindings
                    // still execute from the first component to the last.
                    self.work.extend(
                        components.into_iter().map(|(binder, bindee)| Work::ResidualBinding {
                            binder,
                            bindee,
                            site,
                        }),
                    );
                    return;
                }
                let demand = tail.demands.pattern(&self.norm.source, binder);
                for def in bound {
                    tail.demands.remove(&def);
                }
                self.computations.push(tail);
                self.work.extend([
                    Work::FinishBinding { binder, site },
                    Work::Value(bindee.node, bindee.env, demand),
                ]);
            }
            | Work::FinishBinding { binder, site } => {
                let bindee = self.value();
                let binder = self.norm.pattern(binder);
                let tail = self.computation();
                let node =
                    Let { binder, bindee: bindee.node, tail: tail.node }.build(self.norm, site);
                self.computations
                    .push(Residual { node, demands: tail.demands.join(bindee.demands) });
            }
            | Work::BranchArm { binder, tail, env, known } => {
                let values = self.norm.bind(env, binder, known);
                self.work.extend([
                    Work::FinishBranchArm(binder),
                    Work::Computation(tail, Scope { values, stack: None }),
                ]);
            }
            | Work::FinishBranchArm(binder) => {
                let mut tail = self.computation();
                let demand = tail.demands.pattern(&self.norm.source, binder);
                for def in binder.vars(&self.norm.source) {
                    tail.demands.remove(&def);
                }
                let binder = self.norm.pattern(binder);
                self.arms.push((Matcher { binder, tail: tail.node }, tail.demands, demand));
            }
            | Work::BranchScrutinee { scrut, count, tail, bindee, site } => {
                let first = self.arms.len() - count;
                let (demands, demand) = self.arms[first..].iter_mut().fold(
                    (Demands::default(), Demand::Absent),
                    |(demands, demand), (_, more, scrut)| {
                        (demands.join(std::mem::take(more)), demand.join(std::mem::take(scrut)))
                    },
                );
                let demand = if demand.is_absent() { Demand::Used } else { demand };
                let env = bindee.scope.values;
                self.work.extend([
                    Work::BranchTail { count, tail, bindee, demands, site },
                    Work::Value(scrut, env, demand),
                ]);
            }
            | Work::BranchTail { count, tail, bindee, demands, site } => {
                let scrut = self.value();
                let arms = self
                    .arms
                    .split_off(self.arms.len() - count)
                    .into_iter()
                    .map(|(arm, _, _)| arm)
                    .collect();
                let branch_site = self.norm.source.admin.terms.back(&TermId::Compu(tail)).copied();
                let tail = SCoprodMatch { scrut: scrut.node, arms }.build(self.norm, branch_site);
                self.work.extend([
                    Work::BranchStack { tail, demands: demands.join(scrut.demands), site },
                    Work::Stack(bindee),
                ]);
            }
            | Work::BranchStack { tail, demands, site } => {
                let stack = self.stack();
                let node = Let { binder: Bullet, bindee: stack.node, tail }.build(self.norm, site);
                self.computations.push(Residual { node, demands: demands.join(stack.demands) });
            }
            | Work::PrimitiveValue { operation, operands, site } => {
                if let Some(literal) = self.norm.fold_primitive(operation, operands) {
                    self.values.push(Residual {
                        node: literal.build(self.norm, site),
                        demands: Demands::default(),
                    });
                } else {
                    self.work.push(Work::FinishPrimitiveValue { operation, site });
                    self.work.extend(
                        operands
                            .into_iter()
                            .rev()
                            .map(|value| Work::Value(value.node, value.env, Demand::Used)),
                    );
                }
            }
            | Work::FinishPrimitiveValue { operation, site } => {
                let second = self.value();
                let first = self.value();
                let node = Primitive { operation, operands: [first.node, second.node] }
                    .build(self.norm, site);
                self.values.push(Residual { node, demands: first.demands.join(second.demands) });
            }
            | Work::PrimitiveContinuation { binder, operation, operands, folded, site } => {
                let mut tail = self.computation();
                let bound = binder.vars(&self.norm.source);
                let discardable = folded
                    || (!operation.may_trap()
                        && operands.iter().all(|value| self.norm.discardable(value.node)));
                if discardable && !bound.iter().any(|def| tail.demands.contains(def)) {
                    self.computations.push(tail);
                    return;
                }
                for def in bound {
                    tail.demands.remove(&def);
                }
                self.computations.push(tail);
                self.work.extend([
                    Work::FinishBinding { binder, site },
                    Work::PrimitiveValue { operation, operands, site },
                ]);
            }
            | Work::PrimitiveReturn { site } => {
                let value = self.value();
                let stack = self.stack();
                let node = SReturn { stack: stack.node, value: value.node }.build(self.norm, site);
                self.computations
                    .push(Residual { node, demands: stack.demands.join(value.demands) });
            }
            | Work::PrimitiveStack { operation, site } => {
                let stack = self.stack();
                let defs = ["__primitive_first__", "__primitive_second__"].map(|name| {
                    let def = self.norm.arena.admin.fresh();
                    self.norm.arena.admin.insert_def(def, VarName(name.into()));
                    def
                });
                let operands = defs.map(|def| def.build(self.norm, site));
                let value = Primitive { operation, operands }.build(self.norm, site);
                let ambient = Bullet.build(self.norm, site);
                let tail = SReturn { stack: ambient, value }.build(self.norm, site);
                let second = defs[1].build(self.norm, None);
                let ambient = Bullet.build(self.norm, site);
                let tail = Let { binder: Cons(second, Bullet), bindee: ambient, tail }
                    .build(self.norm, site);
                let first = defs[0].build(self.norm, None);
                let node = Let { binder: Cons(first, Bullet), bindee: stack.node, tail }
                    .build(self.norm, site);
                self.computations.push(Residual { node, demands: stack.demands });
            }
            | Work::ExternalStack { function, site } => {
                let stack = self.stack();
                let node = ExternCall { function, stack: stack.node }.build(self.norm, site);
                self.computations.push(Residual { node, demands: stack.demands });
            }
        }
    }
}
