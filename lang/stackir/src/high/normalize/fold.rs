//! Reconstruction frames preserve forward facts and backward consumer demands.

use super::*;
use std::marker::PhantomData;
use zydeco_utils::fold::{Folder, Step};

pub(super) enum Work {
    Value(ValueId, EnvId, Demand),
    FinishValue(ValueId, bool),
    OffsetDisplacement {
        source: ValueId,
        displacement: ScopedValue,
        protocol: bool,
    },
    ValueChildren {
        source: ValueId,
        position: usize,
        env: EnvId,
        demand: Demand,
        protocol: bool,
    },
    Stack(ScopedStack),
    FinishStack(StackId),
    StackRest {
        source: StackId,
        stack: ScopedStack,
    },
    ContinuationBody {
        source: StackId,
        binder: VPatId,
    },
    Computation(CompuId, Scope),
    MemoryAddress(CompuId, EnvId),
    MemoryValue(CompuId, EnvId),
    FinishMemory(CompuId),
    FinishComputation(CompuId),
    ComputationValue {
        source: CompuId,
        value: ScopedValue,
    },
    CoCaseArms {
        source: CompuId,
        position: usize,
        env: EnvId,
    },
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
    BindingComponents {
        remaining: std::vec::IntoIter<(VPatId, ScopedValue)>,
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
    BranchArms {
        source: CompuId,
        position: usize,
        known: Rc<KnownValue>,
        bindee: ScopedStack,
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
    PrimitiveSecond {
        operation: PrimitiveOp,
        second: ScopedValue,
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
    PrimitiveResult {
        operation: PrimitiveOp,
        operands: [ScopedValue; 2],
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

pub(super) struct NormalizationFolder<'a, D> {
    norm: &'a mut Normalization,
    driver: PhantomData<D>,
    values: Vec<Residual<ValueId>>,
    stacks: Vec<Residual<StackId>>,
    computations: Vec<Residual<CompuId>>,
    arms: Vec<(Matcher<VPatId, CompuId>, Demands, Demand)>,
}

impl<'a, D: Driver> NormalizationFolder<'a, D> {
    pub(super) fn new(norm: &'a mut Normalization) -> Self {
        Self {
            norm,
            driver: PhantomData,
            values: Vec::new(),
            stacks: Vec::new(),
            computations: Vec::new(),
            arms: Vec::new(),
        }
    }

    pub(super) fn run(mut self, root: CompuId) -> CompuId {
        D::run(&mut self, Work::Computation(root, Scope { values: EnvId(0), stack: None }));
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

    fn visit_value(&mut self, id: ValueId, env: EnvId, demand: Demand) -> Step<Self> {
        let site = self.norm.source.admin.terms.back(&TermId::Value(id)).copied();
        if demand.is_absent() && self.norm.discardable(id) {
            self.values
                .push(Residual { node: Triv.build(self.norm, site), demands: Demands::default() });
            return Step::Return(());
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
                    return Step::TailCall(Work::Value(thunk, env, demand));
                }
                return Step::Call {
                    input: Work::Computation(body, Scope { values: env, stack: None }),
                    frame: Work::FinishValue(id, protocol),
                };
            }
            | Value::Ctor(Ctor(_, body)) => {
                let demand = if demand.is_absent() { Demand::Absent } else { Demand::Used };
                return Step::Call {
                    input: Work::Value(body, env, demand),
                    frame: Work::FinishValue(id, protocol),
                };
            }
            | Value::VCons(_) => {
                let demand =
                    if demand.is_absent() { Demand::Fields(Default::default()) } else { demand };
                return Step::TailCall(Work::ValueChildren {
                    source: id,
                    position: 0,
                    env,
                    demand,
                    protocol,
                });
            }
            | Value::AddrOffset(AddrOffset { base, displacement }) => {
                let displacement = ScopedValue { node: displacement, env };
                if self.norm.zero_displacement(displacement) {
                    return Step::TailCall(Work::Value(base, env, demand));
                }
                return Step::Call {
                    input: Work::Value(base, env, Demand::Used),
                    frame: Work::OffsetDisplacement { source: id, displacement, protocol },
                };
            }
            | Value::Primitive(Primitive { operation, operands }) => {
                return Step::TailCall(Work::PrimitiveValue {
                    operation,
                    operands: operands.map(|node| ScopedValue { node, env }),
                    site,
                });
            }
            | value => self.build_value(id, value, Demands::default(), protocol),
        }
        Step::Return(())
    }

    fn build_value(
        &mut self, source: ValueId, value: impl Into<Value>, demands: Demands, protocol: bool,
    ) {
        let site = self.norm.source.admin.terms.back(&TermId::Value(source)).copied();
        let node = value.into().build(self.norm, site);
        if let Some(role) = self.norm.source.inner.builtin_functions.get(&source) {
            self.norm.arena.inner.builtin_functions.insert_new(node, *role);
        }
        if protocol && let Some(protocol) = self.norm.source.inner.value_protocols.get(&source) {
            self.norm.arena.inner.value_protocols.insert_new(node, protocol.clone());
        }
        self.values.push(Residual { node, demands });
    }

    fn visit_stack(&mut self, stack: ScopedStack) -> Step<Self> {
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
                return Step::Call {
                    input: Work::Value(value, scope.values, Demand::Used),
                    frame: Work::StackRest { source: id, stack: ScopedStack { node: rest, scope } },
                };
            }
            | Stack::Tag(Cons(_, rest)) => {
                return Step::Call {
                    input: Work::Stack(ScopedStack { node: rest, scope }),
                    frame: Work::FinishStack(id),
                };
            }
            | Stack::Kont(Kont { binder, body }) => {
                if let ValuePattern::Var(def) = self.norm.source.inner.vpats[&binder]
                    && let Computation::Ret(SReturn { value, stack }) =
                        self.norm.source.inner.compus[&body]
                    && matches!(self.norm.source.inner.values[&value], Value::Var(returned) if returned == def)
                    && matches!(self.norm.source.inner.stacks[&stack], Stack::Var(Bullet))
                {
                    return Step::TailCall(Work::Stack(ScopedStack { node: stack, scope }));
                }
                let values = self.norm.bind(scope.values, binder, Rc::default());
                return Step::Call {
                    input: Work::Computation(body, Scope { values, ..scope }),
                    frame: Work::ContinuationBody { source: id, binder },
                };
            }
        }
        Step::Return(())
    }

    fn visit_computation(&mut self, id: CompuId, scope: Scope) -> Step<Self> {
        let site = self.norm.source.admin.terms.back(&TermId::Compu(id)).copied();
        match self.norm.source.inner.compus[&id].clone() {
            | Computation::Memory(step) => {
                let env = scope.values;
                let (next, values) = match step {
                    | MemoryStep::Load { result, next, .. } => {
                        (next, self.norm.bind(env, result, Rc::default()))
                    }
                    | MemoryStep::Store { next, .. } => (next, env),
                };
                Step::Call {
                    input: Work::Computation(next, Scope { values, ..scope }),
                    frame: Work::MemoryAddress(id, env),
                }
            }
            | Computation::Hole(SHole(stack)) => Step::Call {
                input: Work::Stack(ScopedStack { node: stack, scope }),
                frame: Work::FinishComputation(id),
            },
            | Computation::Force(SForce { thunk, stack }) => {
                let known = self.norm.known(thunk, scope.values);
                let stack = ScopedStack { node: stack, scope: scope.clone() };
                if let KnownValue::External(function) = known.as_ref() {
                    self.external_call(function.clone(), stack, site)
                } else if self.norm.movable_stack(stack.node)
                    && let KnownValue::BuiltinBody(body) = known.as_ref()
                {
                    let stack = Some(self.norm.delay_stack(stack));
                    Step::TailCall(Work::Computation(*body, Scope { values: scope.values, stack }))
                } else if self.norm.movable_stack(stack.node)
                    && let Value::Closure(Closure { body, .. }) =
                        self.norm.source.inner.values[&thunk]
                {
                    let stack = Some(self.norm.delay_stack(stack));
                    Step::TailCall(Work::Computation(body, Scope { values: scope.values, stack }))
                } else if self.norm.movable_stack(stack.node)
                    && let KnownValue::Closure { body, env } = known.as_ref()
                {
                    let stack = Some(self.norm.delay_stack(stack));
                    Step::TailCall(Work::Computation(*body, Scope { values: *env, stack }))
                } else {
                    Step::Call {
                        input: Work::Stack(stack),
                        frame: Work::ComputationValue {
                            source: id,
                            value: ScopedValue { node: thunk, env: scope.values },
                        },
                    }
                }
            }
            | Computation::Ret(SReturn { stack, value }) => {
                let stack =
                    self.norm.resolve_stack(ScopedStack { node: stack, scope: scope.clone() });
                if let Stack::Kont(Kont { binder, body }) =
                    self.norm.source.inner.stacks[&stack.node]
                {
                    Step::TailCall(Work::Binding {
                        binder,
                        bindee: ScopedValue { node: value, env: scope.values },
                        tail: body,
                        scope: stack.scope,
                        site,
                    })
                } else {
                    Step::Call {
                        input: Work::Stack(stack),
                        frame: Work::ComputationValue {
                            source: id,
                            value: ScopedValue { node: value, env: scope.values },
                        },
                    }
                }
            }
            | Computation::Fix(SFix { param, stack, body }) => Step::Call {
                input: Work::Stack(ScopedStack { node: stack, scope: scope.clone() }),
                frame: Work::FixBody { source: id, param, body, scope },
            },
            | Computation::ProductMatch(SProductMatch { scrut: bindee, binder, body: tail })
            | Computation::Join(LetJoin::Value(Let { binder, bindee, tail })) => {
                Step::TailCall(Work::Binding {
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
                    Step::TailCall(Work::Binding {
                        binder,
                        bindee: value,
                        tail,
                        scope: Scope { stack: rest, ..scope },
                        site,
                    })
                } else {
                    let values = self.norm.bind(scope.values, binder, Rc::default());
                    Step::Call {
                        input: Work::Computation(tail, Scope { values, stack: None }),
                        frame: Work::ArgumentBody { source: id, binder, stack },
                    }
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
                    Step::TailCall(Work::Computation(*tail, Scope { stack: rest, ..scope }))
                } else {
                    Step::Call {
                        input: Work::Stack(stack),
                        frame: Work::CoCaseArms { source: id, position: 0, env: scope.values },
                    }
                }
            }
            | Computation::ExternCall(ExternCall { function, stack }) => {
                self.external_call(function, ScopedStack { node: stack, scope }, site)
            }
        }
    }

    fn branch(
        &mut self, bindee: StackId, tail: CompuId, scope: Scope, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let Computation::CoprodMatch(SCoprodMatch { scrut, arms }) =
            self.norm.source.inner.compus[&tail].clone()
        else {
            unreachable!("branch-join input guards exactly a coproduct match")
        };
        let known = self.norm.shared(scrut, scope.values);
        if self.norm.movable_stack(bindee) {
            for arm in &arms {
                match self.norm.matches::<D>(arm.binder, &known) {
                    | Some(true) => {
                        let value = ScopedValue { node: scrut, env: scope.values };
                        let stack = Some(
                            self.norm
                                .delay_stack(ScopedStack { node: bindee, scope: scope.clone() }),
                        );
                        return Step::TailCall(Work::Binding {
                            binder: arm.binder,
                            bindee: value,
                            tail: arm.tail,
                            scope: Scope { stack, ..scope },
                            site,
                        });
                    }
                    | Some(false) => {}
                    | None => break,
                }
            }
        }
        Step::TailCall(Work::BranchArms {
            source: tail,
            position: 0,
            known,
            bindee: ScopedStack { node: bindee, scope },
            site,
        })
    }

    fn external_call(
        &mut self, function: ExternalFunction, stack: ScopedStack, site: Option<ss::TermId>,
    ) -> Step<Self> {
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
                    Step::Call {
                        input: Work::Computation(body, Scope { values, ..rest.scope }),
                        frame: Work::PrimitiveContinuation {
                            binder,
                            operation,
                            operands,
                            folded: folded.is_some(),
                            site,
                        },
                    }
                } else {
                    Step::Call {
                        input: Work::Stack(rest),
                        frame: Work::PrimitiveResult { operation, operands, site },
                    }
                }
            } else {
                Step::Call {
                    input: Work::Stack(stack),
                    frame: Work::PrimitiveStack { operation, site },
                }
            }
        } else {
            Step::Call { input: Work::Stack(stack), frame: Work::ExternalStack { function, site } }
        }
    }
}

impl<D: Driver> Folder for NormalizationFolder<'_, D> {
    type Input = Work;
    type Frame = Work;
    type Output = ();

    fn resume(&mut self, frame: Work, (): ()) -> Step<Self> {
        self.enter(frame)
    }

    fn enter(&mut self, work: Work) -> Step<Self> {
        match work {
            | Work::Value(id, env, demand) => return self.visit_value(id, env, demand),
            | Work::Stack(stack) => return self.visit_stack(stack),
            | Work::Computation(id, scope) => return self.visit_computation(id, scope),
            | Work::MemoryAddress(source, env) => {
                let Computation::Memory(step) = self.norm.source.inner.compus[&source].clone()
                else {
                    unreachable!()
                };
                let address = match step {
                    | MemoryStep::Load { address, .. } | MemoryStep::Store { address, .. } => {
                        address
                    }
                };
                return Step::Call {
                    input: Work::Value(address, env, Demand::Used),
                    frame: Work::MemoryValue(source, env),
                };
            }
            | Work::MemoryValue(source, env) => {
                if let Computation::Memory(MemoryStep::Store { value, .. }) =
                    self.norm.source.inner.compus[&source]
                {
                    return Step::Call {
                        input: Work::Value(value, env, Demand::Used),
                        frame: Work::FinishMemory(source),
                    };
                }
                return Step::TailCall(Work::FinishMemory(source));
            }
            | Work::FinishMemory(source) => {
                let mut next = self.computation();
                let Computation::Memory(original) = self.norm.source.inner.compus[&source].clone()
                else {
                    unreachable!()
                };
                let step = match original {
                    | MemoryStep::Load { scalar, result, .. } => {
                        let address = self.value();
                        for def in result.vars(&self.norm.source) {
                            next.demands.remove(&def);
                        }
                        next.demands = next.demands.join(address.demands);
                        let result = self.norm.pattern::<D>(result);
                        MemoryStep::Load { scalar, address: address.node, result, next: next.node }
                    }
                    | MemoryStep::Store { scalar, .. } => {
                        let value = self.value();
                        let address = self.value();
                        next.demands = next.demands.join(address.demands).join(value.demands);
                        MemoryStep::Store {
                            scalar,
                            address: address.node,
                            value: value.node,
                            next: next.node,
                        }
                    }
                };
                self.finish_computation(source, step, next.demands);
            }
            | Work::ValueChildren { source, position, env, demand, protocol } => {
                let Value::VCons(VCons { items, layout }) = &self.norm.source.inner.values[&source]
                else {
                    unreachable!("product children")
                };
                if let Some(&child) = items.get(position) {
                    let child_demand = demand.item(position, items.len(), *layout);
                    return Step::Call {
                        input: Work::Value(child, env, child_demand),
                        frame: Work::ValueChildren {
                            source,
                            position: position + 1,
                            env,
                            demand,
                            protocol,
                        },
                    };
                }
                return Step::TailCall(Work::FinishValue(source, protocol));
            }
            | Work::StackRest { source, stack } => {
                return Step::Call { input: Work::Stack(stack), frame: Work::FinishStack(source) };
            }
            | Work::ComputationValue { source, value } => {
                return Step::Call {
                    input: Work::Value(value.node, value.env, Demand::Used),
                    frame: Work::FinishComputation(source),
                };
            }
            | Work::CoCaseArms { source, position, env } => {
                let Computation::CoCase(SCoMatch { arms, .. }) =
                    &self.norm.source.inner.compus[&source]
                else {
                    unreachable!("comatch arms")
                };
                if let Some(arm) = arms.get(position) {
                    return Step::Call {
                        input: Work::Computation(arm.tail, Scope { values: env, stack: None }),
                        frame: Work::CoCaseArms { source, position: position + 1, env },
                    };
                }
                return Step::TailCall(Work::FinishComputation(source));
            }
            | Work::BindingComponents { mut remaining, site } => {
                if let Some((binder, bindee)) = remaining.next_back() {
                    return Step::Call {
                        input: Work::ResidualBinding { binder, bindee, site },
                        frame: Work::BindingComponents { remaining, site },
                    };
                }
            }
            | Work::BranchArms { source, position, known, bindee, site } => {
                let Computation::CoprodMatch(SCoprodMatch { scrut, arms }) =
                    &self.norm.source.inner.compus[&source]
                else {
                    unreachable!("coproduct arms")
                };
                if let Some(arm) = arms.get(position) {
                    return Step::Call {
                        input: Work::BranchArm {
                            binder: arm.binder,
                            tail: arm.tail,
                            env: bindee.scope.values,
                            known: known.clone(),
                        },
                        frame: Work::BranchArms {
                            source,
                            position: position + 1,
                            known,
                            bindee,
                            site,
                        },
                    };
                }
                return Step::TailCall(Work::BranchScrutinee {
                    scrut: *scrut,
                    count: arms.len(),
                    tail: source,
                    bindee,
                    site,
                });
            }
            | Work::PrimitiveSecond { operation, second, site } => {
                return Step::Call {
                    input: Work::Value(second.node, second.env, Demand::Used),
                    frame: Work::FinishPrimitiveValue { operation, site },
                };
            }
            | Work::PrimitiveResult { operation, operands, site } => {
                return Step::Call {
                    input: Work::PrimitiveValue { operation, operands, site },
                    frame: Work::PrimitiveReturn { site },
                };
            }
            | Work::OffsetDisplacement { source, displacement, protocol } => {
                return Step::Call {
                    input: Work::Value(displacement.node, displacement.env, Demand::Used),
                    frame: Work::FinishValue(source, protocol),
                };
            }
            | Work::FinishValue(id, protocol) => match self.norm.source.inner.values[&id].clone() {
                | Value::AddrOffset(_) => {
                    let displacement = self.value();
                    let base = self.value();
                    self.build_value(
                        id,
                        AddrOffset { base: base.node, displacement: displacement.node },
                        base.demands.join(displacement.demands),
                        protocol,
                    );
                }
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
                let binder = self.norm.pattern::<D>(binder);
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
                    let binder = Cons(self.norm.pattern::<D>(binder), Bullet);
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
                return Step::Call {
                    input: Work::Computation(body, Scope { values, stack: None }),
                    frame: Work::FinishFix { source, param },
                };
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
                return Step::Call {
                    input: Work::Stack(stack),
                    frame: Work::FinishComputation(source),
                };
            }
            | Work::Binding { binder, bindee, tail, scope, site } => {
                let values =
                    self.norm.extend(scope.values, self.norm.binding_facts(binder, bindee));
                return Step::Call {
                    input: Work::Computation(tail, Scope { values, ..scope }),
                    frame: Work::ResidualBinding { binder, bindee, site },
                };
            }
            | Work::ResidualBinding { binder, bindee, site } => {
                let bound = binder.vars(&self.norm.source);
                let mut tail = self.computation();
                if !bound.iter().any(|def| tail.demands.contains(def))
                    && self.norm.discardable(bindee.node)
                {
                    self.computations.push(tail);
                    return Step::Return(());
                }
                if let Some(components) = self.norm.components(binder, bindee) {
                    self.computations.push(tail);
                    // Last component consumes tail demands first; emitted bindings
                    // still execute from the first component to the last.
                    return Step::TailCall(Work::BindingComponents {
                        remaining: components.into_iter(),
                        site,
                    });
                }
                let demand = tail.demands.pattern(&self.norm.source, binder);
                for def in bound {
                    tail.demands.remove(&def);
                }
                self.computations.push(tail);
                return Step::Call {
                    input: Work::Value(bindee.node, bindee.env, demand),
                    frame: Work::FinishBinding { binder, site },
                };
            }
            | Work::FinishBinding { binder, site } => {
                let bindee = self.value();
                let binder = self.norm.pattern::<D>(binder);
                let tail = self.computation();
                let node =
                    Let { binder, bindee: bindee.node, tail: tail.node }.build(self.norm, site);
                self.computations
                    .push(Residual { node, demands: tail.demands.join(bindee.demands) });
            }
            | Work::BranchArm { binder, tail, env, known } => {
                let values = self.norm.bind(env, binder, known);
                return Step::Call {
                    input: Work::Computation(tail, Scope { values, stack: None }),
                    frame: Work::FinishBranchArm(binder),
                };
            }
            | Work::FinishBranchArm(binder) => {
                let mut tail = self.computation();
                let demand = tail.demands.pattern(&self.norm.source, binder);
                for def in binder.vars(&self.norm.source) {
                    tail.demands.remove(&def);
                }
                let binder = self.norm.pattern::<D>(binder);
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
                return Step::Call {
                    input: Work::Value(scrut, env, demand),
                    frame: Work::BranchTail { count, tail, bindee, demands, site },
                };
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
                return Step::Call {
                    input: Work::Stack(bindee),
                    frame: Work::BranchStack { tail, demands: demands.join(scrut.demands), site },
                };
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
                    let [first, second] = operands;
                    return Step::Call {
                        input: Work::Value(first.node, first.env, Demand::Used),
                        frame: Work::PrimitiveSecond { operation, second, site },
                    };
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
                    return Step::Return(());
                }
                for def in bound {
                    tail.demands.remove(&def);
                }
                self.computations.push(tail);
                return Step::Call {
                    input: Work::PrimitiveValue { operation, operands, site },
                    frame: Work::FinishBinding { binder, site },
                };
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
        Step::Return(())
    }
}
