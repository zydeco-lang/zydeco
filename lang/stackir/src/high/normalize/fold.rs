//! Reconstruction frames preserve forward facts and backward consumer demands.

use super::*;
use std::marker::PhantomData;
use zydeco_utils::fold::{Folder, Step};

/// A memory step's operand besides its address: the fresh result binder of a load or the stored
/// value of a store.
enum MemoryOperand {
    Load { result: VPatId },
    Store { value: ValueId },
}

/// The source fields a memory step keeps while its continuation, address, and value rebuild.
pub(super) struct MemoryFrame {
    source: CompuId,
    env: EnvId,
    scalar: memory::MemoryScalar,
    address: ValueId,
    operand: MemoryOperand,
}

/// The kind of an unfinished value and the source fields it keeps; its children arrive as
/// completed results.
pub(super) enum ValueParent {
    AddrOffset,
    Closure,
    Ctor(CtorIdx),
    VCons { count: usize, layout: ProductLayout },
}

/// The kind of an unfinished stack frame; an argument frame's value arrives as a completed result.
pub(super) enum StackParent {
    Arg,
    Tag(DtorIdx),
}

/// The kind of an unfinished computation and the source fields it keeps; a comatch keeps its
/// destructor tags in arm order.
pub(super) enum ComputationParent {
    Hole,
    Force,
    Ret,
    CallProc(DefId),
    LetArg(VPatId),
    CoCase(Vec<Cons<DtorIdx, StackVar>>),
}

pub(super) enum Work {
    Value(ValueId, EnvId, Demand),
    FinishValue {
        source: ValueId,
        parent: ValueParent,
        protocol: bool,
    },
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
    FinishStack {
        source: StackId,
        parent: StackParent,
    },
    StackRest {
        source: StackId,
        stack: ScopedStack,
    },
    ContinuationBody {
        source: StackId,
        binder: VPatId,
    },
    Computation(CompuId, Scope),
    MemoryAddress(MemoryFrame),
    MemoryValue(MemoryFrame),
    FinishMemory(MemoryFrame),
    FinishComputation {
        source: CompuId,
        parent: ComputationParent,
    },
    ComputationValue {
        source: CompuId,
        parent: ComputationParent,
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
    /// Rebuild a procedure's body once its tail still calls it.
    ProcedureBody {
        source: CompuId,
        procedure: DefId,
        body: CompuId,
        env: EnvId,
    },
    FinishProcedure {
        source: CompuId,
        procedure: DefId,
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
    ComparisonChildren {
        source: CompuId,
        compare: CompareBranch,
        position: usize,
        arms: Scope,
        join: Option<ScopedStack>,
        site: Option<ss::TermId>,
    },
    BranchArms {
        source: CompuId,
        position: usize,
        known: Rc<KnownValue>,
        arms: Scope,
        join: Option<ScopedStack>,
        site: Option<ss::TermId>,
    },
    BranchArm {
        binder: VPatId,
        tail: CompuId,
        scope: Scope,
        known: Rc<KnownValue>,
    },
    FinishBranchArm(VPatId),
    BranchScrutinee {
        scrut: ValueId,
        count: usize,
        tail: CompuId,
        env: EnvId,
        join: Option<ScopedStack>,
        site: Option<ss::TermId>,
    },
    BranchTail {
        count: usize,
        tail: CompuId,
        join: Option<ScopedStack>,
        demands: Demands,
        site: Option<ss::TermId>,
    },
    BranchStack {
        tail: CompuId,
        demands: Demands,
        site: Option<ss::TermId>,
    },
    StackLetBody {
        bindee: ScopedStack,
        site: Option<ss::TermId>,
    },
    FinishStackLet {
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
    stacks: Vec<StackResidual>,
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

    fn stack(&mut self) -> StackResidual {
        self.stacks.pop().expect("completed stack child")
    }

    /// The stack a consumer or a pushed frame receives, the frames to name above it, and the
    /// stack's demands. Under the full scheme a consumer receives the ambient stack, so a rebuilt
    /// frame moves onto the spine.
    fn consumed(&mut self, stack: StackResidual) -> (StackId, Vec<StackId>, Demands) {
        let StackResidual { node, mut spine, demands } = stack;
        if self.norm.scheme.names_frames()
            && !matches!(self.norm.arena.inner.stacks[&node], Stack::Var(_))
        {
            spine.push(node);
            (StackVar.build(self.norm, None), spine, demands)
        } else {
            (node, spine, demands)
        }
    }

    /// Every frame of a rebuilt stack, deepest first, for naming above a computation that keeps
    /// consuming the ambient stack.
    fn spine_of(&self, stack: StackResidual) -> (Vec<StackId>, Demands) {
        let StackResidual { node, mut spine, demands } = stack;
        if !matches!(self.norm.arena.inner.stacks[&node], Stack::Var(_)) {
            spine.push(node);
        }
        (spine, demands)
    }

    fn computation(&mut self) -> Residual<CompuId> {
        self.computations.pop().expect("completed computation child")
    }

    /// Finish a rebuilt computation at its source site, naming `spine` above it.
    fn finish_computation(
        &mut self, source: CompuId, compu: impl Into<Computation>, spine: Vec<StackId>,
        demands: Demands,
    ) {
        let site = self.norm.source.admin.term_site(source);
        let node = compu.into().build(self.norm, site);
        if let Some(protocol) = self.norm.source.inner.compu_protocols.get(&source) {
            self.norm.arena.inner.compu_protocols.insert_new(node, protocol.clone());
        }
        let node = self.norm.arena.name_frames(spine, node, site);
        self.computations.push(Residual { node, demands });
    }

    fn visit_value(&mut self, id: ValueId, env: EnvId, demand: Demand) -> Step<Self> {
        let site = self.norm.source.admin.term_site(id);
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
            | Value::Ext(ValueExt::Closure(Closure { body, .. })) => {
                if let Computation::Ext(CompuExt::Force(SForce { thunk, stack })) =
                    self.norm.source.inner.compus[&body]
                    && matches!(self.norm.source.inner.stacks[&stack], Stack::Var(StackVar))
                    && matches!(self.norm.source.inner.values[&thunk], Value::Var(_))
                {
                    return Step::TailCall(Work::Value(thunk, env, demand));
                }
                return Step::Call {
                    input: Work::Computation(body, Scope { values: env, stack: None }),
                    frame: Work::FinishValue { source: id, parent: ValueParent::Closure, protocol },
                };
            }
            | Value::Ctor(Ctor(tag, body)) => {
                let demand = if demand.is_absent() { Demand::Absent } else { Demand::Used };
                return Step::Call {
                    input: Work::Value(body, env, demand),
                    frame: Work::FinishValue {
                        source: id,
                        parent: ValueParent::Ctor(tag),
                        protocol,
                    },
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
        let site = self.norm.source.admin.term_site(source);
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
            | Stack::Var(stack_var) => {
                let site = self.norm.source.admin.term_site(id);
                self.stacks.push(StackResidual {
                    node: stack_var.build(self.norm, site),
                    spine: Vec::new(),
                    demands: Demands::default(),
                });
            }
            | Stack::Arg(Cons(value, rest)) => {
                return Step::Call {
                    input: Work::Value(value, scope.values, Demand::Used),
                    frame: Work::StackRest { source: id, stack: ScopedStack { node: rest, scope } },
                };
            }
            | Stack::Tag(Cons(tag, rest)) => {
                return Step::Call {
                    input: Work::Stack(ScopedStack { node: rest, scope }),
                    frame: Work::FinishStack { source: id, parent: StackParent::Tag(tag) },
                };
            }
            | Stack::Ext(StackExt::Kont(Kont { binder, body })) => {
                if let ValuePattern::Var(def) = self.norm.source.inner.vpats[&binder]
                    && let Computation::Ext(CompuExt::Ret(SReturn { value, stack })) =
                        self.norm.source.inner.compus[&body]
                    && matches!(self.norm.source.inner.values[&value], Value::Var(returned) if returned == def)
                    && matches!(self.norm.source.inner.stacks[&stack], Stack::Var(StackVar))
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

    // Remaining worklist: split computation normalization by form.
    #[allow(clippy::too_many_lines)]
    fn visit_computation(&mut self, id: CompuId, scope: Scope) -> Step<Self> {
        let site = self.norm.source.admin.term_site(id);
        match self.norm.source.inner.compus[&id].clone() {
            | Computation::Memory(step) => {
                let env = scope.values;
                let (next, values, frame) = match step {
                    | MemoryStep::Load { scalar, address, result, next } => {
                        let (result, values) = self.norm.load_pattern::<D>(result, env);
                        let operand = MemoryOperand::Load { result };
                        (next, values, MemoryFrame { source: id, env, scalar, address, operand })
                    }
                    | MemoryStep::Store { scalar, address, value, next } => {
                        let operand = MemoryOperand::Store { value };
                        (next, env, MemoryFrame { source: id, env, scalar, address, operand })
                    }
                };
                Step::Call {
                    input: Work::Computation(next, Scope { values, ..scope }),
                    frame: Work::MemoryAddress(frame),
                }
            }
            | Computation::Hole(SHole(stack)) => Step::Call {
                input: Work::Stack(ScopedStack { node: stack, scope }),
                frame: Work::FinishComputation { source: id, parent: ComputationParent::Hole },
            },
            | Computation::Ext(CompuExt::Force(SForce { thunk, stack })) => {
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
                    && let Value::Ext(ValueExt::Closure(Closure { body, .. })) =
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
                            parent: ComputationParent::Force,
                            value: ScopedValue { node: thunk, env: scope.values },
                        },
                    }
                }
            }
            | Computation::Ext(CompuExt::Ret(SReturn { stack, value })) => {
                let stack =
                    self.norm.resolve_stack(ScopedStack { node: stack, scope: scope.clone() });
                if let Stack::Ext(StackExt::Kont(Kont { binder, body })) =
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
                            parent: ComputationParent::Ret,
                            value: ScopedValue { node: value, env: scope.values },
                        },
                    }
                }
            }
            | Computation::Ext(CompuExt::Fix(SFix { param, stack, body })) => Step::Call {
                input: Work::Stack(ScopedStack { node: stack, scope: scope.clone() }),
                frame: Work::FixBody { source: id, param, body, scope },
            },
            | Computation::Ext(CompuExt::LetProc(LetProc { procedure, body, tail, .. })) => {
                let env = scope.values;
                let fact = self.norm.procedure_fact(procedure, body, env);
                let values = self.norm.extend(env, [(procedure, fact)]);
                Step::Call {
                    input: Work::Computation(tail, Scope { values, ..scope }),
                    frame: Work::ProcedureBody { source: id, procedure, body, env },
                }
            }
            | Computation::Ext(CompuExt::CallProc(CallProc { procedure, stack })) => {
                let known = self.norm.lookup(scope.values, procedure);
                let stack = ScopedStack { node: stack, scope };
                if self.norm.movable_stack(stack.node)
                    && let KnownValue::Procedure { body, env } = known.as_ref()
                {
                    let stack = Some(self.norm.delay_stack(stack));
                    Step::TailCall(Work::Computation(*body, Scope { values: *env, stack }))
                } else {
                    Step::Call {
                        input: Work::Stack(stack),
                        frame: Work::FinishComputation {
                            source: id,
                            parent: ComputationParent::CallProc(procedure),
                        },
                    }
                }
            }
            | Computation::ProductMatch(SProductMatch { scrut: bindee, binder, body: tail })
            | Computation::LetValue(Let { binder, bindee, tail }) => {
                Step::TailCall(Work::Binding {
                    binder,
                    bindee: ScopedValue { node: bindee, env: scope.values },
                    tail,
                    scope,
                    site,
                })
            }
            | Computation::LetStack(Let { bindee, tail, .. }) => {
                if self.norm.scheme.names_frames() {
                    self.stack_let(bindee, tail, scope, site)
                } else {
                    self.branch(tail, scope, Some(bindee), site)
                }
            }
            | Computation::Compare(_) | Computation::CoprodMatch(_) => {
                self.branch(id, scope, None, site)
            }
            | Computation::LetArg(Let { binder: Cons(binder, StackVar), bindee, tail }) => {
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

    /// Normalize a value case split or scalar comparison. A branch under a stack let shares the
    /// stack the let names; the arms of a bare branch each consume the current delayed stack,
    /// rebuilding it on their own.
    fn branch(
        &mut self, branch: CompuId, scope: Scope, guard: Option<StackId>, site: Option<ss::TermId>,
    ) -> Step<Self> {
        // Selecting a successor moves a guard's stack into it, so the guard must be movable.
        let selectable = guard.is_none_or(|bindee| self.norm.movable_stack(bindee));
        if let Computation::Compare(compare) = &self.norm.source.inner.compus[&branch] {
            let compare = compare.clone();
            let known = compare.operands.map(|value| self.norm.known(value, scope.values));
            if selectable
                && compare.operands.iter().all(|value| self.norm.discardable(*value))
                && let [KnownValue::Literal(first), KnownValue::Literal(second)] =
                    known.each_ref().map(|value| value.as_ref())
                && let Ok(condition) = compare.operation.evaluate(&[first.clone(), second.clone()])
            {
                let scope = self.selected(guard, scope);
                return Step::TailCall(Work::Computation(
                    if condition { compare.when_true } else { compare.when_false },
                    scope,
                ));
            }
            let (arms, join) = self.unknown(guard, scope);
            return Step::TailCall(Work::ComparisonChildren {
                source: branch,
                compare,
                position: 0,
                arms,
                join,
                site,
            });
        }
        let Computation::CoprodMatch(SCoprodMatch { scrut, arms }) =
            self.norm.source.inner.compus[&branch].clone()
        else {
            unreachable!("a stack let guards a branch")
        };
        let known = self.norm.shared(scrut, scope.values);
        if selectable {
            for arm in &arms {
                match self.norm.matches::<D>(arm.binder, &known) {
                    | Some(true) => {
                        let value = ScopedValue { node: scrut, env: scope.values };
                        // A known tag does not expose a payload stored in a variable.
                        // Keep its match and complete tag table instead of creating
                        // a constructor pattern in an ordinary value binding.
                        if matches!(
                            self.norm.source.inner.vpats[&arm.binder],
                            ValuePattern::Ctor(_)
                        ) && self.norm.components(arm.binder, value).is_none()
                        {
                            break;
                        }
                        let scope = self.selected(guard, scope);
                        return Step::TailCall(Work::Binding {
                            binder: arm.binder,
                            bindee: value,
                            tail: arm.tail,
                            scope,
                            site,
                        });
                    }
                    | Some(false) => {}
                    | None => break,
                }
            }
        }
        let (arms, join) = self.unknown(guard, scope);
        Step::TailCall(Work::BranchArms { source: branch, position: 0, known, arms, join, site })
    }

    /// Normalize a full-scheme stack let-binding, which names one frame. A movable frame is
    /// delayed, so reductions see through the let; a frame whose argument may trap keeps its let,
    /// and the frames delayed beneath it are named above it, so the trap stays where it was pushed.
    fn stack_let(
        &mut self, bindee: StackId, tail: CompuId, scope: Scope, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let bindee = ScopedStack { node: bindee, scope: scope.clone() };
        if self.norm.movable_stack(bindee.node) {
            let stack = Some(self.norm.delay_stack(bindee));
            return Step::TailCall(Work::Computation(tail, Scope { stack, ..scope }));
        }
        Step::Call {
            input: Work::Computation(tail, Scope { values: scope.values, stack: None }),
            frame: Work::StackLetBody { bindee, site },
        }
    }

    /// The scope a selected successor or arm continues in: a guard's stack is delayed into it,
    /// and a bare branch keeps the current delayed stack.
    fn selected(&mut self, guard: Option<StackId>, scope: Scope) -> Scope {
        match guard {
            | Some(bindee) => {
                let bindee = ScopedStack { node: bindee, scope: scope.clone() };
                Scope { stack: Some(self.norm.delay_stack(bindee)), ..scope }
            }
            | None => scope,
        }
    }

    /// Finish a rebuilt branch. A guarded branch rebuilds its guard's stack and names it again;
    /// a bare branch is complete.
    fn join_branch(
        &mut self, branch: CompuId, demands: Demands, join: Option<ScopedStack>,
        site: Option<ss::TermId>,
    ) -> Step<Self> {
        match join {
            | Some(bindee) => Step::Call {
                input: Work::Stack(bindee),
                frame: Work::BranchStack { tail: branch, demands, site },
            },
            | None => {
                self.computations.push(Residual { node: branch, demands });
                Step::Return(())
            }
        }
    }

    /// The scope unknown arms run in, and the stack a rebuilt branch names above it. The arms of
    /// a guarded branch consume the guard's `•`. The full scheme names a bare branch's delayed
    /// consumer above it, while the duplicating scheme lets every arm rebuild that consumer.
    fn unknown(&self, guard: Option<StackId>, scope: Scope) -> (Scope, Option<ScopedStack>) {
        match guard {
            | Some(bindee) => (
                Scope { values: scope.values, stack: None },
                Some(ScopedStack { node: bindee, scope }),
            ),
            | None if self.norm.scheme.names_frames() => {
                let join = scope
                    .stack
                    .map(|id| self.norm.delayed_stacks[id.0].clone())
                    .filter(|stack| !self.norm.ambient(stack));
                (Scope { values: scope.values, stack: None }, join)
            }
            | None => (scope, None),
        }
    }

    fn external_call(
        &mut self, function: ExternalFunction, stack: ScopedStack, site: Option<ss::TermId>,
    ) -> Step<Self> {
        if let ExternalFunction::Host(role) = &function
            && let Some(operation) = PrimitiveOp::from_builtin(*role)
        {
            if let Some((operands, rest)) = self.norm.primitive_arguments(stack.clone()) {
                if let Stack::Ext(StackExt::Kont(Kont { binder, body })) =
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

    // Values

    fn resume_value_children(
        &mut self, source: ValueId, position: usize, env: EnvId, demand: Demand, protocol: bool,
    ) -> Step<Self> {
        let Value::VCons(VCons { items, layout }) = &self.norm.source.inner.values[&source] else {
            unreachable!("a product frame is pushed only for a product value")
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
        let parent = ValueParent::VCons { count: items.len(), layout: *layout };
        Step::TailCall(Work::FinishValue { source, parent, protocol })
    }

    fn resume_offset_displacement(
        &mut self, source: ValueId, displacement: ScopedValue, protocol: bool,
    ) -> Step<Self> {
        Step::Call {
            input: Work::Value(displacement.node, displacement.env, Demand::Used),
            frame: Work::FinishValue { source, parent: ValueParent::AddrOffset, protocol },
        }
    }

    fn resume_finish_value(
        &mut self, source: ValueId, parent: ValueParent, protocol: bool,
    ) -> Step<Self> {
        match parent {
            | ValueParent::AddrOffset => {
                let displacement = self.value();
                let base = self.value();
                self.build_value(
                    source,
                    AddrOffset { base: base.node, displacement: displacement.node },
                    base.demands.join(displacement.demands),
                    protocol,
                );
            }
            | ValueParent::Closure => {
                let body = self.computation();
                self.build_value(
                    source,
                    Closure { stack: StackVar, body: body.node },
                    body.demands,
                    protocol,
                );
            }
            | ValueParent::Ctor(tag) => {
                let body = self.value();
                self.build_value(source, Ctor(tag, body.node), body.demands, protocol);
            }
            | ValueParent::VCons { count, layout } => {
                let children = self.values.split_off(self.values.len() - count);
                let (items, demands): (Vec<_>, Vec<_>) =
                    children.into_iter().map(|child| (child.node, child.demands)).unzip();
                self.build_value(
                    source,
                    VCons::new(items, layout),
                    demands.into_iter().fold(Demands::default(), Demands::join),
                    protocol,
                );
            }
        }
        Step::Return(())
    }

    // Stacks

    fn resume_stack_rest(&mut self, source: StackId, stack: ScopedStack) -> Step<Self> {
        Step::Call {
            input: Work::Stack(stack),
            frame: Work::FinishStack { source, parent: StackParent::Arg },
        }
    }

    fn resume_finish_stack(&mut self, source: StackId, parent: StackParent) -> Step<Self> {
        let site = self.norm.source.admin.term_site(source);
        let tail = self.stack();
        let (rest, spine, rest_demands) = self.consumed(tail);
        let (stack, demands): (Stack, _) = match parent {
            | StackParent::Arg => {
                let value = self.value();
                (Cons(value.node, rest).into(), value.demands.join(rest_demands))
            }
            | StackParent::Tag(tag) => (Cons(tag, rest).into(), rest_demands),
        };
        let node = stack.build(self.norm, site);
        self.stacks.push(StackResidual { node, spine, demands });
        Step::Return(())
    }

    fn resume_continuation_body(&mut self, source: StackId, binder: VPatId) -> Step<Self> {
        let mut body = self.computation();
        for def in binder.vars(&self.norm.source) {
            body.demands.remove(&def);
        }
        let binder = self.norm.pattern::<D>(binder);
        let site = self.norm.source.admin.term_site(source);
        let node = Kont { binder, body: body.node }.build(self.norm, site);
        self.stacks.push(StackResidual { node, spine: Vec::new(), demands: body.demands });
        Step::Return(())
    }

    // Computations

    fn resume_memory_address(&mut self, frame: MemoryFrame) -> Step<Self> {
        Step::Call {
            input: Work::Value(frame.address, frame.env, Demand::Used),
            frame: Work::MemoryValue(frame),
        }
    }

    fn resume_memory_value(&mut self, frame: MemoryFrame) -> Step<Self> {
        if let MemoryOperand::Store { value } = frame.operand {
            return Step::Call {
                input: Work::Value(value, frame.env, Demand::Used),
                frame: Work::FinishMemory(frame),
            };
        }
        Step::TailCall(Work::FinishMemory(frame))
    }

    fn resume_finish_memory(&mut self, frame: MemoryFrame) -> Step<Self> {
        let MemoryFrame { source, scalar, operand, .. } = frame;
        let mut next = self.computation();
        let step = match operand {
            | MemoryOperand::Load { result } => {
                let address = self.value();
                for def in result.vars(&self.norm.arena) {
                    next.demands.remove(&def);
                }
                next.demands = next.demands.join(address.demands);
                MemoryStep::Load { scalar, address: address.node, result, next: next.node }
            }
            | MemoryOperand::Store { .. } => {
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
        self.finish_computation(source, step, Vec::new(), next.demands);
        Step::Return(())
    }

    fn resume_computation_value(
        &mut self, source: CompuId, parent: ComputationParent, value: ScopedValue,
    ) -> Step<Self> {
        Step::Call {
            input: Work::Value(value.node, value.env, Demand::Used),
            frame: Work::FinishComputation { source, parent },
        }
    }

    fn resume_co_case_arms(&mut self, source: CompuId, position: usize, env: EnvId) -> Step<Self> {
        let Computation::CoCase(SCoMatch { arms, .. }) = &self.norm.source.inner.compus[&source]
        else {
            unreachable!("a comatch frame is pushed only for a comatch")
        };
        if let Some(arm) = arms.get(position) {
            return Step::Call {
                input: Work::Computation(arm.tail, Scope { values: env, stack: None }),
                frame: Work::CoCaseArms { source, position: position + 1, env },
            };
        }
        let dtors = arms.iter().map(|arm| arm.dtor.clone()).collect();
        let parent = ComputationParent::CoCase(dtors);
        Step::TailCall(Work::FinishComputation { source, parent })
    }

    fn resume_finish_computation(
        &mut self, source: CompuId, parent: ComputationParent,
    ) -> Step<Self> {
        match parent {
            | ComputationParent::Hole => {
                let stack = self.stack();
                let (stack, spine, demands) = self.consumed(stack);
                self.finish_computation(source, SHole(stack), spine, demands);
            }
            | ComputationParent::Force => {
                let thunk = self.value();
                let stack = self.stack();
                let (stack, spine, demands) = self.consumed(stack);
                self.finish_computation(
                    source,
                    SForce { thunk: thunk.node, stack },
                    spine,
                    thunk.demands.join(demands),
                );
            }
            | ComputationParent::CallProc(procedure) => {
                let stack = self.stack();
                let (stack, spine, demands) = self.consumed(stack);
                // A surviving call keeps its procedure's binding alive.
                let demands = demands.join(Demands::singleton(procedure, Demand::Used));
                self.finish_computation(source, CallProc { procedure, stack }, spine, demands);
            }
            | ComputationParent::Ret => {
                let value = self.value();
                let stack = self.stack();
                let (stack, spine, demands) = self.consumed(stack);
                self.finish_computation(
                    source,
                    SReturn { stack, value: value.node },
                    spine,
                    demands.join(value.demands),
                );
            }
            | ComputationParent::LetArg(binder) => {
                let tail = self.computation();
                let bindee = self.stack();
                let (bindee, spine, demands) = self.consumed(bindee);
                let binder = Cons(self.norm.pattern::<D>(binder), StackVar);
                self.finish_computation(
                    source,
                    Let { binder, bindee, tail: tail.node },
                    spine,
                    demands.join(tail.demands),
                );
            }
            | ComputationParent::CoCase(dtors) => {
                let tails = self.computations.split_off(self.computations.len() - dtors.len());
                let (arms, demands): (Vec<_>, Vec<_>) = dtors
                    .into_iter()
                    .zip(tails)
                    .map(|(dtor, tail)| (CoMatcher { dtor, tail: tail.node }, tail.demands))
                    .unzip();
                let scrut = self.stack();
                let (scrut, spine, scrut_demands) = self.consumed(scrut);
                self.finish_computation(
                    source,
                    SCoMatch { scrut, arms },
                    spine,
                    demands.into_iter().fold(scrut_demands, Demands::join),
                );
            }
        }
        Step::Return(())
    }

    fn resume_fix_body(
        &mut self, source: CompuId, param: DefId, body: CompuId, scope: Scope,
    ) -> Step<Self> {
        let values = self.norm.extend(scope.values, [(param, Rc::default())]);
        Step::Call {
            input: Work::Computation(body, Scope { values, stack: None }),
            frame: Work::FinishFix { source, param },
        }
    }

    fn resume_finish_fix(&mut self, source: CompuId, param: DefId) -> Step<Self> {
        let mut body = self.computation();
        body.demands.remove(&param);
        let stack = self.stack();
        let (stack, spine, demands) = self.consumed(stack);
        self.finish_computation(
            source,
            SFix { param, stack, body: body.node },
            spine,
            demands.join(body.demands),
        );
        Step::Return(())
    }

    fn resume_procedure_body(
        &mut self, source: CompuId, procedure: DefId, body: CompuId, env: EnvId,
    ) -> Step<Self> {
        // The tail is complete; a procedure that no rebuilt call reaches is dead.
        let called = self.computations.last().is_some_and(|tail| tail.demands.contains(&procedure));
        if !called {
            return Step::Return(());
        }
        Step::Call {
            input: Work::Computation(body, Scope { values: env, stack: None }),
            frame: Work::FinishProcedure { source, procedure },
        }
    }

    fn resume_finish_procedure(&mut self, source: CompuId, procedure: DefId) -> Step<Self> {
        let body = self.computation();
        let mut tail = self.computation();
        tail.demands.remove(&procedure);
        self.finish_computation(
            source,
            LetProc { procedure, stack: StackVar, body: body.node, tail: tail.node },
            Vec::new(),
            tail.demands.join(body.demands),
        );
        Step::Return(())
    }

    fn resume_argument_body(
        &mut self, source: CompuId, binder: VPatId, stack: ScopedStack,
    ) -> Step<Self> {
        let tail = self.computations.last_mut().expect("argument consumer body");
        for def in binder.vars(&self.norm.source) {
            tail.demands.remove(&def);
        }
        Step::Call {
            input: Work::Stack(stack),
            frame: Work::FinishComputation { source, parent: ComputationParent::LetArg(binder) },
        }
    }

    fn resume_binding(
        &mut self, binder: VPatId, bindee: ScopedValue, tail: CompuId, scope: Scope,
        site: Option<ss::TermId>,
    ) -> Step<Self> {
        let values = self.norm.extend(scope.values, self.norm.binding_facts(binder, bindee));
        Step::Call {
            input: Work::Computation(tail, Scope { values, ..scope }),
            frame: Work::ResidualBinding { binder, bindee, site },
        }
    }

    fn resume_residual_binding(
        &mut self, binder: VPatId, bindee: ScopedValue, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let bound = binder.vars(&self.norm.source);
        let mut tail = self.computation();
        if !bound.iter().any(|def| tail.demands.contains(def)) && self.norm.discardable(bindee.node)
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
        Step::Call {
            input: Work::Value(bindee.node, bindee.env, demand),
            frame: Work::FinishBinding { binder, site },
        }
    }

    fn resume_binding_components(
        &mut self, mut remaining: std::vec::IntoIter<(VPatId, ScopedValue)>,
        site: Option<ss::TermId>,
    ) -> Step<Self> {
        if let Some((binder, bindee)) = remaining.next_back() {
            return Step::Call {
                input: Work::ResidualBinding { binder, bindee, site },
                frame: Work::BindingComponents { remaining, site },
            };
        }
        Step::Return(())
    }

    fn resume_finish_binding(&mut self, binder: VPatId, site: Option<ss::TermId>) -> Step<Self> {
        let bindee = self.value();
        let binder = self.norm.pattern::<D>(binder);
        let tail = self.computation();
        let node = Let { binder, bindee: bindee.node, tail: tail.node }.build(self.norm, site);
        self.computations.push(Residual { node, demands: tail.demands.join(bindee.demands) });
        Step::Return(())
    }

    fn resume_stack_let_body(
        &mut self, bindee: ScopedStack, site: Option<ss::TermId>,
    ) -> Step<Self> {
        Step::Call { input: Work::Stack(bindee), frame: Work::FinishStackLet { site } }
    }

    fn resume_finish_stack_let(&mut self, site: Option<ss::TermId>) -> Step<Self> {
        let bindee = self.stack();
        let tail = self.computation();
        let (spine, bindee_demands) = self.spine_of(bindee);
        let node = self.norm.arena.name_frames(spine, tail.node, site);
        self.computations.push(Residual { node, demands: tail.demands.join(bindee_demands) });
        Step::Return(())
    }

    // Branches

    fn resume_comparison_children(
        &mut self, source: CompuId, compare: CompareBranch, position: usize, arms: Scope,
        join: Option<ScopedStack>, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let input = match position {
            | 0 => Some(Work::Computation(compare.when_true, arms.clone())),
            | 1 => Some(Work::Computation(compare.when_false, arms.clone())),
            | 2 | 3 => Some(Work::Value(compare.operands[position - 2], arms.values, Demand::Used)),
            | _ => None,
        };
        if let Some(input) = input {
            return Step::Call {
                input,
                frame: Work::ComparisonChildren {
                    source,
                    compare,
                    position: position + 1,
                    arms,
                    join,
                    site,
                },
            };
        }
        let second = self.value();
        let first = self.value();
        let when_false = self.computation();
        let when_true = self.computation();
        let branch_site = self.norm.source.admin.term_site(source);
        let tail = CompareBranch {
            operation: compare.operation,
            operands: [first.node, second.node],
            when_true: when_true.node,
            when_false: when_false.node,
        }
        .build(self.norm, branch_site);
        let demands =
            first.demands.join(second.demands).join(when_true.demands).join(when_false.demands);
        self.join_branch(tail, demands, join, site)
    }

    fn resume_branch_arms(
        &mut self, source: CompuId, position: usize, known: Rc<KnownValue>, scope: Scope,
        join: Option<ScopedStack>, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let Computation::CoprodMatch(SCoprodMatch { scrut, arms }) =
            &self.norm.source.inner.compus[&source]
        else {
            unreachable!("a coproduct-arm frame is pushed only for a coproduct match")
        };
        if let Some(arm) = arms.get(position) {
            return Step::Call {
                input: Work::BranchArm {
                    binder: arm.binder,
                    tail: arm.tail,
                    scope: scope.clone(),
                    known: known.clone(),
                },
                frame: Work::BranchArms {
                    source,
                    position: position + 1,
                    known,
                    arms: scope,
                    join,
                    site,
                },
            };
        }
        Step::TailCall(Work::BranchScrutinee {
            scrut: *scrut,
            count: arms.len(),
            tail: source,
            env: scope.values,
            join,
            site,
        })
    }

    fn resume_branch_arm(
        &mut self, binder: VPatId, tail: CompuId, scope: Scope, known: Rc<KnownValue>,
    ) -> Step<Self> {
        let values = self.norm.bind(scope.values, binder, known);
        Step::Call {
            input: Work::Computation(tail, Scope { values, ..scope }),
            frame: Work::FinishBranchArm(binder),
        }
    }

    fn resume_finish_branch_arm(&mut self, binder: VPatId) -> Step<Self> {
        let mut tail = self.computation();
        let demand = tail.demands.pattern(&self.norm.source, binder);
        for def in binder.vars(&self.norm.source) {
            tail.demands.remove(&def);
        }
        let binder = self.norm.pattern::<D>(binder);
        self.arms.push((Matcher { binder, tail: tail.node }, tail.demands, demand));
        Step::Return(())
    }

    fn resume_branch_scrutinee(
        &mut self, scrut: ValueId, count: usize, tail: CompuId, env: EnvId,
        join: Option<ScopedStack>, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let first = self.arms.len() - count;
        let (demands, demand) = self.arms[first..].iter_mut().fold(
            (Demands::default(), Demand::Absent),
            |(demands, demand), (_, more, scrut)| {
                (demands.join(std::mem::take(more)), demand.join(std::mem::take(scrut)))
            },
        );
        let demand = if demand.is_absent() { Demand::Used } else { demand };
        Step::Call {
            input: Work::Value(scrut, env, demand),
            frame: Work::BranchTail { count, tail, join, demands, site },
        }
    }

    fn resume_branch_tail(
        &mut self, count: usize, tail: CompuId, join: Option<ScopedStack>, demands: Demands,
        site: Option<ss::TermId>,
    ) -> Step<Self> {
        let scrut = self.value();
        let arms = self
            .arms
            .split_off(self.arms.len() - count)
            .into_iter()
            .map(|(arm, _, _)| arm)
            .collect();
        let branch_site = self.norm.source.admin.term_site(tail);
        let tail = SCoprodMatch { scrut: scrut.node, arms }.build(self.norm, branch_site);
        self.join_branch(tail, demands.join(scrut.demands), join, site)
    }

    fn resume_branch_stack(
        &mut self, tail: CompuId, demands: Demands, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let stack = self.stack();
        // Under the full scheme the delayed consumer of a bare branch is named frame by
        // frame above it; a branch-join guard names the whole stack its arms share.
        let (node, stack_demands) = if self.norm.scheme.names_frames() {
            let (spine, stack_demands) = self.spine_of(stack);
            (self.norm.arena.name_frames(spine, tail, site), stack_demands)
        } else {
            let node = Let { binder: StackVar, bindee: stack.node, tail }.build(self.norm, site);
            (node, stack.demands)
        };
        self.computations.push(Residual { node, demands: demands.join(stack_demands) });
        Step::Return(())
    }

    // Primitives

    fn resume_primitive_value(
        &mut self, operation: PrimitiveOp, operands: [ScopedValue; 2], site: Option<ss::TermId>,
    ) -> Step<Self> {
        if let Some(literal) = self.norm.fold_primitive(operation, operands) {
            self.values.push(Residual {
                node: literal.build(self.norm, site),
                demands: Demands::default(),
            });
            return Step::Return(());
        }
        let [first, second] = operands;
        Step::Call {
            input: Work::Value(first.node, first.env, Demand::Used),
            frame: Work::PrimitiveSecond { operation, second, site },
        }
    }

    fn resume_primitive_second(
        &mut self, operation: PrimitiveOp, second: ScopedValue, site: Option<ss::TermId>,
    ) -> Step<Self> {
        Step::Call {
            input: Work::Value(second.node, second.env, Demand::Used),
            frame: Work::FinishPrimitiveValue { operation, site },
        }
    }

    fn resume_finish_primitive_value(
        &mut self, operation: PrimitiveOp, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let second = self.value();
        let first = self.value();
        let node =
            Primitive { operation, operands: [first.node, second.node] }.build(self.norm, site);
        self.values.push(Residual { node, demands: first.demands.join(second.demands) });
        Step::Return(())
    }

    fn resume_primitive_continuation(
        &mut self, binder: VPatId, operation: PrimitiveOp, operands: [ScopedValue; 2],
        folded: bool, site: Option<ss::TermId>,
    ) -> Step<Self> {
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
        Step::Call {
            input: Work::PrimitiveValue { operation, operands, site },
            frame: Work::FinishBinding { binder, site },
        }
    }

    fn resume_primitive_result(
        &mut self, operation: PrimitiveOp, operands: [ScopedValue; 2], site: Option<ss::TermId>,
    ) -> Step<Self> {
        Step::Call {
            input: Work::PrimitiveValue { operation, operands, site },
            frame: Work::PrimitiveReturn { site },
        }
    }

    fn resume_primitive_return(&mut self, site: Option<ss::TermId>) -> Step<Self> {
        let value = self.value();
        let stack = self.stack();
        let (stack, spine, demands) = self.consumed(stack);
        let node = SReturn { stack, value: value.node }.build(self.norm, site);
        let node = self.norm.arena.name_frames(spine, node, site);
        self.computations.push(Residual { node, demands: demands.join(value.demands) });
        Step::Return(())
    }

    fn resume_primitive_stack(
        &mut self, operation: PrimitiveOp, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let stack = self.stack();
        let (stack, spine, demands) = self.consumed(stack);
        let defs = ["__primitive_first__", "__primitive_second__"].map(|name| {
            let def = self.norm.arena.admin.fresh_def();
            self.norm.arena.admin.insert_def(def, VarName(name.into()));
            def
        });
        let operands = defs.map(|def| def.build(self.norm, site));
        let value = Primitive { operation, operands }.build(self.norm, site);
        let ambient = StackVar.build(self.norm, site);
        let tail = SReturn { stack: ambient, value }.build(self.norm, site);
        let second = defs[1].build(self.norm, None);
        let ambient = StackVar.build(self.norm, site);
        let tail =
            Let { binder: Cons(second, StackVar), bindee: ambient, tail }.build(self.norm, site);
        let first = defs[0].build(self.norm, None);
        let node =
            Let { binder: Cons(first, StackVar), bindee: stack, tail }.build(self.norm, site);
        let node = self.norm.arena.name_frames(spine, node, site);
        self.computations.push(Residual { node, demands });
        Step::Return(())
    }

    fn resume_external_stack(
        &mut self, function: ExternalFunction, site: Option<ss::TermId>,
    ) -> Step<Self> {
        let stack = self.stack();
        let (stack, spine, demands) = self.consumed(stack);
        let node = ExternCall { function, stack }.build(self.norm, site);
        let node = self.norm.arena.name_frames(spine, node, site);
        self.computations.push(Residual { node, demands });
        Step::Return(())
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
            | Work::Value(id, env, demand) => self.visit_value(id, env, demand),
            | Work::Stack(stack) => self.visit_stack(stack),
            | Work::Computation(id, scope) => self.visit_computation(id, scope),
            | Work::FinishValue { source, parent, protocol } => {
                self.resume_finish_value(source, parent, protocol)
            }
            | Work::OffsetDisplacement { source, displacement, protocol } => {
                self.resume_offset_displacement(source, displacement, protocol)
            }
            | Work::ValueChildren { source, position, env, demand, protocol } => {
                self.resume_value_children(source, position, env, demand, protocol)
            }
            | Work::FinishStack { source, parent } => self.resume_finish_stack(source, parent),
            | Work::StackRest { source, stack } => self.resume_stack_rest(source, stack),
            | Work::ContinuationBody { source, binder } => {
                self.resume_continuation_body(source, binder)
            }
            | Work::MemoryAddress(frame) => self.resume_memory_address(frame),
            | Work::MemoryValue(frame) => self.resume_memory_value(frame),
            | Work::FinishMemory(frame) => self.resume_finish_memory(frame),
            | Work::FinishComputation { source, parent } => {
                self.resume_finish_computation(source, parent)
            }
            | Work::ComputationValue { source, parent, value } => {
                self.resume_computation_value(source, parent, value)
            }
            | Work::CoCaseArms { source, position, env } => {
                self.resume_co_case_arms(source, position, env)
            }
            | Work::FixBody { source, param, body, scope } => {
                self.resume_fix_body(source, param, body, scope)
            }
            | Work::FinishFix { source, param } => self.resume_finish_fix(source, param),
            | Work::ProcedureBody { source, procedure, body, env } => {
                self.resume_procedure_body(source, procedure, body, env)
            }
            | Work::FinishProcedure { source, procedure } => {
                self.resume_finish_procedure(source, procedure)
            }
            | Work::ArgumentBody { source, binder, stack } => {
                self.resume_argument_body(source, binder, stack)
            }
            | Work::Binding { binder, bindee, tail, scope, site } => {
                self.resume_binding(binder, bindee, tail, scope, site)
            }
            | Work::BindingComponents { remaining, site } => {
                self.resume_binding_components(remaining, site)
            }
            | Work::ResidualBinding { binder, bindee, site } => {
                self.resume_residual_binding(binder, bindee, site)
            }
            | Work::FinishBinding { binder, site } => self.resume_finish_binding(binder, site),
            | Work::ComparisonChildren { source, compare, position, arms, join, site } => {
                self.resume_comparison_children(source, compare, position, arms, join, site)
            }
            | Work::BranchArms { source, position, known, arms, join, site } => {
                self.resume_branch_arms(source, position, known, arms, join, site)
            }
            | Work::BranchArm { binder, tail, scope, known } => {
                self.resume_branch_arm(binder, tail, scope, known)
            }
            | Work::FinishBranchArm(binder) => self.resume_finish_branch_arm(binder),
            | Work::BranchScrutinee { scrut, count, tail, env, join, site } => {
                self.resume_branch_scrutinee(scrut, count, tail, env, join, site)
            }
            | Work::BranchTail { count, tail, join, demands, site } => {
                self.resume_branch_tail(count, tail, join, demands, site)
            }
            | Work::BranchStack { tail, demands, site } => {
                self.resume_branch_stack(tail, demands, site)
            }
            | Work::StackLetBody { bindee, site } => self.resume_stack_let_body(bindee, site),
            | Work::FinishStackLet { site } => self.resume_finish_stack_let(site),
            | Work::PrimitiveValue { operation, operands, site } => {
                self.resume_primitive_value(operation, operands, site)
            }
            | Work::PrimitiveSecond { operation, second, site } => {
                self.resume_primitive_second(operation, second, site)
            }
            | Work::FinishPrimitiveValue { operation, site } => {
                self.resume_finish_primitive_value(operation, site)
            }
            | Work::PrimitiveContinuation { binder, operation, operands, folded, site } => {
                self.resume_primitive_continuation(binder, operation, operands, folded, site)
            }
            | Work::PrimitiveResult { operation, operands, site } => {
                self.resume_primitive_result(operation, operands, site)
            }
            | Work::PrimitiveReturn { site } => self.resume_primitive_return(site),
            | Work::PrimitiveStack { operation, site } => {
                self.resume_primitive_stack(operation, site)
            }
            | Work::ExternalStack { function, site } => self.resume_external_stack(function, site),
        }
    }
}
