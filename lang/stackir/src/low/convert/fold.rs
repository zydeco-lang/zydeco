//! Resumable closure conversion with occurrence-local capture and renaming state.

use super::*;
use std::marker::PhantomData;
use zydeco_utils::fold::{Folder, Step};

pub(super) struct Captures {
    env: RenameEnvId,
    sources: Vec<high::DefId>,
    bindings: Vec<(high::DefId, high::DefId)>,
}

/// Each frame carries the source data its resumption needs, so completing a child never
/// re-establishes the source node's variant. Frames that walk a source vector carry its id
/// and the next position instead of the vector.
pub(super) enum Work {
    Value(high::ValueId, RenameEnvId),
    /// Convert the items of a product value in order.
    ProductItems {
        source: high::ValueId,
        env: RenameEnvId,
        position: usize,
        layout: high::ProductLayout,
    },
    FinishProduct {
        source: high::ValueId,
        count: usize,
        layout: high::ProductLayout,
    },
    /// Convert the second operand of a primitive once the first is complete.
    PrimitiveOperand {
        source: high::ValueId,
        env: RenameEnvId,
        operation: high::PrimitiveOp,
        operand: high::ValueId,
    },
    FinishPrimitive {
        source: high::ValueId,
        operation: high::PrimitiveOp,
    },
    /// Convert the displacement of an address offset once its base is complete.
    OffsetDisplacement {
        source: high::ValueId,
        env: RenameEnvId,
        displacement: high::ValueId,
    },
    FinishAddrOffset(high::ValueId),
    FinishCtor {
        source: high::ValueId,
        tag: high::CtorIdx,
    },
    Closure {
        source: high::ValueId,
        captures: Captures,
    },
    Stack(high::StackId, RenameEnvId),
    StackRest {
        source: high::StackId,
        rest: high::StackId,
        env: RenameEnvId,
    },
    FinishArg(high::StackId),
    FinishTag {
        source: high::StackId,
        tag: high::DtorIdx,
    },
    Continuation {
        source: high::StackId,
        binder: low::VPatId,
        /// The source binder, whose protocol the continuation entry inherits.
        result: high::VPatId,
        captures: Captures,
    },
    Computation(high::CompuId, RenameEnvId),
    /// Convert both operands and both branches of a comparison in order.
    CompareChildren {
        source: high::CompuId,
        env: RenameEnvId,
        position: usize,
        branch: high::CompareBranch,
    },
    StoreAddress {
        source: high::CompuId,
        env: RenameEnvId,
        scalar: high::memory::MemoryScalar,
        address: high::ValueId,
        next: high::CompuId,
    },
    StoreNext {
        source: high::CompuId,
        env: RenameEnvId,
        scalar: high::memory::MemoryScalar,
        next: high::CompuId,
    },
    FinishStore {
        source: high::CompuId,
        scalar: high::memory::MemoryScalar,
    },
    LoadNext {
        source: high::CompuId,
        env: RenameEnvId,
        scalar: high::memory::MemoryScalar,
        result: high::VPatId,
        next: high::CompuId,
    },
    FinishLoad {
        source: high::CompuId,
        scalar: high::memory::MemoryScalar,
        binder: low::VPatId,
    },
    FinishHole(high::CompuId),
    FinishExternCall {
        source: high::CompuId,
        function: high::ExternalFunction,
    },
    ForceStack {
        source: high::CompuId,
        env: RenameEnvId,
        stack: high::StackId,
    },
    FinishForce(high::CompuId),
    ReturnValue {
        source: high::CompuId,
        env: RenameEnvId,
        value: high::ValueId,
    },
    FinishReturn(high::CompuId),
    ProductMatchBody {
        source: high::CompuId,
        env: RenameEnvId,
        binder: high::VPatId,
        body: high::CompuId,
    },
    FinishProductMatch {
        source: high::CompuId,
        binder: low::VPatId,
    },
    LetValueTail {
        source: high::CompuId,
        env: RenameEnvId,
        binder: high::VPatId,
        tail: high::CompuId,
    },
    FinishLetValue {
        source: high::CompuId,
        binder: low::VPatId,
    },
    LetStackTail {
        source: high::CompuId,
        env: RenameEnvId,
        tail: high::CompuId,
    },
    FinishLetStack(high::CompuId),
    LetArgTail {
        source: high::CompuId,
        env: RenameEnvId,
        binder: high::VPatId,
        tail: high::CompuId,
    },
    FinishLetArg {
        source: high::CompuId,
        binder: low::VPatId,
    },
    MatchArms {
        source: high::CompuId,
        env: RenameEnvId,
        position: usize,
        arms: Vec<low::Matcher<low::VPatId, low::CompuId>>,
    },
    MatchArm {
        source: high::CompuId,
        env: RenameEnvId,
        position: usize,
        arms: Vec<low::Matcher<low::VPatId, low::CompuId>>,
        binder: low::VPatId,
    },
    CoCaseArms {
        source: high::CompuId,
        env: RenameEnvId,
        position: usize,
        /// The destructor of each arm scheduled so far, in arm order.
        dtors: Vec<high::Cons<high::DtorIdx, high::StackVar>>,
    },
    FinishCoCase {
        source: high::CompuId,
        dtors: Vec<high::Cons<high::DtorIdx, high::StackVar>>,
    },
    FixBody {
        source: high::CompuId,
        env: RenameEnvId,
        param: high::DefId,
        body: high::CompuId,
    },
    Fix {
        source: high::CompuId,
        captures: Captures,
        recursive: high::DefId,
        label: high::DefId,
    },
    /// Convert a procedure's tail once its label body is complete.
    ProcedureTail {
        source: high::CompuId,
        label: high::DefId,
        kind: low::LabelKind,
        /// A closed label's parameters, popped in order: each source variable and its binder.
        parameters: Vec<(high::DefId, high::DefId)>,
        tail: high::CompuId,
        env: RenameEnvId,
    },
    FinishProcedure {
        source: high::CompuId,
        label: high::DefId,
        kind: low::LabelKind,
        /// The number of variables a closed label's jumps push.
        pushed: usize,
    },
    FinishCall {
        source: high::CompuId,
        env: RenameEnvId,
        procedure: high::DefId,
    },
}

pub(super) struct ConversionFolder<'a, 'source, D> {
    conversion: &'a mut ClosureConversion<'source>,
    driver: PhantomData<D>,
    values: Vec<low::ValueId>,
    stacks: Vec<low::StackId>,
    computations: Vec<low::CompuId>,
}

impl<'a, 'source, D: Driver> ConversionFolder<'a, 'source, D> {
    pub(super) fn new(conversion: &'a mut ClosureConversion<'source>) -> Self {
        Self {
            conversion,
            driver: PhantomData,
            values: Vec::new(),
            stacks: Vec::new(),
            computations: Vec::new(),
        }
    }

    pub(super) fn run(mut self, root: high::CompuId) -> low::CompuId {
        D::run(&mut self, Work::Computation(root, RenameEnvId(0)));
        assert!(self.values.is_empty() && self.stacks.is_empty());
        let root = self.computation();
        assert!(self.computations.is_empty());
        root
    }

    fn value(&mut self) -> low::ValueId {
        self.values.pop().expect("completed value child")
    }
    fn stack(&mut self) -> low::StackId {
        self.stacks.pop().expect("completed stack child")
    }
    fn computation(&mut self) -> low::CompuId {
        self.computations.pop().expect("completed computation child")
    }

    /// The item of a product value at `position`, if any.
    fn product_item(&self, source: high::ValueId, position: usize) -> Option<high::ValueId> {
        let high::Value::VCons(high::VCons { items, .. }) =
            &self.conversion.source.inner.values[&source]
        else {
            unreachable!("a product item frame resumes a product value")
        };
        items.get(position).copied()
    }

    /// The arm of a coproduct match at `position`, if any.
    fn coprod_arm(
        &self, source: high::CompuId, position: usize,
    ) -> Option<high::Matcher<high::VPatId, high::CompuId>> {
        let high::Computation::CoprodMatch(high::SCoprodMatch { arms, .. }) =
            &self.conversion.source.inner.compus[&source]
        else {
            unreachable!("a match arm frame resumes a coproduct match")
        };
        arms.get(position).cloned()
    }

    /// The arm of a stack case at `position`, if any.
    fn cocase_arm(
        &self, source: high::CompuId, position: usize,
    ) -> Option<high::CoMatcher<high::Cons<high::DtorIdx, high::StackVar>, high::CompuId>> {
        let high::Computation::CoCase(high::SCoMatch { arms, .. }) =
            &self.conversion.source.inner.compus[&source]
        else {
            unreachable!("a case arm frame resumes a stack case")
        };
        arms.get(position).cloned()
    }

    fn pattern(&mut self, source: high::VPatId) -> PatternTranslation {
        D::run(&mut pattern::PatternFolder { conversion: self.conversion }, source)
    }

    fn captures(
        &mut self, body: high::CompuId, excluded: Context<high::DefId>, env: RenameEnvId,
    ) -> Captures {
        let sources = self.conversion.sorted_free_vars(body, excluded, env);
        let bindings = self.conversion.capture_bindings(&sources);
        Captures { env, sources, bindings }
    }

    fn finish_value(&mut self, source: high::ValueId, value: impl Into<low::Value>) {
        let site = self.conversion.value_site(source);
        let value = value.into().build(self.conversion, site);
        if let Some(protocol) = self.conversion.source.inner.value_protocols.get(&source) {
            self.conversion.arena.inner.value_protocols.insert_new(value, protocol.clone());
        }
        self.values.push(value);
    }

    fn finish_computation(&mut self, source: high::CompuId, compu: impl Into<low::Computation>) {
        let site = self.conversion.compu_site(source);
        self.computations.push(compu.into().build(self.conversion, site));
    }

    /// Translate a binder, then convert the body it scopes over under `frame`.
    fn binding_body(
        &mut self, binder: high::VPatId, body: high::CompuId, env: RenameEnvId,
        frame: impl FnOnce(low::VPatId) -> Work,
    ) -> Step<Self> {
        let PatternTranslation { pattern: binder, bindings } = self.pattern(binder);
        let env = self.conversion.extend_env(env, bindings);
        Step::Call { input: Work::Computation(body, env), frame: frame(binder) }
    }

    // Values

    fn visit_value(&mut self, source: high::ValueId, env: RenameEnvId) -> Step<Self> {
        match self.conversion.source.inner.values[&source].clone() {
            | high::Value::Hole(_) => self.finish_value(source, low::Hole),
            | high::Value::Var(def) => {
                let def = self.conversion.renamed_def(env, def);
                self.finish_value(source, def);
            }
            | high::Value::Triv(_) => self.finish_value(source, low::Triv),
            | high::Value::Literal(literal) => self.finish_value(source, literal),
            | high::Value::Closure(high::Closure { body, .. }) => {
                let captures = self.captures(body, Context::new(), env);
                let env = self.conversion.extend_env(env, captures.bindings.iter().copied());
                return Step::Call {
                    input: Work::Computation(body, env),
                    frame: Work::Closure { source, captures },
                };
            }
            | high::Value::Ctor(high::Ctor(tag, body)) => {
                return Step::Call {
                    input: Work::Value(body, env),
                    frame: Work::FinishCtor { source, tag },
                };
            }
            | high::Value::VCons(high::VCons { layout, .. }) => {
                return Step::TailCall(Work::ProductItems { source, env, position: 0, layout });
            }
            | high::Value::Primitive(high::Primitive { operation, operands: [first, second] }) => {
                return Step::Call {
                    input: Work::Value(first, env),
                    frame: Work::PrimitiveOperand { source, env, operation, operand: second },
                };
            }
            | high::Value::AddrOffset(high::AddrOffset { base, displacement }) => {
                return Step::Call {
                    input: Work::Value(base, env),
                    frame: Work::OffsetDisplacement { source, env, displacement },
                };
            }
        }
        Step::Return(())
    }

    fn resume_product_items(
        &mut self, source: high::ValueId, env: RenameEnvId, position: usize,
        layout: high::ProductLayout,
    ) -> Step<Self> {
        match self.product_item(source, position) {
            | Some(item) => Step::Call {
                input: Work::Value(item, env),
                frame: Work::ProductItems { source, env, position: position + 1, layout },
            },
            | None => Step::TailCall(Work::FinishProduct { source, count: position, layout }),
        }
    }

    fn resume_finish_product(
        &mut self, source: high::ValueId, count: usize, layout: high::ProductLayout,
    ) -> Step<Self> {
        let items = self.values.split_off(self.values.len() - count);
        self.finish_value(source, low::VCons::new(items, layout));
        Step::Return(())
    }

    fn resume_primitive_operand(
        &mut self, source: high::ValueId, env: RenameEnvId, operation: high::PrimitiveOp,
        operand: high::ValueId,
    ) -> Step<Self> {
        Step::Call {
            input: Work::Value(operand, env),
            frame: Work::FinishPrimitive { source, operation },
        }
    }

    fn resume_finish_primitive(
        &mut self, source: high::ValueId, operation: high::PrimitiveOp,
    ) -> Step<Self> {
        let second = self.value();
        let first = self.value();
        self.finish_value(source, low::Primitive { operation, operands: [first, second] });
        Step::Return(())
    }

    fn resume_offset_displacement(
        &mut self, source: high::ValueId, env: RenameEnvId, displacement: high::ValueId,
    ) -> Step<Self> {
        Step::Call { input: Work::Value(displacement, env), frame: Work::FinishAddrOffset(source) }
    }

    fn resume_finish_addr_offset(&mut self, source: high::ValueId) -> Step<Self> {
        let displacement = self.value();
        let base = self.value();
        self.finish_value(source, low::AddrOffset { base, displacement });
        Step::Return(())
    }

    fn resume_finish_ctor(&mut self, source: high::ValueId, tag: high::CtorIdx) -> Step<Self> {
        let body = self.value();
        self.finish_value(source, low::Ctor(tag, body));
        Step::Return(())
    }

    fn resume_closure(&mut self, source: high::ValueId, captures: Captures) -> Step<Self> {
        let body = self.computation();
        let site = self.conversion.value_site(source);
        let protocol = match self.conversion.source.inner.value_protocols.get(&source) {
            | Some(ValueProtocol::Thunk(protocol)) => *protocol.clone(),
            | _ => StackProtocol::Unknown,
        };
        let environment = self.conversion.captured_pattern(&captures.bindings);
        let label = self.conversion.alloc_label("closure");
        let entry = low::EntryParameters::Closure { environment };
        let code = low::Block { label, entry, body }.build(self.conversion, site);
        self.conversion
            .arena
            .inner
            .entry_protocols
            .insert_new(code, low::EntryProtocol::Closure(protocol));
        let environment =
            self.conversion.captured_value_outside(&captures.sources, captures.env, site);
        self.finish_value(source, low::ClosurePackage { environment, code });
        Step::Return(())
    }

    // Stacks

    fn visit_stack(&mut self, source: high::StackId, env: RenameEnvId) -> Step<Self> {
        match self.conversion.source.inner.stacks[&source].clone() {
            | high::Stack::Var(_) => {
                let site = self.conversion.stack_site(source);
                self.stacks.push(low::StackVar.build(self.conversion, site));
            }
            | high::Stack::Arg(high::Cons(value, rest)) => {
                return Step::Call {
                    input: Work::Value(value, env),
                    frame: Work::StackRest { source, rest, env },
                };
            }
            | high::Stack::Tag(high::Cons(tag, rest)) => {
                return Step::Call {
                    input: Work::Stack(rest, env),
                    frame: Work::FinishTag { source, tag },
                };
            }
            | high::Stack::Kont(high::Kont { binder: result, body }) => {
                let excluded = self
                    .conversion
                    .variables
                    .bound_variables(result)
                    .expect("validated continuation binder")
                    .clone();
                let captures = self.captures(body, excluded, env);
                let PatternTranslation { pattern: binder, bindings } = self.pattern(result);
                let env = self.conversion.extend_env(env, captures.bindings.iter().copied());
                let env = self.conversion.extend_env(env, bindings);
                return Step::Call {
                    input: Work::Computation(body, env),
                    frame: Work::Continuation { source, binder, result, captures },
                };
            }
        }
        Step::Return(())
    }

    fn resume_stack_rest(
        &mut self, source: high::StackId, rest: high::StackId, env: RenameEnvId,
    ) -> Step<Self> {
        Step::Call { input: Work::Stack(rest, env), frame: Work::FinishArg(source) }
    }

    fn resume_finish_arg(&mut self, source: high::StackId) -> Step<Self> {
        let rest = self.stack();
        let value = self.value();
        let site = self.conversion.stack_site(source);
        self.stacks.push(low::Cons(value, rest).build(self.conversion, site));
        Step::Return(())
    }

    fn resume_finish_tag(&mut self, source: high::StackId, tag: high::DtorIdx) -> Step<Self> {
        let rest = self.stack();
        let site = self.conversion.stack_site(source);
        self.stacks.push(low::Cons(tag, rest).build(self.conversion, site));
        Step::Return(())
    }

    fn resume_continuation(
        &mut self, source: high::StackId, binder: low::VPatId, result: high::VPatId,
        captures: Captures,
    ) -> Step<Self> {
        let body = self.computation();
        let site = self.conversion.stack_site(source);
        let protocol = self
            .conversion
            .source
            .inner
            .pattern_protocols
            .get(&result)
            .cloned()
            .unwrap_or_default();
        let entry = low::ContinuationEntry {
            result: binder,
            body,
            captures: captures
                .bindings
                .iter()
                .map(|&(source, binding)| low::CaptureBinding {
                    source: self.conversion.renamed_def(captures.env, source),
                    binding,
                })
                .collect(),
        };
        let environment = self.conversion.captured_pattern(&captures.bindings);
        let label = self.conversion.alloc_label("continuation");
        let parameters = low::EntryParameters::Continuation { result: binder, environment };
        let code = low::Block { label, entry: parameters, body }.build(self.conversion, site);
        self.conversion
            .arena
            .inner
            .entry_protocols
            .insert_new(code, low::EntryProtocol::Continuation(protocol));
        let environment =
            self.conversion.captured_value_outside(&captures.sources, captures.env, site);
        let ambient = low::StackVar.build(self.conversion, site);
        let residual = low::Cons(environment, ambient).build(self.conversion, site);
        let package = low::ContinuationPackage { code, residual }.build(self.conversion, site);
        self.conversion.arena.inner.continuations.insert_new(package, entry);
        self.stacks.push(package);
        Step::Return(())
    }

    // Computations

    fn visit_computation(&mut self, source: high::CompuId, env: RenameEnvId) -> Step<Self> {
        match self.conversion.source.inner.compus[&source].clone() {
            | high::Computation::Compare(branch) => {
                Step::TailCall(Work::CompareChildren { source, env, position: 0, branch })
            }
            | high::Computation::Memory(high::MemoryStep::Load {
                scalar,
                address,
                result,
                next,
            }) => Step::Call {
                input: Work::Value(address, env),
                frame: Work::LoadNext { source, env, scalar, result, next },
            },
            | high::Computation::Memory(high::MemoryStep::Store {
                scalar,
                address,
                value,
                next,
            }) => Step::Call {
                input: Work::Value(value, env),
                frame: Work::StoreAddress { source, env, scalar, address, next },
            },
            | high::Computation::Hole(high::SHole(stack)) => {
                Step::Call { input: Work::Stack(stack, env), frame: Work::FinishHole(source) }
            }
            | high::Computation::ExternCall(high::ExternCall { function, stack }) => Step::Call {
                input: Work::Stack(stack, env),
                frame: Work::FinishExternCall { source, function },
            },
            | high::Computation::Force(high::SForce { thunk, stack }) => Step::Call {
                input: Work::Value(thunk, env),
                frame: Work::ForceStack { source, env, stack },
            },
            | high::Computation::ProductMatch(high::SProductMatch { scrut, binder, body }) => {
                Step::Call {
                    input: Work::Value(scrut, env),
                    frame: Work::ProductMatchBody { source, env, binder, body },
                }
            }
            | high::Computation::CoprodMatch(high::SCoprodMatch { scrut, .. }) => Step::Call {
                input: Work::Value(scrut, env),
                frame: Work::MatchArms { source, env, position: 0, arms: Vec::new() },
            },
            | high::Computation::Join(high::LetJoin::Value(high::Let { binder, bindee, tail })) => {
                Step::Call {
                    input: Work::Value(bindee, env),
                    frame: Work::LetValueTail { source, env, binder, tail },
                }
            }
            | high::Computation::Ret(high::SReturn { stack, value }) => Step::Call {
                input: Work::Stack(stack, env),
                frame: Work::ReturnValue { source, env, value },
            },
            | high::Computation::Fix(high::SFix { param, stack, body }) => Step::Call {
                input: Work::Stack(stack, env),
                frame: Work::FixBody { source, env, param, body },
            },
            | high::Computation::Join(high::LetJoin::Stack(high::Let { bindee, tail, .. })) => {
                Step::Call {
                    input: Work::Stack(bindee, env),
                    frame: Work::LetStackTail { source, env, tail },
                }
            }
            | high::Computation::LetArg(high::Let {
                binder: high::Cons(binder, _),
                bindee,
                tail,
            }) => Step::Call {
                input: Work::Stack(bindee, env),
                frame: Work::LetArgTail { source, env, binder, tail },
            },
            | high::Computation::CoCase(high::SCoMatch { scrut, .. }) => Step::Call {
                input: Work::Stack(scrut, env),
                frame: Work::CoCaseArms { source, env, position: 0, dtors: Vec::new() },
            },
            | high::Computation::LetProc(high::LetProc { procedure, body, tail, .. }) => {
                let Some(kind) = self.conversion.procedures[&source] else {
                    // No call reaches the procedure, so its body is dead.
                    return Step::TailCall(Work::Computation(tail, env));
                };
                let label = self.conversion.alloc_like(procedure);
                let (variables, parameters) = match kind {
                    | low::LabelKind::Local => (Vec::new(), Vec::new()),
                    | low::LabelKind::Closed => {
                        let variables = self.conversion.sorted_free_vars(body, Context::new(), env);
                        let parameters = variables
                            .iter()
                            .map(|variable| (*variable, self.conversion.alloc_like(*variable)))
                            .collect::<Vec<_>>();
                        (variables, parameters)
                    }
                };
                self.conversion.labels.insert(label, variables);
                let body_env = self.conversion.extend_env(env, parameters.iter().copied());
                let tail_env = self.conversion.extend_env(env, [(procedure, label)]);
                Step::Call {
                    input: Work::Computation(body, body_env),
                    frame: Work::ProcedureTail {
                        source,
                        label,
                        kind,
                        parameters,
                        tail,
                        env: tail_env,
                    },
                }
            }
            | high::Computation::CallProc(high::CallProc { procedure, stack }) => Step::Call {
                input: Work::Stack(stack, env),
                frame: Work::FinishCall { source, env, procedure },
            },
        }
    }

    fn resume_compare_children(
        &mut self, source: high::CompuId, env: RenameEnvId, position: usize,
        branch: high::CompareBranch,
    ) -> Step<Self> {
        let input = match position {
            | 0 | 1 => Some(Work::Value(branch.operands[position], env)),
            | 2 => Some(Work::Computation(branch.when_true, env)),
            | 3 => Some(Work::Computation(branch.when_false, env)),
            | _ => None,
        };
        if let Some(input) = input {
            return Step::Call {
                input,
                frame: Work::CompareChildren { source, env, position: position + 1, branch },
            };
        }
        let when_false = self.computation();
        let when_true = self.computation();
        let second = self.value();
        let first = self.value();
        let node = low::CompareBranch {
            operation: branch.operation,
            operands: [first, second],
            when_true,
            when_false,
        };
        self.finish_computation(source, node);
        Step::Return(())
    }

    fn resume_store_address(
        &mut self, source: high::CompuId, env: RenameEnvId, scalar: high::memory::MemoryScalar,
        address: high::ValueId, next: high::CompuId,
    ) -> Step<Self> {
        Step::Call {
            input: Work::Value(address, env),
            frame: Work::StoreNext { source, env, scalar, next },
        }
    }

    fn resume_store_next(
        &mut self, source: high::CompuId, env: RenameEnvId, scalar: high::memory::MemoryScalar,
        next: high::CompuId,
    ) -> Step<Self> {
        Step::Call {
            input: Work::Computation(next, env),
            frame: Work::FinishStore { source, scalar },
        }
    }

    fn resume_finish_store(
        &mut self, source: high::CompuId, scalar: high::memory::MemoryScalar,
    ) -> Step<Self> {
        let address = self.value();
        let value = self.value();
        let next = self.computation();
        self.finish_computation(source, low::MemoryStep::Store { scalar, address, value, next });
        Step::Return(())
    }

    fn resume_load_next(
        &mut self, source: high::CompuId, env: RenameEnvId, scalar: high::memory::MemoryScalar,
        result: high::VPatId, next: high::CompuId,
    ) -> Step<Self> {
        self.binding_body(result, next, env, |binder| Work::FinishLoad { source, scalar, binder })
    }

    fn resume_finish_load(
        &mut self, source: high::CompuId, scalar: high::memory::MemoryScalar, binder: low::VPatId,
    ) -> Step<Self> {
        let next = self.computation();
        let address = self.value();
        self.finish_computation(
            source,
            low::MemoryStep::Load { scalar, address, result: binder, next },
        );
        Step::Return(())
    }

    fn resume_finish_hole(&mut self, source: high::CompuId) -> Step<Self> {
        let stack = self.stack();
        self.finish_computation(source, low::SHole(stack));
        Step::Return(())
    }

    fn resume_finish_extern_call(
        &mut self, source: high::CompuId, function: high::ExternalFunction,
    ) -> Step<Self> {
        let stack = self.stack();
        self.finish_computation(source, low::ExternCall { function, stack });
        Step::Return(())
    }

    fn resume_force_stack(
        &mut self, source: high::CompuId, env: RenameEnvId, stack: high::StackId,
    ) -> Step<Self> {
        Step::Call { input: Work::Stack(stack, env), frame: Work::FinishForce(source) }
    }

    fn resume_finish_force(&mut self, source: high::CompuId) -> Step<Self> {
        let site = self.conversion.compu_site(source);
        let package = self.value();
        let stack = self.stack();
        let environment_def = self.conversion.alloc_def(VarName("__environment__".into()));
        let code_def = self.conversion.alloc_def(VarName("__closure_code__".into()));
        let environment: low::VPatId = environment_def.build(self.conversion, None);
        let code: low::VPatId = code_def.build(self.conversion, None);
        let environment_value = environment_def.build(self.conversion, site);
        let target = code_def.build(self.conversion, site);
        let argument = low::EntryArgument::Closure { environment: environment_value };
        let body = low::Jump { target, argument, stack }.build(self.conversion, site);
        self.finish_computation(source, low::OpenClosure { package, environment, code, body });
        Step::Return(())
    }

    fn resume_return_value(
        &mut self, source: high::CompuId, env: RenameEnvId, value: high::ValueId,
    ) -> Step<Self> {
        Step::Call { input: Work::Value(value, env), frame: Work::FinishReturn(source) }
    }

    fn resume_finish_return(&mut self, source: high::CompuId) -> Step<Self> {
        let site = self.conversion.compu_site(source);
        let package = self.stack();
        let value = self.value();
        let code_def = self.conversion.alloc_def(VarName("__continuation_code__".into()));
        let code = code_def.build(self.conversion, None);
        let target = code_def.build(self.conversion, site);
        let stack = low::StackVar.build(self.conversion, site);
        let argument = low::EntryArgument::Continuation { result: value };
        let body = low::Jump { target, argument, stack }.build(self.conversion, site);
        self.finish_computation(source, low::OpenContinuation { package, code, body });
        Step::Return(())
    }

    fn resume_product_match_body(
        &mut self, source: high::CompuId, env: RenameEnvId, binder: high::VPatId,
        body: high::CompuId,
    ) -> Step<Self> {
        self.binding_body(binder, body, env, |binder| Work::FinishProductMatch { source, binder })
    }

    fn resume_finish_product_match(
        &mut self, source: high::CompuId, binder: low::VPatId,
    ) -> Step<Self> {
        let body = self.computation();
        let scrut = self.value();
        self.finish_computation(source, low::SProductMatch { scrut, binder, body });
        Step::Return(())
    }

    fn resume_let_value_tail(
        &mut self, source: high::CompuId, env: RenameEnvId, binder: high::VPatId,
        tail: high::CompuId,
    ) -> Step<Self> {
        self.binding_body(binder, tail, env, |binder| Work::FinishLetValue { source, binder })
    }

    fn resume_finish_let_value(
        &mut self, source: high::CompuId, binder: low::VPatId,
    ) -> Step<Self> {
        let tail = self.computation();
        let bindee = self.value();
        self.finish_computation(source, low::LetValue { binder, bindee, tail });
        Step::Return(())
    }

    fn resume_let_stack_tail(
        &mut self, source: high::CompuId, env: RenameEnvId, tail: high::CompuId,
    ) -> Step<Self> {
        Step::Call { input: Work::Computation(tail, env), frame: Work::FinishLetStack(source) }
    }

    fn resume_finish_let_stack(&mut self, source: high::CompuId) -> Step<Self> {
        let bindee = self.stack();
        let tail = self.computation();
        self.finish_computation(source, low::LetStack { binder: low::StackVar, bindee, tail });
        Step::Return(())
    }

    fn resume_let_arg_tail(
        &mut self, source: high::CompuId, env: RenameEnvId, binder: high::VPatId,
        tail: high::CompuId,
    ) -> Step<Self> {
        self.binding_body(binder, tail, env, |binder| Work::FinishLetArg { source, binder })
    }

    fn resume_finish_let_arg(&mut self, source: high::CompuId, binder: low::VPatId) -> Step<Self> {
        let tail = self.computation();
        let bindee = self.stack();
        self.finish_computation(
            source,
            low::LetArg { binder: low::Cons(binder, low::StackVar), bindee, tail },
        );
        Step::Return(())
    }

    fn resume_match_arms(
        &mut self, source: high::CompuId, env: RenameEnvId, position: usize,
        arms: Vec<low::Matcher<low::VPatId, low::CompuId>>,
    ) -> Step<Self> {
        if let Some(high::Matcher { binder, tail }) = self.coprod_arm(source, position) {
            let PatternTranslation { pattern: binder, bindings } = self.pattern(binder);
            let body_env = self.conversion.extend_env(env, bindings);
            return Step::Call {
                input: Work::Computation(tail, body_env),
                frame: Work::MatchArm { source, env, position, arms, binder },
            };
        }
        let scrut = self.value();
        self.finish_computation(source, low::SCoprodMatch { scrut, arms });
        Step::Return(())
    }

    fn resume_match_arm(
        &mut self, source: high::CompuId, env: RenameEnvId, position: usize,
        mut arms: Vec<low::Matcher<low::VPatId, low::CompuId>>, binder: low::VPatId,
    ) -> Step<Self> {
        arms.push(low::Matcher { binder, tail: self.computation() });
        Step::TailCall(Work::MatchArms { source, env, position: position + 1, arms })
    }

    fn resume_co_case_arms(
        &mut self, source: high::CompuId, env: RenameEnvId, position: usize,
        mut dtors: Vec<high::Cons<high::DtorIdx, high::StackVar>>,
    ) -> Step<Self> {
        match self.cocase_arm(source, position) {
            | Some(high::CoMatcher { dtor, tail }) => {
                dtors.push(dtor);
                Step::Call {
                    input: Work::Computation(tail, env),
                    frame: Work::CoCaseArms { source, env, position: position + 1, dtors },
                }
            }
            | None => Step::TailCall(Work::FinishCoCase { source, dtors }),
        }
    }

    fn resume_finish_co_case(
        &mut self, source: high::CompuId, dtors: Vec<high::Cons<high::DtorIdx, high::StackVar>>,
    ) -> Step<Self> {
        let tails = self.computations.split_off(self.computations.len() - dtors.len());
        let arms = dtors
            .into_iter()
            .zip(tails)
            .map(|(dtor, tail)| low::CoMatcher { dtor, tail })
            .collect();
        let scrut = self.stack();
        let site = self.conversion.compu_site(source);
        let output = low::SCoMatch { scrut, arms }.build(self.conversion, site);
        if let Some(protocol) = self.conversion.source.inner.compu_protocols.get(&source) {
            self.conversion.arena.inner.case_protocols.insert_new(output, protocol.clone());
        }
        self.computations.push(output);
        Step::Return(())
    }

    fn resume_fix_body(
        &mut self, source: high::CompuId, env: RenameEnvId, param: high::DefId, body: high::CompuId,
    ) -> Step<Self> {
        let captures = self.captures(body, Context::singleton(param), env);
        let recursive = self.conversion.alloc_like(param);
        let label = self.conversion.alloc_label("fix");
        let env = self
            .conversion
            .extend_env(env, captures.bindings.iter().copied().chain([(param, recursive)]));
        Step::Call {
            input: Work::Computation(body, env),
            frame: Work::Fix { source, captures, recursive, label },
        }
    }

    fn resume_fix(
        &mut self, source: high::CompuId, captures: Captures, recursive: high::DefId,
        label: high::DefId,
    ) -> Step<Self> {
        let body = self.computation();
        let site = self.conversion.compu_site(source);
        let environment = self.conversion.captured_value_inside(&captures.bindings, site);
        let code: low::ValueId = label.build(self.conversion, site);
        let closure = low::ClosurePackage { environment, code }.build(self.conversion, site);
        let binder = recursive.build(self.conversion, None);
        let body =
            low::LetValue { binder, bindee: closure, tail: body }.build(self.conversion, site);
        let environment = self.conversion.captured_pattern(&captures.bindings);
        let entry = low::EntryParameters::Closure { environment };
        let block = low::Block { label, entry, body }.build(self.conversion, site);
        let protocol =
            self.conversion.source.inner.compu_protocols.get(&source).cloned().unwrap_or_default();
        self.conversion
            .arena
            .inner
            .entry_protocols
            .insert_new(block, low::EntryProtocol::Closure(protocol));
        let environment =
            self.conversion.captured_value_outside(&captures.sources, captures.env, site);
        let argument = low::EntryArgument::Closure { environment };
        let stack = self.stack();
        self.computations
            .push(low::Jump { target: block, argument, stack }.build(self.conversion, site));
        Step::Return(())
    }

    fn resume_procedure_tail(
        &mut self, source: high::CompuId, label: high::DefId, kind: low::LabelKind,
        parameters: Vec<(high::DefId, high::DefId)>, tail: high::CompuId, env: RenameEnvId,
    ) -> Step<Self> {
        let body = self.computation();
        let site = self.conversion.compu_site(source);
        let pushed = parameters.len();
        // A closed label pops the variables each jump pushes, the first on top.
        let body = parameters.into_iter().rev().fold(body, |body, (_, parameter)| {
            let binder: low::VPatId = parameter.build(self.conversion, None);
            let ambient = low::StackVar.build(self.conversion, site);
            low::LetArg { binder: low::Cons(binder, low::StackVar), bindee: ambient, tail: body }
                .build(self.conversion, site)
        });
        self.computations.push(body);
        Step::Call {
            input: Work::Computation(tail, env),
            frame: Work::FinishProcedure { source, label, kind, pushed },
        }
    }

    fn resume_finish_procedure(
        &mut self, source: high::CompuId, label: high::DefId, kind: low::LabelKind, pushed: usize,
    ) -> Step<Self> {
        let tail = self.computation();
        let body = self.computation();
        let site = self.conversion.compu_site(source);
        let node = low::LetLabel { label, kind, body, tail }.build(self.conversion, site);
        // Jumps supply the procedure's stack under the variables a closed label pops.
        if let Some(protocol) = self.conversion.source.inner.compu_protocols.get(&source) {
            let protocol = (0..pushed).fold(protocol.clone(), |rest, _| {
                StackProtocol::Argument(Box::new(ValueProtocol::Unknown), Box::new(rest))
            });
            self.conversion.arena.inner.label_protocols.insert_new(label, protocol);
        }
        self.computations.push(node);
        Step::Return(())
    }

    fn resume_finish_call(
        &mut self, source: high::CompuId, env: RenameEnvId, procedure: high::DefId,
    ) -> Step<Self> {
        let stack = self.stack();
        let site = self.conversion.compu_site(source);
        let label = self.conversion.renamed_def(env, procedure);
        let variables = self.conversion.labels[&label].clone();
        let jump = if self.conversion.scheme.names_frames() {
            // The full scheme names each pushed variable with a stack let, deepest first,
            // so that the jump receives the ambient stack.
            let jump = low::JumpLabel { label, stack }.build(self.conversion, site);
            variables.iter().fold(jump, |tail, variable| {
                let value = self.conversion.translated_var(*variable, env, site);
                let ambient = low::StackVar.build(self.conversion, site);
                let bindee = low::Cons(value, ambient).build(self.conversion, site);
                low::LetStack { binder: low::StackVar, bindee, tail }.build(self.conversion, site)
            })
        } else {
            let stack = variables.iter().rev().fold(stack, |rest, variable| {
                let value = self.conversion.translated_var(*variable, env, site);
                low::Cons(value, rest).build(self.conversion, site)
            });
            low::JumpLabel { label, stack }.build(self.conversion, site)
        };
        self.computations.push(jump);
        Step::Return(())
    }
}

impl<D: Driver> Folder for ConversionFolder<'_, '_, D> {
    type Input = Work;
    type Frame = Work;
    type Output = ();

    fn resume(&mut self, frame: Work, (): ()) -> Step<Self> {
        self.enter(frame)
    }

    fn enter(&mut self, work: Work) -> Step<Self> {
        match work {
            | Work::Value(source, env) => self.visit_value(source, env),
            | Work::ProductItems { source, env, position, layout } => {
                self.resume_product_items(source, env, position, layout)
            }
            | Work::FinishProduct { source, count, layout } => {
                self.resume_finish_product(source, count, layout)
            }
            | Work::PrimitiveOperand { source, env, operation, operand } => {
                self.resume_primitive_operand(source, env, operation, operand)
            }
            | Work::FinishPrimitive { source, operation } => {
                self.resume_finish_primitive(source, operation)
            }
            | Work::OffsetDisplacement { source, env, displacement } => {
                self.resume_offset_displacement(source, env, displacement)
            }
            | Work::FinishAddrOffset(source) => self.resume_finish_addr_offset(source),
            | Work::FinishCtor { source, tag } => self.resume_finish_ctor(source, tag),
            | Work::Closure { source, captures } => self.resume_closure(source, captures),
            | Work::Stack(source, env) => self.visit_stack(source, env),
            | Work::StackRest { source, rest, env } => self.resume_stack_rest(source, rest, env),
            | Work::FinishArg(source) => self.resume_finish_arg(source),
            | Work::FinishTag { source, tag } => self.resume_finish_tag(source, tag),
            | Work::Continuation { source, binder, result, captures } => {
                self.resume_continuation(source, binder, result, captures)
            }
            | Work::Computation(source, env) => self.visit_computation(source, env),
            | Work::CompareChildren { source, env, position, branch } => {
                self.resume_compare_children(source, env, position, branch)
            }
            | Work::StoreAddress { source, env, scalar, address, next } => {
                self.resume_store_address(source, env, scalar, address, next)
            }
            | Work::StoreNext { source, env, scalar, next } => {
                self.resume_store_next(source, env, scalar, next)
            }
            | Work::FinishStore { source, scalar } => self.resume_finish_store(source, scalar),
            | Work::LoadNext { source, env, scalar, result, next } => {
                self.resume_load_next(source, env, scalar, result, next)
            }
            | Work::FinishLoad { source, scalar, binder } => {
                self.resume_finish_load(source, scalar, binder)
            }
            | Work::FinishHole(source) => self.resume_finish_hole(source),
            | Work::FinishExternCall { source, function } => {
                self.resume_finish_extern_call(source, function)
            }
            | Work::ForceStack { source, env, stack } => {
                self.resume_force_stack(source, env, stack)
            }
            | Work::FinishForce(source) => self.resume_finish_force(source),
            | Work::ReturnValue { source, env, value } => {
                self.resume_return_value(source, env, value)
            }
            | Work::FinishReturn(source) => self.resume_finish_return(source),
            | Work::ProductMatchBody { source, env, binder, body } => {
                self.resume_product_match_body(source, env, binder, body)
            }
            | Work::FinishProductMatch { source, binder } => {
                self.resume_finish_product_match(source, binder)
            }
            | Work::LetValueTail { source, env, binder, tail } => {
                self.resume_let_value_tail(source, env, binder, tail)
            }
            | Work::FinishLetValue { source, binder } => {
                self.resume_finish_let_value(source, binder)
            }
            | Work::LetStackTail { source, env, tail } => {
                self.resume_let_stack_tail(source, env, tail)
            }
            | Work::FinishLetStack(source) => self.resume_finish_let_stack(source),
            | Work::LetArgTail { source, env, binder, tail } => {
                self.resume_let_arg_tail(source, env, binder, tail)
            }
            | Work::FinishLetArg { source, binder } => self.resume_finish_let_arg(source, binder),
            | Work::MatchArms { source, env, position, arms } => {
                self.resume_match_arms(source, env, position, arms)
            }
            | Work::MatchArm { source, env, position, arms, binder } => {
                self.resume_match_arm(source, env, position, arms, binder)
            }
            | Work::CoCaseArms { source, env, position, dtors } => {
                self.resume_co_case_arms(source, env, position, dtors)
            }
            | Work::FinishCoCase { source, dtors } => self.resume_finish_co_case(source, dtors),
            | Work::FixBody { source, env, param, body } => {
                self.resume_fix_body(source, env, param, body)
            }
            | Work::Fix { source, captures, recursive, label } => {
                self.resume_fix(source, captures, recursive, label)
            }
            | Work::ProcedureTail { source, label, kind, parameters, tail, env } => {
                self.resume_procedure_tail(source, label, kind, parameters, tail, env)
            }
            | Work::FinishProcedure { source, label, kind, pushed } => {
                self.resume_finish_procedure(source, label, kind, pushed)
            }
            | Work::FinishCall { source, env, procedure } => {
                self.resume_finish_call(source, env, procedure)
            }
        }
    }
}

#[cfg(test)]
pub(super) mod tests;
