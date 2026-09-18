//! Resumable closure conversion with occurrence-local capture and renaming state.

use super::*;
use std::marker::PhantomData;
use zydeco_utils::fold::{Folder, Step};

pub(super) struct Captures {
    env: RenameEnvId,
    sources: Vec<high::DefId>,
    bindings: Vec<(high::DefId, high::DefId)>,
}

pub(super) enum Work {
    Value(high::ValueId, RenameEnvId),
    ValueChildren(high::ValueId, RenameEnvId, usize),
    FinishValue(high::ValueId),
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
    FinishStack(high::StackId),
    Continuation {
        source: high::StackId,
        binder: low::VPatId,
        captures: Captures,
    },
    Computation(high::CompuId, RenameEnvId),
    ComparisonChildren(high::CompuId, RenameEnvId, usize),
    StoreAddress(high::CompuId, RenameEnvId),
    StoreBody(high::CompuId, RenameEnvId),
    AfterValue(high::CompuId, RenameEnvId),
    AfterStack(high::CompuId, RenameEnvId),
    FinishComputation(high::CompuId),
    FinishBinding {
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
    CoCaseArms(high::CompuId, RenameEnvId, usize),
    Fix {
        source: high::CompuId,
        captures: Captures,
        recursive: high::DefId,
        label: high::DefId,
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

    fn pattern(&mut self, source: high::VPatId) -> PatternTranslation {
        D::run(&mut pattern::PatternFolder { conversion: self.conversion }, source)
    }

    fn captures(
        &mut self, body: high::CompuId, excluded: Context<high::DefId>, env: RenameEnvId,
    ) -> Captures {
        let sources = self.conversion.sorted_free_vars(body, excluded);
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
            | high::Value::Ctor(high::Ctor(_, body)) => {
                return Step::Call {
                    input: Work::Value(body, env),
                    frame: Work::FinishValue(source),
                };
            }
            | high::Value::VCons(_) | high::Value::Primitive(_) | high::Value::AddrOffset(_) => {
                return Step::TailCall(Work::ValueChildren(source, env, 0));
            }
        }
        Step::Return(())
    }

    fn visit_stack(&mut self, source: high::StackId, env: RenameEnvId) -> Step<Self> {
        match self.conversion.source.inner.stacks[&source].clone() {
            | high::Stack::Var(_) => {
                let site = self.conversion.stack_site(source);
                self.stacks.push(low::Bullet.build(self.conversion, site));
            }
            | high::Stack::Arg(high::Cons(value, rest)) => {
                return Step::Call {
                    input: Work::Value(value, env),
                    frame: Work::StackRest { source, rest, env },
                };
            }
            | high::Stack::Tag(high::Cons(_, rest)) => {
                return Step::Call {
                    input: Work::Stack(rest, env),
                    frame: Work::FinishStack(source),
                };
            }
            | high::Stack::Kont(high::Kont { binder, body }) => {
                let excluded = self
                    .conversion
                    .variables
                    .bound_variables(binder)
                    .expect("validated continuation binder")
                    .clone();
                let captures = self.captures(body, excluded, env);
                let PatternTranslation { pattern: binder, bindings } = self.pattern(binder);
                let env = self.conversion.extend_env(env, captures.bindings.iter().copied());
                let env = self.conversion.extend_env(env, bindings);
                return Step::Call {
                    input: Work::Computation(body, env),
                    frame: Work::Continuation { source, binder, captures },
                };
            }
        }
        Step::Return(())
    }

    fn visit_computation(&mut self, source: high::CompuId, env: RenameEnvId) -> Step<Self> {
        match self.conversion.source.inner.compus[&source].clone() {
            | high::Computation::Compare(_) => {
                Step::TailCall(Work::ComparisonChildren(source, env, 0))
            }
            | high::Computation::Memory(high::MemoryStep::Load { address, .. }) => Step::Call {
                input: Work::Value(address, env),
                frame: Work::AfterValue(source, env),
            },
            | high::Computation::Memory(high::MemoryStep::Store { value, .. }) => Step::Call {
                input: Work::Value(value, env),
                frame: Work::StoreAddress(source, env),
            },
            | high::Computation::Hole(high::SHole(stack))
            | high::Computation::ExternCall(high::ExternCall { stack, .. }) => Step::Call {
                input: Work::Stack(stack, env),
                frame: Work::FinishComputation(source),
            },
            | high::Computation::Force(high::SForce { thunk, .. }) => {
                Step::Call { input: Work::Value(thunk, env), frame: Work::AfterValue(source, env) }
            }
            | high::Computation::ProductMatch(high::SProductMatch { scrut, .. })
            | high::Computation::CoprodMatch(high::SCoprodMatch { scrut, .. }) => {
                Step::Call { input: Work::Value(scrut, env), frame: Work::AfterValue(source, env) }
            }
            | high::Computation::Join(high::LetJoin::Value(high::Let {
                binder,
                bindee: scrut,
                ..
            })) => {
                // A thunk bound to a plain variable lends the variable's name to its block.
                if let (high::ValuePattern::Var(owner), high::Value::Closure(_)) = (
                    &self.conversion.source.inner.vpats[&binder],
                    &self.conversion.source.inner.values[&scrut],
                ) {
                    self.conversion.thunk_owners.insert(scrut, *owner);
                }
                Step::Call { input: Work::Value(scrut, env), frame: Work::AfterValue(source, env) }
            }
            | high::Computation::Ret(high::SReturn { stack, .. })
            | high::Computation::Fix(high::SFix { stack, .. })
            | high::Computation::Join(high::LetJoin::Stack(high::Let {
                bindee: stack, ..
            }))
            | high::Computation::LetArg(high::Let { bindee: stack, .. })
            | high::Computation::CoCase(high::SCoMatch { scrut: stack, .. }) => {
                Step::Call { input: Work::Stack(stack, env), frame: Work::AfterStack(source, env) }
            }
        }
    }

    fn binding_body(
        &mut self, source: high::CompuId, binder: high::VPatId, body: high::CompuId,
        env: RenameEnvId,
    ) -> Step<Self> {
        let PatternTranslation { pattern: binder, bindings } = self.pattern(binder);
        let env = self.conversion.extend_env(env, bindings);
        Step::Call {
            input: Work::Computation(body, env),
            frame: Work::FinishBinding { source, binder },
        }
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
            | Work::Value(source, env) => return self.visit_value(source, env),
            | Work::Stack(source, env) => return self.visit_stack(source, env),
            | Work::Computation(source, env) => return self.visit_computation(source, env),
            | Work::ComparisonChildren(source, env, position) => {
                let high::Computation::Compare(high::CompareBranch {
                    operation,
                    operands,
                    when_true,
                    when_false,
                }) = self.conversion.source.inner.compus[&source]
                else {
                    unreachable!("comparison branch")
                };
                let input = match position {
                    | 0 | 1 => Some(Work::Value(operands[position], env)),
                    | 2 => Some(Work::Computation(when_true, env)),
                    | 3 => Some(Work::Computation(when_false, env)),
                    | _ => None,
                };
                if let Some(input) = input {
                    return Step::Call {
                        input,
                        frame: Work::ComparisonChildren(source, env, position + 1),
                    };
                }
                let when_false = self.computation();
                let when_true = self.computation();
                let second = self.value();
                let first = self.value();
                let site = self.conversion.compu_site(source);
                let node = low::CompareBranch {
                    operation,
                    operands: [first, second],
                    when_true,
                    when_false,
                }
                .build(self.conversion, site);
                self.computations.push(node);
            }
            | Work::StoreAddress(source, env) => {
                let high::Computation::Memory(high::MemoryStep::Store { address, .. }) =
                    self.conversion.source.inner.compus[&source]
                else {
                    unreachable!()
                };
                return Step::Call {
                    input: Work::Value(address, env),
                    frame: Work::StoreBody(source, env),
                };
            }
            | Work::StoreBody(source, env) => {
                let high::Computation::Memory(high::MemoryStep::Store { next, .. }) =
                    self.conversion.source.inner.compus[&source]
                else {
                    unreachable!()
                };
                return Step::Call {
                    input: Work::Computation(next, env),
                    frame: Work::FinishComputation(source),
                };
            }
            | Work::ValueChildren(source, env, position) => {
                let child = match &self.conversion.source.inner.values[&source] {
                    | high::Value::VCons(high::VCons { items, .. }) => items.get(position),
                    | high::Value::Primitive(high::Primitive { operands, .. }) => {
                        operands.get(position)
                    }
                    | high::Value::AddrOffset(high::AddrOffset { base, displacement }) => {
                        match position {
                            | 0 => Some(base),
                            | 1 => Some(displacement),
                            | _ => None,
                        }
                    }
                    | _ => unreachable!("value fields"),
                };
                return match child {
                    | Some(&child) => Step::Call {
                        input: Work::Value(child, env),
                        frame: Work::ValueChildren(source, env, position + 1),
                    },
                    | None => Step::TailCall(Work::FinishValue(source)),
                };
            }
            | Work::FinishValue(source) => {
                let value: low::Value = match self.conversion.source.inner.values[&source].clone() {
                    | high::Value::Ctor(high::Ctor(tag, _)) => low::Ctor(tag, self.value()).into(),
                    | high::Value::VCons(high::VCons { items, layout }) => {
                        let items = self.values.split_off(self.values.len() - items.len());
                        low::VCons::new(items, layout).into()
                    }
                    | high::Value::Primitive(high::Primitive { operation, .. }) => {
                        let second = self.value();
                        let first = self.value();
                        low::Primitive { operation, operands: [first, second] }.into()
                    }
                    | high::Value::AddrOffset(_) => {
                        let displacement = self.value();
                        let base = self.value();
                        low::AddrOffset { base, displacement }.into()
                    }
                    | _ => unreachable!("value with children"),
                };
                self.finish_value(source, value);
            }
            | Work::Closure { source, captures } => {
                let body = self.computation();
                let site = self.conversion.value_site(source);
                let protocol = match self.conversion.source.inner.value_protocols.get(&source) {
                    | Some(ValueProtocol::Thunk(protocol)) => *protocol.clone(),
                    | _ => StackProtocol::Unknown,
                };
                let environment = self.conversion.captured_pattern(&captures.bindings);
                let owner = self.conversion.thunk_owners.get(&source).copied();
                let label = self.conversion.alloc_label(owner, "thunk", "code");
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
            }
            | Work::StackRest { source, rest, env } => {
                return Step::Call {
                    input: Work::Stack(rest, env),
                    frame: Work::FinishStack(source),
                };
            }
            | Work::FinishStack(source) => {
                let rest = self.stack();
                let stack: low::Stack = match self.conversion.source.inner.stacks[&source].clone() {
                    | high::Stack::Arg(_) => low::Cons(self.value(), rest).into(),
                    | high::Stack::Tag(high::Cons(tag, _)) => low::Cons(tag, rest).into(),
                    | _ => unreachable!("stack with a tail"),
                };
                let site = self.conversion.stack_site(source);
                self.stacks.push(stack.build(self.conversion, site));
            }
            | Work::Continuation { source, binder, captures } => {
                let body = self.computation();
                let site = self.conversion.stack_site(source);
                let high::Stack::Kont(high::Kont { binder: original, .. }) =
                    self.conversion.source.inner.stacks[&source]
                else {
                    unreachable!()
                };
                let protocol = self
                    .conversion
                    .source
                    .inner
                    .pattern_protocols
                    .get(&original)
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
                let owner = self.conversion.plain_binder(binder);
                let label = self.conversion.alloc_label(owner, "result", "kont");
                let parameters = low::EntryParameters::Continuation { result: binder, environment };
                let code =
                    low::Block { label, entry: parameters, body }.build(self.conversion, site);
                self.conversion
                    .arena
                    .inner
                    .entry_protocols
                    .insert_new(code, low::EntryProtocol::Continuation(protocol));
                let environment =
                    self.conversion.captured_value_outside(&captures.sources, captures.env, site);
                let ambient = low::Bullet.build(self.conversion, site);
                let residual = low::Cons(environment, ambient).build(self.conversion, site);
                let package =
                    low::ContinuationPackage { code, residual }.build(self.conversion, site);
                self.conversion.arena.inner.continuations.insert_new(package, entry);
                self.stacks.push(package);
            }
            | Work::AfterValue(source, env) => {
                return match self.conversion.source.inner.compus[&source].clone() {
                    | high::Computation::Force(high::SForce { stack, .. }) => Step::Call {
                        input: Work::Stack(stack, env),
                        frame: Work::FinishComputation(source),
                    },
                    | high::Computation::ProductMatch(high::SProductMatch {
                        binder, body, ..
                    })
                    | high::Computation::Join(high::LetJoin::Value(high::Let {
                        binder,
                        tail: body,
                        ..
                    }))
                    | high::Computation::Memory(high::MemoryStep::Load {
                        result: binder,
                        next: body,
                        ..
                    }) => self.binding_body(source, binder, body, env),
                    | high::Computation::CoprodMatch(_) => Step::TailCall(Work::MatchArms {
                        source,
                        env,
                        position: 0,
                        arms: Vec::new(),
                    }),
                    | _ => unreachable!("computation with a value prefix"),
                };
            }
            | Work::AfterStack(source, env) => {
                return match self.conversion.source.inner.compus[&source].clone() {
                    | high::Computation::Ret(high::SReturn { value, .. }) => Step::Call {
                        input: Work::Value(value, env),
                        frame: Work::FinishComputation(source),
                    },
                    | high::Computation::Join(high::LetJoin::Stack(high::Let { tail, .. })) => {
                        Step::Call {
                            input: Work::Computation(tail, env),
                            frame: Work::FinishComputation(source),
                        }
                    }
                    | high::Computation::LetArg(high::Let {
                        binder: high::Cons(binder, _),
                        tail,
                        ..
                    }) => self.binding_body(source, binder, tail, env),
                    | high::Computation::CoCase(_) => {
                        Step::TailCall(Work::CoCaseArms(source, env, 0))
                    }
                    | high::Computation::Fix(high::SFix { param, body, .. }) => {
                        let captures = self.captures(body, Context::singleton(param), env);
                        let recursive = self.conversion.alloc_like(param);
                        let label = self.conversion.alloc_label(Some(param), "fix", "code");
                        let env = self.conversion.extend_env(
                            env,
                            captures.bindings.iter().copied().chain([(param, recursive)]),
                        );
                        Step::Call {
                            input: Work::Computation(body, env),
                            frame: Work::Fix { source, captures, recursive, label },
                        }
                    }
                    | _ => unreachable!("computation with a stack prefix"),
                };
            }
            | Work::FinishBinding { source, binder } => {
                let body = self.computation();
                let compu: low::Computation = match self.conversion.source.inner.compus[&source] {
                    | high::Computation::Memory(high::MemoryStep::Load { scalar, .. }) => {
                        low::MemoryStep::Load {
                            scalar,
                            address: self.value(),
                            result: binder,
                            next: body,
                        }
                        .into()
                    }
                    | high::Computation::ProductMatch(_) => {
                        low::SProductMatch { scrut: self.value(), binder, body }.into()
                    }
                    | high::Computation::Join(high::LetJoin::Value(_)) => {
                        low::LetValue { binder, bindee: self.value(), tail: body }.into()
                    }
                    | high::Computation::LetArg(_) => low::LetArg {
                        binder: low::Cons(binder, low::Bullet),
                        bindee: self.stack(),
                        tail: body,
                    }
                    .into(),
                    | _ => unreachable!("binding body"),
                };
                let site = self.conversion.compu_site(source);
                self.computations.push(compu.build(self.conversion, site));
            }
            | Work::MatchArms { source, env, position, arms } => {
                let high::Computation::CoprodMatch(high::SCoprodMatch { arms: originals, .. }) =
                    &self.conversion.source.inner.compus[&source]
                else {
                    unreachable!()
                };
                if let Some(arm) = originals.get(position) {
                    let high::Matcher { binder, tail } = *arm;
                    let PatternTranslation { pattern: binder, bindings } = self.pattern(binder);
                    let body_env = self.conversion.extend_env(env, bindings);
                    return Step::Call {
                        input: Work::Computation(tail, body_env),
                        frame: Work::MatchArm { source, env, position, arms, binder },
                    };
                }
                let scrut = self.value();
                let site = self.conversion.compu_site(source);
                self.computations
                    .push(low::SCoprodMatch { scrut, arms }.build(self.conversion, site));
            }
            | Work::MatchArm { source, env, position, mut arms, binder } => {
                arms.push(low::Matcher { binder, tail: self.computation() });
                return Step::TailCall(Work::MatchArms {
                    source,
                    env,
                    position: position + 1,
                    arms,
                });
            }
            | Work::CoCaseArms(source, env, position) => {
                let high::Computation::CoCase(high::SCoMatch { arms, .. }) =
                    &self.conversion.source.inner.compus[&source]
                else {
                    unreachable!()
                };
                return match arms.get(position) {
                    | Some(arm) => Step::Call {
                        input: Work::Computation(arm.tail, env),
                        frame: Work::CoCaseArms(source, env, position + 1),
                    },
                    | None => Step::TailCall(Work::FinishComputation(source)),
                };
            }
            | Work::Fix { source, captures, recursive, label } => {
                let body = self.computation();
                let site = self.conversion.compu_site(source);
                let environment = self.conversion.captured_value_inside(&captures.bindings, site);
                let code: low::ValueId = label.build(self.conversion, site);
                let closure =
                    low::ClosurePackage { environment, code }.build(self.conversion, site);
                let binder = recursive.build(self.conversion, None);
                let body = low::LetValue { binder, bindee: closure, tail: body }
                    .build(self.conversion, site);
                let environment = self.conversion.captured_pattern(&captures.bindings);
                let entry = low::EntryParameters::Closure { environment };
                let block = low::Block { label, entry, body }.build(self.conversion, site);
                let protocol = self
                    .conversion
                    .source
                    .inner
                    .compu_protocols
                    .get(&source)
                    .cloned()
                    .unwrap_or_default();
                self.conversion
                    .arena
                    .inner
                    .entry_protocols
                    .insert_new(block, low::EntryProtocol::Closure(protocol));
                let environment =
                    self.conversion.captured_value_outside(&captures.sources, captures.env, site);
                let argument = low::EntryArgument::Closure { environment };
                let stack = self.stack();
                self.computations.push(
                    low::Jump { target: block, argument, stack }.build(self.conversion, site),
                );
            }
            | Work::FinishComputation(source) => {
                let site = self.conversion.compu_site(source);
                let compu: low::Computation = match self.conversion.source.inner.compus[&source]
                    .clone()
                {
                    | high::Computation::Memory(high::MemoryStep::Store { scalar, .. }) => {
                        let address = self.value();
                        let value = self.value();
                        low::MemoryStep::Store { scalar, address, value, next: self.computation() }
                            .into()
                    }
                    | high::Computation::Hole(_) => low::SHole(self.stack()).into(),
                    | high::Computation::ExternCall(high::ExternCall { function, .. }) => {
                        low::ExternCall { function, stack: self.stack() }.into()
                    }
                    | high::Computation::Force(_) => {
                        let package = self.value();
                        let stack = self.stack();
                        let environment_def = self.conversion.alloc_def(VarName("env".into()));
                        let code_def = self.conversion.alloc_def(VarName("code".into()));
                        let environment: low::VPatId = environment_def.build(self.conversion, None);
                        let code: low::VPatId = code_def.build(self.conversion, None);
                        let environment_value = environment_def.build(self.conversion, site);
                        let target = code_def.build(self.conversion, site);
                        let argument =
                            low::EntryArgument::Closure { environment: environment_value };
                        let body =
                            low::Jump { target, argument, stack }.build(self.conversion, site);
                        low::OpenClosure { package, environment, code, body }.into()
                    }
                    | high::Computation::Ret(_) => {
                        let package = self.stack();
                        let value = self.value();
                        let code_def = self.conversion.alloc_def(VarName("kont".into()));
                        let code = code_def.build(self.conversion, None);
                        let target = code_def.build(self.conversion, site);
                        let stack = low::Bullet.build(self.conversion, site);
                        let argument = low::EntryArgument::Continuation { result: value };
                        let body =
                            low::Jump { target, argument, stack }.build(self.conversion, site);
                        low::OpenContinuation { package, code, body }.into()
                    }
                    | high::Computation::Join(high::LetJoin::Stack(_)) => low::LetStack {
                        binder: low::Bullet,
                        bindee: self.stack(),
                        tail: self.computation(),
                    }
                    .into(),
                    | high::Computation::CoCase(high::SCoMatch { arms, .. }) => {
                        let tails =
                            self.computations.split_off(self.computations.len() - arms.len());
                        let arms = arms
                            .into_iter()
                            .zip(tails)
                            .map(|(arm, tail)| low::CoMatcher { dtor: arm.dtor, tail })
                            .collect();
                        low::SCoMatch { scrut: self.stack(), arms }.into()
                    }
                    | _ => unreachable!("ordinary computation reconstruction"),
                };
                let output = compu.build(self.conversion, site);
                if matches!(
                    self.conversion.source.inner.compus[&source],
                    high::Computation::CoCase(_)
                ) && let Some(protocol) =
                    self.conversion.source.inner.compu_protocols.get(&source)
                {
                    self.conversion.arena.inner.case_protocols.insert_new(output, protocol.clone());
                }
                self.computations.push(output);
            }
        }
        Step::Return(())
    }
}

#[cfg(test)]
pub(super) mod tests;
