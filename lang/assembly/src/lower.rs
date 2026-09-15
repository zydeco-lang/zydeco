//! Lower SPSLow values, patterns, stacks, and computations into [`AssemblyArena`].
//!
//! These syntax rules return work to the CPS folder in `lower/folder.rs`, which
//! owns compilation consumers, child-entry return frames, and instruction publication.

use super::{
    arena::{AssemblyArena, AssemblyBuild},
    syntax::*,
};
use derive_more::{AsMut, AsRef};
use std::collections::HashMap;
use zydeco_machine::closure::Closure;
use zydeco_stackir::{SpsLowProgram, arena::DefinitionNames as _, low::syntax as sk};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_utils::fold::{Driver, Step};

mod folder;
use folder::*;

#[cfg(test)]
mod tests;

#[derive(AsRef, AsMut)]
pub struct Lowerer<'a> {
    /// Sequential issuer scoped to this lowering run.
    #[as_mut(IdAllocator<AssemblyScope>)]
    allocator: IdAllocator<AssemblyScope>,
    #[as_ref]
    #[as_mut]
    pub arena: AssemblyArena,
    pub spans: &'a SpanArena,
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
    pub sps_low: &'a sk::SpsLowArena,
    pub root: sk::CompuId,
    unboxing: crate::unbox::LocalUnboxing,
    unboxed_var_slots: HashMap<sk::DefId, Vec<VarId>>,
    native_frames: bool,
}

impl<'a> Lowerer<'a> {
    pub fn new(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        sps_low: &'a SpsLowProgram,
    ) -> Self {
        Self::with_policy(spans, scoped, statics, sps_low, &crate::representation::Local)
    }

    pub fn with_policy<P: crate::representation::RepresentationPolicy + ?Sized>(
        spans: &'a SpanArena, scoped: &'a ScopedArena, statics: &'a StaticsArena,
        sps_low: &'a SpsLowProgram, policy: &P,
    ) -> Self {
        let arena = AssemblyArena::default();
        let unboxing = crate::unbox::LocalUnboxing::with_policy(sps_low, policy);
        Self {
            allocator: IdAllocator::new(),
            arena,
            spans,
            scoped,
            statics,
            sps_low: sps_low.arena(),
            root: sps_low.root(),
            unboxing,
            unboxed_var_slots: HashMap::new(),
            native_frames: false,
        }
    }

    pub(crate) fn with_native_frames(mut self) -> Self {
        self.native_frames = true;
        self
    }

    pub(crate) fn run_with_driver<D: Driver>(mut self) -> AssemblyBuild {
        let root = self.root;
        let root = D::run(&mut Lowering::new(&mut self), Work::Start(root));
        AssemblyBuild { arena: self.arena, root }
    }
}

impl Lowering<'_, '_> {
    fn pattern(&mut self, id: sk::VPatId, context: Context, next: ContId) -> Step<Self> {
        use sk::ValuePattern as VPat;
        match self.lo.sps_low.inner.vpats[&id].clone() {
            | VPat::Hole(Hole) | VPat::Triv(Triv) => {
                let var = VarName::from("_").build(self.lo, None);
                self.emit(Pop(var), context, ContextUpdate::Bind(var), next)
            }
            | VPat::Var(def) => {
                let name =
                    self.lo.sps_low.admin.def_name(self.lo.scoped, self.lo.statics, &def).clone();
                if let Some(&arity) = self.lo.unboxing.unboxed_vars.get(&def) {
                    let vars = (0..arity)
                        .map(|index| {
                            VarName::from(format!("{}#unbox{}", name.plain(), index))
                                .build(self.lo, None)
                        })
                        .collect::<Vec<_>>();
                    self.lo.unboxed_var_slots.insert(def, vars.clone());
                    let next = self.sequence(Sequence::PopSlots(vars.into_iter()), next);
                    Step::TailCall(Work::Apply(next, context))
                } else {
                    let var = name.build(self.lo, Some(def));
                    self.emit(Pop(var), context, ContextUpdate::Bind(var), next)
                }
            }
            | VPat::Ctor(_) => unreachable!("Ctor patterns should not directly appear in ZASM"),
            | VPat::Alias(Alias(patterns)) => {
                let variable = VarName::from("__alias__").build(self.lo, None);
                let next = self.sequence(
                    Sequence::Aliases { variable, patterns: patterns.into_vec().into_iter() },
                    next,
                );
                self.emit(Pop(variable), context, ContextUpdate::Bind(variable), next)
            }
            | VPat::VCons(sk::VCons { items, layout }) => {
                let elements = items.len();
                let next = self.sequence(Sequence::Patterns(items.into_iter()), next);
                if self.lo.unboxing.patterns.contains(&id) {
                    Step::TailCall(Work::Apply(next, context))
                } else {
                    self.emit(
                        Unpack(ProductLayout::new(layout.arity, elements)),
                        context,
                        ContextUpdate::Keep,
                        next,
                    )
                }
            }
        }
    }

    /// A value's entry pushes its words before applying the supplied consumer.
    fn value(&mut self, id: sk::ValueId, context: Context, next: ContId) -> Step<Self> {
        use sk::Value;
        match self.lo.sps_low.inner.values[&id].clone() {
            | Value::Hole(Hole) => Step::Return(Abort.build(self.lo, context)),
            | Value::Var(def) => {
                if let Some(vars) = self.lo.unboxed_var_slots.get(&def) {
                    let vars = vars.clone();
                    let next = self.sequence(Sequence::PushSlots(vars.into_iter()), next);
                    Step::TailCall(Work::Apply(next, context))
                } else {
                    let atom = match self.lo.arena.defs[&def] {
                        | DefId::Var(var) => Atom::Var(var),
                        | DefId::Sym(sym) => Atom::Sym(sym),
                    };
                    self.emit(Push(atom), context, ContextUpdate::Keep, next)
                }
            }
            | Value::Block(sk::Block { label, entry, body }) => {
                let name = self
                    .lo
                    .sps_low
                    .admin
                    .def_name(self.lo.scoped, self.lo.statics, &label)
                    .plain()
                    .to_owned();
                let symbol = Undefined.build(self.lo, (Some(name.clone()), Some(label)));
                let body = self.save(Continuation::Body(body));
                let body = entry
                    .words()
                    .rev()
                    .fold(body, |next, (_, pattern)| self.then(Action::Pattern(pattern), next));
                Step::Call {
                    input: Work::Apply(body, Context::new()),
                    frame: Frame::Block { symbol, name, context, next },
                }
            }
            | Value::ClosurePackage(sk::ClosurePackage { environment, code }) => {
                let next = if self.lo.unboxing.values.contains(&id) {
                    next
                } else {
                    self.instruction(
                        Pack(ProductLayout::new(Closure::<u64>::WORDS, Closure::<u64>::WORDS)),
                        next,
                    )
                };
                let next = Closure { environment, code }
                    .into_words()
                    .into_iter()
                    .fold(next, |next, value| self.then(Action::Value(value), next));
                Step::TailCall(Work::Apply(next, context))
            }
            | Value::Ctor(Ctor(ctor, body)) => {
                let next = self.instruction(Pack(ProductLayout::new(2, 2)), next);
                let tag = Tag { idx: ctor.idx, name: Some(ctor.name.plain().to_owned()) };
                let next = self.instruction(Push(tag), next);
                Step::TailCall(Work::Value(body, context, next))
            }
            | Value::Triv(Triv) => {
                self.emit(Push(Atom::Imm(Imm::Triv(Triv))), context, ContextUpdate::Keep, next)
            }
            | Value::VCons(sk::VCons { items, layout }) => {
                let next = if self.lo.unboxing.values.contains(&id) {
                    next
                } else {
                    self.instruction(Pack(ProductLayout::new(layout.arity, items.len())), next)
                };
                let next = self.sequence(Sequence::Values(items.into_iter()), next);
                Step::TailCall(Work::Apply(next, context))
            }
            | Value::Literal(literal) => {
                let atom = match literal {
                    | Literal::Integer(value) => Atom::Imm(Imm::Integer(value)),
                    | Literal::Float(value) => Atom::Imm(Imm::Float(value)),
                    | Literal::Char(value) => Atom::Imm(Imm::Char(value)),
                    | Literal::String(value) => {
                        Atom::Sym(value.build(self.lo, (Some(String::new()), None)))
                    }
                };
                self.emit(Push(atom), context, ContextUpdate::Keep, next)
            }
            | Value::Primitive(sk::Primitive { operation, operands }) => {
                let next = self.instruction(operation, next);
                let next = operands
                    .into_iter()
                    .fold(next, |next, value| self.then(Action::Value(value), next));
                Step::TailCall(Work::Apply(next, context))
            }
        }
    }

    fn stack(&mut self, id: sk::StackId, context: Context, next: ContId) -> Step<Self> {
        let stack = self.lo.sps_low.inner.stacks[&id].clone();
        if self.lo.native_frames
            && let Some(entry) = self.lo.sps_low.inner.continuations.get(&id).cloned()
        {
            let sk::Stack::ContinuationPackage(sk::ContinuationPackage { code, .. }) = stack else {
                unreachable!()
            };
            let sk::Value::Block(sk::Block { label, .. }) = self.lo.sps_low.inner.values[&code]
            else {
                unreachable!()
            };
            let bindings = entry
                .captures
                .into_iter()
                .map(|capture| {
                    let DefId::Var(source) = self.lo.arena.defs[&capture.source] else {
                        panic!("capture must have an activation slot")
                    };
                    let name = self
                        .lo
                        .sps_low
                        .admin
                        .def_name(self.lo.scoped, self.lo.statics, &capture.binding)
                        .clone();
                    let binding = name.build(self.lo, Some(capture.binding));
                    (binding, source)
                })
                .collect::<Vec<_>>();
            let entry_context = bindings.iter().map(|(binding, _)| *binding).collect();
            let body = self.save(Continuation::Body(entry.body));
            return Step::Call {
                input: Work::Pattern(entry.result, entry_context, body),
                frame: Frame::ResumeEntry { label, bindings, context, next },
            };
        }
        match stack {
            | sk::Stack::Var(sk::Bullet) => Step::TailCall(Work::Apply(next, context)),
            | sk::Stack::Arg(Cons(value, rest)) => {
                let next = self.then(Action::Value(value), next);
                Step::TailCall(Work::Stack(rest, context, next))
            }
            | sk::Stack::Tag(Cons(dtor, rest)) => {
                let tag = Tag { idx: dtor.idx, name: Some(dtor.name.plain().to_owned()) };
                let next = self.instruction(Push(tag), next);
                Step::TailCall(Work::Stack(rest, context, next))
            }
            | sk::Stack::ContinuationPackage(sk::ContinuationPackage { code, residual }) => {
                let next = self.then(Action::Value(code), next);
                Step::TailCall(Work::Stack(residual, context, next))
            }
        }
    }

    fn compu(&mut self, id: sk::CompuId, context: Context) -> Step<Self> {
        use sk::Computation as Compu;
        match self.lo.sps_low.inner.compus[&id].clone() {
            | Compu::Hole(sk::SHole(stack)) => {
                let next = self.save(Continuation::End(Abort.into()));
                Step::TailCall(Work::Stack(stack, context, next))
            }
            | Compu::Jump(sk::Jump { target, argument, stack }) => {
                let next = self.save(Continuation::End(PopJump.into()));
                let next = self
                    .then(Action::Emit(Alloc(ContextMarker).into(), ContextUpdate::Clear), next);
                let next = self.then(Action::Value(target), next);
                let next = self.then(Action::Value(argument.word().1), next);
                Step::TailCall(Work::Stack(stack, context, next))
            }
            | Compu::ProductMatch(sk::SProductMatch { scrut, binder, body })
            | Compu::LetValue(sk::LetValue { binder, bindee: scrut, tail: body }) => {
                let next = self.save(Continuation::Body(body));
                let next = self.then(Action::Pattern(binder), next);
                Step::TailCall(Work::Value(scrut, context, next))
            }
            | Compu::CoprodMatch(sk::SCoprodMatch { scrut, arms }) => {
                let next = self.save(Continuation::CoprodMatch(arms));
                Step::TailCall(Work::Value(scrut, context, next))
            }
            | Compu::LetStack(sk::LetStack { binder: sk::Bullet, bindee, tail }) => {
                let next = self.save(Continuation::Body(tail));
                Step::TailCall(Work::Stack(bindee, context, next))
            }
            | Compu::LetArg(sk::LetArg { binder: Cons(pattern, sk::Bullet), bindee, tail }) => {
                let next = self.save(Continuation::Body(tail));
                let next = self.then(Action::Pattern(pattern), next);
                Step::TailCall(Work::Stack(bindee, context, next))
            }
            | Compu::CoCase(sk::SCoMatch { scrut, arms }) => {
                let next = self.save(Continuation::CoCase(arms));
                Step::TailCall(Work::Stack(scrut, context, next))
            }
            | Compu::OpenClosure(sk::OpenClosure { package, environment, code, body }) => {
                let next = self.save(Continuation::Body(body));
                let next = Closure { environment, code }
                    .into_words()
                    .into_iter()
                    .rev()
                    .fold(next, |next, pattern| self.then(Action::Pattern(pattern), next));
                let next = if self.lo.unboxing.values.contains(&package) {
                    next
                } else {
                    self.instruction(
                        Unpack(ProductLayout::new(Closure::<u64>::WORDS, Closure::<u64>::WORDS)),
                        next,
                    )
                };
                Step::TailCall(Work::Value(package, context, next))
            }
            | Compu::OpenContinuation(sk::OpenContinuation { package, code, body }) => {
                let next = self.save(Continuation::Body(body));
                let next = self.then(Action::Pattern(code), next);
                Step::TailCall(Work::Stack(package, context, next))
            }
            | Compu::ExternCall(sk::ExternCall { function, stack }) => {
                let external = match function {
                    | sk::ExternalFunction::Host(role) => Extern::Host {
                        role,
                        name: role.host_name(),
                        arity: role.arity(),
                        mode: sk::HostCallMode::for_role(role).into(),
                    },
                    | sk::ExternalFunction::Foreign(import) => Extern::Foreign(import),
                    | sk::ExternalFunction::Unit(import) => Extern::Unit(import),
                };
                if !self.lo.arena.externs.contains(&external) {
                    self.lo.arena.externs.push(external.clone());
                }
                let next = self.save(Continuation::End(external.into()));
                Step::TailCall(Work::Stack(stack, context, next))
            }
        }
    }
}

impl From<sk::HostCallMode> for ExternMode {
    fn from(mode: sk::HostCallMode) -> Self {
        match mode {
            | sk::HostCallMode::Returning => Self::Returning,
            | sk::HostCallMode::Control => Self::Control,
        }
    }
}
