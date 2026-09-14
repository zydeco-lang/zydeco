//! Defunctionalized CPS consumers and the assembly construction schedule.
//!
//! A consumer continues compilation with a new context. A folder frame resumes
//! construction after a child returns an entry ID, which may still be unpublished.

use super::*;
use std::vec::IntoIter;
use zydeco_utils::fold::{Folder, Step};

/// Internal IDs never consume an assembly allocation slot. Links are affine:
/// a value hole may abandon a consumer, but no consumer may run twice.
pub(super) struct ContId(usize);

pub(super) enum ContextUpdate {
    Keep,
    Bind(VarId),
    Clear,
}

impl ContextUpdate {
    fn apply(self, context: &Context) -> Context {
        match self {
            | Self::Keep => context.clone(),
            | Self::Bind(variable) => context.clone() + [variable],
            | Self::Clear => Context::new(),
        }
    }
}

pub(super) enum Action {
    Value(sk::ValueId),
    Pattern(sk::VPatId),
    Emit(Instruction, ContextUpdate),
    Alias { variable: VarId, pattern: sk::VPatId },
}

pub(super) enum Sequence {
    Values(IntoIter<sk::ValueId>),
    Patterns(IntoIter<sk::VPatId>),
    Aliases { variable: VarId, patterns: IntoIter<sk::VPatId> },
    PopSlots(IntoIter<VarId>),
    PushSlots(IntoIter<VarId>),
}

impl Sequence {
    fn next(&mut self) -> Option<Action> {
        match self {
            // Producers push the last field first; consumers pop the first field first.
            | Self::Values(values) => values.next_back().map(Action::Value),
            | Self::Patterns(patterns) => patterns.next().map(Action::Pattern),
            | Self::Aliases { variable, patterns } => {
                patterns.next().map(|pattern| Action::Alias { variable: *variable, pattern })
            }
            | Self::PopSlots(slots) => {
                slots.next().map(|var| Action::Emit(Pop(var).into(), ContextUpdate::Bind(var)))
            }
            | Self::PushSlots(slots) => slots
                .next_back()
                .map(|var| Action::Emit(Push(Atom::Var(var)).into(), ContextUpdate::Keep)),
        }
    }
}

pub(super) enum Continuation {
    Body(sk::CompuId),
    End(Terminator),
    Next { action: Action, next: ContId },
    Sequence { sequence: Sequence, next: ContId },
    CoprodMatch(Vec<Matcher<sk::VPatId, sk::CompuId>>),
    CoCase(Vec<CoMatcher<Cons<sk::DtorIdx, sk::Bullet>, sk::CompuId>>),
}

struct PendingInstruction {
    id: ProgId,
    instruction: Instruction,
    context: Context,
    update: ContextUpdate,
    next: ContId,
}

enum BranchArms {
    Coproduct(IntoIter<Matcher<sk::VPatId, sk::CompuId>>),
    Codata(IntoIter<CoMatcher<Cons<sk::DtorIdx, sk::Bullet>, sk::CompuId>>),
}

pub(super) struct Branches {
    remaining: BranchArms,
    lowered: Vec<(Tag, ProgId)>,
    context: Context,
}

pub(super) enum Work {
    Start(sk::CompuId),
    Value(sk::ValueId, Context, ContId),
    Pattern(sk::VPatId, Context, ContId),
    Stack(sk::StackId, Context, ContId),
    Compu(sk::CompuId, Context),
    Apply(ContId, Context),
    Branches(Branches),
    Drain(ProgId),
}

pub(super) enum Frame {
    Root,
    Publish { id: ProgId, instruction: Instruction, context: Context, root: ProgId },
    Block { symbol: SymId, name: String, context: Context, next: ContId },
    ResumeEntry { label: sk::DefId, bindings: Vec<(VarId, VarId)>, context: Context, next: ContId },
    Branch { branches: Branches, tag: Tag },
}

pub(super) struct Lowering<'lo, 'ir> {
    pub(super) lo: &'lo mut Lowerer<'ir>,
    // Consumed suffix slots are reused; interior holes preserve outstanding IDs.
    // Abandoned consumers remain occupied and drop without following their links.
    continuations: Vec<Option<Continuation>>,
    pending: Vec<PendingInstruction>,
}

impl<'lo, 'ir> Lowering<'lo, 'ir> {
    pub(super) fn new(lo: &'lo mut Lowerer<'ir>) -> Self {
        Self { lo, continuations: Vec::new(), pending: Vec::new() }
    }

    pub(super) fn save(&mut self, continuation: Continuation) -> ContId {
        let id = ContId(self.continuations.len());
        self.continuations.push(Some(continuation));
        id
    }

    pub(super) fn then(&mut self, action: Action, next: ContId) -> ContId {
        self.save(Continuation::Next { action, next })
    }

    pub(super) fn sequence(&mut self, sequence: Sequence, next: ContId) -> ContId {
        self.save(Continuation::Sequence { sequence, next })
    }

    pub(super) fn instruction(
        &mut self, instruction: impl Into<Instruction>, next: ContId,
    ) -> ContId {
        self.then(Action::Emit(instruction.into(), ContextUpdate::Keep), next)
    }

    pub(super) fn emit(
        &mut self, instruction: impl Into<Instruction>, context: Context, update: ContextUpdate,
        next: ContId,
    ) -> Step<Self> {
        let id = self.lo.allocator.alloc();
        self.pending.push(PendingInstruction {
            id,
            instruction: instruction.into(),
            context,
            update,
            next,
        });
        Step::Return(id)
    }

    fn action(&mut self, action: Action, context: Context, next: ContId) -> Step<Self> {
        match action {
            | Action::Value(value) => Step::TailCall(Work::Value(value, context, next)),
            | Action::Pattern(pattern) => Step::TailCall(Work::Pattern(pattern, context, next)),
            | Action::Emit(instruction, update) => self.emit(instruction, context, update, next),
            | Action::Alias { variable, pattern } => {
                let next = self.then(Action::Pattern(pattern), next);
                self.emit(Push(Atom::Var(variable)), context, ContextUpdate::Keep, next)
            }
        }
    }

    fn apply(&mut self, id: ContId, context: Context) -> Step<Self> {
        let continuation = if id.0 + 1 == self.continuations.len() {
            self.continuations.pop().flatten().expect("consumer already applied")
        } else {
            self.continuations[id.0].take().expect("consumer already applied")
        };
        // Retire the vacant suffix without shifting any outstanding consumer.
        while self.continuations.last().is_some_and(Option::is_none) {
            self.continuations.pop();
        }
        match continuation {
            | Continuation::Body(body) => Step::TailCall(Work::Compu(body, context)),
            | Continuation::End(end) => {
                Step::Return(Program::Terminator(end).build(self.lo, context))
            }
            | Continuation::Next { action, next } => self.action(action, context, next),
            | Continuation::Sequence { mut sequence, next } => {
                let Some(action) = sequence.next() else {
                    return Step::TailCall(Work::Apply(next, context));
                };
                let next = self.sequence(sequence, next);
                self.action(action, context, next)
            }
            | Continuation::CoprodMatch(arms) => {
                // Empty partial-pattern fallthrough also applies to scalar scrutinees.
                if arms.is_empty() {
                    return Step::Return(Abort.build(self.lo, context));
                }
                if arms.iter().all(|arm| {
                    matches!(self.lo.sps_low.inner.vpats[&arm.binder], sk::ValuePattern::Ctor(_))
                }) {
                    Step::TailCall(Work::Branches(Branches {
                        remaining: BranchArms::Coproduct(arms.into_iter()),
                        lowered: Vec::new(),
                        context,
                    }))
                } else {
                    assert_eq!(arms.len(), 1, "Irrefutable pattern matcher must be unique in ZASM");
                    let Matcher { binder, tail } = arms[0];
                    let next = self.save(Continuation::Body(tail));
                    Step::TailCall(Work::Pattern(binder, context, next))
                }
            }
            | Continuation::CoCase(arms) => Step::TailCall(Work::Branches(Branches {
                remaining: BranchArms::Codata(arms.into_iter()),
                lowered: Vec::new(),
                context,
            })),
        }
    }

    fn branches(&mut self, mut branches: Branches) -> Step<Self> {
        let child = match &mut branches.remaining {
            | BranchArms::Coproduct(arms) => arms.next().map(|Matcher { binder, tail }| {
                let sk::ValuePattern::Ctor(Ctor(ctor, binder)) =
                    self.lo.sps_low.inner.vpats[&binder].clone()
                else {
                    unreachable!("constructor arms were checked before starting the table")
                };
                let tag = Tag { idx: ctor.idx, name: Some(ctor.name.plain().to_owned()) };
                let next = self.save(Continuation::Body(tail));
                (tag, Work::Pattern(binder, branches.context.clone(), next))
            }),
            | BranchArms::Codata(arms) => {
                arms.next().map(|CoMatcher { dtor: Cons(dtor, sk::Bullet), tail }| {
                    let tag = Tag { idx: dtor.idx, name: Some(dtor.name.plain().to_owned()) };
                    (tag, Work::Compu(tail, branches.context.clone()))
                })
            }
        };
        if let Some((tag, input)) = child {
            return Step::Call { input, frame: Frame::Branch { branches, tag } };
        }
        let end = PopBranch(branches.lowered);
        match branches.remaining {
            | BranchArms::Coproduct(_) => {
                let next = self.save(Continuation::End(end.into()));
                self.emit(
                    Unpack(ProductLayout::new(2, 2)),
                    branches.context,
                    ContextUpdate::Keep,
                    next,
                )
            }
            | BranchArms::Codata(_) => Step::Return(end.build(self.lo, branches.context)),
        }
    }
}

#[cfg(test)]
mod tests;

impl Folder for Lowering<'_, '_> {
    type Input = Work;
    type Output = ProgId;
    type Frame = Frame;

    fn enter(&mut self, input: Work) -> Step<Self> {
        match input {
            | Work::Start(root) => {
                Step::Call { input: Work::Compu(root, Context::new()), frame: Frame::Root }
            }
            | Work::Value(value, context, next) => self.value(value, context, next),
            | Work::Pattern(pattern, context, next) => self.pattern(pattern, context, next),
            | Work::Stack(stack, context, next) => self.stack(stack, context, next),
            | Work::Compu(compu, context) => self.compu(compu, context),
            | Work::Apply(next, context) => self.apply(next, context),
            | Work::Branches(branches) => self.branches(branches),
            | Work::Drain(root) => {
                let Some(pending) = self.pending.pop() else { return Step::Return(root) };
                let PendingInstruction { id, instruction, context, update, next } = pending;
                let next_context = update.apply(&context);
                Step::Call {
                    input: Work::Apply(next, next_context),
                    frame: Frame::Publish { id, instruction, context, root },
                }
            }
        }
    }

    fn resume(&mut self, frame: Frame, child: ProgId) -> Step<Self> {
        match frame {
            | Frame::Root => {
                if self.lo.native_frames {
                    self.lo.arena.frame_entries.insert(child, crate::frames::Entry::Fresh);
                }
                Step::TailCall(Work::Drain(child))
            }
            | Frame::Publish { id, instruction, context, root } => {
                self.lo.arena.insert_program(id, Program::Instruction(instruction, child), context);
                // Publish this instruction before draining any jobs its consumer queued.
                Step::TailCall(Work::Drain(root))
            }
            | Frame::Block { symbol, name, context, next } => {
                if self.lo.native_frames {
                    self.lo.arena.frame_entries.insert(child, crate::frames::Entry::Fresh);
                }
                self.lo
                    .arena
                    .symbols
                    .replace_existing(symbol, NamedSymbol { name, inner: Symbol::Prog(child) });
                self.lo.arena.labels.insert_new(child, symbol);
                self.emit(Push(Atom::Sym(symbol)), context, ContextUpdate::Keep, next)
            }
            | Frame::ResumeEntry { label, bindings, context, next } => {
                let name = self
                    .lo
                    .sps_low
                    .admin
                    .def_name(self.lo.scoped, self.lo.statics, &label)
                    .plain()
                    .to_owned();
                let symbol = child.build(self.lo, (Some(name), Some(label)));
                let captures = bindings.iter().map(|(_, source)| *source).collect();
                self.lo
                    .arena
                    .frame_entries
                    .insert(child, crate::frames::Entry::Resume { bindings });
                let next = self.instruction(Push(Atom::Sym(symbol)), next);
                self.emit(
                    RetainFrame { entry: child, captures },
                    context,
                    ContextUpdate::Keep,
                    next,
                )
            }
            | Frame::Branch { mut branches, tag } => {
                let name = match branches.remaining {
                    | BranchArms::Coproduct(_) => "arm",
                    | BranchArms::Codata(_) => "coarm",
                };
                let _symbol = child.build(self.lo, (Some(name.to_owned()), None));
                branches.lowered.push((tag, child));
                Step::TailCall(Work::Branches(branches))
            }
        }
    }
}
