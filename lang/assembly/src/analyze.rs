use super::syntax::*;
use derive_more::{AsMut, AsRef};
use zydeco_utils::pass::CompilerPass;

zydeco_utils::new_key_type! {
    pub struct SlotId;
}

/// Allocation and owning storage scope for temporary stack-analysis slots.
#[derive(Debug)]
pub enum StackAnalysisScope {}

impl Allocates<SlotId> for StackAnalysisScope {}
impl ArenaSchema<SlotId> for StackAnalysisScope {
    type Item = Slot;
}

#[derive(Clone)]
pub struct Layout {
    pub control: im::Vector<SlotId>,
    pub context: im::Vector<(VarId, Slot)>,
}

#[derive(Clone)]
pub enum Slot {
    Sym(SymId),
    Imm(Imm),
    Product(Vec<Slot>),
    /// A product allocated in the current stack frame rather than the copying heap.
    StackProduct(Vec<Slot>),
    Unknown,
}

pub trait StackMeasure<'a> {
    fn stack_measure(self, si: &mut StackAnalyzer<'a>, layout: Layout);
}

pub trait StackInline<'a> {
    fn stack_inline(self, si: &mut StackAnalyzer<'a>);
}

#[derive(AsRef, AsMut)]
pub struct StackAnalyzer<'a> {
    #[as_ref(AssemblyArena)]
    #[as_mut(AssemblyArena)]
    pub arena: &'a mut AssemblyArena,
    pub root: ProgId,
    /// Issuer scoped to this analysis run's temporary stack slots.
    allocator: IdAllocator<StackAnalysisScope>,
    /// The stack layouts *before* each program point.
    pub layouts: ArenaAssoc<ProgId, Layout>,
    /// The slots in the control stack.
    pub slots: ArenaSparse<StackAnalysisScope, SlotId>,
    /// Whether the slot will be inlined.
    pub inlined: ArenaAssoc<SlotId, bool>,
}

/// Durable results of stack analysis; the temporary slot issuer has been dropped.
pub struct StackAnalysis<'a> {
    pub arena: &'a mut AssemblyArena,
    pub layouts: ArenaAssoc<ProgId, Layout>,
    pub slots: ArenaSparse<StackAnalysisScope, SlotId>,
    pub inlined: ArenaAssoc<SlotId, bool>,
}

impl<'a> StackAnalyzer<'a> {
    pub fn new(program: &'a mut AssemblyProgram) -> Self {
        let AssemblyProgram { arena, root } = program;
        Self {
            arena,
            root: *root,
            allocator: IdAllocator::new(),
            layouts: ArenaAssoc::new(),
            slots: ArenaSparse::new(),
            inlined: ArenaAssoc::new(),
        }
    }
    pub fn push_control(&mut self, layout: &mut Layout, slot: Slot) -> SlotId {
        let slot_id = self.allocator.alloc();
        self.slots.insert_new(slot_id, slot);
        self.inlined.insert_new(slot_id, false);
        layout.control.push_back(slot_id);
        slot_id
    }
    pub fn pop_control(&mut self, layout: &mut Layout) -> Option<Slot> {
        let slot_id = layout.control.pop_back();
        slot_id.map(|slot_id| self.slots[&slot_id].clone())
    }

    fn program_chain(&self, start: ProgId) -> Vec<ProgId> {
        std::iter::successors(Some(start), |program| match self.arena.programs[program] {
            | Program::Instruction(_, next) => Some(next),
            | Program::Terminator(_) => None,
        })
        .collect()
    }
}

impl<'a> CompilerPass for StackAnalyzer<'a> {
    type Arena = AssemblyArena;
    type Out = StackAnalysis<'a>;
    type Error = std::convert::Infallible;
    fn run(mut self) -> Result<Self::Out, Self::Error> {
        let symbol_programs: Vec<_> = self
            .arena
            .symbols
            .iter()
            .filter_map(|(_, sym)| match sym.inner {
                | Symbol::Prog(prog) => Some(prog),
                | Symbol::Undefined(_) | Symbol::StringLiteral(_) => None,
            })
            .collect();
        for prog in symbol_programs.iter().copied() {
            let context = self.arena.contexts[&prog]
                .iter()
                .copied()
                .map(|var| (var, Slot::Unknown))
                .collect();
            let layout = Layout { control: im::Vector::new(), context };
            prog.stack_measure(&mut self, layout);
        }
        let context = self.arena.contexts[&self.root]
            .iter()
            .copied()
            .map(|var| (var, Slot::Unknown))
            .collect();
        let layout = Layout { control: im::Vector::new(), context };
        self.root.stack_measure(&mut self, layout);
        for prog in symbol_programs {
            prog.stack_inline(&mut self);
        }
        self.root.stack_inline(&mut self);
        let Self { arena, root: _, allocator: _, layouts, slots, inlined } = self;
        Ok(StackAnalysis { arena, layouts, slots, inlined })
    }
}

impl<'a> StackMeasure<'a> for ProgId {
    fn stack_measure(self, si: &mut StackAnalyzer<'a>, layout: Layout) {
        let _ =
            si.program_chain(self).into_iter().fold(layout, |mut layout, program_id| {
                let program = si.arena.programs[&program_id].to_owned();
                let _ = si.layouts.upsert(program_id, layout.to_owned());
                match program {
                    | Program::Terminator(terminator) => match terminator {
                        | Terminator::PopJump(PopJump) => {
                            if let Some(slot_id) = layout.control.pop_back()
                                && matches!(
                                    si.slots[&slot_id],
                                    Slot::Sym(sym)
                                        if matches!(
                                            si.arena.symbols[&sym].inner,
                                            Symbol::Prog(_)
                                        )
                                )
                            {
                                si.inlined[&slot_id] = true;
                            }
                        }
                        | Terminator::Jump(_)
                        | Terminator::PopBranch(_)
                        | Terminator::Abort(_)
                        | Terminator::Extern(_) => {}
                    },
                    | Program::Instruction(instruction, _) => match instruction {
                        | Instruction::PackProduct(Pack(product)) => {
                            let items = (0..product.elements)
                                .map(|_| si.pop_control(&mut layout).unwrap_or(Slot::Unknown))
                                .collect();
                            if product.stack_alloc {
                                si.push_control(&mut layout, Slot::StackProduct(items));
                            } else {
                                si.push_control(&mut layout, Slot::Product(items));
                            }
                        }
                        | Instruction::UnpackProduct(Unpack(product)) => {
                            let items = match si.pop_control(&mut layout) {
                                | Some(Slot::Product(items)) if items.len() == product.elements => {
                                    items
                                }
                                | Some(Slot::StackProduct(items))
                                    if items.len() == product.elements =>
                                {
                                    items
                                }
                                | _ => vec![Slot::Unknown; product.elements],
                            };
                            items.into_iter().rev().for_each(|item| {
                                si.push_control(&mut layout, item);
                            });
                        }
                        | Instruction::AllocContext(Alloc(ContextMarker)) => {
                            layout.context.clear();
                        }
                        | Instruction::PushArg(Push(atom)) => {
                            let slot = match atom {
                                | Atom::Var(var) => layout
                                    .context
                                    .iter()
                                    .find(|(candidate, _)| candidate == &var)
                                    .map(|(_, slot)| slot.clone())
                                    .unwrap_or(Slot::Unknown),
                                | Atom::Sym(sym) => Slot::Sym(sym),
                                | Atom::Imm(imm) => Slot::Imm(imm),
                            };
                            si.push_control(&mut layout, slot);
                        }
                        | Instruction::PopArg(Pop(var)) => {
                            let slot = si.pop_control(&mut layout).unwrap_or(Slot::Unknown);
                            layout.context.push_back((var, slot));
                        }
                        | Instruction::PushTag(Push(_)) => {
                            si.push_control(&mut layout, Slot::Unknown);
                        }
                        | Instruction::Intrinsic(Intrinsic { name: _, arity }) => {
                            (0..arity).for_each(|_| {
                                si.pop_control(&mut layout);
                            });
                            si.push_control(&mut layout, Slot::Unknown);
                        }
                        | Instruction::Clear(context) => {
                            let context = std::collections::HashSet::<_>::from_iter(context);
                            layout.context.retain(|(var, _)| !context.contains(var));
                        }
                    },
                }
                layout
            });
    }
}

impl<'a> StackInline<'a> for ProgId {
    fn stack_inline(self, si: &mut StackAnalyzer<'a>) {
        si.program_chain(self).into_iter().rev().for_each(|program_id| {
            let program = si.arena.programs[&program_id].to_owned();
            let layout = si.layouts[&program_id].to_owned();
            match program {
                | Program::Terminator(Terminator::PopJump(PopJump)) => {
                    if let Some(target) =
                        layout.control.last().and_then(|slot_id| match si.slots[slot_id] {
                            | Slot::Sym(sym) => match si.arena.symbols[&sym].inner {
                                | Symbol::Prog(target) => Some(target),
                                | Symbol::Undefined(_) | Symbol::StringLiteral(_) => None,
                            },
                            | Slot::Imm(_)
                            | Slot::Product(_)
                            | Slot::StackProduct(_)
                            | Slot::Unknown => None,
                        })
                    {
                        si.arena
                            .programs
                            .replace_existing_with(program_id, Terminator::Jump(Jump(target)));
                    }
                }
                | Program::Instruction(Instruction::PushArg(Push(_)), next) => {
                    let new_slot = si.layouts[&next].control.last().unwrap();
                    if si.inlined[new_slot] {
                        let next_program = si.arena.programs[&next].to_owned();
                        si.arena.programs.replace_existing(program_id, next_program);
                    }
                }
                | Program::Terminator(_)
                | Program::Instruction(
                    Instruction::PackProduct(_)
                    | Instruction::UnpackProduct(_)
                    | Instruction::AllocContext(_)
                    | Instruction::PopArg(_)
                    | Instruction::PushTag(_)
                    | Instruction::Intrinsic(_)
                    | Instruction::Clear(_),
                    _,
                ) => {}
            }
        });
    }
}
