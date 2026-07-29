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
    pub fn new(arena: &'a mut AssemblyArena) -> Self {
        Self {
            arena,
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
        let entries: Vec<_> = self.arena.entry.iter().map(|(entry, ())| *entry).collect();

        for prog in symbol_programs.iter().copied() {
            let context = self.arena.contexts[&prog]
                .to_owned()
                .into_iter()
                .map(|var| (var, Slot::Unknown))
                .collect();
            let layout = Layout { control: im::Vector::new(), context };
            prog.stack_measure(&mut self, layout);
        }
        for entry in entries.iter().copied() {
            let context = self.arena.contexts[&entry]
                .to_owned()
                .into_iter()
                .map(|var| (var, Slot::Unknown))
                .collect();
            let layout = Layout { control: im::Vector::new(), context };
            entry.stack_measure(&mut self, layout);
        }
        for prog in symbol_programs {
            prog.stack_inline(&mut self);
        }
        for entry in entries {
            entry.stack_inline(&mut self);
        }
        let Self { arena, allocator: _, layouts, slots, inlined } = self;
        Ok(StackAnalysis { arena, layouts, slots, inlined })
    }
}

impl<'a> StackMeasure<'a> for ProgId {
    fn stack_measure(self, si: &mut StackAnalyzer<'a>, mut layout: Layout) {
        let prog = si.arena.programs[&self].to_owned();
        let _ = si.layouts.upsert(self, layout.to_owned());
        match prog {
            | Program::Terminator(terminator) => match terminator {
                | Terminator::Jump(Jump(_)) => {}
                | Terminator::PopJump(PopJump) => {
                    let Some(slot_id) = layout.control.pop_back() else { return };
                    let slot = si.slots[&slot_id].clone();
                    match slot {
                        | Slot::Sym(sym) => match si.arena.symbols[&sym].inner {
                            | Symbol::Prog(_) => si.inlined[&slot_id] = true,
                            | _ => {}
                        },
                        | Slot::Imm(_) | Slot::Product(_) | Slot::Unknown => {}
                    }
                }
                | Terminator::LeapJump(_) => {}
                | Terminator::PopBranch(_) => {}
                | Terminator::Abort(Abort) => {}
                | Terminator::Extern(_) => {}
            },
            | Program::Instruction(instruction, next) => {
                let layout = match instruction {
                    | Instruction::PackProduct(Pack(product)) => {
                        let items = (0..product.elements)
                            .map(|_| si.pop_control(&mut layout).unwrap_or(Slot::Unknown))
                            .collect();
                        si.push_control(&mut layout, Slot::Product(items));
                        layout
                    }
                    | Instruction::UnpackProduct(Unpack(product)) => {
                        let items = match si.pop_control(&mut layout) {
                            | Some(Slot::Product(items)) if items.len() == product.elements => {
                                items
                            }
                            | _ => vec![Slot::Unknown; product.elements],
                        };
                        for item in items.into_iter().rev() {
                            si.push_control(&mut layout, item);
                        }
                        layout
                    }
                    | Instruction::PushContext(Push(ContextMarker)) => {
                        si.push_control(&mut layout, Slot::Unknown);
                        layout
                    }
                    | Instruction::PopContext(Pop(ContextMarker)) => {
                        si.pop_control(&mut layout);
                        layout
                    }
                    | Instruction::AllocContext(Alloc(ContextMarker)) => {
                        layout.context.clear();
                        layout
                    }
                    | Instruction::PushArg(Push(atom)) => {
                        let slot = match atom {
                            | Atom::Var(var) => layout
                                .context
                                .iter()
                                .find(|(v, _)| v == &var)
                                .map(|(_, s)| s.clone())
                                .unwrap_or(Slot::Unknown),
                            | Atom::Sym(sym) => Slot::Sym(sym),
                            | Atom::Imm(imm) => Slot::Imm(imm),
                        };
                        si.push_control(&mut layout, slot);
                        layout
                    }
                    | Instruction::PopArg(Pop(var)) => {
                        let slot = match si.pop_control(&mut layout) {
                            | Some(slot) => slot,
                            | None => Slot::Unknown,
                        };
                        layout.context.push_back((var, slot));
                        layout
                    }
                    | Instruction::PushTag(Push(_)) => {
                        si.push_control(&mut layout, Slot::Unknown);
                        layout
                    }
                    | Instruction::Intrinsic(Intrinsic { name: _, arity }) => {
                        for _ in 0..arity {
                            si.pop_control(&mut layout);
                        }
                        si.push_control(&mut layout, Slot::Unknown);
                        layout
                    }
                    | Instruction::Swap(Swap) => {
                        if layout.control.len() < 2 {
                            layout
                        } else {
                            let a = si.pop_control(&mut layout).unwrap();
                            let b = si.pop_control(&mut layout).unwrap();
                            si.push_control(&mut layout, a);
                            si.push_control(&mut layout, b);
                            layout
                        }
                    }
                    | Instruction::Clear(context) => {
                        let context = std::collections::HashSet::<_>::from_iter(context);
                        layout.context.retain(|(var, _)| !context.contains(var));
                        layout
                    }
                };
                next.stack_measure(si, layout)
            }
        }
    }
}

impl<'a> StackInline<'a> for ProgId {
    fn stack_inline(self, si: &mut StackAnalyzer<'a>) {
        let prog = si.arena.programs[&self].to_owned();
        let layout = si.layouts[&self].to_owned();
        match prog {
            | Program::Terminator(terminator) => match terminator {
                | Terminator::Jump(_) => {}
                | Terminator::PopJump(PopJump) => {
                    let Some(slot_id) = layout.control.last() else { return };
                    let slot = si.slots[&slot_id].clone();
                    match slot {
                        | Slot::Sym(sym) => match si.arena.symbols[&sym].inner {
                            | Symbol::Prog(target) => si
                                .arena
                                .programs
                                .replace_existing_with(self, Terminator::Jump(Jump(target))),
                            | _ => {}
                        },
                        | Slot::Imm(_) | Slot::Product(_) | Slot::Unknown => {}
                    }
                }
                | Terminator::LeapJump(LeapJump) => {}
                | Terminator::PopBranch(PopBranch(_arms)) => {}
                | Terminator::Abort(Abort) => {}
                | Terminator::Extern(Extern { .. }) => {}
            },
            | Program::Instruction(instruction, next) => {
                match instruction {
                    | Instruction::PackProduct(Pack(_)) => {}
                    | Instruction::UnpackProduct(Unpack(_)) => {}
                    | Instruction::PushContext(Push(ContextMarker)) => {}
                    | Instruction::PopContext(Pop(ContextMarker)) => {}
                    | Instruction::AllocContext(Alloc(ContextMarker)) => {}
                    | Instruction::PushArg(Push(_)) => {
                        let new_slot = si.layouts[&next].control.last().unwrap();
                        if si.inlined[&new_slot] {
                            next.stack_inline(si);
                            let prog = si.arena.programs[&next].to_owned();
                            si.arena.programs.replace_existing(self, prog);
                            return;
                        }
                    }
                    | Instruction::PopArg(Pop(_var)) => {}
                    | Instruction::PushTag(Push(_tag)) => {}
                    | Instruction::Intrinsic(Intrinsic { .. }) => {}
                    | Instruction::Swap(Swap) => {}
                    | Instruction::Clear(_context) => {}
                };
                next.stack_inline(si);
            }
        }
    }
}
