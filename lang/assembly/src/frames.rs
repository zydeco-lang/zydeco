//! Native preparation preserves activation ownership and checks entry contexts
//! before assigning slots. Portable ZASM consumers use the ordinary lowering path.

use crate::syntax::*;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use zydeco_machine::frames::{Layout, LayoutId};

#[derive(Clone, Debug)]
pub enum Entry {
    Fresh,
    /// (Entry binding, captured source). Both names address the source's slot.
    Resume {
        bindings: Vec<(VarId, VarId)>,
    },
}

#[derive(Clone, Debug, thiserror::Error, PartialEq, Eq)]
pub enum FramePlanError {
    #[error("program {program:?} belongs to incompatible activations")]
    Owner { program: ProgId },
    #[error("variable {variable:?} is unavailable at native program {program:?}")]
    Uninitialized { program: ProgId, variable: VarId },
    #[error("variable {variable:?} has an invalid frame alias")]
    Alias { variable: VarId },
    #[error("native program {program:?} has no activation owner")]
    MissingOwner { program: ProgId },
    #[error("native suspension at {program:?} disagrees with its resumption context")]
    ResumptionContext { program: ProgId },
}

pub struct NativeProgram {
    assembly: AssemblyProgram,
    frames: FramePlan,
}

impl NativeProgram {
    pub(crate) fn prepare(program: AssemblyBuild) -> Result<Self, FramePlanError> {
        let frames = FramePlan::analyze(&program.arena, program.root)?;
        Ok(Self { assembly: program.finish(), frames })
    }
    pub fn assembly(&self) -> &AssemblyProgram {
        &self.assembly
    }
    pub fn frames(&self) -> &FramePlan {
        &self.frames
    }
}

pub struct FramePlan {
    pub entries: BTreeMap<ProgId, Entry>,
    pub layouts: Vec<Layout>,
    pub owners: BTreeMap<ProgId, LayoutId>,
    pub slots: BTreeMap<VarId, usize>,
    pub live: BTreeMap<ProgId, Vec<usize>>,
}

impl FramePlan {
    fn successors(arena: &AssemblyArena, id: ProgId) -> Vec<ProgId> {
        let targets = match &arena.programs[&id] {
            | Program::Instruction(_, next) => vec![*next],
            | Program::Terminator(Terminator::Jump(Jump(target))) => vec![*target],
            | Program::Terminator(Terminator::PopBranch(PopBranch(arms))) => {
                arms.iter().map(|(_, target)| *target).collect()
            }
            | Program::Terminator(_) => Vec::new(),
        };
        targets.into_iter().filter(|target| !arena.frame_entries.contains_key(target)).collect()
    }

    fn uses(program: &Program) -> BTreeSet<VarId> {
        match program {
            | Program::Instruction(Instruction::PushArg(Push(Atom::Var(var))), _) => [*var].into(),
            | Program::Instruction(Instruction::RetainFrame(retain), _) => {
                retain.captures.iter().copied().collect()
            }
            | _ => BTreeSet::new(),
        }
    }

    fn after(program: &Program, mut variables: BTreeSet<VarId>) -> BTreeSet<VarId> {
        if let Program::Instruction(instruction, _) = program {
            match instruction {
                | Instruction::PopArg(Pop(var)) => {
                    variables.insert(*var);
                }
                | Instruction::AllocContext(_) => variables.clear(),
                | Instruction::Clear(cleared) => {
                    variables.retain(|var| !cleared.iter().any(|v| v == var))
                }
                | _ => {}
            }
        }
        variables
    }

    fn before(program: &Program, mut variables: BTreeSet<VarId>) -> BTreeSet<VarId> {
        if let Program::Instruction(instruction, _) = program {
            match instruction {
                | Instruction::PopArg(Pop(var)) => {
                    variables.remove(var);
                }
                | Instruction::AllocContext(_) => variables.clear(),
                | _ => {}
            }
        }
        variables.extend(Self::uses(program));
        variables
    }

    fn analyze(arena: &AssemblyArena, root: ProgId) -> Result<Self, FramePlanError> {
        // Ordinary edges stay within an activation; a retained entry belongs to
        // the activation that constructs it, even though it runs after a return.
        let mut owners = BTreeMap::new();
        let mut layouts = Vec::new();
        let mut queue = VecDeque::new();
        for (&entry, kind) in &arena.frame_entries {
            if matches!(kind, Entry::Fresh) {
                let id = LayoutId(layouts.len());
                layouts.push(Layout { id, words: 0 });
                queue.push_back((entry, id));
            }
        }
        while let Some((program, owner)) = queue.pop_front() {
            if let Some(previous) = owners.insert(program, owner) {
                if previous != owner {
                    return Err(FramePlanError::Owner { program });
                }
                continue;
            }
            queue.extend(Self::successors(arena, program).into_iter().map(|next| (next, owner)));
            if let Program::Instruction(Instruction::RetainFrame(retain), _) =
                &arena.programs[&program]
            {
                let Some(Entry::Resume { bindings }) = arena.frame_entries.get(&retain.entry)
                else {
                    return Err(FramePlanError::ResumptionContext { program });
                };
                if !bindings.iter().map(|(_, source)| source).eq(retain.captures.iter()) {
                    return Err(FramePlanError::ResumptionContext { program });
                }
                queue.push_back((retain.entry, owner));
            }
        }
        if !owners.contains_key(&root) {
            return Err(FramePlanError::MissingOwner { program: root });
        }

        // A resumption declares precisely its captured aliases. Forward must-
        // initialization checks actual definitions, rather than trusting contexts.
        let mut available = BTreeMap::<ProgId, BTreeSet<VarId>>::new();
        let mut queue = VecDeque::new();
        let mut aliases = BTreeMap::new();
        let mut variable_owners = BTreeMap::new();
        for (&entry, kind) in &arena.frame_entries {
            let owner =
                *owners.get(&entry).ok_or(FramePlanError::MissingOwner { program: entry })?;
            let bindings = match kind {
                | Entry::Fresh => BTreeSet::new(),
                | Entry::Resume { bindings } => {
                    for &(binding, source) in bindings {
                        if aliases.insert(binding, source).is_some() {
                            return Err(FramePlanError::Alias { variable: binding });
                        }
                        variable_owners.insert(binding, owner);
                    }
                    bindings.iter().map(|(binding, _)| *binding).collect()
                }
            };
            available.insert(entry, bindings);
            queue.push_back(entry);
        }
        while let Some(program) = queue.pop_front() {
            let outgoing = Self::after(&arena.programs[&program], available[&program].clone());
            for next in Self::successors(arena, program) {
                let merged = match available.get(&next) {
                    | Some(old) => old.intersection(&outgoing).copied().collect(),
                    | None => outgoing.clone(),
                };
                if available.get(&next) != Some(&merged) {
                    available.insert(next, merged);
                    queue.push_back(next);
                }
            }
        }
        for program in owners.keys() {
            let node = &arena.programs[program];
            let initialized =
                available.get(program).ok_or(FramePlanError::MissingOwner { program: *program })?;
            if let Some(variable) = Self::uses(node).difference(initialized).next() {
                return Err(FramePlanError::Uninitialized {
                    program: *program,
                    variable: *variable,
                });
            }
            if let Program::Instruction(Instruction::PopArg(Pop(var)), _) = node {
                // Rebinding an alias would overwrite a retained source slot.
                if aliases.contains_key(var) {
                    return Err(FramePlanError::Alias { variable: *var });
                }
                if let Some(previous) = variable_owners.insert(*var, owners[program])
                    && previous != owners[program]
                {
                    return Err(FramePlanError::Owner { program: *program });
                }
            }
        }
        let mut sources = BTreeMap::new();
        for &variable in variable_owners.keys() {
            let mut source = variable;
            let mut seen = BTreeSet::new();
            while let Some(&parent) = aliases.get(&source) {
                if !seen.insert(source) {
                    return Err(FramePlanError::Alias { variable });
                }
                source = parent;
            }
            if variable_owners.get(&source) != variable_owners.get(&variable) {
                return Err(FramePlanError::Alias { variable });
            }
            if !variable_owners.contains_key(&source) {
                return Err(FramePlanError::Alias { variable });
            }
            sources.insert(variable, source);
        }

        // Backward liveness stops at entry boundaries. Pending suspensions supply
        // their own root sets, including other live slots in this same activation.
        let mut predecessors = BTreeMap::<ProgId, Vec<ProgId>>::new();
        for program in owners.keys() {
            for next in Self::successors(arena, *program) {
                predecessors.entry(next).or_default().push(*program);
            }
        }
        let mut live = BTreeMap::<ProgId, BTreeSet<VarId>>::new();
        let mut queue = owners.keys().copied().collect::<VecDeque<_>>();
        while let Some(program) = queue.pop_front() {
            let outgoing = Self::successors(arena, program)
                .into_iter()
                .flat_map(|next| live.get(&next).into_iter().flatten().copied())
                .collect();
            let incoming = Self::before(&arena.programs[&program], outgoing);
            if live.get(&program) != Some(&incoming) {
                live.insert(program, incoming);
                queue.extend(predecessors.get(&program).into_iter().flatten().copied());
            }
        }
        // A pending continuation keeps its captures alive even while local code
        // prepares a callee or resumes another continuation in this activation.
        // The suspension-to-resumption edge consumes that suspension: it carries
        // the *incoming* pending set, retaining any older suspensions. At joins a
        // may-union is conservative. These sets describe preservation, whereas
        // `live` describes the values read by the currently executing code.
        let mut pending =
            owners.keys().map(|&id| (id, BTreeSet::new())).collect::<BTreeMap<_, _>>();
        let mut queue = owners.keys().copied().collect::<VecDeque<_>>();
        while let Some(program) = queue.pop_front() {
            let mut outgoing = pending[&program].clone();
            let mut edges = Vec::new();
            if let Program::Instruction(Instruction::RetainFrame(retain), _) =
                &arena.programs[&program]
            {
                edges.push((retain.entry, outgoing.clone()));
                outgoing.extend(retain.captures.iter().map(|var| sources[var]));
            }
            edges.extend(
                Self::successors(arena, program).into_iter().map(|id| (id, outgoing.clone())),
            );
            for (next, required) in edges {
                let previous = pending[&next].len();
                pending.get_mut(&next).unwrap().extend(required);
                if pending[&next].len() != previous {
                    queue.push_back(next);
                }
            }
        }

        // Color the interference graph independently for each activation. Even
        // a dead definition writes a slot, so it conflicts with every value
        // preserved across that write. Aliases never introduce a second slot.
        let mut conflicts =
            sources.values().map(|&var| (var, BTreeSet::new())).collect::<BTreeMap<_, _>>();
        for &program in owners.keys() {
            let mut required = pending[&program].clone();
            required.extend(live[&program].iter().map(|var| sources[var]));
            Self::interfere(&mut conflicts, &required);
            if let Program::Instruction(Instruction::PopArg(Pop(var)), _) =
                &arena.programs[&program]
            {
                let mut across = pending[&program].clone();
                across.extend(
                    Self::successors(arena, program)
                        .into_iter()
                        .flat_map(|next| live[&next].iter().map(|var| sources[var])),
                );
                across.insert(sources[var]);
                Self::interfere(&mut conflicts, &across);
            }
        }
        let mut order = conflicts.keys().copied().collect::<Vec<_>>();
        // Largest degree first, with stable IDs breaking ties for reproducibility.
        order.sort_by_key(|var| (std::cmp::Reverse(conflicts[var].len()), *var));
        let mut slots = BTreeMap::new();
        for variable in order {
            let occupied = conflicts[&variable]
                .iter()
                .filter_map(|var| slots.get(var).copied())
                .collect::<BTreeSet<_>>();
            let slot = (0..).find(|slot| !occupied.contains(slot)).unwrap();
            slots.insert(variable, slot);
            let layout = &mut layouts[variable_owners[&variable].0];
            layout.words = layout.words.max(slot + 1);
        }
        for (&variable, &source) in &sources {
            slots.insert(variable, slots[&source]);
        }
        let live = live
            .into_iter()
            .map(|(program, variables)| {
                let words = variables.into_iter().map(|var| slots[&var]).collect::<BTreeSet<_>>();
                (program, words.into_iter().collect())
            })
            .collect();
        Ok(Self { entries: arena.frame_entries.clone(), layouts, owners, slots, live })
    }

    fn interfere(conflicts: &mut BTreeMap<VarId, BTreeSet<VarId>>, variables: &BTreeSet<VarId>) {
        for &variable in variables {
            conflicts
                .get_mut(&variable)
                .unwrap()
                .extend(variables.iter().copied().filter(|other| *other != variable));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Graph {
        ids: IdAllocator<AssemblyScope>,
        arena: AssemblyArena,
    }

    impl Default for Graph {
        fn default() -> Self {
            Self { ids: IdAllocator::new(), arena: AssemblyArena::default() }
        }
    }

    impl Graph {
        fn variable(&mut self, name: &str) -> VarId {
            let id = self.ids.alloc();
            self.arena.variables.insert_new(id, VarName::from(name));
            id
        }
        fn instruction(&mut self, instruction: impl Into<Instruction>, next: ProgId) -> ProgId {
            let id = self.ids.alloc();
            self.arena.insert_program(
                id,
                Program::Instruction(instruction.into(), next),
                Context::new(),
            );
            id
        }
        fn end(&mut self) -> ProgId {
            let id = self.ids.alloc();
            self.arena.insert_program(id, Program::Terminator(Abort.into()), Context::new());
            id
        }
        fn plan(mut self, root: ProgId) -> FramePlan {
            self.arena.frame_entries.insert(root, Entry::Fresh);
            FramePlan::analyze(&self.arena, root).unwrap()
        }
    }

    #[test]
    fn sequential_values_share_slots_but_a_dead_write_cannot_clobber_a_live_value() {
        let mut graph = Graph::default();
        let first = graph.variable("first");
        let dead = graph.variable("dead");
        let second = graph.variable("second");
        let end = graph.end();
        let read_second = graph.instruction(Push(Atom::Var(second)), end);
        let write_second = graph.instruction(Pop(second), read_second);
        let read_first = graph.instruction(Push(Atom::Var(first)), write_second);
        let write_dead = graph.instruction(Pop(dead), read_first);
        let root = graph.instruction(Pop(first), write_dead);
        let plan = graph.plan(root);
        assert_eq!(plan.layouts[0].words, 2);
        assert_eq!(plan.slots[&first], plan.slots[&second]);
        assert_ne!(plan.slots[&first], plan.slots[&dead]);
        assert_eq!(plan.live[&read_second], [plan.slots[&second]]);
    }

    #[test]
    fn pending_captures_survive_preparation_and_an_inner_resumption() {
        let mut graph = Graph::default();
        let x = graph.variable("outer");
        let y = graph.variable("inner");
        let alias_x = graph.variable("outer_alias");
        let alias_y = graph.variable("inner_alias");
        let scratch = graph.variable("scratch");
        let later = graph.variable("later");
        let end = graph.end();
        let outer = graph.instruction(Push(Atom::Var(alias_x)), end);
        graph.arena.frame_entries.insert(outer, Entry::Resume { bindings: vec![(alias_x, x)] });
        let inner_end = graph.end();
        let read_later = graph.instruction(Push(Atom::Var(later)), inner_end);
        let write_later = graph.instruction(Pop(later), read_later);
        let inner = graph.instruction(Push(Atom::Var(alias_y)), write_later);
        graph.arena.frame_entries.insert(inner, Entry::Resume { bindings: vec![(alias_y, y)] });
        let call = graph.end();
        let prepare = graph.instruction(Pop(scratch), call);
        let retain_inner =
            graph.instruction(RetainFrame { entry: inner, captures: vec![y] }, prepare);
        let retain_outer =
            graph.instruction(RetainFrame { entry: outer, captures: vec![x] }, retain_inner);
        let write_y = graph.instruction(Pop(y), retain_outer);
        let root = graph.instruction(Pop(x), write_y);
        let plan = graph.plan(root);
        assert_eq!(plan.layouts[0].words, 3);
        assert_ne!(plan.slots[&x], plan.slots[&y]);
        assert_ne!(plan.slots[&scratch], plan.slots[&x]);
        assert_ne!(plan.slots[&scratch], plan.slots[&y]);
        assert_ne!(plan.slots[&later], plan.slots[&x]);
        assert_eq!(plan.slots[&alias_x], plan.slots[&x]);
        assert_eq!(plan.slots[&alias_y], plan.slots[&y]);
    }

    #[test]
    fn a_consumed_capture_can_share_storage_with_a_later_result() {
        let mut graph = Graph::default();
        let source = graph.variable("source");
        let alias = graph.variable("alias");
        let result = graph.variable("result");
        let end = graph.end();
        let read = graph.instruction(Push(Atom::Var(result)), end);
        let write = graph.instruction(Pop(result), read);
        let resume = graph.instruction(Push(Atom::Var(alias)), write);
        graph.arena.frame_entries.insert(resume, Entry::Resume { bindings: vec![(alias, source)] });
        let call = graph.end();
        let retain = graph.instruction(RetainFrame { entry: resume, captures: vec![source] }, call);
        let root = graph.instruction(Pop(source), retain);
        let plan = graph.plan(root);
        assert_eq!(plan.layouts[0].words, 1);
        assert_eq!(plan.slots[&source], plan.slots[&result]);
        assert_eq!(plan.slots[&alias], plan.slots[&result]);
    }

    #[test]
    fn initialization_is_checked_even_if_a_context_claims_a_binding_exists() {
        let mut allocator = IdAllocator::<AssemblyScope>::new();
        let mut arena = AssemblyArena::default();
        let variable = allocator.alloc();
        arena.variables.insert_new(variable, VarName::from("local"));
        let end = allocator.alloc();
        arena.insert_program(end, Program::Terminator(Abort.into()), Context::singleton(variable));
        let read = allocator.alloc();
        arena.insert_program(
            read,
            Program::Instruction(Push(Atom::Var(variable)).into(), end),
            Context::singleton(variable),
        );
        let root = allocator.alloc();
        arena.insert_program(
            root,
            Program::Instruction(Pop(variable).into(), read),
            Context::new(),
        );
        arena.frame_entries.insert(root, Entry::Fresh);
        let plan = FramePlan::analyze(&arena, root).unwrap();
        assert_eq!(plan.layouts[0].words, 1);
        assert!(plan.live[&root].is_empty());
        assert_eq!(plan.live[&read], [0]);

        arena.programs.replace_existing(root, Program::Terminator(Jump(read).into()));
        assert!(matches!(FramePlan::analyze(&arena, root),
            Err(FramePlanError::Uninitialized { program, variable: missing })
                if program == read && missing == variable));
    }

    #[test]
    fn resumption_aliases_require_matching_suspension_captures() {
        let mut allocator = IdAllocator::<AssemblyScope>::new();
        let mut arena = AssemblyArena::default();
        let source = allocator.alloc();
        let alias = allocator.alloc();
        arena.variables.insert_new(source, VarName::from("source"));
        arena.variables.insert_new(alias, VarName::from("capture"));
        let end = allocator.alloc();
        arena.insert_program(end, Program::Terminator(Abort.into()), Context::new());
        let resumed_end = allocator.alloc();
        arena.insert_program(
            resumed_end,
            Program::Terminator(Abort.into()),
            Context::singleton(alias),
        );
        let resume = allocator.alloc();
        arena.insert_program(
            resume,
            Program::Instruction(Push(Atom::Var(alias)).into(), resumed_end),
            Context::singleton(alias),
        );
        arena.frame_entries.insert(resume, Entry::Resume { bindings: vec![(alias, source)] });
        let suspension = allocator.alloc();
        arena.insert_program(
            suspension,
            Program::Instruction(RetainFrame { entry: resume, captures: vec![source] }.into(), end),
            Context::singleton(source),
        );
        let root = allocator.alloc();
        arena.insert_program(
            root,
            Program::Instruction(Pop(source).into(), suspension),
            Context::new(),
        );
        arena.frame_entries.insert(root, Entry::Fresh);

        let plan = FramePlan::analyze(&arena, root).unwrap();
        assert_eq!(plan.slots[&alias], plan.slots[&source]);
        assert_eq!(plan.layouts[0].words, 1);
        assert_eq!(plan.live[&resume], [0]);
        arena
            .programs
            .replace_existing(resume, Program::Instruction(Pop(alias).into(), resumed_end));
        assert_eq!(
            FramePlan::analyze(&arena, root).err(),
            Some(FramePlanError::Alias { variable: alias })
        );
        arena.programs.replace_existing(
            resume,
            Program::Instruction(Push(Atom::Var(alias)).into(), resumed_end),
        );
        arena.programs.replace_existing(
            suspension,
            Program::Instruction(RetainFrame { entry: resume, captures: vec![] }.into(), end),
        );
        assert_eq!(
            FramePlan::analyze(&arena, root).err(),
            Some(FramePlanError::ResumptionContext { program: suspension })
        );
        arena.programs.replace_existing(suspension, Program::Terminator(Jump(end).into()));
        assert_eq!(
            FramePlan::analyze(&arena, root).err(),
            Some(FramePlanError::MissingOwner { program: resume })
        );
    }
}
