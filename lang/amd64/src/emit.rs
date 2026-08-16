use super::syntax::*;
use derive_more::{AsMut, AsRef};
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use zydeco_assembly::{
    arena::{AssemblyArena, AssemblyArenaRefLike, AssemblyProgram},
    gc::{self as asm_gc, GcRootMap},
    syntax::{
        self as sa, Atom, FieldClass, Instruction, Intrinsic, ProgId, Program, Symbol, Terminator,
    },
};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_syntax::*;
use zydeco_utils::pass::CompilerPass;

pub const ENV_REG: Reg = Reg::Rbp;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetFormat {
    Elf,
    MachO,
}

/// Alignment of `rsp` at the current assembly position.
///
/// The SysV amd64 ABI requires `rsp % 16 == 0` immediately before a `call`.
/// Function entries receive `rsp % 16 == 8`, and every emitted push/pop flips
/// the parity. [`StackParity::Unknown`] marks positions whose parity depends on
/// a dynamically chosen continuation, where calls are aligned at runtime.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
enum StackParity {
    /// `rsp % 16 == 0`
    Aligned,
    /// `rsp % 16 == 8`
    Misaligned,
    /// Reachable with either parity; align each call dynamically.
    #[default]
    Unknown,
}

impl StackParity {
    fn flip(self) -> Self {
        match self {
            | Self::Aligned => Self::Misaligned,
            | Self::Misaligned => Self::Aligned,
            | Self::Unknown => Self::Unknown,
        }
    }
}

pub trait Emit<'a> {
    type Env;
    fn emit(&self, env: Self::Env, em: &mut Emitter);
}

#[derive(AsRef, AsMut)]
pub struct Emitter<'e> {
    pub spans: &'e SpanArena,
    pub scoped: &'e ScopedArena,
    pub statics: &'e StaticsArena,
    pub assembly: &'e AssemblyArena,
    pub root: ProgId,

    #[as_ref]
    #[as_mut]
    pub asm: AsmFile,

    target_format: TargetFormat,
    tables: Vec<JumpTable>,
    visited: HashSet<ProgId>,
    gc_maps: HashMap<ProgId, GcRootMap>,
    stack_parity: StackParity,
    entry_parities: HashMap<ProgId, StackParity>,
    dynamic_entries: HashSet<ProgId>,
}

impl<'e> Emitter<'e> {
    pub fn new(
        spans: &'e SpanArena, scoped: &'e ScopedArena, statics: &'e StaticsArena,
        assembly: &'e AssemblyProgram, target_format: TargetFormat,
    ) -> Self {
        let entry_parities = Self::compute_entry_parities(&assembly.arena, assembly.root);
        let dynamic_entries = Self::compute_dynamic_entries(&assembly.arena);
        let gc_maps = assembly
            .layouts
            .iter()
            .map(|(program, layout)| {
                (*program, asm_gc::root_map(&assembly.arena, layout, &assembly.slots))
            })
            .collect();
        Self {
            spans,
            scoped,
            statics,
            assembly: &assembly.arena,
            root: assembly.root,
            asm: AsmFile::default(),
            target_format,
            tables: Vec::new(),
            visited: HashSet::new(),
            gc_maps,
            stack_parity: entry_parities
                .get(&assembly.root)
                .copied()
                .unwrap_or(StackParity::Unknown),
            entry_parities,
            dynamic_entries,
        }
    }

    /// Propagate the known `rsp` parity at function entry through the program graph.
    ///
    /// The root program is entered by a SysV call, so its parity is
    /// [`StackParity::Misaligned`]. Each program edge applies the stack effect of
    /// the instruction or terminator that connects it to its successors. Programs
    /// that are only reachable through dynamic continuations have no single
    /// statically known entry parity and stay [`StackParity::Unknown`].
    fn compute_entry_parities(
        assembly: &AssemblyArena, root: ProgId,
    ) -> HashMap<ProgId, StackParity> {
        const ODD: u8 = 0b01;
        const EVEN: u8 = 0b10;

        fn flip_mask(mask: u8) -> u8 {
            ((mask & ODD) << 1) | ((mask & EVEN) >> 1)
        }

        fn merge(
            parities: &mut HashMap<ProgId, u8>, queue: &mut VecDeque<ProgId>, target: ProgId,
            mask: u8,
        ) {
            let merged = parities.get(&target).copied().unwrap_or_default() | mask;
            if parities.insert(target, merged) != Some(merged) {
                queue.push_back(target);
            }
        }

        let mut parities = HashMap::new();
        let mut queue = VecDeque::new();
        parities.insert(root, ODD);
        queue.push_back(root);

        while let Some(prog_id) = queue.pop_front() {
            let Some(&mask) = parities.get(&prog_id) else { continue };
            match &assembly.programs[&prog_id] {
                | Program::Instruction(instruction, next) => {
                    let mask = if Self::instruction_flips_stack(instruction) {
                        flip_mask(mask)
                    } else {
                        mask
                    };
                    merge(&mut parities, &mut queue, *next, mask);
                }
                | Program::Terminator(terminator) => match terminator {
                    | Terminator::Jump(sa::Jump(target)) => {
                        merge(&mut parities, &mut queue, *target, mask);
                    }
                    | Terminator::PopBranch(sa::PopBranch(arms)) => {
                        let mask = flip_mask(mask);
                        for (_, target) in arms {
                            merge(&mut parities, &mut queue, *target, mask);
                        }
                    }
                    | Terminator::PopJump(_) | Terminator::Extern(_) | Terminator::Abort(_) => {}
                },
            }
        }

        parities
            .into_iter()
            .map(|(prog_id, mask)| {
                let parity = match mask {
                    | ODD => StackParity::Misaligned,
                    | EVEN => StackParity::Aligned,
                    | _ => StackParity::Unknown,
                };
                (prog_id, parity)
            })
            .collect()
    }

    fn instruction_flips_stack(instruction: &Instruction) -> bool {
        match instruction {
            | Instruction::PackProduct(sa::Pack(layout))
            | Instruction::UnpackProduct(sa::Unpack(layout)) => layout.elements % 2 == 0,
            | Instruction::PushArg(_) | Instruction::PushTag(_) | Instruction::PopArg(_) => true,
            | Instruction::Intrinsic(Intrinsic { arity, .. }) => arity % 2 == 0,
            | Instruction::AllocContext(_) | Instruction::Clear(_) => false,
        }
    }

    /// Collect program labels that are pushed as code addresses and therefore
    /// can be entered through a dynamic `popjmp` with unknown parity.
    fn compute_dynamic_entries(assembly: &AssemblyArena) -> HashSet<ProgId> {
        let mut dynamic_entries = HashSet::new();
        for (_, program) in &assembly.programs {
            if let Program::Instruction(Instruction::PushArg(sa::Push(Atom::Sym(sym_id))), _) =
                program
                && let Symbol::Prog(prog_id) = assembly.symbols[sym_id].inner
            {
                dynamic_entries.insert(prog_id);
            }
        }
        dynamic_entries
    }

    /// Apply the net stack effect of a batch of emitted stack operations.
    ///
    /// Only the parity matters for ABI alignment, so an even number of moved
    /// words leaves [`Self::stack_parity`] unchanged.
    fn shift_stack_parity(&mut self, words: i64) {
        if words % 2 != 0 {
            self.stack_parity = self.stack_parity.flip();
        }
    }

    /// Check in debug builds that a static edge reaches `target` with the
    /// parity propagated by [`Self::compute_entry_parities`].
    fn debug_assert_edge_parity(&self, target: ProgId) {
        if self.dynamic_entries.contains(&target) {
            return;
        }
        if let Some(&target_parity) = self.entry_parities.get(&target)
            && self.stack_parity != StackParity::Unknown
            && target_parity != StackParity::Unknown
        {
            debug_assert_eq!(
                self.stack_parity,
                target_parity,
                "static stack-parity analysis disagrees with emission for {}",
                target.concise_inner()
            );
        }
    }

    /// Emit a `call` that satisfies the SysV ABI at its entry.
    ///
    /// When the current parity is statically known, one balanced `sub`/`add`
    /// pair fixes it. For dynamic continuations with unknown parity, save the
    /// original `rsp` below the aligned stack, call, and restore it afterwards.
    fn emit_aligned_call(&mut self, target: JmpArgs) {
        match self.stack_parity {
            | StackParity::Aligned => self.asm.text.push(Instr::Call(target)),
            | StackParity::Misaligned => {
                self.asm.text.extend([
                    Instr::Comment("pad the stack for the SysV call".to_string()),
                    Instr::Sub(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8))),
                    Instr::Call(target),
                    Instr::Add(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8))),
                ]);
            }
            | StackParity::Unknown => {
                self.asm.text.extend([
                    Instr::Comment(
                        "align rsp for the host call and restore it afterwards".to_string(),
                    ),
                    Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::Rsp))),
                    Instr::And(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(-16))),
                    Instr::Sub(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(16))),
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: Reg::Rsp, offset: 0 },
                        Reg32::Reg(Reg::Rax),
                    )),
                    Instr::Call(target),
                    Instr::Mov(MovArgs::ToReg(
                        Reg::Rcx,
                        Arg64::Mem(MemRef { reg: Reg::Rsp, offset: 0 }),
                    )),
                    Instr::Mov(MovArgs::ToReg(Reg::Rsp, Arg64::Reg(Reg::Rcx))),
                ]);
            }
        }
    }

    fn gc_label(&self, id: ProgId, kind: &str) -> String {
        format!("{kind}_{}", id.concise_inner().replace('#', "_"))
    }

    /// Append the root map and object descriptor for one allocation site to
    /// `.rodata`, returning the labels that address them.
    fn emit_gc_metadata(&mut self, id: ProgId, fields: &[FieldClass]) -> (String, String) {
        let map = self.gc_maps.get(&id).expect("allocation site lacks a GC root map");
        let map_label = self.gc_label(id, "gc_map");
        let descriptor_label = self.gc_label(id, "gc_descriptor");
        self.asm.rodata.extend([
            Instr::Comment(format!("GC root map for {}", id.concise())),
            Instr::Label(map_label.clone()),
            Instr::Db(ByteSequence(map.encode())),
            Instr::Comment(format!("GC object descriptor for {}", id.concise())),
            Instr::Label(descriptor_label.clone()),
            Instr::Db(ByteSequence(asm_gc::descriptor_bytes(fields))),
        ]);
        (map_label, descriptor_label)
    }

    /// Call `zydeco_gc_alloc(size, descriptor, map, caller_rsp, rbp)`.
    ///
    /// `rcx` captures the caller `rsp` *before* the alignment fixups of
    /// [`Self::emit_aligned_call`], so root-map offsets stay relative to the
    /// untouched control stack.
    fn emit_gc_alloc_call(&mut self, size_words: usize, map_label: &str, descriptor_label: &str) {
        self.asm.text.extend([
            Instr::Comment("allocate a GC-managed product cell".to_string()),
            Instr::Mov(MovArgs::ToReg(
                Reg::Rdi,
                Arg64::Unsigned(u64::try_from(size_words).expect("product arity overflow")),
            )),
            Instr::Lea(
                Reg::Rsi,
                LeaArgs::RelLabel(RelLabel { label: descriptor_label.to_string(), offset: None }),
            ),
            Instr::Lea(
                Reg::Rdx,
                LeaArgs::RelLabel(RelLabel { label: map_label.to_string(), offset: None }),
            ),
            Instr::Mov(MovArgs::ToReg(Reg::Rcx, Arg64::Reg(Reg::Rsp))),
            Instr::Mov(MovArgs::ToReg(Reg::R8, Arg64::Reg(Reg::Rbp))),
        ]);
        self.emit_aligned_call(JmpArgs::Label("zydeco_gc_alloc".to_string()));
    }
}

impl<'e> CompilerPass for Emitter<'e> {
    type Arena = AsmFile;
    type Out = AsmFile;
    type Error = std::convert::Infallible;
    fn run(mut self) -> Result<Self::Out, Self::Error> {
        self.asm.text.extend([
            // zydeco_abort
            Instr::Extern("zydeco_abort".to_string()),
            // zydeco_gc_alloc
            Instr::Extern("zydeco_gc_alloc".to_string()),
            // host callback used by runtime-created argument-fold thunks
            Instr::Extern("zydeco_arg_fold_resume".to_string()),
            // construct an owned host string from static UTF-8 bytes
            Instr::Extern("zydeco_string_literal".to_string()),
        ]);

        // Emit the externs
        for sa::Extern { name, .. } in self.assembly.externs.iter() {
            let label = format!("zydeco_{}", name);
            self.asm.text.push(Instr::Extern(label));
        }

        // Emit host-to-Zydeco resumption bridges. Each bridge is entered by a
        // SysV call when Rust invokes it, so its entry parity is misaligned;
        // it pushes arguments before tail-jumping into Zydeco code.
        self.stack_parity = StackParity::Misaligned;
        self.asm.text.extend([
            Instr::Global("rust_resume_zydeco_0".to_string()),
            Instr::Label("rust_resume_zydeco_0".to_string()),
            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(MemRef { reg: Reg::Rdi, offset: 8 }))),
            Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))),
            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 8 }))),
            Instr::Push(Arg32::Reg(Reg::Rsi)),
            Instr::Jmp(JmpArgs::Reg(Reg::Rax)),
        ]);
        self.shift_stack_parity(1);

        self.stack_parity = StackParity::Misaligned;
        self.asm.text.extend([
            Instr::Global("rust_resume_zydeco_1".to_string()),
            Instr::Label("rust_resume_zydeco_1".to_string()),
            Instr::Mov(MovArgs::ToReg(Reg::Rdx, Arg64::Mem(MemRef { reg: Reg::Rdi, offset: 16 }))),
            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(MemRef { reg: Reg::Rdi, offset: 8 }))),
            Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))),
            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 8 }))),
            Instr::Push(Arg32::Reg(Reg::Rdx)),
            Instr::Push(Arg32::Reg(Reg::Rsi)),
            Instr::Jmp(JmpArgs::Reg(Reg::Rax)),
        ]);
        self.shift_stack_parity(2);

        self.stack_parity = StackParity::Misaligned;
        self.asm.text.extend([
            Instr::Global("rust_resume_zydeco_2".to_string()),
            Instr::Label("rust_resume_zydeco_2".to_string()),
            Instr::Mov(MovArgs::ToReg(Reg::Rdx, Arg64::Mem(MemRef { reg: Reg::Rdi, offset: 16 }))),
            Instr::Mov(MovArgs::ToReg(Reg::Rcx, Arg64::Mem(MemRef { reg: Reg::Rdi, offset: 24 }))),
            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(MemRef { reg: Reg::Rdi, offset: 8 }))),
            Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))),
            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 8 }))),
            Instr::Push(Arg32::Reg(Reg::Rcx)),
            Instr::Push(Arg32::Reg(Reg::Rdx)),
            Instr::Push(Arg32::Reg(Reg::Rsi)),
            Instr::Jmp(JmpArgs::Reg(Reg::Rax)),
        ]);
        self.shift_stack_parity(3);

        // This tail is reached by a jump through a runtime-created closure, so
        // its entry parity is not statically known.
        self.stack_parity = StackParity::Unknown;
        self.asm.text.extend([
            Instr::Global("rust_arg_fold_tail".to_string()),
            Instr::Label("rust_arg_fold_tail".to_string()),
            Instr::Comment("pass the runtime-created thunk environment to Rust".to_string()),
            Instr::Pop(Loc::Reg(Reg::Rdi)),
        ]);
        self.shift_stack_parity(-1);
        self.emit_aligned_call(JmpArgs::Label("zydeco_arg_fold_resume".to_string()));
        self.asm.text.extend([
            Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Reg(Reg::Rax))),
            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem(MemRef { reg: Reg::Rdi, offset: 0 }))),
            Instr::Jmp(JmpArgs::Reg(Reg::Rax)),
        ]);

        self.stack_parity =
            self.entry_parities.get(&self.root).copied().unwrap_or(StackParity::Unknown);
        self.asm.text.extend([
            Instr::Global("entry".to_string()),
            Instr::Label("entry".to_string()),
            Instr::Comment("initialize environment".to_string()),
            // initialize the environment
            Instr::Mov(MovArgs::ToReg(ENV_REG, Arg64::Reg(Reg::Rdi))),
        ]);

        let root = self.root;
        root.emit((), &mut self);

        // Emit the named blocks
        for (prog_id, _) in &self.assembly.programs {
            if let Some(label) = self.assembly.prog_label(prog_id) {
                self.stack_parity = if self.dynamic_entries.contains(prog_id) {
                    StackParity::Unknown
                } else {
                    self.entry_parities.get(prog_id).copied().unwrap_or(StackParity::Unknown)
                };
                self.asm.text.push(Instr::Label(label));
                prog_id.emit((), &mut self);
            }
        }

        // Emit the jump tables
        for table in &self.tables {
            let label = table.rodata_label();
            self.asm.rodata.extend([
                Instr::Comment(format!("jump table for {}", table.id.concise_inner())),
                Instr::Label(label.clone()),
            ]);
            for (idx, (name, prog_id)) in table.arms.iter().enumerate() {
                self.asm.rodata.extend([Instr::Comment(format!(
                    "arm {} for {}",
                    name.clone().unwrap_or_else(|| format!("#{}", idx)),
                    prog_id.concise()
                ))]);
                let arm_label = self.assembly.prog_label(prog_id).expect("block name not found");
                match self.target_format {
                    | TargetFormat::Elf => {
                        // Store relative offset from current entry to target for PIC.
                        self.asm.rodata.push(Instr::Dq(format!("{} - $", arm_label)));
                    }
                    | TargetFormat::MachO => {
                        // Store the absolute label; Mach-O relocates the pointer.
                        self.asm.rodata.push(Instr::Dq(arm_label));
                    }
                }
            }
        }

        let string_literals = self
            .assembly
            .symbols
            .iter()
            .filter_map(|(symbol, named)| match &named.inner {
                | Symbol::StringLiteral(characters) => {
                    Some(NativeStringLiteral::new(self.assembly.sym_label(symbol), characters))
                }
                | Symbol::Undefined(_) | Symbol::Prog(_) => None,
            })
            .flat_map(NativeStringLiteral::declaration)
            .collect::<Vec<_>>();
        self.asm.rodata.extend(string_literals);

        Ok(self.asm)
    }
}

struct JumpTable {
    id: ProgId,
    arms: Vec<(Option<String>, ProgId)>,
}

struct NativeStringLiteral {
    label: String,
    bytes: Vec<u8>,
}

impl NativeStringLiteral {
    fn new(label: String, string: &Utf8String) -> Self {
        let bytes = string.as_bytes().to_vec();
        Self { label, bytes }
    }

    fn length(&self) -> usize {
        self.bytes.len()
    }

    fn declaration(self) -> [Instr; 3] {
        let storage = if self.bytes.is_empty() { vec![0] } else { self.bytes };
        [
            Instr::Comment("UTF-8 string literal".to_string()),
            Instr::Label(self.label),
            Instr::Db(ByteSequence(storage)),
        ]
    }
}
impl JumpTable {
    fn rodata_label(&self) -> String {
        format!("jump_table_{}", self.id.concise_inner().replace('#', "_"))
    }
}

impl<'a> Emit<'a> for ProgId {
    type Env = ();
    fn emit(&self, (): Self::Env, em: &mut Emitter) {
        // Avoid infinite loops
        assert!(!em.visited.contains(self), "infinite loop detected");
        em.visited.insert(*self);

        // Emit the program
        match &em.assembly.programs[self] {
            | Program::Terminator(terminator) => terminator.emit(*self, em),
            | Program::Instruction(instr, next) => {
                instr.emit(*self, em);
                next.emit((), em);
            }
        }
    }
}

impl<'a> Emit<'a> for Terminator {
    type Env = ProgId;
    fn emit(&self, id: Self::Env, em: &mut Emitter) {
        match self {
            | Terminator::Jump(sa::Jump(target)) => {
                match em.assembly.prog_label(target) {
                    | Some(label) => {
                        // if the target is a named block, then jump to the label
                        em.debug_assert_edge_parity(*target);
                        em.asm.text.push(Instr::Jmp(JmpArgs::Label(label)));
                    }
                    | None => {
                        // otherwise, directly emit the target program
                        em.asm.text.push(Instr::Comment(format!(
                            "inlined jump to {}",
                            target.concise_inner()
                        )));
                        target.emit((), em);
                    }
                }
            }
            | Terminator::PopJump(sa::PopJump) => {
                // pop value and jump to it
                em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                em.shift_stack_parity(-1);
                em.asm.text.push(Instr::Jmp(JmpArgs::Reg(Reg::Rax)));
            }
            | Terminator::PopBranch(sa::PopBranch(arms)) => {
                // pop tag and jump to the corresponding program
                em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                em.shift_stack_parity(-1);
                for (_, target) in arms {
                    em.debug_assert_edge_parity(*target);
                }
                // register the jump table
                let sorted_arms: BTreeMap<_, _> = arms
                    .iter()
                    .map(|(sa::Tag { idx, name }, prog_id)| (idx, (name, prog_id)))
                    .collect();
                let table = JumpTable {
                    id,
                    arms: sorted_arms
                        .into_iter()
                        .map(|(_, (name, prog_id))| (name.clone(), *prog_id))
                        .collect(),
                };
                let label = table.rodata_label();
                em.tables.push(table);
                // emit jump to the jump table arm
                // Mach-O doesn't support [rel label + reg * scale], so we need:
                // 1. lea rcx, [rel jump_table] - load jump table base address
                // 2. lea rcx, [rcx + rax * 8] - compute address of table entry
                // 3. mov rax, [rcx] - load entry payload (offset for ELF, address for Mach-O)
                em.asm.text.push(Instr::Lea(
                    Reg::Rcx,
                    LeaArgs::RelLabel(RelLabel { label, offset: None }),
                ));
                em.asm.text.push(Instr::Lea(
                    Reg::Rcx,
                    LeaArgs::Displace {
                        base: Reg::Rcx,
                        scaled_index: Some((Reg::Rax, 8)),
                        offset: None,
                    },
                ));
                em.asm.text.push(Instr::Mov(MovArgs::ToReg(
                    Reg::Rax,
                    Arg64::Mem(MemRef { reg: Reg::Rcx, offset: 0 }),
                )));
                match em.target_format {
                    | TargetFormat::Elf => {
                        // table entry is a relative offset from the entry address
                        em.asm
                            .text
                            .push(Instr::Add(BinArgs::ToReg(Reg::Rcx, Arg32::Reg(Reg::Rax))));
                        em.asm.text.push(Instr::Jmp(JmpArgs::Reg(Reg::Rcx)));
                    }
                    | TargetFormat::MachO => {
                        // table entry is the absolute target address
                        em.asm.text.push(Instr::Jmp(JmpArgs::Reg(Reg::Rax)));
                    }
                }
            }
            | Terminator::Extern(sa::Extern { name, arity, mode }) => {
                em.asm.text.push(Instr::Comment(format!("extern: {}/{}", name, arity)));

                let zydeco_extern_name = format!("zydeco_{}", name);
                for i in 1..=*arity {
                    // place the arguments accordingly
                    // using system V AMD64 ABI
                    if i <= 6 {
                        let reg = match i {
                            | 1 => Reg::Rdi,
                            | 2 => Reg::Rsi,
                            | 3 => Reg::Rdx,
                            | 4 => Reg::Rcx,
                            | 5 => Reg::R8,
                            | 6 => Reg::R9,
                            | _ => unreachable!(),
                        };
                        em.asm.text.push(Instr::Pop(Loc::Reg(reg)));
                    } else {
                        // load to stack - but it's already on the stack
                        // we just need to make sure the position is correct
                        todo!()
                    }
                }
                em.shift_stack_parity(-i64::try_from(*arity).expect("extern arity overflow"));
                // All externs must be non-tail called so that we can restore the
                // alignment padding from the stack.
                em.emit_aligned_call(JmpArgs::Label(zydeco_extern_name));
                match mode {
                    | sa::ExternMode::Returning => {
                        em.asm.text.extend([
                            Instr::Comment(
                                "return the host result through the current Zydeco continuation"
                                    .to_string(),
                            ),
                            Instr::Mov(MovArgs::ToReg(Reg::Rcx, Arg64::Reg(Reg::Rax))),
                            Instr::Pop(Loc::Reg(Reg::Rax)),
                            Instr::Push(Arg32::Reg(Reg::Rcx)),
                            Instr::Jmp(JmpArgs::Reg(Reg::Rax)),
                        ]);
                        em.shift_stack_parity(-1);
                        em.shift_stack_parity(1);
                    }
                    | sa::ExternMode::Control => {
                        em.asm.text.extend([
                            Instr::Comment(
                                "resume the host-selected Zydeco computation".to_string(),
                            ),
                            Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Reg(Reg::Rax))),
                            Instr::Mov(MovArgs::ToReg(
                                Reg::Rax,
                                Arg64::Mem(MemRef { reg: Reg::Rdi, offset: 0 }),
                            )),
                            Instr::Jmp(JmpArgs::Reg(Reg::Rax)),
                        ]);
                    }
                }
            }
            | Terminator::Abort(sa::Abort) => {
                em.asm.text.push(Instr::Comment("abort".to_string()));
                em.asm.text.push(Instr::Jmp(JmpArgs::Label("zydeco_abort".to_string())));
            }
        }
    }
}

impl<'a> Emit<'a> for Instruction {
    type Env = ProgId;
    fn emit(&self, id: Self::Env, em: &mut Emitter) {
        match self {
            | Instruction::PackProduct(sa::Pack(layout)) => {
                em.asm.text.push(Instr::Comment(format!(
                    "pack_product {}/{}",
                    layout.elements, layout.arity
                )));
                let (map_label, descriptor_label) = em.emit_gc_metadata(id, &layout.fields);
                em.emit_gc_alloc_call(layout.arity, &map_label, &descriptor_label);
                for index in 0..layout.elements {
                    let destination = i32::try_from(index * 8).expect("product offset overflow");
                    if index + 1 == layout.elements && layout.elements < layout.arity {
                        em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rdx)));
                        for field in index..layout.arity {
                            let source = i32::try_from((field - index) * 8)
                                .expect("product offset overflow");
                            let destination =
                                i32::try_from(field * 8).expect("product offset overflow");
                            em.asm.text.extend([
                                Instr::Mov(MovArgs::ToReg(
                                    Reg::Rcx,
                                    Arg64::Mem(MemRef { reg: Reg::Rdx, offset: source }),
                                )),
                                Instr::Mov(MovArgs::ToMem(
                                    MemRef { reg: Reg::Rax, offset: destination },
                                    Reg32::Reg(Reg::Rcx),
                                )),
                            ]);
                        }
                    } else {
                        em.asm.text.extend([
                            Instr::Pop(Loc::Reg(Reg::Rcx)),
                            Instr::Mov(MovArgs::ToMem(
                                MemRef { reg: Reg::Rax, offset: destination },
                                Reg32::Reg(Reg::Rcx),
                            )),
                        ]);
                    }
                }
                em.shift_stack_parity(
                    -i64::try_from(layout.elements).expect("product elements overflow"),
                );
                em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
                em.shift_stack_parity(1);
            }
            | Instruction::UnpackProduct(sa::Unpack(layout)) => {
                em.asm.text.push(Instr::Comment(format!(
                    "unpack_product {}/{}",
                    layout.elements, layout.arity
                )));
                em.asm.text.push(Instr::Pop(Loc::Reg(Reg::Rax)));
                let last = layout.elements - 1;
                let last_offset = i32::try_from(last * 8).expect("product offset overflow");
                if layout.elements < layout.arity {
                    em.asm.text.extend([
                        Instr::Lea(
                            Reg::Rcx,
                            LeaArgs::Displace {
                                base: Reg::Rax,
                                scaled_index: None,
                                offset: Some(last_offset),
                            },
                        ),
                        Instr::Push(Arg32::Reg(Reg::Rcx)),
                    ]);
                } else {
                    em.asm.text.extend([
                        Instr::Mov(MovArgs::ToReg(
                            Reg::Rcx,
                            Arg64::Mem(MemRef { reg: Reg::Rax, offset: last_offset }),
                        )),
                        Instr::Push(Arg32::Reg(Reg::Rcx)),
                    ]);
                }
                for index in (0..last).rev() {
                    let offset = i32::try_from(index * 8).expect("product offset overflow");
                    em.asm.text.extend([
                        Instr::Mov(MovArgs::ToReg(
                            Reg::Rcx,
                            Arg64::Mem(MemRef { reg: Reg::Rax, offset }),
                        )),
                        Instr::Push(Arg32::Reg(Reg::Rcx)),
                    ]);
                }
                em.shift_stack_parity(
                    i64::try_from(layout.elements).expect("product elements overflow") - 1,
                );
            }
            | Instruction::AllocContext(sa::Alloc(sa::ContextMarker)) => {
                // Allocate new context
                let frame = em.assembly.contexts[&id].iter().len() as i32;
                em.asm.text.extend([
                    Instr::Comment("alloc_context".to_string()),
                    Instr::Add(BinArgs::ToReg(Reg::Rbp, Arg32::Signed(8 * frame))),
                ]);
            }
            | Instruction::PushArg(sa::Push(atom)) => {
                // Push argument onto stack
                atom.emit(id, em);
            }
            | Instruction::PopArg(sa::Pop(var_id)) => {
                // Pop argument from stack into variable
                let var_name = &em.assembly.variables[var_id];
                let idx = em.assembly.contexts[&id].iter().len() as i32;
                em.asm.text.extend([
                    Instr::Comment(format!("pop_arg {}{}", var_name.plain(), var_id.concise())),
                    // pop from stack
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    // store to [rbp + 8 * idx]
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: ENV_REG, offset: 8 * idx },
                        Reg32::Reg(Reg::Rax),
                    )),
                ]);
                em.shift_stack_parity(-1);
            }
            | Instruction::PushTag(sa::Push(tag)) => {
                // Push tag onto stack
                em.asm.text.extend([
                    Instr::Comment(format!("push_tag {}", tag.idx)),
                    // push tag to stack
                    Instr::Push(Arg32::Signed(tag.idx as i32)),
                ]);
                em.shift_stack_parity(1);
            }
            | Instruction::Intrinsic(intrinsic) => {
                intrinsic.emit(id, em);
            }
            | Instruction::Clear(_) => {
                // Clear variables from context
                em.asm.text.push(Instr::Comment("clear".to_string()));
                // TODO: Implement context clearing
                todo!()
            }
        }
    }
}

impl<'a> Emit<'a> for Atom {
    type Env = ProgId;
    fn emit(&self, id: Self::Env, em: &mut Emitter) {
        match self {
            | Atom::Var(var_id) => {
                let var_name = &em.assembly.variables[var_id];
                em.asm.text.push(Instr::Comment(format!(
                    "push_var {}{}",
                    var_name.plain(),
                    var_id.concise()
                )));
                let idx = em.assembly.contexts[&id]
                    .iter()
                    .position(|var| var == var_id)
                    .expect("variable not found") as i32;
                // load [rbp + 8 * idx] and push
                em.asm.text.extend([
                    Instr::Mov(MovArgs::ToReg(
                        Reg::Rax,
                        Arg64::Mem(MemRef { reg: ENV_REG, offset: 8 * idx }),
                    )),
                    Instr::Push(Arg32::Reg(Reg::Rax)),
                ]);
                em.shift_stack_parity(1);
            }
            | Atom::Sym(sym_id) => {
                let symbol = &em.assembly.symbols[sym_id];
                match symbol.inner.clone() {
                    | Symbol::Prog(prog_id) => {
                        em.asm.text.push(Instr::Comment(format!(
                            "push_sym_prog {}{}",
                            symbol.name.clone(),
                            sym_id.concise()
                        )));
                        // push the program id
                        let label = em.assembly.prog_label(&prog_id).expect("block name not found");
                        em.asm.text.extend([
                            Instr::Lea(
                                Reg::Rax,
                                LeaArgs::RelLabel(RelLabel { label, offset: None }),
                            ),
                            Instr::Push(Arg32::Reg(Reg::Rax)),
                        ]);
                        em.shift_stack_parity(1);
                    }
                    | Symbol::Undefined(sa::Undefined) => {
                        unreachable!("undefined symbol should never be emitted")
                    }
                    | Symbol::StringLiteral(s) => {
                        em.asm.text.push(Instr::Comment(format!("push_sym_str {:?}", s)));
                        let literal = NativeStringLiteral::new(em.assembly.sym_label(sym_id), &s);
                        let length = literal.length();
                        em.asm.text.extend([
                            Instr::Lea(
                                Reg::Rdi,
                                LeaArgs::RelLabel(RelLabel { label: literal.label, offset: None }),
                            ),
                            Instr::Mov(MovArgs::ToReg(
                                Reg::Rsi,
                                Arg64::Unsigned(
                                    u64::try_from(length).expect("string literal length overflow"),
                                ),
                            )),
                        ]);
                        em.emit_aligned_call(JmpArgs::Label("zydeco_string_literal".to_string()));
                        em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
                        em.shift_stack_parity(1);
                    }
                }
            }
            | Atom::Imm(imm) => match imm.clone() {
                | sa::Imm::Triv(Triv) => {
                    em.asm.text.push(Instr::Comment("push_imm_triv".to_string()));
                    em.asm.text.push(Instr::Push(Arg32::Signed(0)));
                    em.shift_stack_parity(1);
                }
                | sa::Imm::Integer(i) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_integer {:?}", i)));
                    em.asm.text.extend([
                        Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(i.to_word_bits()))),
                        Instr::Push(Arg32::Reg(Reg::Rax)),
                    ]);
                    em.shift_stack_parity(1);
                }
                | sa::Imm::Float(value) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_float {:?}", value)));
                    em.asm.text.extend([
                        Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(value.to_bits()))),
                        Instr::Push(Arg32::Reg(Reg::Rax)),
                    ]);
                    em.shift_stack_parity(1);
                }
                | sa::Imm::Char(c) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_char {:?}", c)));
                    em.asm.text.extend([Instr::Push(Arg32::Signed(c as i32))]);
                    em.shift_stack_parity(1);
                }
            },
        }
    }
}

impl<'a> Emit<'a> for Intrinsic {
    type Env = ProgId;
    fn emit(&self, id: Self::Env, em: &mut Emitter) {
        let Intrinsic { name, arity } = self;

        fn emit_ba(op: fn(BinArgs) -> Instr, em: &mut Emitter) {
            em.asm.text.push(op(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rcx))));
        }
        fn emit_compare(cc: ConditionCode, id: ProgId, em: &mut Emitter) {
            // Allocate the two-word boolean cell while both operands are still
            // on the control stack, so the safepoint map matches `layouts[id]`.
            let (map_label, descriptor_label) =
                em.emit_gc_metadata(id, &[FieldClass::Scalar, FieldClass::Scalar]);
            em.emit_gc_alloc_call(2, &map_label, &descriptor_label);
            // Save the fresh cell and evaluate the comparison.
            em.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::Rdx, Arg64::Reg(Reg::Rax))));
            em.asm.text.extend([Instr::Pop(Loc::Reg(Reg::Rax)), Instr::Pop(Loc::Reg(Reg::Rcx))]);
            em.shift_stack_parity(-2);
            emit_ba(Instr::Cmp, em);
            em.asm.text.extend([
                Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Signed(0))),
                Instr::SetCC(cc, Reg8::Al),
                Instr::Mov(MovArgs::ToMem(
                    MemRef { reg: Reg::Rdx, offset: 0 },
                    Reg32::Reg(Reg::Rax),
                )),
                Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::Rdx, offset: 8 }, Reg32::Imm(0))),
                Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::Rdx))),
            ]);
        }

        match (name.as_str(), arity) {
            | (_, 2) if matches!(name.as_str(), "int_eq" | "int_lt" | "int_gt") => {
                let cc = match name.as_str() {
                    | "int_eq" => ConditionCode::E,
                    | "int_lt" => ConditionCode::L,
                    | "int_gt" => ConditionCode::G,
                    | _ => unreachable!("matched comparison intrinsic"),
                };
                emit_compare(cc, id, em);
                em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
                em.shift_stack_parity(1);
            }
            | (_, 2) => {
                em.asm
                    .text
                    .extend([Instr::Pop(Loc::Reg(Reg::Rax)), Instr::Pop(Loc::Reg(Reg::Rcx))]);
                em.shift_stack_parity(-2);
                match name.as_str() {
                    | "add" => {
                        emit_ba(Instr::Add, em);
                    }
                    | "sub" => {
                        emit_ba(Instr::Sub, em);
                    }
                    | "mul" => {
                        emit_ba(Instr::IMul, em);
                    }
                    | "div" => {
                        em.asm.text.extend([Instr::Cqo, Instr::IDiv(Reg::Rcx)]);
                    }
                    | "mod" => {
                        em.asm.text.extend([
                            Instr::Cqo,
                            Instr::IDiv(Reg::Rcx),
                            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::Rdx))),
                        ]);
                    }
                    | "and" => {
                        emit_ba(Instr::And, em);
                    }
                    | "or" => {
                        emit_ba(Instr::Or, em);
                    }
                    | "xor" => {
                        emit_ba(Instr::Xor, em);
                    }
                    | _ => {
                        unimplemented!("intrinsic {} with arity {} not implemented", name, arity)
                    }
                }
                em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
                em.shift_stack_parity(1);
            }
            | _ => unimplemented!("intrinsic {} with arity {} not implemented", name, arity),
        }
    }
}
