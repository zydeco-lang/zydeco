mod primitive;

use super::syntax::*;
use derive_more::{AsMut, AsRef};
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use zydeco_assembly::frames::{Entry, FramePlan, NativeProgram};
use zydeco_assembly::{
    arena::{AssemblyArena, AssemblyArenaRefLike},
    syntax::{self as sa, Atom, Instruction, ProgId, Program, Symbol, Terminator},
};
use zydeco_machine::frames::{Action, STEP_SYMBOL};
use zydeco_machine::native::{
    AllocationKind, ClosureField, ENTRY_SYMBOL, ResumeArity, TransferField, WORD_BYTES,
};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_syntax::*;

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
    pub frames: &'e FramePlan,
    frame_data: BTreeMap<String, Vec<u64>>,

    #[as_ref]
    #[as_mut]
    pub asm: AsmFile,

    target_format: TargetFormat,
    tables: Vec<JumpTable>,
    visited: HashSet<ProgId>,
    stack_parity: StackParity,
    entry_parities: HashMap<ProgId, StackParity>,
    dynamic_entries: HashSet<ProgId>,
}

impl<'e> Emitter<'e> {
    pub fn new(
        spans: &'e SpanArena, scoped: &'e ScopedArena, statics: &'e StaticsArena,
        native: &'e NativeProgram, target_format: TargetFormat,
    ) -> Self {
        let assembly = native.assembly();
        let arena = assembly.arena();
        let root = assembly.root();
        let entry_parities = Self::compute_entry_parities(arena, root, native.frames());
        let dynamic_entries = Self::compute_dynamic_entries(arena);
        Self {
            spans,
            scoped,
            statics,
            assembly: arena,
            frames: native.frames(),
            frame_data: BTreeMap::new(),
            root,
            asm: AsmFile::default(),
            target_format,
            tables: Vec::new(),
            visited: HashSet::new(),
            stack_parity: entry_parities.get(&root).copied().unwrap_or(StackParity::Unknown),
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
        assembly: &AssemblyArena, root: ProgId, frames: &FramePlan,
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
            let mask = if matches!(frames.entries.get(&prog_id), Some(Entry::Resume { .. })) {
                flip_mask(mask)
            } else {
                mask
            };
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
            | Instruction::PushArg(_)
            | Instruction::PushTag(_)
            | Instruction::PopArg(_)
            | Instruction::RetainFrame(_) => true,
            | Instruction::Primitive(_) => true,
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

    fn argument_register(index: usize) -> Reg {
        match index {
            | 1 => Reg::Rdi,
            | 2 => Reg::Rsi,
            | 3 => Reg::Rdx,
            | 4 => Reg::Rcx,
            | 5 => Reg::R8,
            | 6 => Reg::R9,
            | _ => panic!("SysV register argument index {index} is out of range"),
        }
    }

    fn frame_descriptor(&mut self, label: String, action: Action<u64>, slots: &[usize]) -> String {
        let words =
            action.into_words().into_iter().chain(slots.iter().map(|slot| *slot as u64)).collect();
        self.frame_data.insert(label.clone(), words);
        label
    }

    fn emit_frame_call(&mut self, descriptor: String) {
        self.asm.text.push(Instr::Lea(
            Reg::Rdi,
            LeaArgs::RelLabel(RelLabel { label: descriptor, offset: None }),
        ));
        self.emit_aligned_call(JmpArgs::Label(STEP_SYMBOL.to_string()));
    }

    fn emit_frame_entry(&mut self, id: ProgId) {
        let Some(entry) = self.frames.entries.get(&id) else { return };
        let layout = self.frames.layouts[self.frames.owners[&id].0];
        let action = match entry {
            | Entry::Fresh => Action::enter(layout),
            | Entry::Resume { .. } => {
                // Remove the saved token while keeping the result on the control stack.
                self.asm.text.extend([
                    Instr::Mov(MovArgs::ToReg(
                        Reg::Rsi,
                        Arg64::Mem(MemRef { reg: Reg::Rsp, offset: 8 }),
                    )),
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: Reg::Rsp, offset: 0 },
                        Reg32::Reg(Reg::Rax),
                    )),
                ]);
                self.shift_stack_parity(-1);
                Action::resume(layout.id)
            }
        };
        let descriptor = self.frame_descriptor(
            format!("frame_entry_{}", id.concise_inner().replace('#', "_")),
            action,
            &[],
        );
        self.emit_frame_call(descriptor);
        self.asm.text.push(Instr::Mov(MovArgs::ToReg(ENV_REG, Arg64::Reg(Reg::Rax))));
    }

    /// Allocate one block from the runtime's fixed two-space heap.
    ///
    /// The collector is otherwise runtime-only: it updates the live control-stack
    /// words and the frame slots selected by this descriptor and pending
    /// suspensions. Slot maps establish liveness; word tags identify immediates.
    fn emit_alloc_call(&mut self, size_words: usize, kind: AllocationKind, id: ProgId) {
        let roots = self.frame_descriptor(
            format!("frame_roots_{}", id.concise_inner().replace('#', "_")),
            Action::roots(self.frames.owners[&id], self.frames.live[&id].len()),
            &self.frames.live[&id].clone(),
        );
        self.asm.text.extend([
            Instr::Comment(format!(
                "allocate {} block in the copying heap",
                match kind {
                    | AllocationKind::Scanned => "scanned",
                    | AllocationKind::Opaque => "opaque",
                }
            )),
            Instr::Mov(MovArgs::ToReg(
                Reg::Rdi,
                Arg64::Unsigned(u64::try_from(size_words).expect("product arity overflow")),
            )),
            // Capture the root cursor before `emit_aligned_call` adds any temporary
            // ABI padding beneath it.
            Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Reg(Reg::Rsp))),
            Instr::Lea(Reg::Rdx, LeaArgs::RelLabel(RelLabel { label: roots, offset: None })),
        ]);
        self.emit_aligned_call(JmpArgs::Label(kind.symbol().to_string()));
    }

    fn emit_boxed_bits(&mut self, bits: u64, id: ProgId) {
        self.emit_alloc_call(1, AllocationKind::Opaque, id);
        self.asm.text.extend([
            Instr::Mov(MovArgs::ToReg(Reg::Rcx, Arg64::Unsigned(bits))),
            Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::Rax, offset: 0 }, Reg32::Reg(Reg::Rcx))),
            Instr::Push(Arg32::Reg(Reg::Rax)),
        ]);
        self.shift_stack_parity(1);
    }

    fn foreign_symbol(&self, symbol: &ForeignSymbolName) -> String {
        match self.target_format {
            | TargetFormat::Elf => symbol.to_string(),
            | TargetFormat::MachO => format!("_{symbol}"),
        }
    }

    fn emit_foreign_call(&mut self, id: ProgId, import: &ForeignImport) {
        assert_eq!(import.target.abi, ForeignAbi::C);
        let arguments = import.signature.arguments().collect::<Vec<_>>();
        let scratch_words = arguments.len();
        let scratch_bytes =
            i32::try_from(scratch_words * 8).expect("foreign scratch size overflow");
        self.asm.text.push(Instr::Comment(format!(
            "ffi: {} from -l{}",
            import.target.symbol, import.target.library
        )));

        // Keep source values above a scratch frame until the C call returns. Marshalling
        // helpers never allocate, and the foreign contract forbids reentry into Zydeco:
        // no collection can observe the raw pointers, lengths, or integers in this frame.
        if scratch_words != 0 {
            self.asm.text.push(Instr::Sub(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(scratch_bytes))));
            self.shift_stack_parity(scratch_words as i64);
        }
        for (index, parameter) in import.signature.parameters().iter().enumerate() {
            self.asm.text.push(Instr::Mov(MovArgs::ToReg(
                Reg::Rdi,
                Arg64::Mem(MemRef { reg: Reg::Rsp, offset: scratch_bytes + (index * 8) as i32 }),
            )));
            let helper = match parameter {
                | ForeignParameter::BorrowedMemory => "zydeco_ffi_borrow_memory".to_string(),
                | ForeignParameter::Integer(integer) => {
                    format!("zydeco_ffi_decode_{}", integer.source_name())
                }
            };
            self.emit_aligned_call(JmpArgs::Label(helper));
            self.asm.text.extend(
                arguments
                    .iter()
                    .enumerate()
                    .filter(|(_, argument)| argument.parameter == index)
                    .map(|(slot, argument)| {
                        let register = match argument.component {
                            | ForeignComponent::MemoryPointer | ForeignComponent::Integer(_) => {
                                Reg::Rax
                            }
                        };
                        Instr::Mov(MovArgs::ToMem(
                            MemRef { reg: Reg::Rsp, offset: (slot * 8) as i32 },
                            Reg32::Reg(register),
                        ))
                    }),
            );
        }
        self.asm.text.extend(arguments.iter().enumerate().map(|(slot, _)| {
            Instr::Mov(MovArgs::ToReg(
                Self::argument_register(slot + 1),
                Arg64::Mem(MemRef { reg: Reg::Rsp, offset: (slot * 8) as i32 }),
            ))
        }));
        self.emit_aligned_call(JmpArgs::Label(self.foreign_symbol(&import.target.symbol)));
        self.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::R12, Arg64::Reg(Reg::Rax))));

        // Discard all raw scratch words before the allocation safe point. R12 holds
        // unboxed scalar bits, not a GC root, and is callee-saved across the allocation.
        let consumed_words = scratch_words + import.signature.parameters().len();
        if consumed_words != 0 {
            self.asm.text.push(Instr::Add(BinArgs::ToReg(
                Reg::Rsp,
                Arg32::Signed((consumed_words * 8) as i32),
            )));
            self.shift_stack_parity(-(consumed_words as i64));
        }
        match import.signature.result() {
            | ForeignResult::Unit => {
                self.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Signed(1))));
            }
            | ForeignResult::Integer(integer) => {
                if integer.bits() == 64 {
                    self.emit_alloc_call(1, AllocationKind::Opaque, id);
                    self.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Reg(Reg::Rax))));
                } else {
                    self.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Signed(0))));
                }
                self.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Reg(Reg::R12))));
                self.emit_aligned_call(JmpArgs::Label(format!(
                    "zydeco_ffi_encode_{}",
                    integer.source_name()
                )));
            }
        }
        self.asm.text.extend([
            Instr::Mov(MovArgs::ToReg(Reg::Rcx, Arg64::Reg(Reg::Rax))),
            Instr::Pop(Loc::Reg(Reg::Rax)),
            Instr::Push(Arg32::Reg(Reg::Rcx)),
            Instr::Jmp(JmpArgs::Reg(Reg::Rax)),
        ]);
        self.shift_stack_parity(-1);
        self.shift_stack_parity(1);
    }
}

impl Emitter<'_> {
    pub fn run(mut self) -> AsmFile {
        self.asm.text.extend([
            Instr::Extern(STEP_SYMBOL.to_string()),
            // zydeco_abort
            Instr::Extern("zydeco_abort".to_string()),
            // fixed-heap allocation entry points
            Instr::Extern(AllocationKind::Scanned.symbol().to_string()),
            Instr::Extern(AllocationKind::Opaque.symbol().to_string()),
            Instr::Extern("zydeco_integer_division_by_zero".to_string()),
            Instr::Extern("zydeco_integer_remainder_by_zero".to_string()),
            // construct an owned host string from static UTF-8 bytes
            Instr::Extern("zydeco_string_literal".to_string()),
            // source-to-C marshalling helpers
            Instr::Extern("zydeco_ffi_borrow_memory".to_string()),
        ]);
        for integer in <IntegerType as strum::VariantArray>::VARIANTS {
            for operation in ["decode", "encode"] {
                self.asm.text.push(Instr::Extern(format!(
                    "zydeco_ffi_{operation}_{}",
                    integer.source_name()
                )));
            }
        }

        // Emit the externs. The arena's extern order is not stable across runs, so
        // sort the declarations to keep emitted assembly byte-reproducible.
        let mut externs = self
            .assembly
            .externs
            .iter()
            .map(|external| match external {
                | sa::Extern::Host { name, .. } => format!("zydeco_{name}"),
                | sa::Extern::Foreign(import) => self.foreign_symbol(&import.target.symbol),
            })
            .collect::<Vec<_>>();
        externs.sort();
        self.asm.text.extend(externs.into_iter().map(Instr::Extern));

        // A host call returns a transfer record; the control terminator jumps here.
        // The shared catalog describes consumption order, so push its arguments in reverse.
        for &arity in ResumeArity::ALL {
            self.stack_parity = StackParity::Unknown;
            self.asm.text.extend([
                Instr::Global(arity.symbol().to_string()),
                Instr::Label(arity.symbol().to_string()),
            ]);
            self.asm.text.extend([
                Instr::Mov(MovArgs::ToReg(
                    Reg::Rax,
                    Arg64::Mem(MemRef {
                        reg: Reg::Rdi,
                        offset: TransferField::Closure.offset() as i32,
                    }),
                )),
                Instr::Mov(MovArgs::ToReg(
                    Reg::Rsi,
                    Arg64::Mem(MemRef {
                        reg: Reg::Rax,
                        offset: ClosureField::Environment.offset() as i32,
                    }),
                )),
                Instr::Mov(MovArgs::ToReg(
                    Reg::Rax,
                    Arg64::Mem(MemRef {
                        reg: Reg::Rax,
                        offset: ClosureField::Code.offset() as i32,
                    }),
                )),
            ]);
            self.asm.text.extend(arity.arguments().iter().rev().map(|field| {
                Instr::Push(Arg32::Mem(MemRef { reg: Reg::Rdi, offset: field.offset() as i32 }))
            }));
            self.asm
                .text
                .extend([Instr::Push(Arg32::Reg(Reg::Rsi)), Instr::Jmp(JmpArgs::Reg(Reg::Rax))]);
        }

        self.stack_parity =
            self.entry_parities.get(&self.root).copied().unwrap_or(StackParity::Unknown);
        self.asm.text.extend([
            Instr::Global(ENTRY_SYMBOL.to_string()),
            Instr::Label(ENTRY_SYMBOL.to_string()),
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

        for (label, words) in &self.frame_data {
            self.asm.rodata.push(Instr::Label(label.clone()));
            self.asm.rodata.extend(words.iter().map(|word| Instr::Dq(word.to_string())));
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

        self.asm
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

        em.emit_frame_entry(*self);

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
                em.asm.text.extend([
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    Instr::Shr(ShArgs { reg: Reg::Rax, by: 1 }),
                ]);
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
            | Terminator::Extern(sa::Extern::Host { role, name, arity, mode }) => {
                em.asm.text.push(Instr::Comment(format!("extern: {}/{}", name, arity)));

                let zydeco_extern_name = format!("zydeco_{}", name);
                let spare_box = role.spare_box();
                match spare_box {
                    | Some(SpareBox::Opaque) => {
                        em.emit_alloc_call(1, AllocationKind::Opaque, id);
                        em.asm
                            .text
                            .push(Instr::Mov(MovArgs::ToReg(Reg::R11, Arg64::Reg(Reg::Rax))));
                    }
                    | Some(SpareBox::Unused) => {
                        em.asm.text.push(Instr::Mov(MovArgs::ToReg(Reg::R11, Arg64::Unsigned(0))))
                    }
                    | None => {}
                }
                for i in 1..=*arity {
                    // place the arguments accordingly
                    // using system V AMD64 ABI
                    if i <= 6 {
                        let reg = Emitter::argument_register(i);
                        em.asm.text.push(Instr::Pop(Loc::Reg(reg)));
                    } else {
                        // load to stack - but it's already on the stack
                        // we just need to make sure the position is correct
                        todo!()
                    }
                }
                if spare_box.is_some() {
                    let spare_index = arity + 1;
                    let spare_register = Emitter::argument_register(spare_index);
                    em.asm
                        .text
                        .push(Instr::Mov(MovArgs::ToReg(spare_register, Arg64::Reg(Reg::R11))));
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
                                Arg64::Mem(MemRef {
                                    reg: Reg::Rdi,
                                    offset: TransferField::Resume.offset() as i32,
                                }),
                            )),
                            Instr::Jmp(JmpArgs::Reg(Reg::Rax)),
                        ]);
                    }
                }
            }
            | Terminator::Extern(sa::Extern::Foreign(import)) => {
                em.emit_foreign_call(id, import);
            }
            | Terminator::Abort(sa::Abort) => {
                em.asm.text.push(Instr::Comment("abort".to_string()));
                em.emit_aligned_call(JmpArgs::Label("zydeco_abort".to_string()));
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
                em.emit_alloc_call(layout.arity, AllocationKind::Scanned, id);
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
                em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
                em.shift_stack_parity(
                    -i64::try_from(layout.elements).expect("product elements overflow"),
                );
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
                // Outgoing values are staged; the destination entry chooses reuse or restoration.
                em.asm.text.push(Instr::Comment("leave local context for transfer".to_string()));
            }
            | Instruction::RetainFrame(retain) => {
                let slots =
                    retain.captures.iter().map(|var| em.frames.slots[var]).collect::<Vec<_>>();
                let owner = em.frames.owners[&id];
                let descriptor = em.frame_descriptor(
                    format!("frame_suspend_{}", id.concise_inner().replace('#', "_")),
                    Action::suspend(owner, slots.len()),
                    &slots,
                );
                em.asm.text.push(Instr::Comment("retain activation slots for return".to_string()));
                em.emit_frame_call(descriptor);
                em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
                em.shift_stack_parity(1);
            }
            | Instruction::PushArg(sa::Push(atom)) => {
                // Push argument onto stack
                atom.emit(id, em);
            }
            | Instruction::PopArg(sa::Pop(var_id)) => {
                // Pop argument from stack into variable
                let var_name = &em.assembly.variables[var_id];
                let idx = i32::try_from(em.frames.slots[var_id]).expect("frame slot overflow");
                em.asm.text.extend([
                    Instr::Comment(format!("pop_arg {}{}", var_name.plain(), var_id.concise())),
                    // pop from stack
                    Instr::Pop(Loc::Reg(Reg::Rax)),
                    // store to [rbp + 8 * idx]
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: ENV_REG, offset: WORD_BYTES as i32 * idx },
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
                    Instr::Push(Arg32::Unsigned(
                        u32::try_from(
                            RuntimeWord::index(tag.idx)
                                .expect("runtime tag index does not fit an immediate"),
                        )
                        .expect("runtime tag does not fit a push immediate"),
                    )),
                ]);
                em.shift_stack_parity(1);
            }
            | Instruction::Primitive(operation) => {
                operation.emit(id, em);
            }
            | Instruction::Clear(_) => {
                // Slot maps exclude dead bindings from collection. A pending continuation
                // may still retain the same physical slot, so do not overwrite it here.
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
                let idx = i32::try_from(em.frames.slots[var_id]).expect("frame slot overflow");
                // load [rbp + 8 * idx] and push
                em.asm.text.extend([
                    Instr::Mov(MovArgs::ToReg(
                        Reg::Rax,
                        Arg64::Mem(MemRef { reg: ENV_REG, offset: WORD_BYTES as i32 * idx }),
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
                    em.asm.text.push(Instr::Push(Arg32::Unsigned(
                        u32::try_from(
                            RuntimeWord::index(0)
                                .expect("runtime tag index does not fit an immediate"),
                        )
                        .unwrap(),
                    )));
                    em.shift_stack_parity(1);
                }
                | sa::Imm::Integer(i) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_integer {:?}", i)));
                    match i.encode_runtime().expect("unresolved integer literal reached emission") {
                        | EncodedScalar::Immediate(word) => {
                            em.asm.text.extend([
                                Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(word))),
                                Instr::Push(Arg32::Reg(Reg::Rax)),
                            ]);
                            em.shift_stack_parity(1);
                        }
                        | EncodedScalar::Boxed(bits) => {
                            em.emit_boxed_bits(bits, id);
                        }
                    }
                }
                | sa::Imm::Float(value) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_float {:?}", value)));
                    match value.encode_runtime() {
                        | EncodedScalar::Immediate(word) => {
                            em.asm.text.extend([
                                Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(word))),
                                Instr::Push(Arg32::Reg(Reg::Rax)),
                            ]);
                            em.shift_stack_parity(1);
                        }
                        | EncodedScalar::Boxed(bits) => {
                            em.emit_boxed_bits(bits, id);
                        }
                    }
                }
                | sa::Imm::Char(c) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_char {:?}", c)));
                    em.asm.text.push(Instr::Push(Arg32::Unsigned(
                        u32::try_from(
                            RuntimeWord::index(c as usize)
                                .expect("runtime tag index does not fit an immediate"),
                        )
                        .expect("tagged character does not fit a push immediate"),
                    )));
                    em.shift_stack_parity(1);
                }
            },
        }
    }
}
