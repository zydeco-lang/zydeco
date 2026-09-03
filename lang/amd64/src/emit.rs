use super::syntax::*;
use derive_more::{AsMut, AsRef};
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use zydeco_assembly::{
    arena::{AssemblyArena, AssemblyArenaRefLike, AssemblyProgram},
    syntax::{self as sa, Atom, Instruction, Intrinsic, ProgId, Program, Symbol, Terminator},
};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::arena::SpanArena};
use zydeco_syntax::*;
use zydeco_utils::pass::CompilerPass;

pub const ENV_REG: Reg = Reg::Rbp;

const IMMEDIATE_TAG: u64 = 1;
const IMMEDIATE_UNSIGNED_MAX: u64 = u64::MAX >> 1;
const IMMEDIATE_SIGNED_MIN: i64 = -(1_i64 << 62);
const IMMEDIATE_SIGNED_MAX: i64 = (1_i64 << 62) - 1;

#[derive(Clone, Copy)]
enum AllocationKind {
    Scanned,
    Opaque,
}

impl AllocationKind {
    fn symbol(self) -> &'static str {
        match self {
            | Self::Scanned => "zydeco_alloc_scanned",
            | Self::Opaque => "zydeco_alloc_opaque",
        }
    }
}

enum EncodedLiteral {
    Immediate(u64),
    Boxed(u64),
}

/// The one-word value convention shared with the native runtime.
///
/// Odd words are immediates. Even words are pointer-shaped values; the collector
/// only moves those that point into its active semispace. Wide scalars that cannot
/// surrender one tag bit use one-word opaque heap blocks.
struct TaggedValue;

impl TaggedValue {
    fn unsigned(value: u64) -> Option<u64> {
        (value <= IMMEDIATE_UNSIGNED_MAX).then_some((value << 1) | IMMEDIATE_TAG)
    }

    fn signed(value: i64) -> Option<u64> {
        (IMMEDIATE_SIGNED_MIN..=IMMEDIATE_SIGNED_MAX)
            .contains(&value)
            .then_some(((value as u64) << 1) | IMMEDIATE_TAG)
    }

    fn integer(value: IntegerLiteral) -> EncodedLiteral {
        use IntegerLiteral::*;
        let immediate = match value {
            | Int8(value) => Self::signed(value.into()),
            | Int16(value) => Self::signed(value.into()),
            | Int32(value) => Self::signed(value.into()),
            | Int64(value) => Self::signed(value),
            | UInt8(value) => Self::unsigned(value.into()),
            | UInt16(value) => Self::unsigned(value.into()),
            | UInt32(value) => Self::unsigned(value.into()),
            | UInt64(value) => Self::unsigned(value),
            | Unresolved(_) => panic!("unresolved integer literal reached emission"),
        };
        immediate
            .map_or_else(|| EncodedLiteral::Boxed(value.to_word_bits()), EncodedLiteral::Immediate)
    }

    fn float(value: FloatLiteral) -> EncodedLiteral {
        match value {
            | FloatLiteral::Float32(bits) => EncodedLiteral::Immediate(
                Self::unsigned(bits.into()).expect("Float32 payload fits an immediate"),
            ),
            | FloatLiteral::Float64(bits) => EncodedLiteral::Boxed(bits),
        }
    }

    fn index(value: usize) -> u64 {
        Self::unsigned(u64::try_from(value).expect("runtime tag index overflow"))
            .expect("runtime tag index does not fit an immediate")
    }
}

struct HostCall;

#[derive(Clone, Copy)]
enum SpareBox {
    Unused,
    Opaque,
}

impl HostCall {
    /// Numeric arithmetic receives a hidden spare-box pointer after its source arguments.
    fn spare_box(role: BuiltinValueRole) -> Option<SpareBox> {
        match role {
            | BuiltinValueRole::Integer(
                integer,
                IntegerOperation::Add
                | IntegerOperation::Sub
                | IntegerOperation::Mul
                | IntegerOperation::Div
                | IntegerOperation::Mod,
            ) => Some(if matches!(integer, IntegerType::Int64 | IntegerType::UInt64) {
                SpareBox::Opaque
            } else {
                SpareBox::Unused
            }),
            | BuiltinValueRole::Float(
                float,
                FloatOperation::Add
                | FloatOperation::Sub
                | FloatOperation::Mul
                | FloatOperation::Div,
            ) => {
                Some(if float == FloatType::Float64 { SpareBox::Opaque } else { SpareBox::Unused })
            }
            | BuiltinValueRole::StrParseInt
            | BuiltinValueRole::ReadLineAsInt
            | BuiltinValueRole::RandomInt => Some(SpareBox::Opaque),
            | _ => None,
        }
    }
}

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
    stack_parity: StackParity,
    entry_parities: HashMap<ProgId, StackParity>,
    dynamic_entries: HashSet<ProgId>,
}

impl<'e> Emitter<'e> {
    pub fn new(
        spans: &'e SpanArena, scoped: &'e ScopedArena, statics: &'e StaticsArena,
        assembly: &'e AssemblyProgram, target_format: TargetFormat,
    ) -> Self {
        let arena = assembly.arena();
        let root = assembly.root();
        let entry_parities = Self::compute_entry_parities(arena, root);
        let dynamic_entries = Self::compute_dynamic_entries(arena);
        Self {
            spans,
            scoped,
            statics,
            assembly: arena,
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

    /// Allocate one block from the runtime's fixed two-space heap.
    ///
    /// The collector is otherwise runtime-only: it updates the live control-stack
    /// words and the current environment frame passed here. Tagged immediates make
    /// pointer recognition precise without stack maps.
    fn emit_alloc_call(&mut self, size_words: usize, kind: AllocationKind, context_words: usize) {
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
            Instr::Mov(MovArgs::ToReg(Reg::Rdx, Arg64::Reg(ENV_REG))),
            Instr::Mov(MovArgs::ToReg(
                Reg::Rcx,
                Arg64::Unsigned(
                    u64::try_from(context_words).expect("environment root count overflow"),
                ),
            )),
        ]);
        self.emit_aligned_call(JmpArgs::Label(kind.symbol().to_string()));
    }

    fn emit_boxed_bits(&mut self, bits: u64, context_words: usize) {
        self.emit_alloc_call(1, AllocationKind::Opaque, context_words);
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
        assert_eq!(import.signature.result(), ForeignResult::UInt64);
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
                | ForeignParameter::BorrowedBytes => "zydeco_ffi_borrow_bytes",
                | ForeignParameter::UInt64 => "zydeco_ffi_decode_u64",
            };
            self.emit_aligned_call(JmpArgs::Label(helper.to_string()));
            self.asm.text.extend(
                arguments
                    .iter()
                    .enumerate()
                    .filter(|(_, argument)| argument.parameter == index)
                    .map(|(slot, argument)| {
                        let register = match argument.component {
                            | ForeignComponent::BytesPointer | ForeignComponent::UInt64 => Reg::Rax,
                            | ForeignComponent::BytesLength => Reg::Rdx,
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
        let context_words = self.assembly.contexts[&id].iter().len();
        self.emit_alloc_call(1, AllocationKind::Opaque, context_words);
        self.asm.text.extend([
            Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Reg(Reg::R12))),
            Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Reg(Reg::Rax))),
        ]);
        self.emit_aligned_call(JmpArgs::Label("zydeco_ffi_encode_u64".to_string()));
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

impl<'e> CompilerPass for Emitter<'e> {
    type Out = AsmFile;
    type Error = std::convert::Infallible;
    fn run(mut self) -> Result<Self::Out, Self::Error> {
        self.asm.text.extend([
            // zydeco_abort
            Instr::Extern("zydeco_abort".to_string()),
            // fixed-heap allocation entry points
            Instr::Extern("zydeco_alloc_scanned".to_string()),
            Instr::Extern("zydeco_alloc_opaque".to_string()),
            // legacy intrinsic comparison helpers
            Instr::Extern("zydeco_intrinsic_int64_eq".to_string()),
            Instr::Extern("zydeco_intrinsic_int64_lt".to_string()),
            Instr::Extern("zydeco_intrinsic_int64_gt".to_string()),
            Instr::Extern("zydeco_intrinsic_int64_and".to_string()),
            Instr::Extern("zydeco_intrinsic_int64_or".to_string()),
            Instr::Extern("zydeco_intrinsic_int64_xor".to_string()),
            // host callback used by runtime-created argument-fold thunks
            Instr::Extern("zydeco_arg_fold_resume".to_string()),
            // construct an owned host string from static UTF-8 bytes
            Instr::Extern("zydeco_string_literal".to_string()),
            // source-to-C marshalling helpers
            Instr::Extern("zydeco_ffi_borrow_bytes".to_string()),
            Instr::Extern("zydeco_ffi_decode_u64".to_string()),
            Instr::Extern("zydeco_ffi_encode_u64".to_string()),
        ]);

        // Emit the externs
        for external in self.assembly.externs.iter() {
            let label = match external {
                | sa::Extern::Host { name, .. } => format!("zydeco_{name}"),
                | sa::Extern::Foreign(import) => self.foreign_symbol(&import.target.symbol),
            };
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
                let spare_box = HostCall::spare_box(*role);
                match spare_box {
                    | Some(SpareBox::Opaque) => {
                        let context_words = em.assembly.contexts[&id].iter().len();
                        em.emit_alloc_call(1, AllocationKind::Opaque, context_words);
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
                                Arg64::Mem(MemRef { reg: Reg::Rdi, offset: 0 }),
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
                if layout.stack_alloc {
                    em.asm.text.extend([
                        Instr::Comment("allocate product in the current stack frame".to_string()),
                        Instr::Sub(BinArgs::ToReg(
                            Reg::Rsp,
                            Arg32::Signed(8 * layout.arity as i32),
                        )),
                        Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::Rsp))),
                    ]);
                } else {
                    let context_words = em.assembly.contexts[&id].iter().len();
                    em.emit_alloc_call(layout.arity, AllocationKind::Scanned, context_words);
                }
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
                if layout.stack_alloc {
                    let flips = layout.arity + layout.elements + 1;
                    em.shift_stack_parity(i64::try_from(flips).expect("stack parity overflow"));
                } else {
                    em.shift_stack_parity(
                        -i64::try_from(layout.elements).expect("product elements overflow"),
                    );
                    em.shift_stack_parity(1);
                }
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
                // Calls are tail calls: everything needed by the callee has already
                // moved to the control stack, so the next environment can reuse the
                // fixed buffer from offset zero. Keeping Rbp unchanged also prevents
                // recursive programs from exhausting a separate environment arena.
                em.asm.text.push(Instr::Comment("reuse environment for tail call".to_string()));
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
                    Instr::Push(Arg32::Unsigned(
                        u32::try_from(TaggedValue::index(tag.idx))
                            .expect("runtime tag does not fit a push immediate"),
                    )),
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
                    em.asm.text.push(Instr::Push(Arg32::Unsigned(
                        u32::try_from(TaggedValue::index(0)).unwrap(),
                    )));
                    em.shift_stack_parity(1);
                }
                | sa::Imm::Integer(i) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_integer {:?}", i)));
                    match TaggedValue::integer(i) {
                        | EncodedLiteral::Immediate(word) => {
                            em.asm.text.extend([
                                Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(word))),
                                Instr::Push(Arg32::Reg(Reg::Rax)),
                            ]);
                            em.shift_stack_parity(1);
                        }
                        | EncodedLiteral::Boxed(bits) => {
                            let context_words = em.assembly.contexts[&id].iter().len();
                            em.emit_boxed_bits(bits, context_words);
                        }
                    }
                }
                | sa::Imm::Float(value) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_float {:?}", value)));
                    match TaggedValue::float(value) {
                        | EncodedLiteral::Immediate(word) => {
                            em.asm.text.extend([
                                Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(word))),
                                Instr::Push(Arg32::Reg(Reg::Rax)),
                            ]);
                            em.shift_stack_parity(1);
                        }
                        | EncodedLiteral::Boxed(bits) => {
                            let context_words = em.assembly.contexts[&id].iter().len();
                            em.emit_boxed_bits(bits, context_words);
                        }
                    }
                }
                | sa::Imm::Char(c) => {
                    em.asm.text.push(Instr::Comment(format!("push_imm_char {:?}", c)));
                    em.asm.text.push(Instr::Push(Arg32::Unsigned(
                        u32::try_from(TaggedValue::index(c as usize))
                            .expect("tagged character does not fit a push immediate"),
                    )));
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

        match (name.as_str(), arity) {
            | (_, 2) if matches!(name.as_str(), "int_eq" | "int_lt" | "int_gt") => {
                let target = match name.as_str() {
                    | "int_eq" => "zydeco_intrinsic_int64_eq",
                    | "int_lt" => "zydeco_intrinsic_int64_lt",
                    | "int_gt" => "zydeco_intrinsic_int64_gt",
                    | _ => unreachable!("matched comparison intrinsic"),
                };
                em.asm
                    .text
                    .extend([Instr::Pop(Loc::Reg(Reg::Rdi)), Instr::Pop(Loc::Reg(Reg::Rsi))]);
                em.shift_stack_parity(-2);
                em.emit_aligned_call(JmpArgs::Label(target.to_string()));
                // Keep the tagged constructor index rooted while allocation may collect.
                em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
                em.shift_stack_parity(1);
                let context_words = em.assembly.contexts[&id].iter().len();
                em.emit_alloc_call(2, AllocationKind::Scanned, context_words);
                em.asm.text.extend([
                    Instr::Mov(MovArgs::ToReg(Reg::Rdx, Arg64::Reg(Reg::Rax))),
                    Instr::Pop(Loc::Reg(Reg::Rcx)),
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: Reg::Rdx, offset: 0 },
                        Reg32::Reg(Reg::Rcx),
                    )),
                    Instr::Mov(MovArgs::ToMem(
                        MemRef { reg: Reg::Rdx, offset: 8 },
                        Reg32::Imm(IMMEDIATE_TAG as i32),
                    )),
                    Instr::Push(Arg32::Reg(Reg::Rdx)),
                ]);
                em.shift_stack_parity(-1);
                em.shift_stack_parity(1);
            }
            | (_, 2) => {
                let target = match name.as_str() {
                    | "add" => "zydeco_int64_add",
                    | "sub" => "zydeco_int64_sub",
                    | "mul" => "zydeco_int64_mul",
                    | "div" => "zydeco_int64_div",
                    | "mod" => "zydeco_int64_mod",
                    | "and" => "zydeco_intrinsic_int64_and",
                    | "or" => "zydeco_intrinsic_int64_or",
                    | "xor" => "zydeco_intrinsic_int64_xor",
                    | _ => {
                        unimplemented!("intrinsic {} with arity {} not implemented", name, arity)
                    }
                };
                let context_words = em.assembly.contexts[&id].iter().len();
                em.emit_alloc_call(1, AllocationKind::Opaque, context_words);
                em.asm.text.extend([
                    Instr::Mov(MovArgs::ToReg(Reg::R11, Arg64::Reg(Reg::Rax))),
                    Instr::Pop(Loc::Reg(Reg::Rdi)),
                    Instr::Pop(Loc::Reg(Reg::Rsi)),
                    Instr::Mov(MovArgs::ToReg(Reg::Rdx, Arg64::Reg(Reg::R11))),
                ]);
                em.shift_stack_parity(-2);
                em.emit_aligned_call(JmpArgs::Label(target.to_string()));
                em.asm.text.push(Instr::Push(Arg32::Reg(Reg::Rax)));
                em.shift_stack_parity(1);
            }
            | _ => unimplemented!("intrinsic {} with arity {} not implemented", name, arity),
        }
    }
}
