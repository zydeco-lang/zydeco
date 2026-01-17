use std::fmt;

/// Unadorned reg is a 64-bit reg
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reg {
    Rax,
    Rbx,
    Rdx,
    Rcx,
    Rsi,
    Rdi,
    Rsp,
    Rbp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reg8 {
    Ah,
    Al,
    Ch,
    Cl,
    Dh,
    Dl,
    Bh,
    Bl,
    Spl,
    Bpl,
    Sil,
    Dil,
    R8b,
    R9b,
    R10b,
    R11b,
    R12b,
    R13b,
    R14b,
    R15b,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConditionCode {
    E,
    NE,
    L,
    LE,
    G,
    GE,
    S,
    Z,
    NZ,
    O,
    NO,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MemRef {
    pub reg: Reg,
    pub offset: i32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
/// [rel label + reg * scale]
pub struct RelLabel {
    pub label: String,
    /// register and offset, if any
    pub offset: Option<(Reg, i32)>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Arg64 {
    Reg(Reg),
    Signed(i64),
    Unsigned(u64),
    Mem(MemRef),
    Label(String),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Arg32 {
    Reg(Reg),
    Signed(i32),
    Unsigned(u32),
    Mem(MemRef),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reg32 {
    Reg(Reg),
    Imm(i32),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum MovArgs {
    ToReg(Reg, Arg64),
    ToMem(MemRef, Reg32),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum LeaArgs {
    Displace { base: Reg, scaled_index: Option<(Reg, i32)>, offset: Option<i32> },
    RelLabel(RelLabel),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinArgs {
    ToReg(Reg, Arg32),
    ToMem(MemRef, Reg32),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ShArgs {
    pub reg: Reg,
    pub by: u8,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum JmpArgs {
    Label(String),
    Reg(Reg),
    Mem(MemRef),
    RelLabel(RelLabel),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Loc {
    Reg(Reg),
    Mem(MemRef),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instr {
    Mov(MovArgs),
    Lea(Reg, LeaArgs),

    Add(BinArgs),
    Sub(BinArgs),
    IMul(BinArgs),
    And(BinArgs),
    Or(BinArgs),
    Xor(BinArgs),
    Sal(ShArgs),
    Sar(ShArgs),
    Shl(ShArgs),
    Shr(ShArgs),
    Cmp(BinArgs),
    Test(BinArgs),
    Xchg(Reg, Reg),

    Push(Arg32),
    Pop(Loc),

    Label(String),
    Comment(String),
    Global(String),
    Extern(String),

    Jmp(JmpArgs),
    Call(JmpArgs),
    Ret,

    // Conditional mov, jmp and set
    CMovCC(ConditionCode, BinArgs),
    JCC(ConditionCode, JmpArgs),
    SetCC(ConditionCode, Reg8),

    // Define data
    Dq(String),
}

/// Represents a complete x86-64 assembly file organized by ELF sections.
///
/// This struct organizes x86 assembly instructions into the standard ELF sections
/// used by linkers and loaders:
///
/// - **`.text`**: Executable code (instructions). This is where all program logic lives.
/// - **`.data`**: Initialized writable data (global variables with non-zero initial values).
/// - **`.rodata`**: Read-only data (constants, string literals, jump tables). The linker
///   places this in a read-only memory segment.
/// - **`.bss`**: Uninitialized writable data (zero-initialized globals). The linker
///   reserves space but doesn't store data in the object file.
///
/// # x86-64 Context
///
/// This abstraction targets x86-64 (64-bit) assembly using Intel syntax.
/// The instructions in each section are expected to be valid x86-64 instructions
/// that can be assembled by NASM or compatible assemblers.
#[derive(Default, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct AsmFile {
    /// Instructions for the `.text` section (executable code).
    pub text: Vec<Instr>,
    /// Instructions and data for the `.data` section (initialized writable data).
    pub data: Vec<Instr>,
    /// Instructions and data for the `.rodata` section (read-only data).
    pub rodata: Vec<Instr>,
    /// Instructions and data for the `.bss` section (uninitialized writable data).
    pub bss: Vec<Instr>,
}

impl AsmFile {
    pub fn new() -> Self {
        Self { text: Vec::new(), data: Vec::new(), rodata: Vec::new(), bss: Vec::new() }
    }
}

impl fmt::Display for ConditionCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ConditionCode::*;
        match self {
            | E => write!(f, "e"),
            | NE => write!(f, "ne"),
            | L => write!(f, "l"),
            | LE => write!(f, "le"),
            | G => write!(f, "g"),
            | GE => write!(f, "ge"),
            | S => write!(f, "s"),
            | Z => write!(f, "z"),
            | NZ => write!(f, "nz"),
            | O => write!(f, "o"),
            | NO => write!(f, "no"),
        }
    }
}

impl fmt::Display for Reg8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Reg8::*;
        match self {
            | Al => write!(f, "al"),
            | Ah => write!(f, "ah"),
            | Cl => write!(f, "cl"),
            | Ch => write!(f, "ch"),
            | Dl => write!(f, "dl"),
            | Dh => write!(f, "dh"),
            | Bl => write!(f, "bl"),
            | Bh => write!(f, "bh"),
            | Spl => write!(f, "spl"),
            | Bpl => write!(f, "bpl"),
            | Sil => write!(f, "sil"),
            | Dil => write!(f, "dil"),
            | R8b => write!(f, "r8b"),
            | R9b => write!(f, "r9b"),
            | R10b => write!(f, "r10b"),
            | R11b => write!(f, "r11b"),
            | R12b => write!(f, "r12b"),
            | R13b => write!(f, "r13b"),
            | R14b => write!(f, "r14b"),
            | R15b => write!(f, "r15b"),
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | Reg::Rax => write!(f, "rax"),
            | Reg::Rbx => write!(f, "rbx"),
            | Reg::Rcx => write!(f, "rcx"),
            | Reg::Rdx => write!(f, "rdx"),
            | Reg::Rsi => write!(f, "rsi"),
            | Reg::Rdi => write!(f, "rdi"),
            | Reg::Rsp => write!(f, "rsp"),
            | Reg::Rbp => write!(f, "rbp"),
            | Reg::R8 => write!(f, "r8"),
            | Reg::R9 => write!(f, "r9"),
            | Reg::R10 => write!(f, "r10"),
            | Reg::R11 => write!(f, "r11"),
            | Reg::R12 => write!(f, "r12"),
            | Reg::R13 => write!(f, "r13"),
            | Reg::R14 => write!(f, "r14"),
            | Reg::R15 => write!(f, "r15"),
        }
    }
}

impl fmt::Display for MemRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::cmp::Ordering;
        let offset = match self.offset.cmp(&0) {
            | Ordering::Less => format!(" - {}", -self.offset),
            | Ordering::Equal => String::new(),
            | Ordering::Greater => format!(" + {}", self.offset),
        };
        write!(f, "[{}{}]", self.reg, offset)
    }
}

impl fmt::Display for RelLabel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.offset {
            | Some((reg, offset)) => write!(f, "[rel {} + {} * {}]", self.label, reg, offset),
            | None => write!(f, "[rel {}]", self.label),
        }
    }
}

impl fmt::Display for Reg32 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | Reg32::Reg(r) => write!(f, "{}", r),
            | Reg32::Imm(i) => write!(f, "{}", i),
        }
    }
}

impl fmt::Display for Arg32 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | Arg32::Reg(r) => write!(f, "{}", r),
            | Arg32::Signed(i) => write!(f, "{}", i),
            | Arg32::Unsigned(u) => write!(f, "0x{:08x}", u),
            | Arg32::Mem(m) => write!(f, "QWORD {}", m),
        }
    }
}

impl fmt::Display for Arg64 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | Arg64::Reg(r) => write!(f, "{}", r),
            | Arg64::Signed(i) => write!(f, "{}", i),
            | Arg64::Unsigned(u) => write!(f, "0x{:016x}", u),
            | Arg64::Mem(m) => write!(f, "QWORD {}", m),
            | Arg64::Label(l) => write!(f, "{}", l),
        }
    }
}

impl fmt::Display for MovArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | MovArgs::ToReg(r, arg) => write!(f, "{}, {}", r, arg),
            | MovArgs::ToMem(mem, arg) => write!(f, "QWORD {}, {}", mem, arg),
        }
    }
}

impl fmt::Display for LeaArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | LeaArgs::Displace { base, scaled_index, offset } => {
                let scaled_index = scaled_index
                    .map_or_else(String::new, |(index, scale)| format!(" + {}*{}", index, scale));
                let offset = offset.map_or_else(String::new, |offset| format!(" + {}", offset));

                write!(f, "[{}{}{}]", base, scaled_index, offset)
            }
            | LeaArgs::RelLabel(rl) => write!(f, "{}", rl),
        }
    }
}

impl fmt::Display for BinArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | BinArgs::ToReg(r, arg) => write!(f, "{}, {}", r, arg),
            | BinArgs::ToMem(mem, arg) => write!(f, "QWORD {}, {}", mem, arg),
        }
    }
}

impl fmt::Display for ShArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}, {}", self.reg, self.by)
    }
}

impl fmt::Display for JmpArgs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | JmpArgs::Label(l) => write!(f, "{}", l),
            | JmpArgs::Reg(r) => write!(f, "{}", r),
            | JmpArgs::Mem(m) => write!(f, "{}", m),
            | JmpArgs::RelLabel(rl) => write!(f, "{}", rl),
        }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | Loc::Reg(r) => write!(f, "{}", r),
            | Loc::Mem(m) => write!(f, "QWORD {}", m),
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            | Instr::Mov(args) => {
                write!(f, "        mov {}", args)
            }
            | Instr::Lea(r, args) => {
                write!(f, "        lea {}, {}", r, args)
            }
            | Instr::Add(args) => {
                write!(f, "        add {}", args)
            }
            | Instr::Sub(args) => {
                write!(f, "        sub {}", args)
            }
            | Instr::IMul(args) => {
                write!(f, "        imul {}", args)
            }
            | Instr::And(args) => {
                write!(f, "        and {}", args)
            }
            | Instr::Or(args) => {
                write!(f, "        or {}", args)
            }
            | Instr::Xor(args) => {
                write!(f, "        xor {}", args)
            }
            | Instr::Sal(args) => {
                write!(f, "        sal {}", args)
            }
            | Instr::Sar(args) => {
                write!(f, "        sar {}", args)
            }
            | Instr::Shl(args) => {
                write!(f, "        shl {}", args)
            }
            | Instr::Shr(args) => {
                write!(f, "        shr {}", args)
            }
            | Instr::Cmp(args) => {
                write!(f, "        cmp {}", args)
            }
            | Instr::Test(args) => {
                write!(f, "        test {}", args)
            }
            | Instr::Xchg(r1, r2) => {
                write!(f, "        xchg {}, {}", r1, r2)
            }
            | Instr::Push(arg) => {
                write!(f, "        push {}", arg)
            }
            | Instr::Pop(loc) => {
                write!(f, "        pop {}", loc)
            }
            | Instr::Label(s) => {
                write!(f, "{}:", s)
            }
            | Instr::Comment(s) => {
                write!(f, ";;; {}", s)
            }
            | Instr::Global(s) => {
                write!(f, "        global {}", s)
            }
            | Instr::Extern(s) => {
                write!(f, "        extern {}", s)
            }
            | Instr::Jmp(s) => {
                write!(f, "        jmp {}", s)
            }
            | Instr::Call(s) => {
                write!(f, "        call {}", s)
            }
            | Instr::Ret => {
                write!(f, "        ret")
            }
            | Instr::CMovCC(cc, args) => {
                write!(f, "        cmov{} {}", cc, args)
            }
            | Instr::JCC(cc, l) => {
                write!(f, "        j{} {}", cc, l)
            }
            | Instr::SetCC(cc, a) => {
                write!(f, "        set{} {}", cc, a)
            }
            | Instr::Dq(s) => {
                write!(f, "        dq {}", s)
            }
        }
    }
}

impl fmt::Display for AsmFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Use RIP-relative addressing by default for position-independent code
        writeln!(f, "default rel")?;
        // Emit sections in standard ELF order: .text, .data, .rodata, .bss
        if !self.text.is_empty() {
            writeln!(f, "section .text")?;
            for instr in &self.text {
                writeln!(f, "{}", instr)?;
            }
        }

        if !self.data.is_empty() {
            writeln!(f, "section .data")?;
            for instr in &self.data {
                writeln!(f, "{}", instr)?;
            }
        }

        if !self.rodata.is_empty() {
            writeln!(f, "section .rodata")?;
            for instr in &self.rodata {
                writeln!(f, "{}", instr)?;
            }
        }

        if !self.bss.is_empty() {
            writeln!(f, "section .bss")?;
            for instr in &self.bss {
                writeln!(f, "{}", instr)?;
            }
        }

        Ok(())
    }
}
