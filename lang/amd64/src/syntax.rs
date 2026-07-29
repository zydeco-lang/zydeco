use derive_more::Display;
use std::fmt;

/// Unadorned reg is a 64-bit reg
#[derive(Copy, Clone, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[display(rename_all = "lowercase")]
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

#[derive(Copy, Clone, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[display(rename_all = "lowercase")]
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

#[derive(Clone, Copy, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[display(rename_all = "lowercase")]
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

#[derive(Clone, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Arg64 {
    Reg(Reg),
    Signed(i64),
    #[display("0x{_0:016x}")]
    Unsigned(u64),
    #[display("QWORD {_0}")]
    Mem(MemRef),
    Label(String),
}

#[derive(Clone, Copy, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Arg32 {
    Reg(Reg),
    Signed(i32),
    #[display("0x{_0:08x}")]
    Unsigned(u32),
    #[display("QWORD {_0}")]
    Mem(MemRef),
}

#[derive(Clone, Copy, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Reg32 {
    Reg(Reg),
    Imm(i32),
}

#[derive(Clone, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum MovArgs {
    #[display("{_0}, {_1}")]
    ToReg(Reg, Arg64),
    #[display("QWORD {_0}, {_1}")]
    ToMem(MemRef, Reg32),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum LeaArgs {
    Displace { base: Reg, scaled_index: Option<(Reg, i32)>, offset: Option<i32> },
    RelLabel(RelLabel),
}

#[derive(Clone, Copy, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinArgs {
    #[display("{_0}, {_1}")]
    ToReg(Reg, Arg32),
    #[display("QWORD {_0}, {_1}")]
    ToMem(MemRef, Reg32),
}

#[derive(Clone, Copy, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[display("{reg}, {by}")]
pub struct ShArgs {
    pub reg: Reg,
    pub by: u8,
}

#[derive(Clone, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum JmpArgs {
    Label(String),
    Reg(Reg),
    Mem(MemRef),
    RelLabel(RelLabel),
}

#[derive(Clone, Copy, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Loc {
    Reg(Reg),
    #[display("QWORD {_0}")]
    Mem(MemRef),
}

#[derive(Clone, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instr {
    #[display("        mov {_0}")]
    Mov(MovArgs),
    #[display("        lea {_0}, {_1}")]
    Lea(Reg, LeaArgs),

    #[display("        add {_0}")]
    Add(BinArgs),
    #[display("        sub {_0}")]
    Sub(BinArgs),
    #[display("        imul {_0}")]
    IMul(BinArgs),
    #[display("        and {_0}")]
    And(BinArgs),
    #[display("        or {_0}")]
    Or(BinArgs),
    #[display("        xor {_0}")]
    Xor(BinArgs),
    #[display("        sal {_0}")]
    Sal(ShArgs),
    #[display("        sar {_0}")]
    Sar(ShArgs),
    #[display("        shl {_0}")]
    Shl(ShArgs),
    #[display("        shr {_0}")]
    Shr(ShArgs),
    #[display("        cmp {_0}")]
    Cmp(BinArgs),
    #[display("        test {_0}")]
    Test(BinArgs),
    #[display("        xchg {_0}, {_1}")]
    Xchg(Reg, Reg),

    #[display("        push {_0}")]
    Push(Arg32),
    #[display("        pop {_0}")]
    Pop(Loc),

    #[display("{_0}:")]
    Label(String),
    #[display(";;; {_0}")]
    Comment(String),
    #[display("        global {_0}")]
    Global(String),
    #[display("        extern {_0}")]
    Extern(String),

    #[display("        jmp {_0}")]
    Jmp(JmpArgs),
    #[display("        call {_0}")]
    Call(JmpArgs),
    #[display("        ret")]
    Ret,

    // Conditional mov, jmp and set
    #[display("        cmov{_0} {_1}")]
    CMovCC(ConditionCode, BinArgs),
    #[display("        j{_0} {_1}")]
    JCC(ConditionCode, JmpArgs),
    #[display("        set{_0} {_1}")]
    SetCC(ConditionCode, Reg8),

    // Define data
    #[display("        dq {_0}")]
    Dq(String),
}

/// Represents a complete amd64 assembly file organized by ELF sections.
///
/// This struct organizes amd64 assembly instructions into the standard ELF sections
/// used by linkers and loaders:
///
/// - **`.text`**: Executable code (instructions). This is where all program logic lives.
/// - **`.data`**: Initialized writable data (global variables with non-zero initial values).
/// - **`.rodata`**: Read-only data (constants, string literals, jump tables). The linker
///   places this in a read-only memory segment.
/// - **`.bss`**: Uninitialized writable data (zero-initialized globals). The linker
///   reserves space but doesn't store data in the object file.
///
/// # amd64 Context
///
/// This abstraction targets amd64 (64-bit) assembly using Intel syntax.
/// The instructions in each section are expected to be valid amd64 instructions
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
