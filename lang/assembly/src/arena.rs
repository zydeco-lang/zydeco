use super::syntax::*;
use zydeco_stack::syntax as sk;

pub struct AssemblyArena {
    /// All programs are attached with a ProgId.
    pub programs: ArenaSparse<ProgId, Program>,
    /// All variables are named.
    pub variables: ArenaSparse<VarId, VarName>,
    /// All symbols are named.
    pub symbols: ArenaSparse<SymId, Symbol>,

    /// Map from DefId to VarId or SymId
    pub defs: ArenaBijective<sk::DefId, DefId>,

    /// Programs are (optionally) labeled.
    pub labels: ArenaAssoc<ProgId, SymId>,
    /// Externs that are variables.
    pub externs: ArenaAssoc<VarId, ()>,
    /// Number of nominations of a program as an individual block.
    /// - If 0 / unexist, the program should be obliviated.
    /// - If 1, the program is inlined.
    /// - If > 1, the program is a block.
    pub blocks: ArenaAssoc<ProgId, usize>,
    /// The whole object has an optional entry point.
    pub entry: ArenaAssoc<ProgId, ()>,
}

impl AssemblyArena {
    pub fn new(alloc: ArcGlobalAlloc) -> Self {
        Self {
            programs: ArenaSparse::new(alloc.alloc()),
            variables: ArenaSparse::new(alloc.alloc()),
            symbols: ArenaSparse::new(alloc.alloc()),
            defs: ArenaBijective::new(),
            labels: ArenaAssoc::new(),
            externs: ArenaAssoc::new(),
            blocks: ArenaAssoc::new(),
            entry: ArenaAssoc::new(),
        }
    }
}
impl AsRef<AssemblyArena> for AssemblyArena {
    fn as_ref(&self) -> &AssemblyArena {
        self
    }
}
impl AsMut<AssemblyArena> for AssemblyArena {
    fn as_mut(&mut self) -> &mut AssemblyArena {
        self
    }
}

pub trait AssemblyArenaRefLike {
    fn sym_label(&self, sym: &SymId) -> String;
    fn prog_label(&self, prog: &ProgId) -> Option<String>;
}

impl<T> AssemblyArenaRefLike for T
where
    T: AsRef<AssemblyArena>,
{
    fn sym_label(&self, sym: &SymId) -> String {
        let this = self.as_ref();
        format!("{}_{}", this.symbols[&sym].name, sym.concise_inner())
    }
    fn prog_label(&self, prog: &ProgId) -> Option<String> {
        let this = self.as_ref();
        this.labels.get(&prog).map(|sym| this.sym_label(sym))
    }
}

pub trait AssemblyArenaMutLike {
    /// Find out if a program is nominated as an individual block, and decide what label to use.
    fn block_name(&mut self, prog: ProgId) -> Option<SymId>;
    /// Allocate a variable.
    fn var(&mut self, site: Option<sk::DefId>, var: impl Into<VarName>) -> VarId;
    /// Allocate a symbol.
    fn sym(
        &mut self, site: Option<sk::DefId>, name: impl Into<String>, inner: impl Into<SymbolInner>,
    ) -> SymId;
    /// Allocate a program that is anonymous, i.e. has no meaningful label.
    fn prog_anon(&mut self, prog: impl Into<Program>) -> ProgId;
    /// Allocate an instruction.
    fn instr(
        &mut self, instr: impl Into<Instruction>, kont: impl FnOnce(&mut Self) -> ProgId,
    ) -> ProgId;
}

impl<T> AssemblyArenaMutLike for T
where
    T: AsMut<AssemblyArena>,
{
    fn block_name(&mut self, prog: ProgId) -> Option<SymId> {
        let this = self.as_mut();
        let count = *this.blocks.get(&prog)?;
        if count < 2 {
            return None;
        }
        // let uniquifier = |name: &str| format!("{}_{}", name, prog.concise_inner());
        // let label = match this.labels.get(&prog) {
        //     | Some(label) => uniquifier(label.plain()),
        //     | None => uniquifier("block"),
        // };
        // Some(Label(label))
        let sym = match this.labels.get(&prog).cloned() {
            | Some(sym) => sym,
            | None => this.symbols.alloc(Symbol {
                name: format!("block_{}", prog.concise_inner()),
                inner: SymbolInner::Prog(prog),
            }),
        };
        Some(sym)
    }
    fn var(&mut self, site: Option<sk::DefId>, var: impl Into<VarName>) -> VarId {
        let this = &mut *self.as_mut();
        let id = this.variables.alloc(var.into());
        if let Some(site) = site {
            this.defs.insert(site, DefId::Var(id));
        }
        id
    }
    fn sym(
        &mut self, site: Option<sk::DefId>, name: impl Into<String>, inner: impl Into<SymbolInner>,
    ) -> SymId {
        let this = &mut *self.as_mut();
        let symbol = Symbol { name: name.into(), inner: inner.into() };
        let is_prog = match symbol.inner {
            | SymbolInner::Prog(prog_id) => Some(prog_id),
            | _ => None,
        };
        let id = this.symbols.alloc(symbol);
        if let Some(prog_id) = is_prog {
            // Strongly nominate the symbol if it is a program, ensuring a block.
            *this.blocks.entry(prog_id).or_insert(0) += 2;
            // Add a label to the program.
            this.labels.insert(prog_id, id);
        }
        if let Some(site) = site {
            this.defs.insert(site, DefId::Sym(id));
        }
        id
    }
    fn prog_anon(&mut self, prog: impl Into<Program>) -> ProgId {
        let this = &mut *self.as_mut();
        let program = prog.into();
        // Nominate inner programs if they are mentioned in the program.
        let mut nominate = |&prog| {
            *this.blocks.entry(prog).or_insert(0) += 1;
        };
        match &program {
            | Program::Instruction(_, prog_id) => nominate(prog_id),
            | Program::Jump(Jump(prog_id)) => nominate(prog_id),
            | Program::EqJump(EqJump(prog_id)) => nominate(prog_id),
            | Program::PopJump(PopJump) => {}
            | Program::LeapJump(LeapJump) => {}
            | Program::PopBranch(PopBranch(brs)) => {
                for (_, prog_id) in brs {
                    nominate(prog_id);
                }
            }
            | Program::Panic(Panic) => {}
        }
        let id = this.programs.alloc(program);
        id
    }
    fn instr(
        &mut self, instr: impl Into<Instruction>, kont: impl FnOnce(&mut Self) -> ProgId,
    ) -> ProgId {
        let next = kont(self);
        let this = &mut *self.as_mut();
        let id = this.prog_anon(Program::Instruction(instr.into(), next));
        id
    }
}
