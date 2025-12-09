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
    pub defs: ArenaBijective<zydeco_stack::syntax::DefId, DefId>,

    /// Programs are (optionally) labeled.
    pub labels: ArenaAssoc<ProgId, Label>,
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
            entry: ArenaAssoc::new(),
        }
    }
}
impl AsMut<AssemblyArena> for AssemblyArena {
    fn as_mut(&mut self) -> &mut AssemblyArena {
        self
    }
}

pub trait AssemblyArenaLike {
    /// Allocate a variable.
    fn var(&mut self, site: Option<sk::DefId>, var: impl Into<VarName>) -> VarId;
    /// Allocate a symbol.
    fn sym(&mut self, site: Option<sk::DefId>, sym: impl Into<Symbol>) -> SymId;
    /// Allocate a program that is named, i.e. has a meaningful label
    fn prog_named(&mut self, prog: impl Into<Program>, label: Label) -> ProgId;
    /// Allocate a program that is anonymous, i.e. has no meaningful label
    fn prog_anon(&mut self, prog: impl Into<Program>) -> ProgId;
    /// Allocate a fix point program, e.g. a recursive function.
    fn prog_fix_point(
        &mut self, prog: impl FnOnce(ProgId, &mut Self) -> Program, label: Label,
    ) -> ProgId;
    /// Allocate an instruction.
    fn instr(
        &mut self, instr: impl Into<Instruction>, kont: impl FnOnce(&mut Self) -> ProgId,
    ) -> ProgId;
}

impl<T> AssemblyArenaLike for T
where
    T: AsMut<AssemblyArena>,
{
    fn var(&mut self, site: Option<sk::DefId>, var: impl Into<VarName>) -> VarId {
        let this = &mut *self.as_mut();
        let id = this.variables.alloc(var.into());
        if let Some(site) = site {
            this.defs.insert(site, DefId::Var(id));
        }
        id
    }
    fn sym(&mut self, site: Option<sk::DefId>, sym: impl Into<Symbol>) -> SymId {
        let this = &mut *self.as_mut();
        let id = this.symbols.alloc(sym.into());
        if let Some(site) = site {
            this.defs.insert(site, DefId::Sym(id));
        }
        id
    }
    fn prog_named(&mut self, prog: impl Into<Program>, label: Label) -> ProgId {
        let this = &mut *self.as_mut();
        let id = this.programs.alloc(prog.into());
        this.labels.insert(id, label);
        id
    }
    fn prog_anon(&mut self, prog: impl Into<Program>) -> ProgId {
        let this = &mut *self.as_mut();
        let id = this.programs.alloc(prog.into());
        id
    }
    fn prog_fix_point(
        &mut self, prog: impl FnOnce(ProgId, &mut Self) -> Program, label: Label,
    ) -> ProgId {
        let this = &mut *self.as_mut();
        let id = this.programs.alloc(Program::Panic(Panic));
        let prog = prog(id, self);
        let this = &mut *self.as_mut();
        this.labels.insert(id, label);
        this.programs[&id] = prog;
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
