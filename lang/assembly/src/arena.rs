use super::syntax::*;
use zydeco_derive::{AsMutSelf, AsRefSelf};
use zydeco_stackir::syntax as sk;

#[derive(AsRefSelf, AsMutSelf)]
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
    pub externs: Vec<Extern>,
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
            externs: Vec::new(),
            blocks: ArenaAssoc::new(),
            entry: ArenaAssoc::new(),
        }
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
        format!("{}_{}", this.symbols[&sym].name, sym.concise_inner().replace('#', "_"))
    }
    fn prog_label(&self, prog: &ProgId) -> Option<String> {
        let this = self.as_ref();
        this.labels.get(&prog).map(|sym| this.sym_label(sym))
    }
}

pub trait Construct<S, T, Arena>: Sized + Into<S> {
    type Site;
    fn build(self, arena: &mut Arena, site: Self::Site) -> T;
}

impl<U, Arena> Construct<VarName, VarId, Arena> for U
where
    Arena: AsMut<AssemblyArena>,
    U: Into<VarName>,
{
    type Site = Option<sk::DefId>;
    fn build(self, arena: &mut Arena, site: Self::Site) -> VarId {
        let this = &mut *arena.as_mut();
        let id = this.variables.alloc(self.into());
        if let Some(site) = site {
            this.defs.insert(site, DefId::Var(id));
        }
        id
    }
}

impl<U, Arena> Construct<SymbolInner, SymId, Arena> for U
where
    Arena: AsMut<AssemblyArena>,
    U: Into<SymbolInner>,
{
    type Site = (Option<String>, Option<sk::DefId>);
    fn build(self, arena: &mut Arena, (name, site): Self::Site) -> SymId {
        let this = &mut *arena.as_mut();
        let symbol = Symbol { name: name.unwrap_or_default(), inner: self.into() };
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
}

/// Allocate a program that is anonymous, i.e. has no meaningful label.
impl<U, Arena> Construct<Program, ProgId, Arena> for U
where
    Arena: AsMut<AssemblyArena>,
    U: Into<Program>,
{
    type Site = ();
    fn build(self, arena: &mut Arena, (): Self::Site) -> ProgId {
        let this = &mut *arena.as_mut();
        let program = self.into();
        // Nominate inner programs if they are mentioned in the program.
        let mut nominate = |&prog| {
            *this.blocks.entry(prog).or_insert(0) += 1;
        };
        match &program {
            | Program::Instruction(_, prog_id) => nominate(prog_id),
            | Program::Jump(Jump(prog_id)) => nominate(prog_id),
            | Program::PopJump(PopJump) => {}
            | Program::LeapJump(LeapJump) => {}
            | Program::PopBranch(PopBranch(brs)) => {
                for (_, prog_id) in brs {
                    nominate(prog_id);
                }
            }
            | Program::Extern(Extern { .. }) => {}
            | Program::Panic(Panic) => {}
        }
        let id = this.programs.alloc(program);
        id
    }
}

impl<U, Arena> Construct<Instruction, ProgId, Arena> for U
where
    Arena: AsMut<AssemblyArena>,
    U: Into<Instruction>,
{
    /// The continuation.
    type Site = Box<dyn FnOnce(&mut Arena) -> ProgId>;
    fn build(self, arena: &mut Arena, kont: Self::Site) -> ProgId {
        let instr = self.into();
        let next = kont(arena);
        let id = Program::Instruction(instr, next).build(arena, ());
        id
    }
}
