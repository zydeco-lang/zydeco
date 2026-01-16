use super::syntax::*;
use zydeco_derive::{AsMutSelf, AsRefSelf};
use zydeco_stackir::syntax as sk;
use zydeco_utils::{graph::DepGraph, with::With};

#[derive(AsRefSelf, AsMutSelf)]
pub struct AssemblyArena {
    /// All programs are attached with a ProgId.
    pub programs: ArenaSparse<ProgId, Program>,
    /// All variables are named.
    pub variables: ArenaSparse<VarId, VarName>,
    /// All symbols are named.
    pub symbols: ArenaSparse<SymId, NamedSymbol>,

    /// Map from DefId to VarId or SymId
    pub defs: ArenaBijective<sk::DefId, DefId>,

    /// All programs have a context that they depend on.
    pub contexts: ArenaAssoc<ProgId, Context>,
    /// Dependencies of programs; LHS depends on all RHSs.
    ///
    /// In our case, a program depends on all the programs that jumps to it.
    pub deps: DepGraph<ProgId>,
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
            contexts: ArenaAssoc::new(),
            deps: DepGraph::new(),
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
        format!("{}_{}", this.symbols[sym].name, sym.concise_inner().replace('#', "_"))
    }
    fn prog_label(&self, prog: &ProgId) -> Option<String> {
        let this = self.as_ref();
        this.labels.get(prog).map(|sym| this.sym_label(sym))
    }
}

pub trait Construct<'a, S, T, Arena>: Sized + Into<S> {
    type Site;
    fn build<'f: 'a>(self, arena: &'f mut Arena, site: Self::Site) -> T;
}

impl<'a, U, Arena> Construct<'a, VarName, VarId, Arena> for U
where
    Arena: AsMut<AssemblyArena>,
    U: Into<VarName>,
{
    type Site = Option<sk::DefId>;
    fn build<'f: 'a>(self, arena: &'f mut Arena, site: Self::Site) -> VarId {
        let this = &mut *arena.as_mut();
        let id = this.variables.alloc(self.into());
        if let Some(site) = site {
            this.defs.insert(site, DefId::Var(id));
        }
        id
    }
}

impl<'a, U, Arena> Construct<'a, Symbol, SymId, Arena> for U
where
    Arena: AsMut<AssemblyArena>,
    U: Into<Symbol>,
{
    type Site = (Option<String>, Option<sk::DefId>);
    fn build<'f: 'a>(self, arena: &'f mut Arena, (name, site): Self::Site) -> SymId {
        let this = &mut *arena.as_mut();
        let symbol = NamedSymbol { name: name.unwrap_or_default(), inner: self.into() };
        let is_prog = match symbol.inner {
            | Symbol::Prog(prog_id) => Some(prog_id),
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

pub type Kont<'a, Arena> = Box<dyn for<'b> FnOnce(&'b mut Arena, Context) -> ProgId + 'a>;

pub struct CxKont<'a, Arena> {
    pub incr: Box<dyn FnOnce(&Context) -> Context>,
    pub kont: Kont<'a, Arena>,
}

impl<'a, Arena> CxKont<'a, Arena> {
    /// No new binders are created, make [`Self::incr`] to be the identity function.
    pub fn same(kont: Kont<'a, Arena>) -> Self {
        Self { incr: Box::new(|cx| cx.clone()), kont }
    }
    /// Start with a clean slate.
    pub fn clean(kont: Kont<'a, Arena>) -> Self {
        Self { incr: Box::new(|_: &Context| Context::new()), kont }
    }
}

/// Allocate a program that is anonymous, i.e. has no meaningful label.
impl<'a, U, Arena> Construct<'a, Program, ProgId, Arena> for U
where
    Arena: AsMut<AssemblyArena>,
    U: Into<Program>,
{
    type Site = Context;
    fn build<'f: 'a>(self, arena: &'f mut Arena, cx: Self::Site) -> ProgId {
        let this = &mut *arena.as_mut();
        let program = self.into();
        // Nominate inner programs if they are mentioned in the program.
        let mut nominate = |&prog| {
            *this.blocks.entry(prog).or_insert(0) += 1;
        };
        match &program {
            | Program::Instruction(_, prog_id) => nominate(prog_id),
            | Program::Terminator(t) => match t {
                | Terminator::Jump(Jump(prog_id)) => nominate(prog_id),
                | Terminator::PopJump(PopJump) => {}
                | Terminator::LeapJump(LeapJump) => {}
                | Terminator::PopBranch(PopBranch(brs)) => {
                    for (_, prog_id) in brs {
                        nominate(prog_id);
                    }
                }
                | Terminator::Extern(Extern { .. }) => {}
                | Terminator::Abort(Abort) => {}
            },
        }
        let id = this.programs.alloc(program.clone());
        this.contexts.insert(id, cx);

        // Update dependencies.
        match program {
            | Program::Instruction(_, prog_id) => this.deps.add(prog_id, [id]),
            | Program::Terminator(t) => match t {
                | Terminator::Jump(Jump(prog_id)) => this.deps.add(id, [prog_id]),
                | Terminator::PopJump(PopJump) => {}
                | Terminator::LeapJump(LeapJump) => {}
                | Terminator::PopBranch(PopBranch(brs)) => {
                    for (_, prog_id) in brs {
                        this.deps.add(id, [prog_id]);
                    }
                }
                | Terminator::Extern(Extern { .. }) => {}
                | Terminator::Abort(Abort) => {}
            },
        }

        id
    }
}

impl<'a, U, Arena> Construct<'a, Terminator, ProgId, Arena> for U
where
    Arena: AsMut<AssemblyArena>,
    U: Into<Terminator>,
{
    type Site = Context;
    fn build<'f: 'a>(self, arena: &'f mut Arena, cx: Self::Site) -> ProgId {
        let terminator = self.into();
        Program::Terminator(terminator).build(arena, cx)
    }
}

impl<'a, U, Arena> Construct<'a, Instruction, ProgId, Arena> for U
where
    Arena: AsMut<AssemblyArena> + 'a,
    U: Into<Instruction>,
{
    /// The continuation.
    type Site = With<Context, CxKont<'a, Arena>>;
    fn build<'f: 'a>(
        self, arena: &'f mut Arena, With { info: cx, inner: CxKont { incr, kont } }: Self::Site,
    ) -> ProgId {
        let instr = self.into();
        let cx_incr = incr(&cx);
        let next = kont(arena, cx_incr);
        Program::Instruction(instr, next).build(arena, cx)
    }
}
