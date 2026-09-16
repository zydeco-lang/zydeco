use super::syntax::*;
use zydeco_derive::{AsMutSelf, AsRefSelf};
use zydeco_stackir::low::syntax as sk;
use zydeco_utils::graph::DepGraph;

/// Allocation and owning storage scope for assembly nodes.
#[derive(Debug)]
pub enum AssemblyScope {}

impl Allocates<VarId> for AssemblyScope {}
impl Allocates<SymId> for AssemblyScope {}
impl Allocates<ProgId> for AssemblyScope {}

impl ArenaSchema<VarId> for AssemblyScope {
    type Item = VarName;
}
impl ArenaSchema<SymId> for AssemblyScope {
    type Item = NamedSymbol;
}
impl ArenaSchema<ProgId> for AssemblyScope {
    type Item = Program;
}

#[derive(Default, AsRefSelf, AsMutSelf)]
pub struct AssemblyArena {
    #[cfg(test)]
    pub(crate) publication_order: Vec<ProgId>,
    pub(crate) frame_entries: std::collections::BTreeMap<ProgId, crate::frames::Entry>,
    /// All programs are attached with a ProgId.
    pub programs: ArenaSparse<AssemblyScope, ProgId>,
    /// All variables are named.
    pub variables: ArenaSparse<AssemblyScope, VarId>,
    /// All symbols are named.
    pub symbols: ArenaSparse<AssemblyScope, SymId>,

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
}

/// A complete assembly program with one executable program root.
pub struct AssemblyProgram {
    arena: FrozenArena<AssemblyArena>,
    root: ProgId,
}

/// Mutable assembly construction state, kept inside the lowering pipeline.
pub(crate) struct AssemblyBuild {
    pub arena: AssemblyArena,
    pub root: ProgId,
}

impl AssemblyBuild {
    pub(crate) fn finish(self) -> AssemblyProgram {
        AssemblyProgram { arena: FrozenArena::new(self.arena), root: self.root }
    }
}

impl AssemblyProgram {
    pub fn arena(&self) -> &AssemblyArena {
        &self.arena
    }

    pub fn root(&self) -> ProgId {
        self.root
    }

    pub fn into_parts(self) -> (AssemblyArena, ProgId) {
        (self.arena.into_inner(), self.root)
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

impl AssemblyArena {
    pub(crate) fn insert_program(&mut self, id: ProgId, program: Program, context: Context) {
        #[cfg(test)]
        self.publication_order.push(id);
        self.programs.insert_new(id, program.clone());
        self.contexts.insert_new(id, context);

        match program {
            | Program::Instruction(_, next) => self.deps.add(next, [id]),
            | Program::Terminator(terminator) => match terminator {
                | Terminator::Jump(Jump(target)) => self.deps.add(id, [target]),
                | Terminator::PopJump(PopJump)
                | Terminator::Extern(
                    Extern::Host { .. } | Extern::Foreign(_) | Extern::Unit(_),
                )
                | Terminator::Abort(Abort) => {}
                | Terminator::Compare(CompareBranch { when_true, when_false, .. }) => {
                    self.deps.add(id, [when_true, when_false]);
                }
                | Terminator::PopBranch(PopBranch(branches)) => {
                    self.deps.add(id, branches.into_iter().map(|(_, target)| target));
                }
            },
        }
    }
}

pub trait Construct<'a, S, T, Arena>: Sized + Into<S> {
    type Site;
    fn build(self, arena: &mut Arena, site: Self::Site) -> T;
}

impl<'a, U, Arena> Construct<'a, VarName, VarId, Arena> for U
where
    Arena: AsMut<AssemblyArena> + AsMut<IdAllocator<AssemblyScope>>,
    U: Into<VarName>,
{
    type Site = Option<sk::DefId>;
    fn build(self, arena: &mut Arena, site: Self::Site) -> VarId {
        let id = AsMut::<IdAllocator<AssemblyScope>>::as_mut(arena).alloc();
        let this = AsMut::<AssemblyArena>::as_mut(arena);
        this.variables.insert_new(id, self.into());
        if let Some(site) = site {
            this.defs.insert_new(site, DefId::Var(id));
        }
        id
    }
}

impl<'a, U, Arena> Construct<'a, Symbol, SymId, Arena> for U
where
    Arena: AsMut<AssemblyArena> + AsMut<IdAllocator<AssemblyScope>>,
    U: Into<Symbol>,
{
    type Site = (Option<String>, Option<sk::DefId>);
    fn build(self, arena: &mut Arena, (name, site): Self::Site) -> SymId {
        let id = AsMut::<IdAllocator<AssemblyScope>>::as_mut(arena).alloc();
        let this = AsMut::<AssemblyArena>::as_mut(arena);
        let symbol = NamedSymbol { name: name.unwrap_or_default(), inner: self.into() };
        let is_prog = match symbol.inner {
            | Symbol::Prog(prog_id) => Some(prog_id),
            | _ => None,
        };
        this.symbols.insert_new(id, symbol);
        if let Some(prog_id) = is_prog {
            // Add a label to the program.
            this.labels.insert_new(prog_id, id);
        }
        if let Some(site) = site {
            this.defs.insert_new(site, DefId::Sym(id));
        }
        id
    }
}

/// Allocate a program that is anonymous, i.e. has no meaningful label.
impl<'a, U, Arena> Construct<'a, Program, ProgId, Arena> for U
where
    Arena: AsMut<AssemblyArena> + AsMut<IdAllocator<AssemblyScope>>,
    U: Into<Program>,
{
    type Site = Context;
    fn build(self, arena: &mut Arena, cx: Self::Site) -> ProgId {
        let id = AsMut::<IdAllocator<AssemblyScope>>::as_mut(arena).alloc();
        let this = AsMut::<AssemblyArena>::as_mut(arena);
        this.insert_program(id, self.into(), cx);
        id
    }
}

impl<'a, U, Arena> Construct<'a, Terminator, ProgId, Arena> for U
where
    Arena: AsMut<AssemblyArena> + AsMut<IdAllocator<AssemblyScope>>,
    U: Into<Terminator>,
{
    type Site = Context;
    fn build(self, arena: &mut Arena, cx: Self::Site) -> ProgId {
        let terminator = self.into();
        Program::Terminator(terminator).build(arena, cx)
    }
}
