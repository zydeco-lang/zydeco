//! Owning arenas and constructors for high Stack IR.

use super::syntax::*;
use crate::static_syntax as ss;
use derive_more::{AsMut, AsRef};
use zydeco_derive::{AsMutSelf, AsRefSelf};
use zydeco_surface::scoped::arena::ScopedScope;

/// Allocation scope for high Stack IR nodes and synthetic scoped definitions.
#[derive(Debug)]
pub enum SpsScope {}

impl Allocates<VPatId> for SpsScope {}
impl Allocates<ValueId> for SpsScope {}
impl Allocates<StackId> for SpsScope {}
impl Allocates<CompuId> for SpsScope {}
impl Allocates<DefId> for SpsScope {}

/// Generated definition names and typed-source provenance for high Stack IR.
#[derive(Debug, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct AdminArena {
    /// ID allocator shared by all stack-IR node categories.
    pub(crate) allocator: IdAllocator<SpsScope>,

    /// Names introduced by Stack IR lowering. Source and typed-elaboration
    /// names stay in their immutable phase arenas.
    pub defs: ArenaSparse<ScopedScope, DefId>,

    /// One source pattern may originate multiple generated ZIR patterns; every
    /// generated pattern has at most one source pattern.
    pub pats: ArenaForth<ss::PatId, VPatId>,
    /// One source term may originate multiple generated ZIR nodes; every
    /// generated node has at most one source term.
    pub terms: ArenaForth<ss::TermId, TermId>,
}

impl AdminArena {
    pub fn new() -> Self {
        Self {
            allocator: IdAllocator::new(),
            defs: ArenaSparse::default(),
            pats: ArenaForth::new(),
            terms: ArenaForth::new(),
        }
    }

    pub(crate) fn fresh<Id>(&mut self) -> Id
    where
        Id: ArenaId,
        SpsScope: Allocates<Id>,
    {
        self.allocator.alloc()
    }

    pub(crate) fn insert_def(&mut self, id: DefId, name: VarName) {
        self.defs.insert_new(id, name);
    }
}

impl DefinitionNames for AdminArena {
    fn generated_defs(&self) -> &ArenaSparse<ScopedScope, DefId> {
        &self.defs
    }
}

impl Default for AdminArena {
    fn default() -> Self {
        Self::new()
    }
}

impl ArenaSchema<VPatId> for SpsScope {
    type Item = ValuePattern;
}
impl ArenaSchema<ValueId> for SpsScope {
    type Item = Value;
}
impl ArenaSchema<StackId> for SpsScope {
    type Item = Stack;
}
impl ArenaSchema<CompuId> for SpsScope {
    type Item = Computation<LetJoin>;
}

/// All arenas for the stack-passing style ZIR.
/// The definitions and patterns are equivalent to the ones in
/// [`zydeco_statics::arena::StaticsArena`].
#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SpsArena {
    /// administrative arena
    #[as_ref]
    #[as_mut]
    pub admin: AdminArena,

    /// inner arena that stores the nodes and associated properties
    #[as_ref]
    #[as_mut]
    pub inner: SpsInnerArena,
}

#[derive(Debug, Default, AsRef, AsMut, AsRefSelf, AsMutSelf)]
pub struct SpsInnerArena {
    pub protocols: std::sync::Arc<crate::protocol::ProtocolGraph>,
    /// Closed compiler-defined builtin bodies may be exposed at every known call.
    /// This is compiler provenance, not an annotation on arbitrary source closures.
    pub(crate) builtin_functions: ArenaAssoc<ValueId, BuiltinValueRole>,
    pub value_protocols: ArenaAssoc<ValueId, crate::protocol::ValueProtocol>,
    pub pattern_protocols: ArenaAssoc<VPatId, crate::protocol::ValueProtocol>,
    /// Intrinsic protocols of recursive entries and codata eliminations.
    pub compu_protocols: ArenaAssoc<CompuId, crate::protocol::StackProtocol>,
    /// value pattern arena
    pub vpats: ArenaSparse<SpsScope, VPatId>,
    /// value arena
    pub values: ArenaSparse<SpsScope, ValueId>,
    /// stack arena
    pub stacks: ArenaSparse<SpsScope, StackId>,
    /// computation arena
    pub compus: ArenaSparse<SpsScope, CompuId>,
}

/// A complete Stack IR program with one computation at its top level.
#[derive(Debug)]
pub struct SpsProgram {
    arena: FrozenArena<SpsArena>,
    root: CompuId,
}

/// Read-only source storage and empty target storage for a structural rebuild.
pub(crate) struct SpsRebuild {
    pub source: SpsArena,
    pub target: SpsArena,
    pub root: CompuId,
}

impl SpsProgram {
    pub fn new(arena: SpsArena, root: CompuId) -> Self {
        Self { arena: FrozenArena::new(arena), root }
    }

    pub fn arena(&self) -> &SpsArena {
        &self.arena
    }

    pub fn root(&self) -> CompuId {
        self.root
    }

    pub(crate) fn into_rebuild(self) -> SpsRebuild {
        let Self { arena, root } = self;
        let SpsArena { mut admin, inner } = arena.into_inner();
        let source_admin = AdminArena {
            pats: std::mem::take(&mut admin.pats),
            terms: std::mem::take(&mut admin.terms),
            ..AdminArena::default()
        };
        let protocols = inner.protocols.clone();
        SpsRebuild {
            source: SpsArena { admin: source_admin, inner },
            target: SpsArena { admin, inner: SpsInnerArena { protocols, ..Default::default() } },
            root,
        }
    }
}

impl<U, Arena> Construct<ValuePattern, VPatId, Arena> for U
where
    Arena: AsMut<SpsArena>,
    U: Into<ValuePattern>,
{
    type Site = ss::PatId;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> VPatId {
        let this = &mut *arena.as_mut();
        let vpat_id = this.admin.fresh();
        this.inner.vpats.insert_new(vpat_id, self.into());
        if let Some(site) = site {
            this.admin.pats.insert_new(site, vpat_id);
        }
        vpat_id
    }
}

impl<U, Arena> Construct<Value, ValueId, Arena> for U
where
    Arena: AsMut<SpsArena>,
    U: Into<Value>,
{
    type Site = ss::TermId;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> ValueId {
        let this = &mut *arena.as_mut();
        let value_id = this.admin.fresh();
        this.inner.values.insert_new(value_id, self.into());
        if let Some(site) = site {
            this.admin.terms.insert_new(site, TermId::Value(value_id));
        }
        value_id
    }
}

impl<U, Arena> Construct<Stack, StackId, Arena> for U
where
    Arena: AsMut<SpsArena>,
    U: Into<Stack>,
{
    type Site = ss::TermId;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> StackId {
        let this = &mut *arena.as_mut();
        let stack_id = this.admin.fresh();
        this.inner.stacks.insert_new(stack_id, self.into());
        if let Some(site) = site {
            this.admin.terms.insert_new(site, TermId::Stack(stack_id));
        }
        stack_id
    }
}

impl<U, Arena> Construct<Computation<LetJoin>, CompuId, Arena> for U
where
    Arena: AsMut<SpsArena>,
    U: Into<Computation<LetJoin>>,
{
    type Site = ss::TermId;
    fn build(self, arena: &mut Arena, site: Option<Self::Site>) -> CompuId {
        let this = &mut *arena.as_mut();
        let compu_id = this.admin.fresh();
        this.inner.compus.insert_new(compu_id, self.into());
        if let Some(site) = site {
            this.admin.terms.insert_new(site, TermId::Compu(compu_id));
        }
        compu_id
    }
}
