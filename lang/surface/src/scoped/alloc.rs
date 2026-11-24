//! Scoped syntax allocation, provenance tracking, and CBPV primitives.

use super::syntax::*;
use crate::textual::syntax as t;
use std::collections::HashSet;
use zydeco_foundation::prelude::IdAllocator;

pub(super) struct ScopedBuilder {
    allocator: IdAllocator<ScopedScope>,
    pub origins: TextualOrigins,
    pub defs: ArenaSparse<ScopedScope, DefId>,
    pub pats: ArenaIndexed<ScopedScope, PatId>,
    pub terms: ArenaIndexed<ScopedScope, TermId>,
    pub blocks: ArenaAssoc<TermId, ContextualTerm<BindingContext, BlockBody>>,
    pub partial_binders: HashSet<t::PatId>,
    pub default_prelude_defs: rustc_hash::FxHashSet<DefId>,
}

pub(super) enum Introduction {
    Thunk,
    Return,
}

impl ScopedBuilder {
    pub fn new() -> Self {
        Self {
            allocator: IdAllocator::new(),
            origins: TextualOrigins::default(),
            defs: ArenaSparse::default(),
            pats: ArenaIndexed::default(),
            terms: ArenaIndexed::default(),
            blocks: ArenaAssoc::default(),
            partial_binders: HashSet::new(),
            default_prelude_defs: Default::default(),
        }
    }

    pub fn alloc_default_prelude_def(&mut self, name: VarName) -> DefId {
        let id: DefId = self.allocator.alloc();
        self.origins.insert_synthetic(id.into());
        self.defs.insert_new(id, name);
        self.default_prelude_defs.insert(id);
        id
    }

    pub fn alloc_def(&mut self, name: VarName, origin: t::EntityId) -> DefId {
        let id: DefId = self.allocator.alloc();
        self.origins.insert_new(origin, id.into());
        self.defs.insert_new(id, name);
        id
    }

    pub fn alloc_pat(&mut self, pat: Pattern, origin: t::EntityId) -> PatId {
        let id: PatId = self.allocator.alloc();
        self.origins.insert_new(origin, id.into());
        self.pats.insert_new(id, pat);
        id
    }

    pub fn reserve_term(&mut self, origin: t::EntityId) -> TermId {
        let id: TermId = self.allocator.alloc();
        self.origins.insert_new(origin, id.into());
        id
    }

    pub fn fill_term(&mut self, id: TermId, term: Term) {
        self.terms.insert_new(id, term);
    }

    pub fn alloc_term(&mut self, term: impl Into<Term>, origin: t::EntityId) -> TermId {
        let id = self.reserve_term(origin);
        self.fill_term(id, term.into());
        id
    }

    pub fn alloc_synthetic_term(&mut self, term: impl Into<Term>) -> TermId {
        let id: TermId = self.allocator.alloc();
        self.origins.insert_synthetic(id.into());
        self.terms.insert_new(id, term.into());
        id
    }

    pub fn alloc_term_from_origin_of(&mut self, source: TermId, term: impl Into<Term>) -> TermId {
        let origin = self.origins.source(&source.into()).expect("a source term retains its origin");
        self.alloc_term(term, origin)
    }

    pub fn introduce(
        &mut self, introduction: Introduction, body: TermId, origin: t::EntityId,
    ) -> TermId {
        let term: Term = match introduction {
            | Introduction::Thunk => Thunk(body).into(),
            | Introduction::Return => Return(body).into(),
        };
        let tm = self.alloc_term(term, origin);
        let constructor = match introduction {
            | Introduction::Thunk => self.thunk(origin),
            | Introduction::Return => self.ret(origin),
        };
        let hole = self.alloc_term(Hole, origin);
        let ty = self.alloc_term(App(constructor, vec![hole]), origin);
        self.alloc_term(Ann { tm, ty }, origin)
    }

    pub fn monadic(
        &mut self, body: TermId, monad: TermId, algebra: TermId, origin: t::EntityId,
    ) -> TermId {
        self.alloc_term(MoBlock { body, basis: MonadicBasis { monad, algebra } }, origin)
    }

    /// Build a call-by-value block node over a `Block` boundary term.
    pub fn cbv(&mut self, block_body: TermId, origin: t::EntityId) -> TermId {
        self.alloc_term(CbvBlock { body: block_body }, origin)
    }

    pub fn intrinsic(&mut self, role: IntrinsicRole, origin: t::EntityId) -> TermId {
        match role {
            | IntrinsicRole::VType => self.vtype(origin),
            | IntrinsicRole::CType => self.ctype(origin),
            | IntrinsicRole::Thk => self.thunk(origin),
            | IntrinsicRole::Ret => self.ret(origin),
            | IntrinsicRole::Unit => self.unit(origin),
            | IntrinsicRole::Primitive(primitive) => self.primitive(primitive, origin),
            | IntrinsicRole::ValueInt(operation) => {
                self.alloc_term(Internal::ValueInt(operation), origin)
            }
            | IntrinsicRole::Operation(operation) => {
                self.alloc_term(Internal::Operation(operation), origin)
            }
        }
    }

    pub fn primitive(&mut self, primitive: PrimitiveType, origin: t::EntityId) -> TermId {
        self.alloc_term(Internal::Primitive(primitive), origin)
    }

    pub fn vtype(&mut self, origin: t::EntityId) -> TermId {
        self.alloc_term(Internal::VType, origin)
    }

    pub fn ctype(&mut self, origin: t::EntityId) -> TermId {
        self.alloc_term(Internal::CType, origin)
    }

    pub fn thunk(&mut self, origin: t::EntityId) -> TermId {
        self.alloc_term(Internal::Thk, origin)
    }

    pub fn ret(&mut self, origin: t::EntityId) -> TermId {
        self.alloc_term(Internal::Ret, origin)
    }

    pub fn unit(&mut self, origin: t::EntityId) -> TermId {
        self.alloc_term(Internal::Unit, origin)
    }

    pub fn finish(self, users: ArenaForth<DefId, TermId>) -> ScopedArena {
        ScopedArena {
            defs: self.defs,
            pats: self.pats,
            terms: self.terms,
            blocks: self.blocks,
            origins: self.origins,
            users,
            partial_binders: self.partial_binders,
            default_prelude_defs: self.default_prelude_defs,
        }
    }
}
