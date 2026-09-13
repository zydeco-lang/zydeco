use crate::bitter::syntax as b;
use crate::textual::syntax as t;
use derive_more::{AsMut, AsRef};
use zydeco_utils::prelude::{FrozenArena, IdAllocator};

/// Owns bitter allocation and provenance independently of a lowering operation.
#[derive(Debug, AsRef, AsMut)]
pub struct BitterBuilder {
    allocator: IdAllocator<b::BitterScope>,
    #[as_ref]
    #[as_mut]
    pub arena: b::BitterArena,
}

impl Default for BitterBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl BitterBuilder {
    pub fn new() -> Self {
        Self { allocator: IdAllocator::new(), arena: b::BitterArena::default() }
    }

    pub fn finish(self) -> FrozenArena<b::BitterArena> {
        FrozenArena::new(self.arena)
    }
}

pub trait Alloc {
    type Entity;
    type Prev;
    fn alloc(builder: &mut BitterBuilder, entity: Self::Entity, prev: Self::Prev) -> Self;
}

impl Alloc for b::DefId {
    type Entity = b::VarName;
    type Prev = t::EntityId;
    fn alloc(builder: &mut BitterBuilder, entity: Self::Entity, prev: Self::Prev) -> Self {
        let curr = builder.allocator.alloc();
        builder.arena.defs.insert_new(curr, entity);
        builder.arena.origins.insert_new(prev, curr.into());
        curr
    }
}
impl Alloc for b::PatId {
    type Entity = b::Pattern;
    type Prev = t::EntityId;
    fn alloc(builder: &mut BitterBuilder, entity: Self::Entity, prev: Self::Prev) -> Self {
        let curr = builder.allocator.alloc();
        builder.arena.pats.insert_new(curr, entity);
        builder.arena.origins.insert_new(prev, curr.into());
        curr
    }
}
impl Alloc for b::TermId {
    type Entity = b::Term<b::VarName>;
    type Prev = t::EntityId;
    fn alloc(builder: &mut BitterBuilder, entity: Self::Entity, prev: Self::Prev) -> Self {
        let curr = builder.allocator.alloc();
        builder.arena.terms.insert_new(curr, entity);
        builder.arena.origins.insert_new(prev, curr.into());
        curr
    }
}
