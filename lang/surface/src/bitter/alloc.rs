use crate::bitter::{desugar::Desugarer, syntax as b};
use crate::textual::syntax as t;

pub trait Alloc {
    type Entity;
    type Prev;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity, prev: Self::Prev) -> Self;
}

impl Alloc for b::DefId {
    type Entity = b::VarName;
    type Prev = t::EntityId;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity, prev: Self::Prev) -> Self {
        let curr = desugarer.fresh();
        desugarer.bitter.defs.insert_new(curr, entity);
        desugarer.bitter.textual.insert_new(prev, curr.into());
        curr
    }
}
impl Alloc for b::PatId {
    type Entity = b::Pattern;
    type Prev = t::EntityId;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity, prev: Self::Prev) -> Self {
        let curr = desugarer.fresh();
        desugarer.bitter.pats.insert_new(curr, entity);
        desugarer.bitter.textual.insert_new(prev, curr.into());
        curr
    }
}
impl Alloc for b::TermId {
    type Entity = b::Term<b::VarName>;
    type Prev = t::EntityId;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity, prev: Self::Prev) -> Self {
        let curr = desugarer.fresh();
        desugarer.bitter.terms.insert_new(curr, entity);
        desugarer.bitter.textual.insert_new(prev, curr.into());
        curr
    }
}
impl Alloc for b::DeclId {
    type Entity = b::Modifiers<b::Declaration>;
    type Prev = t::EntityId;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity, prev: Self::Prev) -> Self {
        let curr = desugarer.fresh();
        desugarer.bitter.decls.insert_new(curr, entity);
        desugarer.bitter.textual.insert_new(prev, curr.into());
        curr
    }
}
