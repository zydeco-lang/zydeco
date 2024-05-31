use crate::bitter::{desugar::Desugarer, syntax as b};

pub trait Alloc {
    type Entity;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self;
}

impl Alloc for b::DefId {
    type Entity = b::VarName;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self {
        desugarer.bitter.defs.alloc(entity)
    }
}
impl Alloc for b::PatId {
    type Entity = b::Pattern;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self {
        desugarer.bitter.pats.alloc(entity)
    }
}
impl Alloc for b::TermId {
    type Entity = b::Term<b::NameRef<b::VarName>>;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self {
        desugarer.bitter.terms.alloc(entity)
    }
}
impl Alloc for b::DeclId {
    type Entity = b::Modifiers<b::Declaration>;
    fn alloc(desugarer: &mut Desugarer, entity: Self::Entity) -> Self {
        desugarer.bitter.decls.alloc(entity)
    }
}
