use crate::scoped::syntax::*;

/// Extract binder definitions introduced by a pattern.
pub trait Binders {
    type Arena;
    fn binders(&self, arena: &Self::Arena) -> im::HashMap<VarName, DefId>;
}

impl Binders for PatId {
    type Arena = BitterArena;
    fn binders(&self, arena: &Self::Arena) -> im::HashMap<VarName, DefId> {
        let pat = &arena.pats[self];
        match pat {
            | Pattern::Ann(pat) => {
                let Ann { tm, ty: _ } = pat;
                tm.binders(arena)
            }
            | Pattern::Hole(pat) => {
                let Hole = pat;
                im::HashMap::new()
            }
            | Pattern::Triv(Triv) => im::HashMap::new(),
            | Pattern::Var(pat) => {
                let def = pat;
                im::hashmap! { arena.defs[def].clone() => *def }
            }
            | Pattern::Named(pat) => {
                let Named(_name, inner) = pat;
                inner.binders(arena)
            }
            | Pattern::Ctor(pat) => {
                let Ctor(_ctor, args) = pat;
                args.binders(arena)
            }
            | Pattern::Alias(Alias(pat)) | Pattern::Cons(pat) => pat
                .iter()
                .fold(im::HashMap::new(), |binders, item| binders.union(item.binders(arena))),
        }
    }
}
