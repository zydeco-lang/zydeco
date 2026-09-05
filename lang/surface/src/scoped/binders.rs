use crate::scoped::syntax::*;

/// Extract binder definitions introduced by a pattern.
pub trait Binders {
    type Arena;
    fn binders(&self, arena: &Self::Arena) -> rpds::HashTrieMapSync<VarName, DefId>;
}

impl Binders for PatId {
    type Arena = BitterArena;
    fn binders(&self, arena: &Self::Arena) -> rpds::HashTrieMapSync<VarName, DefId> {
        let pat = &arena.pats[self];
        match pat {
            | Pattern::Ann(pat) => {
                let Ann { tm, ty: _ } = pat;
                tm.binders(arena)
            }
            | Pattern::Hole(pat) => {
                let Hole = pat;
                rpds::HashTrieMapSync::new_sync()
            }
            | Pattern::Lit(_) => rpds::HashTrieMapSync::new_sync(),
            | Pattern::Triv(Triv) => rpds::HashTrieMapSync::new_sync(),
            | Pattern::Var(pat) => {
                let def = pat;
                rpds::HashTrieMapSync::new_sync().insert(arena.defs[def].clone(), *def)
            }
            | Pattern::Named(pat) => {
                let Named(_name, inner) = pat;
                inner.binders(arena)
            }
            | Pattern::Ctor(pat) => {
                let Ctor(_ctor, args) = pat;
                args.binders(arena)
            }
            | Pattern::Project(ProjectionPattern(_, pattern)) => pattern.binders(arena),
            | Pattern::View(ViewPattern { function: _, pattern }) => pattern.binders(arena),
            | Pattern::Alias(Alias(pat)) => {
                pat.iter().fold(rpds::HashTrieMapSync::new_sync(), |binders, item| {
                    item.binders(arena).iter().fold(binders, |binders, (name, definition)| {
                        binders.insert(name.clone(), *definition)
                    })
                })
            }
            | Pattern::Cons(pat) => {
                pat.iter().fold(rpds::HashTrieMapSync::new_sync(), |binders, item| {
                    item.binders(arena).iter().fold(binders, |binders, (name, definition)| {
                        binders.insert(name.clone(), *definition)
                    })
                })
            }
        }
    }
}
