use super::syntax::*;
use crate::textual::syntax as t;
use zydeco_utils::{deps::DepGraph, scc::SccGraph};

#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait ArenaScoped {
    fn def(&self, id: &DefId) -> VarName;
    fn pat(&self, id: &PatId) -> Pattern;
    fn term(&self, id: &TermId) -> Term<DefId>;
    fn decl(&self, id: &DeclId) -> Declaration;
}

#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait FoldingScoped<'er, Er: 'er + ArenaScoped, R> {
    fn def_pat(&self, er: Er, res: R) -> R;
    fn def_term(&self, er: Er, res: R) -> R;
    fn pat(&self, er: Er, item: Pattern, res: R) -> R;
    fn term(&self, er: Er, item: Term<DefId>, res: R) -> R;
    fn decl(&self, er: Er, item: Declaration, res: R) -> R;
}

/* ---------------------------------- Arena --------------------------------- */

#[derive(Debug)]
pub struct ScopedArena {
    // arenas
    pub defs: ArenaSparse<DefId, VarName>,
    pub pats: ArenaSparse<PatId, Pattern>,
    pub terms: ArenaSparse<TermId, Term<DefId>>,
    pub decls: ArenaSparse<DeclId, Declaration>,
    /// entity maps from textural syntax
    pub textual: ArenaForth<t::EntityId, EntityId>,

    /// def user map
    pub users: ArenaForth<DefId, TermId>,
    /// contexts upon terms
    pub ctxs: ArenaAssoc<TermId, Context<()>>,
    /// co-contexts upon terms
    pub coctxs: ArenaAssoc<TermId, CoContext<()>>,
    /// externs to defs
    pub exts: ArenaAssoc<DeclId, (Internal, DefId)>,
    /// non-(optionally-mutual-)recursive declarations
    pub unis: ArenaAssoc<DeclId, ()>,
    /// dependency graph of the top level declarations
    pub deps: DepGraph<DeclId>,
    /// scc graph of the top level declarations
    pub top: SccGraph<DeclId>,
}

impl ArenaScoped for ScopedArena {
    fn def(&self, id: &DefId) -> VarName {
        self.defs[id].to_owned()
    }
    fn pat(&self, id: &PatId) -> Pattern {
        self.pats[id].to_owned()
    }
    fn term(&self, id: &TermId) -> Term<DefId> {
        self.terms[id].to_owned()
    }
    fn decl(&self, id: &DeclId) -> Declaration {
        self.decls[id].to_owned()
    }
}

// pub trait PatternFirstFold<'er, Er: 'er + ArenaScoped> {}
// pub trait TermFirstFold<'er, Er: 'er + ArenaScoped>: Sized {
//     type Item;
//     fn fold<R, F>(self, er: Er, f: F, res: R) -> R
//     where
//         F: FoldingScoped<'er, Er, R>;
// }

// use super::Collector;

// impl ArenaScoped for Collector {
//     fn def(&self, id: &DefId) -> VarName {
//         self.defs[id].to_owned()
//     }
//     fn pat(&self, id: &PatId) -> Pattern {
//         self.pats[id].to_owned()
//     }
//     fn term(&self, id: &TermId) -> Term<DefId> {
//         self.terms[id].to_owned()
//     }
//     fn decl(&self, id: &DeclId) -> Declaration {
//         self.decls[id].to_owned()
//     }
// }

// impl<'er> TermFirstFold<'er, &'er mut Collector> for PatId {
//     type Item = Pattern;

//     fn fold<R, F>(self, er: &'er mut Collector, f: F, res: R) -> R
//     where
//         F: FoldingScoped<'er, &'er mut Collector, R>,
//     {
//         let item = er.pat(&self);
//         match item {
//             | Pattern::Ann(pat) => {
//                 let Ann { tm, ty } = pat;
//                 let res = ty.fold(er, f, res);
//                 tm.fold(er, f, res)
//             }
//             | Pattern::Hole(pat) => {
//                 let Hole = pat;
//                 res
//             }
//             | Pattern::Var(pat) => {
//                 let def = pat;
//                 todo!()
//             }
//             | Pattern::Ctor(pat) => {
//                 let Ctor(_ctorv, body) = pat;
//                 body.fold(er, f, res)
//             }
//             | Pattern::Triv(pat) => {
//                 let Triv = pat;
//                 res
//             }
//             | Pattern::Cons(pat) => {
//                 let Cons(a, b) = pat;
//                 let res = a.fold(er, f, res);
//                 b.fold(er, f, res)
//             }
//         }
//     }
// }

// impl<'er> TermFirstFold<'er, &'er mut Collector> for TermId {
//     type Item = Term<DefId>;

//     fn fold<R, F>(self, er: &'er mut Collector, f: F, res: R) -> R
//     where
//         F: FoldingScoped<'er, &'er mut Collector, R>,
//     {
//         let item = er.term(&self);
//         match item {
//             Term::Internal(term) => {}
//             Term::Sealed(term) => {}
//             Term::Ann(term) => {}
//             Term::Hole(term) => {}
//             Term::Var(term) => {}
//             Term::Triv(term) => {}
//             Term::Cons(term) => {}
//             Term::Abs(term) => {
//                 let Abs(param, body) = term;
//                 let res = body.fold(er, f);
//                 param.fold(er, f)
//             }
//             Term::App(term) => {}
//             Term::Fix(term) => {}
//             Term::Pi(term) => {}
//             Term::Sigma(term) => {}
//             Term::Thunk(term) => {}
//             Term::Force(term) => {}
//             Term::Ret(term) => {}
//             Term::Do(term) => {}
//             Term::Let(term) => {}
//             Term::MoBlock(term) => {}
//             Term::Data(term) => {}
//             Term::CoData(term) => {}
//             Term::Ctor(term) => {}
//             Term::Match(term) => {}
//             Term::CoMatch(term) => {}
//             Term::Dtor(term) => {}
//             Term::Lit(term) => {}
//         }
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use std::collections::HashMap;

//     #[test]
//     fn working_term_fold() {
//         let mut alloc = GlobalAlloc::new();
//         let deps = DepGraph::new();
//         let mut collector = Collector {
//             defs: ArenaSparse::new(alloc.alloc()),
//             pats: ArenaSparse::new(alloc.alloc()),
//             terms: ArenaSparse::new(alloc.alloc()),
//             decls: ArenaSparse::new(alloc.alloc()),
//             textual: ArenaForth::new(),
//             users: ArenaForth::new(),
//             ctxs: ArenaAssoc::new(),
//             coctxs: ArenaAssoc::new(),
//             unis: ArenaAssoc::new(),
//             top: SccGraph::new(&deps, HashMap::new()),
//             deps,
//         };
//         let name_correct = VarName("x".to_string());
//         let def = collector.defs.alloc(name_correct.to_owned());
//         let name = def.fold(&mut collector, |name, _er, _ress| name);
//         assert_eq!(name, name_correct)
//     }
// }
