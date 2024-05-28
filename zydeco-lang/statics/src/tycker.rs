use crate::err::*;
use crate::surface_syntax::{self as su, PrimDef, ScopedArena, SpanArena};
use crate::syntax as so;
use std::collections::HashSet;
use zydeco_utils::arena::ArenaAssoc;

pub struct Tycker {
    pub spans: SpanArena,
    pub prim: PrimDef,
    pub scoped: ScopedArena,
    // pub sorted: ArenaAssoc<so::TermId, so::Sort>,
}

impl Tycker {
    pub fn run(mut self) -> Result<()> {
        let mut top = self.scoped.top.clone();
        loop {
            let groups = top.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                SccDeclarations(&group).tyck(&mut self)?;
                top.release(group);
            }
        }
        Ok(())
    }
}

pub trait Tyck {
    type Out;
    fn tyck(&self, tycker: &mut Tycker) -> Result<Self::Out>;
}

pub enum Step<In, Out> {
    Syn(In),
    Ana(In, Out),
    Done(Out),
}

pub struct SccDeclarations<'decl>(pub &'decl HashSet<so::DeclId>);
impl<'decl> Tyck for SccDeclarations<'decl> {
    type Out = ();
    
    fn tyck(&self, tycker: &mut Tycker) -> Result<Self::Out> {
        let SccDeclarations(decls) = self;
        match decls.len() {
            0 => unreachable!(),
            1 => {
                // just synthesize
            }
            _ => {
                // mutually recursive declarations must..
                // 1. all be types, and
                // 2. all have kind annotations
            }
        }
        todo!()
    }
}

// impl Tyck for su::DeclId {
//     type Out = ();

//     fn sort(&self, sorter: &mut Tycker) -> Result<Self::Out> {
//         let decl = sorter.scoped.decls[*self].clone();
//         use su::Declaration as Decl;
//         match decl {
//             Decl::Alias(decl) => {
//                 let su::Alias { binder, bindee } = decl;
//                 binder.sort(sorter)?;
//                 bindee.sort(sorter)?;
//             }
//             Decl::Extern(decl) => {
//                 let su::Extern { comp: _, binder, params, ty } = decl;
//                 binder.sort(sorter)?;
//                 if let Some(params) = params {
//                     params.sort(sorter)?;
//                 } else {
//                 };
//                 if let Some(ty) = ty {
//                     ty.sort(sorter)?;
//                 } else {
//                 };
//             }
//             Decl::Main(decl) => {
//                 let su::Main(term) = decl;
//                 term.sort(sorter)?;
//             }
//         }
//         Ok(())
//     }
// }

// // impl Sort for su::DefId {
// //     type Out = ();

// //     fn sort(&self, _sorter: &mut Sorter) -> Result<Self::Out> {
// //         Ok(())
// //     }
// // }

// impl Tyck for su::PatId {
//     type Out = ();

//     fn sort(&self, sorter: &mut Tycker) -> Result<Self::Out> {
//         let pat = sorter.scoped.pats[*self].clone();
//         use su::Pattern as Pat;
//         match pat {
//             Pat::Ann(pat) => {
//                 let su::Ann { tm, ty } = pat;
//                 tm.sort(sorter)?;
//                 ty.sort(sorter)?;
//             }
//             Pat::Hole(pat) => {
//                 let su::Hole = pat;
//             }
//             Pat::Var(_def) => {}
//             Pat::Ctor(pat) => {
//                 let su::Ctor(_ctor, tail) = pat;
//                 tail.sort(sorter)?;
//             }
//             Pat::Paren(pat) => {
//                 let su::Paren(pats) = pat;
//                 for pat in pats {
//                     pat.sort(sorter)?;
//                 }
//             }
//         }
//         Ok(())
//     }
// }

// impl Tyck for su::CoPatId {
//     type Out = ();

//     fn sort(&self, sorter: &mut Tycker) -> Result<Self::Out> {
//         let copat = sorter.scoped.copats[*self].clone();
//         use su::CoPattern as CoPat;
//         match copat {
//             CoPat::Pat(pat) => {
//                 pat.sort(sorter)?;
//             }
//             CoPat::Dtor(_dtor) => {}
//             CoPat::App(copats) => {
//                 let su::App(copats) = copats;
//                 for copat in copats {
//                     copat.sort(sorter)?;
//                 }
//             }
//         }
//         Ok(())
//     }
// }

// impl Tyck for su::TermId {
//     type Out = ();

//     fn sort(&self, sorter: &mut Tycker) -> Result<Self::Out> {
//         let term = sorter.scoped.terms[*self].clone();
//         use su::Term as Tm;
//         match term {
//             Tm::Internal(_) => unreachable!(),
//             Tm::Sealed(term) => {
//                 let su::Sealed(term) = term;
//                 let sort = term.sort(sorter)?;
//                 // sorter.sorted.insert(*self, sort);
//                 Ok(sort)
//             }
//             Tm::Ann(term) => {
//                 let su::Ann { tm, ty } = term;
//                 tm.sort(sorter)?;
//                 ty.sort(sorter)?;
//                 // Ok(so::Sort::Type)
//                 Ok(())
//             }
//             Tm::Hole(term) => {
//                 todo!()
//             }
//             Tm::Var(term) => {
//                 todo!()
//             }
//             Tm::Paren(term) => {
//                 todo!()
//             }
//             Tm::Abs(term) => {
//                 todo!()
//             }
//             Tm::App(term) => {
//                 todo!()
//             }
//             Tm::Rec(term) => {
//                 todo!()
//             }
//             Tm::Pi(term) => {
//                 todo!()
//             }
//             Tm::Sigma(term) => {
//                 todo!()
//             }
//             Tm::Thunk(term) => {
//                 todo!()
//             }
//             Tm::Force(term) => {
//                 todo!()
//             }
//             Tm::Ret(term) => {
//                 todo!()
//             }
//             Tm::Do(term) => {
//                 todo!()
//             }
//             Tm::Let(term) => {
//                 todo!()
//             }
//             Tm::Data(term) => {
//                 todo!()
//             }
//             Tm::CoData(term) => {
//                 todo!()
//             }
//             Tm::Ctor(term) => {
//                 todo!()
//             }
//             Tm::Match(term) => {
//                 todo!()
//             }
//             Tm::CoMatch(term) => {
//                 todo!()
//             }
//             Tm::Dtor(term) => {
//                 todo!()
//             }
//             Tm::Lit(term) => {
//                 todo!()
//             }
//         }
//     }
// }
