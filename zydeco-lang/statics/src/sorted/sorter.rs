use crate::sorted::err::*;
use crate::surface_syntax::{self as sc, PrimDef, ScopedArena, SpanArena};
use crate::syntax::{self as ss, SortedArena};

pub struct Sorter {
    pub spans: SpanArena,
    pub prim: PrimDef,
    pub scoped: ScopedArena,
    pub sorted: SortedArena,
}

// impl Sorter {
//     pub fn run(mut self) -> Result<()> {
//         let scc = self.scoped.scc.clone();
//         loop {
//             let top = scc.top();
//             if top.is_empty() {
//                 break;
//             }
//             let mut sorted = Vec::new();
//             for group in top {
//                 for decl in group {
//                     decl.sort(&mut self)?;
//                     sorted.push(decl);
//                 }
//             }
//             scc.release(sorted);
//         }
//         Ok(())
//     }
// }

// pub trait Sort {
//     type Out;
//     fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out>;
// }

// impl Sort for sc::DeclId {
//     type Out = ();

//     fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out> {
//         let decl = &sorter.scoped.decls[*self];
//         use sc::Declaration as Decl;
//         match decl {
//             Decl::Alias(decl) => {
//                 let sc::Alias { binder, bindee  } = decl;
//                 binder.sort(sorter)?;
//                 bindee.sort(sorter)?;
//             }
//             Decl::Extern(decl) => {
//                 let sc::Extern { comp, binder, params, ty } = decl;
//                 binder.sort(sorter)?;
//                 for param in params {
//                     param.sort(sorter)?;
//                 }
//                 for ty in ty {
//                     ty.sort(sorter)?;
//                 }
//             }
//             Decl::Main(decl) => {
//                 let sc::Main(term) = decl;
//                 term.sort(sorter)?;
//             }
//         }
//         Ok(())
//     }
// }

// impl Sort for sc::DefId {
//     type Out = ss::DefId;

//     fn sort(&self, _sorter: &mut Sorter) -> Result<Self::Out> {
//         Ok(*self)
//     }
// }

// impl Sort for sc::PatId {
//     type Out;

//     fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out> {
//         todo!()
//     }
// }

// impl Sort for sc::CoPatId {
//     type Out;

//     fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out> {
//         todo!()
//     }
// }

// impl Sort for sc::TermId {
//     type Out;

//     fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out> {
//         let term = &sorter.surface.terms[*self];
//     }
// }
