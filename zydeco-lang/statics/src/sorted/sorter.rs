use crate::sorted::{err::*, syntax as so};
use crate::surface_syntax::{self as su, PrimDef, ScopedArena, SpanArena};
use zydeco_utils::arena::ArenaAssoc;

pub struct Sorter {
    pub spans: SpanArena,
    pub prim: PrimDef,
    pub scoped: ScopedArena,
    pub sorted: ArenaAssoc<so::TermId, so::Sort>,
}

impl Sorter {
    pub fn run(mut self) -> Result<()> {
        let mut top = self.scoped.top.clone();
        loop {
            let groups = top.top();
            if groups.is_empty() {
                break;
            }
            let mut sorted = Vec::new();
            for group in groups {
                for decl in group {
                    decl.sort(&mut self)?;
                    sorted.push(decl);
                }
            }
            top.release(sorted);
        }
        Ok(())
    }
}

pub trait Sort {
    type Out;
    fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out>;
}

impl Sort for su::DeclId {
    type Out = ();

    fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out> {
        let decl = sorter.scoped.decls[*self].clone();
        use su::Declaration as Decl;
        match decl {
            Decl::Alias(decl) => {
                let su::Alias { binder, bindee } = decl;
                binder.sort(sorter)?;
                bindee.sort(sorter)?;
            }
            Decl::Extern(decl) => {
                let su::Extern { comp: _, binder, params, ty } = decl;
                binder.sort(sorter)?;
                if let Some(params) = params {
                    params.sort(sorter)?
                } else {
                };
                if let Some(ty) = ty {
                    ty.sort(sorter)?
                } else {
                };
            }
            Decl::Main(decl) => {
                let su::Main(term) = decl;
                term.sort(sorter)?;
            }
        }
        Ok(())
    }
}

// impl Sort for su::DefId {
//     type Out = ();

//     fn sort(&self, _sorter: &mut Sorter) -> Result<Self::Out> {
//         Ok(())
//     }
// }

impl Sort for su::PatId {
    type Out = ();

    fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out> {
        todo!()
    }
}

impl Sort for su::CoPatId {
    type Out = ();

    fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out> {
        todo!()
    }
}

impl Sort for su::TermId {
    type Out = ();

    fn sort(&self, sorter: &mut Sorter) -> Result<Self::Out> {
        let term = &sorter.scoped.terms[*self];
        todo!()
    }
}
