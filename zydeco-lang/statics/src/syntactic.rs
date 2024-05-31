use crate::*;

pub trait SyntacticallyAnnotated {
    fn syntactically_annotated(&self, tycker: &mut Tycker) -> Option<su::TermId>;
}
impl SyntacticallyAnnotated for su::Declaration {
    fn syntactically_annotated(&self, tycker: &mut Tycker) -> Option<su::TermId> {
        use su::Declaration as Decl;
        match self {
            | Decl::AliasBody(su::AliasBody { binder, bindee }) => {
                let _ = binder;
                bindee.syntactically_annotated(tycker)
            }
            | Decl::AliasHead(su::AliasHead { binder, ty }) => {
                let _ = binder;
                *ty
            }
            | Decl::Main(su::Main(_term)) => None,
        }
    }
}
impl SyntacticallyAnnotated for su::TermId {
    fn syntactically_annotated(&self, tycker: &mut Tycker) -> Option<su::TermId> {
        let term = tycker.scoped.terms[self].clone();
        use su::Term as Tm;
        match &term {
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(term) => {
                let su::Sealed(term) = term;
                term.syntactically_annotated(tycker)
            }
            | Tm::Ann(term) => {
                let su::Ann { tm: _, ty } = term;
                Some(*ty)
            }
            | Tm::Abs(term) => {
                let su::Abs(param, body) = term;
                // add param to pi
                // let span = tycker.spans.terms[body].clone();
                // let pi = tycker.spans.terms.alloc(span);
                let body = body.syntactically_annotated(tycker)?;
                // use zydeco_surface::bitter::desugar::Alloc;
                let _pi: su::Term<su::DefId> = su::Pi(*param, body).into();
                // Some(pi)
                todo!()
            }
            | Tm::Var(_)
            | Tm::Hole(_)
            | Tm::Triv(_)
            | Tm::Cons(_)
            | Tm::App(_)
            | Tm::Rec(_)
            | Tm::Pi(_)
            | Tm::Sigma(_)
            | Tm::Thunk(_)
            | Tm::Force(_)
            | Tm::Ret(_)
            | Tm::Do(_)
            | Tm::Let(_)
            | Tm::Data(_)
            | Tm::CoData(_)
            | Tm::Ctor(_)
            | Tm::Match(_)
            | Tm::CoMatch(_)
            | Tm::Dtor(_)
            | Tm::Lit(_) => None,
        }
    }
}
