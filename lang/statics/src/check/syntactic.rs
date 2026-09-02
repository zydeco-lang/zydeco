use crate::*;

pub trait SyntacticallyAnnotated {
    fn syntactically_annotated(&self, tycker: &mut Tycker) -> Option<su::TermId>;
}
impl SyntacticallyAnnotated for su::Binding {
    fn syntactically_annotated(&self, tycker: &mut Tycker) -> Option<su::TermId> {
        match &self.inner {
            | su::BindingForm::Parameter(_) => None,
            | su::BindingForm::Definition(su::Definition { binder, bindee }) => {
                let _ = binder;
                bindee.syntactically_annotated(tycker)
            }
        }
    }
}
impl SyntacticallyAnnotated for su::TermId {
    fn syntactically_annotated(&self, tycker: &mut Tycker) -> Option<su::TermId> {
        let term = tycker.scoped.terms[self].clone();
        use su::Term as Tm;
        match term {
            | Tm::Meta(term) => {
                let su::MetaT(meta, term) = *term;
                let _ = meta;
                term.syntactically_annotated(tycker)
            }
            | Tm::SourceBoundary(su::SourceBoundary(term)) => term.syntactically_annotated(tycker),
            | Tm::SignatureBoundary(su::SignatureBoundary(term)) => {
                term.syntactically_annotated(tycker)
            }
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(term) => {
                let su::Sealed(term) = term;
                term.syntactically_annotated(tycker)
            }
            | Tm::Ann(term) => {
                let su::Ann { tm: _, ty } = term;
                Some(ty)
            }
            | Tm::TypeOf(_)
            | Tm::Abs(_)
            | Tm::ValAbs(_)
            | Tm::Var(_)
            | Tm::Named(_)
            | Tm::Label(_)
            | Tm::Hole(_)
            | Tm::Triv(_)
            | Tm::Cons(_)
            | Tm::App(_)
            | Tm::Fix(_)
            | Tm::Pi(_)
            | Tm::ValPi(_)
            | Tm::Sigma(_)
            | Tm::ManifestExists(_)
            | Tm::Pack(_)
            | Tm::Thunk(_)
            | Tm::Force(_)
            | Tm::Ret(_)
            | Tm::Do(_)
            | Tm::Let(_)
            | Tm::MobileParam(_)
            | Tm::MobileBind(_)
            | Tm::RecGroup(_)
            | Tm::MoBlock(_)
            | Tm::Data(_)
            | Tm::CoData(_)
            | Tm::Ctor(_)
            | Tm::Match(_)
            | Tm::CoMatchClauses(_)
            | Tm::CoMatch(_)
            | Tm::Dtor(_)
            | Tm::Proj(_)
            | Tm::Lit(_) => None,
            | Tm::Residual(su::Residual(body)) => body.syntactically_annotated(tycker),
            | Tm::Block(su::Block(body)) => body.syntactically_annotated(tycker),
        }
    }
}

pub trait SyntacticallySealed {
    fn syntactically_sealed(&self, tycker: &mut Tycker) -> Option<su::TermId>;
}

impl SyntacticallySealed for su::TermId {
    fn syntactically_sealed(&self, tycker: &mut Tycker) -> Option<surface_syntax::TermId> {
        let term = tycker.scoped.terms[self].clone();
        use surface_syntax::Term as Tm;
        match term {
            | Tm::Meta(term) => {
                let su::MetaT(meta, term) = *term;
                let _ = meta;
                term.syntactically_sealed(tycker)
            }
            | Tm::SourceBoundary(su::SourceBoundary(term)) => term.syntactically_sealed(tycker),
            | Tm::SignatureBoundary(su::SignatureBoundary(term)) => {
                term.syntactically_sealed(tycker)
            }
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(term) => {
                let su::Sealed(term) = term;
                Some(term)
            }
            | Tm::TypeOf(_)
            | Tm::Ann(_)
            | Tm::Hole(_)
            | Tm::Var(_)
            | Tm::Named(_)
            | Tm::Label(_)
            | Tm::Triv(_)
            | Tm::Cons(_)
            | Tm::Abs(_)
            | Tm::ValAbs(_)
            | Tm::App(_)
            | Tm::Fix(_)
            | Tm::Pi(_)
            | Tm::ValPi(_)
            | Tm::Sigma(_)
            | Tm::ManifestExists(_)
            | Tm::Pack(_)
            | Tm::Thunk(_)
            | Tm::Force(_)
            | Tm::Ret(_)
            | Tm::Do(_)
            | Tm::Let(_)
            | Tm::MobileParam(_)
            | Tm::MobileBind(_)
            | Tm::RecGroup(_)
            | Tm::MoBlock(_)
            | Tm::Data(_)
            | Tm::CoData(_)
            | Tm::Ctor(_)
            | Tm::Match(_)
            | Tm::CoMatchClauses(_)
            | Tm::CoMatch(_)
            | Tm::Dtor(_)
            | Tm::Proj(_)
            | Tm::Lit(_) => None,
            | Tm::Residual(su::Residual(body)) => body.syntactically_sealed(tycker),
            | Tm::Block(su::Block(body)) => body.syntactically_sealed(tycker),
        }
    }
}
