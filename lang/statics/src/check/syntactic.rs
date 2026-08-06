use crate::*;

pub trait SyntacticallyUsed {
    fn syntactically_used(&self, tycker: &mut Tycker) -> bool;
}
impl SyntacticallyUsed for su::DefId {
    fn syntactically_used(&self, tycker: &mut Tycker) -> bool {
        !tycker.scoped.users.forth(self).is_empty()
    }
}
impl SyntacticallyUsed for su::PatId {
    fn syntactically_used(&self, tycker: &mut Tycker) -> bool {
        let pat = tycker.scoped.pats[self].clone();
        use su::Pattern as Pat;
        match pat {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty: _ } = pat;
                tm.syntactically_used(tycker)
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
                false
            }
            | Pat::Triv(su::Triv) => false,
            | Pat::Var(def) => def.syntactically_used(tycker),
            | Pat::Named(pat) => {
                let su::Named(_name, inner) = pat;
                inner.syntactically_used(tycker)
            }
            | Pat::Ctor(pat) => {
                let su::Ctor(_ctor, pat) = pat;
                pat.syntactically_used(tycker)
            }
            | Pat::Alias(su::Alias(patterns)) => {
                patterns.into_iter().any(|pattern| pattern.syntactically_used(tycker))
            }
            | Pat::Cons(pat) => pat.into_iter().any(|item| item.syntactically_used(tycker)),
        }
    }
}
impl SyntacticallyUsed for ss::TPatId {
    fn syntactically_used(&self, tycker: &mut Tycker) -> bool {
        let pat = tycker.statics.tpats[self].clone();
        use ss::TypePattern as Pat;
        match pat {
            | Pat::Hole(pat) => {
                let ss::Hole = pat;
                false
            }
            | Pat::Var(def) => def.syntactically_used(tycker),
            | Pat::Named(ss::Named(_, inner)) => inner.syntactically_used(tycker),
        }
    }
}
impl SyntacticallyUsed for ss::VPatId {
    fn syntactically_used(&self, tycker: &mut Tycker) -> bool {
        let pat = tycker.statics.vpats[self].clone();
        use ss::ValuePattern as Pat;
        match pat {
            | Pat::Hole(pat) => {
                let ss::Hole = pat;
                false
            }
            | Pat::Triv(ss::Triv) => false,
            | Pat::Var(def) => def.syntactically_used(tycker),
            | Pat::Named(pat) => {
                let ss::Named(_name, inner) = pat;
                inner.syntactically_used(tycker)
            }
            | Pat::Ctor(pat) => {
                let ss::Ctor(_ctor, pat) = pat;
                pat.syntactically_used(tycker)
            }
            | Pat::Alias(ss::Alias(patterns)) => {
                patterns.into_iter().any(|pattern| pattern.syntactically_used(tycker))
            }
            | Pat::VCons(pat) => pat.into_iter().any(|item| item.syntactically_used(tycker)),
            | Pat::SCons(pat) => {
                let ss::ConsN(_witnesses, body) = pat;
                // Hack: assuming that we don't care whether the abstracted type is used
                //       well, technically, it's not syntactic but semantic,
                //       so indeed not syntactically used
                body.syntactically_used(tycker)
            }
        }
    }
}

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
                let su::MetaT(meta, term) = term;
                let _ = meta;
                term.syntactically_annotated(tycker)
            }
            | Tm::SourceBoundary(su::SourceBoundary(term)) => term.syntactically_annotated(tycker),
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(term) => {
                let su::Sealed(term) = term;
                term.syntactically_annotated(tycker)
            }
            | Tm::Ann(term) => {
                let su::Ann { tm: _, ty } = term;
                Some(ty)
            }
            | Tm::Abs(_)
            | Tm::Var(_)
            | Tm::Named(_)
            | Tm::Label(_)
            | Tm::Hole(_)
            | Tm::Triv(_)
            | Tm::Cons(_)
            | Tm::App(_)
            | Tm::Fix(_)
            | Tm::Pi(_)
            | Tm::Sigma(_)
            | Tm::ManifestExists(_)
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
                let su::MetaT(meta, term) = term;
                let _ = meta;
                term.syntactically_sealed(tycker)
            }
            | Tm::SourceBoundary(su::SourceBoundary(term)) => term.syntactically_sealed(tycker),
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(term) => {
                let su::Sealed(term) = term;
                Some(term)
            }
            | Tm::Ann(_)
            | Tm::Hole(_)
            | Tm::Var(_)
            | Tm::Named(_)
            | Tm::Label(_)
            | Tm::Triv(_)
            | Tm::Cons(_)
            | Tm::Abs(_)
            | Tm::App(_)
            | Tm::Fix(_)
            | Tm::Pi(_)
            | Tm::Sigma(_)
            | Tm::ManifestExists(_)
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
