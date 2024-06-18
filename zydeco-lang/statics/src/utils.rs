use crate::*;
use ss::*;

impl Tycker {
    pub fn extract_tpat(&mut self, tpat: TPatId) -> (Option<DefId>, KindId) {
        match self.statics.tpats[&tpat].clone() {
            | TypePattern::Hole(Hole) => todo!(),
            | TypePattern::Var(_def) => {
                todo!()
            }
        }
    }
}

pub trait SyntacticallyUsed {
    fn syntactically_used(&self, tycker: &mut Tycker) -> bool;
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
            | Pat::Var(def) => !tycker.scoped.users.forth(&def).is_empty(),
            | Pat::Ctor(pat) => {
                let su::Ctor(_ctor, pat) = pat;
                pat.syntactically_used(tycker)
            }
            | Pat::Triv(pat) => {
                let su::Triv = pat;
                false
            }
            | Pat::Cons(pat) => {
                let su::Cons(pat1, pat2) = pat;
                pat1.syntactically_used(tycker) || pat2.syntactically_used(tycker)
            }
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
            | Pat::Var(def) => !tycker.scoped.users.forth(&def).is_empty(),
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
            | Pat::Var(def) => !tycker.scoped.users.forth(&def).is_empty(),
            | Pat::Ctor(pat) => {
                let ss::Ctor(_ctor, pat) = pat;
                pat.syntactically_used(tycker)
            }
            | Pat::Triv(pat) => {
                let ss::Triv = pat;
                false
            }
            | Pat::VCons(pat) => {
                let ss::Cons(pat1, pat2) = pat;
                pat1.syntactically_used(tycker) || pat2.syntactically_used(tycker)
            }
            | Pat::TCons(pat) => {
                let ss::Cons(pat1, pat2) = pat;
                pat1.syntactically_used(tycker) || pat2.syntactically_used(tycker)
            }
        }
    }
}

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
        match term {
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
            | Tm::WithBlock(_)
            | Tm::Lit(_) => None,
        }
    }
}
