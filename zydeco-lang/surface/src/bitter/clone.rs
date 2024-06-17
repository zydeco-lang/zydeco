use crate::bitter::{syntax as b, *};

pub trait DeepClone {
    fn deep_clone(&self, desugarer: &mut Desugarer) -> Self;
}
impl<T> DeepClone for Vec<T>
where
    T: DeepClone,
{
    fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
        self.iter().map(|x| x.deep_clone(desugarer)).collect()
    }
}
impl DeepClone for b::DefId {
    fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
        let def = desugarer.bitter.defs[self].clone();
        let prev = *desugarer.bitter.textual.back(&(*self).into()).unwrap();
        Alloc::alloc(desugarer, def, prev)
    }
}
impl DeepClone for b::PatId {
    fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
        let pat = desugarer.bitter.pats[self].clone();
        let prev = *desugarer.bitter.textual.back(&(*self).into()).unwrap();
        let pat = match &pat {
            | b::Pattern::Ann(pat) => {
                let b::Ann { tm, ty } = pat;
                let tm = tm.deep_clone(desugarer);
                let ty = ty.deep_clone(desugarer);
                b::Ann { tm, ty }.into()
            }
            | b::Pattern::Hole(_pat) => b::Hole.into(),
            | b::Pattern::Var(pat) => pat.deep_clone(desugarer).into(),
            | b::Pattern::Ctor(pat) => {
                let b::Ctor(name, pat) = pat;
                let pat = pat.deep_clone(desugarer);
                b::Ctor(name.clone(), pat).into()
            }
            | b::Pattern::Triv(_pat) => b::Triv.into(),
            | b::Pattern::Cons(pat) => {
                let b::Cons(a, b) = pat;
                let a = a.deep_clone(desugarer);
                let b = b.deep_clone(desugarer);
                b::Cons(a, b).into()
            }
        };
        Alloc::alloc(desugarer, pat, prev)
    }
}
impl DeepClone for b::TermId {
    fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
        let term = desugarer.bitter.terms[self].clone();
        let prev = *desugarer.bitter.textual.back(&(*self).into()).unwrap();
        let term = match &term {
            | b::Term::Internal(term) => {
                use crate::syntax::Internal;
                match term {
                    | Internal::VType => {
                        return desugarer.vtype(prev);
                    }
                    | Internal::CType => {
                        return desugarer.ctype(prev);
                    }
                    | Internal::Thunk => {
                        return desugarer.thunk(prev);
                    }
                    | Internal::Ret => {
                        return desugarer.ret(prev);
                    }
                    | Internal::Unit => {
                        return desugarer.unit(prev);
                    }
                    | Internal::Int => {
                        return desugarer.int(prev);
                    }
                    | Internal::Char => {
                        return desugarer.char(prev);
                    }
                    | Internal::String => {
                        return desugarer.string(prev);
                    }
                    | Internal::OS => {
                        return desugarer.os(prev);
                    }
                    | Internal::Monad => {
                        return desugarer.monad(prev);
                    }
                    | Internal::Algebra => {
                        return desugarer.algebra(prev);
                    }
                }
            }
            | b::Term::Sealed(_term) => {
                unreachable!()
                // let b::Sealed(term) = term;
                // let term = term.deep_clone(desugarer);
                // b::Sealed(term).into()
            }
            | b::Term::Ann(term) => {
                let b::Ann { tm, ty } = term;
                let tm = tm.deep_clone(desugarer);
                let ty = ty.deep_clone(desugarer);
                b::Ann { tm, ty }.into()
            }
            | b::Term::Hole(_term) => b::Hole.into(),
            | b::Term::Var(name) => b::Term::Var(name.clone()),
            | b::Term::Triv(_term) => b::Triv.into(),
            | b::Term::Cons(term) => {
                let b::Cons(a, b) = term;
                let a = a.deep_clone(desugarer);
                let b = b.deep_clone(desugarer);
                b::Cons(a, b).into()
            }
            | b::Term::Abs(term) => {
                let b::Abs(params, tail) = term;
                let params = params.deep_clone(desugarer);
                let tail = tail.deep_clone(desugarer);
                b::Abs(params, tail).into()
            }
            | b::Term::App(term) => {
                let b::App(a, b) = term;
                let a = a.deep_clone(desugarer);
                let b = b.deep_clone(desugarer);
                b::App(a, b).into()
            }
            | b::Term::Rec(term) => {
                let b::Rec(pat, term) = term;
                let pat = pat.deep_clone(desugarer);
                let term = term.deep_clone(desugarer);
                b::Rec(pat, term).into()
            }
            | b::Term::Pi(term) => {
                let b::Pi(params, ty) = term;
                let params = params.deep_clone(desugarer);
                let ty = ty.deep_clone(desugarer);
                b::Pi(params, ty).into()
            }
            | b::Term::Sigma(term) => {
                let b::Sigma(params, ty) = term;
                let params = params.deep_clone(desugarer);
                let ty = ty.deep_clone(desugarer);
                b::Sigma(params, ty).into()
            }
            | b::Term::Thunk(term) => {
                let b::Thunk(term) = term;
                let term = term.deep_clone(desugarer);
                b::Thunk(term).into()
            }
            | b::Term::Force(term) => {
                let b::Force(term) = term;
                let term = term.deep_clone(desugarer);
                b::Force(term).into()
            }
            | b::Term::Ret(term) => {
                let b::Ret(term) = term;
                let term = term.deep_clone(desugarer);
                b::Ret(term).into()
            }
            | b::Term::Do(term) => {
                let b::Bind { binder, bindee, tail } = term;
                let binder = binder.deep_clone(desugarer);
                let bindee = bindee.deep_clone(desugarer);
                let tail = tail.deep_clone(desugarer);
                b::Bind { binder, bindee, tail }.into()
            }
            | b::Term::Let(term) => {
                let b::PureBind { binder, bindee, tail } = term;
                let binder = binder.deep_clone(desugarer);
                let bindee = bindee.deep_clone(desugarer);
                let tail = tail.deep_clone(desugarer);
                b::PureBind { binder, bindee, tail }.into()
            }
            | b::Term::Data(term) => {
                let b::Data { arms } = term;
                let arms = arms
                    .into_iter()
                    .map(|b::DataArm { name, param }| {
                        let name = name.clone();
                        let param = param.deep_clone(desugarer);
                        b::DataArm { name, param }
                    })
                    .collect();
                b::Data { arms }.into()
            }
            | b::Term::CoData(term) => {
                let b::CoData { arms } = term;
                let arms = arms
                    .into_iter()
                    .map(|b::CoDataArm { name, out }| {
                        let name = name.clone();
                        let out = out.deep_clone(desugarer);
                        b::CoDataArm { name, out }
                    })
                    .collect();
                b::CoData { arms }.into()
            }
            | b::Term::Ctor(term) => {
                let b::Ctor(name, term) = term;
                let term = term.deep_clone(desugarer);
                let name = name.clone();
                b::Ctor(name, term).into()
            }
            | b::Term::Match(term) => {
                let b::Match { scrut, arms } = term;
                let scrut = scrut.deep_clone(desugarer);
                let arms = arms
                    .into_iter()
                    .map(|b::Matcher { binder, tail }| {
                        let binder = binder.deep_clone(desugarer);
                        let tail = tail.deep_clone(desugarer);
                        b::Matcher { binder, tail }
                    })
                    .collect();
                b::Match { scrut, arms }.into()
            }
            | b::Term::CoMatch(term) => {
                let b::CoMatch { arms } = term;
                let arms = arms
                    .into_iter()
                    .map(|b::CoMatcher { dtor, tail }| {
                        let dtor = dtor.clone();
                        let tail = tail.deep_clone(desugarer);
                        b::CoMatcher { dtor, tail }
                    })
                    .collect();
                b::CoMatch { arms }.into()
            }
            | b::Term::Dtor(term) => {
                let b::Dtor(term, name) = term;
                let term = term.deep_clone(desugarer);
                let name = name.clone();
                b::Dtor(term, name).into()
            }
            | b::Term::WithBlock(term) => {
                let b::WithBlock { structs: monad_ty, imports, body } = term;
                let monad_ty = monad_ty.deep_clone(desugarer);
                let imports = imports
                    .into_iter()
                    .map(|b::Import { binder: name, body: def }| {
                        let name = name.clone();
                        let def = def.deep_clone(desugarer);
                        b::Import { binder: name, body: def }
                    })
                    .collect();
                let body = body.deep_clone(desugarer);
                b::WithBlock { structs: monad_ty, imports, body }.into()
            }
            | b::Term::Lit(term) => term.clone().into(),
        };
        Alloc::alloc(desugarer, term, prev)
    }
}
