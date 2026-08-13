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
impl DeepClone for b::CoPatternItem {
    fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
        match self {
            | Self::Pat(pattern) => Self::Pat(pattern.deep_clone(desugarer)),
            | Self::Dtor(dtor) => Self::Dtor(dtor.clone()),
        }
    }
}
impl DeepClone for b::CoPatternSpine {
    fn deep_clone(&self, desugarer: &mut Desugarer) -> Self {
        Self { head: self.head.deep_clone(desugarer), tail: self.tail.deep_clone(desugarer) }
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
            | b::Pattern::Named(pat) => {
                let b::Named(name, inner) = pat;
                b::Named(name.clone(), inner.deep_clone(desugarer)).into()
            }
            | b::Pattern::Triv(_pat) => b::Triv.into(),
            | b::Pattern::Ctor(pat) => {
                let b::Ctor(name, pat) = pat;
                let pat = pat.deep_clone(desugarer);
                b::Ctor(name.clone(), pat).into()
            }
            | b::Pattern::Project(b::ProjectionPattern(field, pattern)) => {
                b::ProjectionPattern(field.clone(), pattern.deep_clone(desugarer)).into()
            }
            | b::Pattern::Alias(b::Alias(patterns)) => {
                let patterns = patterns.iter().map(|pat| pat.deep_clone(desugarer)).collect();
                b::Alias(b::ConsN::from_vec(patterns).unwrap()).into()
            }
            | b::Pattern::Cons(pat) => {
                let b::ConsN(pats, tail) = pat;
                b::ConsN(pats.deep_clone(desugarer), tail.deep_clone(desugarer)).into()
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
            | b::Term::Meta(term) => {
                let b::MetaT(meta, term) = term;
                let term = term.deep_clone(desugarer);
                b::MetaT(meta.clone(), term).into()
            }
            | b::Term::SourceBoundary(term) => {
                let b::SourceBoundary(term) = term;
                b::SourceBoundary(term.deep_clone(desugarer)).into()
            }
            | b::Term::SignatureBoundary(term) => {
                let b::SignatureBoundary(term) = term;
                b::SignatureBoundary(term.deep_clone(desugarer)).into()
            }
            | b::Term::Internal(term) => {
                use crate::syntax::Internal;
                match term {
                    | Internal::VType => {
                        return desugarer.vtype(prev);
                    }
                    | Internal::CType => {
                        return desugarer.ctype(prev);
                    }
                    | Internal::Thk => {
                        return desugarer.thunk(prev);
                    }
                    | Internal::Ret => {
                        return desugarer.ret(prev);
                    }
                    | Internal::Unit => {
                        return desugarer.unit(prev);
                    }
                    | Internal::Primitive(primitive) => {
                        return desugarer.primitive(*primitive, prev);
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
            }
            | b::Term::Ann(term) => {
                let b::Ann { tm, ty } = term;
                let tm = tm.deep_clone(desugarer);
                let ty = ty.deep_clone(desugarer);
                b::Ann { tm, ty }.into()
            }
            | b::Term::Hole(_term) => b::Hole.into(),
            | b::Term::Var(name) => b::Term::Var(name.clone()),
            | b::Term::Named(term) => {
                let b::Named(name, inner) = term;
                b::Named(name.clone(), inner.deep_clone(desugarer)).into()
            }
            | b::Term::Label(term) => {
                let b::Label(name, inner) = term;
                b::Label(name.clone(), inner.deep_clone(desugarer)).into()
            }
            | b::Term::Triv(_term) => b::Triv.into(),
            | b::Term::Cons(term) => {
                let b::ConsN(terms, tail) = term;
                b::ConsN(terms.deep_clone(desugarer), tail.deep_clone(desugarer)).into()
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
            | b::Term::Fix(term) => {
                let b::Fix(pat, term) = term;
                let pat = pat.deep_clone(desugarer);
                let term = term.deep_clone(desugarer);
                b::Fix(pat, term).into()
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
            | b::Term::ManifestExists(term) => {
                let b::ManifestExists { binder, definition, body } = term;
                b::ManifestExists {
                    binder: binder.deep_clone(desugarer),
                    definition: definition.deep_clone(desugarer),
                    body: body.deep_clone(desugarer),
                }
                .into()
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
                let b::Return(term) = term;
                let term = term.deep_clone(desugarer);
                b::Return(term).into()
            }
            | b::Term::Do(term) => {
                let b::Bind { binder, bindee, tail } = term;
                let binder = binder.deep_clone(desugarer);
                let bindee = bindee.deep_clone(desugarer);
                let tail = tail.deep_clone(desugarer);
                b::Bind { binder, bindee, tail }.into()
            }
            | b::Term::Let(term) => {
                let b::Let { binder, bindee, tail } = term;
                let binder = binder.deep_clone(desugarer);
                let bindee = bindee.deep_clone(desugarer);
                let tail = tail.deep_clone(desugarer);
                b::Let { binder, bindee, tail }.into()
            }
            | b::Term::MobileParam(term) => {
                let b::MobileParam { binder, tail } = term;
                b::MobileParam {
                    binder: binder.deep_clone(desugarer),
                    tail: tail.deep_clone(desugarer),
                }
                .into()
            }
            | b::Term::MobileBind(term) => {
                let b::MobileBind { binder, bindee, tail } = term;
                b::MobileBind {
                    binder: binder.deep_clone(desugarer),
                    bindee: bindee.deep_clone(desugarer),
                    tail: tail.deep_clone(desugarer),
                }
                .into()
            }
            | b::Term::Residual(term) => {
                let b::Residual(body) = term;
                b::Residual(body.deep_clone(desugarer)).into()
            }
            | b::Term::Block(term) => {
                let b::Block(body) = term;
                b::Block(body.deep_clone(desugarer)).into()
            }
            | b::Term::RecGroup(term) => {
                let b::RecGroup { definitions, tail } = term;
                b::RecGroup {
                    definitions: definitions
                        .iter()
                        .map(|b::RecursiveDefinition { binder, bindee }| b::RecursiveDefinition {
                            binder: binder.deep_clone(desugarer),
                            bindee: bindee.deep_clone(desugarer),
                        })
                        .collect(),
                    tail: tail.deep_clone(desugarer),
                }
                .into()
            }
            | b::Term::MoBlock(term) => {
                let b::MoBlock { body, basis } = term;
                let body = body.deep_clone(desugarer);
                let basis = b::MonadicBasis {
                    monad: basis.monad.deep_clone(desugarer),
                    algebra: basis.algebra.deep_clone(desugarer),
                };
                b::MoBlock { body, basis }.into()
            }
            | b::Term::Data(term) => {
                let b::Data { arms } = term;
                let arms = arms
                    .iter()
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
                    .iter()
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
                    .iter()
                    .map(|b::Matcher { binder, tail }| {
                        let binder = binder.deep_clone(desugarer);
                        let tail = tail.deep_clone(desugarer);
                        b::Matcher { binder, tail }
                    })
                    .collect();
                b::Match { scrut, arms }.into()
            }
            | b::Term::CoMatchClauses(term) => {
                let b::CoMatchClauses { clauses } = term;
                let clauses = clauses
                    .iter()
                    .map(|b::CoPatternClause { spine, tail }| b::CoPatternClause {
                        spine: spine.deep_clone(desugarer),
                        tail: tail.deep_clone(desugarer),
                    })
                    .collect();
                b::CoMatchClauses { clauses }.into()
            }
            | b::Term::CoMatch(term) => {
                let b::CoMatch { arms } = term;
                let arms = arms
                    .iter()
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
            | b::Term::Proj(term) => {
                let b::Proj(head, name) = term;
                b::Proj(head.deep_clone(desugarer), name.clone()).into()
            }
            | b::Term::Lit(term) => term.clone().into(),
        };
        Alloc::alloc(desugarer, term, prev)
    }
}
