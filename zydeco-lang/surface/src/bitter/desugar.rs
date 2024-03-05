use crate::{
    bitter::syntax as b,
    textual::syntax::{self as t},
};
use zydeco_utils::span::Span;

pub trait Desugar {
    type Out;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out;
}

pub struct Desugarer {
    pub tspans: t::SpanArenaTextual,
    pub tctx: t::Ctx,
    pub bspans: b::SpanArenaBitter,
    pub bctx: b::Ctx,
}

pub struct DesugarOut {
    pub spans: b::SpanArenaBitter,
    pub ctx: b::Ctx,
    pub top: b::TopLevel,
}

impl Desugarer {
    pub fn run(self, top: t::TopLevel) -> DesugarOut {
        let mut desugarer = self;
        let top = top.desugar(&mut desugarer);
        let Desugarer { bctx: ctx, bspans: spans, .. } = desugarer;
        DesugarOut { spans, ctx, top }
    }
}

trait Spanned {
    fn span(&self, desugarer: &mut Desugarer) -> Span;
}

impl<T> Desugar for Vec<T>
where
    T: Desugar,
{
    type Out = Vec<T::Out>;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        self.into_iter().map(|x| x.desugar(desugarer)).collect()
    }
}

impl Desugar for t::TopLevel {
    type Out = b::TopLevel;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::TopLevel(decls) = self;
        let mut decls_new = Vec::new();
        for t::Modifiers { public, inner } in decls {
            use t::Declaration as Decl;
            let inner = match inner {
                Decl::DataDef(decl) => {
                    let t::DataDef { name, params, def } = decl;
                    let span = name.span(desugarer);
                    // name -> pat
                    let name = name.desugar(desugarer);
                    let pat = desugarer.span_pat(span.clone());
                    let pat = desugarer.pat(pat, name.into());
                    // params -> copattern
                    let params = params.desugar(desugarer);
                    let params = params
                        .into_iter()
                        .map(|param| {
                            // param -> copattern
                            let span = param.span(desugarer);
                            let co = desugarer.span_copat(span);
                            let param = desugarer.copat(co, param.into());
                            param
                        })
                        .collect::<Vec<_>>();
                    let co = desugarer.span_copat(span.clone());
                    let params = desugarer.copat(co, b::App(params).into());
                    // def -> term
                    let def = span.make(def).desugar(desugarer);
                    // params & def -> abs term
                    let abs = desugarer.span_term(span.clone());
                    let abs = desugarer.term(abs, b::Abs(params, def).into());
                    // abs -> sealed
                    let sealed = desugarer.span_term(span.clone());
                    let sealed = desugarer.term(sealed, b::Sealed(abs).into());
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: sealed }.into()
                }
                Decl::CoDataDef(decl) => {
                    let t::CoDataDef { name, params, def } = decl;
                    let span = name.span(desugarer);
                    // name -> pat
                    let name = name.desugar(desugarer);
                    let pat = desugarer.span_pat(span.clone());
                    let pat = desugarer.pat(pat, name.into());
                    // params -> copattern
                    let params = params.desugar(desugarer);
                    let params = params
                        .into_iter()
                        .map(|param| {
                            // param -> copattern
                            let span = param.span(desugarer);
                            let co = desugarer.span_copat(span);
                            let param = desugarer.copat(co, param.into());
                            param
                        })
                        .collect::<Vec<_>>();
                    let co = desugarer.span_copat(span.clone());
                    let params = desugarer.copat(co, b::App(params).into());
                    // def -> term
                    let def = span.make(def).desugar(desugarer);
                    // params & def -> abs term
                    let abs = desugarer.span_term(span.clone());
                    let abs = desugarer.term(abs, b::Abs(params, def).into());
                    // abs -> sealed
                    let sealed = desugarer.span_term(span.clone());
                    let sealed = desugarer.term(sealed, b::Sealed(abs).into());
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: sealed }.into()
                }
                Decl::Define(decl) => {
                    let t::Define(genbind) = decl;
                    let (pat, term) = genbind.desugar(desugarer);
                    // sealed
                    let span = pat.span(desugarer);
                    let sealed = desugarer.span_term(span);
                    let sealed = desugarer.term(sealed, b::Sealed(term).into());
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: sealed }.into()
                }
                Decl::Alias(decl) => {
                    let t::Alias(genbind) = decl;
                    let (pat, term) = genbind.desugar(desugarer);
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: term }.into()
                }
                Decl::Extern(decl) => {
                    let t::Extern(t::GenBind { rec: _, comp: _, binder, params, ty, bindee: () }) =
                        decl;
                    let binder = binder.desugar(desugarer);
                    let params = params.map(|params| params.desugar(desugarer));
                    let ty = ty.map(|ty| ty.desugar(desugarer));
                    b::Extern { binder, params, ty }.into()
                }
                Decl::Module(decl) => {
                    let t::Module { name, top } = decl;
                    let top = top.desugar(desugarer);
                    b::Module { name, top }.into()
                }
                Decl::UseDef(decl) => {
                    let t::UseDef(uses) = decl;
                    // Todo: uses
                    b::UseDef(uses).into()
                }
                Decl::UseBlock(decl) => {
                    let t::UseBlock { uses, top } = decl;
                    // Todo: uses
                    let top = top.desugar(desugarer);
                    b::UseBlock { uses, top }.into()
                }
                Decl::Main(decl) => {
                    let t::Main(term) = decl;
                    let term = term.desugar(desugarer);
                    b::Main(term).into()
                }
            };
            decls_new.push(b::Modifiers { public, inner })
        }
        b::TopLevel(decls_new)
    }
}

impl Desugar for t::DefId {
    type Out = b::DefId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        // look old span
        let span = id.span(desugarer);
        // lookup def
        let def = desugarer.lookup_def(id);
        // write new span
        let id = desugarer.span_def(span);
        // write new def
        desugarer.def(id, def)
    }
}

impl Desugar for t::PatId {
    type Out = b::PatId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let span = id.span(desugarer);
        let pat = desugarer.lookup_pat(id);
        let id = desugarer.span_pat(span);
        use t::Pattern as Pat;
        match pat {
            Pat::Ann(pat) => {
                let t::Ann { tm, ty } = pat;
                let tm = tm.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.pat(id, b::Ann { tm, ty }.into())
            }
            Pat::Hole(pat) => {
                let t::Hole = pat;
                desugarer.pat(id, b::Hole.into())
            }
            Pat::Var(name) => {
                let name = name.desugar(desugarer).into();
                desugarer.pat(id, name)
            }
            Pat::Ctor(pat) => {
                let t::Ctor(name, pat) = pat;
                let pat = pat.desugar(desugarer);
                desugarer.pat(id, b::Ctor(name, pat).into())
            }
            Pat::Paren(pat) => {
                let t::Paren(pats) = pat;
                let pats = pats.desugar(desugarer);
                // if there is only one pat like `(p)`, remove the redundant paren
                if pats.len() == 1 {
                    pats.into_iter().next().unwrap()
                } else {
                    desugarer.pat(id, b::Paren(pats).into())
                }
            }
        }
    }
}

impl Desugar for t::CoPatId {
    type Out = b::CoPatId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let span = id.span(desugarer);
        let copat = desugarer.lookup_copat(id);
        let id = desugarer.span_copat(span);
        use t::CoPattern as CoPat;
        match copat {
            CoPat::Pat(copat) => {
                let copat = copat.desugar(desugarer);
                desugarer.copat(id, copat.into())
            }
            CoPat::Dtor(name) => desugarer.copat(id, name.into()),
            CoPat::App(copat) => {
                let t::App(copats) = copat;
                let copats = copats.desugar(desugarer);
                desugarer.copat(id, b::App(copats).into())
            }
        }
    }
}

impl Desugar for t::TermId {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let id = self;
        let span = id.span(desugarer);
        let term = desugarer.lookup_term(id);
        let id = desugarer.span_term(span);
        use t::Term as Tm;
        match term {
            Tm::Ann(term) => {
                let t::Ann { tm, ty } = term;
                let tm = tm.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Ann { tm, ty }.into())
            }
            Tm::Hole(term) => {
                let t::Hole = term;
                desugarer.term(id, b::Hole.into())
            }
            Tm::Var(name) => desugarer.term(id, b::Term::Var(name).into()),
            Tm::Paren(term) => {
                let t::Paren(terms) = term;
                let terms = terms.desugar(desugarer);
                // if there is only one term like `(t)`, remove the redundant paren
                if terms.len() == 1 {
                    terms.into_iter().next().unwrap()
                } else {
                    desugarer.term(id, b::Paren(terms).into())
                }
            }
            Tm::Abs(term) => {
                let t::Abs(params, tail) = term;
                let params = params.desugar(desugarer);
                let tail = tail.desugar(desugarer);
                desugarer.term(id, b::Abs(params, tail).into())
            }
            Tm::App(term) => {
                let t::App(terms) = term;
                let terms = terms.desugar(desugarer);
                desugarer.term(id, b::App(terms).into())
            }
            Tm::Rec(term) => {
                let t::Rec(pat, term) = term;
                let pat = pat.desugar(desugarer);
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Rec(pat, term).into())
            }
            Tm::Pi(term) => {
                let t::Pi(params, ty) = term;
                let params = params.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Pi(params, ty).into())
            }
            Tm::Arrow(term) => {
                let t::Arrow(params, ty) = term;
                let params = params.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Arrow(params, ty).into())
            }
            Tm::Forall(term) => {
                let t::Forall(params, ty) = term;
                let params = params.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Forall(params, ty).into())
            }
            Tm::Sigma(term) => {
                let t::Sigma(params, ty) = term;
                let params = params.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Sigma(params, ty).into())
            }
            Tm::Prod(term) => {
                let t::Prod(terms) = term;
                let terms = terms.desugar(desugarer);
                desugarer.term(id, b::Prod(terms).into())
            }
            Tm::Exists(term) => {
                let t::Exists(params, ty) = term;
                let params = params.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Exists(params, ty).into())
            }
            Tm::Thunk(term) => {
                let t::Thunk(term) = term;
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Thunk(term).into())
            }
            Tm::Force(term) => {
                let t::Force(term) = term;
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Force(term).into())
            }
            Tm::Ret(term) => {
                let t::Return(term) = term;
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Return(term).into())
            }
            Tm::Do(term) => {
                let t::Bind { binder, bindee, tail } = term;
                let binder = binder.desugar(desugarer);
                let bindee = bindee.desugar(desugarer);
                let tail = tail.desugar(desugarer);
                desugarer.term(id, b::Bind { binder, bindee, tail }.into())
            }
            Tm::Let(term) => {
                let t::PureBind { binding, tail } = term;
                let (binder, bindee) = binding.desugar(desugarer);
                let tail = tail.desugar(desugarer);
                desugarer.term(id, b::PureBind { binder, bindee, tail }.into())
            }
            Tm::UseLet(term) => {
                let t::UseBind { uses, tail } = term;
                // Todo: uses
                let tail = tail.desugar(desugarer);
                desugarer.term(id, b::UseBind { uses, tail }.into())
            }
            Tm::Data(data) => {
                let span = id.span(desugarer);
                span.make(data).desugar(desugarer)
            }
            Tm::CoData(codata) => {
                let span = id.span(desugarer);
                span.make(codata).desugar(desugarer)
            }
            Tm::Ctor(term) => {
                let t::Ctor(name, term) = term;
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Ctor(name, term).into())
            }
            Tm::Match(term) => {
                let t::Match { scrut, arms } = term;
                let scrut = scrut.desugar(desugarer);
                let arms = arms
                    .into_iter()
                    .map(|t::Matcher { binder, tail }| {
                        let binder = binder.desugar(desugarer);
                        let tail = tail.desugar(desugarer);
                        b::Matcher { binder, tail }
                    })
                    .collect();
                desugarer.term(id, b::Match { scrut, arms }.into())
            }
            Tm::CoMatch(term) => {
                let t::CoMatch { arms } = term;
                let arms = arms
                    .into_iter()
                    .map(|t::CoMatcher { params, tail }| {
                        let params = params.desugar(desugarer);
                        let tail = tail.desugar(desugarer);
                        b::CoMatcher { params, tail }
                    })
                    .collect();
                desugarer.term(id, b::CoMatch { arms }.into())
            }
            Tm::Dtor(term) => {
                let t::Dtor(term, name) = term;
                let term = term.desugar(desugarer);
                desugarer.term(id, b::Dtor(term, name).into())
            }
            Tm::Lit(term) => desugarer.term(id, term.into()),
        }
    }
}

impl Desugar for t::GenBind<t::TermId> {
    type Out = (b::PatId, b::TermId);
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::GenBind { rec, comp, binder, params, ty, bindee } = self;
        // binder
        let binder = binder.desugar(desugarer);
        // bindee & ty -> ann
        let bindee = bindee.desugar(desugarer);
        let ty = ty.map(|ty| ty.desugar(desugarer));
        let bindee = if let Some(ty) = ty {
            let span = bindee.span(desugarer);
            let ann = desugarer.span_term(span);
            desugarer.term(ann, b::Ann { tm: bindee, ty }.into())
        } else {
            bindee
        };
        // params
        let params = params.map(|params| params.desugar(desugarer));
        // params? & bindee -> abs term binding
        let mut binding = if let Some(params) = params {
            let span = binder.span(desugarer);
            let binding = desugarer.span_term(span);
            desugarer.term(binding, b::Abs(params, bindee).into())
        } else {
            bindee
        };
        // rec?
        if rec {
            let span = binding.span(desugarer);
            let rec = desugarer.span_term(span);
            binding = desugarer.term(rec, b::Rec(binder, binding).into());
        }
        // add thunk?
        if rec || comp {
            let span = binding.span(desugarer);
            let thunk = desugarer.span_term(span);
            binding = desugarer.term(thunk, b::Thunk(binding).into());
        }
        (binder, binding)
    }
}

impl Desugar for t::Sp<t::Data> {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::Data { arms } = self.inner;
        let arms = arms
            .into_iter()
            .map(|t::DataArm { name, param }| {
                let param = param.desugar(desugarer);
                b::DataArm { name, param }
            })
            .collect();
        let term = desugarer.span_term(self.info);
        desugarer.term(term, b::Data { arms }.into())
    }
}

impl Desugar for t::Sp<t::CoData> {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out {
        let t::CoData { arms } = self.inner;
        let arms = arms
            .into_iter()
            .map(|t::CoDataArm { name, params, out }| {
                let params = params.map(|params| params.desugar(desugarer));
                let out = out.desugar(desugarer);
                b::CoDataArm { name, params, out }
            })
            .collect();
        let term = desugarer.span_term(self.info);
        desugarer.term(term, b::CoData { arms }.into())
    }
}

mod impls {
    use super::*;

    impl Spanned for t::DefId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.defs[*self].clone()
        }
    }
    impl Spanned for t::PatId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.pats[*self].clone()
        }
    }
    impl Spanned for t::CoPatId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.copats[*self].clone()
        }
    }
    impl Spanned for t::TermId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.terms[*self].clone()
        }
    }
    impl Spanned for b::DefId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.bspans.defs[*self].clone()
        }
    }
    impl Spanned for b::PatId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.bspans.pats[*self].clone()
        }
    }
    impl Spanned for b::CoPatId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.bspans.copats[*self].clone()
        }
    }
    impl Spanned for b::TermId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.bspans.terms[*self].clone()
        }
    }

    impl Desugarer {
        pub fn lookup_def(&self, id: t::DefId) -> t::VarName {
            self.tctx.defs[id].clone()
        }
        pub fn lookup_pat(&self, id: t::PatId) -> t::Pattern {
            self.tctx.pats[id].clone()
        }
        pub fn lookup_copat(&self, id: t::CoPatId) -> t::CoPattern {
            self.tctx.copats[id].clone()
        }
        pub fn lookup_term(&self, id: t::TermId) -> t::Term {
            self.tctx.terms[id].clone()
        }

        pub fn span_def(&mut self, span: Span) -> b::DefId {
            self.bspans.defs.alloc(span)
        }
        pub fn span_pat(&mut self, span: Span) -> b::PatId {
            self.bspans.pats.alloc(span)
        }
        pub fn span_copat(&mut self, span: Span) -> b::CoPatId {
            self.bspans.copats.alloc(span)
        }
        pub fn span_term(&mut self, span: Span) -> b::TermId {
            self.bspans.terms.alloc(span)
        }

        pub fn def(&mut self, id: b::DefId, def: b::VarName) -> b::DefId {
            self.bctx.defs.insert(id, def);
            id
        }
        pub fn pat(&mut self, id: b::PatId, pat: b::Pattern) -> b::PatId {
            self.bctx.pats.insert(id, pat);
            id
        }
        pub fn copat(&mut self, id: b::CoPatId, copat: b::CoPattern) -> b::CoPatId {
            self.bctx.copats.insert(id, copat);
            id
        }
        pub fn term(&mut self, id: b::TermId, term: b::Term<b::NameRef<b::VarName>>) -> b::TermId {
            self.bctx.terms.insert(id, term);
            id
        }
    }
}
