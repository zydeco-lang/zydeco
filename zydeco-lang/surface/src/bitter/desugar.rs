use crate::{
    bitter::syntax::{self as b},
    syntax::*,
    textual::syntax::{self as t},
};
use zydeco_utils::span::Span;

pub trait Desugar {
    type Out;
    fn desugar(self, desugarer: &mut Desugarer) -> Self::Out;
}

pub struct Desugarer {
    pub tspans: t::SpanArenaTextual,
    pub t: t::Arena,
    pub bspans: b::SpanArenaBitter,
    pub b: b::Arena,
}

pub struct DesugarOut {
    pub spans: b::SpanArenaBitter,
    pub arena: b::Arena,
    pub top: b::TopLevel,
}

impl Desugarer {
    pub fn run(self, top: t::TopLevel) -> DesugarOut {
        let mut desugarer = self;
        let top = top.desugar(&mut desugarer);
        let Desugarer { b: arena, bspans: spans, .. } = desugarer;
        DesugarOut { spans, arena, top }
    }
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
        for decl in decls {
            let Modifiers { public, inner } = desugarer.t.decls[decl].clone();
            use t::Declaration as Decl;
            let inner = match inner {
                Decl::DataDef(decl) => {
                    let t::DataDef { name, params, def } = decl;
                    let span = name.span(desugarer);
                    // name -> pat
                    let name = name.desugar(desugarer);
                    let pat = Alloc::alloc(desugarer, span.clone());
                    let pat = desugarer.pat(pat, name.into());
                    // params -> copattern
                    let params = params.desugar(desugarer);
                    let params = params
                        .into_iter()
                        .map(|param| {
                            // param -> copattern
                            let span = param.span(desugarer);
                            let co = Alloc::alloc(desugarer, span);
                            let param = desugarer.copat(co, param.into());
                            param
                        })
                        .collect::<Vec<_>>();
                    let co = Alloc::alloc(desugarer, span.clone());
                    let params = desugarer.copat(co, b::App(params).into());
                    // def -> term
                    let def = span.make(def).desugar(desugarer);
                    // params & def -> abs term
                    let abs = Alloc::alloc(desugarer, span.clone());
                    let abs = desugarer.term(abs, b::Abs(params, def).into());
                    // abs -> sealed
                    let sealed = Alloc::alloc(desugarer, span.clone());
                    let sealed = desugarer.term(sealed, b::Sealed(abs).into());
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: sealed }.into()
                }
                Decl::CoDataDef(decl) => {
                    let t::CoDataDef { name, params, def } = decl;
                    let span = name.span(desugarer);
                    // name -> pat
                    let name = name.desugar(desugarer);
                    let pat = Alloc::alloc(desugarer, span.clone());
                    let pat = desugarer.pat(pat, name.into());
                    // params -> copattern
                    let params = params.desugar(desugarer);
                    let params = params
                        .into_iter()
                        .map(|param| {
                            // param -> copattern
                            let span = param.span(desugarer);
                            let co = Alloc::alloc(desugarer, span);
                            let param = desugarer.copat(co, param.into());
                            param
                        })
                        .collect::<Vec<_>>();
                    let co = Alloc::alloc(desugarer, span.clone());
                    let params = desugarer.copat(co, b::App(params).into());
                    // def -> term
                    let def = span.make(def).desugar(desugarer);
                    // params & def -> abs term
                    let abs = Alloc::alloc(desugarer, span.clone());
                    let abs = desugarer.term(abs, b::Abs(params, def).into());
                    // abs -> sealed
                    let sealed = Alloc::alloc(desugarer, span.clone());
                    let sealed = desugarer.term(sealed, b::Sealed(abs).into());
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: sealed }.into()
                }
                Decl::Define(decl) => {
                    let t::Define(genbind) = decl;
                    let (pat, term) = genbind.desugar(desugarer);
                    // sealed
                    let span = pat.span(desugarer);
                    let sealed = Alloc::alloc(desugarer, span);
                    let sealed = desugarer.term(sealed, b::Sealed(term).into());
                    // pat & sealed -> alias
                    b::Alias { binder: pat, bindee: sealed }.into()
                }
                Decl::Alias(decl) => {
                    let t::Alias(genbind) = decl;
                    let (pat, term) = genbind.desugar(desugarer);
                    // pat & term -> alias
                    b::Alias { binder: pat, bindee: term }.into()
                }
                Decl::Extern(decl) => {
                    // Todo: error on rec
                    let t::Extern(t::GenBind { rec: _, comp, binder, params, ty, bindee: () }) =
                        decl;
                    let binder = binder.desugar(desugarer);
                    let params = params.map(|params| params.desugar(desugarer));
                    let ty = ty.map(|ty| ty.desugar(desugarer));
                    b::Extern { comp, binder, params, ty }.into()
                }
                // Decl::Layer(decl) => {
                //     let t::Layer { name, uses, top } = decl;
                //     let top = top.desugar(desugarer);
                //     let uses = uses
                //         .into_iter()
                //         .map(|Modifiers { public, inner }| Modifiers { public, inner })
                //         .collect();
                //     b::Layer { name, uses, top }.into()
                // }
                // Decl::UseDef(decl) => {
                //     let t::UseDef(uses) = decl;
                //     b::UseDef(uses).into()
                // }
                // Decl::UseBlock(decl) => {
                //     let t::UseBlock { uses, top } = decl;
                //     let top = top.desugar(desugarer);
                //     b::UseBlock { uses, top }.into()
                // }
                Decl::Main(decl) => {
                    let t::Main(term) = decl;
                    let term = term.desugar(desugarer);
                    b::Main(term).into()
                }
            };
            let span = decl.span(desugarer);
            let decl = Alloc::alloc(desugarer, span);
            decls_new.push(desugarer.decl(decl, Modifiers { public, inner }))
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
        let id = Alloc::alloc(desugarer, span);
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
        let id = Alloc::alloc(desugarer, span);
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
        let id = Alloc::alloc(desugarer, span);
        use t::CoPattern as CoPat;
        match copat {
            CoPat::Pat(pat) => {
                let pat = pat.desugar(desugarer);
                desugarer.copat(id, pat.into())
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
        let id = Alloc::alloc(desugarer, span);
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
                // params -> ann = (hole: params)
                let params = params.desugar(desugarer);
                let span = params.span(desugarer);
                let hole = Alloc::alloc(desugarer, span.clone());
                let hole = desugarer.pat(hole, b::Hole.into());
                let ann = Alloc::alloc(desugarer, span.clone());
                let ann = desugarer.pat(ann, b::Ann { tm: hole, ty: params }.into());
                // ann -> copat
                let copat = Alloc::alloc(desugarer, span);
                let copat = desugarer.copat(copat, ann.into());
                // copat & ty -> pi
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Pi(copat, ty).into())
            }
            Tm::Forall(term) => {
                let t::Forall(params, ty) = term;
                let params = params.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Pi(params, ty).into())
            }
            Tm::Sigma(term) => {
                let t::Sigma(params, ty) = term;
                let params = params.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Sigma(params, ty).into())
            }
            Tm::Prod(term) => {
                let t::Prod(terms) = term;
                let mut terms = terms.desugar(desugarer);
                let mut out = terms.pop().unwrap();
                while let Some(term) = terms.pop() {
                    // term -> ann = (hole: pat)
                    let span = term.span(desugarer);
                    let hole = Alloc::alloc(desugarer, span.clone());
                    let hole = desugarer.pat(hole, b::Hole.into());
                    let ann = Alloc::alloc(desugarer, span.clone());
                    let ann = desugarer.pat(ann, b::Ann { tm: hole, ty: term }.into());
                    // ann -> copat
                    let copat = Alloc::alloc(desugarer, span.clone());
                    let copat = desugarer.copat(copat, ann.into());
                    // copat & sigma -> sigma
                    let span = out.span(desugarer);
                    let sigma = Alloc::alloc(desugarer, span);
                    out = desugarer.term(sigma, b::Sigma(copat, out).into());
                }
                out
            }
            Tm::Exists(term) => {
                let t::Exists(params, ty) = term;
                let params = params.desugar(desugarer);
                let ty = ty.desugar(desugarer);
                desugarer.term(id, b::Sigma(params, ty).into())
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
            // Tm::UseLet(term) => {
            //     let t::UseBind { uses, tail } = term;
            //     // Todo: uses
            //     let tail = tail.desugar(desugarer);
            //     desugarer.term(id, b::UseBind { uses, tail }.into())
            // }
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
            let ann = Alloc::alloc(desugarer, span);
            desugarer.term(ann, b::Ann { tm: bindee, ty }.into())
        } else {
            bindee
        };
        // params
        let params = params.map(|params| params.desugar(desugarer));
        // params? & bindee -> abs term binding
        let mut binding = if let Some(params) = params {
            let span = binder.span(desugarer);
            let binding = Alloc::alloc(desugarer, span);
            desugarer.term(binding, b::Abs(params, bindee).into())
        } else {
            bindee
        };
        // rec?
        if rec {
            let span = binding.span(desugarer);
            let rec = Alloc::alloc(desugarer, span);
            binding = desugarer.term(rec, b::Rec(binder, binding).into());
        }
        // add thunk?
        if rec || comp {
            let span = binding.span(desugarer);
            let thunk = Alloc::alloc(desugarer, span);
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
        let term = Alloc::alloc(desugarer, self.info);
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
        let term = Alloc::alloc(desugarer, self.info);
        desugarer.term(term, b::CoData { arms }.into())
    }
}

use impls::*;

mod impls {
    use super::*;

    pub(super) trait Spanned {
        fn span(&self, desugarer: &mut Desugarer) -> Span;
    }

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
    impl Spanned for t::DeclId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.tspans.decls[*self].clone()
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
    impl Spanned for b::DeclId {
        fn span(&self, desugarer: &mut Desugarer) -> Span {
            desugarer.bspans.decls[*self].clone()
        }
    }

    pub(super) trait Alloc {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self;
    }

    impl Alloc for b::DefId {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self {
            desugarer.bspans.defs.alloc(span)
        }
    }
    impl Alloc for b::PatId {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self {
            desugarer.bspans.pats.alloc(span)
        }
    }
    impl Alloc for b::CoPatId {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self {
            desugarer.bspans.copats.alloc(span)
        }
    }
    impl Alloc for b::TermId {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self {
            desugarer.bspans.terms.alloc(span)
        }
    }
    impl Alloc for b::DeclId {
        fn alloc(desugarer: &mut Desugarer, span: Span) -> Self {
            desugarer.bspans.decls.alloc(span)
        }
    }

    impl Desugarer {
        pub fn lookup_def(&self, id: t::DefId) -> t::VarName {
            self.t.defs[id].clone()
        }
        pub fn lookup_pat(&self, id: t::PatId) -> t::Pattern {
            self.t.pats[id].clone()
        }
        pub fn lookup_copat(&self, id: t::CoPatId) -> t::CoPattern {
            self.t.copats[id].clone()
        }
        pub fn lookup_term(&self, id: t::TermId) -> t::Term {
            self.t.terms[id].clone()
        }
        pub fn lookup_decl(&self, id: t::DeclId) -> Modifiers<t::Declaration> {
            self.t.decls[id].clone()
        }

        pub fn def(&mut self, id: b::DefId, def: b::VarName) -> b::DefId {
            self.b.defs.insert(id, def);
            id
        }
        pub fn pat(&mut self, id: b::PatId, pat: b::Pattern) -> b::PatId {
            self.b.pats.insert(id, pat);
            id
        }
        pub fn copat(&mut self, id: b::CoPatId, copat: b::CoPattern) -> b::CoPatId {
            self.b.copats.insert(id, copat);
            id
        }
        pub fn term(&mut self, id: b::TermId, term: b::Term<b::NameRef<b::VarName>>) -> b::TermId {
            self.b.terms.insert(id, term);
            id
        }
        pub fn decl(&mut self, id: b::DeclId, decl: Modifiers<b::Declaration>) -> b::DeclId {
            self.b.decls.insert(id, decl);
            id
        }
    }
}
